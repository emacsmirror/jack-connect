;;; -*- lexical-binding: t -*-
;;; jack-connect.el --- Manage jack connections within Emacs

;; Copyright (C) 2014-2019 Stefano Barbi
;; Author: Stefano Barbi <stefanobarbi@gmail.com>
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; jack-connect and jack-disconnect allow to manage connections of
;; jackd audio server from Emacs minibuffer.

;;; Code:


(require 'let-alist)

;;; helper functions

(defvar *jack-ports* nil
  "A global var representing a list of jack ports.")

(defun jack-port-filter (pred)
  "Filter ports according to PRED.
PRED is a function of a port record returning boolean.
ALIST is an annotated alist of ports as produced by jack_lsp."
  (seq-filter pred *jack-ports*))

(defun jack-port-find (pred)
  "Filter ports according to PRED.
PRED is a function of a port record returning boolean.
ALIST is an annotated alist of ports as produced by jack_lsp."
  (seq-find pred *jack-ports*))

(defun jack-port-input-p (port)
  "Return t if PORT is an input port."
  (member 'input (alist-get 'properties port)))

(defun jack-port-output-p (port)
  "Return t if PORT is an output port."
  (member 'output (alist-get 'properties port)))

(defun jack-port-name (port)
  "Return the name of the PORT."
  (alist-get 'name port))

(defun jack-port-can-connect-p (port)
  "Return a predicate that tell if a port can be connected to PORT."
  (let ((port-connections (alist-get 'connections port))
	(port-type (alist-get 'type port))
        (port-name (alist-get 'name port)))
    (if (jack-port-input-p port)
        (lambda (oprt)
          (let-alist oprt
            (and (not (eq port oprt))
               (not (equal .name port-name))
               (jack-port-output-p oprt)
	       (equal .type port-type)
	       (not (member .name port-connections)))))
      (lambda (oprt)
          (let-alist oprt
            (and (not (eq port oprt))
               (not (equal .name port-name))
               (jack-port-input-p oprt)
	       (equal .type port-type)
	       (not (member .name port-connections))))))))

(defun make-empty-port ()
  "Make an empty port."
  (copy-tree `((name)
               (connections)
               (type)
               (properties))))

(defun jack-lsp ()
  "Update the port alist parsing the output of jack_lsp."
  (let ((ports) (current-port (make-empty-port)))
    (dolist (line (reverse (process-lines "jack_lsp" "-ctp")))
      (cond
       ((string-match "^[ \t]+properties: \\(.*\\)" line)
        (setf (alist-get 'properties current-port)
              (mapcar 'intern
                      (split-string (replace-match "\\1" nil nil line) "," t))))

       ((string-match "^ \\{3\\}\\(.*\\)" line)
	(push (replace-match "\\1" nil nil line)
              (alist-get 'connections current-port)))
                   
       ((string-match "^[ \t]+\\(.*\\)" line)
        (setf (alist-get 'type current-port)
              (replace-match "\\1" nil nil line)))

       ;; port name (this is the last element parsed when the output
       ;; of jack_lsp is reverted)
       (t
        (setf (alist-get 'name current-port) line)
        (push current-port ports)
        (setq current-port (make-empty-port)))))
    (setf *jack-ports* ports)))


(defun jack-get-port (name)
  "Retrieve a port by NAME."
  (seq-find (lambda (prt) (equal (jack-port-name prt) name))
            *jack-ports*))

;;;###autoload
(defun jack-connect (port1 port2)
  "Connect PORT1 to PORT2."
  (interactive
   (progn
     (jack-lsp)
     (let ((from-ports (jack-port-filter #'jack-port-output-p)))
       (if from-ports
	   (let* ((from-port-string (completing-read "Output port: " (mapcar #'jack-port-name from-ports)))
		  (from-port        (jack-get-port from-port-string))
                  (to-ports         (jack-port-filter (jack-port-can-connect-p from-port)))
		  (to-port-string   (completing-read
		        	     (format "Connect %s (%s) to: " from-port-string (alist-get 'type from-port))
		        	     (mapcar #'jack-port-name to-ports))))
	     (list from-port-string to-port-string))
         (progn (message "No port can be connected")
	        (list nil nil))))))
  (when port1
    (call-process "jack_connect" nil nil nil port1 port2)))

;;;###autoload
(defun jack-disconnect (port1 port2)
  "Disconnect the two connected ports PORT1 and PORT2."
  (interactive
   (progn
     (jack-lsp)
     (let ((from-ports (jack-port-filter (lambda (p) (alist-get 'connections p)))))
       (if from-ports
	   (let* ((from-port-string (completing-read "Disconnect port: " (mapcar #'jack-port-name from-ports)))
		  (from-port (jack-get-port from-port-string))
		  (to-ports (alist-get 'connections from-port))
		  (to-port-string (completing-read "From port: " to-ports)))
	     (list from-port-string to-port-string))
         (progn (message "No port can be disconnected")
	        (list nil nil))))))
  (when port1
    (call-process "jack_disconnect" nil nil nil port1 port2)))

;;;###autoload
(defun jack-disconnect-all-from (from connections)
  "Disconnect all the ports connected to FROM port.
CONNECTIONS is the list of ports connected to FROM."
  (interactive
   (progn
     (jack-lsp)
     (let ((from-ports (jack-port-filter (lambda (p) (alist-get 'connections p)))))
       (if from-ports
	   (let ((from-port-string (completing-read "Disconnect all connections from port: "
					     (mapcar #'jack-port-name from-ports))))
	     (if (yes-or-no-p (format "Disconnecting all connections from %s. Are you sure"
				      from-port-string))
	         (list from-port-string (alist-get 'connections (jack-port-get from-port-string)))
	       (list nil nil)))
         (progn (message "No port can be disconnected")
	        (list nil nil))))))
  (when from
    (dolist (to connections)
      (jack-disconnect from to))))

(provide 'jack-connect)

;; jack-connect.el ends here
