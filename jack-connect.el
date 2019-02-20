;;; jack-connect.el --- Manage jack connections within Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2014-2019 Stefano Barbi
;; Author: Stefano Barbi <stefanobarbi@gmail.com>
;; Version: 0.2

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

(require 'cl-lib)
(require 'dash)
(require 'seq)
(require 'j-nodes)

(defvar *j-ports* (make-hash-table :test #'equal))

(defun jack--make-port (name &optional client port-name)
  "Create the port record NAME with optional CLIENT and PORT-NAME."
  (puthash name
           (copy-tree `((:client ,@client)
                        (:name   ,@port-name)
                        (:connections)
                        (:type)
                        (:properties)))
           *j-ports*))

(defun jack--list-ports ()
  "Return a list of port names."
  (let ((lst))
    (maphash (lambda (k v) (push k lst)) *j-ports*)
    lst))

(defmacro jack-get-port (port)
  `(gethash ,port *j-ports*))


(defmacro define-jack-port-accessor (name kw)
  "Define NAME as both a getter and setter of a jack port property KW."
  `(progn
     (defun ,name (port)
       (alist-get ,kw (jack-get-port port)))
     (gv-define-simple-setter
      ,name
      (lambda (port value)
        (setf (alist-get ,kw (jack-get-port port)) value)))))

(define-jack-port-accessor jack-port-properties  :properties)
(define-jack-port-accessor jack-port-type        :type)
(define-jack-port-accessor jack-port-name        :name)
(define-jack-port-accessor jack-port-connections :connections)
(define-jack-port-accessor jack-port-client      :client)


(defun jack-port-input-p (port)
  "Return t if PORT is an input port."
  (and (memq 'input (jack-port-properties port)) t))

(defun jack-port-output-p (port)
  "Return t if PORT is an output port."
  (and (memq 'output (jack-port-properties port)) t))

(defun jack-port-audio-p (port)
  "Return t if PORT is an output port."
  (string-match-p "audio" (jack-port-type port)))

(defun jack-port-midi-p (port)
  "Return t if PORT is an output port."
  (string-match-p "midi" (jack-port-type port)))

(defun jack-port-connected-p (port)
  "Return t if PORT is an output port."
  (jack-port-connections port))

(defun jack-running-p ()
  "Check if jack is running."
  (pcase (process-lines "jack_wait" "-c")
    (`("running") t)
    (any nil)))

(defun jack-lsp ()
  "Update the port tree parsing the output of jack_lsp."
  (let ((current-port nil))
    (clrhash *j-ports*)
    (dolist (line (process-lines "jack_lsp" "-ctp"))
      (cond
       ;; port properties
       ((string-match "^[ \t]+properties: \\(.*\\)" line)
        (setf (jack-port-properties current-port)
              (mapcar #'intern
                      (split-string (replace-match "\\1" nil nil line) "," t))))

       ;; port connection
       ((string-match "^ \\{3\\}\\(.*\\)" line)
        (push (replace-match "\\1" nil nil line)
              (jack-port-connections current-port)))

       ;; port type
       ((string-match "^[ \t]+\\(.*\\)" line)
        (setf (jack-port-type current-port)
              (replace-match "\\1" nil nil line)))

       ;; port name (sets current-port)
       (t
        (cl-destructuring-bind (client port)
            (split-string line ":")
          (setf current-port line)
          (jack--make-port current-port client port)))))))

;;;###autoload
(defun jack-connect (p1s p2s)
  "Connect port selection P1S with port selection P2S."
  (interactive
   (progn
     (jack-lsp)
     (let* ((tree   (-> (jack--list-ports)
                       (make-j-nodes)))
            (node1 (-> tree
                      (j-nodes-filter #'jack-port-output-p)
                      (j-nodes-disband (lambda (p)
                                         (list
                                          (jack-port-client p)
                                          (jack-port-type p))))
                      (j-nodes-compress)
                      (j-nodes-flatten)
                      (j-nodes-decorate)))
            (sel1  (completing-read "connect: " node1))
            (p1s   (cdr (assoc sel1 node1)))
            (type  (jack-port-type (car p1s)))
            (node2 (-> tree
                      (j-nodes-filter #'jack-port-input-p)
                      (j-nodes-filter (lambda (p)
                                        (string= (jack-port-type p) type)))
                      (j-nodes-compress)
                      (j-nodes-flatten)
                      (j-nodes-decorate)))
            (sel2  (completing-read (format "connect %s to: "
                                            sel1)
                                    node2)))
       (list p1s (cdr (assoc sel2 node2))))))
  (when p1s
    (cl-mapc (lambda (p1 p2)
               (call-process "jack_connect" nil nil nil
                             p1
                             p2))
             p1s
             p2s)))


(defun jack--merge-connections (ports)
  "Return the union of the connections of PORTS."
  (cl-reduce #'cl-union
             (mapcar #'jack-port-connections ports)))

;;;###autoload
(defun jack-disconnect (p1s p2s)
  "Disconnect port set P1S from port set P2S."
  (interactive
   (progn
     (jack-lsp)
     (let* ((node1 (-> (jack--list-ports)
                      (make-j-nodes)
                      (j-nodes-disband #'jack-port-client)
                      (j-nodes-filter #'jack-port-connected-p)
                      (j-nodes-compress)
                      (j-nodes-flatten)
                      (j-nodes-decorate)))
            (sel1  (completing-read "disconnect jack port(s): " node1))
            (p1s   (cdr (assoc sel1 node1)))
            ;; make an alst with p1s
            (node2 (-> (jack--merge-connections p1s)
                      (make-j-nodes)
                      (j-nodes-compress)
                      (j-nodes-flatten)
                      (j-nodes-decorate)))
            (sel2  (completing-read (format "disconnect %s from: "
                                            sel1)
                                    node2))
            (p2s   (cdr (assoc sel2 node2))))
       (list p1s p2s))))
  (when p1s
    (dolist (p1 p1s)
      (dolist (p2 p2s)
        (when (member p2 p2s)
          (call-process "jack_disconnect" nil nil nil
                        p1 p2))))))

(provide 'jack-connect)
;;; jack-connect.el ends here
