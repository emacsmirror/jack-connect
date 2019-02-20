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

(defvar *j-tree* nil)
(defvar *j-ports* (make-hash-table :test #'equal))

(defun make-empty-port ()
  "Make an empty port."
  (copy-tree `((client)
               (name)
               (connections)
               (type)
               (properties))))

(defmacro jack-get-port (port)
  `(gethash ,port *j-ports*))

(defmacro jack-port-properties (port)
  `(alist-get :properties  (jack-get-port ,port)))

(defmacro jack-port-type (port)
  `(alist-get :type        (jack-get-port ,port)))

(defmacro jack-port-name (port)
  `(alist-get :name        (jack-get-port ,port)))

(defmacro jack-port-connections (port)
  `(alist-get :connections (jack-get-port ,port)))

(defmacro jack-port-client (port)
  `(alist-get :client      (jack-get-port ,port)))

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


;;; node: atom || (ch node ...))
;;; nodes|tree: (node ...)

(defun j-nodes-push (nodes keystr elem)
  "Add the atomic value ELEM to NODES, indexed by KEYSTR."
  (if (string-empty-p keystr)
      ;; terminal node. elem must be atomic
      (cons elem nodes)
    (let* ((first-char    (substring keystr 0 1))
           (rest-string   (substring keystr 1))
           (matching-node (assoc first-char nodes #'string=)))
      (pcase matching-node
        ;; found a node
        (`(,_ . ,children)
         (setf (cdr matching-node)
               (j-nodes-push children
                             rest-string
                             elem))
         
         nodes)
        ;; no matching node found
        (_
         (let ((new-elt
                `(,first-char
                  ,@(j-nodes-push (list)
                                  rest-string
                                  elem))))
           (cons new-elt nodes)))))))

(defun make-jack-nodes (strings)
  "Construct a jack-nodes tree from STRINGS."
  (let ((tree))
    (dolist (str strings)
      (setf tree (j-nodes-push tree str str)))
    (j-nodes-compress tree)))


(defun j-node-prepend-string (node prefix)
  "Prepend a PREFIX to the string of a NODE."
  (pcase node
    ((pred atom) node)

    (`(,suffix . ,nodes)
     `(,(concat prefix suffix) ,@nodes))))


;;; TODO: make this an inner function?
(defun j-compress-node* (node)
  (pcase node
    ;; only one child
    ((pred atom) node)
    
    (`(,prefix (,suf . ,nodes))
     (j-compress-node*
      `(,(concat prefix suf) ,@nodes)))
    
    ;; (`(,prefix ,child)
    ;;  (j-compress-node* (j-node-prepend-string child prefix)))

    (`(,prefix . ,nodes)
     `(,prefix ,@(mapcar #'j-compress-node* nodes)))
                            
    (err (error "Invalid node %s" err))))


(defun j-nodes-compress (nodes)
  "Compress NODES by merging parents of one child with their children."
  (mapcar #'j-compress-node* nodes))


(defun j-nodes-disband (nodes keep-key)
  "Disband NODES when KEEP-KEY on the discendent atoms gives different results."
  (letrec ((j-extract-properties
            (lambda (node)
              (pcase node
                ((pred atom)
                 ;; (props node)
                 `(,(funcall keep-key node) ,node))
                
                (`(,prefix . ,nodes)
                 (let* ((prop+nodes   (mapcar j-extract-properties nodes))
                        (prop         (mapcar #'car prop+nodes))
                        (nodes        (mapcan #'cdr prop+nodes))
                        (grouped-prop (cl-reduce (lambda (a b)
                                                   (and (equal a b) a))
                                                 prop)))
                   (if grouped-prop
                       ;; (props node)
                       `(,grouped-prop (,prefix ,@nodes))
                     ;; (props node*)
                     `(nil ,@(mapcar (lambda (node)
                                     (j-node-prepend-string node prefix))
                                   nodes)))))))))
    (let* ((prop+nodes (mapcar j-extract-properties nodes))
           (nodes      (mapcan #'cdr prop+nodes)))
      (j-nodes-compress nodes))))


;; (defvar *j-tst* nil)

;; (setf *j-tst* nil
;;       *j-tst* (j-nodes-push *j-tst* "pippo"    'dog)
;;       *j-tst* (j-nodes-push *j-tst* "pluto"    'dog)
;;       *j-tst* (j-nodes-push *j-tst* "paperino" 'duck)
;;       *j-tst* (j-nodes-push *j-tst* "paperone" 'duck)
;;       *j-tst* (j-nodes-push *j-tst* "ciccio"   'duck)
;;       *j-tst* (j-nodes-push *j-tst* "pitagora" 'duck))

;; (setf *j-tst* (j-nodes-compress *j-tst*))

;; (j-nodes-disband *j-tst* #'identity)
;; (j-nodes-filter *j-tst* (lambda (p) (eq p 'dog)))

(defun j-nodes-filter (nodes predicate)
  "Keep only NODES where PREDICATE is t."
  (cl-flet ((filter-node
             (node)
             (pcase node
               ((pred atom)
                (and (funcall predicate node) node))
               (`(,prefix . ,nodes)
                (let ((nodes (j-nodes-filter nodes
                                             predicate)))
                  (and nodes (cons prefix
                                  nodes)))))))
    (seq-filter #'identity
                (mapcar #'filter-node nodes))))

(defun j-nodes-flatten (nodes)
  "Traverse NODES recursively and accumulate atoms into a flatten alist."
  (let ((alst))
    (letrec ((collect-node
              (lambda (node prefix)
                (pcase node
                  ((pred atom)
                   (list node))
                  
                  (`(,suffix . ,nodes)
                   (let* ((prefix (concat prefix suffix))
                          (atoms (seq-mapcat
                                  (lambda (node)
                                    (funcall collect-node
                                             node
                                             prefix))
                                  nodes))
                          (sorted-atoms
                           (sort atoms #'string-lessp)))
                     
                     (push `(,prefix ,@sorted-atoms) alst)
                     sorted-atoms))))))
      (dolist (node nodes)
        (funcall collect-node node ""))
      alst)))

(defun j-nodes-mutate (nodes mutator)
  (let ((mutate-node
         (lambda (node)
           (pcase node
             ((pred atom) (funcall mutator node))
             (`(,ch . ,nodes) (j-nodes-mutate nodes mutator))))))
    (mapcar 'mutate-node nodes)))

(defun j-nodes-decorate (alst)
  "Append `*' to keys with more than one value in ALST."
  (mapcar (lambda (kv)
            (pcase kv
              ((pred atom) kv)
              (`(,k ,v) kv)
              (`(,k . ,v) `(,(concat k "*") ,@v))))
          alst))

(defun jack-running-p ()
  "Check if jack is running."
  (pcase (process-lines "jack_wait" "-c")
    (`("running") t)
    (any nil)))

(defun jack-lsp ()
  "Update the port tree parsing the output of jack_lsp."
  (let ((current-port nil)
        (tree (list)))
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
          (puthash current-port (make-empty-port) *j-ports*)
          (setf (jack-port-name current-port)  port
                (jack-port-client current-port) client)
          (setf tree (j-nodes-push tree current-port current-port))))))
    (setf *j-tree* (j-nodes-compress tree))))

;;;###autoload
(defun jack-connect (p1s p2s)
  "Connect port selection P1S with port selection P2S."
  (interactive
   (progn
     (jack-lsp)
     (let* ((node1 (-> *j-tree*
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
            (node2 (-> *j-tree*
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


(defun jack-merge-connections (ports)
  "Return the union of the connections of PORTS."
  (cl-reduce #'cl-union
             (mapcar (lambda (p) (jack-port-connections p))
                     ports)))

;;;###autoload
(defun jack-disconnect (p1s p2s)
  "Disconnect port set P1S from port set P2S."
  (interactive
   (progn
     (jack-lsp)
     (let* ((node1 (-> *j-tree*
                      (j-nodes-disband (lambda (p) (jack-port-client p)))
                      (j-nodes-filter #'jack-port-connected-p)
                      (j-nodes-compress)
                      (j-nodes-flatten)
                      (j-nodes-decorate)))
            (sel1  (completing-read "disconnect jack port(s): " node1))
            (p1s   (cdr (assoc sel1 node1)))
            ;; make an alst with p1s
            (node2 (-> (jack-merge-connections p1s)
                      (make-jack-nodes)
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
