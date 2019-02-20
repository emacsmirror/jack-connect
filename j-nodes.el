;;; -*- lexical-binding: t -*-
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

;;; node: atom || (ch node ...))
;;; nodes|tree: (node ...)

(require 'cl-lib)
(require 'seq)

(defun j--nodes-push (nodes keystr elem)
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
               (j--nodes-push children
                             rest-string
                             elem))
         
         nodes)
        ;; no matching node found
        (_
         (let ((new-elt
                `(,first-char
                  ,@(j--nodes-push (list)
                                  rest-string
                                  elem))))
           (cons new-elt nodes)))))))

(defun make-j--nodes (strings)
  "Construct a jack-nodes tree from STRINGS."
  (let ((tree))
    (dolist (str strings)
      (setf tree (j--nodes-push tree str str)))
    (j--nodes-compress tree)))


(defun j--node-prepend-string (node prefix)
  "Prepend a PREFIX to the string of a NODE."
  (pcase node
    ((pred atom) node)

    (`(,suffix . ,nodes)
     `(,(concat prefix suffix) ,@nodes))))

;;; TODO: make this an inner function?
(defun j--compress-node* (node)
  (pcase node
    ;; only one child
    ((pred atom) node)
    
    (`(,prefix (,suf . ,nodes))
     (j--compress-node*
      `(,(concat prefix suf) ,@nodes)))
    
    ;; (`(,prefix ,child)
    ;;  (j-compress-node* (j--node-prepend-string child prefix)))

    (`(,prefix . ,nodes)
     `(,prefix ,@(mapcar #'j--compress-node* nodes)))
                            
    (err (error "Invalid node %s" err))))


(defun j--nodes-compress (nodes)
  "Compress NODES by merging parents of one child with their children."
  (mapcar #'j--compress-node* nodes))


(defun j--nodes-disband (nodes key)
  "Disband NODES when KEY on the discendent atoms gives different results."
  ;; disband: ("a" ("b") ("c")) => (("ab") ("ac"))
  (letrec ((extract-properties
            (lambda (node)
              (pcase node
                ((pred atom)
                 ;; (props node)
                 `(,(funcall key node) ,node))
                
                (`(,prefix . ,nodes)
                 (let* ((prop+nodes   (mapcar extract-properties nodes))
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
                                     (j--node-prepend-string node prefix))
                                   nodes)))))))))
    (let* ((prop+nodes (mapcar extract-properties nodes))
           (nodes      (mapcan #'cdr prop+nodes)))
      (j--nodes-compress nodes))))

(defun j--nodes-filter (nodes predicate)
  "Keep only NODES where PREDICATE is t."
  (cl-flet ((filter-node
             (node)
             (pcase node
               ((pred atom)
                (and (funcall predicate node) node))
               (`(,prefix . ,nodes)
                (let ((nodes (j--nodes-filter nodes
                                              predicate)))
                  (and nodes (cons prefix
                                 nodes)))))))
    (seq-filter #'identity
                (mapcar #'filter-node nodes))))

(defun j--nodes-flatten (nodes)
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

(defun j--nodes-mutate (nodes mutator)
  "Mutate terminal elements in NODES by applying the function of one argument MUTATOR."
  (let ((mutate-node
         (lambda (node)
           (pcase node
             ((pred atom) (funcall mutator node))
             (`(,ch . ,nodes) (j--nodes-mutate nodes mutator))))))
    (mapcar 'mutate-node nodes)))

(defun j--nodes-decorate (alst)
  "Append `*' to keys with more than one value in ALST."
  (mapcar (lambda (kv)
            (pcase kv
              ((pred atom) kv)
              (`(,k ,v) kv)
              (`(,k . ,v) `(,(concat k "*") ,@v))))
          alst))
(provide 'j-nodes)
;;; j-nodes.el ends here
