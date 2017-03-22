(in-package :cl-wumpus)

(defparameter *string-set* *text-en*)

(defun get-text (id)
  (second (assoc id *string-set*)))

(defun txt (string-symbol-id &optional &rest expr-list)
  (apply #'format t (get-text string-symbol-id) expr-list))
