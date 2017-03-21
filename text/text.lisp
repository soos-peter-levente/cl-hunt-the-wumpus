(in-package :cl-wumpus)

(defparameter *string-set* *text-en*)

(defun get-text (id)
  (second (assoc id *string-set*)))

(defmacro txt (string-symbol-id &optional &rest varlist)
  (if (not varlist)
      `(format t ,(get-text string-symbol-id))
      `(format t ,(get-text string-symbol-id) ,@varlist)))
