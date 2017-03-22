(in-package :cl-wumpus)

(defparameter *string-set* *text-en*)

(defun get-text (id)
  (second (assoc id *string-set*)))

(defun txt (string-symbol-id &optional &rest varlist)
  (if (not varlist)
      (format t (get-text string-symbol-id))
      (eval `(format t ,(get-text string-symbol-id)
                     ,@(mapcar #'(lambda (expr)
                                   `',expr)
                               `,varlist)))))
