(in-package :sbind)

(defmacro slet (bindings &body body)
  "For each (form value) list in bindings,
the values of each binding
are bound to the variables in the corresponding form.
All the variable bindings are bound at the same time,
almost like a destructuring LET."
  (let ((var-forms (mapcar #'car bindings))
	(val-forms (mapcar #'cadr bindings))
	(v (gensym)))
    `(let ((,v (list ,@val-forms)))
       (sbind ,var-forms
	   ,v
	 ,@body))))
#|
;; This code works
;; but if the value of the second n doesn't match
;; the value of the first n, then error.
(slet (((m n) '(1 2))
       ((n k) '(2 3)))
  (list m n k))
|#

(defun slet*-loop (bindings body)
  (if (not bindings)
      `(progn ,@body)
      (let ((x (gensym))
	    (binding (car bindings)))
	`(let ((,x ,(second binding)))
	   (sbind ,(car binding)
	       ,x
	     ,(slet*-loop (cdr bindings) body))))))

(defmacro slet* (bindings &body body)
  "Like SLET except that the forms in the bindings
are evaluated one at a time which means that
the value of one form can refer to earlier forms in SLET*."
  (slet*-loop bindings body))

#|
(slet* (((one two) '(1 2))
	((a b) (list two one))) ; two and one take values from the previous form
  (list a b)) ;=> '(2 1)
|#
