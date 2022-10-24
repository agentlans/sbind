(in-package :sbind)

(defun quoted-form? (x)
  (and (listp x) (equal (car x) 'quote)))

(defun get-quoted-object (x)
  (second x))

(defun literal? (x)
  (or (equal x nil) (quoted-form? x)
      (and (atom x) (not (symbolp x)))))

(defun get-literal (x)
  (if (quoted-form? x)
      (get-quoted-object x)
      x))

(defun trek (x from how collector)
  "Steps through the tree and calls collector at each node.
from, how: the path that led to x.
collector: a function that gathers values."
  (macrolet ((collect (x)
	       `(funcall collector (list ,x (list how from)))))
    (cond ((literal? x) (collect (list (get-literal x))))
	  ((symbolp x) (collect x))
	  (t (let ((name (gensym)))
	       (collect name)
	       (trek (car x) name 'car collector)
	       (trek (cdr x) name 'cdr collector))))))

(defun make-collector ()
  "Returns a closure that saves objects."
  (let ((coll nil))
    (lambda (&rest x)
      (if x
	  ;; If given an argument, add to collection
	  (push (car x) coll)
	  ;; If no arguments, return all results
	  (reverse coll)))))

(defun deconstruct-tree (tree)
  "Steps through the tree and returns a list of node connections."
  (let ((coll (make-collector)))
    (trek tree nil nil coll)
    (funcall coll)))

(defun arrange (pairs vars-seen body)
  "Processes the deconstructed tree into code."
  (if (not pairs)
      `(progn ,@body) ; just the code body
      ;; Go through each variable in the deconstruction
      (let* ((pair (car pairs))
	     (lhs (car pair))
	     (rhs (cadr pair)))
	(cond ((listp lhs) ; match a literal value
	       `(if (equal ,rhs (quote ,(car lhs)))
		    ,(arrange (cdr pairs) vars-seen body)
		    (error "Literal doesn't match")))
	      ((member lhs vars-seen)
	       ;; Already seen this variable before. Make sure it's the same
	       `(if (equal ,rhs ,lhs)
		    ,(arrange (cdr pairs) vars-seen body)
		    (error "Inconsistent values")))
	      (t `(let ((,lhs ,rhs)) ; new variable so introduce LET form
		    ,(arrange (cdr pairs)
			      (cons lhs vars-seen)
			      body)))))))

(defun sbind-code (des-tree expr body)
  "Generates code for the sbind macro.
The first variable needs to be bound to expr."
  (let ((first-var (caar des-tree)))
    `(let ((,first-var ,expr))
       ,(arrange (cdr des-tree) nil body))))

(defmacro sbind (forms expr &body body)
  "Binds expr to the given forms then executes body.
Similar to DESTRUCTURING-BIND except literals and repeated
variables are allowed in form."
  (sbind-code (deconstruct-tree forms)
	      expr body))
