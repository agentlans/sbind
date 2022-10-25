(in-package :sbind)

(defmacro if-sbind ((expr pattern) body-if-true &optional (body-if-false nil))
  "Matches expr against pattern. If pattern matches, runs body-if-true with
the matched variable bindings, otherwise runs body-if-false."
  (let ((e (gensym)))
    `(handler-case
	 (sbind ,pattern ,expr
	   ,body-if-true)
       (error (,e)
	 (declare (ignorable ,e))
	 ,body-if-false))))
#|
(if-sbind ('(1 2 c) (a b 'd))
	  (+ a b) 'no)
|#

(defmacro when-sbind ((expr pattern) &body body)
  "If expr matches the pattern, then executes body
with the bindings from the pattern."
  `(if-sbind (,expr ,pattern)
	     (progn ,@body)
       nil))

(defmacro unless-sbind ((expr pattern) &body body)
  "Executes body if expr doesn't match the pattern."
  `(if-sbind (,expr ,pattern)
	     nil
       (progn ,@body)))

(defun scase-code (expr cases)
  "Generates the code needed to match expr against cases."
  (if (not cases)
      nil
      (sbind ((pattern out-expr) . other-cases)
	  cases
	(if (member pattern '(t else :otherwise))
	    out-expr ; the else clause
	    `(if-sbind (,expr ,pattern)
		       ,out-expr ; if match
		 ,(scase-code expr other-cases)))))) ; if not match
;; (scase-code 'foo '((a 1) (b 2)))

(defmacro scase (expr . cases)
  "Matches expr against the patterns in cases.
Returns the corresponding value if it matches.
A case can start with T, else, or :otherwise which will always
match expr."
  (let ((x (gensym)))
    `(let ((,x ,expr))
       ,(scase-code x cases))))
#|
(scase '(triangle 100 200)
       (('triangle base height) (/ (* base height) 2))
       (('rectangle width height) (* width height))
       (t 'unknown))
|#
