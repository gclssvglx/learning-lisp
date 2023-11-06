;; Fundamental data structures in Lisp are: symbols, numbers and lists.

;; These are made of cons cells. The code of a Lisp program is made out
;; of the same stuff!

;; It is this symmetry that makes the Lisp macro system possible.

(defmacro let1 (var val &body body)
  `(let ((,var ,val))
    ,@body))

;; Note the difference between these two identical functions...
;; (let ((foo (+ 2 3)))
;;   (* foo foo))

;; (let1 foo (+ 2 3)
;;   (* foo foo))


;; the &body in the macro sucks up the remaining params
;; much like Ruby, so you can do this...
;; > (let1 a (+ 2 3)
;;     (princ "Hello world")
;;       (* a a))
;; Hello World
;; 25

;; A REPL...

(defun repl ()
  (do ()
      (nil)
    (format t "~%$ ")
    (print (eval (read)))))


;; macro to set a variable to nil
;; (defmacro nil! (x)
;;   (list `setf x nil))
;; NIL!
;; [9]> (nil! y)
;; NIL
;; [10]> y
;; NIL
