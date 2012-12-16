;;;; P46-50. Solutions to all problems in section headed:
;;;; "Logic and Codes"

;;; P46
(defun my-and (a b)
  (when a
    (when b
      b)))

(defun my-or (a b)
  (if a
    a
    (if b
      b)))

(defun nand (a b)
  (not (my-and a b)))

(defun nor (a b)
  (not (my-or a b)))

(defun xor (a b)
  (if a
    (unless b
      a)
    (when b
      b)))

(defun impl (a b)
  (if a
    b
    t))

(defun equ (a b)
  (if a
    b
    (not b)))

;;; P46
(defmacro table (a b logical-expr)
  (flet ((row (a-val b-val)
    `(format t "~{~:[fail~;true~] ~}~%"
             (list ,a-val ,b-val 
                   (eval (subst ,a-val (quote ,a)
                                (subst ,b-val (quote ,b)
                                       (quote ,logical-expr)))))))) 
    `(progn
       ,(row nil nil)
       ,(row nil t)
       ,(row t nil)
       ,(row t t))))
