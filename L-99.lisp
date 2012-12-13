;;;; Solutions to Lisp problems at http://goo.gl/ckddS

;;; P01
(defun my-last (list)
  (if (cdr list)
    (my-last (cdr list))
    (car list)))

;;; P02
(defun my-but-last (list)
  (if (cddr list)
    (my-but-last (cdr list))
    (car list)))

;;; P03
(defun element-at (list i)
  (when (>= i 0)
    (if (zerop i)
      (car list)
      (element-at (cdr list) (1- i)))))
