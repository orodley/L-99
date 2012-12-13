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

;;; P04
(defun my-length (list &optional (acc 0))
  (if (car list)
    (my-length (cdr list) (1+ acc))
    acc))

;;; P05
;;; REALLY inefficient compared to builtin
(defun my-reverse (list)
  (if (cdr list)
    (append (my-reverse (cdr list))
            (list (car list)))
    list))

;;; P06
(defun palindromep (list)
  (equal list (reverse list)))

;;; P07
(defun flatten (list)
  (when list
    (append (if (listp (car list))
              (flatten (car list))
              (list (car list)))
            (flatten (cdr list)))))

;;; P08
(defun compress (list)
  (when list
    (if (eq (car list)
            (cadr list))
      (compress (cdr list))
      (cons (car list)
            (compress (cdr list))))))
