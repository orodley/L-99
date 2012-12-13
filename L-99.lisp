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

;;; P09
(defun pack (list)
  (when list
    (let ((list-car
            (if (listp (car list))
              (car list)
              (list (car list)))))
      (if (member (cadr list)
                  list-car)
        (pack (cons (append list-car
                            (list (cadr list))) 
                    (cddr list)))
        (cons list-car
              (pack (cdr list)))))))

;;; P10
(defun encode (list)
  (mapcar (lambda (repeated-elements)
            (list (length repeated-elements)
                  (car repeated-elements)))
          (pack list)))

;;; P11
(defun encode-modified (list)
  (mapcar (lambda (repeated-elements)
            (let ((len (length repeated-elements))
                  (element (car repeated-elements)))
              (if (= len 1)
                element
                (list len element))))
          (pack list)))

;;; P12
(defun decode (encoded-list)
  (reduce #'append
          (mapcar (lambda (item)
                    (if (listp item)
                      (loop repeat (car item)
                            collecting (cadr item))
                      (list item)))
                  encoded-list)))

;;; P13
(defun direct-encode (list)
  (when list
    (let ((list-car
            (if (listp (car list))
              (car list)
              (list (car list)))))
      (if (member (cadr list)
                  list-car) 
        (direct-encode (cons (append
                               (if (= (length list-car) 1)
                                 '(2)
                                 (list (1+ (car list-car))))
                               (list (cadr list))) 
                             (cddr list))) 
        (cons (if (= (length list-car) 1)
                (car list-car)
                list-car)
              (direct-encode (cdr list)))))))
