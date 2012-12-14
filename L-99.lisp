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

;;; P14
(defun dupli (list)
  (reduce #'append
          (mapcar (lambda (x)
                    (list x x))
                  list)))

;;; P15
(defun repli (list n)
  (reduce #'append
          (mapcar (lambda (x)
                    (loop repeat n
                          collect x))
                  list)))

;;; P16
(defun drop (list step)
  (loop for item in list
        count item into index
        when (zerop (mod index step))
        collect item))

;;; P17
(defun split (list n)
  (list (loop for i from 0 to (1- n)
              collecting (elt list i))
        (loop for i from n to (1- (length list))
              collecting (elt list i))))

;;; P18
(defun slice (list start end)
  (loop for i from (1- start) to (1- end)
        collecting (elt list i)))

;;; P19
(defun rotate (list places)
  (apply #'append
         (reverse (split list
                         (mod places
                              (length list))))))

;;; P20
(defun remove-at (list place)
  (loop for item in list
        count item into index
        when (/= index place)
        collect item))

;;; P21
(defun insert-at (item list place)
  (append (slice list 1 (1- place))
          (list item)
          (slice list place (length list))))

;;; P22
(defun range (start end)
  (if (< start end) 
    (loop for n from start to end
          collecting n)
    (loop for n from start downto end
          collecting n)))

;;; P23
(defun rnd-select (list num-items)
  (loop repeat (- (length list) num-items)
        do (setf list (remove-at list
                                 (1+ (random (length list))))))
  list)

;;; P24
(defun lotto-select (num-items range-end)
  (rnd-select (range 1 range-end)
              num-items))

;;; P25
;;; Feels kinda dirty
(defun rnd-permu (list)
  (let ((shuffled ()))
    (loop while list
          do (let ((rnd-elt
                     (car (rnd-select list 1))))
               (setf list (remove rnd-elt list))
               (push rnd-elt shuffled)))
    shuffled))
