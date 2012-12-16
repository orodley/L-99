;;;; P31-P41. Solutions to all problems in section headed:
;;;; "Arithmetic"

;;; P31
(defun primep (n)
  (or (= n 2)
      (let ((limit (ceiling (sqrt n))))
        (do ((x 2 (1+ x)))
          ((or (>= x limit)
               (zerop (mod n x)))
           (= x limit))))))

;;; P32
(defun my-gcd (a b)
  ;; Make sure a is the highest and b the lowest
  (let ((a (max a b))
        (b (min a b)))
    (let ((rem (rem a b)))
      (if (zerop rem)
        a
        (gcd b rem)))))

;;; P33
(defun coprime (a b)
  (= 1 (gcd a b)))

;;; P34
(defun totient-phi (m)
  (loop for n from 1 below m
        when (coprime m n)
        count n))
