;;;; P31-P41. Solutions to all problems in section headed:
;;;; "Arithmetic"

;;; P31
(defun primep (n)
  (or (= n 2)
      (let ((limit (1+ (floor (sqrt n)))))
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

;;; P35
;;; The partial duplication of the primep code is to avoid
;;; having to repeat the trial division to find the factor
;;; in the case that n is not prime
(defun prime-factors (n)
  (let ((limit (1+ (floor (sqrt n)))))
    (do ((x 2 (1+ x)))
      ((or (= x limit)
           (zerop (mod n x)))
       (if (= x limit)
         (list n)
         (append (list x)
                 (prime-factors (/ n x))))))))

;;; P36
(load "working-with-lists.lisp")
(defun prime-factors-mult (n)
  (mapcar #'reverse
          (encode (prime-factors n))))

;;; P37
;;; Formula given doesn't seem to work, alternative
;;; formula used instead
#+nil
(defun phi-improved (m)
  (reduce #'+
          (mapcar (lambda (x)
                    (* (1- (car x))
                       (expt (car x)
                             (1- (cadr x)))))
                  (prime-factors-mult m))))

(defun phi-improved (m)
  (reduce #'*
          (mapcar (lambda (x)
                    (- 1 (/ 1 x)))
                  (remove-duplicates
                    (prime-factors m)))
          :initial-value m))

;;; P37
(defun compare-methods ()
  (print "Stats for first method:")
  (time (totient-phi 10090))
  (print "Stats for second method:")
  (time (phi-improved 10090)))

;;; P39
(defun primes-in-range (start end)
  (loop for x from start to end
        when (primep x)
        collect x))

;;; P40
(defun goldbach (n)
  (when (and (evenp n)
             (> n 2))
    (loop for prime in (primes-in-range 2 n)
          when (primep (- n prime))
          return (list prime (- n prime)))))

;;; P41
(defun goldbach-list (start end)
  (loop for x from (if (evenp start)
                     start
                     (1+ start))
        to end by 2 do
        (destructuring-bind (a b) (goldbach x)
          (format t "~d = ~d + ~d~%" x a b))))

(defun goldbach-list-exceeding (start end limit)
  (loop for x from (if (evenp start)
                     start
                     (1+ start))
        to end by 2 when (> x 2) do
        (destructuring-bind (a b) (goldbach x)
          (when (and (> a limit)
                     (> b limit))
            (format t "~d = ~d + ~d~%" x a b)))))
