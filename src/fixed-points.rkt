#lang racket
(provide fixed-point)
 
(define tolerance 0.00001)
(define (close-enough? x y)
  (< (abs (- x y)) tolerance))
(define (average x y)
  (/ (+ x y) 2))

(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? next guess)
          next
          (try next))))
  (try first-guess))

(define (fixed-point-debug f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? next guess)
          next
          (try next))))
  (try first-guess))

;;continued fraction
(define (cont-frac n d k)
  (define (denominator i)
    (if (= i k)
        (/ (n i) (d i))
        (+ (d i) (/ (n (+ i 1)) (denominator (+ i 1))))))
  (/ (n 1) (denominator 1)))

(define (cont-frac-iter n d k)
  (define (iter i accu)
    (let ((new-value (/ (n i)(+ (d i) accu)))) 
    (cond ((= i 1) new-value)
          (else (iter (- i 1) new-value)))))
  (iter (- k 1) (/ (n k)(d k))))

;;application
(define (e-2 k)
  (cont-frac-iter (lambda (i) 1.0)
                  (lambda (i)
                    (cond ((= 0 (remainder (+ i 1) 3))
                           (* 2 (/ (+ i 1) 3)))
                          (else 1)))
                  k))

(define (tan-cf x k)
  (cont-frac-iter (lambda (i)
                    (if (= 1 i)
                        x
                        (- (* x x))))
                  (lambda (i)
                    (- (* i 2) 1))
                  k))