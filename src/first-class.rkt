#lang racket
(require "basic.rkt")
(require "fixed-points.rkt")
(provide sqrt-m)

(define dx 0.00001)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

 (define (sqrt x)
    (newtons-method (lambda (y)(- (square y) x)) 1.0))

;;1.40
(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))
;;1.41
(define (double f)
  (lambda (x)
    (f (f x))))
;;1.42
(define (compose f g)
  (lambda (x)(f (g x))))
;;1.43
(define (repeated f n)
  (lambda (x)
    (if (= n 1)
        (f x)
        ((repeated f (- n 1))(f x)))))
(define (repeated-c f n)
  (if (= n 1)
      f
      (compose f (repeated-c f (- n 1)))))
;;1.44
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx)) (f x) (f (+ x dx))) 3)))
(define (n-fold-smooth f n)
  (repeated (smooth f) n))
;;1.45
(define (average-dump f)
  (lambda (x) (average x (f x))))
(define (n-root x n m)
  (fixed-point
   ((repeated-c average-dump m)(lambda (y) (/ x (fast-expr y (- n 1))))) 1.0))
;;1.46
(define (iterative-improve good-enough? improve)
  (define (check-guess guess)
    (if (good-enough? guess)
        guess
        (check-guess (improve guess))))
  check-guess)

(define (sqrt-m x)
  ((iterative-improve
   (lambda (guess)
     (< (abs (- (square guess) x)) 0.00001))
   (lambda (guess)
     (average guess (/ x guess)))) 1.0))
  
