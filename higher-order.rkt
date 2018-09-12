#lang racket
(require "prime.rkt")

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))
(define (inc a) (+ a 1))
(define (identify a) a)
(define (cube a) (* a a a))
;;-------------------------------------------------------
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2)) add-dx b) dx))
;;1.29
(define (simpson-rule f a b n)
  (define (g k)
    (cond ((or (= k 0) (= k n)) 1)
          ((even? k) 2)
          (else 4)))
  (define (h k)
    (* (f (+ a (* k (/ (- b a) n)))) (g k)))
  (* (/ (/ (- b a) n) 3)
     (sum h 0 inc n)))
;;sum iterative
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))
;;product
(define (product term a next b)
  (if (> a b)
      1
      (* a (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (appro-pi n)
  (define (term k)
    (cond ((even? k)(/ (+ 2 k)(+ 1 k)))
          (else (/ (+ 1 k)(+ 2 k)))))
  (* 4.0 (product-iter term 1 inc n)))
;;accumulate
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (accumulate combiner null-value term (next a) next b)(term a)))) 

(define (sum-1 term a next b)
  (define (combiner result a)
    (+ result a))
  (accumulate combiner 0 term a next b))
;;accumulate + filter
(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter a) (iter (next a) (combiner result (term a))))
          (else (iter (next a) result))))
  (iter a null-value))

(define (prime-square-sum a b)
  (define (combiner result a)
    (+ result a))
  (define (term a)
    (* a a))
  (filtered-accumulate prime? combiner 0 term a inc b))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(define (product-prime n)
  (filtered-accumulate (lambda (i)(= 1 (gcd n i)))
                       (lambda (a b) (* a b))
                       1 identify 1 inc n))
