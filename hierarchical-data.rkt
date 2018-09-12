#lang racket
(require "basic.rkt")
(provide append)

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

;;2.17
(define (last-pair l)
  (define (last-pair-rec first rest)
    (if (null? rest)
        (list first)
        (last-pair-rec (car rest) (cdr rest))))
  (last-pair-rec (car l) (cdr l)))
;;2.18
(define (reverse items)
  (let ((rest (cdr items)))
    (if (null? rest)
        items
        (append (reverse rest) (list (car items))))))
;;2.20
(define (same-parity first . rest)
  (define (same-parity-list first l)
    (cond ((null? l) (list first))
          ((or (and (even? first) (even? (car l)))
               (and (not (even? first))(not (even? (car l)))))
           (cons first (same-parity-list (car l) (cdr l))))
          (else (same-parity-list first (cdr l)))))
  (same-parity-list first rest))

;;2.27
(define (deep-reverse items)
  (let ((rest (cdr items)))
    (if (null? rest)
        (if (pair? (car items))
            (list (deep-reverse (car items)))
            items)
        (append (deep-reverse rest)
                (list (if (pair? (car items))
                          (deep-reverse (car items))
                          (car items)))))))
;;2.28
(define (fringe items)
  (cond ((null? items) items)
        ((not (pair? items)) (list items))
        (else (append (fringe (car items))
                      (fringe (cdr items))))))
;;2.29
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))
(define (left-branch mobile)
  (if (pair? mobile)
      (car mobile)
      mobile))
(define (right-branch mobile)
  (if (not (pair? mobile))
      mobile
      (let ((right (cdr mobile)))
        (if (pair? right)
            (car right)
            right))))
(define (total-weight x)
  (cond ((not (pair? x)) 0)
        ((not (pair? (right-branch x))) (right-branch x))
        (else (+ (total-weight (left-branch x))
                 (total-weight (right-branch x))))))
(define (balanced? x)
  (define (is-mobile m)
    (and (pair? (left-branch m))(pair? (right-branch m))))
  (let ((self-balanced (= (* (left-branch (left-branch x))
                             (total-weight (left-branch x)))
                          (* (left-branch (right-branch x))
                             (total-weight (right-branch x)))))
        (left-balanced (if (is-mobile (right-branch (left-branch x)))
                           (balanced? (right-branch (left-branch x)))
                           true))
        (right-balanced (if (is-mobile (right-branch (right-branch x)))
                            (balanced? (right-branch (right-branch x)))
                            true)))
    (and self-balanced left-balanced right-balanced)))
        
(define m1 (make-mobile 
            (make-branch 4 6) 
            (make-branch 5 
                         (make-mobile 
                          (make-branch 3 7) 
                          (make-branch 9 8)))))
(define m2 (make-mobile 
            (make-branch 4 6) 
            (make-branch 2 
                         (make-mobile 
                          (make-branch 5 8) 
                          (make-branch 10 4)))))

;;sequence operations
(define nil '())
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;;2.35
(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x)
                     (if (pair? x)
                         (count-leaves x)
                         1))
                   t)))
;;2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
;;2.40
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
;;2.41
(define (remove item seq)
  (filter (lambda(i)(not (= item i)))
          seq))
(define (permutation s)
  (if (null? s)
      (list nil)
      (flatmap (lambda(i)
                 (map (lambda(j)
                        (cons i j))
                      (permutation (remove i s))))
               s)))

(define (ordered-triples-sum n s) 
  (filter (lambda (items) (= (accumulate + 0 items) s)) 
          (flatmap 
           (lambda (i) 
             (flatmap (lambda (j) 
                        (map (lambda (k) (list i j k)) 
                             (enumerate-interval 1 (- j 1)))) 
                      (enumerate-interval 1 (- i 1)))) 
           (enumerate-interval 1 n)))) 

;;2.42 eight-queens puzzle
(define empty-board nil)
(define (adjoin-position new-row k rest-of-queens)
  (cons (cons new-row k) rest-of-queens))
(define (safe? k positions)
  (let ((k-row (car (car positions))))
    (null? (filter (lambda (p)
              (let ((diff (abs (- (cdr p) k)))
                    (p-row (car p)))
                (or (= k-row p-row)
                    (= k-row (+ p-row diff))
                    (= k-row (- p-row diff)))))
            (cdr positions)))))
  
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))
