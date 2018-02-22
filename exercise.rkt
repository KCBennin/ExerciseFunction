#lang racket

(define prod-list (lambda (lst)
                      (if (null? lst)
                          1
                          (* (car lst) (prod-list (cdr lst))))))


(define reduce-list (lambda (lst base f)
                      (if (null? lst)
                          base
                          (f (car lst) (reduce-list (cdr lst) base f)))))


(define sum-list (lambda (lst)
                     (if (null? lst)
                         0
                         (+ (car lst) (sum-list (cdr lst))))))

(define list-length (lambda (lst)
                     (if (null? lst)
                         0
                         (+ 1 (list-length (cdr lst))))))

(define factorial (lambda (n)
          
                    (if (< n 2)
                        1
                        (* n (factorial (- n 1))))))

(define (fib f)
  (if (<= f 2)
      1
      (+ (fib (- f 1))
         (fib (- f 2)))))

(define (countdown c)
  (if (= c 0)
      null
      (begin
        (display c)
        (newline)
        (countdown (- c 1)))))

(define (map m lst)
  (if (null? lst)
      null
      (cons (m (car lst))
            (map m (cdr lst)))))


(define (fact n)
  (if (= 0 n)
      1
      (* n (fact (- n 1)))))

(define (square x) (* x x))

(define (abs x)
  (if (< x 0)
      (- x)
      x))


(define alpha1 '(9 6 7))

(define alpha2 (sort alpha1 <))

(define alpha3 '(10 12 14 13))
(set! alpha3 (sort alpha3 <))

(define eloh (list 1 2 3 4 5))

(define (improved-code q) (* q 2))

(define code-quality 4)

(define (double value) (* 2 value))

(define (apply-twice fn value) (fn (fn value)))

