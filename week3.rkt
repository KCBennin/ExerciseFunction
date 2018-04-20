#lang racket
(define balance 100)
(define (withdraw amount) (if (>= balance amount)
(begin (set! balance (- balance amount)) balance)
"Insufficient funds"))

;; (withdraw 1000) -> "Insufficient funds"
;; (withdraw 10) -> 90
;; (withdraw 10) -> 80



(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
(begin (set! balance (- balance amount))
       balance)
"Insufficient funds"))
  
  (define (deposit amount)
(set! balance (+ balance amount))
balance)
  
(define (dispatch m n)
(cond ((eq? m withdraw) (withdraw n))
      ((eq? m deposit) (deposit n))
      (else
(error "Unknown request -- MAKE-ACCOUNT" m))))
  dispatch)
;; (define myacc (make-account 100)) -> returns a function
;; (myacc 'withdraw 10) -> 90
;; (myacc 'deposit 10) -> 100




