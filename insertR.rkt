#lang racket
(define insertR (lambda (new old lat)
(cond
((null? lat) (quote ())) (else
(cond
((eq? (car lat) old) (cdr lat)) (else (cons (car lat)
(insertR new old (cdr lat)))))))))

(insertR '(2 3 5 6) '(2 45) '(450))