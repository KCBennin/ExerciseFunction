#lang racket

(require srfi/1)
(require srfi/13)
(require srfi/48)

(define descriptions '((1 "You are on the Plains")
                       (2 "You are in the Highlands")
                       (3 "You are in the Lowlands")
                       (4 "You are in the Eastlands")
                       (5 "You are in the Westlands")
                       (6 "You are in the Swamp lands")
                       (7 "You have entered the Elf Forest")
                       (8 "You are in the Dwarf Kingdom")
                       (9 "You are on Dragon Stone")
                       (10 "You have entered Tamaray")
                       (11 "You are in the Outlands")
                       (12 "You are in the Skylands!")))


(define weapons      '((1 "A Staff")
                       (2 "A Sword")
                       (2 "A Rope, a Hammer, a Lance, ")
                       (2 "A Hammer")
                       (3 "You")
                       (6 "You are in the swamplands")
                       (7 "You have entered the Elf Forest")
                       (8 "You are in the Dwarf Kingdom")
                       (9 "You are on Dragon Stone")
                       (10 "You have entered Tamaray")
                       (11 "You are in the mountains")
                       (12 "Your quest has ended!")))






(define look '(((directions) look) ((look) look) ((examine room) look)))
(define fight '(((attack) fight) ((battle) fight) ((destroy) destroy)))
(define run '(((go) run) ((speed) run) ((go now) run)))
(define collect '(((pick) collect) ((take) pick) ((pick up) pick)))

(define quit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit)))
(define actions `(,@look ,@quit ,@run ,@collect))

(define decisiontable `((1 ((north) 2) ((north end) 7) ,@actions)
                        (2 ((south) 3) ,@actions)
                        (3 ((east) 4)  ,@actions)
                        (4 ((west) 5) ,@actions)
                        (5 ((western) 9) ,@actions)
                        (6 ,@actions)))


;(define (get-directions id)
;  (let ((record (assq id decisiontable)))
;    (let ((result (filter (lambda (n) (number? (second n))) (cdr record))))
;      (printf "You can see exits to the ")nor
;      (for-each (lambda (direction) (printf "~a " (first direction))) result))
;      (printf "\n")))

(define (slist->string l)
  (string-join (map symbol->string l)))

(define (get-directions id)
  (let ((record (assq id decisiontable)))
    (let* ((result (filter (lambda (n) (number? (second n))) (cdr record)))
           (n (length result)))
      (cond ((= 0 n)
             (printf "You appear to have entered a room with no exits.\n"))
            ((= 1 n)
             (printf "You can see an exit to the ~a.\n" (slist->string (caar result))))
            (else
             (let* ((losym (map (lambda (x) (car x)) result))
                    (lostr (map (lambda (x) (slist->string x)) losym)))
               (printf "You can see exits to the ~a.\n" (string-join lostr " and "))))))))

(define (assq-ref assqlist id)
  (cdr (assq id assqlist)))

(define (assv-ref assqlist id)
  (cdr (assv id assqlist)))

(define (get-response id)
  (car (assq-ref descriptions id)))

(define (get-keywords id)
  (let ((keys (assq-ref decisiontable id)))
    (map (lambda (key) (car key)) keys)))


;; outputs a list in the form: (0 0 0 2 0 0)
(define (list-of-lengths keylist tokens)
  (map 
   (lambda (x)
     (let ((set (lset-intersection eq? tokens x)))
       ;; apply some weighting to the result
       (* (/ (length set) (length x)) (length set))))
   keylist))

(define (index-of-largest-number list-of-numbers)
  (let ((n (car (sort list-of-numbers >))))
    (if (zero? n)
      #f
      (list-index (lambda (x) (eq? x n)) list-of-numbers))))


(define (lookup id tokens)
  (let* ((record (assv-ref decisiontable id))
         (keylist (get-keywords id))
         (index (index-of-largest-number (list-of-lengths keylist tokens))))
    (if index 
      (cadr (list-ref record index))
      #f)))


(define (startgame initial-id)
  (let loop ((id initial-id) (description #t))
    (if description
        (printf "~a\n> " (get-response id))
        (printf "> "))
    (let* ((input (read-line))
           (string-tokens (string-tokenize input))
           (tokens (map string->symbol string-tokens)))
      (let ((response (lookup id tokens)))
        (cond ((number? response)
               (loop response #t))
              ((eq? #f response)
               (format #t "huh? I didn't understand that!\n")
               (loop id #f))
              ((eq? response 'look)
               (get-directions id)
               (loop id #f))
              ((eq? response 'quit)
               (format #t "So Long, and Thanks for All the Fish...\n")
               (exit)))))))

;(startgame 1)
