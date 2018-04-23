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


(define weapons      '((1 "A Staff, a Chair, a Candle")
                       (2 "A Sword")
                       (2 "A Rope, a Ladder, a Lance")
                       (2 "A Hammer, a Spear, an Axe, Arrows")
                       (3 "A Phoenix, a Unicorn")
                       (4 "A Mountain, a High Tower, a Cliff Edge")
                       (5 "Plant, a Tree")
                       (6 "Grow Potion, Strength Potion, Speed Potion")
                       (6 "Invisibilty Potion, Disguise Potion")
                       (6 "Bottles of Water, Food")
                       (7 "Food Supplies")
                       (7 "Fire, a Lantern, a Flame")
                       (7 "A Bonfire")
                       (8 "A Castle, a Barnyard")
                       (8 "An Old Hut, a Shack")
                       (9 "A Tree house")
                       (9 "Arrow men, Spear men")
                       (10 "Sword men, Tree men")
                       (10 "A Falcon, an Owl, a Sparrow")
                       (11 "A flock of Eagles")
                       (11 "A Shark, A Whale,")
                       (12 "A pod of Dolphins")))


(define enemies '((3 "A Dragon")
                  (9 "A Monster")
                  (11 "The Wizard")))


;; Game Actions and Options
(define look '(((directions) look) ((look) look) ((examine room) look)))
(define fight '(((attack) fight) ((battle) fight) ((destroy) destroy)))
(define run '(((go) run) ((speed) run) ((go now) run)))
(define food '(((food) food) ((eat) food) ((food time) food)))
(define climb '(((climb) climb) ((go up) climb) ((upwards) climb)))
(define partner '(((partner) partner) ((friend) partner) ((back up) partner)))
(define light '(((light) light) ((fire) light) ((light up) fire)))
(define arsenal '(((pick) arsenal) ((weapon) arsenal) ((arm up) arsenal)))

(define quit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit)))
(define actions `(,@look ,@quit ,@run ,@fight ,@climb ,@partner
                         ,@light ,@food ,@arsenal))


;; Direction Options
(define decisiontable `((1 ((north) 2) ((north west) 3) ((north east) 4) ,@actions)
                        (2 ((south) 1) ,@actions)
                        (3 ((east) 6) (west 7) ,@actions)
                        (4 ((west) 7) ,@actions)
                        (5 ((north east) 9) ,@actions)
                        (6 ((north west) 7) ,@actions)
                        (7 ((south west) 10) ,@actions)
                        (8 ((south east) 12) ,@actions)
                        (9 ((west) 8)  ,@actions)
                        (10 ((east) 1) ,@actions)
                        (11 ((west) 6) ,@actions)
                        (12 ((south) 11) ,@actions)))



(define arsenaldb (make-hash))
(define fooddb (make-hash))
(define partnerdb (make-hash))
(define lightdb (make-hash))


(define (add-arsenal db id arsenal)
  (if (hash-has-key? db id)
      (let ((record (hash-ref db id)))
        (hash-set! db id (cons arsenal record)))
        (hash-set! db id (cons arsenal empty))))


(define (add-food db id food)
  (if (hash-has-key? db id)
      (let ((record (hash-ref db id)))
        (hash-set! db id (cons food record)))
        (hash-set! db id (cons food empty))))


(define (add-light db id light)
  (if (hash-has-key? db id)
      (let ((record (hash-ref db id)))
        (hash-set! db id (cons light record)))
        (hash-set! db id (cons light empty))))


(define (add-partner db id partner)
  (if (hash-has-key? db id)
      (let ((record (hash-ref db id)))
        (hash-set! db id (cons partner record)))
        (hash-set! db id (cons partner empty))))

;;
(define (add-arsenals db)
 (for-each
(lambda (r)  
(add-arsenal db (first r) (second r))) arsenal))

;;
(define (add-foods db) (for-each
(lambda (r)
(add-food db (first r) (second r))) food))

;;
(define (add-partners db) (for-each
(lambda (r)
(add-partner db (first r) (second r))) partner))

;;
(define (add-lights db) (for-each
(lambda (r)
(add-food db (first r) (second r))) light))



;;
(define (display-arsenals db id)
(when (hash-has-key? db id)
  
;;join string if multiple items

  (let* ((record (hash-ref db id))
(output (string-join record " and " ))) ;;depending on the parameter provide different answer
(when (not (equal? output ""))
(if (eq? id 'store)
         (printf "You are carrying ~a.\n" output)
         (printf "You can see ~a.\n" output))))))
        
                    
                   
;;(define (remove-object-from-room db id str)
  ;;(when (hash-has-key? db id)
;;(let* ((record (hash-ref db id))
;;(result (remove (lambda (x) (string-suffix-ci? str x)) record))
;;item is the difference with the previous lists e.g. the item collected

;;(item (lset-difference equal? record result))) (cond ((null? item)
;;(printf "I don't see that item in the Room!\n")) (else
;;(printf "Added ~a to your bag.\n" (first item)) (add-object inventorydb 'bag (first item)) (hash-set! db id result)))))
;;(when (hash-has-key? db 'bag)
;;(let* ((record (hash-ref db 'bag))
;;(result (remove (lambda (x) (string-suffix-ci? str x)) record))
;;(item (lset-difference equal? record result))) (cond ((null? item)
;;(printf "You are not carrying that item!\n")) (else
;;(printf "Removed ~a from your bag.\n" (first item)) (add-object objectdb id (first item))
;;(hash-set! db 'bag result))))))




;;(add-enemies enemydb) (add-objects objectdb)
;;this function can be called to check the items in the room or ;;items available in the inventory
;;(define (display-objects db id)
;;(when (hash-has-key? db id)
;;join string if multiple items (let* ((record (hash-ref db id))
;;(output (string-join record " and " ))) ;;depending on the parameter provide different answer
;;(when (not (equal? output ""))
;;(if (eq? id (printf (printf




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

(startgame 1)
