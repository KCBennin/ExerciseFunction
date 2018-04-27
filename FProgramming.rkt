#lang racket
(require srfi/1)
(require srfi/13)


(define descriptions '((1 "You are on the Plains")
                       (2 "You have entered the Werewood")
                       (3 "You are in the Lowlands")
                       (4 "You have entered Dwarf Kingdom")
                       (5 "You are in the Westlands")
                       (6 "You are in the Swampland")
                       (7 "You have entered the Elf Forest")
                       (8 "You are on the Mountain")
                       (9 "You are on Dragon Stone")
                       (10 "You have entered the Void")
                       (11 "You are at Valleys End!")))


(define objects      '((1 "Rope, a Bag of food")
                       (2 "A Sword, a Flower")
                       (3 "A Hammer, a Spear, an Axe, a Bow and Arrow")
                       (3 " Throwing Stars")
                       (4 "Plant, Disguise Potion")
                       (5 "Elf Sword, Dragon Sword")
                       (6 "Strength Potion, Speed Potion")
                       (7 "Invisibilty Potion, Sleeping Potion, Anger Potion")
                       (8 "A Lantern, Food Bag, Water" ) 
                       (9 "Magic eye glass, Map")
                       (10 "Flute, Harp, a Singing Fairy")
                       (10 "A Falcon, a Unicorn, a Sparrow, an Eagle")
                       (11 "Magic Wand")))


(define opponents '((10 "A Dragon")
                   (7 "A Monster")
                   (4 "Giant Spider")
                   (11 "The Wizard")))

(define partners '((4 "Sword Man")
                  (7 "An Elf Princess")))

;; Game Actions and Options
(define look '(((directions) look) ((look) look) ((examine room) look)))
(define fight '(((attack) fight) ((battle) fight) ((destroy) destroy)))
(define run '(((go) run) ((speed) run) ((go now) run)))
(define pick '(((get) pick) ((pick it up) pick) ((take) pick)))
(define climb '(((climb) climb) ((go up) climb) ((upwards) climb)))
(define partner '(((partner) partner) ((friend) partner) ((back up) partner)))
(define put '(((put) drop) ((drop) drop) ((place) drop) ((remove) drop)))
(define arsenal '(((armed) arsenal) ((weapon) arsenal) ((arm up) arsenal)))

(define quit '(((exit game) quit) ((quit game) quit) ((exit) quit) ((quit) quit)))
(define actions `(,@look ,@quit ,@run ,@fight ,@climb ,@partner
                         ,@pick ,@put ,@arsenal))


;; Direction Options

(define decisiontable `((1 ((west) 2) ,@actions)
                        (2 ((south) 10) ((north) 4) ((west) 3)
                           ((east) 1) ,@actions)
                        (3 ((west) 2) ((east) 5) ,@actions)
                        (4 ((north west) 6) ((north east) 7)
                           ((south) 2) ((south east) 5) ,@actions)
                        (5 ((north east) 8) ((south east) 9)
                           ((west) 3) ((north) 4) ((south) 10) ,@actions)
                        (6 ((south) 4) ,@actions)
                        (7 ((south) 4) ,@actions)
                        (8 ((west) 5) ,@actions)
                        (9 ((west) 5) ,@actions)
                        (10 ((east) 11) ((north) 2) ((south) 12)
                            ((north east) 5) ,@actions)
                        (11 ((east) 10) ,@actions)
                        (12 ((north) 11) ,@actions)))
               

;; hash tables 
(define objectdb (make-hash))
(define partnerdb (make-hash))
(define opponentdb (make-hash))
(define inventorydb (make-hash))

(define (add-object db id object)
  (if (hash-has-key? db id)
    (let ((record (hash-ref db id)))
      (hash-set! db id (cons object record)))
        (hash-set! db id (cons object empty))))

(define (add-partner db id partner)
  (if (hash-has-key? db id)
      (let ((record (hash-ref db id)))
        (hash-set! db id (cons partner record)))
        (hash-set! db id (cons partner empty))))

(define (add-opponent db id opponent)
  (if (hash-has-key? db id)
      (let ((record (hash-ref db id)))
        (hash-set! db id (cons opponent record)))
        (hash-set! db id (cons opponent empty))))

;; for each function to add items from assoc list to object
(define (add-objects db)
 (for-each
(lambda (r)  
(add-object db (first r) (second r))) objects))

(define (add-partners db)
  (for-each
(lambda (r)
(add-partner db (first r) (second r))) partners))

;;
(define (add-opponents db)
  (for-each
    (lambda (r)
(add-opponent db (first r) (second r))) opponents))


(add-partners partnerdb)
(add-opponents opponentdb)
(add-objects objectdb)

;;displays room or inventory items
(define (display-objects db id)
(when (hash-has-key? db id)
  (let* ((record (hash-ref db id))
        (output (string-join record " and " )))
    (when (not (equal? output ""))
       (if (eq? id 'bag)
         (printf "You have collected ~a.\n" output)
         (printf "You can choose a ~a.\n" output))))))

(define (remove-object-from-room db id str)
  (when (hash-has-key? db id)
    (let* ((record (hash-ref db id))
          (result (remove (lambda (x) (string-suffix-ci? str x)) record))
          
          (item (lset-difference equal? record result)))
      (cond ((null? item)
          (printf "I don't see that item in the Room!\n"))
         (else
          (printf "Added ~a to your bag.\n" (first item))
          (add-object inventorydb 'bag (first item))
          (hash-set! db id result)))))
(when (hash-has-key? db 'bag)
    (let* ((record (hash-ref db 'bag))
          (result (remove (lambda (x) (string-suffix-ci? str x)) record))
          (item (lset-difference equal? record result)))
       (cond ((null? item)
          (printf "You are not carrying that item!\n"))
        (else
          (printf "Removed ~a from your bag.\n" (first item))
          (add-object objectdb id (first item))
          (hash-set! db 'bag result))))))

(define (display-opponents db id)
        (when (hash-has-key? db id) 
       (let* ((record (hash-ref db id))
      (output (string-join record " and ")))
      (when (not (equal? output ""))
        (if (eq? id 'ene)
            (printf  "You have defeated ~a.\n" output)
            (printf "You have in front of you ~a.\n" output))))))

(define (remove-opponent-from-room db id str)
  (when (hash-has-key? db id)
     (let* ((record (hash-ref db id))
           (result (remove (lambda (x) (string-suffix-ci? str x)) record))
           (item (lset-difference equal? record result)))
        (cond ((null? item)
              (printf "Your opponent is no longer here!\n"))
           (else
             (printf "Vanquished ~a.\n" (first item))
             (hash-set! db id result))))))

(define (pick-item id input)
   (let ((item (string-join (cdr (string-split input)))))
       (remove-object-from-room objectdb id item)))

(define (attack-opponent id input)
    (let ((item (string-join (cdr (string-split input)))))
       (remove-opponent-from-room opponentdb id item)))

(define (put-item id input)
    (let ((item (string-join (cdr (string-split input)))))
       (remove-object-from-room inventorydb id item)))


(define (pick-partner id input)
   (let ((item (string-join (cdr (string-split input)))))
       (remove-object-from-room partnerdb id item)))


(define (now-eat id input)
   (printf "You have eaten a nice meal"))

(define (go-climb id input)
   (printf "You have climbed a great height"))

(define (now-sit id input)
   (printf "You have earned yourself a nice rest"))

(define (display-inventory)
   (display-objects inventorydb 'bag))

(define (slist->string l)
  (string-join (map symbol->string l)))

(define (get-directions id)
   (let ((record (assq id decisiontable)))
   (let* ((result (filter (lambda (n) (number? (second n)))
         (cdr record))) (n (length result)))
  (cond ((= 0 n)
      (printf "You have hit a dead End!\n"))
        ((= 1 n)
      (printf "There is a passage through ~a.\n" (slist->string
   (caar result))))  
       (else
         (let* ((losym (map (lambda (x) (car x)) result))
               (lostr (map (lambda (x) (slist->string x)) losym)))
             (printf "You can exit to the ~a. \n"  
         (string-join lostr  " and "))))))))


;; Option request definition
(define (assq-ref assqlist id) 
  (cdr (assq id assqlist)))

;; Option description
(define (get-description id)
  (car (assq-ref descriptions id)))

(define (get-keywords id)
  (let ((keys (assq-ref decisiontable id)))
 (map (lambda (key) (car key)) keys)))

;; Returns the length of a list.
;; outputs in form of (0 0 0 2 0 0)
(define (list-of-lengths keylist tokens)
 (map
  (lambda(x)
   (let ((set (lset-intersection eq? tokens x)))
     (* (/ (length set) (length x)) (length set))))
       keylist))

;; Return the index of the largest number in a list.
(define (index-of-largest-number list-of-numbers)
  (let ((n (car (sort list-of-numbers >))))
     (if (zero? n)
         #f
         (list-index (lambda (x) (eq? x n)) list-of-numbers))))

(define (lookup id tokens)
  (let* ((record (assq-ref decisiontable id))
 (keylist (get-keywords id))
  (index (index-of-largest-number (list-of-lengths keylist tokens))))
  (if index
      (cadr (list-ref record index))
         #f)))

(define (display-description id)
  (printf "~a.\n" (get-description id)))

;;Game start function definition
(define (startgame initial-id)
   (let loop ((id initial-id) (description #t))
      (when description
        (display-description id)
        (display-objects objectdb id)
        (display-opponents opponentdb id)
         (printf "> "))
     
     (let* ((input (read-line))
    (string-tokens (string-tokenize input))
    (tokens (map string->symbol string-tokens)))
  
  (let ((response (lookup id tokens)))
   (cond ((number? response)
    (loop response #t))
     ((eq? #f response)
      (printf "huh? I didn't understand that!\n")
      (loop id #f))

   ((eq? response 'look)
    (get-directions id)
     (loop id #t))

   ((eq? response 'fight)
    (attack-opponent id input
     (loop id #f))

   ((eq? response 'pick)
   (pick-item id input)
   (loop id #f))

   ((eq? response 'climb)
   (put-item id input)
   (loop id #f))

   ((eq? response 'partner)
   (pick-partner id input)
   (loop id #f))

   (define (now-eat id input)
   (printf "You have eaten a good meal"))

   (define (go-climb id input)
   (printf "You are almost at the top"))

   (define (now-sit id input)
   (printf "You have earned a long rest"))

   (define (now-run id input)
   (printf "Run faster"))

   ((eq? response 'inventory)
   (display-inventory)
   (loop id #f))

   (eq? response 'quit)
   (printf "Thank for playing...\n")
   (exit)))))))
  
  (startgame 1)




