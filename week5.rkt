#lang racket

(require srfi/1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define videos
  (quote
   ([(id 1)
     (title "Star Wars")
     (certificate PG)
     (runtime 90)
     (genre scifi)
     (country "USA")]
    
    [(id 2)
     (title "Cars 2")
     (genre comedy)]
    [(id 3)
     (title "Cars 1")
     (genre comedy)
    (country "USA")])))


(define (load-database db videos)
  (for-each
   (lambda (video)
     (hash-set! db (cadar video) video))
   videos))

(define (get-video-data db id field)
  (if (hash? db)
    (let ((record (hash-ref db id)))
      (if (memq field '(title certificate runtime genre country))
        (cadr (assq field record))
        "wrong field type"))
    "not valid hash table"))


(define db (make-hash))

(load-database db videos)

;; (get-video-data db 1 'title)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-train name departs carriages carriage_capacity)
  `((name ,name) 
    (departs ,departs) 
    (carriages ,carriages) 
    (carriage_capacity ,carriage_capacity)))

(define gordon
  (make-train "Gordon" "10:00" 5 20))
(define thomas
  (make-train "Thomas" "09:00" 10 40))
(define edward
  (make-train "Edward" "11:32" 7 25))
(define henry
  (make-train "Henry" "10:07" 2 50))

(define trains (list gordon thomas edward henry))

;; list of train capacities
(define (traincap list-of-trains)
  (map 
   (lambda (train)
     (assq 'carriage_capacity train))
   list-of-trains))

;; total capacity of all trains
(define (sumtraincap list-of-trains)
  (let ((cap (traincap list-of-trains)))
    (fold + 0 (map (lambda (x) (second x)) cap))))
