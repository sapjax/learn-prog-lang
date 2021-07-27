
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below
;;Problems 1
(define (sequence low high stride)
  	(cond 
	  	[(> low high) '()]
		[(= low high) (list low)]
		[#t (cons low (sequence (+ low stride) high stride))]))

;;Problems 2
(define (string-append-map xs suffix)
	(map (lambda (x) (string-append x suffix)) xs))

;;Problems 3
(define	(list-nth-mod xs n)
	(cond
		[(< n 0) (error "list-nth-mod: negative number")]
		[(null? xs) (error "list-nth-mod: empty list")]
		[#t (car (list-tail xs (remainder n (length xs))))]))
              
;;Problems 4
(define (stream-for-n-steps s n)
	(let ([v (s)])
		(cond
			[(= 0 n) '()]
			[#t (cons (car v) (stream-for-n-steps (cdr v) (- n 1)))]
		)))

;;Problems 5
(define funny-number-stream
	(letrec ([f (lambda (x) (cons x (lambda () (f 
		(cond
			[(= 0 (remainder (+ 1 x) 5)) (- 0 (+ 1 x))]
			[(< x 0) (+ (- 0 x) 1)]
			[#t (+ 1 x)]
		)))))])
	(lambda () (f 1))))

;;Problems 6`
(define dan-then-dog
	(letrec ([f (lambda(x) (cons x (lambda () (f 
		(if (equal? x "dan.jpg") "dog.jpg" "dan.jpg")))))])
	(lambda () (f "dan.jpg"))))

;;Problems 7
(define (stream-add-zero s)
	(letrec ([f (lambda (x) 
		(cons (cons 0 (car x)) (lambda () (f ((cdr x))))))])
	(lambda () (f (s)))))

;;Problems 8
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
        (cons (cons (list-nth-mod xs n) (list-nth-mod ys n))
			(lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

;;Problems 9
(define (vector-assoc v vec)
    (letrec ([f (lambda (i) (cond 
		[(equal? (vector-length vec) i ) #f]
		[(pair? (vector-ref vec i)) (if 
            (equal? v (car (vector-ref vec i))) 
			(vector-ref vec i)
			(f (+ i 1)))]
		[#t (f (+ i 1))]))])
    (f 0)))


;;Problems 10 
