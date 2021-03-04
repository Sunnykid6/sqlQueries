#lang racket


(provide (all-defined-out)) ;; so we can put tests in a second file

; part 1
(define nat-num-stream
  (letrec
      ([f (lambda (x)
            (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 0))))

;#1
(define (add-pointwise l1 l2)
  (if (and (list? l1) (list? l2))
      (cond [(> (- (length l1) (length l2)) 0)
             (map + l1 (append l2 (make-list (- (length l1) (length l2)) 0)))]
            [(< (- (length l1) (length l2)) 0)
             (map + (append l1 (make-list (- (length l2) (length l1)) 0)) l2)]
            [(= (-(length l1) (length l2)) 0)
             (map + l1 l2)])
      (error "illegal parameter")))

;#2
(define (add-pointwise-lists l1)
  (if (list? l1)
      (cond [(null? l1)
             null]
            [#t 
             (add-pointwise (car l1) (add-pointwise-lists (cdr l1)))])
      (error "illegal parameter")))

;#3
(define (add-pointwise-lists-2 l1)
  (if (list? l1)
      (foldl add-pointwise '() l1)
      (error "illegal paramer")))

;#4
(define (stream-for-n-steps s n)
  (let ([s1 (s)])
        (if (= n 0)
            null
            (cons (car s1) (stream-for-n-steps (cdr s1) (- n 1))))))

;#5
(define fibo-stream
(letrec ([f (lambda (x y) (cons x (lambda () (f y (+ x y)))))])
  (lambda () (f 0 1))))

;#6
(define (filter-stream f s)
  (letrec ([g (lambda(x) (let ([s1 (x)]) (if (f (car s1))
                                              (cons (car s1) (lambda () (g (cdr s1))))
                                              (g (cdr s1)))))])
    (lambda () (g s))))

;#7
(define palyndromic-numbers
  (filter-stream (lambda (x) (equal? (reverse (string->list(number->string x))) (string->list(number->string x)))) nat-num-stream))

;#8 macro create-stream
(define-syntax create-stream
  (syntax-rules (using starting at with increment)
    [(create-stream e1 using e2 starting at e3 with increment e4)
     (define e1 (letrec ([f (lambda (x) (cons (e2 x) (lambda () (f (+ x e4)))))])
                  (lambda () (f e3))))]))
; part 2

;#1
(define (vector-assoc v vec)
  (letrec ([f (lambda (x) (if (< x (vector-length vec))
                              (if (and (pair? (vector-ref vec x)) (equal? v (car (vector-ref vec x))))
                                  (vector-ref vec x)
                                  (f (+ x 1)))
                              #f))])
    (f 0)))

;#2
(define (cached-assoc xs n)
  (letrec ([cache (make-vector n #f)]
           [i 0])
    (lambda (x)
      (let ([ans (vector-assoc x cache)])
        (if ans
            (cdr ans)
            (let ([new-ans (assoc x xs)])
              (begin (vector-set! cache i (cons x new-ans)) (set! i (modulo (+ i 1) n)) new-ans)))))))
