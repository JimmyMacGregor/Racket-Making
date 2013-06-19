#lang racket
;translate order-of-magnitude value to multiple of 10
(define (oom x)
  (letrec ([f (lambda (x acc)
              (if (<= x 0)
                  acc
                  (f (- x 1) (* acc 10))))])
    (f x 1)))

;run over a list, subtracting (previous head times ten) from (this head)
(define (subtract-previous xs)
  (letrec ([f (lambda (hd ys acc)
                (if (null? ys)
                    acc
                    (f (car ys) (cdr ys) (cons (- (car ys) (* hd 10)) acc))))])
    (f (car xs) (cdr xs) (list (car xs)))))

;uses (oom) and (subtract-previous) to help change a list into a sequence of digits
(define (number->list x)
  (letrec ([f (lambda (goal count acc accn)
                (if (> count goal)
                    (reverse (subtract-previous acc))
                    (let ([n (truncate (/ x (oom count)))])
                      (if (null? acc)
                          (f goal (+ count 1) (cons n acc) n)
                          (f goal (+ count 1) (cons n acc) n))))
                    )])
    (f (order-of-magnitude x) 0 null 0)))