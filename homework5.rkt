#lang scheme

(define (poly_mul a l)
  (if (or (null? a) (null? l))
  '()
  (cons (* (car a) (car l)) (poly_mul (cdr a) (cdr l)))
  )
)

;; need to get rid of nested loop
(define (test a l)
  (if (or (null? a) (null? l))
      (display 0)
      (display 1)
     )
  )

