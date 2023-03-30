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

(define (nestlist l)
  (if (null? (car l))
  (cdr l)
      (if ( or (null? l) (null? (car (car l))))
          '()
          (display (cdr l))
          )
      )
)





