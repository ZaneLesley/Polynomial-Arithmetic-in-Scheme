#lang scheme

(define x '( (1) (0 1 0 1) () (0 1)))
(define y '( (1 -1) (1 2 3) () (3)))
(define z '( (-1 1) (-1 2) (3)))


(define (add a b)
  (letrec ((helper (lambda (a b result)
                     (if (and (null? a) (null? b))
                         result
                         (if (null? a)
                             (helper a (cdr b) (cons (car b) result))
                             (if (null? b)
                                 (helper (cdr a) b (cons (car a) result))
                                 (helper (cdr a) (cdr b) (cons (+ (car a) (car b)) result))))))))
    (rev (helper a b '()))))

(define (rev l)
(letrec
    ((helper (lambda (l result)
               (if (null? l) result
                   (helper (cdr l) (cons (car l) result))))))
  (helper l '())
  ))

(define (nestedlist2 list1 list2 func)
  (letrec ((helper (lambda (l1 l2 result)
                     (cond ((and (null? l1) (null? l2)) (rev result))
                           ((null? l1) (helper l1 (cdr l2) (cons (car l2) result)))
                           ((null? l2) (helper (cdr l1) l2 (cons (car l1) result)))
                           ((or (null? (car l1)) (null? (car l2)))
                            (helper (cdr l1) (cdr l2) result))
                           (else (helper (cdr l1) (cdr l2)
                                         (cons (func (car l1) (car l2)) result)))))))
    (helper list1 list2 '())))
