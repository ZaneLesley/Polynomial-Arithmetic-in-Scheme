#lang scheme

(define x '( (1) (0 1 0 1) () (0 1)))
(define y '( (1 -1) (1 2 3) () (3)))
(define z '( (-1 1) (-1 2) (3)))


(define (add a b)
  (letrec ((helper (lambda (a b result)

                     ;base case
                     (if (and (null? a) (null? b))
                         result

                         ;first list empty, add element from second list
                         (if (null? a)
                             (helper a (cdr b) (cons (car b) result))
                             ;second list empty, add element from first list
                             (if (null? b)
                                 ;add element from list 1 and 2, combine results
                                 (helper (cdr a) b (cons (car a) result))
                                 (helper (cdr a) (cdr b) (cons (+ (car a) (car b)) result))))))))
    (rev (helper a b '()))))

;Reverses list
(define (rev l)
(letrec
    ((helper (lambda (l result)
               (if (null? l) result
                   (helper (cdr l) (cons (car l) result))))))
  (helper l '())
  ))


(define (nestedlist2 list1 list2 func)
  (letrec ((helper (lambda (l1 l2 result)
                     ;base case, rev if empty
                     (cond ((and (null? l1) (null? l2)) (rev result))
                           ;if first list empty, add elements from second list
                           ((null? l1) (helper l1 (cdr l2) (cons (car l2) result)))
                           ;if second list empty, add elements from first list
                           ((null? l2) (helper (cdr l1) l2 (cons (car l1) result)))
                           ;both list have elements, combine, recurse
                           (else (helper (cdr l1) (cdr l2)
                                         (cons (func (car l1) (car l2)) result)))))))
    (helper list1 list2 '())))

;negates a list
(define (negate lst)
  ;base condition, if empty return
  (cond ((null? lst) '())
        ;if we still have a list, call function again, but with the sublist
        ;after recurssion, put them together
        ((list? (car lst))
         (cons (negate (car lst))
               (negate (cdr lst))))
        (else
         ;if not a list, multiply element by -1, then reform the sublist
         (cons (* -1 (car lst))
               (negate (cdr lst))))))


(define (smup a l)
	(if (null? l)
	'()
	(cons (* a (car l)) (smup a (cdr l)))
   )
)

(define (mtup list1 list2)
	(if (null? list1)
	'()
	( (smup (car list1) list2)
	(cons 0 (mtup (cdr list1) list2))
)
)
)

(define (alist list1 list2)
  (cond
    ((equal? list1 '()) list2)
  ((equal? list2 '()) list1)
  (else (cons (+ (car list1) (car list2))
              (alist (cdr list1) (cdr list2))))))
