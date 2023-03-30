#lang scheme

(define x '( (1) (0 1 0 1) () (0 1)))
(define y '( (1 -1) (1 2 3) () (3)))
(define z '( (-1 1) (-1 2) (3)))


(define (nestedlist2 list1 list2 func)
  (let* ((max-length (max (length list1) (length list2)))
         (padded-list1 (pad-list list1 max-length))
         (padded-list2 (pad-list list2 max-length)))
    (letrec ((helper (lambda (l1 l2 result)
                       (cond ((or (null? l1) (null? l2)) (rev result))
                             ((or (null? (car l1)) (null? (car l2)))
                              (helper (cdr l1) (cdr l2) result))
                             (else (helper (cdr l1) (cdr l2)
                                           (cons (func (car l1) (car l2)) result)))))))
      (helper padded-list1 padded-list2 '()))))

(define (pad-list lst n)
  (cond ((>= (length lst) n) lst)
        (else (pad-list (cons '() lst) n))))
The pad-list function takes a list and a desired length n and pads the list with empty sublists until its length is equal to n. The nestedlist2 function uses pad-list to create new lists padded-list1 and padded-list2 that are padded to the maximum length of the two




Zane Lesley
If one list is empty, and the other isnt, i want to add another '() so we can get the last digit from the other list, how to fix

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
                           ((null? l1) (helper '() (cdr l2) (cons (car l2) result)))
                           ((null? l2) (helper (cdr l1) '() (cons (car l1) result)))
                           ((or (null? (car l1)) (null? (car l2)))
                            (helper (cdr l1) (cdr l2) result))
                           (else (helper (cdr l1) (cdr l2)
                                         (cons (func (car l1) (car l2)) result)))))))
    (helper list1 list2 '())))
To add an extra '() to the result if one list is empty and the other isn't, you can add an extra condition in the helper function for when both lists are non-empty, but one is shorter than the other. You can pad the shorter list with '() until it is the same length as the longer list. Here's an updated version of the add and nestedlist2 functions that does this:

scss
Copy code
(define (add a b)
  (letrec ((helper (lambda (a b result)
                     (cond ((and (null? a) (null? b)) result)
                           ((null? a) (helper '() (cdr b) (cons (car b) result)))
                           ((null? b) (helper (cdr a) '() (cons (car a) result)))
                           ((< (length a) (length b))
                            (helper (cons '() a) b result))
                           ((< (length b) (length a))
                            (helper a (cons '() b) result))
                           (else (helper (cdr a) (cdr b)
                                         (cons (+ (car a) (car b)) result)))))))
    (rev (helper a b '()))))

(define (rev l)
  (letrec ((helper (lambda (l result)
                     (if (null? l) result
                         (helper (cdr l) (cons (car l) result))))))
    (helper l '())))

(define (nestedlist2 list1 list2 func)
  (letrec ((helper (lambda (l1 l2 result)
                     (cond ((and (null? l1) (null? l2)) (rev result))
                           ((null? l1) (helper '() (cdr l2) (cons (car l2) result)))
                           ((null? l2) (helper (cdr l1) '() (cons (car l1) result)))
                           ((or (null? (car l1)) (null? (car l2)))
                            (helper (cdr l1) (cdr l2) result))
                           (else (helper (cdr l1) (cdr l2)
                                         (cons (func (car l1) (car l2)) result)))))))
    (helper (if (< (length list1) (length list2))
                (append list1 (make-list (- (length list2) (length list1)) '()))
                list1)
            (if (< (length list2) (length list1))
                (append list2 (make-list (- (length list1) (length list2)) '()))
                list2)
            '())))