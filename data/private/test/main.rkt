#lang racket/base

(require
  rackunit
  data/collection
  data/pvector)

(test-case
 "Basic vector operations"
 (check-equal? (sequence->list (pvector 1 2 3 4)) '(1 2 3 4))
 (check-equal? (sequence->list (conj* (pvector) 1 2 3 4)) '(1 2 3 4))
 (check-equal? (sequence->list (rest (pvector 1 2 3 4))) '(2 3 4))
 (check-equal? (sequence->list (reverse (pvector 1 2 3 4))) '(4 3 2 1)))

(test-case
 "Vector equality"
 (check-equal? (pvector 1 2 3 4) (conj* (pvector) 1 2 3 4)))

(test-case
 "Comprehensions"
 (check-equal?
  (for/pvector ([i (in-range 5)])
    (* i i))
  (pvector 0 1 4 9 16))
 (check-equal?
  (for*/pvector ([i (in-range 1 4)]
                 [j (in-range 1 4)])
    (* i j))
  (pvector 1 2 3
           2 4 6
           3 6 9)))

(test-case
 "make-pvector"
  (check-equal? (make-pvector 0 'a) (pvector))
  (check-equal? (make-pvector 5 'a) (pvector 'a 'a 'a 'a 'a))
  (check-equal? (conj (make-pvector 5 'a) 'b) (pvector 'a 'a 'a 'a 'a 'b))
  (for ([n (in-range 300)])
    (define v (make-pvector n 'a))
    (check-equal? (length v) n)
    (for ([i (in-range n)])
      (check-equal? (nth v i) 'a)
      (check-equal? (nth (set-nth v i 'b) i) 'b)
      (unless (= i 0)
        (check-equal? (nth (set-nth v (sub1 i) 'b) i) 'a))
      (unless (= i (sub1 n))
        (check-equal? (nth (set-nth v (add1 i) 'b) i) 'a)))
    (check-equal? (nth (conj v 'b) n) 'b)))
