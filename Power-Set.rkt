#|
 | Power-Set.rkt
 | Alex Golub
 | 
 | Given a set the program will return it's power set.
|#

#lang racket
(require racket/trace)

; Define trivial sets.
(define a '((1 2) (4 5 6) (7 8)))
(define b '(1 2 3))
(define myset '(3 5 1 9 7))

; The function takes in an element and a set, and than returns a boolean to confirm weather the element is in the set.
(define member? (lambda (element set)
                  (cond [(empty? set) #f]
                        [(equal? (first set) element) #t]
                        [else (member? element (rest set))])))

; The function takes in 2 sets and returns their union.
(define union (lambda (seta setb)
                (cond [(empty? setb) seta]
                      [(member? (first setb) seta) (union seta (rest setb))]
                      [else (cons (first setb) (union seta (rest setb)))])))

; The function adds an element to a set.
(define add (lambda (element set)
              (if (member element set)
                  set
                  (cons element set))))

; The function takes in a set and returns the number of elements.
(define size (lambda (set)
               (if (empty? set)
                   0
                   (+ 1 (size (rest set))))))

; The function takes in an element and a set of sets, and than adds the element to all the sets in a set.
(define addtoallsets (lambda (element set)
                   (if (empty? set)
                       empty
                       (union (addtoallsets element (rest set)) (list (add element (first set)))))))

; The function takes in a set and returns its power set.
(define powerset (lambda (set)
                   (cond [(empty? set) (add empty set)]
                         [else ( union (union (powerset (rest set)) (list (list (first set))))
                                       (addtoallsets (first set) (union (powerset (rest set)) (list (list (first set))))))])))