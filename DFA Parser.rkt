#|
 | DFA Parser.rkt
 | Alex Golub
 | 
 | Takes in a DFA and a string of input symbols and tells you if they are accpted.
|#

#lang racket
(require racket/trace)

; Define some DFA and inputs.
(define DFA1 '((q0 q1) (0 1) ((q0 0 q0) (q0 1 q1) (q1 0 q1) (q1 1 q0)) q0 (q1)))

(define Input1_1 '(0 0 1 1 0 1))

(define Input1_2 '(1 0 1 1 0 1))

(define Input1_3 '(0 1 1 0 0))

(define Input1_4 '(0 1 2 1 0 1))

; The function takes in a list and returns it's length.
(define length (lambda (alist)
                 (cond [(empty? alist) 0]
                       [(equal? alist 0) 1]
                       [ else (+ 1 (length (rest alist)))])))

; The function takes in a list and two integers (size and index), and returns a spacified element of the lsit.
(define get-element (lambda (alist size index)
                      (cond [(or (<= index 0) (> index size)) 'error]
                            [(equal? (- (+ 1 size) index) (length alist)) (first alist)]
                            [else (get-element (rest alist) size index)])))

; The function takes in a state, input, and a list of state transitons, then returns a new state.
(define get-new-state (lambda (state input transition)
                        (cond [(empty? transition) 'error]
                              [(and (equal? state (first (first transition)))
                                    (equal? input (get-element (first transition) 3 2))) (get-element (first transition) 3 3)]
                              [else (get-new-state state input (rest transition))])))

; The function takes in a list and checks wheather the input is not transitive.
(define is-transitive (lambda (alist)
                        (cond [(empty? alist) empty]
                              [(equal? (first alist) 'error) 'rejected-no-transition-for-input]
                              [else (is-transitive (rest alist))])))

; The function takes in an initial state with transition function and an input, and walks toward the last input character.
(define walk (lambda (state transition input)
               (cond [(empty? input) empty]
                     [(equal? (length input) 1) (cons state (list (get-new-state state (first input) transition)))]
                     [else (append (list state) (walk (get-new-state state (first input) transition) transition (rest input)))])))

; The function takes in a DFA and an input list, and returns wheather the input was accepted or rejected.
(define parse (lambda (DFA input)
                (cond [(equal? (list (get-element (walk (get-element DFA 5 4) (get-element DFA 5 3) input)
                                            (+ 1 (length input)) (+ 1 (length input)))) (get-element DFA 5 5)) 'accepted]
                      [(equal? (is-transitive (walk (get-element DFA 5 4) (get-element DFA 5 3) input)) 'rejected-no-transition-for-input)
                       'rejected-no-transition-for-input]
                      [(not (equal? (list (get-element (walk (get-element DFA 5 4) (get-element DFA 5 3) input)
                                            (+ 1 (length input)) (+ 1 (length input)))) (get-element DFA 5 5))) 'rejected-not-final-state])))