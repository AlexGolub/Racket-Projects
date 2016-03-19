#|
 | Letter-Count.rkt
 | Alex Golub
 | 
 | In the command line type (file-letter-count "filename.txt") to count the number of each letter of your text file
|#

#lang racket
(require racket/trace)

; The function takes in a list and returns it's length.
(define length (lambda (alist)
                 (cond [(empty? alist) 0]
                       [(equal? alist 0) 1]
                       [ else (+ 1 (length (rest alist)))])))

; The function takes in an element and a list, and removes all of the occurances of the list.
(define removeall (lambda (elm alist)
                    (cond [(empty? alist) empty]
                          [(equal? elm (first alist)) (removeall elm (rest alist))]
                          [else (cons (first alist) (removeall elm (rest alist)))])))

; The function takes in a file and combines every line into a single string.
(define read-all-lines (lambda (input)
                         (let ([line (read-line input 'any)])
                           (if (string? line)
                               (string-append line (read-all-lines input))
                               (string)))))

; The function takes in a list of characters and returns a list with no spaces or symbols with all letters being capitalised.
(define capitalise (lambda (alist)
                     (cond [(empty? alist) empty]
                           [(and (<= (char->integer (first alist)) 90) (>= (char->integer (first alist)) 65))
                            (cons (first alist) (capitalise (rest alist)))]
                           [(and (<= (char->integer (first alist)) 122) (>= (char->integer (first alist)) 97))
                            (cons (integer->char(- (char->integer (first alist)) 32)) (capitalise (rest alist)))]
                           [else (capitalise (rest alist))])))

; The function takes in a letter and a letter occurance list, and returns the number of occurances of that letter.
(define count-of (lambda (letter alist)
                   (cond [(empty? alist) empty]
                         [(equal? letter (first (first alist))) (first (rest (first alist)))]
                         [else (count-of letter (rest alist))])))

; The function combines the the letter occurances of the two lists.
(define combine-count-lists (lambda (alist blist)
                              (cond [(and (equal? (length alist) (length blist)) (empty? alist)) empty]
                                    [(empty? alist) (list blist)]
                                    [(empty? blist) (list alist)]
                                    [(< (char->integer (first (first alist))) (char->integer (first (first blist))))
                                     (append (list (append (list (first (first alist))) (list (count-of (first (first alist)) alist))))
                                             (combine-count-lists (rest alist) blist))]
                                    [(> (char->integer (first (first alist))) (char->integer (first (first blist))))
                                     (append (list (append (list (first (first blist))) (list (count-of (first (first blist)) blist))))
                                             (combine-count-lists alist (rest blist)))]
                                    [(equal? (first (first alist)) (first (first blist)))
                                     (append (list (append (list (first (first alist)))
                                                     (list (+ (count-of (first (first alist)) alist) (count-of (first (first alist)) blist)))))
                                             (combine-count-lists (rest alist) (rest blist)))])))

; The function takes in a list and counts the accurances of a spacific letter.
(define letter-count (lambda (letter alist)
                       (cond [(empty? alist) 0]
                             [(equal? letter (first alist)) (+ 1 (letter-count letter (rest alist)))]
                             [else (letter-count letter (rest alist))])))

; The function takes in a letter and a number of occurance and pairs them into a list.
(define make-pair (lambda (letter n)
                    (append (list letter) (list n))))

; The function builds a letter occurance list
(define build-pair-list (lambda (letter alist)
                          (cond [(> (char->integer letter) 90) empty]
                                [else (append (list (make-pair letter (letter-count letter alist)))
                                              (build-pair-list (integer->char (+ 1 (char->integer letter))) (removeall letter alist)))])))

; The function takes in a text file and counts the number of every letter that occur in the text.
(define file-letter-count (lambda (file)
                            (let ([in (open-input-file (string-append (path->string (current-directory)) file))])
                            (build-pair-list #\A (capitalise (string->list (read-all-lines in )))))))