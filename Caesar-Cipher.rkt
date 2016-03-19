#|
 | Caesar-Cipher.rkt
 | Alex Golub
 | 
 | Encodes a text file using the Caesar Cipher.
|#

#lang racket
(require racket/trace)

; The function takes in two integers and returns their modulus.
(define mod (lambda (num1 num2)
              (cond [(equal? num1 num2) 0]
                    [(< num1 num2) num1]
                    [else (mod (- num1 num2) num2)])))

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

; The function takes in a list and returns an encoded list.
(define encode (lambda (alist key)
                 (cond[(empty? alist) empty]
                      [(> (+ (char->integer (first alist)) (- (char->integer key) 65)) 90)
                       (cons (integer->char (+ 64 (- (+ (char->integer (first alist)) (- (char->integer key) 65)) 90)))
                             (encode (rest alist) key))]
                      [else (cons (integer->char (+ (char->integer (first alist)) (- (char->integer key) 65))) (encode (rest alist) key))])))

; The function takes in a filename and a character key and encodes the file with the given key.
(define encode-file (lambda (file key)
                      (let ([in (open-input-file (string-append (path->string (current-directory)) file))])
                        (list->string (encode (capitalise (string->list (read-all-lines in))) key)))))