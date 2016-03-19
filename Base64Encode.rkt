#|
 | Base64Encode.rkt
 | Alex Golub
 | 
 | Encodes a string into a 64 base character string, a 64 base character is only 6 bits long.
|#

#lang racket
(require racket/trace)

; Sample encode
; (base64encode "Dog")
; "RG9n"


; The function take in two integers and raises the first integer to the power of the second.
(define exp (lambda (x n)
              (cond [(< n 0) 'error]
                    [(equal? n 0) 1]
                    [(equal? n 1) x]
                    [else (* x (exp x (- n 1)))])))

; The function takes in a list and returns it's length.
(define length (lambda (alist)
                 (cond [(empty? alist) 0]
                       [(equal? alist 0) 1]
                       [ else (+ 1 (length (rest alist)))])))

; The function takes in an integer and returns the number of digits
(define lengthOfDigits (lambda (n)
                         (if (equal? (- n (mod n 10)) 0)
                             1
                             (+ 1 (lengthOfDigits (/ (- n (mod n 10)) 10))))))

; The function takes in an integer and returns the first digit
(define firstDigit (lambda (n)
                     (/ (- n (mod n (exp 10 (- (lengthOfDigits n) 1)))) (exp 10 (- (lengthOfDigits n) 1)))))


; The function takes in an element and a list and appends the element to the given list.
(define append (lambda(elem alist)
                 (if (empty? alist)
                     (list elem)
                     (cons (first alist) (append elem (rest alist))))))

; The function takes in a radix and a list of integers and returns an integer which is the decimal value of the converted number.
(define toDecimal (lambda (radix digits)
                    (cond [(empty? digits) 0]
                          [else (+ (* (first digits) (exp radix (- (length digits) 1))) (toDecimal radix (rest digits)))])))

; The function takes in two integers and returns their modulus.
(define mod (lambda (num1 num2)
              (cond [(equal? num1 num2) 0]
                    [(< num1 num2) num1]
                    [else (mod (- num1 num2) num2)])))

; The function takes in a radix and an integer and returns a list of digits converted to the new radix.
(define fromDecimal (lambda (radix number)
                      (cond [(> radix number) (list number)]
                            [else (append (mod number radix) (fromDecimal radix (/ (- number (mod number radix)) radix)))])))

; The function takes in two radixes and a list of digits then returns a new converted list of digits.
(define radix-convert (lambda (radix1 radix2 digits)
                        (fromDecimal radix2 (toDecimal radix1 digits))))

; The function takes in an integer n and a list, then returns the first n elements
(define firstn (lambda (n alist)
                 (cond [(empty? alist) empty]
                       [(>= n 1) (cons (first alist) (firstn (- n 1) (rest alist)))]
                       [else empty])))

; The function takes in an integer n and a list, then returns a list without the first n elements
(define restn (lambda (n alist)
                (cond [(empty? alist) empty]
                      [(> n 1) (restn (- n 1) (rest alist))]
                      [else (rest alist)])))

; The function takes in an integer 0 <= n <= 63 and returns a Base64 character
(define base64char (lambda (n)
                     (cond [(or (< n 0) (> n 63)) 'error]
                           [(or (<= n 0) (<= n 25)) (integer->char (+ 65 n))]
                           [(or (<= n 26) (<= n 51)) (integer->char (+ 71 n))]
                           [(or (<= n 52) (<= n 61)) (integer->char (- n 4))]
                           [(equal? n 62) (integer->char (43))]
                           [(equal? n 63) (integer->char (47))])))

; the function takes in a list of integers in the range [0, 63], and converts it to a list of Base64 characters.
(define base64codes->base64chars (lambda (alist)
                                   (cond [(empty? alist) empty]
                                         [else (cons (base64char (first alist)) (base64codes->base64chars (rest alist)))])))

; The function takes in a list of characters and returns a list of integers codes.
(define asciichars->asciicodes (lambda (alist)
                                 (cond [(empty? alist) empty]
                                         [else (cons (char->integer (first alist)) (asciichars->asciicodes (rest alist)))])))

; The function takes in a string of characters and encodes it to a string of characters based on BASE64 encoding.
(define base64encode (lambda (str)
                       (cond [(equal? str "") ""]
                             [else (string-append (list->string(base64codes->base64chars
                                                 (radix-convert 256 64 (asciichars->asciicodes (firstn 3 (string->list str))))))
                                                 (base64encode (list->string (restn 3 (string->list str)))))])))