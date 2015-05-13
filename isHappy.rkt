#lang racket
; Is n a happy number.
(define (isHappy n)
  ; returns i squared.
  (define (square i) (* i i))
  ; returns the last digit in n.  
  (define (getLastDigit n) (modulo n 10))
  ; returns n with the last digit omitted. 
  (define (omitLastDigit n) (truncate (/ n 10)))
  ; returns the square of the last digit in n. 
  (define (squareLastDigit n) (square (getLastDigit n)))
  ; returns the sum of the squares of all digits in n.
  (define (sumSquaresOfDigits n sum)
    (if (< n 1) sum
        (+ sum (sumSquaresOfDigits (omitLastDigit n) (squareLastDigit n)))))
  ; the actual implementation of isHappy.
  (define (isHappyImpl n lastN)
    (if (> n 0) 
        (cond ((= n 16) (and (= lastN 4) (display "Unappy :(")))
              ((= (sumSquaresOfDigits n 0) 1) (display "Happy :)"))
              (else (isHappyImpl (sumSquaresOfDigits n 0) n)))
        (display "n must be greater than 0")))
  
  (isHappyImpl n 0))