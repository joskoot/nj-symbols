#lang racket

; Exact implementation of the Racah formula C.21 in appendix C of
; Quantum Mechanics, Volume II, Albert Messiah,
; North-Holland publishing company.
; Documentation can be made with 3j-symbol.scrbl

(require (only-in math/number-theory factorial) "add-root-of-rationals.rkt")

(provide exact-3j-symbol 3j-symbol
         exact-6j-symbol 6j-symbol
         exact-9j-symbol 9j-symbol
         exact-multiple-of_1/2?
         nonnegative-exact-multiple-of_1/2?)

;----------------------------------------------------------------------------------------------------

(define (exact-3j-symbol j1 j2 j3 m1 m2 m3 (raise-error? #f))
 
 (exact-nonnegative-multiple-of_1/2_check exact-3j-symbol j1)
 (exact-nonnegative-multiple-of_1/2_check exact-3j-symbol j2)
 (exact-nonnegative-multiple-of_1/2_check exact-3j-symbol j3)
 (exact-multiple-of_1/2_check exact-3j-symbol m1)
 (exact-multiple-of_1/2_check exact-3j-symbol m2)
 (exact-multiple-of_1/2_check exact-3j-symbol m3)
 (jm-check exact-3j-symbol j1 m1)
 (jm-check exact-3j-symbol j2 m2)
 (jm-check exact-3j-symbol j3 m3)
 
 (unless (exact-nonnegative-integer? (+ j1 j2 j3))
  (error 'exact-3j-symbol
   "j1+j2+j3 must be an integer, given ~s, ~s and ~s" j1 j2 j3))

 (define (error-or-0 . x) (if raise-error? (apply error 'exact-3j-symbol x) 0))
 
 (cond
  ((not (zero? (+ m1 m2 m3)))
   (error-or-0 "m1+m2+m3 is not zero, given ~s, ~s and ~s" m1 m2 m3))
  ((not (triangular? j1 j2 j3))
   (error-or-0 "j arguments do not satify triangular rule, given ~s, ~s and ~s" j1 j2 j3))
  (else
   (define minimal-t (max 0 (- j2 j3 m1) (+ j1 (- j3) m2)))
   (define maximal-t (min (+ j1 j2 (- j3)) (- j1 m1) (+ j2 m2)))
   (define sum
    (for/fold ((sum 0)) ((t (in-range minimal-t (add1 maximal-t))))
     ((if (odd? t) - +) sum
      (/ (* (factorial t)
            (factorial (+ j3 (- j2) m1 t))
            (factorial (+ j3 (- j1) (- m2) t))
            (factorial (+ j1 j2 (- j3) (- t)))
            (factorial (- j1 m1 t))
            (factorial (+ j2 m2 (- t))))))))
   (cond
    ((zero? sum) 0)
    (else
     ((if (xor (negative? sum) (odd? (- j1 j2 m3))) - +)
      (* (sqr sum)
         (delta j1 j2 j3)
         (factorial (+ j1 m1)) (factorial (- j1 m1))
         (factorial (+ j2 m2)) (factorial (- j2 m2))
         (factorial (+ j3 m3)) (factorial (- j3 m3)))))))))

;----------------------------------------------------------------------------------------------------

(define (exact-6j-symbol j1 j2 j3 J1 J2 J3 (raise-error? #f))
        
 (define (arg-error . x) (apply error 'exact-6j-symbol x)) 
 (define (error-or-0 . x) (if raise-error? (apply error 'exact-6j-symbol x) 0))
 
 (exact-nonnegative-multiple-of_1/2_check exact-6j-symbol J1)
 (exact-nonnegative-multiple-of_1/2_check exact-6j-symbol J2)
 (exact-nonnegative-multiple-of_1/2_check exact-6j-symbol J3)
 (exact-nonnegative-multiple-of_1/2_check exact-6j-symbol j1)
 (exact-nonnegative-multiple-of_1/2_check exact-6j-symbol j2)
 (exact-nonnegative-multiple-of_1/2_check exact-6j-symbol j3)
 
 (define j1+j2+j3 (+ j1 j2 j3))
 (define j1+J2+J3 (+ j1 J2 J3))
 (define J1+j2+J3 (+ J1 j2 J3))
 (define J1+J2+j3 (+ J1 J2 j3))

 (define j1+j2+J1+J2 (+ j1 j2 J1 J2))
 (define j2+j3+J2+J3 (+ j2 j3 J2 J3))
 (define j1+j3+J1+J3 (+ j1 j3 J1 J3))
 
 (unless (integer? j1+j2+j3) (arg-error "j1+j2+j3 not integer, given: ~s, ~s and ~s" j1 j2 j3))
 (unless (integer? j1+J2+J3) (arg-error "j1+J2+J3 not integer, given: ~s, ~s and ~s" j1 J2 J3))
 (unless (integer? J1+j2+J3) (arg-error "J1+j2+J3 not integer, given: ~s, ~s and ~s" J1 j2 J3))
 (unless (integer? J1+J2+j3) (arg-error "J1+J2+j3 not integer, given: ~s, ~s and ~s" J1 J2 j3))
 
 (unless (triangular? j1 j2 j3)
  (error-or-0 "j1 j2 j3 not triangular, given: ~s, ~s and ~s" j1 j2 j3))
 (unless (triangular? j1 J2 j3)
  (error-or-0 "j1 J2 J3 not triangular, given: ~s, ~s and ~s" j1 J2 J3))
 (unless (triangular? j1 j2 j3)
  (error-or-0 "J1 j2 J3 not triangular, given: ~s, ~s and ~s" J1 j2 J3))
 (unless (triangular? j1 j2 j3)
  (error-or-0 "J1 J2 j3 not triangular, given: ~s, ~s and ~s" J1 J2 j3))
 
 (define minimal-t (max j1+j2+j3 j1+J2+J3 J1+j2+J3 J1+J2+j3))
 (define maximal-t (min (+ j1+j2+J1+J2) (+ j2+j3+J2+J3) (+ j1+j3+J1+J3)))
 
 (define sum
  (for/fold ((s 0)) ((t (in-range minimal-t (add1 maximal-t))))
   ((if (odd? t) - +) s
    (/ (factorial (add1 t))
       (factorial (- t j1+j2+j3))
       (factorial (- t j1+J2+J3))
       (factorial (- t J1+j2+J3))
       (factorial (- t J1+J2+j3))
       (factorial (- j1+j2+J1+J2 t))
       (factorial (- j2+j3+J2+J3 t))
       (factorial (- j1+j3+J1+J3 t))))))
 
 (* sum (abs sum)
    (delta j1 j2 j3)
    (delta j1 J2 J3)
    (delta J1 j2 J3)
    (delta J1 J2 j3)))

;----------------------------------------------------------------------------------------------------

(define (exact-9j-symbol j1 j2 J12 j3 j4 J34 J13 J24 J (raise-error? #f))

 (define (arg-error . x) (apply error 'exact-9j-symbol x)) 
 (define (error-or-0 . x) (if raise-error? (apply error 'exact-9j-symbol x) 0))

 (exact-nonnegative-multiple-of_1/2_check exact-9j-symbol j1)
 (exact-nonnegative-multiple-of_1/2_check exact-9j-symbol j2)
 (exact-nonnegative-multiple-of_1/2_check exact-9j-symbol j3)
 (exact-nonnegative-multiple-of_1/2_check exact-9j-symbol j4)
 (exact-nonnegative-multiple-of_1/2_check exact-9j-symbol J12)
 (exact-nonnegative-multiple-of_1/2_check exact-9j-symbol J34)
 (exact-nonnegative-multiple-of_1/2_check exact-9j-symbol J13)
 (exact-nonnegative-multiple-of_1/2_check exact-9j-symbol J24)
 (exact-nonnegative-multiple-of_1/2_check exact-9j-symbol J)

 (unless (integer? (+ j1 j2 J12))
         (arg-error "j1+j2+J12 not integer, given: ~s, ~s and ~s" j1 j2 J12))
 (unless (integer? (+ j3 j4 J34))
         (arg-error "j3+j4+J34 not integer, given: ~s, ~s and ~s" j3 j4 J34))
 (unless (integer? (+ j1 j3 J13))
         (arg-error "j1+j3+J13 not integer, given: ~s, ~s and ~s" j1 j3 J13))
 (unless (integer? (+ j2 j4 J24))
         (arg-error "j2+j4+J24 not integer, given: ~s, ~s and ~s" j2 j4 J24))
 (unless (integer? (+ J12 J34 J))
         (arg-error "J12+J34+J not integer, given: ~s, ~s and ~s" J12 J34 J))
 (unless (integer? (+ J13 J24 J))
         (arg-error "J13+J24+J not integer, given: ~s, ~s and ~s" J13 J24 J))

 (unless (triangular? j1 j2 J12)
         (error-or-0 "j1 j2 J12 not triangular, given: ~s, ~s and ~s" j1 j2 J12))
 (unless (triangular? j3 j4 J34)
         (error-or-0 "j3 j4 J34 not triangular, given: ~s, ~s and ~s" j3 j4 J34))
 (unless (triangular? j1 j3 J13)
         (error-or-0 "j1 j3 J13 not triangular, given: ~s, ~s and ~s" j1 j3 J13))
 (unless (triangular? j2 j4 J24)
         (error-or-0 "j2 j4 J24 not triangular, given: ~s, ~s and ~s" j2 j4 J24))
 (unless (triangular? J12 J34 J)
         (error-or-0 "J12 J34 J not triangular, given: ~s, ~s and ~s" J12 J34 J))
 (unless (triangular? J13 J24 J)
         (error-or-0 "J13 J24 J not triangular, given: ~s, ~s and ~s" J13 J24 J))

 (define min-g (max (abs (- j1 J  )) (abs (- J34 j2)) (abs (- j3 J24)) (abs (- j2 J34))))
 (define max-g (min      (+ j1 J  )       (+ J34 j2)       (+ j3 J24)       (+ j2 J34)))
 
  (apply add-root-of-rationals
   (for/list ((g (in-range min-g (add1 max-g))))
    ((if (even? (* 2 g)) + -)
     (* (sqr (+ g g 1))
      (exact-6j-symbol j1  j2  J12
                       J34 J   g  )
      (exact-6j-symbol j3  j4  J34
                       j2  g   J24)
      (exact-6j-symbol J13 J24 J
                       g   j1  j3 ))))))

;----------------------------------------------------------------------------------------------------

(define (3j-symbol j1 j2 j3 m1 m2 m3 (raise-error? #f))
 (define x (exact-3j-symbol j1 j2 j3 m1 m2 m3 raise-error?))
 (cond
  ((negative? x) (- (sqrt (- x))))
  ((positive? x) (sqrt x))
  (else 0)))

(define (6j-symbol j1 j2 j3 m1 m2 m3 (raise-error? #f))
 (define x (exact-6j-symbol j1 j2 j3 m1 m2 m3 raise-error?))
 (cond
  ((negative? x) (- (sqrt (- x))))
  ((positive? x) (sqrt x))
  (else 0)))

(define (9j-symbol j1 j2 J12 j3 j4 J34 J13 J24 J (raise-error? #f))
 (define x (exact-9j-symbol j1 j2 J12 j3 j4 J34 J13 J24 J raise-error?))
 (cond
  ((negative? x) (- (sqrt (- x))))
  ((positive? x) (sqrt x))
  (else 0)))
 

;----------------------------------------------------------------------------------------------------

(define (triangular? j1 j2 j3) (<= (abs (- j1 j2)) j3 (+ j1 j2)))

(define (delta j1 j2 j3)
 (/ (* (factorial (+ j1 j2 (- j3)))
       (factorial (+ j2 j3 (- j1)))
       (factorial (+ j3 j1 (- j2))))
    (factorial (+ j1 j2 j3 1))))

(define (exact-multiple-of_1/2? x)
 (and (rational? x) (exact? x) (integer? (* 2 x)))) 

(define (nonnegative-exact-multiple-of_1/2? x)
 (and (exact-multiple-of_1/2? x) (>= x 0)))

(define-syntax-rule (exact-multiple-of_1/2_check proc-name x)
 (unless (exact-multiple-of_1/2? x)
  (error proc-name
   (string-append (symbol->string 'x) " must be exact multiple of ½, give ~s") x)))

(define-syntax-rule (exact-nonnegative-multiple-of_1/2_check proc-name x)
 (unless (nonnegative-exact-multiple-of_1/2? x)
  (error 'proc-name
   (string-append (symbol->string 'x) " must be nonnegative exact multiple of ½, give ~s") x)))

(define-syntax-rule (jm-check proc-name j m)
 (unless (exact-nonnegative-integer? (- j (abs m)))
  (error 'proc-name
   (format "~s-|~s| must be an nonnegative exact integer, given ~s and ~s" 'j 'm j m))))

#|
;----------------------------------------------------------------------------------------------------
; Some tests.

(define max-j 10)

(time
 (for*/fold ((n 0))
  ((j1 (in-range 0 max-j 1/2))
   (j2 (in-range 0 (+ j1 1/2) 1/2))
   (j3 (in-range (abs (- j1 j2)) (add1 (+ j1 j2))))
   (m1 (in-range (- j1) (add1 j1)))
   (m2 (in-range (- j2) (add1 j2))))
  (define m3 (- (+ m1 m2)))
  (cond
   ((<= (abs m3) j3)
    #;(printf "~s " (list j1 j2 j3 m1 m2 m3))
    (define sign*square-of-3j-symbol (exact-3j-symbol j1 j2 j3 m1 m2 m3))
    #;(printf "---> ~s~n" sign*square-of-3j-symbol)
    (add1 n))
   (else n))))

(time
(for*/and ((l1 (in-range 0 10))
           (l2 (in-range 0 10))
           (l3 (in-range (abs (- l1 l2)) (+ l1 l2 1))))
 (define sign*square-of-3j-symbol (exact-3j-symbol l1 l2 l3 0 0 0))
 (cond
  ((odd? (+ l1 l2 l3)) (zero? sign*square-of-3j-symbol))
  (else
   (define p (/ (+ l1 l2 l3) 2))
   (define sign (if (even? p) 1 -1))
   (and
    (= sign*square-of-3j-symbol
     (* sign (delta l1 l2 l3) (sqr (/ (factorial p)
                                      (factorial (- p l1))
                                      (factorial (- p l2))
                                      (factorial (- p l3))))))
    (or
     (zero? sign*square-of-3j-symbol)
     (xor (even? p) (negative? sign*square-of-3j-symbol)))))))

(time
(for*/and ((j (in-range 0 max-j 1/2))
           (J (in-range 0 max-j 1/2))
           (g (in-range (abs (- j J)) (+ j J 1))))
 (define a ((if (odd? (+ j J g)) - +) (/ 1 (+ j j 1) (+ J J 1))))
 (define b (exact-6j-symbol j j 0 J J g))
 (unless (= a b) (printf "~s ~s ~s ~s ~s~n" j J g a b))
 (= a b))))

(exact-9j-symbol  2 3 4
                  1 2 3
                  2 2 3) ; --> 11/61740

;To do: add more tests.
|#

