#lang racket

(require (only-in math/number-theory factorize))
(provide add-root-of-rationals)

; An rr is plus or minus the square root of a nonnegative rational number.
; An rr can be represented exactly by an rrr.
; An rrr is plus or minus the square of an rr.
; Procedure add-root-of-rationals accepts zero, one or more rrrs
; and returns the exact rrr of the sum of the represented rrs.
; An error is raised when the square of the sum is not a rational number.
; See examples at the end of this module.

(define (add-root-of-rationals . rrrs)
 (cond
  ((null? rrrs) 0)
  ((null? (cdr rrrs)) (car rrrs))
 (else
  (let*
   ((rrrs (remove-opponents rrrs))
    (signs (map (λ (x) (if (positive? x) +1 -1)) rrrs))
    (rrrs (map abs rrrs))
    (numerators (map numerator rrrs))
    (denominators (map denominator rrrs))
    (gcd-numerators (apply gcd numerators))
    (gcd-denominators (apply gcd denominators))
    (numerators (map (λ (x) (/ x gcd-numerators)) numerators))
    (denominators (map (λ (x) (/ x gcd-denominators)) denominators))
    (numerators (map sqrt-factorize numerators))
    (denominators (map sqrt-factorize denominators))
    (terms (map / numerators denominators))
    (sum (apply + (map * signs terms)))
    (sum (* sum (abs sum))))
   (* (/ gcd-numerators gcd-denominators) sum)))))

(define (remove-opponents rrrs)
 (let loop ((r '()) (rrrs rrrs))
  (cond
   ((null? rrrs) r)
   (else
    (define rrr (car rrrs))
    (define -rrr (- rrr))
    (define rop (remove-opponent -rrr (cdr rrrs)))
    (if rop (loop r rop) (loop (cons rrr r) (cdr rrrs)))))))

(define (remove-opponent -x rrrs)
 (let/ec ec
  (let loop ((rrrs rrrs))
   (cond
    ((null? rrrs) (ec #f))
    ((= (car rrrs) -x) (cdr rrrs))
    (else (cons (car rrrs) (loop (cdr rrrs))))))))
        
(define (sqrt-factorize x)
 (define y (factorize x))
 (apply * (map root y)))

(define (root z)
 (define p (car z))
 (define q (cadr z))
 (unless (even? q)
  (error 'root "square expected, found: ~s^~s" p q))
 (expt p (/ q 2)))

#|
(add-root-of-rationals 3 -12) ; --> -3 because √3-√12 = √3(1-√4) = -√3
(add-root-of-rationals 1/3 -1/108 5 -5 -1/5 1/5 -5 5) ; --> 25/108 because:
; √(1/3)-√(1/108) = √(1/3)(1-√(1/36)) = √(1/3)(1-1/6) = √(1/3)(5/6) = √(1/3)√(25/36) = √(25/108)
(add-root-of-rationals -1/3 1/108 5 -5 -1/5 1/5 -5 5) ; --> -25/108
(add-root-of-rationals 1/2 1/2) ; --> 2 because √(1/2)+√(1/2) = 2√(1/2) = √(4/2) = √2
(add-root-of-rationals 1/3 1/3) ; --> 4/3 because √(1/3)+√(1/3) = 2√(1/3) = √(4/3)
(add-root-of-rationals 1/2 1/3) ; --> error: add-root-of-rationals: square expected, found: 3
|#
