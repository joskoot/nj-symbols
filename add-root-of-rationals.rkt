#lang racket

(require (only-in math/number-theory factorize))
(provide add-roots-of-rationals)

; An rr is plus or minus the square root of a nonnegative rational number.
; An rr can be represented exactly by an rrr.
; An rrr is plus or minus the square of an rr.
; Procedure add-root-of-rationals accepts zero, one or more rrrs
; and returns the exact rrr of the sum of the represented rrs.
; An error is raised when the square of the sum is not a rational number.
; See examples at the end of this module.

;----------------------------------------------------------------------------------------------------

(define (add-roots-of-rationals . rrrs)
 (let ((rrrs (read-hash (combine-equals rrrs))))
  (cond
   ((null? rrrs) 0)
   ((null? (cdr rrrs)) (car rrrs))
   (else
    (let*
     ((signs (map (λ (x) (if (positive? x) +1 -1)) rrrs))
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
     (* (/ gcd-numerators gcd-denominators) sum))))))

(define (combine-equals rrrs)
 (for/fold ((h (hash))) ((rrr rrrs))
  (define abs-rrr (abs rrr))
  (if (zero? rrr) h
   (hash-set h abs-rrr ((if (negative? rrr) sub1 add1) (hash-ref h abs-rrr 0))))))

(define (read-hash h)
 (for/fold ((rrrs '())) (((rrr n) (in-hash h)))
  (if (zero? n) rrrs
   (cons (* n (abs n) rrr) rrrs))))

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
 (unless (even? q) (error 'root "square expected, found: ~s^~s" p q))
 (expt p (/ q 2)))

#;(begin
   
 (let ([cp (current-print)]) ; with thanks to Roby Findler
  (current-print
   (λ (x)
     (define sp (open-output-string))
     (parameterize ([current-output-port sp])
       (cp x))
     (display (get-output-string sp)))))
 (add-roots-of-rationals 3 -12) ; --> -3 because √3-√12 = √3(1-√4) = -√3
 (add-roots-of-rationals 1/3 -1/108 5 -5 -1/5 1/5 -5 5) ; --> 25/108 because:
 ; √(1/3)-√(1/108) = √(1/3)(1-√(1/36)) = √(1/3)(1-1/6) = √(1/3)(5/6) = √(1/3)√(25/36) =
 ; √(25/108) 
 (add-roots-of-rationals -1/3 1/108 5 -5 -1/5 1/5 -5 5) ; --> -25/108
 (add-roots-of-rationals 1/2 1/2) ; --> 2 because √(1/2)+√(1/2) = 2√(1/2) = √(4/2) = √2
 (add-roots-of-rationals 1/3 1/3) ; --> 4/3 because √(1/3)+√(1/3) = 2√(1/3) = √(4/3)
 (add-roots-of-rationals 1/2 1/3)) ; --> error: add-root-of-rationals:
;                                        square expected, found: 3^1

