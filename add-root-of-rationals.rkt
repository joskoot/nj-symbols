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
 (let ((rrrs (combine-multiples rrrs)))
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
      (numerators (map sqrt-factorized numerators))
      (denominators (map sqrt-factorized denominators))
      (terms (map / numerators denominators))
      (sum (apply + (map * signs terms)))
      (sum (* sum (abs sum))))
     (* (/ gcd-numerators gcd-denominators) sum))))))

(define (combine-multiples rrrs)
 (let loop ((h (hash)) (rrrs (sort rrrs abs<)))
  (cond
   ((null? rrrs) (read-hash h))
   (else
    (define kar (car rrrs))
    (cond
     ((zero? kar) (loop h (cdr rrrs)))
     (else
      (define abs-kar (abs kar))
      (define-values (new-h new-rrrs)
       (for/fold
        ((h (hash-set h abs-kar ((if (negative? kar) sub1 add1) (hash-ref h abs-kar 0))))
         (rrrs '()))
        ((rrr (in-list (cdr rrrs))))
        (define n ((if (negative? rrr) - +) (sqrt (/ (abs rrr) abs-kar))))
        (cond
         ((integer? n) (values (hash-set h abs-kar (+ (hash-ref h abs-kar) n)) rrrs))
         (else (values h (cons rrr rrrs))))))
      (loop new-h (reverse new-rrrs))))))))

(define (abs< x y) (< (abs x) (abs y)))

(define (read-hash h)
 (for/fold ((rrrs '())) (((rrr n) (in-hash h)))
  (if (zero? n) rrrs
   (cons (* n (abs n) rrr) rrrs))))

(define (sqrt-factorized x)
 (for/fold ((product 1)) ((x (in-list (factorize x))))
  (define p (car x))
  (define q (cadr x))
  (unless (even? q) (error 'sqrt-factorized "square expected, found: ~s^~s" p q))
  (* product (expt p (/ q 2)))))

#;(begin
   
 (let ([cp (current-print)]) ; with thanks to Roby Findler
  (current-print
   (λ (x)
     (define sp (open-output-string))
     (parameterize ([current-output-port sp])
       (cp x))
     (display (get-output-string sp)))))

 (add-roots-of-rationals) ; --> 0
 (add-roots-of-rationals 1/3) ; --> 1/3
 (add-roots-of-rationals 3 -12) ; --> -3 because √3-√12 = √3(1-√4) = -√3
 (add-roots-of-rationals 1/3 -1/108 5 -5 -1/5 1/5 -5 5) ; --> 25/108 because:
 ; √(1/3)-√(1/108) = √(1/3)(1-√(1/36)) = √(1/3)(1-1/6) = √(1/3)(5/6) = √(1/3)√(25/36) =
 ; √(25/108) 
 (add-roots-of-rationals -1/3 1/108 5 -5 -1/5 1/5 -5 5) ; --> -25/108
 (add-roots-of-rationals 1/2 1/2) ; --> 2 because √(1/2)+√(1/2) = 2√(1/2) = √(4/2) = √2
 (add-roots-of-rationals 1/3 1/3) ; --> 4/3 because √(1/3)+√(1/3) = 2√(1/3) = √(4/3)
 (add-roots-of-rationals 1/2 1/3)) ; --> error: add-root-of-rationals:
;                                        square expected, found: 3^1

