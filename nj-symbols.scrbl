#lang scribble/manual

@(require
  scribble/core
  scribble/eval
  racket
  "nj-symbols.rkt"
  (for-label
   "nj-symbols.rkt"
   racket)
  (for-syntax racket))
        
@(define lb linebreak)
@(define (↑lb) (list (↑ (hspace 1)) (lb)))
@(define nb nonbreaking)
@; ignore is a syntax such as to prevent arguments to be evaluated.
@(define-syntax-rule (ignore x ...) (void))
@; Below syntaxes are used such as to allow keyword arguments
@; without explicitly mentioning them in the definitions.
@(define-syntax-rule (nbsl x ...) (nb (seclink    x ...)))
@(define-syntax-rule (nbsr x ...) (nb (secref     x ...)))
@(define-syntax-rule (nbhl x ...) (nb (hyperlink  x ...)))
@(define-syntax-rule (nber x ...) (nb (elemref    x ...)))
@(define-syntax-rule (nbrl x ...) (nb (racketlink x ...)))
@(define-syntax-rule (nbr  x ...) (nb (racket     x ...)))

@(define(minus) (element 'tt "-"))
@(define(-?) (element "roman" ?-))
@(define (note . x) (inset (apply smaller x)))
@(define (inset . x) (apply nested #:style 'inset x))
@(define (expt-1) @↑{@(minus)1})
@(define ↑ superscript)
@(define ↓ subscript)

@title[#:version ""]{nj-symbols}
@author{Jacob J. A. Koot}

@;@(defmodule "nj-symbols.rkt" #:packages ())
@(defmodule nj-symbols/nj-symbols #:packages ())

The values of 3j-, 6j- and 9j-symbols are plus or minus the square roots of
non-negative rational numbers. The squares can be computed exactly.
That is what procedures
@nbr[exact-3j-symbol], @nbr[exact-6j-symbol] and @nbr[exact-9j-symbol] do.
They follow the Racah formulas C.21, C.36 and C.41 in appendix C of
@italic{Quantum Mechanics, Volume II, @nb{Albert Messiah,} North-Holland publishing company}.
The computation of the squares of 3j and 6j symbols is easy by straight following the formulas.
The computation of a 9j symbol requires the additione of square roots of rational numbers,
which is somewhat more compliacted, but is possible,
provided the result already is known to be the square root of a rational number.


@defproc[(exact-3j-symbol
          (j1 nonnegative-exact-multiple-of_1/2?)
          (j2 nonnegative-exact-multiple-of_1/2?)
          (j3 nonnegative-exact-multiple-of_1/2?)
          (m1 exact-multiple-of_1/2?)
          (m2 exact-multiple-of_1/2?)
          (m3 exact-multiple-of_1/2?)
          (raise-error? any/c #f))
         (and/c rational? exact?)]{
@(hspace 3)(@nbr[exact-3j-symbol]@(lb)
@(hspace 3)@(hspace 1)@nbr[j1 j2 j3]@(lb)
@(hspace 3)@(hspace 1)@nbr[m1 m2 m3])

Procedure @nbr[exact-3j-symbol] computes 3j-symbols, one of them each time it is called.
It returns plus or minus
the exact square of the value of the 3j-symbol, the sign being that of the 3j-symbol proper.
The (possibly inexact) value proper can be obtained by means of procedure @nbr[3j-symbol].
Procedure @nbr[exact-3j-symbol] checks all arguments to satisfy their contracts.
The following four conditions are checked too. 

@itemlist[
@item{@nbr[(integer? (+ j1 j2 j3))]}
@item{@nbr[(exact-nonnegative-integer? (- j1 (abs m1)))]}
@item{@nbr[(exact-nonnegative-integer? (- j2 (abs m2)))]}
@item{@nbr[(exact-nonnegative-integer? (- j3 (abs m3)))]}]

If @nbr[raise-error?] is @nbr[#f],
not satisfying both of the following two conditions
makes procedure @nbr[exact-3j-symbol] return @nbr[0].@(lb)
If @nbr[raise-error?] is not @nbr[#f],
not satisfying both of the following two conditions
makes procedure @nbr[exact-3j-symbol] raise an error.

@itemlist[
@item{@nbr[(triangular? j1 j2 j3)]}
@item{@nbr[(zero? (+ m1 m2 m3))]}]}

@defproc[(exact-6j-symbol
          (j1 nonnegative-exact-multiple-of_1/2?)
          (j2 nonnegative-exact-multiple-of_1/2?)
          (j3 nonnegative-exact-multiple-of_1/2?)
          (J1 exact-multiple-of_1/2?)
          (J2 exact-multiple-of_1/2?)
          (J3 exact-multiple-of_1/2?)
          (raise-error? any/c #f))
         (and/c rational? exact?)]{
@(hspace 3)(@nbr[exact-6j-symbol]@(lb)
@(hspace 3)@(hspace 1)@nbr[j1 j2 j3]@(lb)
@(hspace 3)@(hspace 1)@nbr[J1 J2 J3])

Procedure @nbr[exact-6j-symbol] computes 6j-symbols, one of them each time it is called.
It returns plus or minus
the exact square of the value of the 6j-symbol, the sign being that of the 6j-symbol proper.
The (possibly inexact) value proper can be obtained by means of procedure @nbr[6j-symbol].
Procedure @nbr[exact-6j-symbol] checks all arguments to satisfy their contracts.
Consider the triples:

@(hspace 3)@nbr[(j1 j2 j3)]@(lb)
@(hspace 3)@nbr[(j1 J2 J3)]@(lb)
@(hspace 3)@nbr[(J1 j2 J3)]@(lb)
@(hspace 3)@nbr[(J1 J2 j3)]

If the sum of the elements of one or more of these triples is not an integer, an error is raised.
If @nbr[raise-error?] is @nbr[#f] and one or more of the triples does not satisfy the
@nbrl[triangular?]{triangular inequality}, procedure @nbr[exact-6j-symbol] returns 0. 
If @nbr[raise-error?] is not @nbr[#f] and one or more of the triples does not satisfy the
@nbrl[triangular?]{triangular inequality}, an error is raised.}

@defproc[(exact-9j-symbol
         (j1 nonnegative-exact-multiple-of_1/2?)
         (j2 nonnegative-exact-multiple-of_1/2?)
         (J12 nonnegative-exact-multiple-of_1/2?)
         (j3 nonnegative-exact-multiple-of_1/2?)
         (j4 nonnegative-exact-multiple-of_1/2?)
         (J34 nonnegative-exact-multiple-of_1/2?)
         (J13 nonnegative-exact-multiple-of_1/2?)
         (J24 nonnegative-exact-multiple-of_1/2?)
         (J nonnegative-exact-multiple-of_1/2?)
         (raise-error? any/c  #f))
         (and/c rational? exact?)]{
@(hspace 3)(@nbr[exact-9j-symbol]@(lb)
@(hspace 3)@(hspace 1)@nbr[j1]@(hspace  2)@nbr[j2]@(hspace  2)@nbr[J12]@(lb)
@(hspace 3)@(hspace 1)@nbr[j3]@(hspace  2)@nbr[j4]@(hspace  2)@nbr[J34]@(lb)
@(hspace 3)@(hspace 1)@nbr[J13]@(hspace 1)@nbr[J24]@(hspace 1)@nbr[J])

Computes plus or minus the square of a 9j-symbol, the sign being that of the value proper.
The sum in each column and each row must be an integer or else an error is raised.
If @nbr[raise-error?] is @nbr[#f] and one or more columns and/or rows
do not satisfy the @nbrl[triangular?]{triangular inquality}, the procedure returns @nbr[0].
If @nbr[raise-error?] is not @nbr[#f] and one or more columns and/or rows
do not satisfy the @nbrl[triangular?]{triangular inquality}, the procedure raises an error.}

@defproc[(3j-symbol
          (j1 nonnegative-exact-multiple-of_1/2?)
          (j2 nonnegative-exact-multiple-of_1/2?)
          (j3 nonnegative-exact-multiple-of_1/2?)
          (m1 exact-multiple-of_1/2?)
          (m2 exact-multiple-of_1/2?)
          (m3 exact-multiple-of_1/2?)
          (raise-error? any/c #f))
         real?]{
Same as@(lb)
@racketblock[
(let ((exact-3j (exact-3j-symbol j1 j2 j3 m1 m2 m3 raise-error?)))
 (cond
  ((positive? exact-3j) (sqrt exact-3j)
  ((negative? exact-3j) (- (sqrt (- exact-3j)))))
  (else 0)))]
which yields a real number, possibly an inexact one.}

@defproc[(6j-symbol
          (j1 nonnegative-exact-multiple-of_1/2?)
          (j2 nonnegative-exact-multiple-of_1/2?)
          (j3 nonnegative-exact-multiple-of_1/2?)
          (J1 exact-multiple-of_1/2?)
          (J2 exact-multiple-of_1/2?)
          (J3 exact-multiple-of_1/2?)
          (raise-error? any/c #f))
         real?]{
Same as@(lb)
@racketblock[
(let ((exact-6j (exact-6j-symbol j1 j2 j3 J1 J2 J3 raise-error?)))
 (cond
  ((positive? exact-6j) (sqrt exact-6j)
  ((negative? exact-6j) (- (sqrt (- exact-6j)))))
  (else 0)))]
which yields a real number, possibly an inexact one.}

@defproc[(9j-symbol
          (j1 nonnegative-exact-multiple-of_1/2?)
          (j2 nonnegative-exact-multiple-of_1/2?)
          (J12 nonnegative-exact-multiple-of_1/2?)
          (j3 nonnegative-exact-multiple-of_1/2?)
          (j4 nonnegative-exact-multiple-of_1/2?)
          (J34 nonnegative-exact-multiple-of_1/2?)
          (J13 nonnegative-exact-multiple-of_1/2?)
          (J24 nonnegative-exact-multiple-of_1/2?)
          (J nonnegative-exact-multiple-of_1/2?)
          (raise-error? any/c  #f))
         real?]{
Same as@(lb)
@racketblock[
(let ((exact-9j (exact-9j-symbol j1 j2 J12 j3 j4 J34 J13 J24 J raise-error?)))
 (cond
  ((positive? exact-9j) (sqrt exact-9j)
  ((negative? exact-9j) (- (sqrt (- exact-9j)))))
  (else 0)))]
which yields a real number, possibly an inexact one.}

@defproc[#:kind "predicate"
         (exact-multiple-of_1/2? (x any/c)) boolean?]{
Returns @nbr[#t] if and only if @nbr[x] is an exact multiple of ½.}

@defproc[#:kind "predicate"
         (nonnegative-exact-multiple-of_1/2? (x any/c)) boolean?]{
Returns @nbr[#t] if and only if @nbr[x] is a non-negative exact multiple of ½.}

@defproc[(triangular? (j1 any/c) (j2 any/c) (j3 any/c)) boolean?]{
Returns @nbr[#t] if and only if
the three arguments are @nbrl[nonnegative-exact-multiple-of_1/2?]{non-negative exact multiples of ½},
their sum is an integer and satisfy the triangular inequality:

@(hspace 3)|@nbr[j1]@(minus)@nbr[j2]| ≤ @nbr[j3] ≤ @nbr[j1]+@nbr[j2]

which for non-negative @nbr[j1], @nbr[j2] and @nbr[j3] is equivalent with:

@(hspace 3)2@bold{max}(@nbr[j1], @nbr[j2], @nbr[j3]) ≤ @nbr[j1]+@nbr[j2]+@nbr[j3]

Hence, procedure @nbr[triangular?] is invariant under permutation of its arguments.}

@(bold (larger (larger "The end")))

