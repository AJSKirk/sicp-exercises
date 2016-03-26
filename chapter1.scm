; 1.5

(define (p) (p))

(define (test x y)
  (if (= x 0)
    0
    y))

; 1.6

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x) (* x x))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y) (/ (+ x y) 2))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
               x)))

; 1.7 -- Implement derivative good-enough?

(define (good-enough? new-guess old-guess)
  (< (abs (- new-guess old-guess)) .001))

(define (sqrt-iter guess x)
  (if (good-enough? guess (improve guess x))
    guess
    (sqrt-iter (improve guess x)
               x)))

; 1.8 -- Cube root program

(define (cube x) (* x x x))

(define (cube-improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (curt-iter guess x)
  (if (good-enough? guess (cube-improve guess x))
    guess
    (curt-iter (cube-improve guess x)
               x)))

; 1.10 -- Computing Ackermann's function

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)
(A 2 4)
(A 3 3)

; 1.11 -- Recursive vs Iterative Processes

(define (f-recurs n)
  (if (< n 3)
    n
    (+ (f-recurs (- n 1)) (* 2 (f-recurs (- n 2))) (* 3 (f-recurs (- n 3))))))

; Note the massive duplictation of work

; Note that this inner fn has access to n, since it's in the f environment scope. Don't need to pass n

(define (f n)
  (define (iter a b c counter)
    (cond ((< n 3) n)
          ((> counter n) a)
          (else (iter
                  (+ a (* 2 b) (* 3 c))
                  a
                  b
                  (+ counter 1)))))
  (iter 2 1 0 3))

; Tested up to N = 35

(define (test-equal n)
  (define (iter counter)
    (cond ((> counter n) t)
          ((not (= (f counter) (f-recurs counter))) f)
          (else (iter (+ counter 1)))))
  (iter 0))

; 1.12 -- Recursive Pascal's triangle

(define (abs x)
  (if (< x 0) (- x) x))

(define (pascal-place level coord)
  (cond ((and (= level 0) (= coord 0)) 1)
        ((> (abs coord) level) 0)
        (else (+ (pascal-place (- level 1) (- coord 1))
                 (pascal-place (- level 1) (+ coord 1))))))

; 1.16 -- Logarithmic iterative exponentiation

(define (square x) (* x x))

(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt-i b n)
  (define (iter a b counter)
    (cond ((= counter 0) a)
          ((even? counter) (iter a (square b) (/ counter 2)))
          (else (iter (* a b) b (- counter 1)))))
  (iter 1 b n))

; 1.17 -- Logarithmic recutsive multiplication

(define (double x) (* x 2))

(define (halve x) (/ x 2))

(define (fast-mult a b)
  (cond ((= b 1) a)
        ((even? b) (fast-mult (double a) (halve b)))
        (else (+ (fast-mult a (- b 1)) a))))

; 1.19 -- Logarithmic Fibonacci algorithm

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (square q) (* 2 p q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

; 1.22 -- Prime search
 
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test)
  (cond ((> (square test) n) n)
        ((divides? test n) test)
        (else (find-divisor n (+ test 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display "***")
  (display elapsed-time))

(define (search-for-primes from count)
  (define (iter i found)
    (timed-prime-test i)
    (if (< found count)
       (if (prime? i)
         (iter (+ i 2) (+ found 1))
         (iter (+ i 2) found))))
  (if (even? from)
    (iter (+ from 1) 0)
    (iter from 0)))

; 1.23 -- Next test divisor procedure

(define (next x)
  (if (= x 2)
    3
    (+ x 2)))

(define (find-divisor n test)
  (cond ((> (square test) n) n)
        ((divides? test n) test)
        (else (find-divisor n (next test)))))

; 1.24 -- Fermat prime checking

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (expmod b e m)
  (cond ((= e 0) 1)
        ((even? e)
         (remainder (square (expmod b (/ e 2) m))
                    m))
        (else
          (remainder (* b (expmod b (- e 1) m))
                     m))))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
    (report-prime (- (runtime) start-time))))

; 1.28 -- Miller-Rabin test

(define (safe-expmod b e m)
  (define (safe-square a)
    (if (= (remainder (square a) m) 1)
      (if (not (or
            (= a 1)
            (= a (- m 1))))
        0
        (square a))
      (square a)))
  (cond ((= e 0) 1)
        ((even? e)
         (remainder (safe-square (safe-expmod b (/ e 2) m))
                    m))
        (else
          (remainder (* b (safe-expmod b (- e 1) m))
                     m))))

(define (mr-test n)
  (define (try-it a)
    (= (safe-expmod a (- n 1) n) 1))
  (try-it (+ 2 (random (- n 2)))))

; 1.29 -- Simpson's rule

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (inc x)
  (+ x 1))

(define (simp-int f a b n)
  (define (yk k)
    (f (+ a (* k (/ (- b a) n)))))
  (define (coef i)
    (cond ((or (= i 1) (= i n)) 1)
          ((= (remainder i 2) 1) 4)
          (else 2)))
  (define (term i)
    (* (coef i) (yk i)))
  (* (/ (/ (- b a) n) 3)
     (sum term 0 inc n)))

; 1.30 -- Iterative summation

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ result (term a)))))
  (iter a 0))

; 1.31 -- Product function
;
; a) Recusive

(define (product term a next b)
  (if (> a b)
    1
    (* (term a)
       (product term (next a) next b))))

; b) Iterative

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result (term a)))))
  (iter a 1))

; Note we initialise the iterator result to 1 (the multiplicative identity),
; not 0 (additive identity)

(define (identity x)
  x)

(define (factorial n)
  (product identity 1 inc n))

; 1.31 -- Accumumulator function
;
; a) Recusive

(define (accumulate combiner null term a next b)
  (if (> a b)
    null
    (combiner (term a)
              (accumulate combiner
                          null
                          term
                          (next a)
                          next
                          b))))

; b) Iterative

(define (accumulate combiner null term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner result (term a)))))
  (iter a null))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

; 1.33 -- #nofilter

(define (filtered-accumulate fltr? combiner null term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((fltr? a) (iter (next a)
                           (combiner result (term a))))
          (else (iter (next a) result))))
  (iter a null))

; a) Sum of squares of primes

(define (ssprime a b)
  (filtered-accumulate prime? + 0 square a inc b))

; b) Product of numbers relatively prime to n

(define (relative-prime? a b)
  (= (gcd a b) 1))

; With thanks to Euclid
(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (prp n)
  (define (rpn? x)
    (relative-prime? x n))
  (filtered-accumulate rpn? * 1 identity 1 inc n))

; 1.35 -- Fixed point search

(define tolerance .00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

; 1.36 --

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (display next)
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0)
; 34 steps w/o average damping

(fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 2.0)
; 9 steps with average damping

; 1.37 -- Continued fractions

(define (cont-frac n d k)
  (define (cfr i)
    (if (> i k)
      0
      (/ (n i) (+ (d i) (cfr (+ i 1))))))
  (cfr 1))

(define (cont-frac-check k)
  (< (abs (- (cont-frac (lambda (i) 1.0)
                        (lambda (i) 1.0)
                        k)
             (/ 1.0 (fixed-point (lambda (x) (+ 1 (/ 1 x)))
                                 1.0)))) .0001))

; Now iterate!
    ; I think bottom up is more better for iter

(define (cont-frac n d k)
  (define (iter k result)
    (if (< k 1)
      result
      (iter (- k 1) (/ (n k) (+ (d k) result)))))
  (iter k 0))

; Note that this could also be rewritten w/ the accumulator fn

; 1.38 -- Continued Fraction for e-2

(define (d k)
  (define (iter i last)
    (if (= (remainder i 3) 2)
      (+ last 2)
      1))
  (iter 1 0))

(+ 2 (cont-frac (lambda (i) 1.0)
                d
                10))
; 1.39 -- Lambert CF for tan

(define (tan-cf x k)
  (define (iter k result)
    (if (< k 1)
      result
      (iter (- k 1) (/ (square x) (- (+ (* k 2) 1) result)))))
  (iter k 0))

; 1.40 -- Newton's method for cubics

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

; 1.41 -- Double

(define (double f)
  (lambda (x) (f (f x))))

(((double (double double)) inc) 5)

; 1.42 -- Composition

(define (compose f g)
  (lambda (x) (f (g x))))

; 1.43 -- Repeated

(define (repeated f n)
  (define (iter i result)
    (if (> i n)
      result
      (iter (+ i 1) (compose f result))))
  (iter 1 identity))

; 1.44 -- Smoothing

(define (smooth f)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx)))
                 3)))

(define (nsmooth f n)
  ((repeated smooth n) f))

; 1.45 -- Nth Roots

(define (average-damp f)
  (lambda (x) (/ (+ x (f x)) 2)))

; Doesn't work
;(fixed-point (average-damp (lambda (x) (/ 16 (cube x)))) 1.0)

(fixed-point ((repeated average-damp 4)
              (lambda (x) (/ (fast-expt-i 2 31)
                             (fast-expt-i x 30)))) 1.0)
; One smooth works for n < 4
; Two smooth works for n < 8
; Three smooth works for n < 16
;
; => n < 2^(k+1) where k is min smooth

(define (n-root x n)
  (define (iter k)
    (if (> (fast-expt-i 2 (+ k 1)) n)
      k
      (iter (+ k 1))))
  (let ((req-smooths (iter 1)))
    (fixed-point ((repeated average-damp req-smooths)
                  (lambda (y) (/ x
                                 (fast-expt-i y (- n 1)))))
                 1.0)))

; 1.46 -- Iterative improvement

(define (iterative-improvement test improve)
  (define (iter guess)
    (if (test guess)
      guess
      (iter (improve guess))))
  iter)

(define (sqrt x)
  (iterative-improvement (lambda (guess) (< (abs (- (square guess)
                                                    x))
                                            threshold))
                         (lambda (guess) (/ (+ guess (/ x guess))
                                            2))))

(define (fixed-point f)
  (iterative-improvement (lambda (x) (< (abs (- x (f x)))
                                        threshold))
                         (lambda (x) (f x))))

;TODO:
; - Coin counter
; - 1.18
;

