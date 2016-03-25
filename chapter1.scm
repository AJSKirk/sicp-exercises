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


#|--------------------------------------
 | TODO:
 |  - Coin counter
 |  - 1.19
 |#


