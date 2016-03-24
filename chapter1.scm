#; 1.5

(define (p) (p))

(define (test x y)
  (if (= x 0)
    0
    y))

#; 1.6

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

#; 1.7 -- Implement derivative good-enough?

(define (good-enough? new-guess old-guess)
  (< (abs (- new-guess old-guess)) .001))

(define (sqrt-iter guess x)
  (if (good-enough? guess (improve guess x))
    guess
    (sqrt-iter (improve guess x)
               x)))

#; 1.8 -- Cube root program

(define (cube x) (* x x x))

(define (cube-improve guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (curt-iter guess x)
  (if (good-enough? guess (cube-improve guess x))
    guess
    (curt-iter (cube-improve guess x)
               x)))

#; 1.10 -- Computing Ackermann's function

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)
(A 2 4)
(A 3 3)

#; 1.10 -- Recursive vs Iterative Processes

(define (f-recurs n)
  (if (< n 3)
    n
    (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

#; Note the massive duplictation of work

(define (f n)
  (if (< n 3)
    n
    (f-iter 3 2 1 (- n 3))))
