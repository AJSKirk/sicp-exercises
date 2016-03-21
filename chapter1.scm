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


