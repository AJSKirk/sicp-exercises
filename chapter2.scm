; 2.1 -- Neg safe make-rat

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ (if (< 0 d)
                 n
                 (- 0 n))
             g)
          (/ (if (< 0 d)
                 d
                 (- 0 d))
             g))))

; 2.2 -- 2D line segments

(define make-point cons)

(define x-point car)

(define y-point cdr)

(define make-segment cons)

(define start-segment car)

(define end-segment cdr)

(define (midpoint-segment line)
  (make-point (/ (+ (x-point (start-segment line))
                    (x-point (end-segment line)))
                 2)
              (/ (+ (y-point (start-segment line))
                    (y-point (end-segment line)))
                 2)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; 2.3 -- Rectangles in a plane

(define (square x)
  (* x x))

(define (iterative-improvement test improve)
  (define (iter guess)
    (if (test guess)
      guess
      (iter (improve guess))))
  iter)

(define threshold .00001)

; Hardcoding the initial guess to 1.0 for ease

(define (sqrt x)
  ((iterative-improvement (lambda (guess) (< (abs (- (square guess)
                                                    x))
                                            threshold))
                         (lambda (guess) (/ (+ guess (/ x guess))
                                            2))) 1.0))

; Make it general, this will work for any line, not only straight

(define (len-line line)
  (sqrt (+ (square (- (x-point (end-segment line))
                      (x-point (start-segment line))))
           (square (- (y-point (end-segment line))
                      (y-point (start-segment line)))))))

(define (rect-perim rect)
  (+ (* 2 (len-line (make-segment (top-left-corner rect)
                                  (top-right-corner rect))))
     (* 2 (len-line (make-segment (top-left-corner rect)
                                  (bottom-left-corner rect))))))

(define (rect-area rect)
  (* (len-line (make-segment (top-left-corner rect)
                             (top-right-corner rect)))
     (len-line (make-segment (top-left-corner rect)
                             (bottom-left-corner rect)))))

; Representation one -- Corner gen at make time
; Points enumerated clockwise from to right

(define (make-rect top-right bottom-left)
  (cons (cons top-right
              (make-point (x-point top-right)
                          (y-point bottom-left)))
        (cons bottom-left
              (make-point (x-point bottom-left)
                          (y-point top-right)))))

(define (top-left-corner rect)
  (cdr (cdr rect)))

(define (top-right-corner rect)
  (car (car rect)))

(define (bottom-right-corner rect)
  (cdr (car rect)))

(define (bottom-left-corner rect)
  (car (cdr rect)))

; Representation two -- Corner gen at select time

(define make-rect cons)

(define top-right-corner car)
(define bottom-left-corner cdr)

(define (top-left-corner rect)
  (make-point (x-point (bottom-left-corner rect))
              (y-point (top-right-corner rect))))

(define (bottom-right-corner rect)
  (make-point (x-point (top-right-corner rect))
              (y-point (bottom-left-corner rect))))

; Test -- Expect 4 and 8

(rect-area (make-rect (make-point 0 0)
                      (make-point 2 2)))

(rect-perim (make-rect (make-point 0 0)
                      (make-point 2 2)))

; 2.5 -- Integer pair representation

(define (square x)
  (* x x))

(define (fast-expt-i b n)
  (define (iter a b counter)
    (cond ((= counter 0) a)
          ((even? counter) (iter a (square b) (/ counter 2)))
          (else (iter (* a b) b (- counter 1)))))
  (iter 1 b n))

(define (cons x y)
  (* (fast-expt-i 2 x)
     (fast-expt-i 3 y)))

; Having a problem where if I name this car it doesn't overwrite the primitive
; at deeper recursion levels, I get a type error because it's expecting a
; primitive pair

(define (carx prod) ; Recursive
  (if (> (remainder prod 2) 0)
    0
    (+ 1 (carx (/ prod 2)))))

(define (car prod) ; Iterative hits no namespace issues
  (define (iter i prod)
    (if (> (remainder prod 2) 0)
      i
      (iter (+ 1 i) (/ prod 2))))
  (iter 0 prod))

(define (cdrx prod) ; Recursive
  (if (> (remainder prod 3) 0)
    0
    (+ 1 (carx (/ prod 3)))))

(define (cdr prod) ; Iterative hits no namespace issues
  (define (iter i prod)
    (if (> (remainder prod 3) 0)
      i
      (iter (+ 1 i) (/ prod 3))))
  (iter 0 prod))

; 2.6 -- Church numerals

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
; Note that the function application of n means that it must be a Chrich
; numeral (i.e. a function), not an int

; Properties of numbers:
; x + 0 = x
; 1 = (add-one 0)
; (+ n x) = ((repeat n (add-one)) x)
;
; From 1 = (add-one zero)

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (plus a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))

; 2.7 -- Interval arithmetic

(define make-interval cons)
(define lower-bound car)
(define upper-bound cdr)

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

; 2.8 -- Interval subtraction

; Note upper/lower flip for y
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

; 2.9 -- Width change

(define (width interval)
  (/ (- (upper-bound interval) (lower-bound interval))
     2))

; Check width transforms

(define int1 (make-interval 1 5))
(define int2 (make-interval 10 20))

(width int1) ; = 2
(width int2) ; = 5
(width (add-interval int1 int2)) ; = 7 = Sum of widths
(width (sub-interval int1 int2)) ; = 7 = Sum of widths

(width (mul-interval int1 int2)) ; = 45
(width (div-interval int1 int2)) ; = .225

; 2.10 -- Catch span zero errors

(define (spans-zero? interval)
  (and (< (lower-bound interval) 0)
       (> (upper-bound interval) 0)))

(define (div-interval x y)
  (if (or (spans-zero? x)
          (spans-zero? y))
    (error "Spans zero exception")
    (mul-interval x
                  (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y))))))

; 2.11 -- TODO

; 2.12 - Centre/Tolerance representation

; Note we're going to use big pct (10%, not 0.1), even though I hate it


(define (make-center-percent center percent)
  (let ((buffer (* center (/ percent 100.0))))
    (make-interval (- center buffer) (+ center buffer))))

(define (center interval)
  (/ (+ (lower-bound interval) (upper-bound interval))
     2))

(define (percent interval)
  (* 100
     (/ (width interval)
        (center interval))))

; 2.13 -- TODO
; Note that the small pct assumption means that products of pct can be treated
; as zero

; 2.14 - 2.17 -- TODO

; 2.17 --List ends

; Can't get this to work with if, only cond (racket bug, works in MIT Scheme)
; Keeps trying to eval the answer, not sure why

(define (last-pair l)
  (cond ((null? (cdr l)) l)
        (else (last-pair (cdr l)))))

(define (last-pair l)
  (if (null? (cdr l))
             l
             (last-pair (cdr l))))

; 2.18 -- Reverse

(define nil '()) ; MIT Scheme fix

; Thought I'd need this, didn't
; Still a nice peice of code, so keeping it
;
; Easier done w/ a length test, this is more interesting
(define (drop-last l) 
  (if (null? (cdr l))
    nil
    (cons (car l) (drop-last (cdr l)))))

(define (reverse l)
  (if (null? (cdr l))
    l
    (append (reverse (cdr l)) (list (car l)))))

