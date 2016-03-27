;;; Scheme code for Twenty-One Simulator [PS2 Fall '90]

(define (twenty-one player-strategy house-strategy)
  (let ((house-initial-hand (make-new-hand (deal))))
    (let ((player-hand
           (play-hand player-strategy
                      (make-new-hand (deal))
                      (hand-up-card house-initial-hand))))
      (if (> (hand-total player-hand) 21)
          0                                ; ``bust'': player loses
          (let ((house-hand 
                 (play-hand house-strategy
                            house-initial-hand
                            (hand-up-card player-hand))))
            (cond ((> (hand-total house-hand) 21)
                   1)                      ; ``bust'': house loses
                  ((> (hand-total player-hand)
                      (hand-total house-hand))
                   1)                      ; house loses
                  (else 0)))))))           ; player loses

(define (play-hand strategy my-hand opponent-up-card)
  (cond ((> (hand-total my-hand) 21) my-hand) ; I lose... give up
        ((strategy my-hand opponent-up-card) ; hit?
         (play-hand strategy
                    (hand-add-card my-hand (deal))
                    opponent-up-card))
        (else my-hand)))                ; stay


(define (deal) (+ 1 (random 10)))

(define (make-new-hand first-card)
  (make-hand first-card first-card))

(define (make-hand up-card total)
  (cons up-card total))

(define (hand-up-card hand)
  (car hand))

(define (hand-total hand)
  (cdr hand))

(define (hand-add-card hand new-card)
  (make-hand (hand-up-card hand)
             (+ new-card (hand-total hand))))

(define (hit? your-hand opponent-up-card)
  (newline)
  (display "Opponent up card ")
  (display opponent-up-card)
  (newline)
  (display "Your Total: ")
  (display (hand-total your-hand))
  (newline)
  (display "Hit? ")
  (user-says-y?))


(define (user-says-y?) (eq? (read) 'y))

; Problem 2 -- stop-at

(define (stop-at limit)
  (lambda (my-hand opponent-up-card)
    (< (hand-total my-hand) limit)))

; Problem 3 -- Automated testing

(define (test-strategy my-strat house-strat n)
  (define (iter i total)
    (if (> i n)
      total
      (iter (+ i 1) (+ total
                       (twenty-one my-strat
                                   house-strat)))))
  (iter 1 0))

; Problem 4 -- Watch player

(define (watch-player strategy)
  ; This is a wrapper, it must still return a strategy
  (lambda (my-hand opponent-up-card)
    (newline)
    (display "Opponent up card ")
    (display opponent-up-card)
    (newline)
    (display "Your Total: ")
    (display (hand-total my-hand))
    (newline)
    (display (strategy my-hand opponent-up-card))
    (strategy my-hand opponent-up-card)))

(twenty-one (watch-player (stop-at 16))
            (watch-player (stop-at 15)))

; Problem 5 -- Louis

(define (louis my-hand opponent-up-card)
  (cond ((< (hand-total my-hand) 12) #t)
        ((> (hand-total my-hand) 16) #f)
        ((= (hand-total my-hand) 12)
         (< opponent-up-card 4)) ; Note this as a test as a return
        ((= (hand-total my-hand) 16)
         (not (= opponent-up-card 10)))
        (else (> opponent-up-card 6))))

; Problem 6 -- Both

(define (both strat1 strat2)
  (lambda (my-hand opponent-up-card)
    (and (strat1 my-hand opponent-up-card)
         (strat2 my-hand opponent-up-card))))
