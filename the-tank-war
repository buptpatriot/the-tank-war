#lang racket

(require 2htdp/image)
(require (except-in racket/gui/base make-pen make-color))
(require (only-in mrlib/image-core render-image))

(define SCORE 0)
(define GAMEOVER #f)

(define TIMER-INTERVAL 50)

(define SIZE-ENEMY 20)
(define FIREPOWER-ENEMY 100)

(define UNIT-LENGTH 10)
(define FRAME-WIDTH 400)
(define FRAME-HEIGHT 400)

(define-syntax-rule (INITIAL-POSITION)
  (cons (* (- (round (/ (/ (send THE-CANVAS get-width) UNIT-LENGTH) 2)) 1) UNIT-LENGTH)
        (* (- (round (/ (send THE-CANVAS get-height) UNIT-LENGTH)) 3) UNIT-LENGTH)))


(define MY-COLOR "red")
(define ENEMY-COLOR "blue")
(define DEAD-COLOR "black")

(define (inverse num) (- 0 num))

(define (get-tank-image color)
  (let* ([square-inner (square UNIT-LENGTH "solid" color)]
         [double-square (above square-inner square-inner)])
    (overlay/xy  
     (overlay/xy double-square
                 UNIT-LENGTH
                 (inverse UNIT-LENGTH)
                 double-square)
     (* 2 UNIT-LENGTH)
     UNIT-LENGTH
     double-square)))

; My tank and bullet
(define MY-TANK (get-tank-image MY-COLOR))
(define MY-BULLET (square UNIT-LENGTH "solid" MY-COLOR))

; Enemy tank and bullet
(define ENEMY-TANK (get-tank-image ENEMY-COLOR))
(define ENEMY-BULLET (square UNIT-LENGTH "solid" ENEMY-COLOR))

(define DEAD-TANK (get-tank-image DEAD-COLOR))

; Control the direction of the tank
(define (turn-left tank) (rotate 90 tank))
(define (turn-right tank) (rotate 270 tank))
(define (turn-up tank) tank)
(define (turn-down tank) (rotate 180 tank))

(define (new-tank tank-img dir)
  (cond [(eq? dir 'left) (turn-left tank-img)]
        [(eq? dir 'right) (turn-right tank-img)]
        [(eq? dir 'down) (turn-down tank-img)]
        [else (turn-up tank-img)]))
(define (my-tank dir)
  (new-tank MY-TANK dir))
(define (enemy-tank dir)
  (new-tank ENEMY-TANK dir))
(define (dead-tank dir)
  (new-tank DEAD-TANK dir))

(define (on-pause button event) 
  (send THE-CANVAS enable #f)
  (stop-timer))

(define (on-start button event)
  (set! GAMEOVER #f)
  (send THE-CANVAS enable #t)
  (send THE-CANVAS focus)
  (resume-timer))

(define (gameover)
  (set! GAMEOVER #t)
  (send THE-CANVAS refresh-now)
  (send THE-CANVAS enable #f)
  (stop-timer)  
  (sleep 3)
  (set! TANK-MINE (make-tank (INITIAL-POSITION) 'up))
  (set! TANK-ENEMY '())
  (set! TANK-DEAD '())
  (set! BULLET-MINE '())
  (set! BULLET-ENEMY '())
  (set! SCORE 0))

(define (render-gameover dc)
  (when GAMEOVER 
    (let* ([canvas-width (send THE-CANVAS get-width)]
           [canvas-height (send THE-CANVAS get-height)]
           [t (text "Game over!" 36 "indigo")]
           [t-w (image-width t)]
           [t-h (image-height t)]
           [w (/ (- canvas-width t-w) 2)]
           [h (/ (- canvas-height t-h) 2)])
      (render-image t dc w h))))

(define (send-score)
  (send msg set-label (string-append "score : " (number->string SCORE))))

; Make a frame by instantiating the frame% class
(define THE-FRAME (new (class frame%
                       (super-new)
                       (define/augment (on-close)
                         (display "Exitting...")
                         (newline)
                         (exit)))
                       [label "The Tank War!"]
                       [width FRAME-WIDTH]
                       [height FRAME-HEIGHT]))
 

; Make a panel to contain the buttons
(define THE-PANEL (new horizontal-panel%
                       [parent THE-FRAME]
                       [style '(border)]
                       [spacing 10]
                       [alignment '(right center)]
                       [stretchable-height #f]))

(define THE-PANE-IN (new horizontal-pane%
                       [parent THE-PANEL]
                       [spacing 10]
                       [alignment '(left center)]
                       [stretchable-height #f]))

(define BUTTON-START
  (new button% [parent THE-PANE-IN]
             [label "Start"]
             [callback on-start]))

(define BUTTON-PAUSE 
  (new button% [parent THE-PANE-IN]
             [label "Pause"]
             [callback on-pause]))
; Make a static text message in the frame
(define msg (new message% [parent THE-PANEL]
                          [label "score: 0"]
                          [min-width 80]))
 
; Derive a new canvas (a drawing window) class to handle events
(define my-canvas%
  (class canvas% ; The base class is canvas%
    ;(inherit get-dc)
    ; Define overriding method to handle keyboard events
    (define/override (on-char key-event)
      (on-key-event (send key-event get-key-code)))
    ; Call the superclass init, passing on all init args
    (super-new)))

; The paint-callback method
(define (paint! canvas dc)
 ; (send dc set-background "blue"
  (render-gameover dc)
  (render-tank-mine dc)
  (render-bullet-mine dc)
  (render-tank-enemy dc)
  (render-bullet-enemy dc)
  (render-tank-dead dc))

(define (render-tank create-tank-img tank-pst dc)
   (let ([position (tank-position tank-pst)]
         [dir (tank-dir tank-pst)])
    (render-image (create-tank-img dir) dc (car position) (cdr position))
    ))

(define (render-tanks create-tank-img tanks-pst dc)
  (define (render-tank-inner tank-pst)
    (render-tank create-tank-img tank-pst dc))
  (for-each render-tank-inner tanks-pst))

(define (render-tank-mine dc)
  (render-tank my-tank TANK-MINE dc))

(define (render-tank-enemy dc)
  (render-tanks enemy-tank TANK-ENEMY dc))

(define (render-tank-dead dc)
  (render-tanks dead-tank TANK-DEAD dc)
  (set! TANK-DEAD '()))

(define (render-bullet bullet-img bullets-pst dc)
  (define (render-bullet-inner b)
    (let ([position (bullet-position b)])
      (render-image bullet-img dc (car position) (cdr position))))
  (for-each render-bullet-inner bullets-pst))

(define (render-bullet-mine dc)
  (render-bullet MY-BULLET BULLET-MINE dc))

(define (render-bullet-enemy dc)
  (render-bullet ENEMY-BULLET BULLET-ENEMY dc))

; Make a canvas that handles events in the frame
(define THE-CANVAS (new my-canvas%
                        [parent THE-FRAME]
                        [paint-callback paint!]))

; Show the frame by calling its show method
(send THE-FRAME show #t)
(send THE-CANVAS focus)


(define TICK-TOCK 0)
(define-struct tank (position dir))
(define-struct bullet (position dir))
(define TANK-MINE (make-tank (INITIAL-POSITION) 'up))
(define TANK-ENEMY '())
(define TANK-DEAD '())
(define BULLET-MINE '())
(define BULLET-ENEMY '())


(define (change-dir tank dir)
  (if (or (eq? dir 'left)
          (eq? dir 'right)
          (eq? dir 'up)
          (eq? dir 'down))
      (make-tank (tank-position tank) dir)
      tank))

(define (change-dir-mine dir)
  (set! TANK-MINE (change-dir TANK-MINE dir)))

(define (step dir position-old)
  (let ([x (car position-old)]
        [y (cdr position-old)])
    (cond [(eq? dir 'left) (set! x (- x UNIT-LENGTH))]
          [(eq? dir 'right) (set! x (+ x UNIT-LENGTH))]
          [(eq? dir 'up) (set! y (- y UNIT-LENGTH))]
          [(eq? dir 'down) (set! y (+ y UNIT-LENGTH))])
    (cons x y)))

(define (out-of-canvas? position)
  (let* ([canvas-width (send THE-CANVAS get-width)]
         [canvas-height (send THE-CANVAS get-height)]
         [boundary-right (- canvas-width (* 3 UNIT-LENGTH))]
         [boundary-below (- canvas-height (* 3 UNIT-LENGTH))]
         [x (car position)]
         [y (cdr position)])
    (or (< x 0) (> x boundary-right)
        (< y 0) (> y boundary-below))))
        

(define (move-tank tank)
  (let* ([dir (tank-dir tank)]
         [position (tank-position tank)]
         [new-position (step dir position)])
    (if (out-of-canvas? new-position)
        tank
        (make-tank new-position dir))))

(define (move-tank-mine)
  (set! TANK-MINE (move-tank TANK-MINE)))
   
(define-syntax-rule (shot tank-pst bullets) 
  (let* ([dir (tank-dir tank-pst)]
        [position (tank-position tank-pst)]
        [tank-x (car position)]
        [tank-y (cdr position)]
        [bullet-x (+ tank-x UNIT-LENGTH)]
        [bullet-y (+ tank-y UNIT-LENGTH)])
    (cond [(eq? dir 'left) (set! bullet-x (- tank-x UNIT-LENGTH))]
          [(eq? dir 'right) (set! bullet-x (+ tank-x (* 3 UNIT-LENGTH)))]
          [(eq? dir 'up) (set! bullet-y (- tank-y UNIT-LENGTH))]
          [(eq? dir 'down) (set! bullet-y (+ tank-y (* 3 UNIT-LENGTH)))])
    (define bullet-p (cons bullet-x bullet-y))
    (define new-bullet (make-bullet bullet-p dir))
    (set! bullets (cons new-bullet bullets))))

(define (fire)
  (shot TANK-MINE BULLET-MINE))
  
(define (enemy-fire)
  (define (fire-inner tank)
    (when (> FIREPOWER-ENEMY (random 1000))
        (shot tank BULLET-ENEMY)))
  (for-each fire-inner TANK-ENEMY))

(define (point-in-square? p s)
  (let ([p-x (car p)]
        [p-y (cdr p)]
        [s-x (caar s)]
        [s-y (cdar s)]
        [len (cdr s)])
    (and (>= p-x s-x)
         (< p-x (+ s-x len))
         (>= p-y s-y)
         (< p-y (+ s-y len)))))

(define (square-hit-square? square1 square2)
  (define (square-hit-square-inner? s1 s2)
    (let* ([p1 (car s1)]
           [p2 (car s2)]
           [p2-x (car p2)]
           [p2-y (cdr p2)]
           [len (cdr s2)]
           [ss (cons p1 len)]
           [pp1 (cons p2-x p2-y)]
           [pp2 (cons (+ p2-x len) p2-y)]
           [pp3 (cons (+ p2-x len) (+ p2-y len))]
           [pp4 (cons p2-x (+ p2-y len))])
      (or (point-in-square? pp1 ss)
          (point-in-square? pp2 ss)
          (point-in-square? pp3 ss)
          (point-in-square? pp4 ss))))
  (let ([len1 (cdr square1)]
        [len2 (cdr square2)])
    (if (< len1 len2)
        (square-hit-square-inner? square2 square1)
        (square-hit-square-inner? square1 square2))))
 

(define (tank-hit-tank? t1 t2)
  (let* ([p1 (tank-position t1)]
         [p2 (tank-position t2)]
         [len (* 3 UNIT-LENGTH)]
         [s1 (cons p1 len)]
         [s2 (cons p2 len)])
    (square-hit-square? s1 s2)))

(define (bullet-hit-bullet? b1 b2)
  (let* ([p1 (bullet-position b1)]
         [p2 (bullet-position b2)])
    (equal? p1 p2)))

(define (tank-hit-bullet? t b)
  (let* ([t-p (tank-position t)]
         [b-p (bullet-position b)]
         [len (* 3 UNIT-LENGTH)]
         [s (cons t-p len)])
    (point-in-square? b-p s)))

(define (spacious? point)
  (define (spacious-inner? ts)
    (if (null? ts)
        #t
        (let* ([t (car ts)]
               [len (* 3 UNIT-LENGTH)]
               [position (tank-position t)]
               [s1 (cons position len)]
               [s2 (cons point len)])
          (and (not (square-hit-square? s1 s2))
                   (spacious-inner? (cdr ts))))))
  (let ([tanks (cons TANK-MINE TANK-ENEMY)])
    (spacious-inner? tanks)))

(define (do-new-enemy)
  (let* ([canvas-width (send THE-CANVAS get-width)]
         [width-block (round (/ canvas-width UNIT-LENGTH))]
         [random-num (random (- width-block 2))]
         [x (* random-num UNIT-LENGTH)]
         [new-position (cons x 0)]
         [dir (list-ref '(left right down) (random 3))]
         [new-tank (make-tank new-position dir)])
    (when (spacious? new-position)
      (set! TANK-ENEMY (cons new-tank TANK-ENEMY)))))

(define (new-enemy)
  (when (> SIZE-ENEMY (random 1000))
    (do-new-enemy)))

(define (get-another-dir dir)
  (let* ([dirs '(left right up down)]
         [other-dirs '()])
    (for-each 
     (lambda (d)
       (when (not (eq? dir d))
         (set! other-dirs (cons d other-dirs)))) 
     dirs)
    (list-ref other-dirs (random 3))))

(define (tank-out-canvas? tank)
  (out-of-canvas? (tank-position tank)))

(define (tank-hit-tanks? tank tanks)
  (if (null? tanks)
      #f
      (or (tank-hit-tank? tank (car tanks))
          (tank-hit-tanks? tank (cdr tanks))))) 

(define (need-change-dir? tank tanks)
  (let ([new-tank (move-tank  tank)])
    (or (tank-out-canvas? new-tank)
        (tank-hit-tanks? new-tank tanks)
        (eq? tank new-tank))))

(define (get-no-hit-tank tank tanks)
  (if (need-change-dir? tank tanks)
      (change-dir tank (get-another-dir (tank-dir tank)))
      (move-tank tank)))
          

(define (do-move-tank-enemy)
  (define (move-iter new-tanks old-tanks)
    (if (null? old-tanks)
        new-tanks
        (move-iter 
         (cons 
          (get-no-hit-tank (car old-tanks) (append new-tanks (cdr old-tanks))) 
          new-tanks)
         (cdr old-tanks))))
  (set! TANK-ENEMY (move-iter '() TANK-ENEMY)))
    
(define (move-tank-enemy)
  (when (= (modulo TICK-TOCK 10) 0)
    (do-move-tank-enemy)))

(define (refresh-enemy)
  (move-bullet-enemy)
  (new-enemy)
  (move-tank-enemy)
  (enemy-fire))

(define (on-key-event dir)
  (let ([current-dir (tank-dir TANK-MINE)])
    (cond [(eq? dir #\space) (fire)]
          [(eq? dir current-dir) (move-tank-mine)]
          [else (change-dir-mine dir)])
    (send THE-CANVAS refresh-now)))

(define (move-bullet bullet)
  (let* ([dir (bullet-dir bullet)]
         [position (bullet-position bullet)]
         [x (car position)]
         [y (cdr position)]
         [canvas-width (send THE-CANVAS get-width)]
         [canvas-height (send THE-CANVAS get-height)])
    (cond [(eq? dir 'left) (set! x (- x UNIT-LENGTH))]
          [(eq? dir 'right) (set! x (+ x UNIT-LENGTH))]
          [(eq? dir 'up) (set! y (- y UNIT-LENGTH))]
          [(eq? dir 'down) (set! y (+ y UNIT-LENGTH))])
    (if (or (< x 0) (> x canvas-width) (< y 0) (> y canvas-height))
        null
        (make-bullet (cons x y) dir))))

(define (move-bullet-iter new old)
  (if (null? old)
      new
      (let ([new-bullet (move-bullet (car old))])
        (if (null? new-bullet)
            (move-bullet-iter new (cdr old))
            (move-bullet-iter (cons new-bullet new) (cdr old))))))

(define-syntax-rule (move-bullets bullets)
  (set! bullets (move-bullet-iter '() bullets)))

(define (move-bullet-mine)
  (move-bullets BULLET-MINE))

(define (move-bullet-enemy)
  (move-bullets BULLET-ENEMY))


(define (change-world)
  (move-bullet-mine)
  (refresh-enemy))
  
(define (delete item list)
  (if (null? list)
      '()
      (if (equal? item (car list)) 
          (cdr list)
          (cons (car list) (delete item (cdr list))))))

(define-syntax-rule (add-sprite sprite sprite-list)
  (set! sprite-list (cons sprite sprite-list)))

(define-syntax-rule (delete-sprite sprite sprite-list)
  (set! sprite-list (delete sprite sprite-list)))


(define (judge-mytank-hit-tank)
  (define (on-hit enemy-tank)
    (when (tank-hit-tank? enemy-tank TANK-MINE)
      (delete-sprite enemy-tank TANK-ENEMY)
      (add-sprite TANK-MINE TANK-DEAD)
      (gameover)))
  (for-each on-hit TANK-ENEMY))

(define (judge-mytank-hit-bullet)
  (define (on-hit bullet)
    (when (tank-hit-bullet? TANK-MINE bullet)
      (delete-sprite bullet BULLET-ENEMY)
      (add-sprite TANK-MINE TANK-DEAD)
      (gameover)))
  (for-each on-hit BULLET-ENEMY))

(define (judge-mybullet-hit-tank)
  (define (on-hit bullet)
    (define (on-hit-inner the-enemy-tank)
       (when (tank-hit-bullet? the-enemy-tank bullet)
         (set! SCORE (add1 SCORE))
         (delete-sprite bullet BULLET-MINE)
         (delete-sprite the-enemy-tank TANK-ENEMY)
         (add-sprite the-enemy-tank TANK-DEAD)))
     (for-each on-hit-inner TANK-ENEMY))
  (for-each on-hit BULLET-MINE))

(define (judge-mybullet-hit-bullet)
    (define (on-hit enemy-bullet)
      (define (on-hit-inner bullet)
       (when (bullet-hit-bullet? enemy-bullet bullet)
         (delete-sprite bullet BULLET-MINE)
         (delete-sprite enemy-bullet BULLET-ENEMY)))
     (for-each on-hit-inner BULLET-MINE))
  (for-each on-hit BULLET-ENEMY))

(define (judge-tank-mine)
  (judge-mytank-hit-tank)
  (judge-mytank-hit-bullet))

(define (judge-bullet-mine)
  (judge-mybullet-hit-tank)
  (judge-mybullet-hit-bullet))

(define (judge)
  (judge-tank-mine)
  (judge-bullet-mine))
  

(define (tick!) 
  (set! TICK-TOCK (add1 TICK-TOCK))
  (change-world)
  (judge)
  (send THE-CANVAS refresh)
  (send-score))

;; Finally, we set up a timer that will call tick! on every second.
(define THE-TIMER (new timer% 
                       [notify-callback tick!]
                       [interval TIMER-INTERVAL]))
(define (stop-timer) (send THE-TIMER stop))
(define (resume-timer) (send THE-TIMER start TIMER-INTERVAL)) 
                             
