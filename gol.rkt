#lang racket/gui

(require pict racket/draw profile "krikkit/windows.rkt")

(define (make-living-cell i j) (list i j))

(define (world living-cells) (list->set living-cells))

(define (cell-x cell) (car cell))

(define (cell-y cell) (cadr cell))

(define (cell-shift cell shift) (list (+ (cell-x cell) (car shift))
                                      (+ (cell-y cell) (cadr shift))))
(define (empty? world cell) (not (set-member? world cell)))

(define (cells-surrounding cell)
  (let ([directions '(0 1 -1)])
    (map (lambda (s) (cell-shift cell s))
         (cartesian-product directions directions))))

(define (make-zone size) (cons size (make-bytes (* size size))))
(define (get-zone-size zone) (car zone))
(define (get-zone-data zone) (cdr zone))
(define (get-zone-cell zone x y) 
  (bytes-ref (get-zone-data zone) (+ (* y (get-zone-size zone) x))))

(define (set-zone-cell! zone x y v)
  (bytes-set! (get-zone-data zone) (+ (* y (get-zone-size zone)) x) v))

(define (make-world living-cells zone-size)
  (define world (make-hash))
  (for ((cell living-cells)) 
    (match (wc=>zc cell zone-size)
      ((list zkey x-offset y-offset)
       (unless (world-has-zone? world zkey) (world-set-zone! world zkey (make-zone zone-size)))
       (set-zone-cell! (world-zone-ref world zkey) x-offset y-offset 1))))
  world)

(define (world-has-zone? world zkey)
  (hash-has-key? world zkey))

(define (world-zone-ref world zkey)
  (hash-ref world zkey))

(define (world-set-zone! world zkey zone)
  (hash-set! world zkey zone))

(define (wc=>zc cell zone-size) 
  (define-values (x x-offset) (quotient/remainder (cell-x cell) zone-size))
  (define-values (y y-offset) (quotient/remainder (cell-y cell) zone-size))
  `((,x ,y) ,x-offset ,y-offset))

(module+ test
  (require rackunit)
  (check-true
    (equal? (wc=>zc '(1 2) 4) '((0 0) 1 2)))
  (check-true
    (equal? (wc=>zc '(3 4) 2) '((1 2) 1 0)))
  (check-true
    (equal? (make-world '() 4) (make-hash '())))
  (check-true
    (equal? (make-world '((1 2)) 4)
            (make-hash `(((0 0) . (4  . ,(bytes 0 0 0 0
                                                0 0 0 0
                                                0 1 0 0
                                                0 0 0 0)))))))
  (check-true
    (equal? (make-world '((1 2) (0 1)) 2)
            (make-hash `(((0 0) . (2 . ,(bytes 0 0
                                               1 0)))
                         ((0 1) . (2 . ,(bytes 0 1 
                                               0 0))))))))
; world is hashmap of zones
;   cell-coordinates (x, y)
;   zone-coordinates (flr(x/N), flr(y/N), mod(x/N), mod(y/N))
;   world-coordinates=>zone-coordinates

; zone
;   N x N byte string


;; Old update that is about 50% slower
;(define (alive? world cell)
  ;(let ([num-cells (foldl + 0 (map (lambda (c) (if (empty? world c) 0 1))
                                   ;(cdr (cells-surrounding cell))))])
    ;(or (= num-cells 3)
        ;(and (not (empty? world cell)) (= num-cells 2)))))
;(define (update old-world)
  ;(define (update-from remaining-world new-world)
    ;(if (null? remaining-world)
        ;new-world
        ;(update-from (cdr remaining-world)
                     ;(update-new-world (cells-surrounding (car remaining-world))
                                       ;new-world))))
  ;(define (update-new-world cells new-world)
    ;(cond [(null? cells) new-world]
          ;[(alive? old-world (car cells))
              ;(update-new-world (cdr cells) (cons (car cells) new-world))]
          ;[else (update-new-world (cdr cells) new-world)]))
  ;(world (update-from (set->list old-world) '())))

(define (update old-world)
  (for/fold
    ((living (set)) (zombies (set))
     #:result (for/fold
                ((living living))
                ((cell (in-set zombies)))
                (if (= 3 (for/sum
                           ((n (in-list (cdr (cells-surrounding cell)))))
                           (if (set-member? old-world n) 1 0)))
                  (set-add living cell)
                  living)))
    ((cell (in-set old-world)))
    (define-values (alive dead)
      (partition (lambda (n) (set-member? old-world n))
                 (cdr (cells-surrounding cell))))
    (values (if (<= 2 (length alive) 3) (set-add living cell) living)
            (set-union zombies (list->set dead)))))

(define (init-world num-start-cells width height)
  (for/set ((i (in-range num-start-cells)))
    (make-living-cell (random (- (/ width  2) 10))
                      (random (- (/ height 2) 10)))))

;(define (draw-cell dc x y) (draw-pict (rectangle 2 2) dc x y))
(define (draw-cell dc x y) (send dc set-argb-pixels x y 2 2
                                 (bytes 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)))
(define (draw-world world dc max-width max-height)
  (send dc clear)
  (for ((c world))
    (match-define `(,cx ,cy) c)
    (define x (+ cx cx))
    (define y (+ cy cy))
    (when (and (< -1 x max-width) (< -1 y max-height))
      (draw-cell dc x y))))
;; A more complex alternative that may give a tiny performance improvement
;(define (draw-world canvas dc)
  ;(define height (send canvas get-height))
  ;(define width  (send canvas get-width))
  ;(define pitch  (* width 4))
  ;(define bs     (make-bytes (* width height 4) 255))
  ;(for ([y (in-range (/ height 2))])
    ;(define y-offset (* y pitch 2))
    ;(for ([x (in-range (/ width 2))])
      ;(unless (empty? my-world (list x y))
        ;(define offset (+ y-offset (* x 4 2)))
        ;(bytes-copy! bs    offset        (bytes 0 0 0 0 0 0 0 0))
        ;(bytes-copy! bs (+ offset pitch) (bytes 0 0 0 0 0 0 0 0)))))
  ;(send dc set-argb-pixels 0 0 width height bs))

(define (game-of-life)
  (define width 400)
  (define height 600)
  (define starting-cells 20000)
  (define (do-nothing event) void)
  (define w (window do-nothing do-nothing width height))
  (define my-world (init-world starting-cells width height))
  (w 'set-title! "Game of Life")
  (w 'show)
  (w 'set-background 128 128 128)
  (void (thread (lambda ()
          (let loop ()
            (w 'render (lambda (dc) (draw-world my-world dc width height)))
            (sleep 0.1)
            (set! my-world (update my-world))
            (loop))))))

;(profile (main))

(module+ main
  (game-of-life))
