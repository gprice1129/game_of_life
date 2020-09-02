#lang racket/gui

(require pict racket/draw)

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

(define (alive? world cell)
  (let ([num-cells (foldl + 0 (map (lambda (c) (if (empty? world c) 0 1))
                                   (cdr (cells-surrounding cell))))])
    (or (= num-cells 3)
        (and (not (empty? world cell)) (= num-cells 2)))))

(define (update old-world)
  (define (update-from remaining-world new-world)
    (if (null? remaining-world)
        new-world
        (update-from (cdr remaining-world) 
                     (update-new-world (cells-surrounding (car remaining-world)) 
                                       new-world))))
  (define (update-new-world cells new-world)
    (cond [(null? cells) new-world]
          [(alive? old-world (car cells))
              (update-new-world (cdr cells) (cons (car cells) new-world))]
          [else (update-new-world (cdr cells) new-world)]))
  (world (update-from (set->list old-world) '())))

(define width 1200)
(define height 800)

(define (init-world num-start-cells)
  (for/set ((i (in-range num-start-cells)))
    (make-living-cell (random (- (/ width  2) 10))
                      (random (- (/ height 2) 10)))))

(define (draw-world canvas dc)
  (for ([x (in-range (/ (send canvas get-width) 2))])
    (for ([y (in-range (/ (send canvas get-height) 2))])
      (cond [(not (empty? my-world (list x y))) (draw-cell dc (* x 2) (* y 2))]))))

(define (game-of-life canvas)
  (for ([_ (in-naturals)])
    (send canvas refresh-now (lambda (dc) (draw-world canvas dc)))
    (set! my-world (update my-world))))

(define my-world (init-world 20000))

(define (main)
  (let* ([frame (new frame%
          [label "Game of Life"]
          [width width]
          [height height])]
        [canvas (new canvas%
          [parent frame]
          [min-width (send frame get-width)]
          [min-height (send frame get-height)]
          [stretchable-width #f]
          [stretchable-height #f]
          [paint-callback draw-world])])
    (send frame show #t)
    (game-of-life canvas)))

(main)
