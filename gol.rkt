#lang racket

(define (make-living-cell i j) (list i j))
(define (world living-cells) (list->mutable-set living-cells))
(define (cell-x cell) (car cell))
(define (cell-y cell) (cadr cell))
(define (cell-shift cell hs vs) (list (+ (cell-x cell) hs) (+ (cell-y cell) vs)))
(define (cell-t cell) (cell-shift cell 0 1))
(define (cell-tl cell) (cell-shift cell -1 1))
(define (cell-tr cell) (cell-shift cell 1 1))
(define (cell-b cell) (cell-shift cell 0 -1))
(define (cell-bl cell) (cell-shift cell -1 -1))
(define (cell-br cell) (cell-shift cell 1 -1))
(define (cell-r cell) (cell-shift cell 1 0))
(define (cell-l cell) (cell-shift cell -1 0))
(define (empty? world cell) (not (set-member? world cell)))
(define (cells-surrounding cell)
  (list (cell-t cell) (cell-tl cell) (cell-tr cell) (cell-b cell)
        (cell-bl cell) (cell-br cell) (cell-r cell) (cell-l cell)))
(define (alive? world cell)
  (let* ([num-cells (foldl + 0 (map (lambda (c) (if (empty? world c) 0 1))
                                    (cells-surrounding cell)))]
         [is-three? (= num-cells 3)])
    (or is-three?
        (and (not (empty? world cell)) (= num-cells 2)))))

(define (update-world old-world)
  (let ([new-world (world '())]
        [empty-cells (list->mutable-set '())])
    (for ([c (set->list old-world)])
      (cond [(alive? old-world c) (set-add! new-world c)])
      (for ([e (filter (lambda (c) (empty? old-world c))
                       (cells-surrounding c))])
        (set-add! empty-cells e)))
    (for ([e (set->list empty-cells)])
      (cond [(alive? old-world e) (set-add! new-world e)]))
    new-world))

(define (draw-world row-start row-end col-start col-end world)
  (for ([r (in-range row-start row-end)])
    (display "\n")
    (for ([c (in-range col-start col-end)])
      (if (empty? world (list r c))
          (display " ")
          (display "X"))))
  (display "\n------------------------------------------------------------\n"))

(define (draw world) (draw-world -100 100 -100 100 world))

(define (gol-helper world iterations)
  (draw world)
  (sleep 2)
  (if (= iterations 0)
      (display "Done!\n")
      (gol-helper (update-world world) (- iterations 1))))

(define (gol)
  (let ([my-world (world '())])
    (for ([_ (in-range 0 4000)])
      (set-add! my-world (list (random -80 80) (random -80 80))))
    (gol-helper my-world 10000)))
      
(gol)    