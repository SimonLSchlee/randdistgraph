#lang racket

(require racket/draw)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sampling code

(define bucket-amount 1000)
(define bucket-last   (sub1 bucket-amount))
(define (value->bucket-index value)
  (min (inexact->exact (floor (* value bucket-amount)))
       bucket-last))

(define samples-per-bucket 2000)
(define samples-overall (* bucket-amount samples-per-bucket))


(define (sample func)
  (define samples (make-vector bucket-amount))
  (define (add-sample bucket-index)
    (vector-set! samples bucket-index (add1 (vector-ref samples bucket-index))))

  (for ([i (in-range samples-overall)])
    (define value (func))
    (add-sample (value->bucket-index value)))

  (define smoothed (smooth samples 3))

  (define upper-bound (apply max (vector->list smoothed)))
  (define scaled
    (for/vector ([v (in-vector smoothed)])
      (/ v upper-bound)))

  scaled)


;; kernel-width*2+1 is the actual window that is used for smoothing
;; at the edges it is kernel-width+1
;; is this ok? or would it better to use the same 5 samples multiple times?
;; TODO compare the results
(define (smooth samples kernel-width)
  (define length (vector-length samples))

  (define (select-kernel i width)
    (define start  (max 0      (- i width)))
    (define end    (min length (+ i width)))
    (define amount (- end start))
    (define sum
      (for/fold ([sum 0])
                ([i (in-range start end)])
        (+ sum
           (vector-ref samples i))))
    (/ sum amount))

  (for/vector ([(v i) (in-indexed (in-vector samples))])
    (select-kernel i kernel-width)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; png rendering

(define size       256)
(define padding    10)
(define padding2   (* padding 2))
(define max-width  (- size padding2))
(define max-height max-width)
(define top    padding)
(define left   padding)

(define bitmap (make-object bitmap% size size #f #t))
(define dc (send bitmap make-dc))
(define pen-size 4)

(define (with-outline)
  (send dc set-pen   (make-color 30 175 210) pen-size 'solid)
  (send dc set-brush (make-color 20 135 165) 'solid))

(define (fill-only)
  (send dc set-pen   (make-color  0   0   0) 0 'transparent)
  (send dc set-brush (make-color 20 135 165) 'solid))

(define (render-graph-png scaled-dist filename)
  (send dc erase)
  (define width  (/ max-width (vector-length scaled-dist)))

  (define (render-poly)
    (define bottom (+ top max-height))
    (define points
      (for/list ([(probability bucket) (in-indexed (in-vector scaled-dist))])
        (define x (+ left
                     (* bucket width)))
        (define height (* max-height probability))
        (define y  (+ top (- max-height height)))
        (cons x y)))

    (fill-only)
    (define bottom-left  (cons left bottom))
    (define bottom-right (cons (+ left max-width) bottom))
    (define poly-points  (list* bottom-right bottom-left points))
    (send dc draw-polygon poly-points)
    (with-outline)
    (send dc draw-lines points))

  (render-poly)
  (send bitmap save-file filename 'png))


(define (sample-and-render func func-symbol)
  (define dist (sample func))
  (define filename (~a func-symbol ".png"))
  (render-graph-png dist filename))

(define-syntax-rule (sample-and-render-functions functions ...)
  (begin (sample-and-render functions (quote functions)) ...
         (void)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; to be sampled random-distribution functions

;; helpers
(define (x² x)       (* x x))
(define (x³ x)       (* x x x))
(define (x⁴ x)       (x² (x² x)))
(define (coin-flip) (= 1 (random 2)))

(define (uniform)             (random))
(define (x-squared)           (x² (random)))
(define (x-cubed)             (x³ (random)))
(define (x-fourth)            (x⁴ (random)))
(define (one-minus-x-squared) (- 1 (x-squared)))
(define (one-minus-x-cubed)   (- 1 (x-cubed)))
(define (one-minus-x-fourth)  (- 1 (x-fourth)))

(define (two-dice)            (* 1/2 (+ (random) (random))))
(define (three-dice)          (* 1/3 (+ (random) (random) (random))))
(define (four-dice)           (* 1/4 (+ (random) (random) (random) (random))))
(define (two-dice-squared)    (x² (two-dice)))
(define (three-dice-squared)  (x² (three-dice)))
(define (four-dice-squared)   (x² (four-dice)))

(define (extremes)
  (define x1 (x² (random)))
  (define x2 (- x1 1))
  (define x3 (if (coin-flip) (- x2) x2))
  (define x4 (+ x3 1))
  (define x5 (* x4 0.5))
  x5)


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; execution
(define (gen1)
  (sample-and-render-functions four-dice))

(define (gen-all)
  (sample-and-render-functions
   uniform x-squared x-cubed x-fourth one-minus-x-squared one-minus-x-cubed one-minus-x-fourth two-dice three-dice four-dice two-dice-squared three-dice-squared four-dice-squared extremes))

(module+ main
  (gen-all))

