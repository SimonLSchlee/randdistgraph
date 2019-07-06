#lang racket

(define-syntax-rule (print-stuff functions ...)
  (begin (displayln (~a "!["
                        (quote functions)
                        "]("
                        (quote functions) ".png?raw=true \""
                        (quote functions) "\")")) ...
         (void)))

(print-stuff
 uniform x-squared x-cubed x-fourth one-minus-x-squared one-minus-x-cubed one-minus-x-fourth two-dice three-dice four-dice two-dice-squared three-dice-squared four-dice-squared extremes)

;; https://easings.net/
