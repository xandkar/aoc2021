#lang racket

(module+ test
  (require rackunit)

  (check-equal? (file->lines "data/01/input.txt") '("foo")))
