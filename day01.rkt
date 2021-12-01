#lang racket

(define (count-increases xs)
  (cdr (foldl
         (match-lambda**
           [(curr (cons prev count))
            (cons curr
                  (if (> curr prev)
                      (+ 1 count)
                      count))])
         (cons (car xs) 0)
         (cdr xs))))

(define (segment-sums n xs)
  (cons (apply + (take xs n))
        (match (drop xs n)
          ['() '()]
          [_ (segment-sums n (cdr xs))])))

(define (part-1 depths)
  (count-increases depths))

(define (part-2 depths)
  (count-increases (segment-sums 3 depths)))

(module+ test
  (require rackunit)

  (define data-example (map string->number (file->lines "data/01/example.txt")))
  (define data-input   (map string->number (file->lines "data/01/input.txt")))

  (check-equal? (part-1 data-example) 7)
  (check-equal? (part-1 data-input) 1139)
  (check-equal? (segment-sums 3 (range 1 11)) '(6 9 12 15 18 21 24 27))
  (check-equal? (segment-sums 3 data-example) '(607 618 618 617 647 716 769 792))
  (check-equal? (part-2 data-example) 5)
  (check-equal? (part-2 data-input) 1103))
