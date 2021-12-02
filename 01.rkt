#lang racket

(define (count-increases nums)
  (cdr (foldl
         (match-lambda**
           [(curr (cons prev count))
            (cons curr (if (> curr prev) (+ 1 count) count))])
         (cons (car nums) 0)
         (cdr nums))))

(define (sliding-windows width xs)
  (cons (take xs width)
        (match (drop xs width)
          ['() '()]
          [_ (sliding-windows width (cdr xs))])))

(define (sliding-window-sums width nums)
  (map (curry apply +) (sliding-windows width nums)))

(define (part-1 depths)
  (count-increases depths))

(define (part-2 depths)
  (count-increases (sliding-window-sums 3 depths)))

(module+ test
  (require rackunit)

  (define (file->nums path)
    (map string->number (file->lines path)))

  (define data-example (file->nums "data/01/example.txt"))
  (define data-input   (file->nums "data/01/input.txt"))

  (let ([width 10]) (check-equal? (sliding-window-sums width (range 1 11)) '(55)))
  (let ([width 5]) (check-equal? (sliding-window-sums width (range 1 11)) (range 15 41 width)))
  (let ([width 4]) (check-equal? (sliding-window-sums width (range 1 11)) (range 10 35 width)))
  (let ([width 3]) (check-equal? (sliding-window-sums width (range 1 11)) (range 6 28 width)))
  (let ([width 2]) (check-equal? (sliding-window-sums width (range 1 11)) (range 3 20 width)))
  (let ([width 1]) (check-equal? (sliding-window-sums width (range 1 11)) (range 1 11 width)))
  (check-equal? (sliding-windows 3 data-example)
                '((199 200 208)
                  (200 208 210)
                  (208 210 200)
                  (210 200 207)
                  (200 207 240)
                  (207 240 269)
                  (240 269 260)
                  (269 260 263)))
  (check-equal? (sliding-window-sums 3 data-example)
                '(607 618 618 617 647 716 769 792))

  (check-equal? (part-1 data-example) 7)
  (check-equal? (part-1 data-input) 1139)
  (check-equal? (part-2 data-example) 5)
  (check-equal? (part-2 data-input) 1103))
