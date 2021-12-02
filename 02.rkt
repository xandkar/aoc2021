#lang racket

(define dir? (or/c 'forward 'down 'up))

(struct Ins (dir mag) #:transparent)

(struct Pos (horiz depth) #:transparent)

(define Ins/c
  (struct/dc Ins
             [dir dir?]
             [mag exact-nonnegative-integer?]))

(define Pos/c
  (struct/dc Pos
             [horiz exact-integer?]
             [depth exact-integer?]))

(define/contract (str->dir s)
  (-> string? dir?)
  (match s
    ["forward" 'forward]
    ["down" 'down]
    ["up" 'up]))

(define/contract (str->mag s)
  (-> string? exact-nonnegative-integer?)
  (string->number s))

(define/contract (str->ins s)
  (-> string? Ins/c)
  (match (string-split s)
    [(list s1 s2) (Ins (str->dir s1) (str->mag s2))]
    [_ (raise (cons 'invalid-instruction-data s))]))

(define/contract (file->inss path)
  (-> path-string? (listof Ins/c))
  (map str->ins (file->lines path)))

(define/contract (step i p)
  (-> Ins/c Pos/c Pos/c)
  (match i
    [(Ins 'forward m) (struct-copy Pos p [horiz (+ (Pos-horiz p) m)])]
    [(Ins 'down    m) (struct-copy Pos p [depth (+ (Pos-depth p) m)])]
    [(Ins 'up      m) (struct-copy Pos p [depth (- (Pos-depth p) m)])]))

(define/contract (steps is p)
  (-> (listof Ins/c) Pos/c Pos/c)
  (foldl step p is))

(define/contract (part-1 is)
  (-> (listof Ins/c) exact-integer?)
  (match-define (Pos h d) (steps is (Pos 0 0)))
  (* h d))

(module+ test
  (require rackunit)

  (define data-example (file->inss "data/02/example.txt"))
  (define data-input   (file->inss "data/02/input.txt"))

  (check-equal? (step (Ins 'up 1) (Pos 0 0)) (Pos 0 -1))
  (check-equal? (step (Ins 'down 1) (Pos 0 0)) (Pos 0 1))
  (check-equal? (step (Ins 'forward 1) (Pos 0 0)) (Pos 1 0))
  (check-equal? (steps data-example (Pos 0 0)) (Pos 15 10))
  (check-equal? (part-1 data-example) 150)
  (check-equal? (part-1 data-input) 2027977)
  )
