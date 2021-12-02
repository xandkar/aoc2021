#lang racket

(define dir? (or/c 'forward 'down 'up))

(struct Ins (dir mag) #:transparent)

(define Ins/c
  (struct/dc Ins
             [dir dir?]
             [mag exact-nonnegative-integer?]))

(define instructions? (listof Ins/c))

(struct Pos (horiz depth aim) #:transparent)

(define Pos/c
  (struct/dc Pos
             [horiz exact-integer?]
             [depth exact-integer?]
             [aim   exact-integer?]))

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
  (-> path-string? instructions?)
  (map str->ins (file->lines path)))

(define/contract (move-v1 i p)
  (-> Ins/c Pos/c Pos/c)
  (match i
    [(Ins 'forward m) (struct-copy Pos p [horiz (+ (Pos-horiz p) m)])]
    [(Ins 'down    m) (struct-copy Pos p [depth (+ (Pos-depth p) m)])]
    [(Ins 'up      m) (struct-copy Pos p [depth (- (Pos-depth p) m)])]))

(define/contract (move-v2 i p)
  (-> Ins/c Pos/c Pos/c)
  (match i
    [(Ins 'down    m) (struct-copy Pos p [aim (+ (Pos-aim p) m)])]
    [(Ins 'up      m) (struct-copy Pos p [aim (- (Pos-aim p) m)])]
    [(Ins 'forward m) (struct-copy Pos p
                                   [horiz (+ (Pos-horiz p) m)]
                                   [depth (+ (Pos-depth p) (* m (Pos-aim p)))])]))

(define/contract (interpret-v1 instructions pos)
  (-> instructions? Pos/c Pos/c)
  (foldl move-v1 pos instructions))

(define/contract (interpret-v2 instructions pos)
  (-> instructions? Pos/c Pos/c)
  (foldl move-v2 pos instructions))

(define/contract (solve interpret instructions state)
  (-> (-> instructions? Pos/c Pos/c) instructions? Pos/c exact-integer?)
  (match-define (Pos h d _) (interpret instructions state))
  (* h d))

(define/contract (part-1 instructions)
  (-> instructions? exact-integer?)
  (solve interpret-v1 instructions (Pos 0 0 0)))

(define/contract (part-2 instructions)
  (-> instructions? exact-integer?)
  (solve interpret-v2 instructions (Pos 0 0 0)))

(module+ test
  (require rackunit)

  (define data-example (file->inss "data/02/example.txt"))
  (define data-input   (file->inss "data/02/input.txt"))

  (check-equal? (move-v1 (Ins 'up 1) (Pos 0 0 0)) (Pos 0 -1 0))
  (check-equal? (move-v1 (Ins 'down 1) (Pos 0 0 0)) (Pos 0 1 0))
  (check-equal? (move-v1 (Ins 'forward 1) (Pos 0 0 0)) (Pos 1 0 0))

  (check-equal? (move-v2 (Ins 'up 1) (Pos 0 0 0)) (Pos 0 0 -1))
  (check-equal? (move-v2 (Ins 'down 1) (Pos 0 0 0)) (Pos 0 0 1))
  (check-equal? (move-v2 (Ins 'forward 1) (Pos 0 0 0)) (Pos 1 0 0))
  (check-equal? (interpret-v2
                  (list (Ins 'forward 5)
                        (Ins 'down 5)
                        (Ins 'forward 8)
                        (Ins 'up 3)
                        (Ins 'down 8)
                        (Ins 'forward 2))
                  (Pos 0 0 0))
                (Pos 15 60 10))

  (check-equal? (interpret-v1 data-example (Pos 0 0 0)) (Pos 15 10 0))
  (check-equal? (interpret-v2 data-example (Pos 0 0 0)) (Pos 15 60 10))

  (check-equal? (part-1 data-example) 150)
  (check-equal? (part-1 data-input) 2027977)
  (check-equal? (part-2 data-example) 900)
  (check-equal? (part-2 data-input) 1903644897))
