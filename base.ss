#lang racket

(provide starting-board is-white-piece? is-black-piece? copy-board 0:7 starting-board)
;initial board position
(define starting-board 
  (vector  
   (vector 'r 'n 'b 'q 'k 'b 'n 'r)
   (make-vector 8 'p)
   (make-vector 8 '-)
   (make-vector 8 '-)
   (make-vector 8 '-)
   (make-vector 8 '-)
   (make-vector 8 'P)
   (vector 'R 'N 'B 'Q 'K 'B 'N 'R)))
;castling state variables

(define (is-white-piece? piece)
  (or (equal? piece 'P) (equal? piece 'R) (equal? piece 'N)
      (equal? piece 'B) (equal? piece 'Q) (equal? piece 'K)))

(define (is-black-piece? piece)
  (or (equal? piece 'p) (equal? piece 'r) (equal? piece 'n)
      (equal? piece 'b) (equal? piece 'q) (equal? piece 'k)))

(define (copy-board board)
  (define b (build-vector 8 (λ (x) (build-vector 8 (λ(x) 0)))))
  (for ([i 0:7])
    (vector-copy! (vector-ref b i) 0 (vector-ref board i)))
  b)

(define 0:7 '(0 1 2 3 4 5 6 7))

