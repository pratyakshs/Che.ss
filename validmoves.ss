#lang racket
(require racket/mpair)
(require "base.ss")
(require "check.ss")
(provide is-legal? w-king-moved? b-king-moved? make-move-forced make-move-n convert-move castling-rook-fpos is-move-castling?)
(provide a1! e1! h1! a8! e8! h8!)

(define a1 #t)
(define (a1!) (set! a1 #f))
(define e1 #t)
(define (e1!) (set! e1 #f))
(define h1 #t)
(define (h1!) (set! h1 #f))
(define a8 #t)
(define (a8!) (set! a8 #f))
(define e8 #t)
(define (e8!) (set! e8 #f))
(define h8 #t)
(define (h8!) (set! h8 #f))

(define (identify symbol)
  (cond [(equal? symbol #\a) 0]
        [(equal? symbol #\b) 1]
        [(equal? symbol #\c) 2]
        [(equal? symbol #\d) 3]
        [(equal? symbol #\e) 4]
        [(equal? symbol #\f) 5]
        [(equal? symbol #\g) 6]
        [(equal? symbol #\h) 7]
        [else (- (char->integer symbol) 48)]))

;converting a move i.e. string to list of nos. 
(define (convert-move move)
  (let* ([reference (string->list move)]
         [convtd (map (λ(x) (identify x)) reference)])
    
    (list (list-ref convtd 0) (- 8 (list-ref convtd 1))
          (list-ref convtd 2) (- 8 (list-ref convtd 3)))))

(define (is-move-castling? m)
  (or (equal? m (list 4 7 6 7))
      (equal? m (list 4 7 2 7))
      (equal? m (list 4 0 6 0))
      (equal? m (list 4 0 2 0))))

(define (castling-rook-fpos m)
  (cond [(equal? m (list 4 7 6 7)) (list 7 7 5 7)]
        [(equal? m (list 4 7 2 7)) (list 0 7 3 7)]
        [(equal? m (list 4 0 6 0)) (list 7 0 5 0)]
        [(equal? m (list 4 0 2 0)) (list 0 0 3 0)]))

;king moved

(define (w-king-moved?)
  (not e1))
(define (b-king-moved?)
  (not e8))

(define (is-same-color? p1 p2)
  (if (or (and (is-white-piece? p1) (is-white-piece? p2))
          (and (is-black-piece? p1) (is-black-piece? p2)))
      #t #f))


(define (is-col-empty? board col row-i row-f)
  ;returns true if the column BETWEEN row-i and row-f contains no pieces.
  (if (>= row-f row-i)
      (cond [(= row-f row-i) #t]
            [(= 1 (- row-f row-i)) #t]
            [(eq? (vector-ref (vector-ref board (+ 1 row-i)) col) '-)
             (is-col-empty? board col (+ row-i 1) row-f)]
            [else #f])
      (is-col-empty? board col row-f row-i)))

(define (is-row-empty? board row col-i col-f)
  ;returns true if the column BETWEEN row-i and row-f contains no pieces.
  (if (>= col-f col-i)
      (cond [(= col-f col-i) #t]
            [(= 1 (- col-f col-i)) #t]
            [(eq? (vector-ref (vector-ref board row) (+ col-i 1)) '-)
             (is-row-empty? board row (+ 1 col-i) col-f)] 
            [else #f])
      (is-row-empty? board row col-f col-i)))

(define (is-diag-empty? board col-i row-i col-f row-f)
  ;returns true if the diagonal BETWEEN (col-i, row-i) and (col-f, row-f) is empty.
  (if (>= col-f col-i)
      (if (>= row-f row-i)
          (cond [(= row-f row-i) #t]
                [(= 1 (- row-f row-i)) #t] 
                [(eq? (vector-ref (vector-ref board (+ row-i 1)) (+ col-i 1)) '-)
                 (is-diag-empty? board (+ 1 col-i) (+ 1 row-i) col-f row-f)]
                [else #f])
          (cond [(= (- row-i row-f) 1) #t]
                [(eq? (vector-ref (vector-ref board (- row-i 1)) (+ col-i 1)) '-)
                 (is-diag-empty? board (+ 1 col-i) (- row-i 1) col-f row-f)]
                [else #f]))
      (is-diag-empty? board col-f row-f col-i row-i)))


(define (make-move-forced board move)
  (define (helper move)
    (begin
      (vector-set! (vector-ref board (list-ref move 3)) (list-ref move 2)
                   (vector-ref (vector-ref board (list-ref move 1)) (list-ref move 0)))
      (vector-set! (vector-ref board (list-ref move 1)) (list-ref move 0) '-)
      (if (is-move-castling? move)
          (begin (vector-set! (vector-ref board (list-ref (castling-rook-fpos move) 3)) 
                              (list-ref (castling-rook-fpos move) 2)
                              (vector-ref (vector-ref board (list-ref (castling-rook-fpos move) 1))
                                          (list-ref (castling-rook-fpos move) 0)))
                 (vector-set! (vector-ref board (list-ref (castling-rook-fpos move) 1)) (list-ref (castling-rook-fpos move) 0)
                              '-))
          (void))))
  (if (= 5 (string-length move)) 
      (let ([promoted (string-ref move 4)]
            [final 'tobe]
            )
        (begin 
          (cond [(equal? promoted #\q) (if (equal? #\8 (string-ref move 3)) (set! final 'Q) (set! final 'q))]
                [(equal? promoted #\r) (if (equal? #\8 (string-ref move 3)) (set! final 'R) (set! final 'r))]
                [(equal? promoted #\b) (if (equal? #\8 (string-ref move 3)) (set! final 'B) (set! final 'b))]
                [(equal? promoted #\n) (if (equal? #\8 (string-ref move 3)) (set! final 'N) (set! final 'n))]
                )
           (helper (convert-move move))
           (vector-set! (vector-ref board (list-ref (convert-move move) 3)) (list-ref (convert-move move) 2)
                   final)))
      (helper (convert-move move)))
  
  board)



(define (make-move-n board move)
  (define (helper move)
    (begin
      (vector-set! (vector-ref board (list-ref move 3)) (list-ref move 2)
                   (vector-ref (vector-ref board (list-ref move 1)) (list-ref move 0)))
      (vector-set! (vector-ref board (list-ref move 1)) (list-ref move 0) '-)
      (if (is-move-castling? move)
          (begin (vector-set! (vector-ref board (list-ref (castling-rook-fpos move) 3)) 
                              (list-ref (castling-rook-fpos move) 2)
                              (vector-ref (vector-ref board (list-ref (castling-rook-fpos move) 1))
                                          (list-ref (castling-rook-fpos move) 0)))
                 (vector-set! (vector-ref board (list-ref (castling-rook-fpos move) 1)) (list-ref (castling-rook-fpos move) 0)
                              '-))
          (void))))
  (cond [(is-legal? board move) (if (= 5 (string-length move)) 
      (let ([promoted (string-ref move 4)]
            [final 'tobe]
            )
        (begin 
          (cond [(equal? promoted #\q) (if (equal? #\8 (string-ref move 3)) (set! final 'Q) (set! final 'q))]
                [(equal? promoted #\r) (if (equal? #\8 (string-ref move 3)) (set! final 'R) (set! final 'r))]
                [(equal? promoted #\b) (if (equal? #\8 (string-ref move 3)) (set! final 'B) (set! final 'b))]
                [(equal? promoted #\n) (if (equal? #\8 (string-ref move 3)) (set! final 'N) (set! final 'n))]
                )
           (helper (convert-move move))
           (vector-set! (vector-ref board (list-ref (convert-move move) 3)) (list-ref (convert-move move) 2)
                   final)))
      (helper (convert-move move)))])
  
 )




(define (is-legal? board move)  
  (let* ([m (convert-move move)]
         [col-i (car m)]
         [row-i (cadr m)]
         [col-f (caddr m)]
         [row-f (cadddr m)])
    (define (is-valid?)
      (and (>= col-i 0) (>= row-i 0) (>= col-f 0) (>= row-f 0)
           (<= col-i 7) (<= row-i 7) (<= col-f 7) (<= row-f 7)))
    (define (valid-and-check)
      
      (let ([piece-i (vector-ref (vector-ref board row-i) col-i)]
            [piece-f (vector-ref (vector-ref board row-f) col-f)])
        (define (valid-moves)
          (if (or (is-same-color? piece-i piece-f) (eq? piece-i '-)) #f
              (cond               
                ;for king
                [(or (eq? piece-i 'k) (eq? piece-i 'K))
                 (if (and (<= (abs (- col-i col-f)) 1)
                          (<= (abs (- row-i row-f)) 1))
                     #t 
                     (if (or (and (eq? piece-i 'k) (check board 'b)) 
                             (and (eq? piece-i 'K) (check board 'w)))
                         #f
                         (cond [(and (= row-i 0) (= row-f 0) e8)
                            (cond [(and (= col-f 6) h8 (eq? (vector-ref (vector-ref board 0) 7) 'r)) 
                                   (and (is-row-empty? board 0 4 7)
                                        (let* ([temp (copy-board board)])
                                          (not (check (make-move-forced temp "e8f8") 'b))))]
                                  [(and (= col-f 2) a8 (eq? (vector-ref (vector-ref board 0) 0) 'r)) 
                                   (and (is-row-empty? board 0 4 0)
                                        (let* ([temp (copy-board board)])
                                          (not (check (make-move-forced temp "e8d8") 'b))))]
                                  [else #f])]
                           [(and (= row-i 7) (= row-f 7) e1)
                            (cond [(and (= col-f 6) h1 (eq? (vector-ref (vector-ref board 7) 7) 'R))
                                   (and (is-row-empty? board 7 4 7)
                                        (let ([temp (copy-board board)])
                                          (not (check (make-move-forced temp "e1f1") 'w))))]
                                  [(and (= col-f 2) a1 (eq? (vector-ref (vector-ref board 7) 0) 'R))
                                   (and (is-row-empty? board 7 4 0)
                                        (let ([temp (copy-board board)])
                                          (not (check (make-move-forced temp "e1d1") 'w))))]
                                  [else #f])]
                           [else #f])))]
                ;for white pawn
                [(eq? piece-i 'P)
                 (cond 
                   ;moving-straight
                   [(= col-f col-i) 
                    (cond [(and (= (- row-i row-f) 1) (eq? '- piece-f)) #t]
                          [(= (- row-i row-f) 2)
                           (if (and (= row-i 6)
                                    (eq? '- 
                                         (vector-ref (vector-ref board (- row-i 1)) col-i))
                                    (eq? '- 
                                         (vector-ref (vector-ref board (- row-i 2)) col-i)))
                               #t #f)]
                          [else #f])]
                   ;capture
                   [(and (= (- row-i row-f) 1)
                         (= (abs (- col-f col-i)) 1)
                         (not (eq? '- piece-f))) #t]
                   [else #f])]              
                ;for black pawn
                [(eq? piece-i 'p)
                 (cond 
                   ;moving-straight
                   [(= col-f col-i) 
                    (cond [(and (eq? piece-f '-) (= (- row-f row-i) 1)) #t]
                          [(= (- row-f row-i) 2)
                           (if (and (= row-i 1)
                                    (eq? '- 
                                         (vector-ref (vector-ref board (+ row-i 1)) col-i))
                                    (eq? '- 
                                         (vector-ref (vector-ref board (+ row-i 2)) col-i)))
                               #t #f)]
                          [else #f])]
                   ;capture
                   [(and (= (- row-f row-i) 1)
                         (= (abs (- col-f col-i)) 1)
                         (not (eq? '- piece-f))) #t]
                   [else #f])]              
                ;for rook
                [(or (eq? piece-i 'r) (eq? piece-i 'R))
                 (cond [(= col-i col-f) (is-col-empty? board col-i row-i row-f)]
                       [(= row-i row-f) (is-row-empty? board row-i col-i col-f)]
                       [else #f])]              
                ;for bishop
                [(or (eq? piece-i 'b) (eq? piece-i 'B)) 
                 (if (= (abs (- row-i row-f)) (abs (- col-i col-f)))
                     (is-diag-empty? board col-i row-i col-f row-f)
                     #f)]
                ;for queen
                [(or (eq? piece-i 'q) (eq? piece-i 'Q)) 
                 (cond 
                   ;moving in a column
                   [(= col-i col-f) (is-col-empty? board col-i row-i row-f)]                 
                   ;moving in a row
                   [(= row-i row-f) (is-row-empty? board row-i col-i col-f)]                 
                   ;moving diagonal
                   [(= (abs (- row-i row-f)) (abs (- col-i col-f))) 
                    (is-diag-empty? board col-i row-i col-f row-f)]
                   [else #f])]
                
                ;for knight 
                [(or (eq? piece-i 'n) (eq? piece-i 'N)) 
                 (= 2 (abs (* (- col-i col-f) (- row-i row-f))))])))
        
        (define (check-check)
          (define temp-board 
            (make-move-forced (copy-board board) move))
          (if (is-white-piece? piece-i)
              (not (check temp-board 'w)) 
              (if (is-black-piece? piece-i)
                  (not (check temp-board 'b))
                  #f)))
        (and (valid-moves) (check-check))))
    (cond [(is-valid?) (valid-and-check)]
          [else #f])))
