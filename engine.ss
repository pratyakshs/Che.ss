#lang racket
(require compatibility/mlist)
(require "base.ss")
(require "check.ss")
(require "validmoves.ss")
(require "misc.ss")

;defining a for loop like c++
(provide dep eval-board check best-move no-of-pieces)

(define table '{})
(define ref 0)
(define dep 3)

(define sb (copy-board starting-board))



;Piece-squares-tables

;Pawn
(define pawn-table
  #( #( 0 0 0 0 0 0 0 0)
     #( 50 50 50 50 50 50 50 50)
     #( 10 10 20 30 30 20 10 10)
     #( 5 5 10 25 25 10 5 5)
     #( 0 0 0 20 20 0 0 0)
     #( 5 -5 -10 0 0 -10 -5 5)
     #( 5 10 10 -20 -20 10 10 5)
     #( 0 0 0 0 0 0 0 0)))

;rook
(define rook-table
  #( #( 0 0 0 0 0 0 0 0)
     #(  5 10 10 10 10 10 10 5)
     #(   -5 0 0 0 0 0 0 -5)
     #(   -5 0 0 0 0 0 0 -5)
     #(  -5 0 0 0 0 0 0 -5)
     #(   -5 0 0 0 0 0 0 -5)
     #(  -5 0 0 0 0 0 0 -5)
     #(  0 0 0 5 5 0 0 0)))

;bishop
(define bishop-table
  #( #(  -20 -10 -10 -10 -10 -10 -10 -20)
     #(  -10 0 0 0 0 0 0 -10)
     #(  -10 0 5 10 10 5 0 -10)
     #(  -10 5 5 10 10 5 5 -10)
     #(  -10 0 10 10 10 10 0 -10)
     #(  -10 10 10 10 10 10 10 -10)
     #(  -10 5 0 0 0 0 5 -10)
     #(  -20 -10 -10 -10 -10 -10 -10 -20)
     ))

;knight-table
(define knight-table
  #(  #(  -50 -40 -30 -30 -30 -30 -40 -50)
      #(  -40 -20 0 0 0 0 20 -40)
      #(  -30 0 10 15 15 10 0 -30)
      #(  -30 5 15 20 20 15 5 -30)
      #(  -30 0 15 20 20 15 0 -30)
      #(  -30 5 10 15 15 10 5 -30)
      #(  -40 -20 0 5 5 0 -20 -40)
      #(  -50 -40 -30 -30 -30 -30 -40 -50)))

;queen
(define queen-table
  #(  #( -20 -10 -10 -5 -5 -10 -10 -20)
      #(  -10 0 0 0 0 0 0 -10)
      #(  -10 0 5 5 5 5 0 -10)
      #(  -5 0 5 5 5 5 0 -5)
      #(  0 0 5 5 5 5 0 -5)
      #(  -10 5 5 5 5 5 0 -10)
      #(  -10 0 5 0 0 0 0 -10)
      #(  -20 -10 -10 -5 -5 -10 -10 -20)))

;king middle-game)
(define king-table
  #( #(  -30 -40 -40 -50 -50 -40 -40 -30)
     #(  -30 -40 -40 -50 -50 -40 -40 -30)
     #(  -30 -40 -40 -50 -50 -40 -40 -30)
     #(  -30 -40 -40 -50 -50 -40 -40 -30)
     #( -20 -30 -30 -40 -40 -30 -30 -20)
     #(  -10 -20 -20 -20 -20 -20 -20 -10)
     #(  20 20 0 0 0 0 20 20)
     #(  20 30 10 0 0 10 30 20)))

(define null-table 
  #(  #(  0 0 0 0 0 0 0 0)
      #(  0 0 0 0 0 0 0 0)
      #(  0 0 0 0 0 0 0 0)
      #(  0 0 0 0 0 0 0 0)
      #(  0 0 0 0 0 0 0 0)
      #(  0 0 0 0 0 0 0 0)
      #(  0 0 0 0 0 0 0 0)
      #(  0 0 0 0 0 0 0 0)))

;no of pieces on board
(define (no-of-pieces board) 
  (define sum 0)
  (for ([i 0:7])
    (for ([j 0:7])
      (if (equal? (vector-ref (vector-ref board j) i) '-) void 
          (set! sum (+ sum 1)))))
  sum
  )


;choosing table corresponding to a particular piece
(define (choose-table piece)
  (cond ((or (equal? piece 'P) (equal? piece 'p)) pawn-table)
        ((or (equal? piece 'b) (equal? piece 'B)) bishop-table)
        ((or (equal? piece 'N) (equal? piece 'n)) knight-table)
        ((or (equal? piece 'R) (equal? piece 'r)) rook-table)
        ((or (equal? piece 'q) (equal? piece 'Q)) queen-table)
        ((or (equal? piece 'K) (equal? piece 'k)) king-table)
        (else null-table)
        ))


;assigning values to each piece
(define (piece-val piece)
  (cond ((equal? piece 'p) 100)
        ((equal? piece 'P) 100)
        ((equal? piece 'b) 330)
        ((equal? piece 'B) 330)
        ((equal? piece 'n) 320)
        ((equal? piece 'N) 320)
        ((equal? piece 'r) 500)
        ((equal? piece 'R) 500)
        ((equal? piece 'q) 900)
        ((equal? piece 'Q) 900)
        ((equal? piece 'k) 20000)
        ((equal? piece 'K) 20000)
        (else 0)
        ))

(define (attacked-value pos board color)
  (if (equal? color 'w) 
      (if (ch-hof board pos 'p 'r 'n 'b 'q 'k 'down)
          (ch-hof board pos 'p 'r 'n 'b 'q 'k 'down)
          0)
      (if (ch-hof board pos 'P 'R 'N 'B 'Q 'K 'up)
          (ch-hof board pos 'P 'R 'N 'B 'Q 'K 'up)
          0)))

(define (defended-value pos board color)
  (if (equal? color 'w) 
      
      (if (ch-hof board pos 'P 'R 'N 'B 'Q 'K 'up)
          (ch-hof board pos 'P 'R 'N 'B 'Q 'K 'up)
          0)
      (if (ch-hof board pos 'p 'r 'n 'b 'q 'k 'down)
          (ch-hof board pos 'p 'r 'n 'b 'q 'k 'down)
          0)))


(define (white-pawn-count board)
  (define vec (make-vector 8 0))
  (for ([i 0:7]) 
    (begin (define sum 0) 
           (for ([j 0:7])
             (if (equal? (pick (cons j i) board) 'P)
                 (set! sum (+ sum 1))
                 (set! sum (+ sum 0))))
           (begin (vector-set! vec i sum) (set! sum 0)))) vec)



(define (black-pawn-count board)
  (define vec (make-vector 8 0))
  (for ([i 0:7])
    (begin (define sum 0) 
           (for ([j 0:7])
             (if (equal? (pick (cons j i) board) 'p)
                 (set! sum (+ sum 1))
                 (set! sum (+ sum 0))))
           (begin (vector-set! vec i sum) (set! sum 0)))) vec)

;(define bishop-count 0)

(define (eval-board board turn) ;pos-list no-of-moves-list)
  (define bscore 0)
  (define remainingpieces 0)
  (define blackbishopcount 0)
  (define whitebishopcount 0)
  (define knightcount 0)
  (define whitepawncount (white-pawn-count board))
  (define blackpawncount (black-pawn-count board))
  (define middlegamephase #f)
  (define endgamephase #f)
  (define ch-w #f)
  (define ch-b #f)
  
  ;checkmate
  (define (check-mate board side)
    (if (equal? side 'w)
        (if ch-w (if (best-move board 1 side) #f #t) #f)
        (if ch-b (if (best-move board 1 side) #f #t) #f)))
  
  
  ;piecewise evaluation function
  (define (eval-piece-score board pos endgamephase bishop-count)
    (define score 0)
    (define piece (pick pos board))
    (define color (if (is-white-piece? piece) 'w 'b))
    (define av (attacked-value pos board color))
    (define dv (defended-value pos board color))
    
    (define psqpos (if (is-white-piece? piece) pos (cons (- 7 (car pos)) (cdr pos))))
    (begin 
      (set! score (+ score (piece-val piece)))
      (set! score (+ score dv))
      (set! score (- score av))
      
      (if (is-piece-pawn? piece)
          (if (or (= 0 (cdr pos)) (= 7 (cdr pos))) (set! score (- score 15)) (void))
          
          (begin (set! score (+ score (pick psqpos 
                                            (choose-table (pick pos board))
                                            )))
                 (if (equal? turn color) (void) (if (> av dv) (set! score (- score 10)) (void)))
                 
                 
                 (if (eq? color 'w) 
                     (cond ((> (vector-ref whitepawncount (cdr pos)) 1) (set! score (- score 16)))
                           ((= 1 (car pos)) (if (= av 0) 
                                                (begin (vector-set! whitepawncount (cdr pos) (+ (vector-ref whitepawncount (cdr pos)) 200)) 
                                                       (if (> dv 0) (vector-set! whitepawncount (cdr pos) (+ (vector-ref whitepawncount (cdr pos)) 50))
                                                           (void)))
                                                (void)))
                           ((= 2 (car pos)) (if (= av 0) 
                                                (begin (vector-set! whitepawncount (cdr pos) (+ (vector-ref whitepawncount (cdr pos)) 100)) 
                                                       (if (> dv 0) (vector-set! whitepawncount (cdr pos) (+ (vector-ref whitepawncount (cdr pos)) 25))
                                                           (void)))
                                                (void))))
                     (cond ((> (vector-ref blackpawncount (cdr pos)) 1) (set! score (- score 16)))
                           ((= 6 (car pos)) (if (= av 0) 
                                                (begin (vector-set! blackpawncount (cdr pos) (+ (vector-ref blackpawncount (cdr pos)) 200)) 
                                                       (if (> dv 0) (vector-set! blackpawncount (cdr pos) (+ (vector-ref blackpawncount (cdr pos)) 50))
                                                           (void)))
                                                (void)))
                           ((= 5 (car pos)) (if (= av 0) 
                                                (begin (vector-set! blackpawncount (cdr pos) (+ (vector-ref blackpawncount (cdr pos)) 100)) 
                                                       (if (> dv 0) (vector-set! blackpawncount (cdr pos) (+ (vector-ref blackpawncount (cdr pos)) 25))
                                                           (void)))
                                                (void)))))))
      (if (is-piece-knight? piece)
          (begin (set! score (+ score (pick psqpos 
                                            knight-table)))
                 (if (equal? turn color) (void) (if (> av dv) (set! score (- score 30)) (void)))
                 (if (and (not (= av 0)) (= (remainder av 6) 0)) (set! score (- score 10)) (void))
                 (if endgamephase (set! score (- score 10)) (void))
                 
                 ) (void))
      
      (if (is-piece-bishop? piece)
          (begin (cond ((eq? color 'w) (set! whitebishopcount (+ 1 whitebishopcount))
                                       (if (>= whitebishopcount 2) (set! score (+ score 10))
                                           (void)))
                       
                       (else (set! blackbishopcount (+ blackbishopcount 1))
                             (if (>= blackbishopcount 2) (set! score (+ score 10))
                                 (void))))
                 (if endgamephase (set! score (+ score 10)) 
                     (void))
                 (if (equal? turn color) (void) (if (> av dv) (set! score (- score 30)) (void)))
                 (if (and (not (= av 0)) (= (remainder av 6) 0)) (set! score (- score 10)) (void))  
                 (set! score (+ score (pick psqpos bishop-table)))
                 ) (void))
      (if (is-piece-rook? piece)
          (begin 
            (set! score (+ score (pick psqpos rook-table)))
            (if (equal? turn color) (void) (if (> av dv) (set! score (- score 50)) (void)))
            (if (and (not (= av 0)) (= (remainder av 6) 0)) (set! score (- score 20)) (void))
            (void)) (void))
      
      
      
      (if (is-piece-queen? piece)
          (begin
            (set! score (+ score (pick psqpos queen-table)))
            (if (equal? turn color) (void) (if (> av dv) (set! score (- score 90)) (void)))
            (if (and (not (= av 0)) (= (remainder av 6) 0)) (set! score (- score 50)) (void)) 
            (void))
          (void))
      (if (is-piece-king? piece)
          (begin
            (set! score (+ score (pick psqpos king-table)))
            
            )
          (void))
      
      ) score)
  
  
  
  
  
  (for ([i 0:7])
    (for ([j 0:7])
      (let* ([piece (pick (cons i j) board)])
        (begin 
          
          (cond ((not (eq? piece '-)) 
                 (if (is-white-piece? piece) (set! bscore (- bscore (eval-piece-score board (cons i j) whitebishopcount #f)))
                     (set! bscore (+ bscore (eval-piece-score board (cons i j) blackbishopcount #f))))
                 (set! remainingpieces (+ 1 remainingpieces)) ))
          (if (is-piece-knight? piece) (set! knightcount (+ knightcount 1))
              (void))
          ))))
  (begin 
    
    (if (<= remainingpieces 10) (set! endgamephase #t) (void))
    (if (not w-castled) (if (w-king-moved?) (set! bscore (+ bscore 30)) (void)) (set! bscore (- bscore 30)))
    (if (not b-castled) (if (b-king-moved?) (set! bscore (- bscore 30)) (void)) (set! bscore (+ bscore 30))) 
    
    (if (check-mate board 'w) (set! bscore +33000) (void))
    (if (check-mate board 'b) (set! bscore -33000) (void))
    
    (if (check board 'w) (begin (set! ch-w #t) (set! bscore (+ bscore 10)))
        (if (check board 'b) (begin (set! ch-b #t) (set! bscore (- bscore 10))) 
            (void)))
    
    (cond [(and (>= (vector-ref blackpawncount 0) 1)
                (= (vector-ref blackpawncount 1) 0)) (set! bscore (- bscore 12))]
          [(and (= (vector-ref blackpawncount 0) 0)
                (>= (vector-ref blackpawncount 1) 1)
                (= (vector-ref blackpawncount 2) 0)
                ) (set! bscore (- bscore 14))]
          [(and (= (vector-ref blackpawncount 1) 0)
                (>= (vector-ref blackpawncount 2) 1)
                (= (vector-ref blackpawncount 3) 0)
                ) (set! bscore (- bscore 16))]
          [(and (= (vector-ref blackpawncount 2) 0)
                (>= (vector-ref blackpawncount 3) 1)
                (= (vector-ref blackpawncount 4) 0)
                ) (set! bscore (- bscore 20))]
          [(and (= (vector-ref blackpawncount 3) 0)
                (>= (vector-ref blackpawncount 4) 1)
                (= (vector-ref blackpawncount 5) 0)
                ) (set! bscore (- bscore 20))]
          [(and (= (vector-ref blackpawncount 4) 0)
                (>= (vector-ref blackpawncount 5) 1)
                (= (vector-ref blackpawncount 6) 0)
                ) (set! bscore (- bscore 16))]
          [(and (= (vector-ref blackpawncount 5) 0)
                (>= (vector-ref blackpawncount 6) 1)
                (= (vector-ref blackpawncount 7) 0)
                ) (set! bscore (- bscore 14))]
          [(and (= (vector-ref blackpawncount 6) 0)
                (>= (vector-ref blackpawncount 7) 1)
                ) (set! bscore (- bscore 12))])
    (cond [(and (>= (vector-ref whitepawncount 0) 1)
                (= (vector-ref whitepawncount 1) 0)) (set! bscore (+ bscore 12))]
          [(and (= (vector-ref whitepawncount 0) 0)
                (>= (vector-ref whitepawncount 1) 1)
                (= (vector-ref whitepawncount 2) 0)
                ) (set! bscore (+ bscore 14))]
          [(and (= (vector-ref whitepawncount 1) 0)
                (>= (vector-ref whitepawncount 2) 1)
                (= (vector-ref whitepawncount 3) 0)
                ) (set! bscore (+ bscore 16))]
          [(and (= (vector-ref whitepawncount 2) 0)
                (>= (vector-ref whitepawncount 3) 1)
                (= (vector-ref whitepawncount 4) 0)
                ) (set! bscore (+ bscore 20))]
          [(and (= (vector-ref whitepawncount 3) 0)
                (>= (vector-ref whitepawncount 4) 1)
                (= (vector-ref whitepawncount 5) 0)
                ) (set! bscore (+ bscore 20))]
          [(and (= (vector-ref whitepawncount 4) 0)
                (>= (vector-ref whitepawncount 5) 1)
                (= (vector-ref whitepawncount 6) 0)
                ) (set! bscore (+ bscore 16))]
          [(and (= (vector-ref whitepawncount 5) 0)
                (>= (vector-ref whitepawncount 6) 1)
                (= (vector-ref whitepawncount 7) 0)
                ) (set! bscore (+ bscore 14))]
          [(and (= (vector-ref whitepawncount 6) 0)
                (>= (vector-ref whitepawncount 7) 1)
                ) (set! bscore (+ bscore 12))])
    (cond [(and (>= (vector-ref blackpawncount 0) 1)
                (= (vector-ref whitepawncount 0) 0)) 
           (set! bscore (+ bscore (vector-ref blackpawncount 0)))]
          [(and (>= (vector-ref blackpawncount 1) 1)
                (= (vector-ref whitepawncount 1) 0)) 
           (set! bscore (+ bscore (vector-ref blackpawncount 1)))]
          [(and (>= (vector-ref blackpawncount 2) 1)
                (= (vector-ref whitepawncount 2) 0)) 
           (set! bscore (+ bscore (vector-ref blackpawncount 2)))]
          [(and (>= (vector-ref blackpawncount 3) 1)
                (= (vector-ref whitepawncount 3) 0)) 
           (set! bscore (+ bscore (vector-ref blackpawncount 3)))]
          [(and (>= (vector-ref blackpawncount 4) 1)
                (= (vector-ref whitepawncount 4) 0)) 
           (set! bscore (+ bscore (vector-ref blackpawncount 4)))]
          [(and (>= (vector-ref blackpawncount 5) 1)
                (= (vector-ref whitepawncount 5) 0)) 
           (set! bscore (+ bscore (vector-ref blackpawncount 5)))]
          [(and (>= (vector-ref blackpawncount 6) 1)
                (= (vector-ref whitepawncount 6) 0)) 
           (set! bscore (+ bscore (vector-ref blackpawncount 6)))]
          [(and (>= (vector-ref blackpawncount 7) 1)
                (= (vector-ref whitepawncount 7) 0)) 
           (set! bscore (+ bscore (vector-ref blackpawncount 7)))])
    (cond [(and (>= (vector-ref whitepawncount 0) 1)
                (= (vector-ref blackpawncount 0) 0)) 
           (set! bscore (+ bscore (vector-ref whitepawncount 0)))]
          [(and (>= (vector-ref whitepawncount 1) 1)
                (= (vector-ref blackpawncount 1) 0)) 
           (set! bscore (+ bscore (vector-ref whitepawncount 1)))]
          [(and (>= (vector-ref whitepawncount 2) 1)
                (= (vector-ref blackpawncount 2) 0)) 
           (set! bscore (+ bscore (vector-ref whitepawncount 2)))]
          [(and (>= (vector-ref whitepawncount 3) 1)
                (= (vector-ref blackpawncount 3) 0)) 
           (set! bscore (+ bscore (vector-ref whitepawncount 3)))]
          [(and (>= (vector-ref whitepawncount 4) 1)
                (= (vector-ref blackpawncount 4) 0)) 
           (set! bscore (+ bscore (vector-ref whitepawncount 4)))]
          [(and (>= (vector-ref whitepawncount 5) 1)
                (= (vector-ref blackpawncount 5) 0)) 
           (set! bscore (+ bscore (vector-ref whitepawncount 5)))]
          [(and (>= (vector-ref whitepawncount 6) 1)
                (= (vector-ref blackpawncount 6) 0)) 
           (set! bscore (+ bscore (vector-ref whitepawncount 6)))]
          [(and (>= (vector-ref blackpawncount 7) 1)
                (= (vector-ref blackpawncount 7) 0)) 
           (set! bscore (+ bscore (vector-ref whitepawncount 7)))]) 
    (* 1.0 bscore)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (move-format? m)
  (and (= 4 (string-length m))
       (>= (char->integer (string-ref m 0)) 97)
       (<= (char->integer (string-ref m 0)) 104)
       (>= (char->integer (string-ref m 1)) 49)
       (<= (char->integer (string-ref m 1)) 56)
       (>= (char->integer (string-ref m 2)) 97)
       (<= (char->integer (string-ref m 2)) 104)
       (>= (char->integer (string-ref m 3)) 49)
       (<= (char->integer (string-ref m 3)) 56)
       ))


(define (change-color s)
  (if (eq? s 'w) 'b 'w))

(define (num->char symbol)
  (cond [(equal? symbol 0) #\a]
        [(equal? symbol 1) #\b]
        [(equal? symbol 2) #\c]
        [(equal? symbol 3) #\d]
        [(equal? symbol 4) #\e]
        [(equal? symbol 5) #\f]
        [(equal? symbol 6) #\g]
        [(equal? symbol 7) #\h]
        [else #\a]
        )) 
;-----------------------------------------------------------------
(define (indices->notation l)
  (list->string (list (num->char (car l))
                      (integer->char (+ 48 8 (- (cadr l))))
                      (num->char (caddr l))
                      (integer->char (+ 48 8 (- (cadddr l)))))))
;------------------------------------------------------------------

(define (best-move node i-depth i-color)
  (define move-selector '{})
  
  (define (alpha-beta node depth alpha beta color move-str)
    (define a alpha)
    (define b beta)
    (define (helper board)
      
      (define (generate-helper x y board)
        
        (define (check-and-add-to-list a1 b1 c1 d1)
          (let ([m (indices->notation (list a1 b1 c1 d1))]
                )
            (define bb (copy-board board))
            (cond [(is-legal? board m)
                   (if (eq? color 'b)
                       (set! a (max a (alpha-beta (make-move-forced bb m) (- depth 1) a b (change-color color) m)))
                       (set! b (min b (alpha-beta (make-move-forced bb m) (- depth 1) a b (change-color color) m))))])))
        (define (pawn-pro a1 b1 c1 d1)
          (let ([m (string-append (indices->notation (list a1 b1 c1 d1)) "q")]
                )
            (define bb (copy-board board))
            (cond [(is-legal? board m)
                   (if (eq? color 'b)
                       (set! a (max a (alpha-beta (make-move-forced bb m) (- depth 1) a b (change-color color) m)))
                       (set! b (min b (alpha-beta (make-move-forced bb m) (- depth 1) a b (change-color color) m))))])))
        (begin 
          (let ([curr-piece (vector-ref (vector-ref board y) x)])
            (cond [(or (eq? curr-piece 'K) (eq? curr-piece 'k))
                   ;for king.
                   (for ([p (list (- x 2) (- x 1) x (+ x 1) (+ x 2))])
                     (for ([q (list (- y 1) y (+ y 1))]
                           #:break (<= b a))
                       (check-and-add-to-list x y p q)))]
                  [(or (eq? curr-piece 'R) (eq? curr-piece 'r))
                   ;for rook
                   ;moving in a column
                   (for ([p 0:7]
                         #:break (<= b a))
                     (check-and-add-to-list x y p y))
                   ;moving in a row
                   (for ([q 0:7]
                         #:break (<= b a))
                     (check-and-add-to-list x y x q))]
                  ;for bishop
                  [(or (eq? curr-piece 'B) (eq? curr-piece 'b))
                   ;main diagonal
                   (let ([s (- x y)])
                     (for ([p 0:7])
                       (for ([q 0:7]
                             #:when (= (- p q) s)
                             #:break (<= b a))
                         (check-and-add-to-list x y p q))))
                   ;other diagonal
                   (let ([s (+ x y)])
                     (for ([p 0:7])
                       (for ([q 0:7]
                             #:when (= (+ p q) s)
                             #:break (<= b a))
                         (check-and-add-to-list x y p q))))]
                  ;for queen
                  [(or (eq? curr-piece 'Q) (eq? curr-piece 'q))
                   ;moving in a column
                   (for ([p 0:7]
                         #:break (<= b a))
                     (check-and-add-to-list x y p y))
                   ;moving in a row
                   (for ([q 0:7]
                         #:break (<= b a))
                     (check-and-add-to-list x y x q))
                   ;main diagonal
                   (let ([s (- x y)])
                     (for ([p 0:7])
                       (for ([q 0:7]
                             #:when (= (- p q) s)
                             #:break (<= b a))
                         (check-and-add-to-list x y p q))))
                   ;other diagonal
                   (let ([s (+ x y)])
                     (for ([p 0:7])
                       (for ([q 0:7]
                             #:when (= (+ p q) s)
                             #:break (<= b a))
                         (check-and-add-to-list x y p q))))]
                  ;knight
                  [(or (eq? curr-piece 'N) (eq? curr-piece 'n))
                   (for ([p (list (+ x 1) (- x 1)
                                  (- x 2) (+ x 2)
                                  (- x 2) (- x 1)
                                  (+ x 1) (+ x 2))]
                         [q (list (- y 2) (- y 2)
                                  (- y 1) (- y 1)
                                  (+ y 1) (+ y 2) (+ y 2)
                                  (+ y 1))])
                     (check-and-add-to-list x y p q))]
                  ;black pawn
                  [(eq? curr-piece 'p)
                   (for ([p (list (- x 1) x (+ x 1) x)]
                         [q (list (+ y 1) (+ y 1) (+ y 1) (+ y 2))]
                         #:break (<= b a))
                     (if (= y 7) 
                         (pawn-pro x y p q)
                         (check-and-add-to-list x y p q)))]
                  ;white pawn
                  [(eq? curr-piece 'P)
                   (for ([p (list (- x 1) x (+ x 1) x)]
                         [q (list (- y 1) (- y 1) (- y 1) (- y 2))]
                         #:break (<= b a))
                     (if (= y 0) 
                         (pawn-pro x y p q)
                         (check-and-add-to-list x y p q)))]))))
      
      (define (main)
        (for ([j 0:7])
          (for ([i 0:7]
                #:break (<= b a))
            (let ([piece (vector-ref (vector-ref board j) i)])
              (cond [(eq? piece '-) #f]
                    [else (cond [(eq? color 'w) 
                                 (cond [(or (eq? piece 'P) (eq? piece 'B) (eq? piece 'N)
                                            (eq? piece 'R) (eq? piece 'Q) (eq? piece 'K))
                                        (generate-helper i j board)])]
                                [(eq? color 'b) 
                                 (cond [(or (eq? piece 'p) (eq? piece 'b) (eq? piece 'n)
                                            (eq? piece 'r) (eq? piece 'q) (eq? piece 'k))
                                        (generate-helper i j board)])])]))))
        (if (eq? color 'b) a b))
      (let ([val (main)])
        (cond [(= depth (- i-depth 1)) (set! move-selector (mappend move-selector (mlist (mcons val move-str))))])
        val))
    (cond [(and (= depth 0) (= i-depth 1)) 
           (begin (set! move-selector (mappend move-selector (mlist (mcons 'exception move-str)))) 
                  0)]
          [(= depth 0)  (eval-board node color)
                        ]
          [else (helper node)]))
  
  (let ([root-val (alpha-beta node i-depth -inf.0 +inf.0 i-color "abc")])
    (if (= i-depth 1) (massoc 'exception move-selector) (massoc root-val move-selector))))

