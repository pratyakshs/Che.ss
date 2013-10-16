#lang racket
(provide pick find ch-hof check is-piece-pawn? is-piece-queen? is-piece-rook? is-piece-knight? is-piece-bishop?
         is-piece-king? 
         ) 
(define-syntax for
  (syntax-rules (:)                                          ;[pattern1 expand1]
    ;[pattern2 expand2]
    [(for init : bexp : change : statements)                
     (begin 
       init
       (define (loop)
         (cond [bexp (begin 
                       statements
                       change
                       (loop))]))
       (loop))]))



;element picker from 2d vector
(define (pick pos table)
  (vector-ref (vector-ref table (car pos)) (cdr pos)))



;finding a particular piece (specially king) from the board and returning its position
(define (find piece board)
  (define pos (cons 0 0))
  (for (define i 0) : (< i 8) : (set! i (+ i 1)) :
    
    (for (define j 0) : (< j 8) : (set! j (+ j 1)) :
      (cond ((equal? piece (pick (cons i j) board)) (set! pos (cons i j)))))
    )
  pos
  )

;checking if the given position is within the board or not
(define (within-range r c)
  (and (>= r 0) (<= r 7) (>= c 0) (<= c 7)))

(define (is-piece-pawn? p)
  (or (eq? p 'p) (eq? p 'P)))

(define (is-piece-knight? p)
  (or (eq? p 'n) (equal? p 'N)))

(define (is-piece-bishop? p)
  (or (eq? p 'b) (equal? p 'B)))

(define (is-piece-rook? p)
  (or (eq? p 'r) (equal? p 'R)))

(define (is-piece-queen? p)
  (or (eq? p 'q) (equal? p 'Q)))

(define (is-piece-king? p)
  (or (eq? p 'k) (equal? p 'K)))



(define (ch-hof board kpos p r n b q k pawnmovement)
  (define psc 0)
  (define rsc 0)
  (define nsc 0)
  (define bsc 0)
  (define qsc 0)
  (define (helper-c)
    ;(define kpos pos)
    
    
    ;pawn
    (define (check-pawn)
      (define (ch-pawn-up)
        (if (< (car kpos) 7)
            (begin 
              (if (within-range (+ 1 (car kpos)) (+ 1 (cdr kpos))) 
                  (if (equal? (pick (cons (+ (car kpos) 1) (+ (cdr kpos) 1)) board) p) (set! rsc (+ rsc 6)) (void)) (void))
              (if (within-range (+ 1 (car kpos)) (- (cdr kpos) 1))
                  (if (equal? (pick (cons (+ (car kpos) 1) (- (cdr kpos) 1)) board) p) (set! rsc (+ rsc 6))
                      (set! rsc (+ rsc 0))) (set! rsc (+ rsc 0)))) (void)))
      
      (define (ch-pawn-down)
        (if (> (car kpos) 0)
            (begin
              (if (within-range (- (car kpos) 1) (- (cdr kpos) 1)) 
                  (if (equal? (pick (cons (- (car kpos) 1) (- (cdr kpos) 1)) board) p) (set! rsc (+ rsc 6)) (void)) (void))
              (if (within-range (- (car kpos) 1) (+ (cdr kpos) 1))
                  (if (equal? (pick (cons (- (car kpos) 1) (+ (cdr kpos) 1)) board) p) (set! rsc (+ rsc 6))
                      (set! rsc (+ rsc 0))) (set! rsc (+ rsc 0)))) (void)))
      
      (if (eq? pawnmovement 'up) (ch-pawn-up) (ch-pawn-down)))
    
    
    ;hof for rook-check
    (define (check-rook-hof with sym op r-or-c ele)
      (define (check-rook-hof-c ele)
        (define id (if (equal? op -) 0 7))
        (if (sym ele id) (set! rsc (+ rsc 0))
            (if (equal? (pick (cons (car kpos) ele) board) with) (set! rsc (+ rsc 2))
                (if (equal? (pick (cons (car kpos) ele) board) '-)
                    (check-rook-hof-c (op ele 1)) (set! rsc (+ rsc 0))))))
      (define (check-rook-hof-r ele)
        (define id (if (equal? op -) 0 7))
        (if (sym ele id) (set! rsc (+ rsc 0))
            (if (equal? (pick (cons ele (cdr kpos)) board) with) (set! rsc (+ rsc 2))
                (if (equal? (pick (cons ele (cdr kpos)) board) '-)
                    (check-rook-hof-r (op ele 1)) (set! rsc (+ rsc 0))))))
      (if (equal? r-or-c 'c) (check-rook-hof-c ele)
          (check-rook-hof-r ele)))
    
    
    
    ;rook (putting an argument so that it can be used with check-queen just by changing
    ;the argument to 'Q)
    (define (check-rook with)
      (begin (if (within-range (car kpos) (+ 1 (cdr kpos)))
                 (check-rook-hof with > + 'c (+ 1 (cdr kpos))) (set! rsc (+ rsc 0)))
             (if (within-range (car kpos) (- (cdr kpos) 1))
                 (check-rook-hof with < - 'c (- (cdr kpos) 1)) (set! rsc (+ rsc 0)))
             (if (within-range (+ 1 (car kpos)) (cdr kpos))
                 (check-rook-hof with > + 'r (+ 1 (car kpos))) (set! rsc (+ rsc 0)))
             (if (within-range (- (car kpos) 1) (cdr kpos)) 
                 (check-rook-hof with < - 'r (- (car kpos) 1)) (set! rsc (+ rsc 0)))))
    
    ;hof for check-bishop
    (define (check-bishop-hof with sym1 sym2 op1 op2 ele1 ele2 id1 id2)
      (if (or (sym1 ele1 id1) (sym2 ele2 id2)) (set! rsc (+ rsc 0))
          (if (equal? (pick (cons ele1 ele2) board) with) (set! rsc (+ rsc 3))
              (if (equal? (pick (cons ele1 ele2) board) '-)
                  (check-bishop-hof with sym1 sym2 op1 op2 (op1 ele1 1) (op2 ele2 1) id1 id2) (set! rsc (+ rsc 0))))))
    
    ;bishop (putting an argument so that it can be used with check-queen just by changing
    ;the argument to 'Q)
    (define (check-bishop with)
      (begin  (if (within-range (+ 1 (car kpos)) (+ 1 (cdr kpos)))
                  (check-bishop-hof with > > + + (+ 1 (car kpos)) (+ 1 (cdr kpos)) 7 7) (set! rsc (+ rsc 0)))
              (if (within-range (- (car kpos) 1) (+ 1 (cdr kpos)))
                  (check-bishop-hof with < > - + (- (car kpos) 1) (+ 1 (cdr kpos)) 0 7) (set! rsc (+ rsc 0)))
              (if (within-range (- (car kpos) 1) (- (cdr kpos) 1))
                  (check-bishop-hof with < < - - (- (car kpos) 1) (- (cdr kpos) 1) 0 0) (set! rsc (+ rsc 0)))
              (if (within-range (+ 1 (car kpos)) (- (cdr kpos) 1))
                  (check-bishop-hof with > < + - (+ 1 (car kpos)) (- (cdr kpos) 1) 7 0) (set! rsc (+ rsc 0)))))
    
    ;knight
    (define (check-knight)
      (begin (if (within-range (+ 1 (car kpos)) (+ 2 (cdr kpos))) 
                 (if (equal? (pick (cons (+ 1 (car kpos)) (+ 2 (cdr kpos))) board) n) (set! rsc (+ rsc 3)) (set! rsc (+ rsc 0))) (set! rsc (+ rsc 0)))
             (if (within-range (+ 1 (car kpos)) (- (cdr kpos) 2)) 
                 (if (equal? (pick (cons (+ 1 (car kpos)) (- (cdr kpos) 2)) board) n) (set! rsc (+ rsc 3)) (set! rsc (+ rsc 0))) (set! rsc (+ rsc 0)))
             (if (within-range (+ 2 (car kpos)) (+ 1 (cdr kpos))) 
                 (if (equal? (pick (cons (+ 2 (car kpos)) (+ 1 (cdr kpos))) board) n) (set! rsc (+ rsc 3)) (set! rsc (+ rsc 0))) (set! rsc (+ rsc 0)))
             (if (within-range (+ 2 (car kpos)) (- (cdr kpos) 1)) 
                 (if (equal? (pick (cons (+ 2 (car kpos)) (- (cdr kpos) 1)) board) n) (set! rsc (+ rsc 3)) (set! rsc (+ rsc 0))) (set! rsc (+ rsc 0)))
             (if (within-range (- (car kpos) 1) (+ 2 (cdr kpos))) 
                 (if (equal? (pick (cons (- (car kpos) 1) (+ 2 (cdr kpos))) board) n) (set! rsc (+ rsc 3)) (set! rsc (+ rsc 0))) (set! rsc (+ rsc 0)))
             (if (within-range (- (car kpos) 1) (- (cdr kpos) 2)) 
                 (if (equal? (pick (cons (- (car kpos) 1) (- (cdr kpos) 2)) board) n) (set! rsc (+ rsc 3)) (set! rsc (+ rsc 0))) (set! rsc (+ rsc 0)))
             (if (within-range (- (car kpos) 2) (+ 1 (cdr kpos))) 
                 (if (equal? (pick (cons (- (car kpos) 2) (+ 1 (cdr kpos))) board) n) (set! rsc (+ rsc 3)) (set! rsc (+ rsc 0))) (set! rsc (+ rsc 0)))
             (if (within-range (- (car kpos) 2) (- (cdr kpos) 2)) 
                 (if (equal? (pick (cons (- (car kpos) 2) (- (cdr kpos) 1)) board) n) (set! rsc (+ rsc 3)) (set! rsc (+ rsc 0))) (set! rsc (+ rsc 0)))))
    
    ;queen
    (define (check-queen)
      (begin (check-rook q) (check-bishop q)))
    
    ;king
    (define (check-king)
      (if (or (if (within-range (- (car kpos) 1) (- (cdr kpos) 1)) (equal? (pick (cons (- (car kpos) 1) (- (cdr kpos) 1)) board) k) #f)
              (if (within-range (- (car kpos) 1) (+ (cdr kpos) 1)) (equal? (pick (cons (- (car kpos) 1) (+ (cdr kpos) 1)) board) k) #f)
              (if (within-range (+ (car kpos) 1) (- (cdr kpos) 1)) (equal? (pick (cons (+ (car kpos) 1) (- (cdr kpos) 1)) board) k) #f)
              (if (within-range (+ (car kpos) 1) (+ (cdr kpos) 1)) (equal? (pick (cons (+ (car kpos) 1) (+ (cdr kpos) 1)) board) k) #f)
              (if (within-range (- (car kpos) 0) (- (cdr kpos) 1)) (equal? (pick (cons (- (car kpos) 0) (- (cdr kpos) 1)) board) k) #f)
              (if (within-range (- (car kpos) 0) (+ (cdr kpos) 1)) (equal? (pick (cons (- (car kpos) 0) (+ (cdr kpos) 1)) board) k) #f)
              (if (within-range (- (car kpos) 1) (- (cdr kpos) 0)) (equal? (pick (cons (- (car kpos) 1) (- (cdr kpos) 0)) board) k) #f)
              (if (within-range (+ (car kpos) 1) (- (cdr kpos) 0)) (equal? (pick (cons (+ (car kpos) 1) (- (cdr kpos) 0)) board) k) #f))
          (set! rsc (+ rsc 1)) (void)))
    (begin (check-pawn)
           (check-rook r)
           (check-knight)
           (check-bishop b)
           (check-queen)
           (check-king)
           )
    ;check from any one of the pieces
    )
  (begin (helper-c) (if (= rsc 0) #f rsc)))


;for checking check
(define (check board side)
  
  
  (if (equal? side 'w) (if (ch-hof board (find 'K board) 'p 'r 'n 'b 'q 'k 'down) #t 
                           (ch-hof board (find 'K board) 'p 'r 'n 'b 'q 'k 'down))
      (if 
      (ch-hof board (find 'k board) 'P 'R 'N 'B 'Q 'K 'up) #t 
      (ch-hof board (find 'k board) 'P 'R 'N 'B 'Q 'K 'up)
      )))
