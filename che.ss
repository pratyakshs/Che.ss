#lang racket
(require compatibility/mlist)
(require "engine.ss")
(require "validmoves.ss")
(require "base.ss")
(require "misc.ss")

(define b (copy-board starting-board))

(define (parser str)
  (define (parser-l l)
    (let ([state #t])
      (define (helper ans lh)
        (if (null? lh) ans
            (cond [(and (eq? (car lh) #\space) state)
                   (helper ans (cdr lh))]
                  [(and (eq? (car lh) #\space) (not state)) ans]
                  [else (begin (set! state #f)
                               (helper (append ans (list (car lh))) (cdr lh)))])))
      (list->string (helper '() l))))
  (parser-l (string->list str)))

(define (move-format? str)
  (and (string? str) 
       (eq? (string-length str) 4) 
       (let ([a (char->integer (string-ref str 0))]
             [b (char->integer (string-ref str 1))]
             [c (char->integer (string-ref str 2))]
             [d (char->integer (string-ref str 3))])
         (and 
          (>= a 97) (<= a 104)
          (>= b 49) (<= b 56)  
          (>= c 97) (<= c 104)
          (>= d 49) (<= d 56)))))

(define (pawn-promotion? str)
  (and (= (string-length str) 5)
       (move-format? (substring str 0 4))
       (or (eq? (string-ref str 4) #\q)
           (eq? (string-ref str 4) #\b)
           (eq? (string-ref str 4) #\r)
           (eq? (string-ref str 4) #\n))))

(define (main)
  (define (actions str)
    (if (equal? str "quit")
        void
        (begin (let* ([i (parser str)])
                 (cond [(equal? i "xboard") void ]
                       [(move-format? i)
                        (make-move-n b i) 
                        (cond [(or (equal? i "e1c1") (equal? i "e1g1")) (match-w #t)])
                        (let ([s (substring i 0 2)])
                          (cond [(string=? s "a1") (a1!)]
                                [(string=? s "e1") (e1!)]
                                [(string=? s "h1") (h1!)]
                                [(string=? s "a8") (a8!)]
                                [(string=? s "e8") (e8!)]
                                [(string=? s "h8") (h8!)]))
                        (cond [#t (let ([m (mcdr (best-move b 3 'b))])
                                    ;                          
                                    (make-move-forced b m)
                                    (let ([s (substring m 0 2)])
                                      (cond [(string=? s "a1") (a1!)]
                                            [(string=? s "e1") (e1!)]
                                            [(string=? s "h1") (h1!)]
                                            [(string=? s "a8") (a8!)]
                                            [(string=? s "e8") (e8!)]
                                            [(string=? s "h8") (h8!)]))
                                    (cond [(or (equal? m "e8c8") (equal? m "e8g8")) (match-b #t)])
                                    
                                    (display (string-append "move " m)) (newline) (flush-output))])]
                       [(pawn-promotion? i)
                        (make-move-n b i) 
                        (let ([m (mcdr (best-move b 3 'b))])     
                          (make-move-forced b m)
                          (display (string-append "move " m)) (newline) (flush-output))]
                       [(equal? i "protover") (display "feature sigint=0 sigterm=0 setboard=1 done=1") (newline) (flush-output)]
                       [(equal? i "new") (set! b starting-board)]
                       ))
               (actions (read-line)))))
  (actions (read-line)))
(main)