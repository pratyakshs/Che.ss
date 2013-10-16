#lang racket
(provide match-w match-b w-castled b-castled check-mate-w check-mate-b check-m-w check-m-b)

(define w-castled #f)
(define b-castled #f)
(define check-m-w #f)
(define check-m-b #f)
(define (check-mate-w val) (if val (set! check-m-w #t) (set! check-m-w #f)))
(define (check-mate-b val) (if val (set! check-m-b #t) (set! check-m-b #f)))
(define (match-w val)
  (set! w-castled val))
(define (match-b val)
  (set! b-castled val))