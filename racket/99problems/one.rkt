#lang racket

;; no 01

;; last :: [a] -> a
(define/match (last xs)
  [('()) null]
  [((list a)) a]
  [((list a b ...)) (last b)])

;;  no 3

;; nth :: [a] -> Int -> a
(define/match (nth xs n)
  [('[] _) null]
  [((list a _ ...) 0) a]
  [((list a b ...) n) (nth b (- n 1))])

;; no 4
;; count :: [a] -> Int
(define/match (count xs)
  [('[]) 0]
  [((list a)) 1]
  [((list a b ...)) (+ 1 (count b))])

;; no 5 
;; revs :: [a] -> [a]
(define/match (revs xs)
  [('[]) '[]]
  [((list a b ...)) (append (revs b) (list a))])

;; no 6
;; palin? :: [a] -> Bool
(define (palin? xs)
  (equal? xs (revs xs)))

;; def NList = [ a | NList ]

;; no 7
;; flatten a nested list
;; flatten :: NList -> [a]
(define/match (flatten xs)
  [('[]) '[]]
  [((list x xxs ...)) 
   (if (list? x)
       (append (flatten x) (flatten xxs))
       (cons x (flatten xxs)))])

;; no 8
;; eliminates consecutive duplicates of list elements
;; remdupli :: [a] -> [a]
(define/match (remdupli xs)
  [('[]) '[]]
  [((list a)) (list a)]
  [((list a b)) 
   (if (= a b) (list a) (list a b))]
  [((list a b c ...)) 
   (if (= a b) (cons a (remdupli c)) (cons a (remdupli (cons b c))))])

;; no 9 

(define (pack lst)
  (define (iter xlst lelmt llst res)
    (cond [(empty? xlst) (append res (list llst))]
          [(= lelmt (first xlst)) 
           (iter (rest xlst) lelmt (cons lelmt llst) res)]
          [else (iter (rest xlst)
                      (first xlst)
                      (list (first xlst)) 
                      (append res (list llst)))]))
  (if (empty? lst)
      null
      (iter lst (first lst) '() '())))














