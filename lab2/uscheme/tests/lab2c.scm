;;
;; Functions and tests for part C of assignment 2.
;;

;; ---------------------------------------------------------------------- 
;; Functions with fixed numbers of arguments. 
;; ---------------------------------------------------------------------- 

;; map : (a -> b) (list a) -> (list b)
(define map (f lst)
  (if (null? lst)
    nil
    (cons (f (car lst) ) (map f (cdr lst)))
  )
)

(check-expect (map (lambda (n) (* n 2)) nil) nil)
(check-expect (map (lambda (n) (* n 2)) '(1 2 3 4 5)) '(2 4 6 8 10))

;; filter : (a -> bool) (list a) -> (list a)
(define filter (f lst)
  (cond
    ( (null? lst) nil )
    ( (= #f (f (car lst))) (filter f (cdr lst)) )
    ( #t (cons (car lst) (filter f (cdr lst))) )
  )
)

(check-expect (filter (lambda (n) (> n 0)) nil) nil)
(check-expect (filter (lambda (n) (> n 0)) '(-1 1 -2 2 -3 3)) '(1 2 3))

;; exists? : (a -> bool) (list a) -> bool
(define exists? (f lst)
  (cond
    ( (null? lst) #f )
    ( (= #f (f (car lst))) (exists? f (cdr lst)) )
    ( #t #t )
  )
)

(check-expect (exists? (lambda (n) (< n 0)) nil) #f)
(check-expect (exists? (lambda (n) (< n 0)) '(1 2 3 4 5)) #f)
(check-expect (exists? (lambda (n) (< n 0)) '(1 2 3 4 -5)) #t)
(check-expect (exists? (lambda (n) (< n 0)) '(-1 2 3 4 5)) #t)

;; all? : (a -> bool) (list a) -> bool
(define all? (f lst)
  (cond
    ( (null? lst) #t )
    ( (= #f (f (car lst))) #f )
    ( #t (all? f (cdr lst)) )
  )
)

(check-expect (all? (lambda (n) (< n 0)) nil) #t)
(check-expect (all? (lambda (n) (< n 0)) '(1 2 3 4 5)) #f)
(check-expect (all? (lambda (n) (< n 0)) '(1 2 3 4 -5)) #f)
(check-expect (all? (lambda (n) (< n 0)) '(-1 -2 -3 -4 -5)) #t)

;; takewhile : (a -> bool) (list a) -> (list a)
(define takewhile (f lst)
  (cond
    ( (null? lst) nil )
    ( (= #f (f (car lst))) nil )
    ( #t (cons (car lst) (takewhile f (cdr lst))) )
  )
)

(check-expect (takewhile (lambda (n) (> n 0)) nil) nil)
(check-expect (takewhile (lambda (n) (> n 0)) '(1 2 3 4 5)) '(1 2 3 4 5))
(check-expect (takewhile (lambda (n) (> n 0)) '(1 2 -3 4 5)) '(1 2))

;; dropwhile : (a -> bool) (list a) -> (list a)
(define dropwhile (f lst)
  (cond
    ( (null? lst) nil )
    ( (= #f (f (car lst))) lst )
    ( #t (dropwhile f (cdr lst)) )
  )
)

(check-expect (dropwhile (lambda (n) (> n 0)) nil) nil)
(check-expect (dropwhile (lambda (n) (> n 0)) '(1 2 3 4 5)) nil)
(check-expect (dropwhile (lambda (n) (> n 0)) '(1 2 -3 4 5)) '(-3 4 5))

;; foldl : (r -> x -> r) r (list x) -> r
(define foldl (f init lst)
  (if (null? lst)
    init
    (foldl f (f init (car lst)) (cdr lst) )
  )
)

(check-expect (foldl + 0 nil) 0)
(check-expect (foldl + 0 '(1 2 3 4 5)) 15)
(check-expect (foldl - 0 '(1 2 3 4 5)) -15)

(define foldr_iter (f lst init)
  (if (null? lst)
    init
    (foldr_iter f (cdr lst) (f (car lst) init))
  )
)

;; foldr : (x -> r -> r) r (list x) -> r
(define foldr (f init lst)
  (foldr_iter f (reverse lst) init)
)

(check-expect (foldr + 0 nil) 0)
(check-expect (foldr + 0 '(1 2 3 4 5)) 15)
(check-expect (foldr - 0 '(1 2 3 4 5)) 3)

;; curry : (a b -> c) -> (a -> (b -> c))
(define curry (f)
  (lambda (x) (lambda (y) (f x y)))
)

(check-expect 
  (let ([f ((curry +) 2)]
        [g ((curry *) 5)])
    (f (g 10))) 52)

;; uncurry : (a -> (b -> c)) -> (a b -> c)
(define uncurry (f)
  (lambda (x y) ((f x) y))
)

(check-expect 
  (let* ([f (curry +)]
         [g (uncurry f)])
    (g 10 20)) 30)

;; ---------------------------------------------------------------------- 
;; Functions with variable numbers of arguments. 
;; ---------------------------------------------------------------------- 

;; list : (list x1 x2 ...) -> list(x1, x2, ...)
(define list lst
  lst
)

(check-expect (list) nil)
(check-expect (list 1) '(1))
(check-expect (list 1 2 3 4 5) '(1 2 3 4 5))

;; Helper function for min and max.
(define extremum (op lst)
  (if (null? lst)
    (error 'not-enough-arguments)
    (if (null? (cdr lst))
      (car lst)
      (let 
        ((ex (extremum op (cdr lst))))
        (if (op ex (car lst))
          ex
          (car lst)
        )
      )
    )
  )
)

;; min : x1 x2 ... -> min(x1, x2, ...)
(define min lst (extremum < lst))

(check-error (min))
(check-expect (min 1) 1)
(check-expect (min 1 2 3 4 5) 1)
(check-expect (min 5 4 3 2 1 0 1 2 3 4 5) 0)

;; max : x1 x2 ... -> max(x1, x2, ...)
(define max lst (extremum > lst))

(check-error (max))
(check-expect (max 1) 1)
(check-expect (max 1 2 3 4 5) 5)
(check-expect (max 5 4 3 2 1 0 1 2 3 4 5) 5)

(define compose (f g)
  (lambda (x) 
    (f (g x))
  )
)

;; o : (y -> z) (x -> y) ... (b -> c) (a -> b) -> (a -> z)
(define o lst
  (foldr compose (lambda (x) x) lst)
)

(check-expect ((o) 10) 10)
(check-expect ((o (lambda (x) (* x 2))) 10) 20)
(check-expect
  (let ([f1 (lambda (n) (* n 2))]
        [f2 (lambda (n) (+ 3 n))]
        [f3 (lambda (n) (- 4 n))])
    ((o f2 f1 f3) 25))
  -39)

