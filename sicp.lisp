(define (square x) (* x x))

(define (cube x) (* x x x))

(define (abs x) 
    (cond ((< x 0) (- x)) 
          (else    x    )))

(define (abs x)
    (if (< x 0)
        (- x) 
        x))

(define (>= x y)
    (or (> x y) (= x y)))

(define (>= x y)
    (not (< x y)))

(define (<= x y)
    (not (> x y)))

(define (average x y)
    (/ (+ x y) 2))

(define (factorial n)
    (if (= n 1)
        1
        (* n (factorial (- n 1)))))

(define (factorial n)
    (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
    (if (> counter max-count)
        product
        (fact-iter (* counter product)
                   (+ counter 1)
                   max-count)))

(define (pow x y)
    (if (= y 0)
        1
        (* x (pow x (- y 1)))))

(define (pow x y)
    (pow-iter 1 1 x y))

(define (pow-iter product counter base max-count)
    (if (> counter max-count)
        product
        (pow-iter (* product base)
                  (+ counter 1)
                  base
                  max-count)))

;过程
(define (square x) (* x x))
(define (cube x) (* x x x))

(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))

