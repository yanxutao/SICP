(define (square x) (* x x))

(define (abs x)
    (cond ((< x 0) (- x))
          ((= x 0) 0    )
          ((> x 0) x    )))

(define (abs x) 
    (cond ((< x 0) (- x)) 
          (else    x    )))

(define (abs x)
    (if (< x 0) (- x) 
        x))

(define (>= x y)
    (or (> x y) (= x y)))

(define (>= x y)
    (not (< x y)))

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

(define (<= x y)
    (not (> x y)))
(define (p x y z) 
    (cond ((and (<= x y) (<= x z)) (+ y z))
          ((and (<= y x) (<= y z)) (+ x z))
          ((and (<= z x) (<= z y)) (+ x y))))

(define (p) (p))
(define (test x y)
    (if (= x 0)
        0
        y))
(test 0 (p))

(define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x)
                   x)))
(define (improve guess x)
    (average guess (/ x guess)))
(define (average x y)
    (/ (+ x y) 2))
(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
(define (square x) (* x x))
(define (abs x)
    (if (< x 0)
        -x 
        x))
(define (sqrt x)
    (sqrt-iter 1.0 x))

(define (new-if predicate then-clause else-clause)
    (cond (predicate then-clause)
          (else else-clause)))
(define (sqrt-iter guess x)
    (new-if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x)
                   x)))

(define (curt-iter y x)
    (if (good-enough? y x)
        y
        (curt-iter (improve y x)
                   x)))
(define (improve y x)
    (/ (+ (/ x (square y)) (* 2 y)) 3))
(define (square x) (* x x))
(define (good-enough? y x)
    (< (abs (- (cube y) x)) 0.001))
(define (abs x)
    (if (< x 0)
        -x 
        x))
(define (cube x) (* x x x))
(define (curt x)
    (curt-iter 1.0 x))


(define (sqrt x)
    (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))

(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))

(define (abs x)
    (if (< x 0)
        -x 
        x))

(define (square x) (* x x))

(define (improve guess x)
    (average guess (/ x guess)))

(define (average x y)
    (/ (+ x y) 2))


(define (sqrt x)
    (define (abs x)
        (if (< x 0)
            (- x) 
            x))
    (define (square x) (* x x))
    (define (average x y)
        (/ (+ x y) 2))
    (define (good-enough? guess x)
        (< (abs (- (square guess) x)) 0.001))
    (define (improve guess x)
        (average guess (/ x guess)))
    (define (sqrt-iter guess x)
        (if (good-enough? guess x)
            guess
            (sqrt-iter (improve guess x) x)))
    (sqrt-iter 1.0 x))


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

((define (a x y)
    (cond ((= y 0) 0)
          ((= x 0) (* 2 y))
          ((= y 1) 2)
          (else (a (- x 1)
                   (a x (- y 1)))))))


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

(define (fib n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          (else (+ (fib (- n 1))
                   (fib (- n 2))))))

(define (fib n)
    (fib-iter 1 0 n))

(define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))

(define (count-change amount)
    (cc amount 5))

(define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (= kinds-of-coins 0)) 0)
          (else (+ (cc amount 
                       (- kinds-of-coins 1))
                   (cc (- amount (first-denomination kinds-of-coins))
                       kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
          ((= kinds-of-coins 2) 5)
          ((= kinds-of-coins 3) 10)
          ((= kinds-of-coins 4) 25)
          ((= kinds-of-coins 5) 50)))

(define (f n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          ((= n 2) 2)
          (else (+ (f (- n 1))
                   (* 2 (f (- n 2)))
                   (* 3 (f (- n 3)))))))

(define (f n)
    (cond ((= n 0) 0)
          ((= n 1) 1)
          ((= n 2) 2)
          (else (f-iter 0 1 2 (- n 2)))))

(define (f-iter x y z count)
    (if (= count 0)
        z
        (f-iter y z (+ (* x 3) (* y 2) z) (- count 1))))


(define (p x y)
    (cond ((= y 1) 1)
          ((= x y) 1)
          (else (+ (p (- x 1) (- y 1)) (p (- x 1) y)))))

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (abs x)
    (if (< x 0)
        (- x) 
        x))
(define (sine angle)
    (if (not (> (abs angle) 0.1))
        angle
        (p (sine (/ angle 3.0)))))

(define (expt b n)
    (expt-iter b n 1))
(define (expt-iter b counter product)
    (if (= counter 0)
        product
        (expt-iter b (- counter 1) (* product b))))


(define (even? n)
    (= (remainder n 2) 0))
(define (square x) (* x x))
(define (fast-expt b n)
    (cond ((= n 0) 1)
          ((even? n) (square (fast-expt b (/ n 2))))
          (else (* b (fast-expt b (- n 1))))))

(define (sum term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (+ result (term a)))))
    (iter a 0))

(define (cube x) (* x x x))
    
(define (integral f a b dx)
    (define (add-dx x) (+ x dx))
    (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(define (product term a next b)
    (if (> a b)
        1
        (* (term a) (product term (next a) next b))))
(define (factorial n)
    (define (term x) x)
    (define (next x) (+ x 1))
    (product term 1 next n))

(define (product term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (* result (term a)))))
    (iter a 1))
(define (pi-term n)
    (if (= (remainder n 2) 0)
        (/ (+ n 2) (+ n 1))
        (/ (+ n 1) (+ n 2))
        ))
(define (pi-next n) (+ n 1))

(define (accumulate combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a)
                  (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
    (accumulate + 0 term a next b))
(define (product term a next b)
    (accumulate * 1 term a next b))

(define (filtered-accumulate filter combiner null-value term a next b)
    (if (> a b)
        null-value
        (if (filter (term a))
            (combiner (term a)
                  (filtered-accumulate filter combiner null-value term (next a) next b))
            (combiner null-value
                     (filtered-accumulate filter combiner null-value term (next a) next b)))))

(define (accumulate combiner null-value term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (combiner result (term a)))))
    (iter a null-value))

(define (filtered-accumulate filter combiner null-value term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (if (filter (term a))
                (iter (next a) (combiner result (term a)))
                (iter (next a) (combiner result null-value)))))
    (iter a null-value))

(define (plus4 x) (+ x 4))
(define plus4 (lambda (x) (+ x 4)))

((lambda (x y z) (+ x y (square z))) 1 2 3)

(define (search f neg-point pos-point)
    (let ((midpoint (average neg-point pos-point)))
        (if (close-enough? neg-point pos-point)
            midpoint
            (let ((test-value (f midpoint)))
                (cond ((positive? test-value)
                       (search f neg-point midpoint))
                      ((negative? test-value)
                       (search f midpoint pos-point))
                      (else midpoint))))))

(define (close-enough? x y)
    (< (abs (- x y)) 0.001))
(define (half-interval-method f a b)
    (let ((a-value (f a))
          (b-value (f b)))
        (cond ((and (negative? a-value) (positive? b-value))
               (search f a b))
              ((and (negative? b-value) (positive? a-value))
               (search f b a))
              (else (error "Error!"))))))

(half-interval-method sin 2.0 4.0)

(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3)) 1.0 2.0)


(define tolerance 0.00001)

(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2)) tolerance))
    (define (try guess)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                next
                (try next))))
    (try first-guess))

(fixed-point cos 1.0)

(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)

(define (sqrt x)
    (fixed-point (lambda (y) (/ (+ y (/ x y)) 2)) 1.0))


(define fib
   (trace-lambda fib (x)
      (if (<= x 1)
          1
          (+ (fib (- x 1)) (fib (- x 2))))))


(define (cont-frac n d k)
    (define (iter i)
        (if (= i k)
            (/ (n i) (d i))
            (/ (n i) (+ (d i) (iter (+ i 1))))))
    (iter 1))

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           k)

(define (cont-frac n d k)
    (define (iter i result)
        (if (= i 0)
            result
            (iter (- i 1)
                  (/ (n i) (+ (d i) result)))))
    (iter k 0))

(define (n i)
    1.0)
(define (d i)
    (let ((x (// i 3))
          (y (% i 3)))
    (if (= y 2)
        (* 2 (+ x 1))
        1)))

(define (// x y)
    (define (iter x y i)
        (if (< x y)
            i
            (iter (- x y) y (+ i 1))))
    (iter x y 0))

(define (% x y)
    (if (< x y)
        x
        (% (- x y) y)))

(cont-frac n d 10)

(define (e-2 k)
    (define (n i) 1.0)
    (define (d i)
        (let ((x (// i 3))
              (y (% i 3)))
        (if (= y 2)
            (* 2 (+ x 1))
            1)))
    (cont-frac n d k))

(define (tan-cf x k)
    (define (n i)
        (if (= i 1)
            x
            (* (- 1) x x)))
    (define (d i)
        (- (* 2 i) 1))
    (cont-frac n d k))

(define (average-damp f)
    (lambda (x) (average x (f x))))


(define (linear-combinaton a b x y)
    (+ (* a x) (* b y)))

(define (linear-combinaton a b x y)
    (add (mul a x) (mul b y)))

(make-rat n d)
(numer x)
(denom x)

(define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))

(define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))

(define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))

(define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

(define (equal-rat? x y)
    (= (* (numer x) (denom y))
       (* (denom x) (numer y))))

(define (make-rat n d)
    (let ((g (gcd (abs n) (abs d))))
        (if (< (* n d) 0)
            (cons (- (/ (abs n) g)) (/ (abs d) g))
            (cons (/ (abs n) g) (/ (abs d) g)))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
    (display (numer x))
    (display "/")
    (display (denom x)))

(define one-half (make-rat 1 2))
(define one-third (make-rat 1 3))

(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))


(define (make-segment p1 p2) (cons p1 p2))
(define (start-point segment) (car segment))
(define (end-point segment) (cdr segment))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (midpoint-segment segment)
    (make-point (/ (+ (x-point (start-point segment))
                      (x-point (end-point segment)))
                   2)
                (/ (+ (y-point (start-point segment))
                      (y-point (end-point segment)))
                   2)))

(define (print-point p)
    (display "(")
    (display (x-point p))
    (display ", ")
    (display (y-point p))
    (display ")"))

练习2.6

(define (make-interval a b) (cons a b))
(define (upper-bound interval) (car interval))
(define (lower-bound interval) (cdr interval))

(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                   (+ (upper-bound x) (upper-bound y))))

(define (sub-interval x y)
    (make-interval (- (lower-bound x) (upper-bound y))
                   (- (upper-bound x) (lower-bound y))))

(define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
          (p2 (* (lower-bound x) (upper-bound y)))
          (p3 (* (upper-bound x) (lower-bound y)))
          (p4 (* (upper-bound x) (upper-bound y))))
        (make-interval (min p1 p2 p3 p4)
                       (max p1 p2 p3 p4))))

(define (div-interval x y)
    (if (< (* (lower-bound y) (upper-bound y)) 0)
        (error "This interval goes through zero")
        (mul-interval x
                      (make-interval (/ 1.0 (upper-bound y))
                                     (/ 1.0 (lower-bound y))))))

练习2.11

(define (make-center-width c w)
    (make-interval (- c w) (+ c w)))

(define (center i)
    (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
    (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c p)
    (make-center-width c (* c p)))

(define (percent i)
    (let ((c (center i))
          (w (width i)))
        (/ w c)))