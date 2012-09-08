(define (f n)
  (if (= n 0)
    0
    (+ 1 (f (- n 1)))))

(define (g n)
  (display n)
  (newline)
  (f n)
  (g (+ 1 n)))

(g 0)
