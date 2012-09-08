(define (f k)
  (k 1)
  (k 2)
  3)

(display (call-with-current-continuation f))
