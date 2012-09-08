(define return #f)

(display (+ 1 (call/cc
      (lambda (cont)
	(set! return cont)
	1))))

(return 1)
(return 2)
(return 3)
