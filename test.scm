; Comment
(define (newline) (display #\newline))
(define x 10)
(display x)
(newline)
(set! x (+ x 1))
(display x)
(newline)

(if #f (+ 1 1))
(if #t (+ 1 1))


(if 1 (display "1 is true\n") (display "1 is false\n"))
(if 0 (display "0 is true\n") (display "0 is false\n"))
(if #t (display "#t is true\n") (display "#t is false\n"))
(if #f (display "#f is true\n") (display "#f is false\n"))
(if (display "") (display "#unspecified is true\n") (display "#unspecified is false\n"))

(display '(1 . 2))
(newline)
(display '(1 2 . 3))
(newline)
(display (car '(1 2 3)))
(newline)
(display (cdr '(1 2 3)))
(newline)
(display (cons 'a 'b))
(newline)

(define (h op x y) (op x y))

(display (h * 7 42))
(newline)

(display (let ((x 1)
	       (y 2))
	   (+ x y)))
(newline)

(display (let* ((x 1)
	       (y (+ x 1)))
	   y))
(newline)

(begin (display 1)
       (newline)
       (display 2)
       (newline))

(display (car '(1 . 2)))
(display (cdr '(1 . 2)))
(display (caar '((1 . 2) . 3)))
(display (cdar '((1 . 2) . 3)))

(display h)
