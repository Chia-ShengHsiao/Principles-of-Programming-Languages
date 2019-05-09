;Assignment 2

;Question 1
	; a. yes
	; b. no
	; c. no
	; d. yes
	; e. no
	; f. yes
	; g. yes

;Question 2
	; a. single atom
	; b. single list with 4 elements
	; c. neither
	; d. neither.
	; e. single list with 2 elements
	; f. neither
	; g. single list with 3 elements
	; h. single list with 2 elements

;Question 3
	(setf x '(A B C))
    (append x x)

;Question 4
	(QUOTE (QUOTE (A)))
	; 'a = (QUOTE A) is a list of two symbols that evaluates to the symbol A.

;Question 5
	; a.
		(setq y (list 'a 'b))
	; b.
		(list 'd (car y))

;Question 6
	(defun sqr
        (x)
        (list (+ x x x x) (* x x)))

;Question 7
	(defun quadratic (a b c)
       (list (/ (+ (- b)
                   (sqrt (- (* b b) (* 4 a c))))
                (* 2 a))
             (/ (- (- b)
                   (sqrt (- (* b b) (* 4 a c))))
                (* 2 a))))

;Question 8
    (defun areaOfCircle
        (r)
        (* pi (* r r)))

;Question 9
;Fahrenheit to celsius: (Temp-32)* (5/9)
    (defun FTOC
        (temp)
        (* (- temp 32) (/ 5 9)))

;Question 10
	(defun rotate-left
        (whole-list)
        (append (rest whole-list) (list (first whole-list))))

;Question 11
    (defun distancePoints
        (firstList secondList)
        (sqrt (+ (* (- (car firstList) (car secondList)) (- (car firstList) (car secondList)))
        (* (- (cadr firstList) (cadr secondList)) (- (cadr firstList) (cadr secondList))))))

;Question 12
    (defun HEAD
        (myList)
        (first myList))

    (defun TAIL
        (myList)
        (rest myList))

;Question 13
    (defun SWITCH
        (myList)
        (append (rest myList) (list (first myList))))

;Question 14
    ;70.
; ANSWER VERIFIED AND CHECKED


;Question 15
    ; a) (caddr e)
    ; b) (car (car (cdr (car (cdr e)))))
    ; c) (car (car (cdr (car (cdr (car (car '(((A (B (X) D)))))))))))

;Question 16
    ; a) (cons 'a (cons 'b (cons 'x (cons 'd nil))))
    ; b) (cons 'a (cons (cons 'b (cons (cons 'x (cons 'd nil)) nil)) nil))
    ; c) (cons (cons (cons 'a (cons (cons 'b (cons (cons 'x nil) (cons 'd nil))) nil)) nil) nil)

;Question 17
    (list (cons (list (caar e) (cadar e)) (cddar e)) (append (list 'a 'b) (cdr (cdr (caddr e)))))

;Question 18
    (list (append (list (caar e) 'a) (cddar e)) (cadadr e) (cadr (cdaddr e))  (append (list (car (cddadr e))) (cdr (cdr (caddr e))$

;Question 19
    ((90 91 92 93 94 95 96 97 98 99 3) (+ 3 4 – 29 39 49 59 69 79 89 99))

;Question 20
    (append (list (caar e))(append (list (append (list (car(cdaddr e)))(list (cadr (cdaddr e))))) (cddr (cdaddr e))))