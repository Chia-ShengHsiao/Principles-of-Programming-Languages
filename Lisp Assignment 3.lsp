; Name : Chia-Sheng Hsiao
; CUNY ID : 23399066

; Assignment 3

; Exercise 1
(defun MIN-2 (A B) 
	(if (and (numberp A) (numberp B)) 
		(if (> A B) B A)
		'error))

; Exercise 2
(defun SAFE-AVG (A B) 
	(if (and (numberp A) (numberp B)) 
		(/ (+ A B) 2) 
		nil))

; Exercise 3
(defun ODD-GT-MILLION (x) 
	(if (and (integerp x) (oddp x) (> x 1000000))
		T
		nil))

; Exercise 4
(defun MULTIPLE-MEMBER (A B) 
	(if ( and (or (numberp A) (symbolp A)) (listp B) (not(null B)))
		(if (eql (car B) A)
			(member A (cdr B))
			(MULTIPLE-MEMBER A (cdr B)))
		nil))

; Exercise 5
(defun MONTH->INTEGER (x) 
	(cond
		((eql x 'January) 1)
		((eql x 'February) 2)
		((eql x 'March) 3)
		((eql x 'April) 4)
		((eql x 'May) 5)
		((eql x 'June) 6)
		((eql x 'July) 7)
		((eql x 'August) 8)
		((eql x 'September) 9)
		((eql x 'October) 10)
		((eql x 'November) 11)
		((eql x 'December) 12)
		(t 'Error)))

; Exercise 6
(defun SCORE->GRADE (s)
	(if (numberp s)
		(cond
			((>= s 90) 'A)
			((and (< s 90) (>= s 87)) 'A-)
			((and (< s 87) (>= s 83)) 'B+)
			((and (< s 83) (>= s 80)) 'B)
			((and (< s 80) (>= s 77)) 'B-)
			((and (< s 77) (>= s 73)) 'C+)
			((and (< s 73) (>= s 70)) 'C)
			((and (< s 70) (>= s 60)) 'C-)
			(t 'F))
		nil))

; Exercise 7
(defun GT (A B) (and (numberp A) (numberp B) (> A B)))

; Exercise 8
(defun SAME-SIGN (A B) 
	(and 
		(integerp A) 
		(integerp B)
		(or 
			(and (zerop A) (zerop B))
				(and (> A 0) (> B 0))
				(and (< A 0) (< B 0)))))

; Exercise 9
(defun SAFE-DIV (A B) (and (numberp A) (numberp B) (/= B 0) (/ A B)))