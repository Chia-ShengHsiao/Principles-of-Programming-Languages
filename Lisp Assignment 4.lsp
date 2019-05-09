;Assignment 4

;Name : Chia-Sheng Hsiao

;ID : 23399066

;Question 1
(defun sum (L)
     (if (endp L) 
         0 
	     (+ (car L) (sum (cdr L)))))

;Question 2
(defun neg-nums (L)
     (cond ((endp L) ())
         ((< (car L) 0) (cons (car L) (neg-nums (cdr L))))
         (t (neg-nums (cdr L)))))

;Question 3	
(defun inc-list-2 (L N)
     (if (endp L)
         ()
         (cons (+ N (car L)) (inc-list-2 (cdr L) N))))

;Question 4
(defun insert (N L)
     (cond ((endp L) (list N))
         ((<= N (car L)) (cons N L))
         (t (cons (car L) (insert N (cdr L))))))

;Question 5	
(defun isort (L)
     (if (endp L)
         ()
         (insert (car L) (isort (cdr L)))))

;Question 6
(defun split-list (L)
     (if (endp L) 
         '(() ())
         (let ((X (split-list (cdr L))))
             (list (cons (car L) (cadr X)) (car X)))))

;Question 7
(defun partition (L P)
     (if (endp L)
         '(() ())
         (let ((X (partition (cdr l) P)))
             (if (< (car L) P)
                 (list (cons (car L) (car X)) (cadr X))
                 (list (car X) (cons (car L) (cadr X)))))))

;Question 8
(defun pos (E L)
     (cond ((endp L) 0)
         ((equal E (car L)) 1)
         (t (let ((X (pos E (cdr L))))
                 (if (zerop X) 
			         0 
			         (+ 1 X))))))

;Question 9
(defun split-nums (N)
     (if (zerop N)
         '((0) ())
         (let ((X (split-nums (- N 1))))
             (if (evenp N)
                 (list (cons N (car X)) (cadr X))
                 (list (car X) (cons N (cadr X)))))))

;Question 10
(defun set-union (s1 s2)
     (cond ((endp s1) s2)
         ((member (car s1) s2) (set-union (cdr s1) s2))
         (t (cons (car s1) (set-union (cdr s1) s2)))))
		
;Question 11		
(defun set-remove (x s)
     (cond ((endp s) ())
         ((equal x (car s)) (cdr s))
         (t (cons (car s) (set-remove x (cdr s))))))

;Question 12
(defun set-excl-union (s1 s2)
  (cond ((endp s1) s2)
         ((member (car s1) s2) (set-remove (car s1) (set-excl-union (cdr s1) s2)))
         (t (cons (car s1) (set-excl-union (cdr s1) s2)))))
		 
;Question 13
(defun singletons (e)
  (cond ((endp e) ())
         ((member (car e) (cdr e)) (set-remove (car e) (singletons (cdr e))))
         (t (cons (car e) (singletons (cdr e))))))
