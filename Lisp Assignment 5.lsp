;Assignment 5

;Name : Chia-Sheng Hsiao

;ID : 23399066

;Question 1
(defun index (N L)
     (cond ((endp L) 'ERROR)
         ((= N 1) (car L))
         (t (index (- N 1) (cdr L)))))
		 
;Question 2
(defun min-first (L)
     (if (endp (cdr L))
         L
         (let ((X (min-first (cdr L))))
             (if (<= (car L) (car X))
                 L
                 (cons (car X) (cons (car L) (cdr X)))))))
				 
;Question 3
(defun ssort (L)
     (cond ((endp L) ())
         (t (let ((X (min-first L)))
             (cons (car X) (ssort (cdr X)))))))
			 
;Question 4
(defun partition (L P)
     (if (endp L)
         '(() ())
         (let ((X (partition (cdr l) P)))
             (if (< (car L) P)
                 (list (cons (car L) (car X)) (cadr X))
                 (list (car X) (cons (car L) (cadr X)))))))

(defun qsort (L)
     (cond ((endp L) ())
         (t (let ((PL (partition L (car L))))
             (cond ((endp (car PL)) (cons (car L) (qsort (cdr L))))
                 (t (append (qsort (car PL)) (qsort (cadr PL)))))))))
				 
;Question 5
(defun merge-lists (L1 L2)
     (cond ((endp L1) L2)
         ((endp L2) L1)
         ((< (car L1) (car L2))
         (cons (car L1) (merge-lists (cdr L1) L2)))
         (t (cons (car L2) (merge-lists L1 (cdr L2))))))
		 
;Question 6
(defun split-list (L)
     (if (endp L) 
         '(() ())
         (let ((X (split-list (cdr L))))
             (list (cons (car L) (cadr X)) (car X)))))

(defun msort (L)
     (cond ((endp L) ())
         ((endp (cdr L)) L)
         (t (let ((X (split-list L)))
            (merge-lists (msort (car X)) (msort (cadr X)))))))
			 
;Question 7
(defun remove-adj-dupl (L)
     (cond ((or (endp L) (endp (cdr L))) L)
         ((equal (car L) (cadr L)) (cons (car L) (cdr (remove-adj-dupl (cdr L)))))
         (t (cons (car L) (remove-adj-dupl (cdr L))))))
		 
;Question 8
(defun unrepeated-elts (L)
     (cond ((endp L) ())
         ((or (endp (cdr L)) (not (equal (car L) (cadr L)))) (cons (car L) (unrepeated-elts (cdr L))))
         ((or (endp (cddr L)) (not (equal (car L) (caddr L)))) (unrepeated-elts (cddr L)))
         (t (unrepeated-elts (cdr L)))))
		 
;Question 9
(defun repeated-elts (L)
     (cond ((endp L) ())
         ((or (endp (cdr L)) (not (equal (car L) (cadr L)))) (repeated-elts (cdr L)))
         ((or (endp (cddr L)) (not (equal (car L) (caddr L)))) (cons (car L) (repeated-elts (cddr L))))
         (t (repeated-elts (cdr L)))))
		 
;Question 10
(defun count-repetitions (L)
     (cond ((endp L) ())
         ((endp (cdr L)) (list (cons 1 L)))
         (t (let ((X (count-repetitions (cdr L))))
             (if (not (equal (car L) (cadr L)))
                 (cons (list 1 (car L)) X)
                 (cons (list (+ 1 (caar X)) (car L)) (cdr X)))))))
				 
;Question 11
(defun subset (F L)
     (cond ((endp L) ())
         ((funcall F (car L)) (cons (car L) (subset F (cdr L))))
         (t (subset F (cdr L)))))
		 
;Question 12
(defun our-some (F L)
     (if (endp L)
	     nil
	     (let ((X (our-some F (cdr L))))
	         (if (funcall F (car L))
		         L
		         X))))

(defun our-every (F L)
   (if (endp L)
	 t
	 (let ((X (our-every F (cdr L))))
	     (if X (funcall F (car L))))))

;Question 13	
(defun PARTITION1 (f L p)
     (if (endp L)
         (list nil nil)
         (let ((x (partition1 f (cdr L) p)))
		     (cond ((funcall f (car L) p)
		         (append (list (cons (car L) (car x))) (list (cadr x))))
		         (t (append (list (car x)) (list (cons (car L) (cadr x)))))))))	

(defun QSORT1 (f L)
     (cond ((endp L) nil)
	     (t (let ((Z (partition1 f L (car L))))  
	             (cond ((endp (car Z)) (cons (car L) (qsort1 f (cdr L))))
		             (t (let ((x (qsort1 f (car Z)))
		                 (y (qsort1 f (cadr Z)))) 
			             (append  x  y))))))))
						 
;Question 14
(defun FOO (f L)
     (if (endp L) 
         () 
         (let* ((x (foo f (cdr L))) (ls (cdr L))
	         (Z (list (cons (funcall f (car L)) ls)))
	         (Y (mapcar (lambda (a) (cons (car L) a)) x)))
	         (append Z Y))))
			 
;Question 15 (A)
(defun tr-add (L N)
     (if (endp L)
         N
         (tr-add (cdr L) (+ (car L) N))))

(defun tr-mul (L N)
     (if (endp L)
         N
         (tr-mul (cdr L) (* (car L) N))))

(defun tr-fac (L N)
     (if (zerop L) 
	     N 
		 (tr-fac (- L 1) (* L N))))
		 
;Question 15(B)
(defun slow-primep (N)
     (if (equal (mod (tr-fac (- N 1) 1) N) (- N 1))
         t
         nil))
		 
;Question 16(A) 
(defun TRANSPOSE1 (m)
     (cond ((endp (cdr m)) (mapcar #'list (car m)))
	     (t (mapcar #'cons (car m) (transpose1 (cdr m))))))

;Question 16(B)
(defun TRANSPOSE2 (m)
     (cond ((endp (cdar m)) (list (mapcar #'car m)))
	     (t (cons (mapcar #'car m) (transpose2 (mapcar #'rest m))))))

;Question 16(C)
(defun TRANSPOSE3 (m)
     (apply #'mapcar #'list m))
