
;;counts the number of occurances of an element s in a list lst, returns a number
(defun count-occur(s lst)

	(cond
		((not(listp lst)) "error, non list second argument")
		((not(atom s)) "error, non atom first argument")
		((null lst) 0 )
		((listp (car lst))(+(count-occur s (car lst))(count-occur s (cdr lst))))
		((eq(car lst) s) (+ 1 (count-occur s (cdr lst))))
		(t (count-occur s (cdr lst))))
	)

;;takes in a list mono, calculates the first dervative of a single variable monomial of the form cx^i, returns a list 
(defun dervil (mono)
	(if (not (= (length mono) 3)) "error, wrong list size")
	(if (not (listp mono)) "error, non list input")
	(let ((c (car mono)) (x (second mono)) (i (third mono)))

		(cond
			((= c 0) 0)
			((= i 1) c)

			((and (integerp i) (numberp c) (atom x))(list (* i c) x (- i 1)))
			(t (print "error"))
		)
	)

)

;;takes in an arbitrarily nested list , returns a new list of atoms that appear in the same order as the original list
;;uses mapcar as required
(defun my-flatten-helper (lst)

  (cond 
  		((null lst) nil)
        ((atom lst) (list lst))
        ((listp lst) (apply #'nconc (mapcar #'my-flatten-helper lst))))
 )
;;implemented this way to do type checking
(defun	my-flatten(lst) 
	(if(listp lst)(my-flatten-helper lst) "error, non list input")
)

;;(helper function) for my-intersection, checks if element x is a member of list lst
(defun my-member (x lst)
	(cond
		((equal x (car lst)) t)
		((null lst) nil)
		(t (my-member x (cdr lst)))

	)
)
;;uses helper functions (my-member)
;;my-intersection takes in two lists, l1 and l2, 
(defun my-intersection(l1 l2)
	(cond
		((not (listp l1)) "error, non list argument")
		((not (listp l2)) "error, non list argument")
		((null l1) nil)
		((null l2) nil)
		((or (not(listp l1))(not(listp l2))) nil)
		((my-member (car l1) l2)(cons (car l1)(my-intersection(cdr l1) l2)))
		(t (my-intersection (cdr l1) l2))
		)
	)
;;(helper function) checks if the first char of an element is a question mark, useful for scanning for 'variables'
(defun firstcharQ (lst)
	(cond 
		((null lst) t)
		((not (equal (subseq (string (car lst)) 0 1) '"?")) nil)
		(t (firstcharQ (cdr lst)))
	)
)
;;(helper function) returns a single list of variables and their corresponding values
(defun matched-list (pred fact)
	(cond
		((null pred) nil)
		(t (cons (list (car pred) (car fact)) (matched-list (cdr pred) (cdr fact))))
		)
)

;;(helper function) checks if a single fact matches a signle predicate

(defun match-fact (pred fact)
	(cond
		((equal pred fact) t)
		((not (equal (length pred) (length fact))) nil)
		((not (equal (car pred) (car fact))) nil)
		((not (firstcharQ (cdr pred))) nil)
		(t (matched-list (cdr pred) (cdr fact)))
	)
)
;uses helper functions (match-fact), (matched-list), (firstcharQ)
(defun find-match (pred facts)

	(cond
		((not (listp facts)) "error, non list argument")
		((null facts) nil)
		((match-fact pred (car facts)) (match-fact pred (car facts)))
		(t (find-match pred (cdr facts)))
	)
)
;; store fact takes in a list fact and a hashtable ht and stores the fact in the hashtable in two ways, using the predicate name and the complete predication
(defun store-fact (fact ht)
	(cond 
		((not(listp fact)) "error, non list facts")
		((not(hash-table-p ht)) "error, non hashtable")
	)
	(if(not (gethash (car fact) ht))
		(setf (gethash (car fact) ht) fact)
		(setf (gethash (car fact) ht) (list (gethash (car fact) ht) fact)))
	
	(setf (gethash fact ht) 'T)
)

;; store facts takes in a list facts and a hashtable ht and stores the all facts in the hashtable using store-fact
(defun store-facts (facts ht)
	(cond 
		((not(listp facts)) "error, non list facts")
		((not(hash-table-p ht)) "error, non hashtable")
	)
	(mapcar #'(lambda (x) (store-fact x ht)) facts )
)


