(defun report-result (expected real name)
	(terpri)
	(format t "function name is " )
	(format t name )
	(terpri)
	(format t "expected result is : " )
	(write expected)
	(terpri)
	(format t "function output is ")
	(write real)
	(terpri)
	(cond
		((equal real expected) (print "pass"))
		((not(equal real expected)) (print "fail"))
		)
	(terpri)


)


(defun test-count ()
	(print "This part tests count-occur")
	(print "We count the number of occurances of x in ((f x) y (((x z) () x )))")
	(report-result '3 (count-occur 'x '((f x) y (((x z) () x )))) "count-occur")
	(print "We count the number of occurances of x in (x y z a b c x y)")
	(report-result '2 (count-occur 'x '(x y z a b c x y)) "count-occur")
	(print "We count the number of occurances of x in x (should go to error, because the second argument is not a list)")
	(report-result "error, non list second argument" (count-occur 'x 'x) "count-occur")

)

(defun test-dervil ()
	(print "This part tests dervil")
	(print "We get the derivative of 5x^4 (should be 20 x 3)")
	(report-result '(20 x 3) (dervil '(5 x 4)) "dervil")
	(print "We get the derivative of 3x^2 (should be 6 x 1)")
	(report-result '(6 x 1) (dervil '(3 x 2)) "dervil")
	(print "We get the derivative of 3x^1 (should be 3)")
	(report-result '3 (dervil '(3 x 1)) "dervil")

	(print "We get the derivative of 0 x^1 (should be 0)")
	(report-result '0 (dervil '(0 x 1)) "dervil")


)

(defun test-my-flatten ()
	(print "This part tests my-flatten")
	(print "We flatten (1 (1 2) 4)")
	(report-result '(1 1 2 4) (my-flatten '(1 (1 2) 4)) "my-flatten")
	(print "We flatten (1 (1 3 ( 4 5 ( 2 9) )2) 4)")
	(report-result '(1 1 3 4 5 2 9 2 4) (my-flatten '(1 (1 3 ( 4 5 ( 2 9) )2) 4)) "my-flatten")
	(print "We flatten (((a)(b)) (c) (d))")
	(report-result '(a b c d) (my-flatten '(((a)(b)) (c) (d))) "my-flatten")
	(print "We test wrong input 'X (single element not a list)")
	(report-result '"error, non list input" (my-flatten 'X ) "my-flatten")

)

(defun test-my-intersection ()
	(print "This part tests my-intersection")
	(print "We intersect '(A (A B) (C (D)) (E A)) '(B ((A B) C A))")
	(report-result nil (my-intersection '(A (A B) (C (D)) (E A)) '(B ((A B) C A))) "my-intersection")
	(print "We intersect '(1 2 (5 4) 3 ) '(3 2 4)")
	(report-result '(2 3) (my-intersection '(1 2 (5 4) 3 ) '(3 2 4) ) "my-intersection")
	(print "We intersect '(A B) '(A B)")
	(report-result '(A B) (my-intersection '(A B) '(A B) ) "my-intersection")

)
(defun test-find-match ()
	(print "This part tests find-match")
	(print "We match '(on ?x ?y) to '((on-table B1) (on-table B2) (on B3 B2) (on B4 B3))")
	(report-result '((?X B3) (?Y B2)) (find-match '(on ?x ?y) '((on-table B1) (on-table B2) (on B3 B2) (on B4 B3))) "find-match")
	(print "We match '(rainy-weather) to '((on-table B1) (on-table B2) (on B3 B2) (rainy-weather) (on B4 B3))")
	(report-result 'T (find-match '(rainy-weather) '((on-table B1) (on-table B2) (on B3 B2) (rainy-weather) (on B4 B3))) "find-match")
	(print "We match '(rainy-weather) to '((on-table B1) (on-table B2) (on B3 B2) (on B4 B3))")
	(report-result 'NIL (find-match '(rainy-weather) '((on-table B1) (on-table B2) (on B3 B2) (on B4 B3))) "find-match")

)

(defun test-store-facts ()
	(print "This part tests store-facts")
	(print "Our facts are ((on A B) (on C D) (by A B) (by C D))")
	(let ((ht (make-hash-table :test #'equal)) (facts '((on A B) (on C D) (by A B) (by C D))))
	(store-facts facts ht)
	(print "We now querey the key ON")
	(report-result '((ON A B) (ON C D)) (gethash 'ON ht) "store-facts")	
	(print "We now querey the key BY")
	(report-result '((by A B) (by C D)) (gethash 'BY ht) "store-facts")	
	(print "We now querey the key (ON A B) ")
	(report-result 'T (gethash '(ON A B) ht) "store-facts")	
	(print "We now querey the key FOR ")
	(report-result 'NIL (gethash 'FOR ht) "store-facts")	

	)
)


(test-count)
(test-dervil)
(test-my-flatten)
(test-my-intersection)
(test-find-match)
(test-store-facts)