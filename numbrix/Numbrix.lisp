; (remove 4 '(1 3 4 5 9))
;(nth 2 '(1 2 3 4))
;    => 3
;(loadboard "1,2,3,4;6,,10,11;12,13,14,15;" 6)
;(numbrix "C:\\Users\\reniernv\\Desktop\\AI\\AI+Project+1\\AI Project 1\\tboard13.txt")
  (defvar *dim*)
  (defvar *lst*)
  (defvar *a*)
  (defvar *moves* (list))
  (defvar *removable* (list))
  (defvar *instr* (list))
  (defvar *constants* (list))
  (defvar *toggle* 0)
 (defvar *board1* "C:\\Users\\reniernv\\Desktop\\AI\\AI+Project+1\\AI Project 1\\tboard1.txt")
 (defvar *board2* "C:\\Users\\reniernv\\Desktop\\AI\\AI+Project+1\\AI Project 1\\tboard2.txt")
 (defvar *board3* "C:\\Users\\reniernv\\Desktop\\AI\\AI+Project+1\\AI Project 1\\tboard3.txt")
 (defvar *board4* "C:\\Users\\reniernv\\Desktop\\AI\\AI+Project+1\\AI Project 1\\tboard4.txt")
 (defvar *board5* "C:\\Users\\reniernv\\Desktop\\AI\\AI+Project+1\\AI Project 1\\tboard5.txt")
 (defvar *board6* "C:\\Users\\reniernv\\Desktop\\AI\\AI+Project+1\\AI Project 1\\tboard6.txt")
 (defvar *board7* "C:\\Users\\reniernv\\Desktop\\AI\\AI+Project+1\\AI Project 1\\tboard7.txt")
 (defvar *board8* "C:\\Users\\reniernv\\Desktop\\AI\\AI+Project+1\\AI Project 1\\tboard8.txt")
 (defvar *board9* "C:\\Users\\reniernv\\Desktop\\AI\\AI+Project+1\\AI Project 1\\tboard9.txt")
 (defvar *board10* "C:\\Users\\reniernv\\Desktop\\AI\\AI+Project+1\\AI Project 1\\tboard10.txt")
 (defvar *board11* "C:\\Users\\reniernv\\Desktop\\AI\\AI+Project+1\\AI Project 1\\tboard11.txt")
 (defvar *board12* "C:\\Users\\reniernv\\Desktop\\AI\\AI+Project+1\\AI Project 1\\tboard12.txt")
 (defvar *board13* "C:\\Users\\reniernv\\Desktop\\AI\\AI+Project+1\\AI Project 1\\tboard13.txt")
 (defvar *board14* "C:\\Users\\reniernv\\Desktop\\AI\\AI+Project+1\\AI Project 1\\tboard14.txt")
 (defvar *board15* "C:\\Users\\reniernv\\Desktop\\AI\\AI+Project+1\\AI Project 1\\tboard15.txt")
 (defvar *board16* "C:\\Users\\reniernv\\Desktop\\AI\\AI+Project+1\\AI Project 1\\tboard16.txt")
 (defvar *board17* "C:\\Users\\reniernv\\Desktop\\AI\\AI+Project+1\\AI Project 1\\tboard17.txt")

   
(defun drawboard()
 (let* (val)
    (write-char #\linefeed) 
   	(dotimes (c *dim*)  ;display the column numbers
	(write-char #\Tab)
    (write-char #\Tab)
	(princ (list (+ c 1)))
    ); end of dotimes
    (write-char #\linefeed) 
   ;; Start loading items to the actual board
	(dotimes (i *dim*)  ;columns
	(princ (list (- *dim* i))) (write-char #\Tab)
    (if (< (- *dim* i) 10) (write-char #\Tab))	; print the index of the rows.	
		(dotimes (j *dim*) ;rows
	       (princ "|")  (write-char #\Tab)
	        (setf val (aref *a* i j))
	        (if (eq val nil) ; if  nil
	        (progn (princ "-")(write-char #\Tab))
		        (progn   ;  else
		        (princ val) ; print value
			    (write-char #\Tab); if  < 10 true
		        ); end of progn
	        ); end if nil
		);end of row == j 
	(princ "|")
	(write-char #\linefeed) 
	);end of column
   ); end of let
)

(defun removet (rowin colin moves)
  (let* (row col)
  (setf row (- *dim* rowin))
  (setf col (- colin 1))
  (if (not (eq moves nil)) 
    (progn
      (if (subsetp (list rowin colin) (car moves)) 
        (progn
        (print "Removed item") 
         (setf (aref *a* row col) nil)
         ); end of progn
        (removet rowin colin (cdr moves)))
     ); end of progn
     (print " Item not found")
    )
    ); end of let
); end of remove function
   
(defun destruct()
 (setf *dim* nil)
 (setf  *lst* nil)
 (setf *a* nil)
 (setf  *moves* (list))
 (setf *instr* (list))
 (setf *removable* (list))
 (setf *constants* (list))
 (setf *toggle* 0)
 ); end of function
 
(defun gameover()
  (if (< (length *lst*) 2) (return-from gameover t)(return-from gameover nil))
); end function

(defun parseitem (stritem i j) 
  (let* (index val)
  (setf index (search "," stritem)) 
		 (loop while (numberp index) do
		   (setf val (parse-integer (subseq stritem 0 index) :junk-allowed t))
		   (if (numberp val)
		   ;(if (numberp (subseq stritem 0 indexitem))
		      (progn
		        (setf (aref *a* i j) val)
		        (setf *lst*(remove val *lst*))
		      ) 
              ; else
		     );end of if
		       (setf stritem (subseq stritem (+ 1 index) nil))
		       (setf index (search "," stritem))
		       (setf j (+ 1 j))
		  ) ; end of loop
    ; handle the last item on the string
  (setf val (parse-integer stritem :junk-allowed t))
  (if (numberp val)
  (progn
    (setf (aref *a* i j) val)
    (setf *lst*(remove val *lst*))
    )
   ); end of if
   ); end of let
  ); end of function   

  (defun parseline (str i) 
   (let* ((index (search ";" str)) (stritem (subseq str 0 index))) ; set the local variables
   ; (print  stritem)
    (parseitem stritem i 0) ; Get a line
    (if (not (eq index nil))
      (progn 
         (setf str (subseq str (+ 1 index) nil))       ; chop the string
        (parseline str (- i 1))                        ; do it again
       )
      ); end of if
      ); end of let
    ); end of function



(defun validate (row col val)
  ; set the correct indexes 
(let* (v1 v2 v3 v4)
(if (> col 0)
(setf v1 (aref *a* row (- col 1))) ; if col not on low boader get value
(setf v1 nil)) 					  ; else set v1 to very high value

(if (< col (- *dim* 1))
(setf v2 (aref *a* row (+ col 1))) 
(setf v2 nil))

(if (> row 0)
(setf v3 (aref *a* (- row 1) col))
(setf v3 nil))
 
(if (< row (- *dim* 1)) 
(setf v4 (aref *a* (+ row 1) col))
(setf v4 nil))
;(print v1)(print v2) (print v3)(print v4)
  ;; Begin if statements
  (if  (not(eq v1  nil))
    (progn			
      (if  (not(eq v2 nil))
        (progn
         (if  (not(eq v3 nil))
            (progn
            (if  (not(eq v4 nil))
                (progn
	                  (if 
	                    (or 
	                      (eq (- v1 1) val) (eq (+ v1 1) val) (eq (- v2 1) val) (eq (+ v2 1) val)
	                      (eq (- v3 1) val) (eq (+ v3 1) val) (eq (- v4 1) val) (eq (- v4 1) val)
	                     ) ; end of or
	                   (return-from  validate t) 	 ; return true if value == any sequence value for the cells around
	                   (return-from validate nil) ; otherwise return false
	                    ); end of if
                  ); end of forth progn
                (return-from validate t)
                ); end of forth if
              ); end of third progn
              (return-from validate t)
            ); end of third if
          );end of second progn
          (return-from validate t)
        ); end  of second if
      ); end of progn
      (return-from validate t)
    ); end of first if
     (return-from validate t)
); end of let
); end of function
;(loop for x being the elements of)
; end of loadboard function
  (defun play_move (rowin colin value)
(let* ((row (- *dim* rowin)) (col (- colin 1)) (val value))
  (if (and (eq (validate row col val) t) (< val (+ (* *dim* *dim*) 2)) (numberp (position val *lst*)) (eq (aref *a* row col) nil))
 ;(if (and  (> value 0) (< value (+ (* *dim* *dim*) 1)))      
    (progn
          (setf (aref *a* row col) val)
          (setf *moves* (append *moves* (list(list rowin colin val))))
          (setf *lst*(remove val *lst*))	
          (drawboard)
         ) ; end of prog
        (print "Move not Valid") ; else
   ); end of if 
   ); end of let
); end of function

	 
  (defun loadboard (input) 
  (setf *lst* (list 1))
  (let* ((c 2))
 (loop while (> (+ (* *dim* *dim*) 2) c) do
 (setf *lst* (append *lst* (list c)))
 (setf c (+ c 1))
  ) ; end of loop 
  ); end of let
 ;(setf lst (remove 3 lst))
 (setf *a* (make-array (list *dim* *dim*) :initial-element nil) )    
  (parseline input (- *dim* 1))  
  ;(print a)
  (drawboard) 
  (setf *constants* (createlist 0))

  ); end of function
(defun start(location)
(let ((in (open location :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
         while line do (setf *instr* 
                       (concatenate 'string *instr* line))
    ); end of while loop
    
    (close in)))
  
    (let* (index input)
  (setf index (search "|" *instr*)) 
  (setf *dim* (parse-integer 
  (subseq *instr* 0 index) :junk-allowed t))
  (setf input (subseq *instr* (+ index 1) nil))

  (loadboard input)
  ); end of let
); end of function

(defun numbrix()
(let* (col row val cont board)
(loop while (null board) do
(print "[l] load Board. Choose a board number from 1-17.")
(write-char #\linefeed)
(setf board (read))
 ; cond load boards
 (cond
 ((eq board 1) (start *board1*))
 ((eq board 2) (start *board2*))
 ((eq board 3) (start *board3*))
 ((eq board 4) (start *board4*))
 ((eq board 5) (start *board5*))
 ((eq board 6) (start *board6*))
 ((eq board 7) (start *board7*))
 ((eq board 8) (start *board8*))
 ((eq board 9) (start *board9*))   
 ((eq board 10) (start *board10*))
 ((eq board 11) (start *board11*))
 ((eq board 12) (start *board12*))  
 ((eq board 13) (start *board13*))
 ((eq board 14) (start *board14*))
 ((eq board 15) (start *board15*))
 ((eq board 16) (start *board16*))
 ((eq board 17) (start *board17*))
 ( t (progn (setf board nil)(print "Invalid choice. Please choose a number from 1 to 15.")(write-char #\linefeed)))
 ); end of cond
 ); end of while
(loop while (not (or (eql cont 'q)(eql cont 'n))) do 
(write-char #\linefeed)
   (print 
     "Type:
    [n] new game | [m] make move | [r] remove tile | [b] display board
    [d] display moves | [c] check completion | [q] quit game | [a] AI"
     )(write-char #\linefeed)
   (setf cont (read))
(cond 
((eql cont 'n) (progn (destruct) (numbrix))); end of progn
((eql cont 'q)(progn(destruct)))
((eql cont 'm)(progn(drawboard)(print "Please enter the row column and value:")(setf row (read) col (read) val (read) )(play_move row col val)))
((eql cont 'r)(progn(print "Enter row and column to be removed")(setf row (read) col (read))(removet row col *moves*)))
((eql cont 'b)(drawboard))
((eql cont 'd)(print *moves*))
((eql cont 'c)(progn(if (validate_board) (print "You win!") (print "You lose!")) (write-char #\linefeed)))
((eq cont 'a) (progn (timing (solver *constants*)) (drawboard)))
(t (print "Invalid input. Please type n, m, b, d, c, a or e."))
  ); end of cond 
  ) ; end of while
  ) ; end of let 
); end of function

* (defmacro timing (&body forms)
    (let ((real1 (gensym))
	    (real2 (gensym))
	    (run1 (gensym))
	    (run2 (gensym))
	    (result (gensym)))
    `(let* ((,real1 (get-internal-real-time))
	      (,run1 (get-internal-run-time))
	      (,result (progn ,@forms))
	      (,run2 (get-internal-run-time))
	      (,real2 (get-internal-real-time)))
	 (format *debug-io* ";;; Computation took:~%")
	 (format *debug-io* ";;;  ~f seconds of real time~%"
		 (/ (- ,real2 ,real1) internal-time-units-per-second))
	 (format t ";;;  ~f seconds of run time~%"
		 (/ (- ,run2 ,run1) internal-time-units-per-second))
	 ,result)))

;------------------ Validate Board ---------------------------
;;;; Created on 2013-11-23 00:39:34
(defun find_lowest(i lowest) 
  (let* (row col val dim)
    (setf row (floor (/  i *dim*))) 
    (setf col (mod i *dim* ))
    (setf dim (* *dim* *dim*))

    (if
        (< i  dim)
    (progn
    (setf val (aref *a* row col))
    (if (eq val 1) (return-from find_lowest (list row col val)))
    (cond 
       ((null val) (return-from find_lowest (find_lowest (+ i 1) lowest)))
       ((or (null lowest) (< val (car (cdr (cdr lowest)))))
         (progn
           (setf lowest (list row col val))
           ); end of progn
           ) ; end of cond2
     ); end cond
     (return-from find_lowest (find_lowest (+ i 1) lowest))
     ); end of progn
       (return-from find_lowest lowest)
     );
  ); end of let
 ); end of function
 
(defun find_highest(i highlist) 
  (let* (row col val dim)
    (setf row (floor (/  i *dim*)))
    (setf col (mod i *dim* ))
    (setf dim (* *dim* *dim*))
    (if
        (and (not (null i)) (< i  dim))
    (progn
    (setf i (+ i 1))
    (setf val (aref *a* row col))
    (if (eq val dim) (return-from find_highest (list row col val)))
    (cond 
       ((null val) (return-from find_highest (find_highest i highlist)))
       ((null highlist) (setf highlist (list row col val)))
       ((> val (car (cdr (cdr highlist)))) (setf highlist (list row col val)))
     ); end cond
     (return-from find_highest (find_highest i highlist))
     ); end of progn
       (return-from find_highest highlist)
     );
  ); end of let
 ); end of function

(defun createlist(bound)
  (let* (current newlist)
  (setq newlist (list))
  (setq current (list))
  (loop while (< bound (car (last ( find_highest 0 nil))))
    do (progn
    (setq current (find_next 0 nil bound))
    (setq newlist (append newlist (list current))) 
    (setq bound (car (cdr(cdr current))))
         ); end of do
    ); end of while
 (return-from createlist newlist)
 ); end of let
 ); end of function

;;;; Created on 2013-11-23 00:39:34
(defun find_next(i lowest bound) 
  (let* (row col val dim)
    (setf row (floor (/  i *dim*)))
    (setf col (mod i *dim* ))
    (setf dim (* *dim* *dim*))
    (if (null lowest) (setf lowest (find_highest 0 nil)))
    (if
        (< i  dim)
    (progn
    (setf val (aref *a* row col))
    (cond 
       ((null val) (return-from find_next (find_next (+ i 1) lowest bound)))
       ((and (< val (car (cdr (cdr lowest)))) (> val bound))
         (progn
           (setf lowest (list row col val))
           ); end of progn
           ) ; end of cond2
     ); end cond
     (return-from find_next (find_next (+ i 1) lowest bound))
     ); end of progn
       (return-from find_next lowest)
     );
  ); end of let
 ); end of function

;;;; Created on 2013-11-22 15:11:03
(defun up (row col val stopVal)
  (let* (var)
  (setf var (+ row 1))
  (setf val (+ val 1))
    
  (if (and (> *dim* var) (eq (aref *a* var col) val ))   
    (progn 
    (cond 
      ((eq val stopVal) (return-from up 1))
      ((up var col val stopVal) (return-from up 1))
      ((right var col val stopVal) (return-from up 1))
      ((left var col val stopVal) (return-from up 1))
      (t (return-from up nil))
    ); end of cond
    ); end of progn
    (return-from up nil))
    ); end of let
)
(defun down (row col val stopVal)
  (let* (var)
  (setf var (- row 1))
  (setf val (+ val 1))
  (if (and (> var -1) (eq (aref *a* var col) val ))
    (progn 

    (cond 
      ((eq val stopVal) (return-from down 1))
      ((down var col val stopVal) (return-from down 1))
      ((right var col val stopVal) (return-from down 1))
      ((left var col val stopVal) (return-from down 1))
      (t (return-from down nil))
      ); end of cond
    ); end of progn
    (return-from down nil))
    ); end of let
)
(defun right (row col val stopVal)
  (let* (var)
  (setf var (+ col 1))
  (setf val (+ val 1))
  (if (and (> *dim* var) (eq (aref *a* row var) val ))
    (progn 

    (cond 
      ((eq val stopVal) (return-from right 1))
      ((up row var val stopVal) (return-from right 1))
      ((right row var val stopVal) (return-from right 1))
      ((down row var val stopVal) (return-from right 1))
      (t (return-from right nil))
      ); end of cond
    ); end of progn
    (return-from right nil))
    ); end of let
)
(defun left (row col val stopVal)
  (let* (var)
  (setf var (- col 1))
  (setf val (+ val 1))
  (if (and (> var -1) (eq (aref *a* row var) val ))
    (progn  

    (cond 
      ((eq val stopVal) (return-from left 1))
      ((up row var val stopVal) (return-from left 1))
      ((left row var val stopVal) (return-from left 1))
      ((down row var val stopVal) (return-from left 1))
      (t (return-from left nil))
      );end of cond
    ); end of progn
    (return-from left nil))
    ); end of let
)

(defun validate_board ()
 
 ; get lowest go left, down, up, right
  (let* (row col val alist stopVal)
  (setf alist (find_lowest 0 nil))
  (setf row (car alist))
  (setf col (car(cdr alist)))
  (setf val (car (cdr(cdr alist))))
  (setf stopVal (* *dim* *dim*))
   ; if value == dim * dim then board valid return 1
   ; else recurse or return 0;
  (if (eq val 1) () (return-from validate_board nil))
  (if (and (eq val stopVal) (null *removable*)) (return-from validate_board 1)
    (cond
    ((up row col val stopVal) (return-from validate_board 1))
    ((right row col val stopVal)(return-from validate_board 1))
    ((left row col val stopVal) (return-from validate_board 1))
    ((down row col val stopVal)(return-from validate_board 1))
    (t (return-from  validate_board nil))
    ); end cond
   ); end if
   ); end of let
); end of function
(defun validate_seq(row col stopVal)
 
 ; get lowest go left, down, up, right
  (let* (val)
  (setf val (aref *a* row col))
   ; if value == dim * dim then board valid return 1
   ; else recurse or return 0;
  (if (eq val stopVal)  (return-from validate_seq 1)
    (cond
    ((up row col val stopVal) (return-from validate_seq 1))
    ((right row col val stopVal)(return-from validate_seq 1))
    ((left row col val stopVal) (return-from validate_seq 1))
    ((down row col val stopVal)(return-from validate_seq 1))
    (t (return-from  validate_seq nil))
    ); end cond
   ); end if
   ); end of let
); end of function

;---------------------AI -----------------------------

  ;  if *removable* empty  return validate board							  [only validate board once all items have been used in order to save time]
  ;  if distance ==0 and val == stopVal and AI (x y val newstopVal) return 1  [string fits try the next string]
 								         
  ;	 else recurse															  [did not reach end of string yet]
  ;		if distance ==1 
  ;		if a[row, col] == null and (up or right or left); then make play and decrease distance
  ;		else return nil
 
; to do 

(defun solver(constants)
 (let* (firstList row col val secList secrow seccol secval)
 (if (null constants)(return-from solver 1))
 (setf firstList (car constants))
 (setf row (car firstList))
 (setf col (car (cdr firstList)))
 (setf val (car (cdr (cdr firstList))))
 (if (eq val (* *dim* *dim*)) (return-from solver 1))
 (if (eq (length constants) 1) 
   (progn
     (setf secrow (* *dim* *dim*))
     (setf seccol (* *dim* *dim*))
     (setf secval (* *dim* *dim*))
    ); end of progn - true statement
    (progn
     (setf secList (car (cdr constants)))	
	 (setf secrow (car secList))
	 (setf seccol (car (cdr secList)))
	 (setf secval (car (cdr (cdr secList))))
    ); end of progn false statement
  ); end of if
  ;(print "Called solver")(print (list firstList secList))
   (if (eq *toggle* 0) (if (eq val 1) (setf *toggle* 2) (setf *toggle* 1)))
   (if (eq *toggle* 1)
    (progn
    (setf *toggle* 2)
    ;(princ "Inside toggle")
    (cond
    ((and (> (- row 1) -1)     (null (aref *a* (- row 1) col)) (playback (- row 1) col val constants))
      (progn  (return-from solver  1)))
    ((and  (> *dim* (+ col 1)) (null (aref *a* row (+ col 1))) (playback row  (+ col 1) val constants)) 
      (progn  (return-from solver  1)))
    ((and (> (- col 1) -1)     (null (aref *a* row (- col 1))) (playback row  (- col 1) val constants))
      (progn  (return-from solver  1)))
     ((and (> *dim* (+ row 1)) (null (aref *a* (+ row 1) col)) (playback (+ row 1) col val constants)) 
      (progn  (return-from solver   1)))
    (t (progn (return-from solver nil)))
    ); end cond 
    ); end of progn
    );  end if
   
		  (cond
          ((and (eq val (- secval 1))
               (or 
                   (and (> *dim* (+ row 1))(eq (aref *a* (+ row 1) col) secval))
	               (and (> (- row 1) -1)   (eq (aref *a* (- row 1) col) secval))
	               (and (> *dim* (+ col 1))(eq (aref *a* row (+ col 1)) secval))
	               (and (> (- col 1) -1)   (eq (aref *a* row (- col 1)) secval))
                 ); end of or
             ); end of and
             (return-from solver (solver (cdr constants)))
          );end of clause 1
          ); end of cond
         (cond 
          ((and (not (null secrow)) (not (null seccol)) (> secrow row) (> seccol col)) ; go up right left down
		          (cond
			      ((and (> *dim* (+ row 1)) (null (aref *a* (+ row 1) col)) (play (+ row 1) col val secrow seccol secval constants)) (return-from solver 1))
			      ((and (> *dim* (+ col 1)) (null (aref *a* row (+ col 1))) (play row (+ col 1) val secrow seccol secval constants)) (return-from solver 1))
			      ((and (> (- col 1) -1) (null (aref *a* row (- col 1)))    (play row (- col 1) val secrow seccol secval constants)) (return-from solver 1))
			      ((and (> (- row 1) -1) (null (aref *a* (- row 1) col))    (play (- row 1) col val secrow seccol secval constants)) (return-from solver 1))
		          (t (return-from solver nil))
		  )); end clause 1
		  ((and (not (null seccol)) (> seccol col)) ; if seccol < col or secval <= val - go right down up left
		          (cond
			      ((and (> *dim* (+ col 1)) (null (aref *a* row (+ col 1))) (play row (+ col 1) val secrow seccol secval constants)) (return-from solver 1))
			      ((and (> (- row 1) -1) (null (aref *a* (- row 1) col))    (play (- row 1) col val secrow seccol  secval constants)) (return-from solver 1))
		          ((and (> *dim* (+ row 1)) (null (aref *a* (+ row 1) col)) (play (+ row 1) col val secrow seccol secval constants)) (return-from solver 1))
			      ((and (> (- col 1) -1) (null (aref *a* row (- col 1)))    (play row (- col 1) val secrow seccol secval constants)) (return-from solver 1))
			      (t (return-from solver nil))
		  )); end clause 2
		  ((and (not (null secrow)) (> secrow row)) ; if seccol < col or secval <= val  - go up left right down
		          (cond
			      ((and (> *dim* (+ row 1)) (null (aref *a* (+ row 1) col)) (play (+ row 1) col val secrow seccol secval constants)) (return-from solver 1))
			      ((and (> (- col 1) -1) (null (aref *a* row (- col 1)))    (play row (- col 1) val secrow seccol secval constants)) (return-from solver 1))
			      ((and (> *dim* (+ col 1)) (null (aref *a* row (+ col 1))) (play row (+ col 1) val secrow seccol secval constants)) (return-from solver 1))
			      ((and (> (- row 1) -1) (null (aref *a* (- row 1) col))    (play (- row 1) col val secrow seccol secval constants)) (return-from solver 1))
		          (t (return-from solver nil))
		  )); end clause 3
		  ((null seccol)  ; right down up left
                  (cond		
			      ((and (> *dim* (+ col 1)) (null (aref *a* row (+ col 1))) (play row (+ col 1) val secrow seccol secval constants)) (return-from solver 1))
			      ((and (> (- row 1) -1) (null (aref *a* (- row 1) col))    (play (- row 1) col val secrow seccol secval constants)) (return-from solver 1))
		          ((and (> *dim* (+ row 1)) (null (aref *a* (+ row 1) col)) (play (+ row 1) col val secrow seccol secval constants)) (return-from solver 1))
			      ((and (> (- col 1) -1) (null (aref *a* row (- col 1)))    (play row (- col 1) val secrow seccol secval constants)) (return-from solver 1))
			      (t (return-from solver nil))
         ))
		  (t ; if seccol < col or secval <= val - go down left right up
		          (cond
			      ((and (> (- row 1) -1) (null (aref *a* (- row 1) col))    (play (- row 1) col val secrow seccol secval constants)) (return-from solver 1))
		          ((and (> (- col 1) -1) (null (aref *a* row (- col 1)))    (play row (- col 1) val secrow seccol secval constants)) (return-from solver 1))
			      ((and (> *dim* (+ col 1)) (null (aref *a* row (+ col 1))) (play row (+ col 1) val secrow seccol secval constants)) (return-from solver 1))
			      ((and (> *dim* (+ row 1)) (null (aref *a* (+ row 1) col)) (play (+ row 1) col val secrow seccol secval constants)) (return-from solver 1))
			      (t (return-from solver nil))
		  )); end clause 4
		  ); end cond
  ); end of let
  ); end of function


;;;; Created on 2013-11-29 13:07:25
(defun play(row col val secrow seccol stopVal constants)
  (let* (newrow newcol mdist dist)
  (setf newrow (- *dim* row))
  (setf newcol (+ col 1))
  (setf val (+ val 1))
  ;(print "Try up ")(print (list (list newrow newcol val) (list row col val) (list stopVal)))
  (if (and (eq stopVal (* *dim* *dim*)) (eq val stopVal) (play_move_ai newrow newcol val)) (return-from play 1))
  
  (if (/= secrow (* *dim* *dim*)) 
  (progn
  (setf mdist (- stopVal val))
  (setf dist  (+(abs (- secrow row)) (abs (- seccol col))))
  ;(print "distance")(print (list(list dist mdist) (list row col val) (list secrow seccol stopVal)))
  (if (> dist mdist) (return-from play nil))
   ); end of progn
   ); end of if
    (if (> val  (- stopVal 1)) (progn (return-from play nil)))
    (if (eq val (- stopVal 1))
      (if (and (stopCond row col val stopVal) (play_move_ai newrow newcol val))   
      (if (solver (cdr constants)) (return-from play 1) (progn (remove_ai newrow newcol) (return-from play nil)))
      (return-from play nil)
      ); end inner if
    ); end of outter if
    (if (null (play_move_ai newrow newcol val)) (return-from play nil))
    
    (cond 
     ((and (not (null secrow)) (not (null seccol)) (> secrow row) (> seccol col)) ; go right up down left   
		    (cond
		    ((and  (> *dim* (+ col 1)) (null (aref *a* row (+ col 1))) (play row  (+ col 1) val secrow seccol stopVal constants)) 
		      (progn  (return-from play 1)))
		    ((and (> *dim* (+ row 1)) (null (aref *a* (+ row 1) col))  (play (+ row 1) col val secrow seccol stopVal constants)) 
		      (progn   (return-from play 1))); never called
		    ((and (> (- col 1) -1) (null (aref *a* row (- col 1)))     (play row  (- col 1) val secrow seccol stopVal constants)) 
		      (progn  (return-from play 1)))
            ((and (> (- row 1) -1) (null (aref *a* (- row 1) col))     (play (- row 1) col val secrow seccol stopVal constants))
              (progn (return-from play 1)))
		   (t (progn  (remove_ai newrow newcol) (return-from play nil)))
		    ); end cond
		) ;end of clause 1
		((and (not (null seccol)) (> seccol col)) ; if seccol < col or secval <= val - go right down up left 
		    (cond
		    ((and  (> *dim* (+ col 1)) (null (aref *a* row (+ col 1))) (play row  (+ col 1) val secrow seccol stopVal constants)) 
		      (progn  (return-from play 1)))
		    ((and (> (- row 1) -1) (null (aref *a* (- row 1) col))     (play (- row 1) col val secrow seccol stopVal constants))
              (progn (return-from play 1)))
           ((and (> *dim* (+ row 1)) (null (aref *a* (+ row 1) col))   (play (+ row 1) col val secrow seccol stopVal constants)) 
		      (progn   (return-from play 1))); never called
		    ((and (> (- col 1) -1) (null (aref *a* row (- col 1)))     (play row  (- col 1) val secrow seccol stopVal constants)) 
		      (progn  (return-from play 1)))
		   (t (progn  (remove_ai newrow newcol) (return-from play nil)))
		    ); end cond
		) ;end of clause 2
		((and (not (null secrow)) (> secrow row)) ; if seccol < col or secrow >= row  - go left up down right    
		   (cond
		    ((and (> (- col 1) -1) (null (aref *a* row (- col 1)))     (play row  (- col 1) val secrow seccol stopVal constants)) 
		      (progn  (return-from play 1)))
		    ((and (> *dim* (+ row 1)) (null (aref *a* (+ row 1) col))  (play (+ row 1) col val secrow seccol stopVal constants)) 
		      (progn   (return-from play 1))); never called
		    ((and  (> *dim* (+ col 1)) (null (aref *a* row (+ col 1))) (play row  (+ col 1) val secrow seccol stopVal constants)) 
		      (progn  (return-from play 1)))
		    ((and (> (- row 1) -1) (null (aref *a* (- row 1) col))     (play (- row 1) col val secrow seccol stopVal constants))
              (progn (return-from play 1)))
       (t (progn  (remove_ai newrow newcol) (return-from play nil)))
		    ); end cond
		) ;end of clause 3
		((null seccol)    ; right down up left
		    (cond
		    ((and  (> *dim* (+ col 1)) (null (aref *a* row (+ col 1))) (play row  (+ col 1) val secrow seccol stopVal constants)) 
		      (progn  (return-from play 1)))
		    ((and (> (- row 1) -1) (null (aref *a* (- row 1) col))     (play (- row 1) col val secrow seccol stopVal constants))
              (progn (return-from play 1)))
            ((and (> *dim* (+ row 1)) (null (aref *a* (+ row 1) col))  (play (+ row 1) col val secrow seccol stopVal constants)) 
		      (progn   (return-from play 1))); never called
		    ((and (> (- col 1) -1) (null (aref *a* row (- col 1)))     (play row  (- col 1) val secrow seccol stopVal constants)) 
		      (progn  (return-from play 1)))
		   (t (progn  (remove_ai newrow newcol) (return-from play nil)))
		    ); end cond
		) ;end of clause 4
		(t    ;if seccol < col or secval <= val - go down left right up
		   (cond
		   ((and (> (- row 1) -1) (null (aref *a* (- row 1) col))     (play (- row 1) col val secrow seccol stopVal constants))
              (progn (return-from play 1)))
           ((and (> (- col 1) -1) (null (aref *a* row (- col 1)))     (play row  (- col 1) val secrow seccol stopVal constants)) 
		      (progn  (return-from play 1)))
           ((and  (> *dim* (+ col 1)) (null (aref *a* row (+ col 1))) (play row  (+ col 1) val secrow seccol stopVal constants)) 
		      (progn  (return-from play 1)))
		   ((and (> *dim* (+ row 1)) (null (aref *a* (+ row 1) col))  (play (+ row 1) col val secrow seccol stopVal constants)) 
		      (progn   (return-from play 1))); never called
		   (t (progn  (remove_ai newrow newcol) (return-from play nil)))
		    ); end cond
		) ;end of clause 5
		); end of cond
    ); end of let
  ); end of function
   
  
(defun remove_ai (rowin colin)
  (let* (row col)
  (setf row (- *dim* rowin))
  (setf col (- colin 1))
  (if (not (null (aref *a* row col))) 
  (progn
	  (setf *lst* (append *lst* (list (aref *a* row col))))  
	  (setf (aref *a* row col) nil)
      ;(print "Removed item")
    ))
    ); end of let
); end of remove function


(defun play_move_ai (rowin colin val)
(let* ((row (- *dim* rowin)) (col (- colin 1)))
  (if (and (validate row col val) (< val (+ (* *dim* *dim*) 1)) (numberp (position val *lst*)) (null (aref *a* row col)))     
    (progn
          (setf (aref *a* row col) val)
          (setf *moves* (append *moves* (list(list rowin colin val))))
          (setf *lst*(remove val *lst*))	
         ; (drawboard)
          ;(print (list rowin colin val))
          (return-from play_move_ai 1)
         ) ; end of prog
        (progn
         
          ;(print "Move not Valid")(print (list rowin colin val))
          (return-from play_move_ai nil)
          )
   ); end of if 
   ); end of let
); end of function

(defun playback(row col val constants)
  (let* (newrow newcol)
  (setf newrow (- *dim* row))
  (setf newcol (+ col 1))
  (setf val (- val 1))
 ;(print (list (list newrow newcol val) (list row col val)))
    (if (< val 1) (progn (return-from playback nil)))
    (if (eq val 1)
      (if (play_move_ai newrow newcol val) 
        (if (solver constants)
          (progn (return-from playback 1))
          (progn (remove_ai newrow newcol)(return-from playback nil))
          ); end of if
          (return-from playback nil)
        ); end of if
      ); end of if
    (if (null (play_move_ai newrow newcol val)) (return-from playback nil))
    (cond
    ((and (> (- row 1) -1)     (null (aref *a* (- row 1) col)) (playback (- row 1) col val constants))
      (progn (return-from playback 1)))
    ((and  (> *dim* (+ col 1)) (null (aref *a* row (+ col 1))) (playback row  (+ col 1) val constants)) 
      (progn  (return-from playback 1)))
    ((and (> (- col 1) -1)     (null (aref *a* row (- col 1))) (playback row  (- col 1) val constants))
      (progn  (return-from playback 1)))
     ((and (> *dim* (+ row 1)) (null (aref *a* (+ row 1) col)) (playback (+ row 1) col val constants)) 
      (progn  (return-from playback 1)))
    (t (progn  (remove_ai newrow newcol) (return-from playback nil)))
    ); end cond
    ); end of let
  ); end of function
  
;;;; Created on 2013-11-26 20:56:16
;--------------------------------------------------------------------------------
(defun stopCond(row col val stopVal)
(let* (newrow newcol)
(setf newrow (- *dim* row))
(setf newcol (+ col 1))
(if (eq stopval (* *dim* *dim*)) 
  (progn
      (cond
    ((and (> *dim* (+ row 1)) (null (aref *a* (+ row 1) col)))  (play_move_ai (- newrow 1) newcol stopVal)) 
    ((and  (> *dim* (+ col 1)) (null (aref *a* row (+ col 1)))) (play_move_ai newrow (+ newcol 1) stopVal))
    ((and (> (- col 1) -1) (null (aref *a* row (- col 1))))		(play_move_ai newrow (- newcol 1) stopVal))
    ((and (> (- row 1) -1) (null (aref *a* (- row 1) col)))	    (play_move_ai (+ newrow 1) newcol stopVal))
   ;(t (return-from stopCond nil))
    ); end cond
  (return-from stopCond 1)
  ); end of progn
); end of if
(if 
(or
 (and (> *dim* (+ row 1)) (eq (aref *a* (+ row 1) col) stopVal)
   (or 
       (and (> (- row 1) -1)    (eq (aref *a* (- row 1) col) (- val 1)))
       (and (> (- col 1) -1)    (eq (aref *a* row (- col 1)) (- val 1)))
       (and (> *dim* (+ col 1)) (eq (aref *a* row (+ col 1)) (- val 1)))
     ); end of or
     ); end of clause1
 (and (> (- row 1) -1) (eq (aref *a* (- row 1) col) stopVal)
   (or 
       (and (> *dim* (+ row 1))    (eq (aref *a* (+ row 1) col) (- val 1)))
       (and (> (- col 1) -1)    (eq (aref *a* row (- col 1)) (- val 1)))
       (and (> *dim* (+ col 1)) (eq (aref *a* row (+ col 1)) (- val 1)))
     ); end of or
     ); end of clause2
(and (> *dim* (+ col 1)) (eq (aref *a* row (+ col 1)) stopVal)
   (or 
       (and (> (- row 1) -1)    (eq (aref *a* (- row 1) col) (- val 1)))
       (and (> (- col 1) -1)    (eq (aref *a* row (- col 1)) (- val 1)))
       (and (> *dim* (+ row 1))    (eq (aref *a* (+ row 1) col) (- val 1)))
     ); end of or
     ); end of clause3
 (and (> (- col 1) -1) (eq (aref *a* row (- col 1)) stopVal) 
   (or 
       (and (> (- row 1) -1)    (eq (aref *a* (- row 1) col) (- val 1)))
       (and (> *dim* (+ row 1))    (eq (aref *a* (+ row 1) col) (- val 1)))
       (and (> *dim* (+ col 1)) (eq (aref *a* row (+ col 1)) (- val 1)))
     ); end of or
     ); end of clause4
   ); end of or
;(progn (print "Stop cond true")(print (list row col val)) (return-from stopCond 1)) ; True condition
;(progn (print "Stop cond false")(print (list row col val)) (return-from stopCond nil)); false condition
(if (checkstate 0 val)(return-from stopCond 1) (return-from stopCond nil))
(return-from stopCond nil)
   ) ;end of if statement
   ); end of let
); end of function

;;;; Created on 2013-11-22 15:11:03
(defun up (row col val stopVal)
  (let* (var)
  (setf var (+ row 1))
  (setf val (+ val 1))
    
  (if (and (> *dim* var) (eq (aref *a* var col) val ))   
    (progn 
    (cond 
      ((eq val stopVal) (return-from up 1))
      ((up var col val stopVal) (return-from up 1))
      ((right var col val stopVal) (return-from up 1))
      ((left var col val stopVal) (return-from up 1))
      (t (return-from up nil))
    ); end of cond
    ); end of progn
    (return-from up nil))
    ); end of let
)
(defun down (row col val stopVal)
  (let* (var)
  (setf var (- row 1))
  (setf val (+ val 1))
  (if (and (> var -1) (eq (aref *a* var col) val ))
    (progn 

    (cond 
      ((eq val stopVal) (return-from down 1))
      ((down var col val stopVal) (return-from down 1))
      ((right var col val stopVal) (return-from down 1))
      ((left var col val stopVal) (return-from down 1))
      (t (return-from down nil))
      ); end of cond
    ); end of progn
    (return-from down nil))
    ); end of let
)
(defun right (row col val stopVal)
  (let* (var)
  (setf var (+ col 1))
  (setf val (+ val 1))
  (if (and (> *dim* var) (eq (aref *a* row var) val ))
    (progn 

    (cond 
      ((eq val stopVal) (return-from right 1))
      ((up row var val stopVal) (return-from right 1))
      ((right row var val stopVal) (return-from right 1))
      ((down row var val stopVal) (return-from right 1))
      (t (return-from right nil))
      ); end of cond
    ); end of progn
    (return-from right nil))
    ); end of let
)
(defun left (row col val stopVal)
  (let* (var)
  (setf var (- col 1))
  (setf val (+ val 1))
  (if (and (> var -1) (eq (aref *a* row var) val ))
    (progn  

    (cond 
      ((eq val stopVal) (return-from left 1))
      ((up row var val stopVal) (return-from left 1))
      ((left row var val stopVal) (return-from left 1))
      ((down row var val stopVal) (return-from left 1))
      (t (return-from left nil))
      );end of cond
    ); end of progn
    (return-from left nil))
    ); end of let
)

(defun validate_board ()
 
 ; get lowest go left, down, up, right
  (let* (row col val alist stopVal)
  (setf alist (find_lowest 0 nil))
  (setf row (car alist))
  (setf col (car(cdr alist)))
  (setf val (car (cdr(cdr alist))))
  (setf stopVal (* *dim* *dim*))
   ; if value == dim * dim then board valid return 1
   ; else recurse or return 0;
  (if (eq val 1) () (return-from validate_board nil))
  (if (and (eq val stopVal) (null *removable*)) (return-from validate_board 1)
    (cond
    ((up row col val stopVal) (return-from validate_board 1))
    ((right row col val stopVal)(return-from validate_board 1))
    ((left row col val stopVal) (return-from validate_board 1))
    ((down row col val stopVal)(return-from validate_board 1))
    (t (return-from  validate_board nil))
    ); end cond
   ); end if
   ); end of let
); end of function
(defun validate_seq(row col stopVal)
 
 ; get lowest go left, down, up, right
  (let* (val)
  (setf val (aref *a* row col))
   ; if value == dim * dim then board valid return 1
   ; else recurse or return 0;
  (if (eq val stopVal)  (return-from validate_seq 1)
    (cond
    ((up row col val stopVal) (return-from validate_seq 1))
    ((right row col val stopVal)(return-from validate_seq 1))
    ((left row col val stopVal) (return-from validate_seq 1))
    ((down row col val stopVal)(return-from validate_seq 1))
    (t (return-from  validate_seq nil))
    ); end cond
   ); end if
   ); end of let
); end of function

;--------------------- Board State Validation ----------------------

;;;; Created on 2013-11-26 20:56:16
;--------------------------------------------------------------------------------
(defun checkstate(i currentval) 
  (let* (row col max)
   
    (setf max (* *dim* *dim*))
	    (loop while (< i max) do
		    (setf row (floor (/  i *dim*))) 
		    (setf col (mod i *dim* ))
            (if (null (aref *a* row col))
		    (if (null (check row col currentval)) (return-from checkstate nil))
            ); end of if
		    (setf i (+ i 1))
	     ); end of while
     (return-from checkstate 1)
    ); end of let
  ); end of function

(defun check(row col val)
(let* (v1 v2 v3 v4 iscellv1 iscellv2 iscellv3 iscellv4 emptycelv1 emptycelv2 emptycelv3 emptycelv4)
(setf iscellv1 (> *dim* (+ row 1)))
(setf iscellv2 (> *dim* (+ col 1)))
(setf iscellv3 (> (- col 1) -1))
(setf iscellv4 (> (- row 1) -1)) 
     ; if it is a cell get its value
    (if iscellv1 (setf v1 (aref *a* (+ row 1) col)))
    (if iscellv2 (setf v2 (aref *a* row (+ col 1))))
    (if iscellv3 (setf v3 (aref *a* row (- col 1))))
    (if iscellv4 (setf v4 (aref *a* (- row 1) col)))
    
    (setf emptycelv1 (and iscellv1 (null v1)))
    (setf emptycelv2 (and iscellv2 (null v2)))
    (setf emptycelv3 (and iscellv3 (null v3)))
    (setf emptycelv4 (and iscellv4 (null v4)))
    (cond ; if a cell has 2 blanks return 1
    ((and emptycelv1 (or emptycelv2 emptycelv3 emptycelv4))  (return-from check 1))
    ((and emptycelv2 (or emptycelv1 emptycelv3 emptycelv4))  (return-from check 1))
    ((and emptycelv3 (or emptycelv1 emptycelv2 emptycelv4))  (return-from check 1))
    ((and emptycelv4 (or emptycelv1 emptycelv2 emptycelv3))  (return-from check 1))
     ; check if the at least one number is > current val
     ((and iscellv1 v1 (> v1 val)) (return-from check 1)) 
     ((and iscellv2 v2 (> v2 val)) (return-from check 1))
     ((and iscellv3 v3 (> v3 val)) (return-from check 1)) 
     ((and iscellv4 v4 (> v4 val)) (return-from check 1))
     ; if it has only one empty spot do something
    (emptycelv1 (return-from check (checkup   (+ row 1) col    val)))
    (emptycelv2 (return-from check (checkright   row (+ col 1) val)))
   ; (emptycelv3 (return-from check (checkleft    row (- col 1) val)))
   ; (emptycelv4 (return-from check (checkdown (- row 1) col    val)))
    (t (return-from check nil)) ; no empty spots and neighbors < current val
    ); end of cond
  ); end of let
); end of function

(defun checkup(row col val)
(let* (v1 v2 v3 iscellv1 iscellv2 iscellv3 emptycelv1 emptycelv2 emptycelv3)
(setf iscellv1 (> *dim* (+ row 1)))
(setf iscellv2 (> *dim* (+ col 1)))
(setf iscellv3 (> (- col 1) -1))
;(setf iscellv4 (> (- row 1) -1))
   
     ; if it is a cell get its value
    (if  iscellv1 (setf v1 (aref *a* (+ row 1) col)))
    (if  iscellv2 (setf v2 (aref *a* row (+ col 1))))
    (if  iscellv3 (setf v3 (aref *a* row (- col 1))))

    (setf emptycelv1 (and iscellv1 (null v1)))
    (setf emptycelv2 (and iscellv2 (null v2)))
    (setf emptycelv3 (and iscellv3 (null v3)))
  
    (cond ; if a cell has 2 blanks return 1
    ((and emptycelv1 (or emptycelv2 emptycelv3))  (return-from checkup 1))
    ((and emptycelv2 (or emptycelv1 emptycelv3))  (return-from checkup 1))
    ((and emptycelv3 (or emptycelv1 emptycelv2))  (return-from checkup 1))

     ; check if the at least one number is > current val
     ((and iscellv1 v1 (> v1 val)) (return-from checkup 1)) 
     ((and iscellv2 v2 (> v2 val)) (return-from checkup 1))
     ((and iscellv3 v3 (> v3 val)) (return-from checkup 1)) 
     ; if it has only one empty spot do something
    (emptycelv1 (return-from checkup 1))
    (emptycelv2 (return-from checkup 1))
    ;(emptycelv3 (return-from checkup 1))
    (t (return-from checkup nil)) ; no empty spots and neighbors < current val
    ); end of cond
  ); end of let
); end of function

(defun checkright(row col val)
(let* (v1 v2 v4 iscellv1 iscellv2 iscellv4 emptycelv1 emptycelv2 emptycelv4)

(setf iscellv1 (> *dim* (+ row 1)))
(setf iscellv2 (> *dim* (+ col 1)))
;(setf iscellv3 (> (- col 1) -1))
(setf iscellv4 (> (- row 1) -1))
   
     ; if it is a cell get its value
    (if  iscellv1 (setf v1 (aref *a* (+ row 1) col)))
    (if  iscellv2 (setf v2 (aref *a* row (+ col 1))))
   ; (if  iscellv3 (setf v3 (aref *a* row (- col 1))))
    (if  iscellv4 (setf v4 (aref *a* (- row 1) col)))
    
    (setf emptycelv1 (and iscellv1 (null v1)))
    (setf emptycelv2 (and iscellv2 (null v2)))
   ; (setf emptycelv3 (and iscellv3 (null v3)))
    (setf emptycelv4 (and iscellv4 (null v4)))
    (cond ; if a cell has 2 blanks return 1
    ((and emptycelv1 (or emptycelv2 emptycelv4))  (return-from checkright 1))
    ((and emptycelv2 (or emptycelv1 emptycelv4))  (return-from checkright 1))
    ;((and emptycelv3 (or emptycelv1 emptycelv2 emptycelv4))  (return-from check 1))
    ((and emptycelv4 (or emptycelv1 emptycelv2))  (return-from checkright 1))
     ; check if the at least one number is > current val
     ((and iscellv1 v1 (> v1 val)) (return-from checkright 1)) 
     ((and iscellv2 v2 (> v2 val)) (return-from checkright 1))
     ;((and iscellv3 (> v3 val)) (return-from check 1)) 
    ; ((and iscellv4 v4 (> v4 val)) (return-from checkright 1))
     ; if it has only one empty spot do something
    (emptycelv1 (return-from checkright 1))
    (emptycelv2 (return-from checkright 1))
    ;(emptycelv3 (return-from check (checkleft    row (- col 1) )))
    (emptycelv4 (return-from checkright 1))
    (t (return-from checkright nil)) ; no empty spots and neighbors < current val
    ); end of cond
  ); end of let
); end of function
(defun checkleft(row col val)
(let* (v1 v3 v4 iscellv1 iscellv3 iscellv4 emptycelv1 emptycelv3 emptycelv4)
(setf iscellv1 (> *dim* (+ row 1)))
;(setf iscellv2 (> *dim* (+ col 1)))
(setf iscellv3 (> (- col 1) -1))
(setf iscellv4 (> (- row 1) -1))
   
     ; if it is a cell get its value
    (if  iscellv1 (setf v1 (aref *a* (+ row 1) col)))
    ;(if  iscellv2 (setf v2 (aref *a* row (+ col 1))))
    (if  iscellv3 (setf v3 (aref *a* row (- col 1))))
    (if  iscellv4 (setf v4 (aref *a* (- row 1) col)))
    
    (setf emptycelv1 (and iscellv1 (null v1)))
   ; (setf emptycelv2 (and iscellv2 (null v2)))
    (setf emptycelv3 (and iscellv3 (null v3)))
    (setf emptycelv4 (and iscellv4 (null v4)))
    (cond ; if a cell has 2 blanks return 1
    ((and emptycelv1 (or emptycelv3 emptycelv4))  (return-from checkleft 1))
    ;((and emptycelv2 (or emptycelv1 emptycelv3 emptycelv4))  (return-from check 1))
    ((and emptycelv3 (or emptycelv1 emptycelv4))  (return-from checkleft 1))
    ((and emptycelv4 (or emptycelv1 emptycelv3))  (return-from checkleft 1))
     ; check if the at least one number is > current val
     ((and iscellv1 v1 (> v1 val)) (return-from checkleft 1)) 
    ; ((and iscellv2 (> v2 val)) (return-from check 1))
     ((and iscellv3 v3 (> v3 val)) (return-from checkleft 1)) 
     ((and iscellv4 v4 (> v4 val)) (return-from checkleft 1))
     ; if it has only one empty spot do something
    (emptycelv1 (return-from checkleft 1))
    ;(emptycelv2 (return-from check (checkright   row (+ col 1) )))
    (emptycelv3 (return-from checkleft 1))
    (emptycelv4 (return-from checkleft 1))
    (t (return-from checkleft nil)) ; no empty spots and neighbors < current val
    ); end of cond
  ); end of let
); end of function
(defun checkdown(row col val)
(let* (v2 v3 v4 iscellv2 iscellv3 iscellv4 emptycelv2 emptycelv3 emptycelv4)
;(setf iscellv1 (> *dim* (+ row 1)))
(setf iscellv2 (> *dim* (+ col 1)))
(setf iscellv3 (> (- col 1) -1))
(setf iscellv4 (> (- row 1) -1))
   
     ; if it is a cell get its value
    ;(if  iscellv1 (setf v1 (aref *a* (+ row 1) col)))
    (if  iscellv2 (setf v2 (aref *a* row (+ col 1))))
    (if  iscellv3 (setf v3 (aref *a* row (- col 1))))
    (if  iscellv4 (setf v4 (aref *a* (- row 1) col)))
    
   ; (setf emptycelv1 (and iscellv1 (null v1)))
    (setf emptycelv2 (and iscellv2 (null v2)))
    (setf emptycelv3 (and iscellv3 (null v3)))
    (setf emptycelv4 (and iscellv4 (null v4)))
    (cond ; if a cell has 2 blanks return 1
    ;((and emptycelv1 (or emptycelv2 emptycelv3 emptycelv4))  (return-from check 1))
    ((and emptycelv2 (or emptycelv3 emptycelv4))  (return-from checkdown 1))
    ((and emptycelv3 (or emptycelv2 emptycelv4))  (return-from checkdown 1))
    ((and emptycelv4 (or emptycelv2 emptycelv3))  (return-from checkdown 1))
     ; check if the at least one number is > current val
     ;((and iscellv1 (> v1 val)) (return-from check 1)) 
     ((and iscellv2 v2 (> v2 val)) (return-from checkdown 1))
     ((and iscellv3 v3 (> v3 val)) (return-from checkdown 1)) 
     ((and iscellv4 v4 (> v4 val)) (return-from checkdown 1))
     ; if it has only one empty spot do something
    ;(emptycelv1 (return-from check (checkup   (+ row 1) col    )))
    (emptycelv2 (return-from checkdown 1))
    (emptycelv3 (return-from checkdown 1))
    (emptycelv4 (return-from checkdown 1))
    (t (return-from checkdown nil)) ; no empty spots and neighbors < current val
    ); end of cond
  ); end of let
); end of function
 