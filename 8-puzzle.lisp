(defun 8puzzle (times &optional start)
	(if start (setq initial start) (setq initial (randomize goal times)))
	(print initial)
	(general-search initial #'successor #'goalp)
)

(setq goal '(0 1 2 3 4 5 6 7 8))

(defun goalp (node) (equalp goal node))

(defun move-blank (blank-position new-blank-position current-state)
;; moves the blank! i don't think this is used
	(rotatef (elt current-state blank-position) (elt current-state new-blank-position))
	current-state)

(defun misplaced-tile (node)
;;counts misplaced tiles
	(let ((sum 0))
		(loop for i from 0 to 8 do
			(if (eq (elt node i) (elt goal i)) nil (if (> (elt node i) 0) (incf sum))))
	sum))

(defun square-location (square-number)
;; x-y coordinates of numbered squares
	(aref '#( (0 2) (1 2) (2 2)
				 (0 1) (1 1) (2 1)
				 (0 0) (1 0) (2 0)) square-number))

(defun legal-moves (blank-position)
;; list of legal switches
	(aref '#(
				(1 3)
				(0 2 4)
				(1 5)
				(0 4 6)
				(1 3 5 7)
				(2 4 8)
				(3 7)
				(4 6 8)
				(5 7)) blank-position))


(defun randomize (node times)
	;; makes random moves on the goal state to create a solvable state
	(let* ((newlist (copy-seq node)))
		(loop for i from 1 to times do
			(rotatef (elt newlist (position 0 newlist)) (elt newlist (elt (legal-moves (position 0 newlist)) (random (list-length (legal-moves (position 0 newlist)))))))
		)
		newlist))

(defun manhattan-distance (node)
;;calculates manhattan distance
	(let ((sum 0))
		(loop for i from 1 to 8 do
			(incf sum (+ (abs (- (car (square-location (position i node))) (car (square-location (position i goal)))))(abs (- (car (cdr (square-location (position i node)))) (car (cdr (square-location(position i goal))))))))
		)
		sum))

(defun linear-conflict (node)
	;;oh my god how do you implement this in lisp
	;;i give up
)



(defstruct node
  (state nil)
  (parent nil)
  (depth 0)
  (path-cost 0))

(defun switch (list pos1 pos2)
	;;a non-destructive version of rotatef
	(let* ((newlist (copy-seq list)))
	(rotatef (elt newlist pos1) (elt newlist pos2))
	newlist))

(defun successor (state)
	;;expands successors
	(let* ((results ()))
		(loop for i in (legal-moves (position 0 state)) do (push (switch state (position 0 state) i) results))
		results
	)
)

(defun a-star (node)
;; a* search
(+ (node-depth node) (node-path-cost node)))

;;; the rest is all provided code (search.lisp and queue.lisp), with some modifications

;;;provided search code
(defun general-search (initial-state successor goalp &key (samep #'eql))
	;;this function has also been modified, remove enqueue and key key parameters
  (setf *expanded-nodes* 0)
  (let ((fringe (make-q)))
    (q-insert fringe (list (make-node :state initial-state :path-cost (manhattan-distance initial-state) ))) ;;CHANGE HERE
    (values
     (graph-search fringe nil successor goalp samep)
     *expanded-nodes*)
    ))

(defun graph-search (fringe closed successor goalp samep)
;;not modified
  (unless (q-emptyp fringe)
    (let ((node (q-remove fringe)))
      (cond ((funcall goalp (node-state node))
	     (action-sequence node))
            ((member (node-state node) closed
		     :test samep :key #'node-state)
	     (graph-search fringe closed successor goalp samep))
            (t
	     (let ((successors (expand successor node)))
	       (setf *expanded-nodes*
		     (+ *expanded-nodes* (length successors)))
	       (graph-search (q-insert fringe successors)
			     (cons node closed)
			     successor goalp samep)))
            ))
    ))


(defun action-sequence (node &optional (actions nil))
;;changed action to state
  (if (node-parent node)
    (action-sequence (node-parent node) (cons (node-state node) actions))
    actions
    ))


  (defvar *expanded-nodes*)

  (defun expand (successor node)
;; modified to remove action, modified path-cost and depth
  (let ((triples (funcall successor (node-state node))))
    (mapcar (lambda (state)
		(make-node :state state
			   :parent node
			   :depth (1+ (node-depth node))
			   :path-cost (manhattan-distance state) ;;CHANGE HERE
		))
	    triples)
    ))


;;; provided queue code
;;; only modifications: using priority queue, changed identity to a-star

(defstruct q
  (enqueue #'enqueue-priority)
  (key #'a-star)
  (last nil)
  (elements nil))

;;;; Operations on Queues

(defun q-emptyp (q)
  "Returns T if queue is empty."
  (= (length (q-elements q)) 0))       ; (length x) works for both lists and arrays with fill-pointers

(defun q-front (q)
  "Returns the element at the front of the queue."
  (elt (q-elements q) 0))              ; (elt x n) works for both lists and arrays

(defun q-remove (q)
  "Removes the element from the front of the queue and returns it."
  (if (listp (q-elements q))
      (pop (q-elements q))             ; (pop x) alters x by removing the car, then returns the item removed
    (heap-pop (q-elements q) (q-key q))))

(defun q-insert (q items)
  "Inserts the items into the queue, according to the queue's enqueuing function."
  (funcall (q-enqueue q) q items)
  q)

  (defun enqueue-priority (q items)
  "Inserts the items by priority determined by the queue's key function."
  ;; If first insert, create the heap
  (when (null (q-elements q))
    (setf (q-elements q) (make-heap)))
  ;; Now insert the items
  (mapc (lambda (item)
	  (heap-insert (q-elements q) item (q-key q)))
	items)
  )


(defun heap-val (heap i key) (funcall key (elt heap i)))
(defun heap-parent (i) (floor (1- i) 2))
(defun heap-left (i) (+ 1 i i))
(defun heap-right (i) (+ 2 i i))
(defun heap-leafp (heap i) (> i (1- (floor (length heap) 2))))

(defun heapify (heap i key)
  "Assume that the children of i are heaps, but that heap[i] may be
  larger than its children.  If it is, moves heap[i] down where it belongs."
  (unless (heap-leafp heap i)
    (let ((l (heap-left i))
	  (r (heap-right i)))
      (let ((smaller-child (if (and (< r (length heap))
				    (< (heap-val heap r key) (heap-val heap l key)))
			       r l)))
	(when (> (heap-val heap i key) (heap-val heap smaller-child key))
	  (rotatef (elt heap i) (elt heap smaller-child))    ; (rotatef x y) swaps values of x and y
	  (heapify heap smaller-child key))))
    ))

(defun heap-pop (heap key)
  "Pops the best (lowest valued) item off the heap."
  (let ((min (elt heap 0)))
    (setf (elt heap 0) (elt heap (1- (length heap))))
    (decf (fill-pointer heap))        ; (decf x) decrements x
    (heapify heap 0 key)
    min))

(defun heap-insert (heap item key)
  "Puts an item into a heap."
  (vector-push-extend nil heap)       ; (vector-push-extend value array) adds the value to the next
                                      ; available position in the array, incrementing the fill-pointer
                                      ; and increasing the size of the array if necessary.
  (setf (elt heap (heap-find-pos heap (1- (length heap)) (funcall key item) key))
	item)

  )

(defun heap-find-pos (heap i val key)
  "Bubbles up from i to find position for val, moving items down in the process."
  (cond ((or (zerop i) (< (heap-val heap (heap-parent i) key) val))
	 i)
	(t
	 (setf (elt heap i) (elt heap (heap-parent i)))
	 (heap-find-pos heap (heap-parent i) val key))
	))

(defun make-heap (&optional (size 100))
  (make-array size :fill-pointer 0 :adjustable t))





