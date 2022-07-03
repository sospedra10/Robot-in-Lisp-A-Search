
(
	defun create-state(x y)
		(list x y)
)




(
	defun pos-x(state)
		(car state)
)


(
	defun pos-y(state)
		(car(cdr state))
)





(setf *initial-state* (create-state 72 72))


(setf *final-state* (create-state 324 216))



(
	defun is-final(state)
		(equal state *final-state*)
)


(setf *not-valid* '((36 36) (36 72) (36 108) (36 144) (36 360) (72 36) (72 252) (72 288) (72 324) (72 360) 
(108 108) (108 144) (108 180) (108 324) (108 360) (144 36) (144 72) (144 108) (144 144) (144 180) (144 216) 
(180 36) (180 72) (180 108) (180 252) (216 36) (216 180) (252 72) (252 180) (252 216) (288 108) (288 216) 
(288 252) (288 288) (288 324) (288 360) (324 108) (324 144) (324 324) (324 360) (360 36) (360 72) (360 108) (360 144) (360 360)))




(setf *operators* '(move-forward-r1 move-back-r1  move-forward-r2 move-back-r2))



(
	defun is-possible(state)
		(and 
			(not(member state *not-valid* :test #'equal))
			(>= (pos-x state) 36)
			(<= (pos-x state) 360)
			(>= (pos-y state) 36)
			(<= (pos-y state) 360)
		)
		
)


(
	defun move-forward-r1(state)
		(setf new-state (create-state (+ (pos-x state) 36) (pos-y state) ))
		(cond 
			((is-possible new-state) (setf state new-state))
			
		)
)

(
	defun move-back-r1(state)
		(setf new-state (create-state (- (pos-x state) 36) (pos-y state) ))
		(cond 
			((is-possible new-state) (setf state new-state))
		)
)

(
	defun move-forward-r2(state)
		(setf new-state (create-state (pos-x state) (+ (pos-y state) 36)))
		(cond 
			((is-possible new-state) (setf state new-state))
		)
)

(
	defun move-back-r2(state)
		(setf new-state (create-state (pos-x state) (- (pos-y state) 36) ))
		(cond 
			((is-possible new-state) (setf state new-state))
		)
)






(
	defun heuristics(state)
		(setf x (abs (- (pos-x state) (pos-x *final-state*) )))
		(setf y (abs (- (pos-y state) (pos-y *final-state*) )))
		(+ x y)
)


(
	defun cost-of-applying-operator(state operator)
		(+ 1 0)
)



(
	defstruct nodesch
	
		state
		path
		cost
		cost-and-heuristics
)



(
	defun successors(node)
		(setf all_successors '())
		
		(do((i 0 (incf i))) ((= i (length *operators*)) all_successors)
			(setf new_state (successor node (nth i *operators*)))
			
			(cond 	(new_state
					(setf new_cost (+ (cost-of-applying-operator (nodesch-state node) (nth i *operators*)) (nodesch-cost node)))
					(setf new_node (make-nodesch
									:state new_state
									:path (cons (nth i *operators*) (nodesch-path node))
									:cost new_cost
									:cost-and-heuristics (+ new_cost (heuristics new_state))
					))
					(push new_node all_successors)
					)
			)
			
		)
		
)

(
	defun myapply (operator state)
		(funcall (symbol-function operator) state)
)

(
	defun successor(node operator)
		(myapply operator (nodesch-state node))
)



(
	defun is-better(node nodes)
		(dolist (x nodes)
			(if (and (equalp (nodesch-state node) (nodesch-state x)) (<= (nodesch-cost x) (nodesch-cost node))) (return T))
		)
)



(
	defun remove-worse(l1 l2 l3)
		(setf new_list '())
		
		(dolist (x l1 new_list)
			(if (or (is-better x l2) (is-better x l3)) ()(push x new_list))
		)		
)



(
	defun new-or-better-successors(node l1 l2)
		(remove-worse (successors node) l1 l2)
)


(
	defun order-by-cost-plus-heuristics(nodes)
		(sort nodes #'< :key #'nodesch-cost-and-heuristics)
		
)




(
	defun a*algorithm()
		(setf node1 (make-nodesch 
			:state *initial-state*
			:path '()
			:cost 0
			:cost-and-heuristics (heuristics *initial-state*)
		) )

		(setf open '())
		(push node1 open)
		(setf closed '())
		(setf actual nil)
		(setf new-successors '())
		
		(loop  
			(if (= (length open) 0) (return))
			
			(setf actual (car open))
			(setf open (cdr open))
			(push actual closed)
			
			(cond	((is-final (nodesch-state actual))  (return actual))
					(t 
					(setf new-successors (new-or-better-successors actual open closed))
					(setf open (nconc new-successors open))
					(order-by-cost-plus-heuristics open)
					)
			)
			
		)
)




This would be the solution:


#S(NODESCH :STATE (324 216)
   :PATH
   (MOVE-FORWARD-R2 MOVE-FORWARD-R1 MOVE-FORWARD-R2 MOVE-FORWARD-R1
    MOVE-FORWARD-R1 MOVE-FORWARD-R1 MOVE-BACK-R2 MOVE-BACK-R2 MOVE-BACK-R1
    MOVE-BACK-R2 MOVE-BACK-R2 MOVE-FORWARD-R1 MOVE-FORWARD-R1 MOVE-FORWARD-R2
    MOVE-FORWARD-R1 MOVE-FORWARD-R2 MOVE-FORWARD-R1 MOVE-FORWARD-R2
    MOVE-FORWARD-R2 MOVE-FORWARD-R2 MOVE-FORWARD-R2)
   :COST 21 :COST-AND-HEURISTICS 21)



We can obsereve the state is the final state, the total cost of all the movements and the cost-and-heuristics is the same 
as the cost so we can observe that the heuristics are 0 in that state because is the final state.
Moreover, we have the list with all the operators we have used (the movements the robot has done to arrive to the solution)
in the node path ordered backwards.











