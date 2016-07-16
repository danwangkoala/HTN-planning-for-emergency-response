;;;; Created on 2013-10-12 10:15:04
(defdomain test
  (
  	(:method (prepare ?x ?deadline)
  		(	(same ?x a)
  			(not(done ?x)) )
  		(:ordered
  		  	(!fuel ?x ?start)
  			(!done ?x)	)
  			
  		(	(same ?x b)
  			(not(done ?x))	)
  		(:ordered
  		  	(fuel-weapon ?x)
  			(!done ?x)	)
  	)
  	(:method (fuel-weapon ?x)
  		()
  		(:unordered
  			(!fuel ?x ?start)
  			(!weapon ?x ?start)
  			;;(!fuel ?x ?start)
  		)
  	)
  	(:operator (!done ?x)
  		()
  		()
  		((done ?x))
  	)
  	(:operator (!fuel ?x ?start)
  		(
  			(prepare ?x ?deadline)
  			(time-line ?x ?t1)
  			(is ?fuel-station fuel-station)
  			(write-time ?fuel-station ?t2)
  			(assign ?start (call max ?t1 ?t2))
  			(call >= ?deadline (call + ?start 10))
  		)
  		(
  			(time-line ?x ?t1)
  			(write-time ?fuel-station ?t2)
  		)
  		(
  			(fueled ?x (call + ?start 10))
  			(time-line ?x (call + ?start 10))
  			(write-time ?fuel-station (call + ?start 10))
  		)
  	)
  	(:operator (!weapon ?x ?start)
  		(
  			(prepare ?x ?deadline)
  			(time-line ?x ?t1)
  			(is ?weapon-station weapon-station)
  			(write-time ?weapon-station ?t2)
  			(assign ?start (call max ?t1 ?t2))
  			(call >= ?deadline (call + ?start 10))
  		)
  		(
  			(time-line ?x ?t1)
  			(write-time ?weapon-station ?t2)
  		)
  		(
  			(weaponed ?x (call + ?start 10))
  			(time-line ?x (call + ?start 10))
  			(write-time ?weapon-station (call + ?start 10))
  		)
  	)
  	
  	(:- (same ?x ?x) nil) 
  )
)

;;;problem
(defproblem problem test
  (
	(is fuel-station1 fuel-station)
	(write-time fuel-station1 0)
	(is weapon-station1 weapon-station)
	(write-time weapon-station1 0)
	(prepare a 10)
  	(prepare b 20)
	(time-line a 0)
	(time-line b 0)
  )
  (:unordered
  	(prepare a 10)
  	(prepare b 20)
  )
)

(find-plans 'problem :verbose :plans)