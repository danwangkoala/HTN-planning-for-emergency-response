;;;; Created on 2014-07-14 13:10:37

(defdomain test
  (
   ;(material-at 0 3600 B1 400)
   (:method (material-at ?demand-loc ?quantity)
     ((m-at ?demand-loc ?quan1)(call < ?quan1 ?quantity)
      (supply ?supply-loc ?quan2)(call > ?quan2 0)(available-plane ?plane)
      (attribute ?plane ?capacity ?fuel-capacity ?speed ?burn-rate)
      (plane-loc ?plane ?supply-loc)
      (assign ?quan-deliver (call min (call - ?quantity ?quan1) ?quan2 ?capacity)))
     (:unordered
       (:immediate !!update-quan ?supply-loc ?demand-loc ?quan-deliver ?plane)
       (deliver-m ?supply-loc ?demand-loc ?quan-deliver ?plane)
       (material-at ?demand-loc ?quantity))
     ((m-at ?demand-loc ?quan1)(call >= ?quan1 ?quantity))
     ())
   
   (:operator (!!update-quan ?supply-loc ?demand-loc ?quan-deliver ?plane)
     ((supply ?supply-loc ?quan1) (m-at ?demand-loc ?quan2)(available-plane ?plane))
     ((supply ?supply-loc ?quan1) (m-at ?demand-loc ?quan2)(available-plane ?plane))
     ((supply ?supply-loc (call - ?quan1 ?quan-deliver)) (m-at ?demand-loc (call + ?quan2 ?quan-deliver))))
      
   (:method (deliver-m ?supply-loc ?demand-loc ?quan-deliver ?plane)
     ()
     (:ordered
       (load-fuel ?supply-loc ?quan-deliver ?plane)                   ;;----------------------------1
       (fly-unload ?supply-loc ?demand-loc ?quan-deliver ?plane)                           ;;----------------------------2
       (return-s ?demand-loc1 ?supply-loc1 ?plane)))    ;;--------------4
   
   ;;;;---------------------------1------------------------------------------------
   (:method (load-fuel ?supply-loc ?quan-deliver ?plane)
     ((attribute ?plane ?capacity ?fuel-capacity ?speed ?burn-rate)(fuel-level ?plane ?fuel-level)
      (call > ?fuel-capacity ?fuel-level))
     (:unordered
       (!load ?supply-loc ?quan-deliver ?plane ?start-load ?end-load)
       (!fuel ?supply-loc ?plane ?start-fuel ?end-fuel))
     ((attribute ?plane ?capacity ?fuel-capacity ?speed ?burn-rate)
      (fuel-level ?plane ?fuel-level)
      (call = ?fuel-capacity ?fuel-level))
     ((!load ?supply-loc ?quan-deliver ?plane ?start-load ?end-load)))
   
   (:operator (!load ?supply-loc ?quan-deliver ?plane ?start ?end)
     ;;;; we assume that the load rate is 200/h, need 2 task-team
     ((assign ?duration (call * 60 (call / ?quan-deliver 200)))
      (is ?team1 task-team)(at ?team1 ?supply-loc)
      (read-time ?team1 ?t1)(write-time ?team1 ?t2)
      (is ?team2 task-team)(at ?team2 ?supply-loc)
      (read-time ?team2 ?t3)(write-time ?team2 ?t4)
      (plane-loc ?plane ?supply-loc)
      (read-time plane-loc ?plane ?t5)
      (write-time plane-loc ?plane ?t6)
      (assign ?start (call max ?t1 ?t2 ?t3 ?t4 ?t6))
      (assign ?end (call + ?start ?duration))
      (assign ?new-value1 (call max ?t1 ?end))
      (assign ?new-value2 (call max ?t3 ?end))
      (assign ?new-value3 (call max ?t5 ?end)))
     ((read-time ?team1 ?t1)(write-time ?team1 ?t2)
      (read-time ?team2 ?t3)(write-time ?team2 ?t4)
      (read-time plane-loc ?plane ?t5))
     ((write-time ?team1 ?end)(write-time ?team2 ?end)
      (read-time ?team1 ?new-value1)(read-time ?team2 ?new-value2)
      (read-time plane-loc ?plane ?new-value3)))
   
   (:operator (!fuel ?supply-loc ?plane ?start ?end)
     ;;we assume that the fuel rate is 2400L/h,need 1 task-team
     ((attribute ?plane ?capacity ?fuel-capacity ?speed ?burn-rate)
      (fuel-level ?plane ?fuel-level)
      (assign ?duration (call * 60 (call / (call - ?fuel-capacity ?fuel-level) 2400)))
      (is ?team task-team)(at ?team ?supply-loc)
      (read-time ?team ?t1)(write-time ?team ?t2)
      (read-time fuel-level ?plane ?t3)
      (write-time fuel-level ?plane ?t4)
      (plane-loc ?plane ?supply-loc)
      (read-time plane-loc ?plane ?t5)
      (write-time plane-loc ?plane ?t6)
      (assign ?start (call max ?t1 ?t2 ?t3 ?t4 ?t6))
      (assign ?end (call + ?start ?duration))
      (assign ?new-value1 (call max ?t1 ?end))
      (assign ?new-value2 (call max ?t3 ?end))
      (assign ?new-value3 (call max ?t5 ?end)))
     ((fuel-level ?plane ?fuel-level)
      (read-time fuel-level ?plane ?t3)(write-time fuel-level ?plane ?t4)
      (read-time ?team ?t1)(write-time ?team ?t2)
      (read-time plane-loc ?plane ?t5))
     ((fuel-level ?plane ?fuel-capacity)
      (read-time fuel-level ?plane ?new-value2)(write-time fuel-level ?plane ?end)
      (read-time ?team ?new-value1)(write-time ?team ?end)
      (read-time plane-loc ?plane ?new-value3))) 
 ;;;-----------------------------2----------------------------------------------
   (:method (fly-unload ?supply-loc ?demand-loc ?quan-deliver ?plane)
     ((plane-loc ?plane ?supply-loc)
      (read-time plane-loc ?plane ?t1)
      (write-time plane-loc ?plane ?t2)
      (assign ?start (call max ?t1 ?t2))
      (distance ?supply-loc ?demand-loc ?distance)
      (attribute ?plane ?capacity ?fuel-capacity ?speed ?burn-rate)
      (assign ?duration (call * 60 (call / ?distance ?speed)))
      (assign ?end (call + ?start ?duration))
      (plane-num-limit ?demand-loc ?num1)(plane-num ?demand-loc ?num2)
      (call < ?num2 ?num1)
      (write-time plane-num ?demand-loc ?t3)(call >= ?end ?t3)
      )
     (:ordered
       (!fly1 ?supply-loc ?demand-loc ?plane ?start ?end)
       (!unload ?demand-loc ?quan-deliver ?plane ?start-unload ?end-unload))
     
     ((plane-loc ?plane ?supply-loc)
      (read-time plane-loc ?plane ?t1)
      (write-time plane-loc ?plane ?t2)
      (assign ?start (call max ?t1 ?t2))
      (distance ?supply-loc ?demand-loc ?distance)
      (attribute ?plane ?capacity ?fuel-capacity ?speed ?burn-rate)
      (assign ?duration (call * 60 (call / ?distance ?speed)))
      (assign ?end (call + ?start ?duration))
      (plane-num-limit ?demand-loc ?num1)(plane-num ?demand-loc ?num2)
      (call >= ?num2 ?num1)
      (write-time plane-num ?demand-loc ?t3)(call >= ?end ?t3))
     (:ordered
       (!fly2 ?supply-loc ?demand-loc ?plane ?start ?end)
       (alternative-land ?demand-loc ?plane)
       ;(!air-drop ?demand-loc ?quan-deliver ?plane ?start-unload ?end-unload)
       ))
   
   (:operator (!fly1 ?supply-loc ?demand-loc ?plane ?start ?end)
     ((distance ?supply-loc ?demand-loc ?distance)
      (attribute ?plane ?capacity ?fuel-capacity ?speed ?burn-rate)
      (assign ?fuel-consum (call * ?burn-rate ?distance))
      ;(assign ?duration (call * 60 (call / ?distance ?speed)))
      (plane-loc ?plane ?supply-loc)
      (read-time plane-loc ?plane ?t1)(write-time plane-loc ?plane ?t2)
      (fuel-level ?plane ?fuel-level)
      (read-time fuel-level ?plane ?t3)(write-time fuel-level ?plane ?t4)
      (plane-num ?demand-loc ?num1)
      (read-time plane-num ?demand-loc ?t5)(write-time plane-num ?demand-loc ?t6)
      (assign ?new-value1 (call max ?end ?t1))
      (assign ?new-value2 (call max ?end ?t3))
      (assign ?new-value3 (call max ?end ?t5))
      ;(assign ?new-value4 (call max ?end ?t6))
      )
     ((plane-loc ?plane ?supply-loc)
      (read-time plane-loc ?plane ?t1)(write-time plane-loc ?plane ?t2)
      (fuel-level ?plane ?fuel-level)
      (read-time fuel-level ?plane ?t3)(write-time fuel-level ?plane ?t4)
      (plane-num ?demand-loc ?num1)
      (read-time plane-num ?demand-loc ?t5)(write-time plane-num ?demand-loc ?t6))
     ((plane-loc ?plane ?demand-loc)
      (read-time plane-loc ?plane ?new-value1)(write-time plane-loc ?plane ?end)
      (fuel-level ?plane (call - ?fuel-level ?fuel-consum))
      (read-time fuel-level ?plane ?new-value2)
      (write-time fuel-level ?plane ?end)
      (plane-num ?demand-loc (call + ?num1 1))
      (write-time plane-num ?demand-loc ?end)
      (read-time plane-num ?demand-loc ?new-value3)))
   
   (:operator (!fly2 ?supply-loc ?demand-loc ?plane ?start ?end)
     ((distance ?supply-loc ?demand-loc ?distance)
      (attribute ?plane ?capacity ?fuel-capacity ?speed ?burn-rate)
      (assign ?fuel-consum (call * ?burn-rate ?distance))
      ;(assign ?duration (call * 60 (call / ?distance ?speed)))
      (plane-loc ?plane ?supply-loc)
      (read-time plane-loc ?plane ?t1)(write-time plane-loc ?plane ?t2)
      (fuel-level ?plane ?fuel-level)
      (read-time fuel-level ?plane ?t3)(write-time fuel-level ?plane ?t4)
      (plane-num ?demand-loc ?num1)
      (read-time plane-num ?demand-loc ?t5)(write-time plane-num ?demand-loc ?t6)
      (assign ?new-value1 (call max ?end ?t1))
      (assign ?new-value2 (call max ?end ?t3))
      (assign ?new-value3 (call max ?end ?t5)))
     ((plane-loc ?plane ?supply-loc)
      (read-time plane-loc ?plane ?t1)(write-time plane-loc ?plane ?t2)
      (fuel-level ?plane ?fuel-level)
      (read-time fuel-level ?plane ?t3)(write-time fuel-level ?plane ?t4)
      (read-time plane-num ?demand-loc ?t5)
      (write-time plane-num ?demand-loc ?t6))
     ((plane-loc ?plane ?demand-loc)
      (read-time plane-loc ?plane ?new-value1)(write-time plane-loc ?plane ?end)
      (fuel-level ?plane (call - ?fuel-level ?fuel-consum))
      (read-time fuel-level ?plane ?new-value2)
      (write-time fuel-level ?plane ?end)
      (write-time plane-num ?demand-loc ?end)
      (read-time plane-num ?demand-loc ?new-value3)
      ))
   
   (:operator (!unload ?demand-loc ?quan-deliver ?plane ?start ?end)
     ;;we assume that the unload rate is 200/h
     ((assign ?duration (call * 60 (call / ?quan-deliver 200)))
      (plane-loc ?plane ?demand-loc)
      (read-time plane-loc ?plane ?t1)(write-time plane-loc ?plane ?t2)
      (is ?team1 task-team)(at ?team1 ?demand-loc)
      (read-time ?team1 ?t3)(write-time ?team1 ?t4)
      (is ?team2 task-team)(at ?team2 ?demand-loc)
      (read-time ?team2 ?t5)(write-time ?team2 ?t6)
      (assign ?start (call max ?t2 ?t3 ?t4 ?t5 ?t6))
      (assign ?end (call + ?duration ?start))
      (deadline ?demand-loc ?deadline)(call <= ?end ?deadline)
      (assign ?new-value1 (call max ?end ?t1))
      (assign ?new-value2 (call max ?end ?t3))
      (assign ?new-value3 (call max ?end ?t5)))
     ((read-time plane-loc ?plane ?t1)
      (read-time ?team1 ?t3)(write-time ?team1 ?t4)
      (read-time ?team2 ?t5)(write-time ?team2 ?t6))
     ((unloaded ?plane ?end)(read-time plane-loc ?plane ?new-value1)
      (read-time ?team1 ?new-value2)(write-time ?team1 ?end)
      (read-time ?team2 ?new-value3)(write-time ?team2 ?end)))
;;;---------------------------------------------------------  
   ;;we assum that the time needed from demand-loc to its alternate-loc is 40min
   ;;the time need unload in the alternate-loc is 60min
   ;;the fuel needed is 180L
   ;;the transport time from alternate-loc to demand-loc is 60min
   ;;the unload time from trucks in demand-loc is 120min
   ;;we neglect the "transport action here"   
   ;;we add 180 to the end of fly3 to determine the potential makespan if needed   
   (:method (alternative-land ?demand-loc ?plane)
     ((plane-loc ?plane ?demand-loc)(alternate ?demand-loc ?alternate-loc))
     (:ordered
       (!fly3 ?demand-loc ?alternate-loc ?plane ?start-fly ?end-fly)
       ;(atom-transport ?start-trans ?end-trans ?alternate-loc ?demand-loc ?plane)
       ))
   (:operator (!fly3 ?demand-loc ?alternate-loc ?plane ?start-fly ?end-fly)
     ((plane-loc ?plane ?demand-loc)
      (read-time plane-loc ?plane ?t1)
      (write-time plane-loc ?plane ?t2)
      (fuel-level ?plane ?fuel-level)
      (read-time fuel-level ?plane ?t3)
      (write-time fuel-level ?plane ?t4)
      (assign ?start-fly (call max ?t1 ?t2 ?t3 ?t4))
      (assign ?end-fly (call + ?start-fly 100))
      (deadline ?demand-loc ?deadline)
      (call <= (call + ?end-fly 180) ?deadline)
      (assign ?new-value1 (call max ?t1 ?end-fly))
      (assign ?new-value2 (call max ?t3 ?end-fly)))
     ((plane-loc ?plane ?demand-loc)
      (read-time plane-loc ?plane ?t1)
      (write-time plane-loc ?plane ?t2)
      (fuel-level ?plane ?fuel-level)
      (read-time fuel-level ?plane ?t3)
      (write-time fuel-level ?plane ?t4))
     ((unloaded ?plane ?end-fly)
      (plane-loc ?plane ?alternate-loc)
      (read-time plane-loc ?plane ?new-value1)
      (write-time plane-loc ?plane ?end-fly)
      (fuel-level ?plane (call - ?fuel-level 180))
      (read-time fuel-level ?plane ?new-value2)
      (write-time fuel-level ?plane ?end-fly)))

   ;;;----------------------3--------------------------------------------
   (:method (return-s ?plane-loc ?supply-loc ?plane)
     ((supply ?supply-loc ?quan)(call > ?quan 0)
      (plane-loc ?plane ?plane-loc)(m-at ?plane-loc ?quan1))
     ((!return1 ?plane-loc ?supply-loc ?plane ?start-return ?end-return))
     ((supply ?supply-loc ?quan)(call > ?quan 0)
      (plane-loc ?plane ?plane-loc)(alternate ?demand-loc ?plane-loc))
     ((!return2 ?plane-loc ?supply-loc ?plane ?start-return ?end-return))
     ((forall (?supply-loc)(supply ?supply-loc ?quan) (call <= ?quan 0))
      (plane-loc ?plane ?plane-loc)(m-at ?plane-loc ?quan1)(supply ?supply-loc ?quan))
     ((!return1 ?plane-loc ?supply-loc ?plane ?start-return ?end-return))
     ((forall (?supply-loc)(supply ?supply-loc ?quan) (call <= ?quan 0))
      (plane-loc ?plane ?plane-loc)(alternate ?demand-loc ?plane-loc)
      (supply ?supply-loc ?quan))
     ((!return2 ?plane-loc ?supply-loc ?plane ?start-return ?end-return)))
   
   
   
   (:operator (!return1 ?demand-loc ?supply-loc ?plane ?start ?end)
     ((unloaded ?plane ?t)(distance ?demand-loc ?supply-loc ?distance)
      (attribute ?plane ?capacity ?fuel-capacity ?speed ?burn-rate)
      (assign ?duration (call * 60 (call / ?distance ?speed)))
      (assign ?fuel-consum (call * ?burn-rate ?distance))
      (plane-loc ?plane ?demand-loc)
      (read-time plane-loc ?plane ?t1)(write-time plane-loc ?plane ?t2)
      (fuel-level ?plane ?fuel-level)
      (read-time fuel-level ?plane ?t3)(write-time fuel-level ?plane ?t4)
      (plane-num ?demand-loc ?num1)
      (read-time plane-num ?demand-loc ?t5)(write-time plane-num ?demand-loc ?t6)
      (assign ?start (call max ?t ?t1 ?t2 ?t3 ?t4 ?t5 ?t6))
      (assign ?end (call + ?start ?duration))
      (assign ?new-value1 (call max ?end ?t1))
      (assign ?new-value2 (call max ?end ?t3))
      (assign ?new-value3 (call max ?end ?t5))
      (assign ?new-value4 (call max ?end ?t6)))
     ((unloaded ?plane ?t)
      (plane-loc ?plane ?demand-loc)
      (read-time plane-loc ?plane ?t1)(write-time plane-loc ?plane ?t2)
      (fuel-level ?plane ?fuel-level)
      (read-time fuel-level ?plane ?t3)(write-time ?fuel-level ?plane ?t4)
      (plane-num ?demand-loc ?num1)
      (read-time plane-num ?demand-loc ?t5)(write-time plane-num ?demand-loc ?t6))
     ((available-plane ?plane)
      (plane-loc ?plane ?supply-loc)
      (read-time plane-loc ?plane ?new-value1)(write-time plane-loc ?plane ?end)
      (fuel-level ?plane (call - ?fuel-level ?fuel-consum))
      (read-time fuel-level ?plane ?new-value2)(write-time ?fuel-level ?plane ?end)
      (plane-num ?demand-loc (call - ?num1 1))
      (read-time plane-num ?demand-loc ?new-value3)
      (write-time plane-num ?demand-loc ?new-value4)))
   
   (:operator (!return2 ?demand-loc ?supply-loc ?plane ?start ?end)
     ((unloaded ?plane ?t)(distance ?demand-loc ?supply-loc ?distance)
      (attribute ?plane ?capacity ?fuel-capacity ?speed ?burn-rate)
      (assign ?duration (call * 60 (call / ?distance ?speed)))
      (assign ?fuel-consum (call * ?burn-rate ?distance))
      (plane-loc ?plane ?demand-loc)
      (read-time plane-loc ?plane ?t1)(write-time plane-loc ?plane ?t2)
      (fuel-level ?plane ?fuel-level)
      (read-time fuel-level ?plane ?t3)(write-time fuel-level ?plane ?t4)
      (assign ?start (call max ?t ?t1 ?t2 ?t3 ?t4))
      (assign ?end (call + ?start ?duration))
      (assign ?new-value1 (call max ?end ?t1))
      (assign ?new-value2 (call max ?end ?t3)))
     ((plane-loc ?plane ?demand-loc)
      (read-time plane-loc ?plane ?t1)(write-time plane-loc ?plane ?t2)
      (fuel-level ?plane ?fuel-level)
      (read-time fuel-level ?plane ?t3)(write-time ?fuel-level ?plane ?t4))
     ((available-plane ?plane)
      (plane-loc ?plane ?supply-loc)
      (read-time plane-loc ?plane ?new-value1)(write-time plane-loc ?plane ?end)
      (fuel-level ?plane (call - ?fuel-level ?fuel-consum))
      (read-time fuel-level ?plane ?new-value2)
      (write-time ?fuel-level ?plane ?end)))      

   ;;;axiom  
   (:- (same ?x ?x) nil) 
  )
)
 
;;;problem
(defproblem problem test
  ;;state
  (
   ;;supply location information-----------------------------------------------
   (supply A1 1000)
   (supply A2 2000)
   
   ;;demand location information-----------------------------------------------
   (m-at B1 0)(alternate B1 B1-1)
   (deadline B1 3600);;deadline is given here rather than in goals
   (plane-num-limit B1 2) ;;number of planes can be in location B1
   (plane-num B1 1)(write-time plane-num B1 0)(read-time plane-num B1 0)
   
   
   ;;distance information
   (distance A1 B1 200)(distance B1 A1 200)
   (distance A1 B1-1 250)(distance B1-1 A1 250)
   (distance A2 B1 300)(distance B1 A2 300)
   (distance A2 B1-1 350)(distance B1-1 A2 350)
   
   ;;plane information, in this domain, plane is the only transport tool-------
   (plane plane1)(available-plane plane1)(attribute plane1 400 2000 200 2)
   (fuel-level plane1 1000)(write-time fuel-level plane1 0)(read-time fuel-level plane1 0)
   (plane-loc plane1 A1)(write-time plane-loc plane1 0)(read-time plane-loc plane1 0)

   
   (plane plane2)(available-plane plane2)(attribute plane2 200 1000 200 2)
   (fuel-level plane2 600)(write-time fuel-level plane2 0)(read-time fuel-level plane2 0)
   (plane-loc plane2 A1)(write-time plane-loc plane2 0)(read-time plane-loc plane2 0)
   
   (plane plane3)(available-plane plane3)(attribute plane1 400 2000 200 2)
   (fuel-level plane3 1000)(write-time fuel-level plane3 0)(read-time fuel-level plane3 0)
   (plane-loc plane3 A2)(write-time plane-loc plane3 0)(read-time plane-loc plane3 0)
   
   ;;task teams in A1-------------------------------------------------------------------
   (is team1-A1 task-team)(at team1-A1 A1)(read-time team1-A1 0)(write-time team1-A1 0)
   (is team2-A1 task-team)(at team2-A1 A1)(read-time team2-A1 0)(write-time team2-A1 0)
   (is team3-A1 task-team)(at team3-A1 A1)(read-time team3-A1 0)(write-time team3-A1 0)
   (is team4-A1 task-team)(at team4-A1 A1)(read-time team4-A1 0)(write-time team4-A1 0)
   ;;task teams in A2------------------------------------------------------------------
   (is team1-A2 task-team)(at team1-A2 A2)(read-time team1-A2 0)(write-time team1-A2 0)
   (is team2-A2 task-team)(at team2-A2 A2)(read-time team2-A2 0)(write-time team2-A2 0)
   ;(is team3-A1 task-team)(at team3-A1 A1)(read-time team3-A1 0)(write-time team3-A1 0)
   ;(is team4-A1 task-team)(at team4-A1 A1)(read-time team4-A1 0)(write-time team4-A1 0)
   
   ;;task teams in B1-------------------------------------------------------------------
   (is team1-B1 task-team)(at team1-B1 B1)(read-time team1-B1 0)(write-time team1-B1 0)
   (is team2-B1 task-team)(at team2-B1 B1)(read-time team2-B1 0)(write-time team2-B1 0)
   (is team3-B1 task-team)(at team3-B1 B1)(read-time team3-B1 0)(write-time team3-B1 0)
   (is team4-B1 task-team)(at team4-B1 B1)(read-time team4-B1 0)(write-time team4-B1 0)
   

   )
  ;;tasks
  (:unordered   
    (material-at B1 2000)
  )
)

(find-plans 'problem :which :first :verbose :plans)