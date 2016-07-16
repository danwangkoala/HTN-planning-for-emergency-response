;;;; Created on 2014-07-14 11:01:54

(defdomain test
  (
   ;(material-at 0 3600 B1 400)
   (:method (material-at ?demand-loc ?quan)
     ((not (resource-initialized)))
     (:ordered
       (Resource-ini)
       (real-m-deliver ?demand-loc ?quan))
     ((resource-initialized))
     ((real-m-deliver ?demand-loc ?quan))) 
   ;;----------------initialize resource state and state update---------------------
   (:method (Resource-ini)
     ()
     ;;....present all resources in the initial state
     (:ordered
       (initialize team-A1)
       (initialize team-A2)
       (initialize team-B1)
       
       (!!add-r-initialize-state)))
   (:method (initialize ?ResourceID)
     ;;preocnditon
     ((initialized ?ResourceID))
     ()
     (and (not (initialized ?ResourceID))(resource ?ResourceID ?Rtime)(capacity ?ResourceID ?capacity))
     ((!!real-ini ?ResourceID ?capacity ?Rtime)))
   (:operator (!!real-ini ?ResourceID ?capacity ?Rtime)
     ((call R-initialize '?ResourceID ?capacity ?Rtime))
     ()
     ((initialized ?ResourceID)))
   (:operator (!!add-r-initialize-state)
     ()
     ()
     ((resource-initialized)))
   (:operator (!!R-Supdate ?ResourceID ?num ?Tstart ?duration)
     ((call R-SUPDATE '?ResourceID ?num ?Tstart ?duration))
     ()
     ((R-SUPDATE ?ResourceID ?num ?Tstart ?duration)))

;;;;------------------real material deliver------------------------------------------
   (:method (real-m-deliver ?demand-loc ?quan)
     ((m-at ?demand-loc ?quan1)(call < ?quan1 ?quan)(supply ?supply-loc ?quan2)(call > ?quan2 0)
      (available-plane ?plane)(attribute ?plane ?capacity ?fuel-capacity ?speed ?burn-rate)
      (plane-loc ?plane ?supply-loc)(assign ?quan-deliver (call min (call - ?quan ?quan1) ?quan2 ?capacity)))
     (:unordered
       (:immediate !!update-quan ?supply-loc ?demand-loc ?quan-deliver ?plane)
       (deliver-m ?supply-loc ?demand-loc ?quan-deliver ?plane)
       (real-m-deliver ?demand-loc ?quan))
     ((m-at ?demand-loc ?quan1)(call >= ?quan1 ?quan))
     ())
   
   (:operator (!!update-quan ?supply-loc ?demand-loc ?quan-deliver ?plane)
     ((supply ?supply-loc ?quan1) (m-at ?demand-loc ?quan2)(available-plane ?plane))
     ((supply ?supply-loc ?quan1) (m-at ?demand-loc ?quan2)(available-plane ?plane))
     ((supply ?supply-loc (call - ?quan1 ?quan-deliver)) (m-at ?demand-loc (call + ?quan2 ?quan-deliver))))
   
   (:method (deliver-m ?supply-loc ?demand-loc ?quan-deliver ?plane)
     ()
     (:ordered
       (load-fuel ?supply-loc ?quan-deliver ?plane)               ;;------------1
       (fly-unload ?supply-loc ?demand-loc ?quan-deliver ?plane)  ;;------------2
       (return-s ?demand-loc1 ?supply-loc1 ?plane)))              ;;------------3
   
   (:method (load-fuel ?supply-loc ?quan-deliver ?plane)
     ((attribute ?plane ?capacity ?fuel-capacity ?speed ?burn-rate)
      (fuel-level ?plane ?fuel-level)(call > ?fuel-capacity ?fuel-level))
     (:unordered
       (atomload ?start-load ?end-load ?supply-loc ?quan-deliver ?plane)
       (atomfuel ?start-fuel ?end-fuel ?supply-loc ?plane))
     ((attribute ?plane ?capacity ?fuel-capacity ?speed ?burn-rate)
      (fuel-level ?plane ?fuel-level)(call = ?fuel-capacity ?fuel-level))
     ((atomload ?start-load ?end-load ?supply-loc ?quan-deliver ?plane)))
   
   (:method (atomload ?start-load ?end-load ?supply-loc ?quan-deliver ?plane)
     ;; we assume that the load rate is 200/h, need 2 task-team
     (;(not (loaded ?plane ?t))
      (assign ?duration (call * 60 (call / ?quan-deliver 200)))
      (plane-loc ?plane ?supply-loc)
      (write-time plane-loc ?plane ?t1)
      (is ?team task-team)
      (at ?team ?supply-loc)
      (assign ?t2 (call R-available '?team 2))
      (assign ?start-load (call max ?t2 ?t1))
      (assign ?end-load (call + ?start-load ?duration)))
     (:ordered
       (!load-m ?start-load ?end-load ?supply-loc ?quan-deliver ?plane)
       (:immediate !!R-Supdate ?team 2 ?start-load ?duration)))
   (:operator (!load-m ?start-load ?end-load ?supply-loc ?quan-deliver ?plane)
     ((read-time plane-loc ?plane ?t1)
      (assign ?new-value1 (call max ?t1 ?end-load)))
     ((read-time plane-loc ?plane ?t1))
     (;(loaded ?plane ?end-load) 
      (read-time plane-loc ?plane ?new-value1)))
   
   (:method (atomfuel ?start-fuel ?end-fuel ?supply-loc ?plane)
     ((attribute ?plane ?capacity ?fuel-capacity ?speed ?burn-rate)
      (fuel-level ?plane ?fuel-level)
      (assign ?duration (call * 60 (call / (call - ?fuel-capacity ?fuel-level) 2400)))
      (plane-loc ?plane ?supply-loc)
      (write-time plane-loc ?plane ?t1)
      (read-time fuel-level ?plane ?t2)
      (write-time fuel-level ?plane ?t3)
      (is ?team task-team)
      (at ?team ?supply-loc)
      (assign ?t4 (call R-available '?team 1))
      (assign ?start-fuel (call max ?t1 ?t2 ?t3 ?t4))
      (assign ?end-fuel (call + ?start-fuel ?duration)))
     (:ordered
       (!fuel ?start-fuel ?end-fuel ?supply-loc ?plane)
       (:immediate !!R-Supdate ?team 1 ?start-fuel ?duration)))
   (:operator (!fuel ?start-fuel ?end-fuel ?supply-loc ?plane)
     ((attribute ?plane ?capacity ?fuel-capacity ?speed ?burn-rate)
      (fuel-level ?plane ?fuel-level)
      (read-time plane-loc ?plane ?t1)
      (read-time fuel-level ?plane ?t3)
      (write-time fuel-level ?plane ?t4)
      (assign ?new-value1 (call max ?t1 ?end-fuel))
      (assign ?new-value2 (call max ?t3 ?end-fuel)))
     ((fuel-level ?plane ?fuel-level)
      (read-time fuel-level ?plane ?t3)
      (write-time fuel-level ?plane ?t4)
      (read-time plane-loc ?plane ?t1))
     (;(fueled ?plane ?end-fuel)
      (fuel-level ?plane ?fuel-capacity)
      (read-time fuel-level ?plane ?new-value2)
      (write-time fuel-level ?plane ?end-fuel)
      (read-time plane-loc ?plane ?new-value1)))
   
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
      (write-time plane-num ?demand-loc ?t3)(call >= ?end ?t3))
     (:ordered
       (atom-fly1 ?start-fly ?end-fly ?supply-loc ?demand-loc ?plane)
       (atom-unload ?start-unload ?end-unload ?demand-loc ?quan-deliver ?plane))

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
       (atom-fly2 ?start-fly ?end-fly ?supply-loc ?demand-loc ?plane)
       (alternative-land ?demand-loc ?plane)
       ;(atom-airdrop ?start-drop ?end-drop ?demand-loc ?quan-deliver ?plane)
       ))
   
   (:method (atom-fly1 ?start-fly ?end-fly ?supply-loc ?demand-loc ?plane)
     (;(loaded ?plane ?t1)(fueled ?plane ?t2)
      (plane-loc ?plane ?supply-loc)
      (read-time plane-loc ?plane ?t3)
      (write-time plane-loc ?plane ?t4)
      (assign ?start-fly (call max ?t3 ?t4))
      (distance ?supply-loc ?demand-loc ?distance)
      (attribute ?plane ?capacity ?fuel-capacity ?speed ?burn-rate)
      (assign ?duration (call * 60 (call / ?distance ?speed)))
      (assign ?end-fly (call + ?start-fly ?duration)))
     ((!fly1 ?start-fly ?end-fly ?supply-loc ?demand-loc ?plane)))
   
   (:method (atom-fly2 ?start-fly ?end-fly ?supply-loc ?demand-loc ?plane)
     (;(loaded ?plane ?t1)(fueled ?plane ?t2)
      (plane-loc ?plane ?supply-loc)
      (read-time plane-loc ?plane ?t3)
      (write-time plane-loc ?plane ?t4)
      (assign ?start-fly (call max ?t3 ?t4))
      (distance ?supply-loc ?demand-loc ?distance)
      (attribute ?plane ?capacity ?fuel-capacity ?speed ?burn-rate)
      (assign ?duration (call * 60 (call / ?distance ?speed)))
      (assign ?end-fly (call + ?start-fly ?duration)))
     ((!fly2 ?start-fly ?end-fly ?supply-loc ?demand-loc ?plane)))

   (:operator (!fly1 ?start-fly ?end-fly ?supply-loc ?demand-loc ?plane)
     ((distance ?supply-loc ?demand-loc ?distance)
      (attribute ?plane ?capacity ?fuel-capacity ?speed ?burn-rate)
      (assign ?fuel-consum (call * ?burn-rate ?distance))
      (plane-loc ?plane ?supply-loc)(read-time plane-loc ?plane ?t1)
      (write-time plane-loc ?plane ?t2)     
      (fuel-level ?plane ?fuel-level)(read-time fuel-level ?plane ?t3)
      (write-time fuel-level ?plane ?t4)      
      (plane-num ?demand-loc ?num1)(read-time plane-num ?demand-loc ?t5)
      (write-time plane-num ?demand-loc ?t6) 
      (assign ?new-value1 (call max ?end-fly ?t1))
      (assign ?new-value2 (call max ?end-fly ?t3))
      (assign ?new-value3 (call max ?end-fly ?t5)))
     ((plane-loc ?plane ?supply-loc)(read-time plane-loc ?plane ?t1)
      (write-time plane-loc ?plane ?t2)
      (fuel-level ?plane ?fuel-level)(read-time fuel-level ?plane ?t3)
      (write-time fuel-level ?plane ?t4)
      (plane-num ?demand-loc ?num1)(read-time plane-num ?demand-loc ?t5)
      (write-time plane-num ?demand-loc ?t6))
     ((plane-loc ?plane ?demand-loc)(read-time plane-loc ?plane ?new-value1)
      (write-time plane-loc ?plane ?end-fly)
      (fuel-level ?plane (call - ?fuel-level ?fuel-consum))
      (read-time fuel-level ?plane ?new-value2)
      (write-time fuel-level ?plane ?end-fly)
      (plane-num ?demand-loc (call + ?num1 1))
      (write-time plane-num ?demand-loc ?end-fly)
      (read-time plane-num ?demand-loc ?new-value3)))
   
   (:operator (!fly2 ?start-fly ?end-fly ?supply-loc ?demand-loc ?plane)
     ((distance ?supply-loc ?demand-loc ?distance)
      (attribute ?plane ?capacity ?fuel-capacity ?speed ?burn-rate)
      (assign ?fuel-consum (call * ?burn-rate ?distance))
      (plane-loc ?plane ?supply-loc)(read-time plane-loc ?plane ?t1)
      (write-time plane-loc ?plane ?t2)
      (fuel-level ?plane ?fuel-level)(read-time fuel-level ?plane ?t3)
      (write-time fuel-level ?plane ?t4)
      (read-time plane-num ?demand-loc ?t5)
      (write-time plane-num ?demand-loc ?t6) 
      (assign ?new-value1 (call max ?end-fly ?t1))
      (assign ?new-value2 (call max ?end-fly ?t3))
      (assign ?new-value3 (call max ?end-fly ?t5)))
     ((plane-loc ?plane ?supply-loc)(read-time plane-loc ?plane ?t1)
      (write-time plane-loc ?plane ?t2)
      (fuel-level ?plane ?fuel-level)(read-time fuel-level ?plane ?t3)
      (write-time fuel-level ?plane ?t4)
      (read-time plane-num ?demand-loc ?t5)(write-time plane-num ?demand-loc ?t6))
     ((plane-loc ?plane ?demand-loc)(read-time plane-loc ?plane ?new-value1)
      (write-time plane-loc ?plane ?end-fly)
      (fuel-level ?plane (call - ?fuel-level ?fuel-consum))
      (read-time fuel-level ?plane ?new-value2)
      (write-time fuel-level ?plane ?end-fly)
      (write-time plane-num ?demand-loc ?end-fly)
      (read-time plane-num ?demand-loc ?new-value3)))
   
   (:method (atom-unload ?start-unload ?end-unload ?demand-loc ?quan-deliver ?plane)
     ((assign ?duration (call * 60 (call / ?quan-deliver 200)))
      (plane-loc ?plane ?demand-loc)
      ;(read-time plane-loc ?plane ?t1)
      (write-time plane-loc ?plane ?t2)
      (is ?team task-team)(at ?team ?demand-loc)
      (assign ?t3 (call R-available '?team 2))
      (assign ?start-unload (call max ?t2 ?t3))
      (assign ?end-unload (call + ?start-unload ?duration))
      (deadline ?demand-loc ?deadline)(call <= ?end-unload ?deadline))
     (:ordered
       (!unload-m ?start-unload ?end-unload ?demand-loc ?quan-deliver ?plane)
       (:immediate !!R-Supdate ?team 2 ?start-unload ?duration)))
   (:operator (!unload-m ?start-unload ?end-unload ?supply-loc ?quan-deliver ?plane)
     ((read-time plane-loc ?plane ?t1)
      (assign ?new-value (call max ?t1 ?end-unload)))
     ((read-time plane-loc ?plane ?t1))
     ((unloaded ?plane ?end-unload)(read-time plane-loc ?plane ?new-value)))
;;;---------------------------------------------------------  
   ;;we assum that the time needed from demand-loc to its alternate-loc is 40min
   ;;the time need unload in the alternate-loc is 60min
   ;;the fuel needed is 180L
   ;;the transport time from alternate-loc to demand-loc is 60min
   ;;the time needed to unload materials from trucks is 120min
   ;;we neglect the "transport action here"   
   ;;we add 180 to the end of fly3 to determine the potential makespan if needed
   (:method (alternative-land ?demand-loc ?plane)
     ((plane-loc ?plane ?demand-loc)(alternate ?demand-loc ?alternate-loc))
     (:ordered
       (atom-fly3 ?start-fly ?end-fly ?demand-loc ?alternate-loc ?plane)
       ;(atom-transport ?start-trans ?end-trans ?alternate-loc ?demand-loc ?plane)
       ))
   
   (:method (atom-fly3 ?start-fly ?end-fly ?demand-loc ?alternate-loc ?plane)
     ((plane-loc ?plane ?demand-loc)
      (read-time plane-loc ?plane ?t1)
      (write-time plane-loc ?plane ?t2)
      (fuel-level ?plane ?fuel-level)
      (read-time fuel-level ?plane ?t3)
      (write-time fuel-level ?plane ?t4)
      (assign ?start-fly (call max ?t1 ?t2 ?t3 ?t4))
      (assign ?end-fly (call + ?start-fly 100))
      (deadline ?demand-loc ?deadline)
      (call <= (call + ?end-fly 180) ?deadline))
     ((!fly3 ?start-fly ?end-fly ?demand-loc ?alternate-loc ?plane)))
   
   (:operator (!fly3 ?start-fly ?end-fly ?demand-loc ?alternate-loc ?plane)
     ((plane-loc ?plane ?demand-loc)
      (read-time plane-loc ?plane ?t1)
      (write-time plane-loc ?plane ?t2)
      (fuel-level ?plane ?fuel-level)
      (read-time fuel-level ?plane ?t3)
      (write-time fuel-level ?plane ?t4)
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
   
   (:method (return-s ?plane-loc ?supply-loc ?plane)
     ((supply ?supply-loc ?quan)(call > ?quan 0)
      (plane-loc ?plane ?plane-loc)(m-at ?plane-loc ?quan1))
     ((atom-return1 ?start-return ?end-return ?plane-loc ?supply-loc ?plane))
     ((supply ?supply-loc ?quan)(call > ?quan 0)
      (plane-loc ?plane ?plane-loc)(alternate ?demand-loc ?plane-loc))
     ((atom-return2 ?start-return ?end-return ?plane-loc ?supply-loc ?plane))
     ((forall (?supply-loc)(supply ?supply-loc ?quan) (call <= ?quan 0))
      (plane-loc ?plane ?plane-loc)(m-at ?plane-loc ?quan1)(supply ?supply-loc ?quan))
     ((atom-return1 ?start-return ?end-return ?plane-loc ?supply-loc ?plane))
     ((forall (?supply-loc)(supply ?supply-loc ?quan) (call <= ?quan 0))
      (plane-loc ?plane ?plane-loc)(alternate ?demand-loc ?plane-loc)
      (supply ?supply-loc ?quan))
     ((atom-return2 ?start-return ?end-return ?plane-loc ?supply-loc ?plane)))
   
   (:method (atom-return1 ?start-return ?end-return ?demand-loc ?supply-loc ?plane)
     ((unloaded ?plane ?t)(distance ?demand-loc ?supply-loc ?distance)
      (attribute ?plane ?capacity ?fuel-capacity ?speed ?burn-rate)
      (assign ?duration (call * 60 (call / ?distance ?speed)))
      (read-time plane-loc ?plane ?t1)(write-time plane-loc ?plane ?t2)
      (read-time fuel-level ?plane ?t3)(write-time fuel-level ?plane ?t4)
      (read-time plane-num ?demand-loc ?t5)(write-time plane-num ?demand-loc ?t6)
      (assign ?start-return (call max ?t ?t1 ?t2 ?t3 ?t4 ?t5 ?t6))
      (assign ?end-return (call + ?start-return ?duration)))
     ((!return1 ?start-return ?end-return ?demand-loc ?supply-loc ?plane)))
   
   (:method (atom-return2 ?start-return ?end-return ?demand-loc ?supply-loc ?plane)
     ((unloaded ?plane ?t)(distance ?demand-loc ?supply-loc ?distance)
      (attribute ?plane ?capacity ?fuel-capacity ?speed ?burn-rate)
      (assign ?duration (call * 60 (call / ?distance ?speed)))
      (read-time plane-loc ?plane ?t1)(write-time plane-loc ?plane ?t2)
      (read-time fuel-level ?plane ?t3)(write-time fuel-level ?plane ?t4)
      (assign ?start-return (call max ?t ?t1 ?t2 ?t3 ?t4))
      (assign ?end-return (call + ?start-return ?duration)))
     ((!return2 ?start-return ?end-return ?demand-loc ?supply-loc ?plane)))
   
   (:operator (!return1 ?start-return ?end-return ?demand-loc ?supply-loc ?plane)
     ((unloaded ?plane ?t)(distance ?demand-loc ?supply-loc ?distance)
      (attribute ?plane ?capacity ?fuel-capacity ?speed ?burn-rate)
      (assign ?fuel-consum (call * ?burn-rate ?distance))
      (plane-loc ?plane ?demand-loc)(read-time plane-loc ?plane ?t1)
      (write-time plane-loc ?plane ?t2)
      (fuel-level ?plane ?fuel-level)(read-time fuel-level ?plane ?t3)
      (write-time fuel-level ?plane ?t4)
      (plane-num ?demand-loc ?num1)(read-time plane-num ?demand-loc ?t5)
      (write-time plane-num ?demand-loc ?t6)
      (assign ?new-value1 (call max ?end-return ?t1))
      (assign ?new-value2 (call max ?end-return ?t3))
      (assign ?new-value3 (call max ?end-return ?t5)))
     ((unloaded ?plane ?t)(plane-loc ?plane ?demand-loc)
      (read-time plane-loc ?plane ?t1)(write-time plane-loc ?plane ?t2)
      (fuel-level ?plane ?fuel-level)(read-time fuel-level ?plane ?t3)
      (write-time fuel-level ?plane ?t4)
      (plane-num ?demand-loc ?num1)(read-time plane-num ?demand-loc ?t5)
      (write-time plane-num ?demand-loc ?t6))
     ((available-plane ?plane)(plane-loc ?plane ?supply-loc)
      (read-time plane-loc ?plane ?new-value1)
      (write-time plane-loc ?plane ?end-return)
      (fuel-level ?plane (call - ?fuel-level ?fuel-consum))
      (read-time fuel-level ?plane ?new-value2)
      (write-time fuel-level ?plane ?end-return)
      (plane-num ?demand-loc (call - ?num1 1))
      (read-time plane-num ?demand-loc ?new-value3)
      (write-time plane-num ?demand-loc ?end-return)))
   
   (:operator (!return2 ?start-return ?end-return ?demand-loc ?supply-loc ?plane)
     ((unloaded ?plane ?t)(distance ?demand-loc ?supply-loc ?distance)
      (attribute ?plane ?capacity ?fuel-capacity ?speed ?burn-rate)
      (assign ?fuel-consum (call * ?burn-rate ?distance))
      (plane-loc ?plane ?demand-loc)(read-time plane-loc ?plane ?t1)
      (write-time plane-loc ?plane ?t2)
      (fuel-level ?plane ?fuel-level)(read-time fuel-level ?plane ?t3)
      (write-time fuel-level ?plane ?t4)
      (assign ?new-value1 (call max ?end-return ?t1))
      (assign ?new-value2 (call max ?end-return ?t3)))
     ((unloaded ?plane ?t)(plane-loc ?plane ?demand-loc)
      (read-time plane-loc ?plane ?t1)(write-time plane-loc ?plane ?t2)
      (fuel-level ?plane ?fuel-level)(read-time fuel-level ?plane ?t3)
      (write-time fuel-level ?plane ?t4))
     ((available-plane ?plane)(plane-loc ?plane ?supply-loc)
      (read-time plane-loc ?plane ?new-value1)
      (write-time plane-loc ?plane ?end-return)
      (fuel-level ?plane (call - ?fuel-level ?fuel-consum))
      (read-time fuel-level ?plane ?new-value2)
      (write-time fuel-level ?plane ?end-return)))
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
   (supply A2 1000)
   
   ;;demand location information-----------------------------------------------
   (m-at B1 0)(alternate B1 B1-1)
   (deadline B1 3600);;deadline is given here rather than in goals
   (plane-num-limit B1 2) ;;number of planes can be in location B1
   (plane-num B1 0)(write-time plane-num B1 0)(read-time plane-num B1 0)
   
   ;;distance information------------------------------------------------------
   (distance A1 B1 200)(distance B1 A1 200)
   (distance A1 B1-1 250)(distance B1-1 A1 250)
   (distance A2 B1 300)(distance B1 A2 300)
   (distance A2 B1-1 350)(distance B1-1 A2 350)
    
   (plane plane1)(available-plane plane1)(attribute plane1 400 2000 200 2)
   (fuel-level plane1 1000)
   (write-time fuel-level plane1 0)(read-time fuel-level plane1 0)
   (plane-loc plane1 A1)(write-time plane-loc plane1 0)(read-time plane-loc plane1 0)

   
   (plane plane2)(available-plane plane2)(attribute plane2 200 1000 200 2)
   (fuel-level plane2 600)
   (write-time fuel-level plane2 0)(read-time fuel-level plane2 0)
   (plane-loc plane2 A1)(write-time plane-loc plane2 0)(read-time plane-loc plane2 0)
   
   (plane plane3)(available-plane plane3)(attribute plane1 400 2000 200 2)
   (fuel-level plane3 1000)
   (write-time fuel-level plane3 0)(read-time fuel-level plane3 0)
   (plane-loc plane3 A2)(write-time plane-loc plane3 0)(read-time plane-loc plane3 0)
   
      
   ;;----------task teams in A1
   (is team-A1 task-team)(at team-A1 A1)(resource team-A1 0)(capacity team-A1 4)
   ;;---------task teams in A2
   (is team-A2 task-team)(at team-A2 A2)(resource team-A2 0)(capacity team-A2 2)
   ;;---------task teams in B1
   (is team-B1 task-team)(at team-B1 B1)(resource team-B1 0)(capacity team-B1 4)
   
   
   

   
   )
  ;;tasks
  (:unordered   
    (material-at B1 2000)
    
    )
)

(find-plans 'problem :which :first :verbose :plans :time-limit 0.4)