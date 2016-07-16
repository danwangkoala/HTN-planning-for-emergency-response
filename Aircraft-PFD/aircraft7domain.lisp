;;;; Created on 2013-10-24 14:51:57
(defdomain aircraft7domain
  (
    ;; The methods and operators for doing the actual planning
    (:method (mission ?number ?type ?deadline)
    	(
    		(mission ?number ?type ?deadline)
    		(not(selected-aircraft ?number))
    	)
    	(:ordered
    		(select-aircraft ?number)
    		(prepare-aircraft1 ?number)
    		(catapult1 ?number)
    		(landing1 ?number)
    	)
    )
    ;;-----------------------------------------------------------
    ;;-----------------------------------------------------------
    (:method (prepare-aircraft1 ?number)
    	(
    		(selected-aircraft ?number)
    		(not(prepared-aircraft ?number))
    	)
    	(
    		(prepare-aircraft ?number)
    	)
    )
    
    (:method (catapult1 ?number)
    	(
    		(prepared-aircraft ?number)
    		(not(catapulted ?number))
    	)
    	(
    		(catapult ?number)
    	)
    )
    (:method (landing1 ?number)
    	(
    		(catapulted ?number)
    		(not(done ?number))
    	)
    	(
    		(landing ?number)
    	)
    )
    ;;-----------------------------------------------------
    (:operator (!done ?number)
    	()
    	()
    	((done ?number))
    )
    ;;;-----------------------(:method (select-aircraft ?number)---------------------------------
    ;;;------------------------------------------------------------------------------------------
    (:method (select-aircraft ?number)
      	;precondition1
        (:sort-by ?t #'<
			(	(mission ?number ?type ?deadline)
				(same ?type 1)
				(call >= ?deadline 70)    ;;deadline���ò�������滮ʧ�ܣ����ǳ���ɸѡ��û�а����ڻ����ڵ����
				(is ?aircraft SUAV)
				(not(busy ?aircraft))
				(write-time ?aircraft ?t)
				(call <= (call + ?t 70) ?deadline)     ;;�����t1ʱ�̿�ʼִ�У��п��ܲ�����deadline�������		
			)
		)
		;subtasks1
		(
				(!select-aircraft ?number ?aircraft ?t)
		)      
       ;precondition2
        (:sort-by ?t #'<
        	(	(mission ?number ?type ?deadline)
        		(same ?type 2)
        		(call >= ?deadline 100)
        		(is ?aircraft FUAV)
        		(not(busy ?aircraft))
				(write-time ?aircraft ?t)
				(call <= (call + ?t 100) ?deadline)
			)
        )
       ;subtasks2
        (       		
        		(!select-aircraft ?number ?aircraft ?t)	
        )
    )
   ;;------------------------------------------------------------------
   (:operator (!select-aircraft ?number ?aircraft ?t)
   		()
   		()
   		(	
   			(busy ?aircraft)
   			(occupy ?number ?aircraft)
   			(selected-aircraft ?number)
   			(time-line ?number ?t)
   		)
   )
   ;;;----------(:method (prepare-aircraft ?number)------------------ 
   ;;;-----------------------------------------------------------------
   (:method (prepare-aircraft ?number)
   		;;�˴����뽢�ػ�λ����Ϣ�󣬿��Լ����ƶ�������վ��������վ��ʱ��
   		;;precondition1
   		(
   			(occupy ?number ?aircraft)
   			(on-deck ?aircraft)
   			(is ?aircraft SUAV)
   			(not(fueled ?aircraft))
   		)
   		;;subtasks1
   		(:ordered
   			(fuel ?number)
   			(!prepared-aircraft ?number)
   		)
   		;;precondition2
   		(
   			(occupy ?number ?aircraft)
   			(below-deck ?aircraft)
   			(is ?aircraft SUAV)
   		)
   		;;subtasks2
   		(:ordered
   			(!move-to-deck ?number)
   			(fuel ?number)
   			(!prepared-aircraft ?number)
   		)
   		;;precondition3
   		(
   			(occupy ?number ?aircraft)
   			(on-deck ?aircraft)
   			(is ?aircraft FUAV)
   			(not(fueled ?aircraft))
   			(not(weaponed ?aircraft))
   		)
   		;;subtasks3
   		(:ordered
			(fuel-weapon ?number)
   			(!prepared-aircraft ?number)
   		)
   		;;precondition4
   		(
   			(occupy ?number ?aircraft)
   			(below-deck ?aircraft)
   			(is ?aircraft FUAV)
   		)
   		;;subtasks4
   		(:ordered
   			(!move-to-deck ?number)
   			(fuel-weapon ?number)
   			(!prepared-aircraft ?number)
   		)
   )
;;---------------------------------------------------------------------
   (:operator (!prepared-aircraft ?number)
   		()
   		()
   		(
   			(prepared-aircraft ?number)
   		)
   )
   
   (:method (fuel-weapon ?number)
   		()
   		(:unordered
   			
   			(weapon ?number)
   			(fuel ?number)
   		)
   )
;;-------------------------------------------------------------------
   (:operator (!move-to-deck ?number)
   		(:sort-by ?t #'<
   			(
   				(occupy ?number ?aircraft)
   				(time-line ?number ?t1)
   				(is ?elevator elevator)
   				(write-time ?elevator ?t2)
   				(read-time ?elevator ?t3)
   				(assign ?t (call max ?t1 ?t2 ?t3))
   			)	
   		)
   		(
   			(below-deck ?aircraft)
   			(time-line ?number ?t1)
   			(write-time ?elevator ?t2)
   			(read-time ?elevator ?t3)
   		)
   		(
   			(on-deck ?aircraft)
   			(time-line ?number (call + ?t 30))
   			(write-time ?elevator (call + ?t 30))
   			(read-time ?elevator (call + ?t 30))
   		)
   )
 	;;------------------(:method (fuel ?number))----------------------------
   (:method (fuel ?number)
   		(:sort-by ?t #'<
   			(
   				;;(time-line ?number ?t1)
   				(is ?fuel-station fuel-station)
   				(write-time ?fuel-station ?t2)
   				(read-time ?fuel-station ?t3)
   				(assign ?t (call max ?t2 ?t3))
   				;;(call >= t (call + ?t1 20))
   			)	
   		)
   		(:ordered
   			(!move-to-fuel-station ?number ?fuel-station)
   			(!fuel ?number ?fuel-station 10)
   		)
   )
   	;;---------------------------------------------------------------
   (:operator (!move-to-fuel-station ?number ?fuel-station)
   		(
   			(time-line ?number ?t)
   		)
   		(
   			(time-line ?number ?t)
   			(at ?number weapon-station)
   		)
   		(
   			(time-line ?number (call + ?t 20))
   			(at ?number ?fuel-station)
   		)
   )
  	 ;;----------------------------------------------------------------
   (:operator (!fuel ?number ?fuel-station ?duration)
   		(
   			(occupy ?number ?aircraft)
   			(at ?number ?fuel-station)
   			(time-line ?number ?t1)
   			(write-time ?fuel-station ?t2)
   			(read-time ?fuel-station ?t3)
   			(assign ?start (call max ?t1 ?t2 ?t3))
   			(assign ?end (call + ?start ?duration))
   		)
   		(
   			(time-line ?number ?t1)
   			(write-time ?fuel-station ?t2)
   			(read-time ?fuel-station ?t3)
   		)
   		(
   			(fueled ?aircraft)
   			(time-line ?number ?end)
   			(write-time ?fuel-station ?end)
   			(read-time ?fuel-station ?end)
   		)		
   )
   ;;-----------------------(:method (weapon ?number))----------------------------------
   (:method (weapon ?number)
   		()
   		(:ordered
   			(!move-to-weapon-station ?number)
   			(!weapon ?number)
   		)
   )
   (:operator (!move-to-weapon-station ?number)
   		(
   			(time-line ?number ?t)
   		)
   		(
   			(time-line ?number ?t)
   			(at ?number ?fuel-station)	
   		)
   		(
   			(time-line ?number (call + ?t 20))
   			(at ?number weapon-station1)
   		)
   )
   (:operator (!weapon ?number)
   		(
   			(occupy ?number ?aircraft)
   			(time-line ?number ?t1)
   			(is ?weapon-station weapon-station)
   			(write-time ?weapon-station ?t2)
   			(read-time ?weapon-station ?t3)
   			(assign ?t (call max ?t1 ?t2 ?t3))	
   		)
   		(
   			(time-line ?number ?t1)
   			(write-time ?weapon-station ?t2)
   			(read-time ?weapon-station ?t3)
   		)
   		(
   			(weaponed ?aircraft)
   			(time-line ?number (call + ?t 10))
   			(write-time ?weapon-station (call + ?t 10))
   			(read-time ?weapon-station (call + ?t 10))
   		)	
   )  
   ;;;------------------(:method (catapult ?number))----------------------
   ;;;--------------------------------------------------------------------
   (:method (catapult ?number)
   		(:sort-by ?t #'<
   			(
   				(mission ?number ?type ?deadline)
   				(time-line ?number ?t1)
   				(is ?catapult catapult)
   				(write-time ?catapult ?t2)
   				(read-time ?catapult ?t3)
   				(assign ?t (call max ?t2 ?t3))
   				;;(call >= t (call + ?t1 20))  ;;�ƶ�������������Ҫ��ʱ��Ϊ20min
   				(call >= ?deadline (call + ?t1 40))  ;;��Ϊ˳�������¿�����deadline֮ǰ���䣬�����˷
   			)
   		)
   		(:ordered
   			(!move-to-catapult ?number ?catapult)
   			(launch ?number ?catapult)
   		)
   )
   ;;----------------------------------------------------------------------
   (:operator (!move-to-catapult ?number ?catapult)
   		(
   			(time-line ?number ?t)
   		)
   		(
   			(time-line ?number ?t)
   			(at ?number weapon-station1)
   			(at ?number ?fuel-station)
   		)
   		(
   			(time-line ?number (call + ?t 20))
   			(at ?number ?catapult)
   		)
   )
   ;;------------------------------------------------------------------------
   (:method (launch ?number ?catapult)
   		(
   			(at ?number ?catapult)
   			(or
   				(same ?catapult catapult1)
   				(same ?catapult catapult2)
   			)
   		)
   		(
   			(!launch1 ?number ?catapult ?start 20)
   		)
   		(
   			(at ?number ?catapult)
   			(or
   				(same ?catapult catapult3)
   				(same ?catapult catapult4)
   			)
   		)
   		(
   			(!launch2 ?number ?catapult ?start 20)
   		)
   )
   ;;----------------------------------------------------------------------
   (:operator (!launch1 ?number ?catapult ?start ?duration)
   		(
   			(mission ?number ?type ?deadline)
   			(occupy ?number ?aircraft)
   			(time-line ?number ?t)
   			(on-deck-number ?aircraft-number)
   			(write-time ?catapult ?t1)
   			(read-time ?catapult ?t2)
   			;;(write-time deck ?t3)
   			;;(read-time deck ?t4)
   			(assign ?start (call max ?t1 ?t2 ?t))
   			(assign ?end (call + ?start ?duration))
   			(call >= ?deadline ?end)
   			
   		)
   		(
   			(time-line ?number ?t)
   			(on-deck-number ?aircraft-number)
   			(write-time ?catapult ?t1)
   			(read-time ?catapult ?t2)
   			;;(write-time deck ?t3)
   			;;(read-time deck ?t4)
   			(at ?number ?catapult)
   			(on-deck ?aircraft)
   			
   		)
   		(
   			(on-deck-number (call - ?aircraft-number 1))
   			(time-line ?number ?end)  ;;ʱ���ƽ����������
   			(write-time ?catapult ?end)
   			(read-time ?catapult ?end)
   			;;(write-time deck ?start)
   			;;(read-time deck ?start)   ;;����ͬʱ�ж�ܷɻ�һ�����
   			(catapulted ?number)
   		)
   )
   ;;-----------------------------------------------------------------------
   (:operator (!launch2 ?number ?catapult ?start ?duration)
   		(
   			(mission ?number ?type ?deadline)
   			(occupy ?number ?aircraft)
   			(time-line ?number ?t)
   			(on-deck-number ?aircraft-number)
   			(write-time ?catapult ?t1)
   			(read-time ?catapult ?t2)
   			;;(write-time deck ?t3)
   			;;(read-time deck ?t4)
   			(write-time land0 ?t5)
   			(read-time land0 ?t6)
   			(assign ?start (call max ?t1 ?t2 ?t5 ?t6 ?t))
   			(assign ?end (call + ?start ?duration))
   			(call >= ?deadline ?end)
   		)
   		(
   			(time-line ?number ?t)
   			(on-deck-number ?aircraft-number)
   			(write-time ?catapult ?t1)
   			(read-time ?catapult ?t2)
   			;;(write-time deck ?t3)
   			;;(read-time deck ?t4)
   			(write-time land0 ?t5)
   			(read-time land0 ?t6)
   			(at ?number ?catapult)
   			(on-deck ?aircraft)
   		)
   		(
   			(on-deck-number (call - ?aircraft-number 1))
   			(time-line ?number ?end)  ;;ʱ���ƽ����������
   			(write-time ?catapult ?end)
   			(read-time ?catapult ?end)
   			;;(write-time deck ?start)
   			;;(read-time deck ?start)   ;;����ͬʱ�ж�ܷɻ�һ�����
   			(write-time land0 ?end)
   			(read-time land0 ?end)
   			(catapulted ?number)
   		)
   )
   ;;;------------------------------(landing ?number)--------------------------
   ;;;-------------------------------------------------------------------------
   ;;����ʽ�����ǣ�����½�������ʹ��ʱ��-�ɻ�Ӧ�ý���ʱ�䣩Խ��˵���ӳ�Խ�࣬���ȼ�ҲԽ��
   (:method (landing ?number)
   		;;�����½���������ʱ��<���е��ѷ����Ӧ�ý���ʱ�䣬��ѡ��Ӧ�ý���ʱ����С�Ľ��н���
   		;;;;�����½���������ʱ��>����ĳһ����Щ���Ѿ���������Ľ���ʱ�䣬˵�����������ӳ٣�ѡ���ӳ��������Ƚ���
   		(:sort-by ?t #'>
   		  (	(write-time land0 ?t1)
   			(read-time land0 ?t2)
   			(write-time catapult3 ?t3)
   			(read-time catapult3 ?t4)
   			(write-time catapult4 ?t5)
   			(read-time catapult4 ?t6)
   			(assign ?t7 (call max ?t1 ?t2 ?t3 ?t4 ?t5 ?t6))
   			(or
   				   	(
   				   		(occupy ?number ?aircraft)
   				   		(catapulted ?number)
   				   		(time-line ?number ?t8)
   				   		(mission ?number ?type ?deadline)
   				   		(same ?type 1)
   				   		(assign ?t9 (call + ?t8 240))
   				   	)
   				   	(
   				   		(occupy ?number ?aircraft)
   				   		(catapulted ?number)
   				   		(time-line ?number ?t8)
   				   		(mission ?number ?type ?deadline)
   				   		(same ?type 2)
   				   		(assign ?t9 (call + ?t8 120))
   				   	)
   			)
   			(assign ?t (call - ?t7 ?t9))
   			(assign ?tt (call max ?t9 ?t7))  
   		  )
   		)
   		;;subtasks
   		(:ordered
   			(!landing ?number ?tt 30)
   			(locate ?aircraft)
   		)
   )
   ;---------------------------------------------------
   (:operator (!landing ?number ?t ?duration)
   		(
   			(occupy ?number ?aircraft)
   			(catapulted ?number)
   			(time-line ?number ?t1)
   			(write-time ?catapult3 ?t2)
   			(read-time ?catapult3 ?t3)
   			(write-time ?catapult4 ?t4)
   			(read-time ?catapult4 ?t5)
   			(write-time ?land0 ?t6)
   			(read-time ?land0 ?t7)
   			(write-time ?aircraft ?t8)
   			(assign ?end (call + ?t ?duration))
   			;;(write-time deck ?t9)
   			;;(read-time deck ?t10)
   			(on-deck-number ?aircraft-number)
   		)
   		(	
   			(catapulted ?number)
   			(time-line ?number ?t1)
   			(write-time ?catapult3 ?t2)
   			(read-time ?catapult3 ?t3)
   			(write-time ?catapult4 ?t4)
   			(read-time ?catapult4 ?t5)
   			(write-time ?land0 ?t6)
   			(read-time ?land0 ?t7)
   			(write-time ?aircraft ?t8)
   			;;(write-time deck ?t9)
   			;;(read-time deck ?t10)
   			(on-deck-number ?aircraft-number)
   		)
   		(
   			(done ?number)
   			(time-line ?number ?end)
   			(write-time ?catapult3 ?end)
   			(read-time ?catapult3 ?end)
   			(write-time ?catapult4 ?end)
   			(read-time ?catapult4 ?end)
   			(write-time ?land0 ?end)
   			(read-time ?land0 ?end)
   			(write-time ?aircraft ?end)
   			;;(write-time deck ?end)
   			;;(read-time deck ?end)
   			(on-deck-number (call + ?aircraft-number 1))
   			(on-deck ?aircraft)
   		)
   )
   ;----------------------------------------------------------------
   (:method (locate ?aircraft)
   		(
   			(on-deck-number ?aircraft-number)
   			(call <= ?aircraft-number 6)	
   		)
   		(
   			(!release ?aircraft)
   		)
   		(
   			(on-deck-number ?aircraft-number)
   			(call > ?aircraft-number 6)
   		)
   		(:ordered
   			(!move-to-bottom ?aircraft)
   			(!release ?aircraft)
   		)  
   )
   ;------------------------------------------------------------------
    (:operator (!move-to-bottom ?aircraft)  ;;ȱ���ǲ�������ʹ������һ������
   		(:sort-by ?t #'<
   			(
   				(write-time ?aircraft ?t1)
   				(is ?elevator elevator)
   				(write-time ?elevator ?t2)
   				(read-time ?elevator ?t3)
   				(assign ?t (call max ?t1 ?t2 ?t3))
   			)	
   		)
   		(
   			(on-deck ?aircraft)
   			(write-time ?aircraft ?t1)
   			(write-time ?elevator ?t2)
   			(read-time ?elevator ?t3)
   		)
   		(
   			(below-deck ?aircraft)
   			(write-time ?aircraft (call + ?t 30))
   			(write-time ?elevator (call + ?t 30))
   			(read-time ?elevator (call + ?t 30))
   		)
   	)
   	;---------------------------------------------------------------
   	(:operator (!release ?aircraft)
   		()
   		(
   			(busy ?aircraft)
   		)
   		()
   	) 
   ;---------------------------����---------------------------------------
   (:- (same ?x ?x) nil) 
  )
)


(find-plans 'problem :verbose :plans :time-limit 5)