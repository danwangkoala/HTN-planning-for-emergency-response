;;;; Created on 2014-05-21 10:59:42
;;;;---------------------------------------------------------------
;;;;----------------------------------------------------------------
;;;;---------------------------resource----------------------------

(defparameter *ResourceState* (list nil))
(defparameter *start-time* nil)
(defparameter *end-time* nil)
(defparameter *temporal-network* nil)
;;the followling functions are used to initilize resource state

(defun  R-initialized0 (ResourceID capacity Rtime)
  (let* 
    ((l1 (cons (cons capacity Rtime) nil))
     (l2 (cons ResourceID l1)) l3)
    (setf l3 (cons l2 nil))))

(defun R-initialize (ResourceID capacity Rtime)
    (if (null (car *ResourceState*))
        (setf  *ResourceState* (R-initialized0 ResourceID capacity Rtime))  
        (let* (x y)
          (setf x (R-initialized0 ResourceID capacity Rtime))
          (setf y (assoc (caar x) *ResourceState*))     ;;to avoid one resourceID is initialized more than one time
          (if (null y)
          (setf *ResourceState* (append *ResourceState* x))))))

;;the following function is used to return the resource's earliest available time for 
;;specific instance of an operator  ?g---ReourceID, ?num--the number of ?g needed by the opertor instance
;;;;attention!!!! if num is more than 1, this function may generate error, when one action contain another action
(defun R-available (ResourceID num)
  (format t "Ravailable resource~%")
  ;(format t "1. Ravailable resource=~A~%" *ResourceState*)
  ;(format t "run into R-available ResourceID=~A~%" ResourceID)
  (let* (x y z)
    (setf x (assoc ResourceID *ResourceState*))
    (setf y (cdr x))
    ;(format t "1.R-available y=~A~%" y)
    ;;;;;remove the part of list with the end (0,num)
    (let* (p)
      (setf p (cons 0 0))
      ;;;following used to find the cons with the largest cdr (time) whose capacity=0
      (dolist (j y)
        (if (< (car j) num)    ;;if this change to (EQL (car j) 0), can only consider num=1
            (if (> (cdr j) (cdr p))
                (setf p j))))
      ;;;delete all cons in y whose cdr < the one find in just previous
      (dolist (j y)
        (if (and (> (cdr j) 0) (<= (cdr j) (cdr p)))
            (setf y (delete j y)))))
    ;(format t "2.R-available y=~A~%" y)
    (setf y (assoc num y :test #'<=))
    ;(format t "R-available y=~A~%" y)
    ;(format t "R-available time ResourceID=~A,time=~A~%" ResourceID (cdr y))
    (setf z (cdr y))))
;;;;;;;;;;;;;modified version, -------------waiting for completing
;(defun R-available (ResourceID num duration)
 ; (let* (x y z)
  ;  (setf x (assoc ResourceID *ResourceState*))
   ; (setf y (cdr x))
   ; (when t
   ;   (if (>= (caar y) num)
   ;       (let* (x1 y1 z1)
   ;         (setf x1 (+ duration (cdr (car y))))
   ;         (setf y1 (cdr y))
   ;         (dolist (z1 y1)))))))
;;;
;;;the following function is used to update the resource state
;;; when a resource is used in an operator instance 
(defun R-Supdate (ResourceID num StartT Duration)
  (format t "Rupdate resource~%")
  ;(format t "Rupdate resource=~A~%" *ResourceState*)
  (let* (x y z h)
    (setf x (assoc ResourceID *ResourceState*))
    (setf y (cdr x))
    (setf z (+ StartT Duration))
    (dolist (g y)
      (cond ((EQL g nil)
             ())
            ((and (>= (cdr g) StartT)
                  (< (cdr g) z))
             (setf (car g) (- (car g) num)))
            ((>= (cdr g) z)
             ())))
    (setf y (sort y #'> :key #'cdr))
    ;;;----------insert the (capacity StartT)-------
    (setf h (rassoc StartT y :test #'=))
    (if (EQL h nil)   ;;exist
        (let* (i)
          (setf i (rassoc StartT y :test #'>))
          (setf i (cons (- (car i) num) StartT))
          (setf y (adjoin i y))
          (setf y (sort y #'> :key #'cdr)))
        ())
    ;;;--------insert the (capacity (StartT+Duration))----
    (setf h (rassoc z y :test #'=))
    (if (EQL h nil)
        (let* (i)
          (setf i (rassoc z y :test #'>))
          (setf i (cons (+ (car i) num) z))
          (setf y (adjoin i y))
          (setf y (sort y #'> :key #'cdr)))
        ())
    (setf y (sort y #'< :key #'cdr))
    (setf (cdr (assoc ResourceID *ResourceState*)) y)))

;;;the following funtion is used for resource state backtracking

(defun R-Sbacktracking (ResourceID num StartT Duration)
  (format t "R-Sbacktracking~%")
  ;(format t "R-Sbacktracking resource=~A~%" *ResourceState*)
  (let* (x y z i j p q)
    (setf x (assoc ResourceID *ResourceState*))
    (setf y (cdr x))
    (setf z (+ StartT Duration))
    (dolist (g y)
      (cond ((and (>= (cdr g) StartT)
                  (< (cdr g) z))
             (setf (car g) (+ (car g) num))
             )
            ((>= (cdr g) z)
             ())))
    (setf y (sort y #'> :key #'cdr))
    (setf i (rassoc StartT y :test #'=))
    (setf p (rassoc StartT y :test #'>))   ;;;!!attention, pay attention to the >.....StartT>???
    (unless (EQL nil p)
      (if (= (car i) (car p))
             (setf y (delete i y))))
    (setf j (rassoc z y :test #'=))
    (setf q (rassoc z y :test #'>))
    (unless (EQL nil q)
      (if (= (car j) (car q))
             (setf y (delete j y)))) 
    (setf y (sort y #'< :key #'cdr))
    (setf (cdr (assoc ResourceID *ResourceState*)) y)))
;;;;---------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------
;;;---------------------search plan--------------------------------------------
;;;;----------------------------------------------------------------------------
;;;;--------------------------------------------------------------------------------
(defun guide-seek-plans (task task-name task-body state tasks top-tasks temporal-network
                               partial-plan partial-plan-cost depth
                               which-plans protections)
  (format t "run into guide-seek-plans task~%")
  (format t "third temporal-network=~A~%" (third temporal-network))
  ;(format t "run into guide-seek-plans task=~A~%top-task=~A~%temporal-network=~A~%" task top-tasks temporal-network)
  ;;;;more than one task in the top-tasks
  (let* (task-method task-start task-end check-tasks temporal-network1 temporal-network2) 
    (setf check-tasks top-tasks)
    ;;;----------------in order to add atomic tasks which are in the top-tasks to the plan first---------------
    ;(format t "guide-seek-plans check-tasks=~A~%" check-tasks)
    (dolist (task1 check-tasks)
      (let* ((task1-name (get-task-name task1))
             (task1-body (get-task-body task1)))
        (if (primitivep task1-name)
            (progn 
              (seek-plans-primitive task1 task1-name task1-body state tasks top-tasks temporal-network
                            partial-plan partial-plan-cost depth which-plans
                            protections)
              (return-from guide-seek-plans nil)))))
    ;;;-----------------in order to add atomic tasks which are in the top-tasks to the plan first---------------  
    (setf task-method (car (gethash task-name *methods*)))
    (multiple-value-setq (task-start task-end temporal-network1) (obtain-action-time state task-body temporal-network task-method protections depth))
    (format t "guide-seek-plan task-start=~A  task-end=~A~%" task-start task-end)
    (if (eql task-end 0)  ;;precondition is not satisfied
        (let* (x)
          ;(setq x1 (path-consistency-check1 temporal-network1))  
          ;;"path-consistency-check1" is used to check consistency of the network which contains
          ;;relation 4(relation 4 regards to the time already determined for actions),if (temporal-network) pass the check, x1=1 this means
          ;;preconditions other than temporal preconditions are not satisfied, return
          (if (= (third temporal-network1) 0)
              (return-from guide-seek-plans nil)
              (progn 
                (setq x (path-consistency-check1 temporal-network1))
                ;;path consistency-check2 is used to check consistency of the network which does not contain any relation4, if the network passes 
                ;;the check, x2=1 return temporal-network1 (contain the constrain cuased by resource), else, return nil
                (if (EQL x 'consistent)
                    (return-from guide-seek-plans temporal-network1)
                    (return-from guide-seek-plans nil)))))
        (progn
          (setq temporal-network2 (real-guide-seek-plans task task-name task-body task-start task-end 
                                                         state tasks top-tasks temporal-network temporal-network1
                                                         check-tasks partial-plan partial-plan-cost depth which-plans 
                                                         protections))
          (return-from guide-seek-plans temporal-network2)
          ))
    
    ))

(defun real-guide-seek-plans (task task-name task-body start end state tasks top-tasks temporal-network temporal-network2
                                    check-tasks partial-plan partial-plan-cost
                                   depth which-plans protections)
  ;(format t "run into real-guide-seek-plans task=~A~%top-tasks=~A~%*start-time*=~A~%*end-time*=~A~%" task top-tasks *start-time* *end-time*)
  (format t "run into real-guide-seek-plans~%")
  (format t "third temporal-network2=~A~%" (third temporal-network2))
  ;;;if only "task" is in the top-list, doesn't need guide seek process
  (if (eql 1 (length check-tasks))
      (progn 
        (seek-plans-nonprimitive-atom task task-name task-body state tasks top-tasks temporal-network2
                               partial-plan partial-plan-cost depth
                               which-plans protections)
        (return-from real-guide-seek-plans nil)))
  (setf check-tasks (remove task check-tasks))
  (dolist (task1 check-tasks)
    ;(format t "real-guide-seek-plans dolist check-tasks=~A~%" check-tasks)
    ;(format t "real-guide-seek-plans dolist task1=~A~%temporal-network=~A~%" task1 temporal-network)
      (let* ((task1-name (get-task-name task1))
             (task1-body (get-task-body task1))
             task1-start task1-end task1-method temporal-network1 temporal-network3)
      (if (primitivep task1-name)
          (progn 
           (seek-plans-primitive task1 task1-name task1-body state tasks top-tasks temporal-network
                            partial-plan partial-plan-cost depth which-plans
                            protections)
           (return-from real-guide-seek-plans nil))
          (progn
            (if (string-equal "atom" task1-name :start1 0 :end1 4 :start2 0 :end2 4)
                (progn
                  (let* ((task1-end0 (third task1-body))
                         (vars (first temporal-network))
                         (constrain (second temporal-network)) i j m)
                    (setq i (position *end-time* vars))
                    (setq j (position task1-end0 vars))
                    (setq m (aref constrain i j))
                    (when (< (first m) 0)
                      (format t "real-guide-seek-plans is a atom action~%")
                      (setf task1-method (car (gethash task1-name *methods*)))
                      (multiple-value-setq (task1-start task1-end temporal-network1) (obtain-action-time state task1-body 
                                                                                                         temporal-network task1-method protections depth))
                      (format t "real-guide-seek-plans task1-start=~A, task1-end=~A~%" task1-start task1-end)
                      (if (eql task1-end 0)
                          (let* (x)
                            ;(setq x1 (path-consistency-check1 temporal-network1))  
                        ;;"path-consistency-check1" is used to check consistency of the network which contains
                        ;;relation 4(relation 4 regards to the time already determined for actions),if (temporal-network) pass the check, x1=1 this means
                        ;;preconditions other than temporal preconditions are not satisfied, return
                            (if (= (third temporal-network1) 0)
                                (return-from real-guide-seek-plans nil)
                                (progn 
                                  (setq x (path-consistency-check1 temporal-network1))
                            ;;the check, x2=1 return temporal-network1 (contain the constrain cuased by resource), else, return nil
                                  (if (EQL x 'consistent)
                                      (return-from real-guide-seek-plans temporal-network1)
                                      (return-from real-guide-seek-plans nil))))))
                      (if (< task1-end end)   ;;;various conditions, depending on search mechanism(other search mechanism can be added here)
                          (progn
                            (setq temporal-network3 (real-guide-seek-plans task1 task1-name task1-body task1-start task1-end state tasks 
                                                                           top-tasks temporal-network temporal-network1 check-tasks partial-plan 
                                                                           partial-plan-cost depth which-plans protections))
                            (return-from real-guide-seek-plans temporal-network3))
                          (setf check-tasks (remove task1 check-tasks))))
                    (when (>= (first m) 0)
                      (setf check-tasks (remove task1 check-tasks)))))
                (progn
                  (seek-plans-nonprimitive task1 task1-name task1-body state tasks top-tasks temporal-network
                               partial-plan partial-plan-cost depth
                               which-plans protections)
                  (return-from real-guide-seek-plans nil)))))))
  (if (null check-tasks)
      (progn
        (seek-plans-nonprimitive-atom task task-name task-body state tasks top-tasks temporal-network2
                               partial-plan partial-plan-cost depth
                               which-plans protections)
        (return-from real-guide-seek-plans nil)
        )))

(defun obtain-action-time (state task-body temporal-network method protections depth)
  ;;temporal-network: parameter deliver from the upper function
  ;;*temporal-network*:global variable, used in the domain
  ;;temporal-network1: the new temporal-network changed by the domain(add time constrains from resources)
  (format t "run into obtain-action-time task-body=~A~%" task-body)
  (setq *temporal-network* temporal-network)
  ;;;start---------this is used to find the unique start and end labels for the specific taks-------
  (if (string-equal "start" (second task-body) :start1 0 :end1 5 :start2 1 :end2 6)
              (setq *start-time* (second task-body)))
  (if (string-equal "end" (third task-body) :start1 0 :end1 3 :start2 1 :end2 4)
              (setq *end-time* (third task-body))) 
  ;(format t "obtain-action-time *start-time*=~A~%" *start-time*)
  ;(format t "obtain-action-time *start-time*=~A~%" *end-time*)
  ;;;end-----------this is used to find the unique start and end labels for the specific taks-------   
  ;;(declare (ignore protections))
  
  (let ((standardized-method (standardize method))  
        task-unifier state-unifiers pre tail temporal-network1)
    ;;(format t "obtain-action-time standardized-method= ~A~%" standardized-method)
    ;; see if the standardized-method's head unifies with TASK-BODY
    (setq task-unifier (unify (second standardized-method) task-body))
    ;(format t "obtain-action-time task-unifier= ~A~%" task-unifier)
    ;(format t "obtain-action-time current-state= ~A~%" *current-state*)
    (unless (eql task-unifier 'fail)
      ;; STANDARDIZED-METHOD's CDDR is a list 
      ;; (label_1 pre_1 d_1 label_2 pre_2 d_2 ...) which acts like an
      ;; if-then-else: we look for the first true pre_i, and then evaluate d_i
      (do* ((body (cddr standardized-method) (cdddr body)))
           ((null body) nil)
        ;; apply v to PRE and TAIL
        (setq pre (apply-substitution (second body) task-unifier))
        ;(format t "obtain-action-time pre=~A~%" pre)
        (setq tail (apply-substitution (third body) task-unifier))
        ;;(format t "obtain-action-time tail=~A~%" tail)             
        ;; find all matches to the current state
        (setq state-unifiers (find-satisfiers pre state))
        ;(format t "obtain-action-time state-unifiers=~A~%" state-unifiers)
       ;;;;what about state-unifier=nil? here has no handling mechanism for this exception
       ;;;;in which no time argument can be returned.
        (setq temporal-network1 *temporal-network*) 
           ;;in the procondition of the substitution, time constrains from resources are added to the *temporal-constrians*         
        (if state-unifiers
            (let* (starttime endtime)
              (dolist (x (car state-unifiers))
                (if (string-equal "start" (car x) :start1 0 :end1 5 :start2 1 :end2 6)
                    (progn
                      (setf starttime (cdr x))))
                (if (string-equal "end" (car x) :start1 0 :end1 3 :start2 1 :end2 4)
                    (progn
                      (setf endtime (cdr x)))))
              (return-from obtain-action-time (values starttime endtime temporal-network1)))  
                  ;;precondition is satisfied, time constrains from resource do not need to added
                  ;;so simply return (temporal-network) rather than (temporal-network1)
            (progn
              (trace-print :methods (first body)
                     "~2%Depth ~s, inapplicable method ~s~%      task ~s"
                     depth
                     (first body)
                     task-body)
              (return-from obtain-action-time (values 0 0 temporal-network1))))))))

;;;;------------------------------------------------------------------------------------------------
;;;;--------------------------------temporal constrain-------------------------------------------------
;;;------------------------------------------------------------------------------------------------
;;;-------------the following function is used to update the "temporal-network list" in the planner ------ 
;;the element construction in q is (task element1 element2 element3 element4)
(defun add-task-constrains (q temporal-network)
  ;(format t "run into add task-constrain q=~A~% temporal-network=~A~%" q temporal-network)  
  (format t "run into add task-constrain~%")
  (setq temporal-network (add-variable (first q) temporal-network))
  (setq temporal-network (add-constraint (cdr q) temporal-network))  
  temporal-network)

(defun add-variable(var temporal-network)
  ;(setf *variable* (append *variable* st))
  (format t "run-into-add-variables~%")
  (let* ((vars (first temporal-network))
         (constrains (second temporal-network)))
    (dolist (new-var (cdr var))
      ;(format t "1. new-var=~A~%" new-var)
      (setf len (length vars))
      ;(format t "2. len=~A~%" len)
      (dotimes (i len)
        ;(format t "3. i=~A~%" i)
        (setf  (aref constrains  len  i) '(-10000 10000 ))
        (setf  (aref constrains i len) '(-10000 10000 )))
      (setf vars (append vars (list new-var))))
    (setf temporal-network (list vars constrains 1))))

(defun add-constraint (con temporal-network)
  ;;(add-constraint '((x0 x2 (12 20)) (x1 x2 (1 3))))
  (format t "run-into add-constraint con=~A~%" con)
  (let* ((vars (first temporal-network))
         (constrains (second temporal-network)) i j)
    (dolist (st con)
      (format t "st=~A~%" st)
      (setf i (position (second st) vars))
      (format t "i=~A~%" i)
      (setf j (position (third st) vars))
      (format t "j=~A~%" j)
      (setf (aref constrains  i j) (fourth st))
      (setf (aref constrains  j i) (reverse-con (fourth st))))
    (setf temporal-network (list vars constrains 1))))

(defun reverse-con (con)
  (if (> (length con) 2)
      (list (- (second con)) (- (first con)) (- (fourth con)) (- (third con)))
      (list (- (second con)) (- (first con)))))

;;;;--------------------the following function is used to update the *temporal-network* in domain-----------
(defun temporal-network-localupdate (t-start duration)
  (format t "run into temporal-network-localupdate~%")
  (let* ((vars (first *temporal-network*))
         (constrains (second *temporal-network*)) i j)
    (setf i (position 'RT vars))
    ;(format t "i=~A~%" i)
    (setf j (position *start-time* vars))
    ;(format t "j=~A~%" j)
    (setf (aref constrains  i j) (list t-start 10000))
    (setf (aref constrains  j i) (reverse-con (list t-start 10000)))
    (setf i (position *end-time* vars))
    ;(format t "again i=~A~%" i)
    (setf (aref constrains j i) (list duration duration))
    (setf (aref constrains i j) (reverse-con (list duration duration)))
    (setf *temporal-network* (list vars constrains 1))))
;;;;--------------------the following function is used to find out the start-time of a task in domain------------
(defun get-start-time (duration)
  (format t "run into get-start-time~%")
  (let* ((vars (first *temporal-network*))
         (constrain (second *temporal-network*)) check task-time i j)
    (if (= (third *temporal-network*) 1)
        (progn
          (multiple-value-setq (constrain check) (path-consistency vars constrain))
          (format t "get-start-time check=~A~%" check)
          (if (equal check 'fail)
              (progn
                (format t "get-start time fail!~%")
                (return-from get-start-time 'fail))
              (setq *temporal-network* (list vars constrain 0)))))

    (setq i (position 'RT vars))
    (setq j (position *start-time* vars))
    (setq task-time (aref constrain i j))
    (if (< (first task-time) 0)
        (progn
          (format t "get-start time fail!~%")
          (return-from get-start-time 'fail))
        (progn
          (format t "get-start-time task-time=~A~%" (first task-time))
          (return-from get-start-time (first task-time))))))
;;;;-----------------path-consistency-check1(do not contain definitive start-end times that determined during planning)-------------
(defun path-consistency-check1 (temporal-network)
  (format t "run-into-path-consistency-check1~%")
  (let* ((vars (first temporal-network))
         (constrain (second temporal-network)) check len m)
    ;(format t "vars=~A~%" vars)
    (setq len (length vars))
    ;(format t "len=~A~%" len)
    (dotimes (i (- len 2))
      (setq m (aref constrain 0 (+ i 1)))
      ;(format t "m=~A~%" (length m))
      (if (> (length m) 2)
          (setf (aref constrain 0 (+ i 1)) (list (third m) (fourth m)))
          (setf (aref constrain (+ i 1) 0) (reverse-con (aref constrain 0 (+ i 1))))))
    (multiple-value-setq (constrain check) (path-consistency vars constrain))
    (return-from path-consistency-check1 check)))
;;;;------------------path-consistency-check2(do not contain the determined start-end time of a specific task-----------
;;;;------------------this function will remove the determined time of the task, no matter the network is consistent)-----------------------
(defun path-consistency-check2 (temporal-network task-start task-end)
  (format t "run-into-path-consistency-check2~%")
  (let* ((vars (first temporal-network))
         (constrain (second temporal-network)) check m i)
    (setq i (position task-start vars))
    (setq m (aref constrain 0 i))
    (setf (aref constrain 0 i) (list (third m) (fourth m)))
    (setf (aref constrain i 0) (reverse-con (aref constrain 0 i)))
    
    (setq i (position task-end vars))
    (setq m (aref constrain 0 i))
    (setf (aref constrain 0 i) (list (third m) (fourth m)))
    (setf (aref constrain i 0) (reverse-con (aref constrain 0 i)))
    
    (multiple-value-setq (constrain check) (path-consistency vars constrain))
    (if (EQL check 'fail)
        (setq temporal-network1 (list vars constrains 1))
        (setq temporal-network1 (list vars constrains 0)))
    (return-from path-consistency-check2 (values temporal-network1 check))))

;;;;------------------the following function is used to check the consistency of a temporal netowrk--------------

(defun path-consistency (var con)
  ;;Path Consistency algorithmÀ„∑®
  (format t "run-into-path-consistency~%")
  (setf len (length var))
  (dotimes (k len)
    (dotimes (i len) 
      (dotimes (j len)          
        (cond
          ((or (= i k) (= j k) (<= j i))
           ())
          (t
           ;(print (aref con  i j))
           (let* (m n)
             (setf m (intersect (aref con  i j) (composition (aref con  i k) (aref con  k j))))
             (setf n (aref con i j))
             (if (< (second m) (first m))
                 (progn
                   (format t "inconsistency!~%");;inconsistency
                   (return-from path-consistency (values con 'fail))))
             (if (> (length n) 2)
                 (setf (aref con i j) (list (first m) (second m) (third n) (fourth n)))
                 (setf (aref con i j) m)))
           ;(format t "~% current k is ~s and i: ~s VS j: ~s ~% " k i j)
           ;(format t "after ~s and ik is  ~s VS kj is ~s  " (aref con  i j) (aref con  i k) (aref con  k j))
           (setf (aref con  j i) (reverse-con (aref con  i j))))))))
  (return-from path-consistency (values con 'consistent)))

(defun composition (st1 st2)
  (list (+ (first st1) (first st2))
        (+ (second st1) (second st2))))

(defun intersect (st1 st2)
  (list (max (first st1) (first st2))
        (min (second st1) (second st2))))      

;;;-------------------the following function is used to execute compound tasks whose name begin with "atom"----------
(defun seek-plans-nonprimitive-atom (task1 task-name task-body state tasks 
                                      top-tasks temporal-network partial-plan partial-plan-cost
                                      depth which-plans protections)
  (format t "run into seek plans nonprimitive-atom task=~A~%temporal-network=~A~%" task1 (third temporal-network))
  (setq *temporal-network* temporal-network)
  ;;(setq *temporal-network* (adjoin (list (second task-body)) *temporal-network*))  
      ;;the element added here is used to store the time constrain generated by resource
  
  (let (result1 tasks1 top-tasks1 label r temporal-network1 temporal-network2)
    (dolist (m (gethash task-name *methods*))
      (setq result1 (apply-method state task-body m protections depth))
      (setq temporal-network1 *temporal-network*)  ;;now *temporal-network* contains time constrains from resource
                                                         ;;if result1 is not null, *temporal-constrain* also contains the value of 
                                                         ;;the start-time and end-time of the task.
      (format t "result1=~A~%" result1)
      (when result1
        ;;if the subtasks of "task1" is unordered, each subtask can be immediate, 
        ;;in this case, result1 is all possible combinations
        (dolist (lr result1)
          (setq label (car lr))
          (setq r (cdr lr))
	  (when *plan-tree* (record-reduction task1 r))
          (trace-print :methods label
                       "~2%Depth ~s, applying method ~s~%      task ~s~%   precond ~s~% reduction ~s"
                       depth label task1 (fourth m) r)
          (trace-print :tasks task-name
                       "~2%Depth ~s, reduced task ~s~% reduction ~s"
                       depth task1 r)
          (setq top-tasks1 (replace-task-top-list top-tasks task1 r))
          (setq tasks1 (replace-task-main-list tasks task1 r))
          (setq temporal-network2 (seek-plans state tasks1 top-tasks1 temporal-network1 partial-plan
                      partial-plan-cost (1+ depth) which-plans
                      protections))
          (unless (null temporal-network2)
            (let* (x1 task-start task-end temporal-network3)
              (dolist (e task-body)
                (if (string-equal "start" e :start1 0 :end1 5 :start2 1 :end2 6)
                    (setq task-start e))
                (if (string-equal "end" e :start1 0 :end1 3 :start2 1 :end2 4)
                    (setq task-end e)))
              (multiple-value-setq (temporal-network3 x1) (path-consistency-check2 temporal-network2 task-start task-end))
              ;;;"path-consistency-check2" neglect the determined start-time and end-time of the specific "task-body"
              ;;;;two return values, if x1=1, remove the determined time for task-body from temporal-contrains2 and return
              (if (EQL x1 'consistent)
                  (seek-plans-nonprimitive-atom task1 task-name task-body state tasks 
                                      top-tasks temporal-network3 partial-plan partial-plan-cost
                                      depth which-plans protections)
                  (return-from seek-plans-nonprimitive-atom temporal-network3))))
          (and *plans-found* (eq which-plans :first)
               (not (optimize-continue-p))
               (return-from seek-plans-nonprimitive-atom nil)))))))







  