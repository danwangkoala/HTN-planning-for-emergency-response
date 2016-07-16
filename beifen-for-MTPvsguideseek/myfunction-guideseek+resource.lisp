;;;; Created on 2014-05-21 10:59:42
;;;;---------------------------------------------------------------
;;;;----------------------------------------------------------------
;;;;---------------------------resource----------------------------

(defparameter *ResourceState* (list nil))

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
  (format t "Ravailable resource resourceID=~A~%" ResourceID)
  ;(format t "1. Ravailable resource=~A~%" *ResourceState*)
  ;(format t "run into R-available ResourceID=~A~%" ResourceID)
  (let* (x y z)
    (setf x (assoc ResourceID *ResourceState*))
    (setf y (cdr x))
    (format t "1.R-available y=~A~%" y)
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
    (format t "2.R-available y=~A~%" y)
    (if (eql (length y) 1)
        (setf z (cdr (first y)))
        (setf z (cdr (second y))))
    ;(setf y (assoc num y :test #'<=))
    ;(format t "R-available y=~A~%" y)
    ;(format t "R-available time ResourceID=~A,time=~A~%" ResourceID (cdr y))
    ;(setf z (cdr y))
    z))
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
    (format t "1. x=~A~%" x)
    (setf y (cdr x))
    (format t "2. y=~A~%" y)
    (setf z (+ StartT Duration))
    (format t "3. z=~A~%" z)
    (dolist (g y)
      (cond ((and (>= (cdr g) StartT)
                  (< (cdr g) z))
             (setf (car g) (+ (car g) num))
             )
            ((>= (cdr g) z)
             ())))
    (format t "4. y=~A~%" y)
    (setf y (sort y #'> :key #'cdr))
     (format t "6. y=~A~%" y)
   ; (setf i (rassoc StartT y :test #'=))
    ; (format t "7. i=~A~%" i)
   ; (setf p (rassoc StartT y :test #'>))   ;;;!!attention, pay attention to the >.....StartT>???
    ; (format t "8. p=~A~%" p)
    ;(unless (EQL nil p)
    ;  (if (= (car i) (car p))
       ;      (setf y (delete i y))))
   ; (setf j (rassoc z y :test #'=))
    ;(setf q (rassoc z y :test #'>))
    ;(unless (EQL nil q)
      ;(if (= (car j) (car q))
     ;        (setf y (delete j y)))) 
    (setf y (sort y #'< :key #'cdr))
    (format t "5. y=~A~%" y)
    (setf (cdr (assoc ResourceID *ResourceState*)) y)))
;;;;---------------------------------------------------------------------------
;;;;---------------------------------------------------------------------------
;;;---------------------search plan--------------------------------------------
;;;;----------------------------------------------------------------------------
;;;;--------------------------------------------------------------------------------
(defun guide-seek-plans (task task-name task-body state tasks top-tasks
                               partial-plan partial-plan-cost depth
                               which-plans protections)
  (format t "run into guide-seek-plans top task=~A~%" top-tasks)
  
  ;(format t "run into guide-seek-plans task=~A~%top-task=~A~%temporal-network=~A~%" task top-tasks temporal-network)
  ;;;;more than one task in the top-tasks
  (let* (task-method task-start task-end check-tasks) 
    (setf check-tasks top-tasks)
    ;;;----add atomic tasks which are in the top-tasks to the plan first----------
    ;;;----all composite tasks can be decomposed now should be decomposed first---
    ;(format t "guide-seek-plans check-tasks=~A~%" check-tasks)
    (dolist (task1 check-tasks)
      (format t "1. task1=~A~%" task1)
      (let* ((task1-name (get-task-name task1))
             (task1-body (get-task-body task1)))
        (if (primitivep task1-name)
            (progn 
              (seek-plans-primitive task1 task1-name task1-body state tasks top-tasks
                            partial-plan partial-plan-cost depth which-plans
                            protections)
              (return-from guide-seek-plans nil)))))
    ;;;----all composite tasks can be decomposed now should be decomposed first---
    (dolist (task1 check-tasks)
      (format t "2 task1=~A~%" task1)
      (let* ((task1-name (get-task-name task1))
             (task1-body (get-task-body task1)))
        (unless (string-equal "atom" task1-name :start1 0 :end1 4 :start2 0 :end2 4)
          (dolist (m (gethash task1-name *methods*))
            (setq result1 (pre-check-non state task1-body m))
            (when result1
              (progn
                (seek-plans-nonprimitive task1 task1-name task1-body state
                                            tasks top-tasks partial-plan 
                                           partial-plan-cost depth which-plans protections)
                (return-from guide-seek-plans nil))))
          (setf check-tasks (remove task1 check-tasks)))))
           
    ;;;-----------------in order to add atomic tasks which are in the top-tasks to the plan first---------------  
    (setf task-method (car (gethash task-name *methods*)))
    (multiple-value-setq (task-start task-end) (obtain-action-time state task-body task-method protections depth))
    (format t "guide-seek-plan task-start=~A  task-end=~A~%" task-start task-end)
    (if (eql task-end 0)  ;;precondition is not satisfied
        (return-from guide-seek-plans nil)
        (progn
          (real-guide-seek-plans task task-name task-body task-start task-end state tasks top-tasks check-tasks 
                               partial-plan partial-plan-cost depth which-plans protections)
          (return-from guide-seek-plans nil)))))

(defun real-guide-seek-plans (task task-name task-body start end state tasks top-tasks check-tasks partial-plan
                                    partial-plan-cost depth which-plans protections)
  ;(format t "run into real-guide-seek-plans task=~A~%top-tasks=~A~%*start-time*=~A~%*end-time*=~A~%" task top-tasks *start-time* *end-time*)
  (format t "run into real-guide-seek-plans~%")
  ;;;if only "task" is in the top-list, doesn't need guide seek process
  (if (eql 1 (length check-tasks))
      (progn 
        (seek-plans-nonprimitive task task-name task-body state tasks top-tasks partial-plan partial-plan-cost
                                  depth which-plans protections)
        (return-from real-guide-seek-plans nil)))
  (setf check-tasks (remove task check-tasks))
  (dolist (task1 check-tasks)
    ;(format t "real-guide-seek-plans dolist check-tasks=~A~%" check-tasks)
    ;(format t "real-guide-seek-plans dolist task1=~A~%temporal-network=~A~%" task1 temporal-network)
      (let* ((task1-name (get-task-name task1))
             (task1-body (get-task-body task1))
             task1-start task1-end task1-method)
      (if (primitivep task1-name)
          (progn 
           (seek-plans-primitive task1 task1-name task1-body state tasks top-tasks partial-plan 
                                 partial-plan-cost depth which-plans protections)
           (return-from real-guide-seek-plans nil))
          (progn
            (if (string-equal "atom" task1-name :start1 0 :end1 4 :start2 0 :end2 4)
                (progn
                  (setf task1-method (car (gethash task1-name *methods*)))
                  (multiple-value-setq (task1-start task1-end)
                                        (obtain-action-time state task1-body task1-method protections depth))
                      (format t "real-guide-seek-plans task1-start=~A, task1-end=~A~%" task1-start task1-end)
                      (if (and (< task1-end end) (> task1-end 0))  ;;;various conditions, depending on search mechanism(other search mechanism can be added here)
                          (progn
                            (real-guide-seek-plans task1 task1-name task1-body task1-start task1-end state tasks top-tasks 
                                                   check-tasks partial-plan partial-plan-cost depth which-plans protections)
                            (return-from real-guide-seek-plans))
                          (setf check-tasks (remove task1 check-tasks))))
                (progn
                  (seek-plans-nonprimitive task1 task1-name task1-body state tasks top-tasks partial-plan partial-plan-cost 
                                           depth which-plans protections)
                  (return-from real-guide-seek-plans nil)))))))
  (if (null check-tasks)
      (progn
        (seek-plans-nonprimitive task task-name task-body state tasks top-tasks partial-plan partial-plan-cost depth
                               which-plans protections)
        (return-from real-guide-seek-plans nil))))

(defun pre-check-non (state task-body method)
  (format t "run into pre-check-non task-body=~A~%" task-body)
  (let ((standardized-method (standardize method))  
        task-unifier state-unifiers pre tail)
    (setq task-unifier (unify (second standardized-method) task-body))
    (unless (eql task-unifier 'fail)
      (do* ((body (cddr standardized-method) (cdddr body)))
           ((null body) nil)
           (setq pre (apply-substitution (second body) task-unifier))
           (setq tail (apply-substitution (third body) task-unifier))
           (setq state-unifiers (find-satisfiers pre state))
           (if state-unifiers
               (return-from pre-check-non 1)
               (return-from pre-check-non nil))))))
  

  
           

(defun obtain-action-time (state task-body method protections depth)
  ;;temporal-network: parameter deliver from the upper function
  ;;*temporal-network*:global variable, used in the domain
  ;;temporal-network1: the new temporal-network changed by the domain(add time constrains from resources)
  (format t "run into obtain-action-time task-body=~A~%" task-body)

  (let ((standardized-method (standardize method))  
        task-unifier state-unifiers pre tail)
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
              (return-from obtain-action-time (values starttime endtime)))  
            (progn
              (trace-print :methods (first body)
                     "~2%Depth ~s, inapplicable method ~s~%      task ~s"
                     depth
                     (first body)
                     task-body)
              (return-from obtain-action-time (values 0 0))))))))






  