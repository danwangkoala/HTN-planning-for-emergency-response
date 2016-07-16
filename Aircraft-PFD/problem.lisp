;;;; Created on 2013-10-24 14:52:09
(defproblem problem aircraft7domain
 ( 
    (is SUAV1 SUAV)(on-deck SUAV1)(write-time SUAV1 0)(read-time SUAV1 0)
    (is SUAV2 SUAV)(on-deck SUAV2)(write-time SUAV2 0)(read-time SUAV2 0)
    ;(is SUAV3 SUAV)(on-deck SUAV3)(write-time SUAV3 0)(read-time SUAV3 0)
    
    ;(is SUAV4 SUAV)(below-deck SUAV4)(write-time SUAV4 0)(read-time SUAV4 0)
    ;(is SUAV5 SUAV)(below-deck SUAV5)(write-time SUAV5 0)(read-time SUAV5 0)
    ;(is SUAV6 SUAV)(below-deck SUAV6)(write-time SUAV6 0)(read-time SUAV6 0)
    
    (is FUAV1 FUAV)(on-deck FUAV1)(write-time FUAV1 0)(read-time FUAV1 0)
    ;(is FUAV2 FUAV)(on-deck FUAV2)(write-time FUAV2 0)(read-time FUAV2 0)
    ;(is FUAV3 FUAV)(on-deck FUAV3)(write-time FUAV3 0)(read-time FUAV3 0)
    
    ;(is FUAV4 FUAV)(below-deck FUAV4)(write-time FUAV4 0)(read-time FUAV4 0)
    ;(is FUAV5 FUAV)(below-deck FUAV5)(write-time FUAV5 0)(read-time FUAV5 0)
    ;(is FUAV6 FUAV)(below-deck FUAV6)(write-time FUAV6 0)(read-time FUAV6 0)
    
    (is fuel-station1 fuel-station)(write-time fuel-station1 0)(read-time fuel-station1 0)
    (is fuel-station2 fuel-station)(write-time fuel-station2 0)(read-time fuel-station2 0)
    (is weapon-station1 weapon-station)(write-time weapon-station1 0)(read-time weapon-station1 0)
    
    (is elevator1 elevator)(write-time elevator1 0)(read-time elevator1 0)
    (is elevator2 elevator)(write-time elevator2 0)(read-time elevator2 0)
    
    (on-deck-number 6)(write-time deck 0)(read-time deck 0);表示初始时刻甲板上有6加飞机
    (is catapult1 catapult)(write-time catapult1 0)(read-time catapult1 0)
    (is catapult2 catapult)(write-time catapult2 0)(read-time catapult2 0)
    (is catapult3 catapult)(adhere catapult3 land0)(write-time catapult3 0)(read-time catapult3 0)
    (is catapult4 catapult)(adhere catapult4 land0)(write-time catapult4 0)(read-time catapult4 0)
    (is land0 land)(adhere land0 catapult3)(adhere land0 catapult4)(write-time land0 0)(read-time land0 0)
    (mission 1 1 70)
    (mission 2 2 100) 
 )   
 
 (:unordered 
   (mission 2 2 500)
   (mission 2 2 100)
   (mission 1 1 70)
        
    ;;(mission 1 1 70)
    ;;(mission 3 1 80)
    ;;(mission 4 2 90)
    (mission 5 1 120)  
 )
)
