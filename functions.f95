        MODULE functions    
      
        USE constants, ONLY   : dp   ! double precision (i.e. accuracy)    
        IMPLICIT NONE
        
      
        !*..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..*
        !| .**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**. |
        !| |                                                                          | |
        !| |                                                                          | |
        !| |                 INFORMATION SUPPLIED BY THE USER:                        | | 
        !| |                                                                          | |
        !| |    * PROBLEM specification:                                              | |
        !| |                                                                          | |       
        !| |        - The DC componen f1:           'problem1'                        | |
        !| |        - The DC componen f2:           'problem2'                        | |
        !| |        - the number of variables:      'user_n'                          | |               
        !| |                                                                          | |
        !| |    * Different PARAMETERS:                                               | |
        !| |                                                                          | |       
        !| |        - the stopping tolerance:      'user_crit_tol'                    | |
        !| |                                                                          | |       
        !| |        MAIN ITERATION:                                                   | |       
        !| |        - the size of bundle B_1:      'user_size_b1'                     | |       
        !| |        - the size of bundle B_2:      'user_size_b2'                     | |       
        !| |        - the descent parameter:       'user_m'                           | |       
        !| |        - the decrease parameter:      'user_c'                           | |       
        !| |        - the decrease parameter:      'user_r_dec'                       | |       
        !| |        - the increase parameter:      'user_r_inc'                       | |           
        !| |        - the enlargement parameter:   'user_eps_1'                       | |       
        !| |                                                                          | |       
        !| |        CLARKE STATIONARY ALGORITHM:                                      | |       
        !| |        - the size of bundle B:        'user_size'                        | | 
        !| |        - the proximity measure:       'user_eps'                         | |               
        !| |        - the descent parameter:       'user_m_clarke'                    | |       
        !| |                                                                          | |       
        !| |                                                                          | |       
        !| |    * Computation of the value of the DC functions f_1 and f_2:           | |
        !| |        - f1(y, problem1)   the value of DC component f_1 at a point y    | |
        !| |        - f2(y, problem2)   the value of DC component f_2 at a point y    | |           
        !| |                                                                          | |               
        !| |                                                                          | |               
        !| |    * Computation of the subgradient of the DC components f_1 and f_2:    | |
        !| |        - subgradient_f1(y, problem1)    the subgradient of f_1 at y      | |
        !| |        - subgradient_f2(y, problem2)    the subgradient of f_2 at y      | |       
        !| |                                                                          | |
        !| |                                                                          | |
        !| |                                                                          | |
        !| .**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**. |
        !*..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..*
        
        
        !--------------------------------------------------------------------------------
        ! ----------------------------------------------------------------------------- |
        ! |                  INFORMATION ABOUT PARAMETERS:                            | |
        ! ----------------------------------------------------------------------------- |
        !--------------------------------------------------------------------------------       
        
    
        !****************** GLOBAL PARAMETERS *******************************************
 
        ! Test problem 1  : problem1 = 1   &  problem2 = 1
        ! Test problem 2  : problem1 = 2   &  problem2 = 2
        ! Test problem 3  : problem1 = 3   &  problem2 = 3
        ! Test problem 4  : problem1 = 4   &  problem2 = 4
        ! Test problem 5  : problem1 = 5   &  problem2 = 5
        ! Test problem 6  : problem1 = 6   &  problem2 = 6
        ! Test problem 7  : problem1 = 7   &  problem2 = 7
        ! Test problem 8  : problem1 = 8   &  problem2 = 8
        ! Test problem 9  : problem1 = 9   &  problem2 = 9
        ! Test problem 10 : problem1 = 10  &  problem2 = 10
        ! Test problem 11 : problem1 = 11  &  problem2 = 11
        ! Test problem 12 : problem1 = 12  &  problem2 = 12
        ! Test problem 13 : problem1 = 13  &  problem2 = 13
        ! Test problem 14 : problem1 = 14  &  problem2 = 14
        ! Test problem 15 : problem1 = 15  &  problem2 = 15
        ! Test problem 16 : problem1 = 16  &  problem2 = 16
 
        INTEGER, PARAMETER :: problem1 = 4               ! Objective function selected for the DC component f1
        INTEGER, PARAMETER :: problem2 = problem1         ! Objective function selected for the DC component f2
          
        !-------------------------------------------------------------------------------------------------
      
        INTEGER, PARAMETER :: user_n = 200                     ! The number of variables in the problem
        
        !-------------------------------------------------------------------------------------------------      
        !__________________________________________________________________________________________
        !>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>>
        !****************** PARAMETRES NEEDED ONLY IN MAIN ITERATION AlGORITHM ********************
        
        INTEGER, PARAMETER :: user_size_b1 = MIN(user_n + 5,1000) ! The biggest possible size of the bundle B_1 
                                                                  ! If user_size_b1 <= 0 then DEFAULT value MIN(user_n+5,1000) is used 
                                                                   
        INTEGER, PARAMETER :: user_size_b2 = 3                    ! The biggest possible size of the bundle B_2 
                                                                  ! If user_size_b2 <= 0 then DEFAULT value 3 is used  
              
        REAL(KIND=dp), PARAMETER :: user_m = 0.2_dp               ! The descent parameter  If user_m <= 0.0_dp .OR. user_m >= 1.0_dp 
                                                                  !                        then DEFAULT value 0.2_dp is used

        REAL(KIND=dp), PARAMETER :: user_c = -0.1_dp             ! The decrease parameter c in DBDC  
                                                                  ! If user_c <= 0.0_dp or user_c > 1.0_dp then DEFAULT value 0.1_dp is used 

                                                                 
        REAL(KIND=dp), PARAMETER :: user_r_dec = -0.99_dp         ! The decrease parameter r in DBDC 
        
        !If user_r_dec <= 0.0_dp .OR. user_r_dec >= 1.0_dp then DEFAULT value is used.
        !                               
        !   DEFAULT value:                          
        !     If user_n < 10:           user_r_dec = 0.75_dp    
        !     If 10 <= user_n < 300:    user_r_dec = the first two decimals of n/(n+5)
        !     If user_n >= 300:         user_r_dec = 0.99_dp
        !
        !   Some examples of the DEFAULT value of the parameter 'user_r_dec':
        !     If user_n=10:     user_r_dec = 0.66_dp                          
        !     If user_n=20:     user_r_dec = 0.80_dp                         
        !     If user_n=25:     user_r_dec = 0.83_dp                         
        !     If user_n=50:     user_r_dec = 0.90_dp                         
        !     If user_n=100:    user_r_dec = 0.95_dp                         
        !     If user_n=150:    user_r_dec = 0.96_dp     
        !     If user_n=200:    user_r_dec = 0.97_dp                      
        !     If user_n=250:    user_r_dec = 0.98_dp    
        !
        
        REAL(KIND=dp), PARAMETER :: user_r_inc = (10.0_dp)**(7)       ! The increase parameter R: If user_r_inc <= 1.0_dp 
                                                                      !                           then DEFAULT value (10.0_dp)**7 is used
 
        REAL(KIND=dp), PARAMETER :: user_eps_1 = 5*(10.0_dp)**(-5)    ! The enlargement parameter: If user_eps_1 <= 0.0_dp .OR. user_eps_1 > 1.0_dp 
                                                                      !                   then DEFAULT value 5*(10.0_dp)**(-5) is used

 
        !____________________________________________________________________________________________                                                       
        !>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<
        !****************** PARAMETRES NEEDED ONLY IN CLARKE STATIONARY AlGORITHM *******************
        
        
         INTEGER, PARAMETER :: user_size = user_n * 2               ! The biggest possible bundle size for the bundle used in 'Clarke stationary' algorithm

         REAL(KIND=dp), PARAMETER :: user_m_clarke = 0.01_dp        ! The descent parameter: If user_m_clarke <= 0.0_dp .OR. user_m_clarke >= 1.0_dp 
                                                                    !                        then DEFAULT value 0.01_dp is used
                                                                    
          
         REAL(KIND=dp), PARAMETER :: user_eps = -(10.0_dp)**(-7)    ! The proximity measure: If user_eps <= 0.0_dp 
                                                                    !              then DEFAULT value (10.0_dp)**(-6) is used when n <= 50
                                                                    !                                 (10.0_dp)**(-5) is used when n > 50
                                                                    
        !________________________________________________________________________________
        !>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<
        !****************** PARAMETER NEEDED IN STOPPING CONDITIONS ********************
              
        ! ** The stopping tolerance **      
        REAL(KIND=dp), PARAMETER :: user_crit_tol = -(10.0_dp)**(-3)     ! If user_crit_tol <= 0.0_dp then DEFAULT value (10.0_dp)**(-5) is used when n <=200
                                                                         !                                               (10.0_dp)**(-4) is used when n > 200

     
       
        !________________________________________________________________________________
        !>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<>><<
        
        
        
        !*..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..*
        !| .**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**. |
        !| |                                                                          | |
        !| |     To EXECUTE the bundle algorithm DBDC the USER needs to DETERMINE:    | | 
        !| |                                                                          | |       
        !| |        f1(y, problem1)     - value of DC component f_1 at a point y      | |
        !| |        f2(y, problem2)     - value of DC component f_2 at a point y      | |
        !| |                                                                          | |
        !| |        subgradient_f1(y, problem1)   - subgradient of f_1 at y           | |
        !| |        subgradient_f2(y, problem2)   - subgradient of f_2 at y           | |
        !| |                                                                          | |
        !| |                                                                          | |       
        !| .**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**. |
        !*..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..*
    
        
        CONTAINS

        
        !********************************************************************************
        !                                                                               |
        !              FUNCTION VALUES OF THE DC COMPONENTS f_1 AND f_2                 |
        !                                                                               |
        !********************************************************************************

           FUNCTION f1(y, problem1) RESULT(f)        
                !
                ! Calculates the function value of the DC component f_1 at a point 'y'.
                ! Variable 'problem1' identifies the objective function used.
                !
                ! NOTICE: The dimension of 'y' has to be 'user_n'.
                !
                IMPLICIT NONE
                !**************************** NEEDED FROM USER *************************************
                REAL(KIND=dp), DIMENSION(:), INTENT(IN) :: y    ! a point where the function value of the DC component f_1 is calculated
                INTEGER, INTENT(IN) :: problem1                 ! the objective function f_1 for which the value is calculated
                !**************************** OTHER VARIABLES **************************************
                REAL(KIND=dp) :: f                              ! the function value of the DC component f_1 at a point 'y'
                REAL(KIND=dp), DIMENSION(4) :: a                ! help variable
                REAL(KIND=dp), DIMENSION(user_n) :: f_i         ! help variable: only first n-1 places are used                         
                REAL(KIND=dp) :: apu, largest                   ! help variables
                REAL(KIND=dp) :: a1, a2, a3, a4, a5, a6         ! help variables
                INTEGER :: i, j, ind                            ! help variable
                
                SELECT CASE(problem1)

                   !-------------------------------------
                   !           Problem   1
                   !-------------------------------------
                   CASE(1)  
                     a1 = y(1)**4 + y(2)**2
                     a2 = (2.0_dp - y(1))**2 + (2.0_dp - y(2))**2 
                     a3 = 2.0_dp * EXP(-y(1)+y(2))

                     a4 = y(1)**2 - 2.0_dp * y(1) + y(2)**2 - 4.0_dp * y(2) + 4.0_dp
                     a5 = 2.0_dp * y(1)**2 - 5.0_dp * y(1) + y(2)**2 - 2.0_dp * y(2) 
                     a5 = a5 + 4.0_dp
                     a6 = y(1)**2 + 2.0_dp * y(2)**2 - 4.0_dp * y(2) + 1.0_dp               
                
                     f = MAX(a1,a2,a3) + a4 + a5 + a6          

                   !-------------------------------------  
                   
                   !-------------------------------------
                   !           Problem   2
                   !-------------------------------------
                   CASE(2)  
                     IF ((ABS(y(1)) - y(2) ) > 0.0_dp) THEN
                        f = ABS(y(1) - 1.0_dp) + 200.0_dp * (ABS(y(1)) - y(2) )
                     ELSE
                        f = ABS(y(1) - 1.0_dp)
                     END IF
                   !-------------------------------------    
                  
                   !-------------------------------------
                   !           Problem   3
                   !-------------------------------------
                   CASE(3)
                     f = ABS(y(1) - 1.0_dp)
                     f = f + ABS(y(3) - 1.0_dp )                
                     f = f + 4.95_dp * ( ABS(y(2) + y(4) - 2.0_dp ) )
                     f = f + 10.1_dp * ( ABS(y(2)-1.0_dp) + ABS(y(4)-1.0_dp) )      
                
                     IF ((ABS(y(1)) - y(2) ) > 0.0_dp) THEN
                        f = f + 200.0_dp * (ABS(y(1)) - y(2) )
                     END IF 
                
                     IF ((ABS(y(3)) - y(4) ) > 0.0_dp) THEN
                        f = f + 180.0_dp * (ABS(y(3)) - y(4) )
                     END IF                  
                   !-------------------------------------                  
                   
                   !-------------------------------------
                   !           Problem   4
                   !-------------------------------------
                   CASE(4)
                     apu = ABS(y(1))
                     DO i = 2, user_n
                       IF (apu < ABS(y(i)) ) THEN 
                          apu = ABS(y(i))
                       END IF
                     END DO
                     f = user_n * apu               
                   !-------------------------------------
                   
                   !-------------------------------------
                   !           Problem   5
                   !-------------------------------------
                   CASE(5)
                     apu = ABS(summa(y,1)) 
                     DO j = 2, 20
                       IF (apu <= ABS(summa(y,j)) ) THEN 
                         apu = ABS(summa(y,j))
                       END IF
                     END DO
                     f = 20.0_dp * apu
                   !-------------------------------------
                   
                   !-------------------------------------
                   !           Problem   6
                   !-------------------------------------
                   CASE(6)
                     f = y(2) + 0.1_dp * (y(1)**2 + Y(2)**2)  

                     IF (-y(2) <= 0.0_dp ) THEN 
                         f = f
                     ELSE
                         f = f + 10.0_dp * (-y(2))
                     END IF
                   !-------------------------------------
                   
                   !-------------------------------------
                   !           Problem   7
                   !-------------------------------------
                   CASE(7)
                     f = ABS(y(1)-1.0_dp) + 200.0_dp * MAX(0.0_dp, ABS(y(1))-y(2) )                 
                
                     a1 = y(1)**2 + y(2)**2 + ABS(y(2))
                     a2 = y(1) + y(1)**2 + y(2)**2 + ABS(y(2)) - 0.5_dp
                     a3 = ABS( y(1) - y(2) ) + ABS(y(2)) - 1.0_dp
                     a4 = y(1) + y(1)**2 + y(2)**2
                
                     f = f + 10.0_dp * MAX(a1,a2,a3,a4)
                   !-------------------------------------
                   
                   !-------------------------------------
                   !           Problem   8
                   !-------------------------------------                    
                   CASE(8)
                     f = 9.0_dp - 8.0_dp * y(1) -6.0_dp * y(2) - 4.0_dp * y(3)  
                     f = f + 2.0_dp * ABS(y(1)) + 2.0_dp * ABS(y(2))+ 2.0_dp * ABS(y(3)) 
                     f = f + 4.0_dp * y(1)**2 + 2.0_dp * y(2)**2 + 2.0_dp * y(3)**2 

                     a(1) = y(1) + y(2) + 2.0_dp * y(3) - 3.0_dp
                     a(2) = -y(1)
                     a(3) = -y(2)
                     a(4) = -y(3)
                
                     f = f + 10.0_dp * MAX(0.0_dp, a(1), a(2), a(3), a(4))
                   !------------------------------------
                    
                   !-------------------------------------
                   !           Problem   9
                   !-------------------------------------                    
                   CASE(9)
                     f = y(1)**2 + (y(1)-1.0_dp)**2 + 2.0_dp*(y(1)-2.0_dp)**2 
                     f = f + (y(1)-3.0_dp)**2 + 2.0_dp * y(2)**2 + (y(2)-1.0_dp)**2 
                     f = f + 2.0_dp*(y(2)-2.0_dp)**2 + y(3)**2 + (y(3)-1.0_dp)**2 
                     f = f + 2.0_dp*(y(3)-2.0_dp)**2 + (y(3)-3.0_dp)**2 
                     f = f + 2.0_dp*y(4)**2 + (y(4)-1.0_dp)**2 + 2.0_dp*(y(4)-2.0_dp)**2                          
                   !------------------------------------
                               
                   !-------------------------------------
                   !           Problem   10
                   !-------------------------------------
                   CASE(10)
                     f = 0.0_dp
                     DO i = 1, user_n
                       f =  f + y(i)**2                             
                     END DO                    
                   !-------------------------------------

                   !-------------------------------------
                   !           Problem   11
                   !-------------------------------------
                   CASE(11)
                     f = 0.0_dp
                     f = f + 4.0_dp * ABS(y(1)) 
                     f = f + 2.0_dp * ABS(y(2)) 
                     f = f + 2.0_dp * ABS(y(3)) 
                     f = f - 33.0_dp*y(1) + 16.0_dp*y(2) - 24.0_dp*y(3)
                     f = f + 100.0_dp*MAX(0.0_dp, 2.0_dp*ABS(y(2))-3.0_dp*y(1)-7.0_dp)
                     f = f + 100.0_dp*MAX(0.0_dp, ABS(y(3))-4.0_dp*y(1)-11.0_dp)
                     f = f + 20.0_dp * Abs(y(3))
                   !-------------------------------------   
                   
                   !-------------------------------------
                   !           Problem   12
                   !-------------------------------------
                   CASE(12)                
                     f = 0.0_dp
                     
                     DO i = 1, user_n
                        f = f + ABS(y(i))
                     END DO
                     
                     DO i = 1, user_n
                        apu = 2.0_dp * (y(i)*y(i) -y(i) -1.0_dp)
                        IF (apu > 0.0_dp) THEN
                           f = f + 10.0_dp*apu
                        END IF
                     END DO
                 
                   !-------------------------------------   
                   
                   !-------------------------------------
                   !           Problem   13
                   !-------------------------------------
                   CASE(13)         
                     f = 0.0_dp
                     
                     DO i = 1, user_n-1
                        f = f + ABS(y(i)+y(i+1))
                     END DO
                     
                     DO i = 1, user_n-2
                        f = f + ABS(y(i)+y(i+2))
                     END DO
                     
                     f = f + ABS(y(1)+y(9)) 
                     f = f + ABS(y(1)+y(10)) 
                     f = f + ABS(y(2)+y(10)) 
                     f = f + ABS(y(1)+y(5)) 
                     f = f + ABS(y(4)+y(7)) 
                     
                     apu = 0.0_dp
                     DO i = 1, user_n
                        apu = apu + y(i)
                     END DO
                     
                     IF ((apu-1.0_dp)>0.0_dp) THEN 
                        f = f + 10.0_dp * (apu-1.0_dp)
                     END IF
                     
                     DO i = 1, user_n
                        IF (-y(i) > 0.0_dp) THEN 
                            f = f + 10.0_dp*(-y(i))
                        END IF
                     END DO
                   !-------------------------------------   

                   !-------------------------------------
                   !           Problem   14
                   !-------------------------------------
                   CASE(14)
                     f = 0.0_dp
                     
                     apu = 0.0_dp
                     DO j =1, user_n
                        apu = apu + y(j)/(1+j-1.0_dp)
                     END DO
                     largest = ABS(apu) 
                     
                     DO i = 2, user_n
                        apu = 0.0_dp
                        DO j = 1, user_n
                          apu = apu + y(j)/(i+j-1.0_dp)
                        END DO
                        IF (ABS(apu) > largest) THEN 
                           largest = ABS(apu)
                        END IF
                     END DO
                     
                     f = user_n * largest                    
                   !-------------------------------------   

                   !-------------------------------------
                   !           Problem   15
                   !-------------------------------------
                   CASE(15)
                     f = 0.0_dp
                     
                     DO i = 1, user_n-1
                         a(1) = y(i)**4 + y(i+1)**2
                         a(2) = (2.0_dp-y(i))**2 + (2.0_dp-y(i+1))**2
                         a(3) = 2.0_dp*EXP(-y(i)+y(i+1))
                         IF (a(1)>a(2)) THEN
                            IF (a(1)>a(3)) THEN
                              f_i(i) = a(1)
                            ELSE
                              f_i(i) = a(3)
                            END IF
                         ELSE
                            IF (a(2)>a(3)) THEN
                              f_i(i) = a(2)
                            ELSE
                              f_i(i) = a(3)
                            END IF      
                         END IF
                     END DO
                     
                     largest = f_i(1)
                     ind = 1
                     DO i =2, user_n-1
                         IF (f_i(i) > largest) THEN 
                            largest = f_i(i)
                            ind = i
                         END IF
                     END DO
                     
                     f = (user_n-1.0_dp)*largest
                   !-------------------------------------       

                   
                   !-------------------------------------
                   !           Problem 16
                   !-------------------------------------
                   CASE(16)
                     f = 0.0_dp
                     
                     apu = 0.0_dp
                     DO i = 1, user_n-1
                        apu = apu + y(i)**2 + (y(i+1)-1.0_dp)**2
                        apu = apu + y(i+1) -1.0_dp
                     END DO
                     
                    apu = 2.0_dp *apu
                    IF (apu > 0.0_dp) THEN
                       f = apu
                    END IF
                   !-------------------------------------
             
                   !-------------------------------------
                   !           Problem 17
                   !-------------------------------------
                   CASE(17)
                     f = 0.0_dp
                     f = 1.2_dp*Max(0.1_dp*y(1)**2+0.005_dp*y(2)**2,0.005_dp*y(1)**2+0.1_dp*y(2)**2)
                     
                   !-------------------------------------	
				   
                
                END SELECT
                
           END FUNCTION f1      
           
           !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           
           FUNCTION f2(y, problem2) RESULT(f)           
                !
                ! Calculates the function value of DC component f_2 at a point 'y'.
                ! Variable 'problem2' identifies the objective function used.
                !
                ! NOTICE: The dimension of 'y' has to be 'user_n'.
                !
                IMPLICIT NONE
                !**************************** NEEDED FROM USER *************************************
                REAL(KIND=dp), DIMENSION(:), INTENT(IN) :: y    ! a point where the function value of the DC component f_2 is calculated
                INTEGER, INTENT(IN) :: problem2                 ! the objective function f_2 for which the value is calculated               
                !**************************** OTHER VARIABLES **************************************
                REAL(KIND=dp) :: f                          ! the function value of the DC component f_2 at a point 'y'
                REAL(KIND=dp), DIMENSION(user_n-1) :: f_i   ! help variable
                REAL(KIND=dp), DIMENSION(user_n) :: term    ! help variable
                REAL(KIND=dp), DIMENSION(3) :: aa           ! help variable
                REAL(KIND=dp) :: apu, largest               ! help variables 
                REAL(KIND=dp) :: a, b                       ! help variables
                REAL(KIND=dp) :: a4, a5, a6                 ! help variables 
                INTEGER :: i, j, ind                        ! help variables
                

                
                SELECT CASE(problem2)
                
                 
                   !-------------------------------------
                   !           Problem   1
                   !-------------------------------------
                   CASE(1)
                     a4 = y(1)**2 - 2.0_dp * y(1) + y(2)**2 - 4.0_dp * y(2) + 4.0_dp
                     a5 = 2.0_dp * y(1)**2 - 5.0_dp * y(1) + y(2)**2 - 2.0_dp * y(2) 
                     a5 = a5 + 4.0_dp
                     a6 = y(1)**2 + 2.0_dp * y(2)**2 - 4.0_dp * y(2) + 1.0_dp
                
                     f = MAX( (a4 + a5) , (a4 + a6) , (a5 + a6) )                  

                   !-------------------------------------  
                   
                   !-------------------------------------
                   !           Problem   2
                   !-------------------------------------                  
                   CASE(2)
                     f = 100.0_dp * ( ABS(y(1)) - y(2) )
                   !-------------------------------------                   
                   
                   !-------------------------------------
                   !           Problem   3
                   !-------------------------------------
                   CASE(3)
                     f = 4.95_dp * ( ABS(y(2) - y(4)) )
                     f = f + 90.0_dp * ( ABS(y(3)) - y(4) )
                     f = f + 100.0_dp * ( ABS(y(1)) - y(2) )                       
                   !-------------------------------------
                   
                   !-------------------------------------
                   !           Problem   4
                   !-------------------------------------
                   CASE(4)
                     f = 0.0_dp
                     DO i = 1, user_n
                        f = f + ABS(y(i))                                   
                     END DO             
                   !-------------------------------------
                   
                   !-------------------------------------
                   !           Problem   5
                   !-------------------------------------
                   CASE(5)
                     f = 0.0_dp
                     DO j = 1, 20
                        f = f + ABS( summa(y,j) )
                     END DO 
                   !-------------------------------------                  
                   
                   !-------------------------------------
                   !           Problem   6
                   !-------------------------------------
                   CASE(6)
                     f = 0.0_dp
                
                     IF (y(1) <= 0.0_dp ) THEN 
                         f = f - y(1)
                     ELSE
                         f = f + y(1) 
                     END IF
                
                     IF (y(2) <= 0.0_dp ) THEN 
                         f = f - y(2)
                     ELSE
                         f = f + y(2)
                     END IF               
                   !-------------------------------------                   
                   
                   !-------------------------------------
                   !           Problem   7
                   !-------------------------------------
                   CASE(7)
                     f = 10.0_dp * ( y(1)**2 + y(2)**2 + ABS(y(2)) )
                     f = f + 100.0_dp * ( ABS(y(1)) - y(2) )
                   !-------------------------------------
                   
                   !-------------------------------------
                   !           Problem   8
                   !-------------------------------------                   
                   CASE(8)
                     f = ABS( y(1) -y(2) ) + ABS( y(1) -y(3) )
                   !------------------------------------
                    
                   !-------------------------------------
                   !           Problem   9
                   !-------------------------------------                    
                   CASE(9)
                     f = 0.0_dp 

                     a = (y(1) -2.0_dp)**2 + y(2)**2
                     b = (y(3) -2.0_dp)**2 + y(4)**2
                
                     IF ( a >= b) THEN 
                       f = f + a
                     ELSE 
                       f = f + b
                     END IF

                     a = (y(1) -2.0_dp)**2 + (y(2)-1.0_dp)**2
                     b = (y(3) -2.0_dp)**2 + (y(4)-1.0_dp)**2
                
                     IF ( a >= b) THEN 
                       f = f + a
                     ELSE 
                       f = f + b
                     END IF

                     a = (y(1) -3.0_dp)**2 + y(2)**2
                     b = (y(3) -3.0_dp)**2 + y(4)**2
                
                     IF ( a >= b) THEN 
                       f = f + a
                     ELSE 
                       f = f + b
                     END IF

                    a = (y(1))**2 + (y(2)-2.0_dp)**2
                     b = (y(3))**2 + (y(4)-2.0_dp)**2
                
                     IF ( a >= b) THEN 
                       f = f + a
                     ELSE 
                       f = f + b
                     END IF

                     a = (y(1)-1.0_dp)**2 + (y(2)-2.0_dp)**2
                     b = (y(3)-1.0_dp)**2 + (y(4)-2.0_dp)**2
                
                     IF ( a >= b) THEN 
                       f = f + a
                     ELSE 
                       f = f + b
                     END IF                  
                   !------------------------------------  
                   
                   !-------------------------------------
                   !           Problem   10
                   !-------------------------------------
                   CASE(10)
                     f = 0.0_dp
                     DO i = 2, user_n
                         f = f + ABS( y(i) - y(i-1) ) 
                     END DO
                   !-------------------------------------
                   
                   !-------------------------------------
                   !           Problem   11
                   !-------------------------------------
                   CASE(11)
                     apu = - 7.0_dp * y(1) 
                     apu = apu + 2.0_dp * ABS(y(2)) - 18.0_dp
                     f = 20.0_dp * apu
                   !-------------------------------------
                   
                   !-------------------------------------
                   !           Problem   12
                   !-------------------------------------
                   CASE(12)                
                     f = 0.0_dp

                     DO i = 1, user_n
                        apu = (y(i)*y(i) -y(i) -1.0_dp)
                        f = f + 10.0_dp * apu
                     END DO
                     
                     term = 0.0_dp
                     DO i = 1, user_n
                        DO j = 1, user_n 
                           IF (i /= j) THEN
                               term(i) = term(i) + ABS(y(j))
                           END IF
                        END DO
                     END DO
                     
                     largest = term(1)
                     ind = 1 
                     
                     DO i = 2, user_n
                        IF (term(i) > largest) THEN 
                           largest = term(i)
                           ind = i
                        END IF
                     END DO
                     
                     f = f + largest
                   !-------------------------------------   
                   
                   !-------------------------------------
                   !           Problem   13
                   !-------------------------------------
                   CASE(13)         
                     f = 0.0_dp
                     
                     DO i = 1, user_n-1
                        f  = f + ABS(y(i)) + ABS(y(i+1))
                     END DO
                     
                     DO i = 1, user_n-2
                        f  = f + ABS(y(i)) + ABS(y(i+2))
                     END DO                  
                     
                     f = f + 3.0_dp*ABS(y(1))
                     f = f + ABS(y(2))
                     f = f + ABS(y(4))
                     f = f + ABS(y(5))
                     f = f + ABS(y(7))
                     f = f + ABS(y(9))
                     f = f + 2.0_dp * ABS(y(10))                     
                   !-------------------------------------                   
                   
                   !-------------------------------------
                   !           Problem   14
                   !-------------------------------------
                   CASE(14)
                     f = 0.0_dp
                     
                     DO i = 1, user_n
                        apu = 0.0_dp
                        DO j = 1, user_n
                          apu = apu + y(j)/(i+j-1.0_dp)
                        END DO
                        f = f + ABS(apu)
                     END DO
                     
                   !-------------------------------------   

                   !-------------------------------------
                   !           Problem   15
                   !-------------------------------------
                   CASE(15)
                     f = 0.0_dp
                     
                     DO i = 1, user_n-1
                         aa(1) = y(i)**4 + y(i+1)**2
                         aa(2) = (2.0_dp-y(i))**2 + (2.0_dp-y(i+1))**2
                         aa(3) = 2.0_dp*EXP(-y(i)+y(i+1))
                         IF (aa(1)>aa(2)) THEN
                            IF (aa(1)>aa(3)) THEN
                              f_i(i) = aa(1)
                            ELSE
                              f_i(i) = aa(3)
                            END IF
                         ELSE
                            IF (aa(2)>aa(3)) THEN
                              f_i(i) = aa(2)
                            ELSE
                              f_i(i) = aa(3)
                            END IF      
                         END IF
                     END DO

                     DO i =1, user_n-1
                         f = f + f_i(i)
                   END DO               
                   !-------------------------------------
                   
                   !-------------------------------------
                   !           Problem   16         
                   !-------------------------------------
                   CASE(16)
                     f = 0.0_dp

                     DO i = 1, user_n-1
                        f = f + y(i)**2 + (y(i+1)-1.0_dp)**2
                        f = f + y(i+1) -1.0_dp
                     END DO
                   !-------------------------------------           

                   !-------------------------------------
                   !           Problem   17         
                   !-------------------------------------
                   CASE(17)
                     f = 0.0_dp
                     f = Max(-y(1),-0.3_dp*y(2),0.0_dp) 

  				   !-------------------------------------   
 
                END SELECT              
            


           END FUNCTION f2
           
           !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           

        !********************************************************************************
        !                                                                               |
        !                SUBGRADIENTS OF THE DC COMPONENTS f_1 AND f_2                  |
        !                                                                               |       
        !********************************************************************************       
        
           FUNCTION subgradient_f1(y, problem1) RESULT(grad)
                !
                ! Calculates a subgradient of the DC component f_1 at a point 'y'.
                ! Variable 'problem1' identifies the objective function used.
                !
                ! NOTICE: * The dimension of 'y' has to be 'user_n'.
                !         * The dimension of 'grad' is 'user_n'.
                !
                IMPLICIT NONE
                !**************************** NEEDED FROM USER *************************************
                REAL(KIND=dp), DIMENSION(:), INTENT(IN) :: y    ! a point where the subgradient of the DC component f_1 is calculated
                INTEGER, INTENT(IN) :: problem1                 ! the objective function f_1 for which the subgradient is calculated                
                !**************************** OTHER VARIABLES **************************************
                REAL(KIND=dp), DIMENSION(SIZE(y)) :: grad       ! the subgradient of the DC component f_1 at a point 'y'
                REAL(KIND=dp), DIMENSION(user_n) :: abs_sign    ! help varaible
                REAL(KIND=dp), DIMENSION(user_n) :: f_i         ! help varaible (only first n-1 placs are used)             
                REAL(KIND=dp), DIMENSION(4) :: a                ! help variable
                REAL(KIND=dp), DIMENSION(5) :: b                ! help variable
                REAL(KIND=dp) :: a1, a2, a3                     ! help variables
                REAL(KIND=dp) :: apu, largest                   ! help varaibles
                INTEGER :: i, j, ind                            ! help variables
                

                SELECT CASE(problem1)
                

                   !-------------------------------------
                   !           Problem   1
                   !-------------------------------------                    
                   CASE(1)              
                     a1 = y(1)**4 + y(2)**2
                     a2 = (2.0_dp - y(1))**2 + (2.0_dp - y(2))**2 
                     a3 = 2.0_dp * EXP(-y(1)+y(2))              
                
                     IF (a1 >= a2) THEN
                       IF (a1 >= a3) THEN
                         grad(1) = 4.0_dp * y(1)**3 
                         grad(2) = 2.0_dp * y(2)                    
                       ELSE
                         grad(1) = -2.0_dp * EXP(-y(1)+y(2))
                         grad(2) = 2.0_dp * EXP(-y(1)+y(2))                 
                       END IF               
                     ELSE
                       IF (a2 >= a3) THEN
                         grad(1) = 2.0_dp * y(1) - 4.0_dp
                         grad(2) = 2.0_dp * y(2) - 4.0_dp                   
                       ELSE
                         grad(1) = -2.0_dp * EXP(-y(1)+y(2))
                         grad(2) = 2.0_dp * EXP(-y(1)+y(2))                 
                       END IF                   
                     END IF
                
                     grad(1) = grad(1) + 2.0_dp * y(1) - 2.0_dp
                     grad(1) = grad(1) + 4.0_dp * y(1) - 5.0_dp
                     grad(1) = grad(1) + 2.0_dp * y(1)
  
                     grad(2) = grad(2) + 2.0_dp * y(2) - 4.0_dp
                     grad(2) = grad(2) + 2.0_dp * y(2) - 2.0_dp
                     grad(2) = grad(2) + 4.0_dp * y(2) - 4.0_dp                

                   !-------------------------------------      
                   
                   !-------------------------------------
                   !           Problem   2
                   !-------------------------------------                    
                   CASE(2)              
                     IF ( y(1) <= 1.0_dp ) THEN
                        grad(1) = -1.0_dp
                     ELSE
                        grad(1) = 1.0_dp
                     END IF
                
                     IF( ( ABS(y(1))-y(2) ) > 0.0_dp  ) THEN
                        IF(y(1) <= 0.0_dp) THEN 
                          grad(1) = grad(1) - 200.0_dp
                        ELSE
                          grad(1) = grad(1) + 200.0_dp  
                        END IF                   
                        grad(2) = -200.0_dp
                     ELSE        
                        grad(1) = grad(1) 
                        grad(2) = 0.0_dp
                     END IF
                   !-------------------------------------                   

                   !-------------------------------------
                   !           Problem   3
                   !-------------------------------------  
                   CASE(3)
                     grad(1) = 0.0_dp
                     grad(2) = 0.0_dp
                     grad(3) = 0.0_dp
                     grad(4) = 0.0_dp
                
                     IF ( y(1) <= 1.0_dp ) THEN
                       grad(1) = -1.0_dp
                     ELSE
                       grad(1) = 1.0_dp
                     END IF
                
                     IF( ( ABS(y(1))-y(2) ) > 0.0_dp  ) THEN
                       IF(y(1) <= 0.0_dp) THEN 
                          grad(1) = grad(1) - 200.0_dp
                       ELSE
                          grad(1) = grad(1) + 200.0_dp  
                       END IF                    
                       grad(2) = -200.0_dp
                     ELSE        
                       grad(1) = grad(1) 
                       grad(2) = 0.0_dp
                     END IF

                     IF( ( ABS(y(3))-y(4) ) > 0.0_dp  ) THEN
                       IF(y(3) <= 0.0_dp) THEN 
                          grad(3) =  - 180.0_dp
                       ELSE
                          grad(3) =  180.0_dp  
                       END IF                    
                       grad(4) = -180.0_dp
                     ELSE        
                       grad(3) = 0.0_dp 
                       grad(4) = 0.0_dp
                     END IF
                
                     IF ( y(3) <= 1.0_dp ) THEN
                       grad(3) = grad(3) - 1.0_dp
                     ELSE
                       grad(3) = grad(3) + 1.0_dp
                     END IF         
                
                     IF ( y(2) <= 1.0_dp ) THEN
                       grad(2) = grad(2) - 10.1_dp  
                     ELSE
                       grad(2) = grad(2) + 10.1_dp
                     END IF         
                
                     IF ( y(4) <= 1.0_dp ) THEN
                       grad(4) = grad(4) - 10.1_dp  
                     ELSE
                       grad(4) = grad(4) + 10.1_dp
                     END IF 
                
                     IF ( (y(2) + y(4) - 2.0_dp) <= 0.0_dp ) THEN
                       grad(2) = grad(2) - 4.95_dp
                       grad(4) = grad(4) - 4.95_dp  
                     ELSE
                       grad(2) = grad(2) + 4.95_dp
                       grad(4) = grad(4) + 4.95_dp
                     END IF 
                   !-------------------------------------
                   
                   !-------------------------------------
                   !           Problem   4
                   !-------------------------------------  
                   CASE(4)
                     grad = 0.0_dp 
                     apu = ABS(y(1))
                     ind = 1
                
                     DO i = 2, user_n
                       IF (apu < ABS(y(i))) THEN 
                         apu = ABS(y(i))
                         ind = i
                       END IF
                     END DO
                
                     IF (y(ind) <= 0.0_dp) THEN 
                         grad(ind) = - user_n
                     ELSE 
                         grad(ind) = user_n
                    END IF                   
                   !-------------------------------------
                   
                   !-------------------------------------
                   !           Problem   5
                   !-------------------------------------  
                   CASE(5)
                     grad = 0.0_dp 
                     apu = ABS(summa(y,1)) 
                     ind = 1

                     DO j = 2, 20
                       IF (apu <= ABS(summa(y,j)) ) THEN 
                         apu = ABS(summa(y,j))
                         ind = j
                       END IF
                     END DO

                     DO i = 1, user_n
                       grad(i) = (0.05_dp * ind)**(i-1)
                     END DO             
                
                     IF ( summa(y,ind) <= 0.0_dp ) THEN 
                         grad = -20.0_dp * grad
                     ELSE
                         grad = 20.0_dp * grad
                     END IF 
                   !-------------------------------------
                   
                   !-------------------------------------
                   !           Problem   6
                   !-------------------------------------  
                   CASE(6)
                     grad = 0.0_dp 
                
                     grad(1) = 0.2_dp * y(1)
                
                     grad(2) = 1.0_dp + 0.2_dp * y(2) 

                     IF ( -y(2) <= 0.0_dp ) THEN 
                         grad(2) = grad(2)
                     ELSE
                         grad(2) = grad(2) - 10.0_dp
                     END IF                  
                   !-------------------------------------
                   
                   !-------------------------------------
                   !           Problem   7
                   !-------------------------------------  
                   CASE(7)
                     grad = 0.0_dp 
                
                     IF ( (y(1)-1.0_dp) <= 0.0_dp ) THEN 
                       grad(1) = -1.0_dp
                     ELSE
                       grad(1) = 1.0_dp
                     END IF 

                     a(1) = y(1)**2 + y(2)**2 + ABS(y(2))
                     a(2) = y(1) + y(1)**2 + y(2)**2 + ABS(y(2)) - 0.5_dp
                     a(3) = ABS( y(1) - y(2) ) + ABS(y(2)) - 1.0_dp
                     a(4) = y(1) + y(1)**2 + y(2)**2                
                
                     ind = 1
                     DO i = 2, 4
                       IF ( a(ind) < a(i) ) THEN 
                         ind = i
                       END IF 
                     END DO
                
                     IF (ind == 1) THEN
                       grad(1) = grad(1) + 20.0_dp * y(1)
                       grad(2) = grad(2) + 20.0_dp * y(2)
                       IF (y(2) <= 0.0_dp) THEN
                          grad(2) = grad(2) - 10.0_dp
                       ELSE
                          grad(2) = grad(2) + 10.0_dp
                       END IF
                     END IF
                
                     IF (ind == 2) THEN
                       grad(1) = grad(1) + 10.0_dp + 20.0_dp * y(1)
                       grad(2) = grad(2) + 20.0_dp * y(2)
                       IF (y(2) <= 0.0_dp) THEN
                          grad(2) = grad(2) - 10.0_dp
                       ELSE
                          grad(2) = grad(2) + 10.0_dp
                       END IF
                     END IF 
                
                     IF (ind == 3) THEN
                        IF ( ( y(1) - y(2) ) <= 0.0_dp ) THEN
                          grad(1) = grad(1) - 10.0_dp 
                          grad(2) = grad(2) + 10.0_dp
                        ELSE
                          grad(1) = grad(1) + 10.0_dp
                          grad(2) = grad(2) - 10.0_dp                   
                        END IF
                    
                        IF (y(2) <= 0.0_dp) THEN
                          grad(2) = grad(2) - 10.0_dp                   
                        ELSE
                          grad(2) = grad(2) +   10.0_dp                 
                        END IF
                     END IF 
                
                     IF (ind == 4) THEN
                       grad(1) = grad(1) + 10.0_dp + 20.0_dp * y(1)
                       grad(2) = grad(2) + 20.0_dp * y(2)
                     END IF
                
                     IF ( (ABS(y(1)) - y(2) ) >= 0.0_dp ) THEN
                       grad(2) = grad(2) - 200.0_dp
                       IF ( y(1) <= 0.0_dp ) THEN
                         grad(1) = grad(1) - 200.0_dp
                       ELSE
                         grad(1) = grad(1) + 200.0_dp
                       END IF 
                     END IF
                   !-------------------------------------
                   
                   !-------------------------------------
                   !           Problem   8
                   !-------------------------------------                      
                   CASE(8)
                     grad(1) = -8.0_dp + 8.0_dp * y(1)
                     grad(2) = -6.0_dp + 4.0_dp * y(2)
                     grad(3) = -4.0_dp + 4.0_dp * y(3)

                     IF ( y(1) <= 0.0_dp ) THEN 
                         grad(1) = grad(1) - 2.0_dp
                     ELSE
                         grad(1) = grad(1) + 2.0_dp
                     END IF                 
                
                     IF ( y(2) <= 0.0_dp ) THEN 
                         grad(2) = grad(2) - 2.0_dp
                     ELSE
                         grad(2) = grad(2) + 2.0_dp
                     END IF 

                     IF ( y(3) <= 0.0_dp ) THEN 
                         grad(3) = grad(3) - 2.0_dp
                     ELSE
                         grad(3) = grad(3) + 2.0_dp
                     END IF                 
                
                     b(1) = y(1) + y(2) + 2.0_dp * y(3) - 3.0_dp
                     b(2) = -y(1)
                     b(3) = -y(2)
                     b(4) = -y(3)
                     b(5) = 0.0_dp
                
                     ind = 5
                
                     DO i = 1, 4
                       IF ( b(ind) < b(i) ) THEN 
                         ind = i
                       END IF 
                     END DO
                
                     IF (ind == 1) THEN
                       grad(1) = grad(1) + 10.0_dp 
                       grad(2) = grad(2) + 10.0_dp 
                       grad(3) = grad(3) + 20.0_dp 
                     END IF
                
                     IF (ind == 2) THEN
                       grad(1) = grad(1) - 10.0_dp 
                     END IF 
                
                     IF (ind == 3) THEN
                       grad(2) = grad(2) - 10.0_dp 
                     END IF 
                
                     IF (ind == 4) THEN
                       grad(3) = grad(3) - 10.0_dp 
                     END IF
                   !-------------------------------------

                   !-------------------------------------
                   !           Problem   9
                   !-------------------------------------                       
                   CASE(9)
                     grad(1) = 10.0_dp * y(1) - 16.0_dp
                     grad(2) = 10.0_dp * y(2) - 10.0_dp 
                     grad(3) = 10.0_dp * y(3) - 16.0_dp
                     grad(4) = 10.0_dp * y(4) - 10.0_dp                 
                   !------------------------------------
                    
                   !-------------------------------------
                   !           Problem   10
                   !-------------------------------------  
                   CASE(10)
                      grad = 2.0_dp * y                   
                   !------------------------------------- 

                   !-------------------------------------
                   !           Problem   11
                   !-------------------------------------
                   CASE(11)
                     grad = 0.0_dp
                     
                     IF (y(1)> 0.0_dp) THEN
                        grad(1) = grad(1) + 4.0_dp
                     ELSE
                        grad(1) = grad(1) -4.0_dp
                     END IF
                     
                     IF (y(2)> 0.0_dp) THEN
                        grad(2) = grad(2) + 2.0_dp
                     ELSE
                        grad(2) = grad(2) -2.0_dp
                     END IF

                     IF (y(3)> 0.0_dp) THEN
                        grad(3) = grad(3) + 2.0_dp
                     ELSE
                        grad(3) = grad(3) -2.0_dp
                     END IF                  
                     
                     grad(1) = grad(1) -33.0_dp 
                     grad(2) = grad(2) +16.0_dp 
                     grad(3) = grad(3) -24.0_dp 

                     apu = 2.0_dp*ABS(y(2))-3.0_dp*y(1)-7.0_dp
                     IF (apu > 0.0_dp) THEN 
                        grad(1) = grad(1) -300.0_dp
                        IF (y(2)>0.0_dp) THEN 
                           grad(2) = grad(2) +200.0_dp
                        ELSE
                           grad(2) = grad(2) -200.0_dp
                        END IF 
                     END IF
                     
                     apu = ABS(y(3))-4.0_dp*y(1)-11.0_dp
                     IF (apu > 0.0_dp) THEN 
                        grad(1) = grad(1) -400.0_dp
                        IF (y(3)>0.0_dp) THEN 
                           grad(3) = grad(3) +100.0_dp
                        ELSE
                           grad(3) = grad(3) -100.0_dp
                        END IF 
                     END IF  

                     IF (y(3)>0.0_dp) THEN 
                        grad(3) = grad(3)+20.0_dp
                     ELSE
                        grad(3) = grad(3)-20.0_dp
                     END IF  					 
                   !-------------------------------------   

                   !-------------------------------------
                   !           Problem   12
                   !-------------------------------------
                   CASE(12)                
                     grad = 0.0_dp
                     
                     DO i = 1, user_n
                        IF (y(i) > 0.0_dp) THEN 
                           grad(i) = grad(i) +1.0_dp
                        ELSE
                           grad(i) = grad(i) -1.0_dp
                        END IF               
                     END DO                  

                     DO i = 1, user_n
                         apu = 2.0_dp * (y(i)*y(i)- y(i)-1.0_dp) 
                         IF (apu > 0.0_dp) THEN 
                            grad(i) = grad(i) + 40.0_dp * y(i) -20.0_dp
                         END IF
                     END DO
                   !-------------------------------------   
                   
                   !-------------------------------------
                   !           Problem   13
                   !-------------------------------------
                   CASE(13)         
                     grad = 0.0_dp
                     
                     DO i = 1, user_n-1
                        IF ( (y(i)+y(i+1)) > 0.0_dp) THEN 
                           grad(i) = grad(i) + 1.0_dp
                           grad(i+1) = grad(i+1) + 1.0_dp
                        ELSE
                           grad(i) = grad(i) - 1.0_dp
                           grad(i+1) = grad(i+1) - 1.0_dp
                        END IF
                     END DO
                     
                     DO i = 1, user_n-2
                        IF ( (y(i)+y(i+2)) > 0.0_dp) THEN 
                           grad(i) = grad(i) + 1.0_dp
                           grad(i+2) = grad(i+2) + 1.0_dp
                        ELSE
                           grad(i) = grad(i) - 1.0_dp
                           grad(i+2) = grad(i+2) - 1.0_dp
                        END IF
                     END DO
                     
                     IF ( (y(1)+y(9)) > 0.0_dp) THEN 
                        grad(1) = grad(1) + 1.0_dp
                        grad(9) = grad(9) + 1.0_dp
                     ELSE
                        grad(1) = grad(1) - 1.0_dp
                        grad(9) = grad(9) - 1.0_dp
                     END IF 
                     
                     IF ( (y(1)+y(10)) > 0.0_dp) THEN 
                        grad(1) = grad(1) + 1.0_dp
                        grad(10) = grad(10) + 1.0_dp
                     ELSE
                        grad(1) = grad(1) - 1.0_dp
                        grad(10) = grad(10) - 1.0_dp
                     END IF 
                     
                     IF ( (y(2)+y(10)) > 0.0_dp) THEN 
                        grad(2) = grad(2) + 1.0_dp
                        grad(10) = grad(10) + 1.0_dp
                     ELSE
                        grad(2) = grad(2) - 1.0_dp
                        grad(10) = grad(10) - 1.0_dp
                     END IF 
                     
                     IF ( (y(1)+y(5)) > 0.0_dp) THEN 
                        grad(1) = grad(1) + 1.0_dp
                        grad(5) = grad(5) + 1.0_dp
                     ELSE
                        grad(1) = grad(1) - 1.0_dp
                        grad(5) = grad(5) - 1.0_dp
                     END IF 
                     
                     IF ( (y(4)+y(7)) > 0.0_dp) THEN 
                        grad(4) = grad(4) + 1.0_dp
                        grad(7) = grad(7) + 1.0_dp
                     ELSE
                        grad(4) = grad(4) - 1.0_dp
                        grad(7) = grad(7) - 1.0_dp
                     END IF                      
                 
                     apu = 0.0_dp
                     DO i = 1, user_n
                        apu = apu + y(i)
                     END DO
                     
                     IF ((apu-1.0_dp)>0.0_dp) THEN 
                        grad = grad + 10.0_dp 
                     END IF
                     
                     DO i = 1, user_n
                        IF (-y(i) > 0.0_dp) THEN 
                            grad(i) = grad(i) - 10.0_dp
                        END IF
                     END DO
                   !-------------------------------------   
                   
                   !-------------------------------------
                   !           Problem   14
                   !-------------------------------------
                   CASE(14)
                     grad = 0.0_dp
                   
                     apu = 0.0_dp
                     DO j =1, user_n
                        apu = apu + y(j)/(1+j-1.0_dp)
                     END DO
                     IF (apu >= 0.0_dp) THEN 
                           abs_sign(1) = 1.0_dp
                     ELSE   
                           abs_sign(1) = -1.0_dp
                     END IF        
                     largest = ABS(apu) 
                     ind = 1
                     
                     DO i = 2, user_n
                        apu = 0.0_dp
                        DO j = 1, user_n
                          apu = apu + y(j)/(i+j-1.0_dp)
                        END DO
                        IF (apu >= 0.0_dp) THEN 
                           abs_sign(i) = 1.0_dp
                        ELSE   
                           abs_sign(i) = -1.0_dp
                        END IF
                        IF (ABS(apu) > largest) THEN 
                            largest = ABS(apu)
                            ind = i
                        END IF
                     END DO
                     
                     DO j = 1, user_n
                        grad(j) = abs_sign(ind) * user_n / (ind+j-1)
                     END DO                                          
                   !-------------------------------------       

                   !-------------------------------------
                   !           Problem   15
                   !-------------------------------------
                   CASE(15)
                     grad = 0.0_dp
                     
                     DO i = 1, user_n-1
                         a(1) = y(i)**4 + y(i+1)**2
                         a(2) = (2.0_dp-y(i))**2 + (2.0_dp-y(i+1))**2
                         a(3) = 2.0_dp*EXP(-y(i)+y(i+1))
                         IF (a(1)>a(2)) THEN
                            IF (a(1)>a(3)) THEN
                              f_i(i) = a(1)
                            ELSE
                              f_i(i) = a(3)
                            END IF
                         ELSE
                            IF (a(2)>a(3)) THEN
                              f_i(i) = a(2)
                            ELSE
                              f_i(i) = a(3)
                            END IF      
                         END IF
                     END DO
                     
                     largest = f_i(1)
                     ind = 1
                     DO i =2, user_n-1
                         IF (f_i(i) > largest) THEN 
                            largest = f_i(i)
                            ind = i
                         END IF
                     END DO
                     
                         a(1) = y(ind)**4 + y(ind+1)**2
                         a(2) = (2.0_dp-y(ind))**2 + (2.0_dp-y(ind+1))**2
                         a(3) = 2.0_dp*EXP(-y(ind)+y(ind+1))
                         IF (a(1)>a(2)) THEN
                            IF (a(1)>a(3)) THEN
                              grad(ind) = 4.0_dp*y(ind)**3 
                              grad(ind+1) = 2.0_dp*y(ind+1)
                            ELSE
                              grad(ind) = -2.0_dp * EXP(-y(ind)+y(ind+1)) 
                              grad(ind+1) = 2.0_dp * EXP(-y(ind)+y(ind+1)) 
                            END IF
                         ELSE
                            IF (a(2)>a(3)) THEN
                              grad(ind) = -2.0_dp * (2.0_dp -y(ind))
                              grad(ind+1) = -2.0_dp * (2.0_dp -y(ind+1))
                            ELSE
                              grad(ind) = -2.0_dp * EXP(-y(ind)+y(ind+1)) 
                              grad(ind+1) = 2.0_dp * EXP(-y(ind)+y(ind+1)) 
                            END IF      
                         END IF                  
                     
                     grad = (user_n-1.0_dp)*grad
                   !-------------------------------------
                   
                   !-------------------------------------
                   !           Problem   16
                   !-------------------------------------
                   CASE(16)
                     grad = 0.0_dp
                     
                     apu = 0.0_dp
                     DO i = 1, user_n-1
                        apu = apu + y(i)**2 + (y(i+1)-1.0_dp)**2
                        apu = apu + y(i+1) -1.0_dp
                     END DO
                     
                    apu = 2.0_dp *apu
                    IF (apu > 0.0_dp) THEN
                       DO i = 1, user_n-1
                          grad(i) = grad(i) + 4.0_dp *y(i)
                          grad(i+1) = grad(i+1) + 4.0_dp *(y(i+1)-1.0_dp) +2.0_dp
                       END DO
                    END IF
                   !-------------------------------------
  
                   !-------------------------------------
                   !           Problem 17
                   !-------------------------------------
                   CASE(17)
                     grad = 0.0_dp
					 a1 = 0.1_dp * y(1)**2 + 0.005_dp * y(2)**2
					 a2 = 0.005_dp * y(1)**2 + 0.1_dp * y(2)**2
                     IF ( a1 < a2 ) THEN
					    grad(1) = 1.2_dp*(0.01_dp*y(1))
					    grad(2) = 1.2_dp*(0.2_dp*y(2))
					 ELSE
					    grad(1) = 1.2_dp*(0.2_dp*y(1))
					    grad(2) = 1.2_dp*(0.01_dp*y(2))					 
					 END IF

                   !-------------------------------------                    
 
                END SELECT                      
                
           END FUNCTION subgradient_f1      
           
           !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
           
           FUNCTION subgradient_f2(y, problem2) RESULT(grad)                
                !
                ! Calculate a subgradient of the DC component f_2 at a point 'y'.
                ! Variable 'problem1' identifies the objective function used.
                !
                ! NOTICE: * The dimension of 'y' has to be 'user_n'.
                !         * The dimension of 'grad' is also 'user_n'.
                !
                IMPLICIT NONE
                !**************************** NEEDED FROM USER *************************************
                REAL(KIND=dp), DIMENSION(:), INTENT(IN) :: y    ! a point where the subgradient of the DC component f_2 is calculated
                INTEGER, INTENT(IN) :: problem2                 ! the objective function f_2 for which the subgradient is calculated
                !**************************** OTHER VARIABLES **************************************
                REAL(KIND=dp), DIMENSION(SIZE(y)) :: grad       ! the subgradient of the DC component f_2 at a point 'y'
                REAL(KIND=dp), DIMENSION(user_n) :: term        ! help variable     
                REAL(KIND=dp), DIMENSION(3) :: aa               ! help variable   
                REAL(KIND=dp) :: a,b                            ! help variables
                REAL(KIND=dp) :: a4, a5, a6                     ! help varibales
                REAL(KIND=dp) :: apu                            ! help varaible
                REAL(KIND=dp) :: largest, abs_sign              ! help varaibles
                INTEGER :: i, j, ind                            ! help variables
                
               
                SELECT CASE(problem2)
                
                   
                   !-------------------------------------
                   !           Problem   1
                   !-------------------------------------                    
                   CASE(1)              
                     grad = 0.0_dp
                
                     a4 = y(1)**2 - 2.0_dp * y(1) + y(2)**2 - 4.0_dp * y(2) + 4.0_dp
                     a5 = 2.0_dp * y(1)**2 - 5.0_dp * y(1) + y(2)**2 - 2.0_dp * y(2) 
                     a5 = a5 + 4.0_dp
                     a6 = y(1)**2 + 2.0_dp * y(2)**2 - 4.0_dp * y(2) + 1.0_dp               
                
                     IF ( (a4 + a5) >= (a4 + a6) ) THEN
                        IF ( (a4 + a5) >= (a5 + a6)) THEN
                          grad(1) = 6.0_dp * y(1) - 7.0_dp
                          grad(2) = 4.0_dp * y(2) - 6.0_dp
                        ELSE
                          grad(1) = 6.0_dp * y(1) - 5.0_dp
                          grad(2) = 6.0_dp * y(2) - 6.0_dp
                        END IF              
                     ELSE
                        IF ( (a4 + a6) >= (a5 + a6) ) THEN
                          grad(1) = 4.0_dp * y(1) - 2.0_dp
                          grad(2) = 6.0_dp * y(2) - 8.0_dp
                        ELSE
                          grad(1) = 6.0_dp * y(1) - 5.0_dp
                          grad(2) = 6.0_dp * y(2) - 6.0_dp 
                        END IF                  
                     END IF
                   !------------------------------------- 
                   
                   !-------------------------------------
                   !           Problem   2
                   !-------------------------------------                    
                   CASE(2)              
                     IF(y(1) <= 0) THEN
                       grad(1) = -100.0_dp 
                     ELSE
                       grad(1) = 100.0_dp
                     END IF
                
                     grad(2) = -100.0_dp
                   !-------------------------------------                   
                   
                   !-------------------------------------
                   !           Problem   3
                   !-------------------------------------  
                   CASE(3)
                     IF(y(1) <= 0.0_dp) THEN
                       grad(1) = -100.0_dp 
                     ELSE
                       grad(1) = 100.0_dp
                     END IF
                
                     grad(2) = -100.0_dp
                
                     IF(y(3) <= 0.0_dp) THEN
                       grad(3) = -90.0_dp 
                     ELSE
                       grad(3) = 90.0_dp
                     END IF
                
                     grad(4) = -90.0_dp             
                
                     IF( (y(2) - y(4) ) <= 0.0_dp) THEN
                       grad(2) = grad(2) - 4.95_dp 
                       grad(4) = grad(4) + 4.95_dp
                     ELSE
                       grad(2) = grad(2) + 4.95_dp 
                       grad(4) = grad(4) - 4.95_dp                 
                     END IF                
                   !-------------------------------------
                   
                   !-------------------------------------
                   !           Problem   4
                   !-------------------------------------  
                   CASE(4)
                     DO i = 1, user_n
                       IF (y(i) <= 0.0_dp ) THEN 
                         grad(i) = -1.0_dp
                       ELSE
                         grad(i) = 1.0_dp
                       END IF
                     END DO
                   !-------------------------------------
                    
                   !-------------------------------------
                   !           Problem   5
                   !-------------------------------------  
                   CASE(5)
                     grad = 0.0_dp 

                     DO j = 1, 20
                      IF (summa(y,j) <= 0.0_dp) THEN 
                        DO i = 1, user_n
                            grad(i) = grad(i) - (0.05_dp * j)**(i-1)
                        END DO                  
                      ELSE 
                        DO i = 1, user_n
                            grad(i) = grad(i) + (0.05_dp * j)**(i-1)
                        END DO                   
                      END IF
                     END DO                   
                   !-------------------------------------
                   
                   !-------------------------------------
                   !           Problem   6
                   !-------------------------------------  
                   CASE(6)
                     DO i = 1, user_n
                       IF (y(i) <= 0.0_dp ) THEN 
                         grad(i) = -1.0_dp
                       ELSE
                         grad(i) = 1.0_dp
                       END IF
                     END DO                 
                   !-------------------------------------
                   
                   !-------------------------------------
                   !           Problem   7
                   !-------------------------------------  
                   CASE(7)
                     grad = 0.0_dp
                
                     IF (y(2) <= 0.0_dp ) THEN 
                         grad(2) = 20.0_dp * y(2) - 10.0_dp 
                     ELSE
                         grad(2) = 20.0_dp * y(2) + 10.0_dp 
                     END IF
                
                     grad(2) = grad(2) - 100.0_dp
                     grad(1) = 20.0_dp * y(1)

                     IF (y(1) <= 0.0_dp ) THEN 
                         grad(1) = grad(1) - 100.0_dp
                     ELSE
                         grad(1) = grad(1) + 100.0_dp
                     END IF
                   !-------------------------------------
                   
                   !-------------------------------------
                   !           Problem   8
                   !-------------------------------------                    
                   CASE(8)
                     grad = 0.0_dp
                
                     IF ( (y(1)-y(2)) <= 0.0_dp ) THEN 
                         grad(1) = grad(1) - 1.0_dp 
                         grad(2) = grad(2) + 1.0_dp
                     ELSE
                         grad(1) = grad(1) + 1.0_dp
                         grad(2) = grad(2) - 1.0_dp 
                     END IF
                
                     IF ( (y(1)-y(3)) <= 0.0_dp ) THEN 
                         grad(1) = grad(1) - 1.0_dp 
                         grad(3) = grad(3) + 1.0_dp
                     ELSE
                         grad(1) = grad(1) + 1.0_dp
                         grad(3) = grad(3) - 1.0_dp     
                     END IF 
                   !-------------------------------------
                   
                   !-------------------------------------
                   !           Problem   9
                   !-------------------------------------                     
                   CASE(9)
                     grad = 0.0_dp 

                     a = (y(1) -2.0_dp)**2 + y(2)**2
                     b = (y(3) -2.0_dp)**2 + y(4)**2
                
                     IF ( a >= b) THEN 
                       grad(1) = grad(1) + 2.0_dp * (y(1)-2.0_dp)
                       grad(2) = grad(2) + 2.0_dp * y(2)
                     ELSE 
                       grad(3) = grad(3) + 2.0_dp * (y(3)-2.0_dp)
                       grad(4) = grad(4) + 2.0_dp * y(4)
                     END IF

                     a = (y(1) -2.0_dp)**2 + (y(2)-1.0_dp)**2
                     b = (y(3) -2.0_dp)**2 + (y(4)-1.0_dp)**2
                
                     IF ( a >= b) THEN 
                       grad(1) = grad(1) + 2.0_dp * (y(1)-2.0_dp)
                       grad(2) = grad(2) + 2.0_dp * (y(2)-1.0_dp)
                     ELSE 
                       grad(3) = grad(3) + 2.0_dp * (y(3)-2.0_dp)
                       grad(4) = grad(4) + 2.0_dp * (y(4)-1.0_dp)
                     END IF

                     a = (y(1) -3.0_dp)**2 + y(2)**2
                     b = (y(3) -3.0_dp)**2 + y(4)**2
                
                     IF ( a >= b) THEN 
                       grad(1) = grad(1) + 2.0_dp * (y(1)-3.0_dp)
                       grad(2) = grad(2) + 2.0_dp * (y(2))
                     ELSE 
                       grad(3) = grad(3) + 2.0_dp * (y(3)-3.0_dp)
                       grad(4) = grad(4) + 2.0_dp * (y(4)) 
                     END IF

                     a = (y(1))**2 + (y(2)-2.0_dp)**2
                     b = (y(3))**2 + (y(4)-2.0_dp)**2
                
                     IF ( a >= b) THEN 
                       grad(1) = grad(1) + 2.0_dp * (y(1))
                       grad(2) = grad(2) + 2.0_dp * (y(2)-2.0_dp)
                     ELSE 
                       grad(3) = grad(3) + 2.0_dp * (y(3))
                       grad(4) = grad(4) + 2.0_dp * (y(4)-2.0_dp)
                     END IF

                     a = (y(1)-1.0_dp)**2 + (y(2)-2.0_dp)**2
                     b = (y(3)-1.0_dp)**2 + (y(4)-2.0_dp)**2
                
                     IF ( a >= b) THEN 
                       grad(1) = grad(1) + 2.0_dp * (y(1)-1.0_dp)
                       grad(2) = grad(2) + 2.0_dp * (y(2)-2.0_dp)
                     ELSE 
                       grad(3) = grad(3) + 2.0_dp * (y(3)-1.0_dp)
                       grad(4) = grad(4) + 2.0_dp * (y(4)-2.0_dp)
                     END IF                 
                   !------------------------------------
                    
                   !-------------------------------------
                   !           Problem   10
                   !-------------------------------------  
                   CASE(10)
                     grad = 0.0_dp
                
                     DO i = 2, user_n
                       IF ( y(i) - y(i-1) <= 0.0_dp) THEN
                         grad(i-1) = grad(i-1) + 1.0_dp 
                         grad(i) = grad(i) - 1.0_dp 
                       ELSE
                         grad(i-1) = grad(i-1) - 1.0_dp  
                         grad(i) = grad(i) + 1.0_dp     
                       END IF
                     END DO
                   !-------------------------------------
                   
                   !-------------------------------------
                   !           Problem   11
                   !-------------------------------------  
                   CASE(11)
                     grad = 0.0_dp
                
                     grad(1) = -140.0_dp
                     
                     IF (y(2)>0.0_dp) THEN 
                        grad(2) = 40.0_dp
                     ELSE
                        grad(2) = -40.0_dp
                     END IF
               
                   !-------------------------------------       

                   !-------------------------------------
                   !           Problem   12
                   !-------------------------------------
                   CASE(12)                
                     grad = 0.0_dp

                     DO i = 1, user_n
                        grad(i) = grad(i) +20.0_dp*y(i) - 10.0_dp
                     END DO
                     
                     term = 0.0_dp
                     DO i = 1, user_n
                        DO j = 1, user_n 
                           IF (i /= j) THEN
                               term(i) = term(i) + ABS(y(j))
                           END IF
                        END DO
                     END DO
                     
                     largest = term(1)
                     ind = 1 
                     
                     DO i = 2, user_n
                        IF (term(i) > largest) THEN 
                           largest = term(i)
                           ind = i
                        END IF
                     END DO
                     
                     DO i = 1, user_n
                        IF (i /= ind) THEN
                           IF (y(i) > 0.0_dp) THEN 
                              grad(i) = grad(i) +1.0_dp
                           ELSE
                              grad(i) = grad(i) -1.0_dp
                           END IF
                        END IF                   
                     END DO
                   !-------------------------------------   
                   
                   !-------------------------------------
                   !           Problem   13
                   !-------------------------------------
                   CASE(13)         
                     grad = 0.0_dp
                     
                     DO i = 1, user_n-1
                        IF ( y(i) > 0.0_dp) THEN
                           grad(i) = grad(i) + 1.0_dp
                        ELSE
                           grad(i) = grad(i) - 1.0_dp
                        END IF
                        IF ( y(i+1) > 0.0_dp) THEN
                           grad(i+1) = grad(i+1) + 1.0_dp
                        ELSE
                           grad(i+1) = grad(i+1) - 1.0_dp
                        END IF                      
                     END DO
                     
                     DO i = 1, user_n-2
                        IF ( y(i) > 0.0_dp) THEN
                           grad(i) = grad(i) + 1.0_dp
                        ELSE
                           grad(i) = grad(i) - 1.0_dp
                        END IF
                        IF ( y(i+2) > 0.0_dp) THEN
                           grad(i+2) = grad(i+2) + 1.0_dp
                        ELSE
                           grad(i+2) = grad(i+2) - 1.0_dp
                        END IF      
                     END DO                  
                     
                    
                     IF ( y(1) > 0.0_dp) THEN
                        grad(1) = grad(1) + 3.0_dp
                     ELSE
                        grad(1) = grad(1) - 3.0_dp
                     END IF

                     IF ( y(2) > 0.0_dp) THEN
                        grad(2) = grad(2) + 1.0_dp
                     ELSE
                        grad(2) = grad(2) - 1.0_dp
                     END IF
                     
                     IF ( y(4) > 0.0_dp) THEN
                        grad(4) = grad(4) + 1.0_dp
                     ELSE
                        grad(4) = grad(4) - 1.0_dp
                     END IF 

                     IF ( y(5) > 0.0_dp) THEN
                        grad(5) = grad(5) + 1.0_dp
                     ELSE
                        grad(5) = grad(5) - 1.0_dp
                     END IF

                     IF ( y(7) > 0.0_dp) THEN
                        grad(7) = grad(7) + 1.0_dp
                     ELSE
                        grad(7) = grad(7) - 1.0_dp
                     END IF

                     IF ( y(9) > 0.0_dp) THEN
                        grad(9) = grad(9) + 1.0_dp
                     ELSE
                        grad(9) = grad(9) - 1.0_dp
                     END IF

                     IF ( y(10) > 0.0_dp) THEN
                        grad(10) = grad(10) + 2.0_dp
                     ELSE
                        grad(10) = grad(10) - 2.0_dp
                     END IF                                    
                   !-------------------------------------   

                   !-------------------------------------
                   !           Problem   14
                   !-------------------------------------
                   CASE(14)
                     grad = 0.0_dp
                     
                     DO i = 1, user_n
                        apu = 0.0_dp
                        DO j = 1, user_n
                          apu = apu + y(j)/(i+j-1.0_dp)
                        END DO
                        IF (apu >= 0.0_dp) THEN 
                           abs_sign = 1.0_dp
                        ELSE   
                           abs_sign = -1.0_dp
                        END IF
                        DO j = 1, user_n
                          grad(j) = grad(j) + abs_sign / (i+j-1.0_dp)
                        END DO
                     END DO                  
                   !-------------------------------------                      

                   !-------------------------------------
                   !           Problem   15
                   !-------------------------------------
                   CASE(15)
                     grad = 0.0_dp
                                 
                     DO ind = 1, user_n-1
                         aa(1) = y(ind)**4 + y(ind+1)**2
                         aa(2) = (2.0_dp-y(ind))**2 + (2.0_dp-y(ind+1))**2
                         aa(3) = 2.0_dp*EXP(-y(ind)+y(ind+1))
                         IF (aa(1)>aa(2)) THEN
                            IF (aa(1)>aa(3)) THEN
                              grad(ind) = grad(ind) + 4.0_dp*y(ind)**3 
                              grad(ind+1) = grad(ind+1) + 2.0_dp*y(ind+1)
                            ELSE
                              grad(ind) = grad(ind) -2.0_dp * EXP(-y(ind)+y(ind+1)) 
                              grad(ind+1) = grad(ind+1) + 2.0_dp * EXP(-y(ind)+y(ind+1)) 
                            END IF
                         ELSE
                            IF (aa(2)>aa(3)) THEN
                              grad(ind) = grad(ind) - 2.0_dp * (2.0_dp -y(ind))
                              grad(ind+1) = grad(ind+1) - 2.0_dp * (2.0_dp -y(ind+1))
                            ELSE
                              grad(ind) = grad(ind) -2.0_dp * EXP(-y(ind)+y(ind+1)) 
                              grad(ind+1) = grad(ind+1) +2.0_dp * EXP(-y(ind)+y(ind+1)) 
                            END IF      
                         END IF 
                     END DO     
                   !-------------------------------------   

                   
                   !-------------------------------------
                   !           Problem   16
                   !-------------------------------------
                   CASE(16)
                     grad = 0.0_dp

                     DO i = 1, user_n-1
                        grad(i) = grad(i) + 2.0_dp * y(i) 
                        grad(i+1) = grad(i+1) + 2.0_dp * (y(i+1) -1.0_dp) +1.0_dp
                     END DO
                   !-------------------------------------               
 
                   !-------------------------------------
                   !           Problem 17
                   !-------------------------------------
                   CASE(17)
                     grad = 0.0_dp

                     IF ( -y(1) > -0.3_dp*y(2) ) THEN
					    IF (-y(1) > 0.0_dp) THEN
						  grad(1) = -1.0_dp
					      grad(2) = 0.0_dp
						END IF
					 ELSE
					    IF (-0.3_dp*y(2) > 0.0_dp) THEN
						  grad(1) = 0.0_dp
					      grad(2) = -0.3_dp
						END IF
					 END IF
					 
                   !-------------------------------------                    
                
                END SELECT  

           END FUNCTION subgradient_f2
           
           !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -      
           
           
           FUNCTION summa(y, j) RESULT(f)           
                !
                ! Calculates the sum used in f_1 and f_2 for parameter t_j
                !
                ! NOTICE: The dimension of 'y' has to be 'n'. 'j' needs to be an integer from interval [1,20]
                !
                IMPLICIT NONE
                !**************************** NEEDED FROM USER *************************************
                REAL(KIND=dp), DIMENSION(:), INTENT(IN) :: y    ! a point where the function value of the DC component f_2 is calculated
                INTEGER, INTENT(IN) ::  j    ! used to determines the parameter t_j
                !**************************** OTHER VARIABLES **************************************
                REAL(KIND=dp) :: f, t, apu                          ! the function value of the sum used in f_1 and parameter t_j
                INTEGER :: i                                    ! help variable
                
                f = 0.0_dp
                apu = 1.0_dp/ user_n
                
                DO i = 1, user_n 
                    t = (0.05_dp * j )**(i-1)
                    f = f + (y(i)-apu)*t            
                END DO 
            
           END FUNCTION summa



           
      END MODULE functions     