        !*..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..*
        !| .**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**. |
        !| |                                                                                  | |
        !| |                                                                                  | |
        !| |      DBDC - THE PROXIMAL DOUBLE BUNDLE METHOD FOR NONSMOOTH DC OPTIMIZATION      | | 
        !| |                                 (version 2)                                      | |
        !| |                                                                                  | |
        !| |                                                                                  | |
        !| |                                                                                  | |
        !| |                       by Kaisa Joki (last modified September 2017)               | |
        !| |                                                                                  | |
        !| |      Features :                                                                  | |
        !| |                                                                                  | |
        !| |                                                                                  | |
        !| |           * Possibility to use simple stepsize determination after               | |
        !| |             each 'main iteration'.                                               | |
        !| |                                                                                  | |
        !| |           * During each round of 'main iteration' OpenMP can be utilized         | |
        !| |             to calculate subproblems in parallel. However if you DO NOT          | |
        !| |             WANT to use this feature then                                        | |
        !| |                1) in Makefile DELETE '-fopenmp' from line 5                      | |
        !| |                2) in dbdc.f95 COMMENT lines 458-461                              | |
        !| |                                                                                  | |         
        !| |                                                                                  | |
        !| |                                                                                  | |
        !| |     The software is free for academic teaching and research purposes but I       | |
        !| |     ask you to refer the reference given below, if you use it.                   | |
        !| |                                                                                  | |
        !| |                                                                                  | |
        !| .**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**. |
        !*..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..*     
        !|                                                                                      |
        !|                                                                                      |
        !|    Utilizes the new version of PLQDF1 by Ladislav Luksan as a quadratic solver.      |
        !|                                                                                      |
        !|                                                                                      |
        !| .**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**. |
        !*..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..*
        !|                                                                                      |
        !|                                                                                      |
        !|   Codes include:                                                                     |
        !|                                                                                      |
        !|   tdbdc.f95          - Main program for DBDC (this file)                             |
        !|   constants.f95      - Double precision (also some parameters)                       |
        !|   bundle1.f95        - Bundle of DC component f_1                                    |
        !|   bundle2.f95        - Bundle of DC component f_2                                    |
        !|   functions.f95      - User-specified DC components f_1 and f_2 together with        |
        !|                        subgradients of DC components. Contains also user-specified   |
        !|                        initial values for parameters                                 |
        !|   dbdc_non.f95       - DBDC method                                                   |
        !|                                                                                      |
        !|   plqdf1.f           - Quadratic solver by Ladislav Luksan                           |
        !|                                                                                      |
        !|   Makefile           - Makefile                                                      |
        !|                                                                                      |
        !|                                                                                      |
        !|                                                                                      |
        !|   To USE the software MODIFY   tdbdc.f95   and   functions.f95   as needed           |
        !|                                                                                      |
        !|                                                                                      |
        !|   References:                                                                        |
        !|                                                                                      |
        !|   [1] Kaisa Joki, Adil M. Bagirov, Napsu Karmitsa and Marko M. Mäkelä:               |
        !|       "A proximal bundle method for nonsmooth DC optimization utilizing              |
        !|       nonconvex cutting planes". J. Glob. Optim. 68 (2017), pp. 501-535,              | 
        !|       https://doi.org/10.1007/s10898-016-0488-3                                      | 
        !|                                                                                      |
        !|                                                                                      |
        !|   [2] Kaisa Joki, Adil M. Bagirov, Napsu Karmitsa, Marko M. Mäkelä and Sona Taheri:  |
        !|       "Double bundle method for finding Clarke stationary points in nonsmooth        |
        !!        DC programming". SIAM J. Optim., 28 (2018), pp. 1892–1919.                    |
        !|        https://doi.org/10.1137/16M1115733                                            |
        !|                                                                                      |
        !| .**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**. |
        !*..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..**..*       


      PROGRAM tdbdc
      
         USE constants, ONLY : dp   ! double precision (i.e. accuracy)
         USE functions              ! INFORMATION from the USER
         USE bundle1                ! The BUNDLE of the DC component f_1
         USE bundle2                ! The BUNDLE of the DC component f_2
         USE dbdc_non               ! DBDC method
         
        IMPLICIT NONE   
        
        ! 'user_n' is the number of variables in the problem (USER specifies this in MODULE functions.f95)
        REAL (KIND=dp), DIMENSION(user_n) :: x_0           ! The starting point in the DBDC method (specified by USER)
        REAL (KIND=dp), DIMENSION(user_n) :: x_solution    ! The solution obtained to the problem
        
        REAL(KIND=dp) :: f_solution         ! The objective function value at the solution 'x_solution'
        
        INTEGER, DIMENSION(8) :: counter    ! Contains the values of different counteres:
                                            !   counter(1) = iter_counter:         the number of 'main iterations' executed
                                            !   counter(2) = subprob_counter:      the number of subproblems solved in 'main iteration'
                                            !   counter(3) = f_counter:            the number of function values evaluated for DC component in 'main iteration'
                                            !   counter(4) = subgrad1_counter:     the number of subgradients calculated for f_1 in 'main iteration'
                                            !   counter(5) = subgrad2_counter:     the number of subgradients calculated for f_2 in 'main iteration'
                                            !--------------------------------------------------------------------------------------------------------------------------                 
                                            !   counter(6) = stop_cond_counter:    the number of times 'Clarke stationary algorithm' is executed 
                                            !   counter(7) = clarke_f_counter:     the number of function values evaluated for f in 'Clarke stationary algorithms'
                                            !   counter(8) = clarke_sub_counter:   the number of subgradients caluculated for f in 'Clarke stationary algorithms'
 
    
        INTEGER :: iprint                   ! Variable that specifies print option (specified by USER): 
                                            !   iprint = 0 : print is suppressed
                                            !   iprint = 1 : basic print of final result 
                                            !   iprint = -1: basic print of final result (without the solution vector)
                                            !   iprint = 2 : extended print of final result 
                                            !   iprint = -2: extended print of final result (without the solution vector)
                                            !   iprint = 3 : basic print of intermediate results and extended print of final results
                                            !   iprint = -3: basic print of intermediate results and extended print of final results (without the solution vector)
                                            !   iprint = 4 : extended print of intermediate results and extended print of final results 
                                            !   iprint = -4: extended print of intermediate results and extended print of final results (without the solution vectors)
                                            !
                                            ! If 'iprint' <= -5 .OR. 'iprint' >= 5 then DEFAULT value 'iprint'=1 is used    
                                            
        
        INTEGER :: mit                      ! The maximum number of 'main iterations' (specified by USER).
                                            ! If 'mit' <=0 then DEFAULT value 'mit'=1000 is used

        INTEGER :: mrounds                  ! The maximum number of rounds during one 'main iteration' (specified by USER).
                                            ! If 'mrounds' <=0 then DEFAULT value 'mrounds'=5000 is used

        INTEGER :: mrounds_clarke           ! The maximum number of rounds during one 'Clarke stationarity' algorithm (specified by USER).
                                            ! If 'mrounds_clarke' <=0 then DEFAULT value 'mrounds_clarke'=5000 is used
                                            
        INTEGER :: termination              ! The reason for termination in DBDC:
                                            !   1 - the stopping condition is satisfied (i.e. Clarke stationarity)
                                            !   2 - the approximate stopping condition is satisfied (i.e. the step-length beta* < user_eps)
                                            !   3 - the maximum number 'mrounds' of rounds is executed in one main iteration
                                            !   4 - the maximum number of 'main iterations' is executed 
                                            !   5 - the maximum number 'mrounds_clarke' of rounds is executed in one 'Clarke stationary' algorithm

        LOGICAL :: agg_used                 ! If .TRUE. then aggregation is used in DBDC (specified by USER).
        LOGICAL :: stepsize_used            ! If .TRUE. then simple stepsize determination is done in DBDC after each 'main iteration' (specified by USER).
        
        INTEGER :: i
        

          mrounds = 10000           ! maximum number of rounds during one 'main iterations'
          mit = 10000               ! maximum number of 'main iteration'
          mrounds_clarke = 10000    ! maximum number of rounds during one 'Clarke stationary' algorithm
          
          iprint = 3                ! basic print of intermediate results and extended print of final results
          
          agg_used = .TRUE.         ! Aggregation is used
          stepsize_used = .FALSE.   ! Simple stepsize determination is not used


!--------------------------------------------------------------------------------------
          
! The starting points for the test problems presented in MODULE functions.f95 and in the articles [1] and [2]  
        
        SELECT CASE(problem1)       

! Problem 1.  
! x_0 = (/ 2.0_dp, 2.0_dp /)
          CASE(1)
             x_0 = 2.0_dp
            
 
! Problem 2. (L1 version of Rosenbrock) 
! x_0 = (/ -1.20_dp, 1.0_dp /)   
          CASE(2)
            x_0 = 1.0_dp
            x_0(1) = -1.20_dp

            
! Problem 3.
! x_0 = (/ 1.0_dp, 3.0_dp, 3.0_dp, 1.0_dp /)
          CASE(3)
            x_0 = 3.0_dp
            x_0(1) = 1.0_dp
            x_0(user_n) = 1.0_dp


! Problem 4  (for any user_n)
          CASE(4)
            DO i = 1, user_n/2
               x_0(i)  = 1.0_dp * i
            END DO
            DO i = (user_n/2 + 1), user_n
               x_0(i)  = -1.0_dp * i
            END DO           

! Problem 5.  (for any user_n)
          CASE(5)
            x_0 = 0.0_dp

! Problem 6. 
! x_0 = (/ 10.0_dp, 1.0_dp /)
          CASE(6)
            x_0 = 1.0_dp
            x_0(1) = 10.0_dp  

! Problem 7.
! x_0 = (/ -2.0_dp, 1.0_dp /)
          CASE(7)
            x_0 = 1.0_dp 
            x_0(1) = -2.0_dp 
          
! Problem 8.        
! x_0 = (/ 0.5_dp, 0.5_dp, 0.5_dp /)
          CASE(8)
            x_0 = 0.5_dp

! Problem 9.
! x_0= (/ 4.0_dp, 2.0_dp, 4.0_dp, 2.0_dp/)
          CASE(9)
            DO i = 1, user_n
               IF ( (i == 1) .OR. (i == 3)) THEN
                  x_0(i) = 4.0_dp
               ELSE
                  x_0(i) = 2.0_dp              
               END IF
            END DO   
            
! Problem 10.   (for any user_n)
          CASE(10)
            DO i = 1, user_n
               x_0(i)  = 0.1_dp * i
            END DO

! Problem 11.
          CASE(11)
            x_0 = 10.0_dp       

! Problem 12.   (for any user_n)
          CASE(12)
            DO i = 1, user_n
               x_0(i) = 2.0_dp * i 
            END DO           
            
! Problem 13.
          CASE(13)
            x_0 = 10.0_dp
           
! Problem 14.   (for any user_n)
          CASE(14)
            x_0 = 1.0_dp
            
! Problem 15.   (for any user_n)
          CASE(15)
            x_0 = -1.0_dp
            DO i = 1,user_n, 2
              x_0(i) = 1.0_dp
            END DO        
  
! Problem 16.   (for any user_n)
          CASE(16)
            x_0 = -1.5_dp
            DO i = 2, user_n, 2
               x_0(i) = 2.0_dp
            END DO
            
! Problem 17  
         CASE(17)
           x_0=0.0_dp               
            
          END SELECT
!--------------------------------------------------------------------------------------
        

         WRITE(*,*) '------------------------------------------------------------------'
         WRITE(*,*) '** START ** START ** START ** START ** START ** START ** START **'  
         WRITE(*,*) '------------------------------------------------------------------'         
         WRITE(*,*) ' '

         CALL DBDC_algorithm( x_0, x_solution, f_solution, mit, mrounds, &
                            & mrounds_clarke, termination, counter, agg_used,  &
                            & stepsize_used, iprint )



         WRITE(*,*) ' '
         WRITE(*,*) '------------------------------------------------------------------'
         WRITE(*,*) '** END ** END ** END ** END ** END ** END ** END ** END ** END **'  
         WRITE(*,*) '------------------------------------------------------------------'
         WRITE(*,*) ' '      

  
      END PROGRAM tdbdc






















