# DBDC
The double bundle method for nonsmooth DC optimization

DBDC is a proximal double bundle solver (Fortran 95) for nonsmooth DC programming (difference of two convex functions) by K. Joki. DBDC uses a new procedure guaranteeing Clarke stationarity for solutions by utilizing only DC components of the objective function. This optimality condition is stronger than the criticality condition typically used in DC programming. In addition, if a solution candidate is not Clarke stationary, then a descent direction is generated.

The software utilizes OpenMP at each round of 'main iteration' to calculate subproblems in parallel. To turn down OpenMP, see instructions in tpbdc.f95. In addition, there is a possibility to use simple stepsize determination after each 'main iteration'. The software uses code PLQDF1 by Prof. Ladislav Luksan to solve quadratic direction finding problem.

The software is free for academic teaching and research purposes but I ask you to refer the reference given below if you use it. To use the software modify tdbdc.f95 and functions.f95 as needed. If you have any questions conserning the software, please contact directly the author Kaisa Joki.

Codes include:                                                                     
         
   tdbdc.f95          - Main program for DBDC (this file)                             
   constants.f95      - Double precision (also some parameters)                       
   bundle1.f95        - Bundle of DC component f_1                                            
   bundle2.f95        - Bundle of DC component f_2                                    
        
   functions.f95      - User-specified DC components f_1 and f_2 together with subgradients of DC components. Contains also user-specified initial values for parameters                                 
   dbdc_non.f95       - DBDC method                                                   
                                                                                             
   plqdf1.f           - Quadratic solver by Ladislav Luksan                           
                                                                                              
   Makefile           - Makefile         
   
References:                                                                        
                                                                                              
[1] Kaisa Joki, Adil M. Bagirov, Napsu Karmitsa and Marko M. Mäkelä: "A proximal bundle method for nonsmooth DC optimization utilizing nonconvex cutting planes". J. Glob. Optim. 68 (2017), pp. 501-535, https://doi.org/10.1007/s10898-016-0488-3                                       
                                                                                              
[2] Kaisa Joki, Adil M. Bagirov, Napsu Karmitsa, Marko M. Mäkelä and Sona Taheri: "Double bundle method for finding Clarke stationary points in nonsmooth DC programming". SIAM J. Optim., 28 (2018), pp. 1892–1919. https://doi.org/10.1137/16M1115733      
   
