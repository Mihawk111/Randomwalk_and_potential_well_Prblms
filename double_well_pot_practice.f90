 program double_well_pot
      implicit none
      integer::NSTEP,N
      real*8::X(2),T,A,B,H,theta0,phi0 
      N=2;T=0.0;X(1)=1.0;X(2)=1.0
      A=0.0;B=10.0; NSTEP=10000
      H = (B - A) / NSTEP
      open(300,file='double_well_pot_practice.dat',status='unknown')
      open(301,file='double_well_pot_practice_1.dat',status='unknown')
      open(302,file='double_well_pot_practice_2.dat',status='unknown')
      write(300,*)'# DOUBLE WELL POTENTIAL'
      write(300,*)'# x = ',X(1),'p = ',X(2)
      write(300,*)'# A = ',A,'B = ',B,'H = ',H
  !    write(100,8)
      call RK4SYS(N,T,X,H,NSTEP)
! 8    FORMAT(8X,'Time',18X,'theta',22X,'phi')
      STOP
      end program double_well_pot 
  
      SUBROUTINE XPSYS(X,F) 
      REAL*8 X(2),F(12),m,c
      m=1.0     ;c=9.0
      F(1) = X(2)/m
      F(2) = (4*c*X(1))-4*(X(1))**3
      RETURN
      END 

      SUBROUTINE RK4SYS(N,T,X,H,NSTEP)
      REAL*8 X(2),Y(12),F1(12),F2(12),F3(12),F4(12),F(12)
      REAL*8 T,H,H2,START,e,p
      INTEGER N,NSTEP,K
      write(300,7)T,(X(I),I=1,N)
      H2 = 0.5*H  
      START = T   
      DO 6 K = 1,NSTEP      
        CALL XPSYS(X,F1)    
        DO 2 I = 1,N
          Y(I) = X(I) + H2*F1(I)      
   2    CONTINUE
        CALL XPSYS(Y,F2)    
        DO 3 I = 1,N
          Y(I) = X(I) + H2*F2(I)      
   3    CONTINUE
        CALL XPSYS(Y,F3)    
        DO 4 I = 1,N
          Y(I) = X(I) + H*F3(I)       
   4    CONTINUE
        CALL XPSYS(Y,F4)    
        DO 5 I = 1,N
          X(I) = X(I) + H*(F1(I) + 2.0*(F2(I) + F3(I)) + F4(I))/6.0 
   5    CONTINUE
        T = START + REAL(K)*H 
        e=((X(2)**2)/2.0)-(18*x(1)*X(1))+((X(1))**4)
        p=sqrt(2.0*((18.0*X(1)*X(1))-((X(1))**4)))
        write(302,*)X(1),p,-p
        if(e<0)then
        write(300,7)T,(X(I),I = 1,N),e,(-X(I),I = 1,N)
        else
        write(301,7)T,(X(I),I = 1,N),e 
        endif  
   6  CONTINUE
   7  FORMAT(2X,E15.8,5(2X,E22.14))
      RETURN      
      END 
