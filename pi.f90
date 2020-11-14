PROGRAM mcpi
  implicit none
  integer :: NTHROW,HITS,i
  real    :: dist,x2,y2,x,y,ratio,pi
!  This program uses a 'hit and miss' Monte Carlo integration
!  to determine the value of pi.
      !WRITE(6,*) 'Enter the number of throws desired'
      !READ(5,*) NTHROW
      !
      NTHROW = 1000000
      !
      !WRITE(6,*) 'Enter an integer random number seed'
      !READ(5,*) idum
      !
      HITS=0
      DO i=1,NTHROW
         call random_number(x)
         call random_number(y)
         X2=X*X
         Y2=Y*Y
         DIST=SQRT(X2+Y2)
         !
         IF (DIST.LE.1.0) THEN
           HITS=HITS+1
           !write(*,"(e15.8,2x,e15.8,i4)") x,y,2
         else
           !write(*,"(e15.8,2x,e15.8,i4)") x,y,4
         ENDIF
         !
         RATIO=REAL(HITS)/REAL(i)
         !
         PI=4.0*RATIO
         !
         if (mod(i,100)==0) write(10,"(i,2x,e21.12)") i,pi
         !
      enddo
      RATIO=REAL(HITS)/REAL(NTHROW)
      PI=4.0*RATIO
      WRITE(*,"('The value of pi is calculated to be ',F14.8)") PI
      !
END PROGRAM mcpi
