C Program to compute Pi using monte carlo methods

       program monte_pi
       implicit none
       
       integer niter,i,j
       integer seed 
       real*4 count
       real *8 x,y,pi(100),z
       real*8   rand
       
C initialize random numbers
       seed = 35791246
       call srand (seed)
       do j= 1,100
          niter = niter+100
          count =0
          do i=1,niter
             x=rand()
             y=rand()
             z= x*x +y*y
             if (z .le. 1) count =count+1
          end do
          pi(j)= count/niter*4.
       end do
       write(*,10) niter,pi(100)
10     format('Number of trials is: 'i5,'  estimate of pi is:',f8.5)
       end
