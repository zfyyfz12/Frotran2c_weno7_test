program main
          implicit none
         
          integer ::  i
          real*8  ::  f_3=1.9,f_2=2.2,f_1=3.5,f0=4.3,f1=5.0,f2=6.8,f3=7.1,fhp    !weno7的参数,随意赋值
          real ::start, finish ,ft,ct    !计时变量
          
          !fortran test
          call cpu_time(start)
          do i=0,1000000
          call fh_p_weno7_f(f_3,f_2,f_1,f0,f1,f2,f3,fhp)
          end do
          call cpu_time(finish)
          ft=finish-start
          write(*,*)"fortran running time(s) =",ft
          write(*,*)"f fhp =",fhp   
            
          !c test
          call cpu_time(start)
          do i=0,1000000
          call sub_c(f_3,f_2,f_1,f0,f1,f2,f3,fhp)
          end do
          call cpu_time(finish)
          ct=finish-start
          write(*,*)"c running time(s) =",ct 
          write(*,*)"c fhp =",fhp 
          
          !running time comparision 
          write(*,*)"(ctime-ftime)/ctime (%) =",(ct-ft)/ct *100    
end program main


