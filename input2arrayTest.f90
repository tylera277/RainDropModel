! Purpose: Model rain drops falling and have the number that
!          hit the moving object be counter
! Author: Tyler Allen


program input2arrayTest
  implicit none

integer :: i,k,j
integer :: time  !time of system which is updated by DO loop
integer :: counter=0  !counter which counts the number of hits
                      !of rain drops on object
real :: jNew,jNew2
real :: zDropDistance=0.0 !amount the drops fall over time; updated each run
real :: yCoordinate(0:14000000),zCoordinate(0:140000000),xCoordinate(0:140000000)
real :: objectVel  !speed of object moving through rain
real :: xObject  !position of object which changes over time.

write(*,*)"Please enter speed of the object:"
read(*,*) objectVel




do time= 0,3599,1  !time scale over which coordinates are being updated
                   !is an hour of seconds (3600s=1hr)

  zDropDistance = 10*time  !distance object fallen in time
  xObject = objectVel*time
  
  do i=0,36000,3  ! these arrays are laying out the grid pattern/distance of/between raindrops
                  ! for my box in which the rain is falling for over 1 hr 
                  ! (i -> height(slices), j -> width, k ->length)
    do j = 0,4,1  !was 0 4 1
       jNew = j
       jNew2 = jNew/10
      do k = 0,9,1  !was 0 9 1
       xCoordinate(k) = k 
       yCoordinate(j) = jNew2
       zCoordinate(i) = i - zDropDistance  !adjusting z coordinate for distance fallen       
        
       
       if (zCoordinate(i) <=1.83 .and. 0<zCoordinate(i)) then  ! the height of the object
          
          if (yCoordinate(j)>=0 .and. yCoordinate(j)<=0.58) then  ! the width of the object
            
             if (xCoordinate(k) -xObject<=0.000000000000005) then  ! the position of the object, updated
                                                                   !because object is moving into the rain
                counter = counter + 1  !if all conditions meet, this counts as a hit
                                       !and counter increased by 1
                
             end if
          end if
       end if
       
       
      end do
    end do
  end do 
  
end do

write(*,*) counter  ! number of raindrops that "hit" the object/the object ran through







  stop 0
end program input2arrayTest
