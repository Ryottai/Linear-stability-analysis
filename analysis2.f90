program presentation2
implicit none
double precision::x1,y1,h
double precision,external::f1,g1
h=0.00001
write(*,*)'h=',h
call sub1(x1,y1,h,f1,g1)
stop
end program presentation2

function f1(x,y)
implicit none
real(8) x,y,f1
f1=y-x*x*x+x
return
end function f1

function g1(x)
implicit none
real(8) x,g1
g1=-x
return
end function g1

subroutine sub1(x1,y1,h,f1,g1)
implicit none
real(8) x1,y1,h,f1,g1,f1x,f1y,g1x,g1y,J(2,2),trj,detj
!偏微分
f1x=(f1(x1+h,y1)-f1(x1,y1))
f1y=(f1(x1,y1+h)-f1(x1,y1))
g1x=(g1(x1+h,y1)-g1(x1,y1))
g1y=(g1(x1,y1+h)-g1(x1,y1))
J(1,1)=f1x/h
J(1,2)=f1y/h
J(2,1)=g1x/h
J(2,2)=g1y/h
write(*,*)'Please read Equilibrium solution'
read(*,*)x1,y1
write(*,*)'x1=',x1,'y1=',y1
write(*,*) J(1,1),J(1,2)
write(*,*) J(2,1),J(2,2)
!行列式
write(*,*)'trj=J(1,1)+J(2,2),detj=J(1,1)*J(2,1)-J(1,2)*J(2,2)'
trj=J(1,1)+J(2,2)
detj=J(1,1)*J(2,2)-J(1,2)*J(2,1)
write(*,*)'trj=',trj,'detj=',detj
return
end subroutine sub1
    