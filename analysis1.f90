program presentation1
implicit none
double precision::a,b,tol,x0,y0
double precision,external::f1,f2
x0=2d0 
y0=1d0
tol=1d-10
call newton2d(x0,y0,f1,f2,tol,a,b)
write(6,*)a,b," ---result"
stop
end program presentation1

function f1(y)
  implicit none
  double precision,intent(in)::y
  double precision::f1
 f1=y
  return
end function f1

function f2(x,y)
  implicit none
  double precision,intent(in)::x,y
  double precision::f2
 f2=-x+(1-x**2)*y
  return
end function f2

subroutine newton2d(x01,x02,f1,f2,tol,x1,x2)
implicit none
double precision,intent(in)::x01,x02,tol
double precision,intent(out)::x1,x2
double precision,external::f1,f2

integer::i,key
integer,parameter::itrmax=100 ! maximum iteration
double precision,parameter::tny=tiny(1d0) ! machine epsilon
double precision,parameter::h=2d-8 ! stepsize for forward difference
double precision::c1,c2,d1,d2
double precision::J11,J12,J21,J22,J
double precision::A11,A12,A21,A22
double precision::f11,f12,f21,f22,f10,f20
x1=x01
x2=x02
do i=1,itrmax
    key=0
    c1 = x1
    c2 = x2
    ! For forward difference
    f10 = f1(x1 ,x2 )
    f11 = f1(x1+h,x2 )
    f12 = f1(x1 ,x2+h)
    f20 = f2(x1 ,x2 )
    f21 = f2(x1+h,x2 )
    f22 = f2(x1 ,x2+h)
    ! Jacobian
    J11 = (f11-f10)/h ! df1/dx1
    J12 = (f12-f10)/h ! df1/dx2
    J21 = (f21-f20)/h ! df2/dx1
    J22 = (f22-f20)/h ! df2/dx2
    J=J11*J22-J12*J21
if(abs(J).le.tny)exit
    ! Inverse matrix
    A11 =  J22 / J
    A12 = - J12 / J
    A21 = - J21 / J
    A22 =  J11 / J
    d1 = A11*f10 + A12*f20
    d2 = A21*f10 + A22*f20
    x1 = x1 - d1
    x2 = x2 - d2
    !write(6,'(i5,4f25.16)')i,x1,x2
    ! Convergence check
if(abs(x1).ge.1d0.and.abs(d1/x1).le.tol)key=key+1
if(abs(x1).lt.1d0.and.abs(d1  ).le.tol)key=key+1
if(abs(x2).ge.1d0.and.abs(d2/x2).le.tol)key=key+1
if(abs(x2).lt.1d0.and.abs(d2  ).le.tol)key=key+1
if(key.eq.2)exit
enddo
return
end subroutine newton2d