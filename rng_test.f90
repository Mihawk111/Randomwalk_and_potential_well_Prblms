program rng_test
integer::r_i,x_i,y_i,n,k,i,x(1000000),f(20),d
real::avg_rnd,s,sum1,sum2,sum1_0,r,M,exp,p,kai_sq
!integer::n,interval,i
open(100,file="rng_test.dat",status="unknown")
open(102,file="rng_test2.dat",status="unknown")
seed=12
s=0
n=10000
r_i=seed
M=2**16+1

do i=1,n
x(i)=r_i
write(102,*) i,r_i
s=s+r_i
call random_no(r_i,r)
enddo

do i=2,n
if(x(i)==x(1))then
write(102,*) "# Period=",i-1
exit
endif
enddo

avg_rnd=s/n
write(102,*) '# Average is=',avg_rnd

do i=1,(n-2)/2
x_i=x(2*i)
y_i=x(2*i+1)
write(100,*) i,x_i,y_i
enddo

open(101,file="rng_correl.dat",status="unknown")

do k=0,n-1
    sum1=0
    sum2=0
    do i=1,n-k
        sum1=sum1+x(i)*x(i+k)
    enddo
    sum1=sum1/(n-k)
    if (k==0) then
        sum1_0=sum1
    endif
    sum2=(sum1-avg_rnd*avg_rnd)/(sum1_0-avg_rnd*avg_rnd)
    write(101,*) k,sum1,sum2
enddo

! interval count and kai square

d=20
exp=real(n)/d
p=1.0/d
kai_sq=0

do i=1,d
    f(i)=0
enddo

do i=1,n
    do j=1,d
        if((real(x(i))/M)<j*p)then
            f(j)=f(j)+1
            exit
        endif
    enddo
enddo

do i=1,d
    kai_sq=kai_sq+real(f(i)-exp)*real(f(i)-exp)/exp
enddo

write(102,*) "# Kai_square=",kai_sq

end program rng_test


subroutine random_no(rand,r)
!real::rnd,a,M,c,seed,r
integer::a,M,c,rand
real::r
a=75
c=74
M=2**16+1
rand=mod(a*rand+c,M)
r=real(rand)/M
end subroutine random_no