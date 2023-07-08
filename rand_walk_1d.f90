program rand_walk_1d
real::r,p,q,sum1,sum2,avg,avg_sqr,mean_sqr_avg,mean_sqr_th,a,pos(3)
integer::j,k,i,seed,r_i,n,v
open(100,file="rand_walk_1d.dat",status="unknown")
open(101,file="rand_walk_1d_avg.dat",status="unknown")
seed=12
n=100
p=0.5
q=1-p
a=1
k=3
r_i=seed


do j=1,k
	pos(j)=0.0
enddo


do i=1,n
	do j=1,k
		call random_number(r)
		if(r<p)then
		pos(j)=pos(j)+a
		else
		pos(j)=pos(j)-a
		endif
	enddo
	write(100,*)i,(pos(j),j=1,k)
	
	sum1=0.0
	sum2=0.0
	!mean_sqr_avg=0.0
	do j=1,k
		sum1=sum1+pos(j)
		sum2=sum2+(pos(j)*pos(j))
	enddo
	avg=sum1/k
	avg_sqr=sum2/k
	mean_sqr_avg=(avg_sqr)-(avg*avg)
	mean_sqr_th=4*p*q*a*a*i
	write(101,*)i,avg,avg_sqr,mean_sqr_avg,mean_sqr_th
enddo
end program rand_walk_1d

 
subroutine random_no(rand,r)
real::r
integer::a,c,M,rand
a=12345
c=58
M=100007
rand=mod((a*rand+c),M)
r=real(rand)/M
end subroutine random_no
