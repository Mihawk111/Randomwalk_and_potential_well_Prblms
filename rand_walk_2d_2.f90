program rand_walk_2d
real::r,p_x,q_x,p_y,q_y,sum1x,sum2x,sum1y,sum2y,avgx,avg_sqrx,mean_sqr_avgx,avgy,avg_sqry,mean_sqr_avgy,mean_sqr_avg_xy,a_x,a_y
!eta nicher line e na dile hobena, upore eto besi declare hoe ge6e je nite par6ena ar
real::pos_x(500),pos_y(500)
integer::j,k,i,seed,r_i,n,v
open(100,file="rand_walk_2d.dat",status="unknown")
open(101,file="rand_walk_2d_avg.dat",status="unknown")
seed=12
n=10000
p_x=0.25
q_x=0.25
p_y=0.25
q_y=0.25
a_x=1
a_y=1
k=500
r_i=seed


do j=1,k
	pos_x(j)=0.0
	pos_y(j)=0.0
enddo


do i=1,n
	do j=1,k
		call random_no(r_i,r)
		if(r<p_x)then
		pos_x(j)=pos_x(j)+a_x
		else if(r<p_x+q_x)then
		pos_x(j)=pos_x(j)-a_x
		else if(r<p_x+q_x+p_y)then
		pos_y(j)=pos_y(j)+a_y
		else
		pos_y(j)=pos_y(j)-a_y
		endif
	enddo
	write(100,*)i,(pos_x(j),j=1,k),(pos_y(j),j=1,k)
	
!	x axix er jnne

	sum1x=0.0
	sum2x=0.0
	mean_sqr_avgx=0.0
	do j=1,k
		sum1x=sum1x+pos_x(j)
		sum2x=sum2x+(pos_x(j)*pos_x(j))
	enddo
	avgx=sum1x/k
	avg_sqrx=sum2x/k
	mean_sqr_avgx=(avg_sqrx)-(avgx*avgx)
	
!	y axis er dike

	sum1y=0.0
	sum2y=0.0
	mean_sqr_avgy=0.0
	do j=1,k
		sum1y=sum1y+pos_y(j)
		sum2y=sum2y+(pos_y(j)*pos_y(j))
	enddo
	avgy=sum1y/k
	avg_sqry=sum2y/k
	mean_sqr_avgy=(avg_sqry)-(avgy*avgy)

!	duto milie variance

	mean_sqr_avg_xy=mean_sqr_avgx+mean_sqr_avgy

	write(101,*)i,avgx,avg_sqrx,mean_sqr_avgx,avgy,avg_sqry,mean_sqr_avgy,mean_sqr_avg_xy
enddo
end program rand_walk_2d

 
subroutine random_no(rand,r)
real::r
integer::a,c,M,rand
a=75
c=74
M=(2**16+1)
rand=mod((a*rand+c),M)
r=real(rand)/M
end subroutine random_no
