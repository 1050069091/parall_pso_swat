subroutine parall_init_para_value()
use mymodule

integer :: seed_num,allocate_err = 0

allocate(para_start_value_3arr(seed_rank,para_rank),stat=allocate_err)
if(allocate_err/= 0) then
    write(*,*) 'allocate para_start_value_arr3 error'
	stop
end if

allocate(para_start_speed_3arr(seed_rank,para_rank),stat=allocate_err)
if(allocate_err/= 0) then
    write(*,*) 'allocate para_start_speed_arr3 error'
	stop
end if

!随机初始化种群中个体的位置和速度
call init_random_seed(myid+8)
do i=0,interval-1
	seed_num = myid+1 + numprocs*i
	if(seed_num > seed_rank) then
		exit
	end if
	!初始化需要率定参数的值：在各参数范围内随机生成
	call initialize_para_val(seed_num)
	!do k = 1,para_rank
		!write(*,*) myid,seed_num,' :place: ',para_start_value_3arr(seed_num,k)%p
        !write(*,*) myid,seed_num,' :speed: ',para_start_speed_3arr(seed_num,k)%p
	!end do	
end do



end subroutine parall_init_para_value
