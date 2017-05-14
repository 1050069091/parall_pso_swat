subroutine update_value_speed
use mymodule
integer :: para_file_rank = 0,int_k=0,k = 0
real :: temp_real
! real :: rand_num1,rand_num2

!call init_random_seed()
call init_random_seed(myid+8)
!write(*,*) "update"

int_k = 0
outter:do 
    k = myid+1 + int_k * numprocs
    if(k > seed_rank) exit outter

	do i=1,para_rank

		if(sub_or_hru_arr(i) == 0) then !该参数是每sub就一个
    		para_file_rank = substream_rank
        else 
    		para_file_rank = hru_rank
		end if
      call random_number(first_random_spo)
      call random_number(second_random_spo)

    do j=1,para_file_rank

      para_start_speed_3arr(k,i)%p(j) = inertia_factor_spo*para_start_speed_3arr(k,i)%p(j) &
              + first_speed_factor*first_random_spo*(p_best_arr3(k,i)%p(j)-para_start_value_3arr(k,i)%p(j)) &
              + second_speed_factor*second_random_spo*(g_best_arr2(i)%p(j)-para_start_value_3arr(k,i)%p(j))

      !write(*,*) first_random_spo
        temp_real = para_start_value_3arr(k,i)%p(j) + para_start_speed_3arr(k,i)%p(j)
        if(temp_real < max_para_arr(i) .and. temp_real > min_para_arr(i)) then
            para_start_value_3arr(k,i)%p(j) = temp_real
        !else
        !    para_start_value_3arr(k,i)%p(j) = min_para_arr(i) + first_random_spo*(max_para_arr(i)-min_para_arr(i))
        end if
    end do
    ! write(*,*) para_start_value_3arr(k,i)%p,para_start_speed_3arr(k,i)%p
	end do
    int_k = int_k + 1
end do outter

end subroutine update_value_speed
