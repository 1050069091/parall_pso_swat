subroutine calibration()
use mymodule
integer :: seed_num,int_j
real :: fn_obj_val = 0.0
real,dimension(observed_data_rank) :: simu_datas_arr
integer :: best_recur_num=1,best_seed_num=1

outter:do i=1,recur_rank
    int_j = 0
    tmp_g_best_obj_fn_val = -99999999.0
    inner:do 
        seed_num = myid+1 + int_j * numprocs
       ! write(*,*) seed_num,seed_rank,int_j
        if(seed_num > seed_rank) exit inner
    !    write(*,"(' recur num:',I4,'    seed num:',I4)") i,seed_num
        if(.not.(i == 1 .and. seed_num == 1 .and. is_use_befor == 1)) then
		    call modify_para_in_inputfile(seed_num)
        end if
        !write(*,*) './swat2012_627_paral '//myid_str//' '//trim(swat_input_file_postfix_names)//' > txt'//myid_str
        !write(*,*) "myid:",myid,"seed_num:",seed_num,"myid_str:",myid_str
        call system('./swat2012_627_paral '//myid_str//' '//trim(swat_input_file_postfix_names)//' > /dev/null')
        !call system('./swat2012_627_paral '//myid_str//' '//trim(swat_input_file_postfix_names))
		!获得模拟值，并写入文件中
		call extract_swat_output(simu_datas_arr)
        !write(*,*) myid
		if(fn_type == 1) then
			call solve_r2(observed_data_val_arr,simu_datas_arr,observed_data_rank,fn_obj_val)
!            write(*,"(' recur num:',I4,'    seed num:',I4,A10,A6,F10.4,'    <-------- by node',I3)") &
!                                            i,seed_num,'------->','  R2=',fn_obj_val,myid
		else
			call solve_ns(observed_data_val_arr,simu_datas_arr,observed_data_rank,fn_obj_val)
!            write(*,"(' recur num:',I4,'    seed num:',I4,A10,A6,F10.4,'    <-------- by node',I3)") &
!                                            i,seed_num,'------->','  NS=',fn_obj_val,myid
		end if
		if(fn_obj_val > p_best_obj_fn_val_arr(seed_num)) then
			p_best_obj_fn_val_arr(seed_num) = fn_obj_val
			do k = 1,para_rank
				p_best_arr3(seed_num,k)%p => para_start_value_3arr(seed_num,k)%p
			end do
			if(fn_obj_val > g_best_obj_fn_val) then
				do k = 1,para_rank
					g_best_arr2(k)%p => para_start_value_3arr(seed_num,k)%p
					g_best_arr2(k)%size = para_start_value_3arr(seed_num,k)%size
                    g_best_obj_fn_val = fn_obj_val
                    best_recur_num = i
                    best_seed_num = seed_num
				end do
                call system('mv -f out/'//trim(obser_val_name)&
                    //myid_str//'.txt out/best_sim_data.txt'//trim(myid_str))
                call system("cp -f `ls | grep '[.]"//myid_str//"$'` ./out")
			end if
		end if
        if(fn_obj_val > tmp_g_best_obj_fn_val) then
            tmp_g_best_obj_fn_val = fn_obj_val
        end if
        !if(fn_obj_val > obj_fn_threshold) then
            !exit outter
        !end if
        int_j = int_j + 1
    end do inner

    !开始通信
    best_sim_recur_id = best_recur_num
    best_sim_seed_id = best_seed_num

    call communication_to_get_best_values(i)

    if(g_best_obj_fn_val >= obj_fn_threshold) exit outter

    old_g_best_obj_fn_val = g_best_obj_fn_val

	call update_value_speed()

    end do outter

end subroutine calibration
