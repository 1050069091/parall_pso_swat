subroutine parall_pre_calibration
use mymodule

integer :: allocate_err1=0,allocate_err2=0,allocate_err3=0,allocate_err4=0,lentrim=1,all_lentrim
!type(real_arr_p),pointer :: tmp_real_arr_p

allocate(p_best_obj_fn_val_arr(seed_rank),stat=allocate_err1)
allocate(p_best_arr3(seed_rank,para_rank),stat=allocate_err2)
allocate(g_best_arr2(para_rank),stat=allocate_err3)
!allocate(simu_datas_arr(observed_data_rank),stat=allocate_err4)

if(allocate_err1 + allocate_err2 + allocate_err3 /= 0) then
    write(*,*) 'in parall_pre_calibration.f90:12,allocate err!'
	stop
end if

do i=1,seed_rank
	p_best_obj_fn_val_arr(i) = -999999999.0
end do

lentrim = 1
do i=1,file_kind_rank
    lentrim = lentrim + len_trim(detail_each_file_include_para_arr(i)%postfile_name)
    all_lentrim = len_trim(swat_input_file_postfix_names)
    !write(*,*) 'wwwww:',lentrim,all_lentrim
    swat_input_file_postfix_names(all_lentrim+1:all_lentrim+lentrim) = detail_each_file_include_para_arr(i)%postfile_name(1:lentrim)
    !write(*,*) 'wwwww:',swat_input_file_postfix_names, trim(detail_each_file_include_para_arr(i)%postfile_name)

end do

files_need_modi=0
do i=1,para_rank
    !tmp_real_arr_p = para_start_value_3arr(1,i)
    !write(*,*) tmp_real_arr_p%size
    files_need_modi = para_start_value_3arr(myid+1,i)%size + files_need_modi
end do


allocate(g_best_place_vals_arr(files_need_modi),stat=allocate_err1)
if(allocate_err1 /= 0) then
    write(*,*) 'in parall_pre_calibration.f90:35,allocate err!'
    stop
end if
end subroutine parall_pre_calibration
