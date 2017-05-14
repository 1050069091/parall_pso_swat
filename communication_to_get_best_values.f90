subroutine communication_to_get_best_values(recur_num)
use mymodule
use mpi
integer,intent(in) :: recur_num
integer sendpair(2),recv(2),senddata(1),recvdata(1)
integer ierr,index,int_old_best_obj_fn_val,inter_time

sendpair(1) = g_best_obj_fn_val * 10000
sendpair(2) = myid

senddata(1) = tmp_g_best_obj_fn_val * 10000

int_old_best_obj_fn_val = old_g_best_obj_fn_val * 10000

call MPI_ALLREDUCE(sendpair,recv,1,MPI_2INTEGER,MPI_MAXLOC,mycomm,ierr)
call MPI_ALLREDUCE(senddata,recvdata,1,MPI_INTEGER,MPI_MAX,mycomm,ierr)
!write(*,*) "myid,recv:",myid,recv

if(myid == 0) then

     call date_and_time(b(1), b(2), b(3), date_time2)
     inter_time = date_time2(7)-date_time1(7) + 60*(date_time2(6)-date_time1(6)) &
            + 60*60*(date_time2(5)-date_time1(5)) + 60*60*60*(date_time2(4)-date_time1(4))&
            + 60*60*60*12*(date_time2(3)-date_time1(3))
     write(*,"(' ***************************粒子群算法进化代数:',I4,&
        ' 最大目标函数值:',F8.4,'******',' 耗时:',I6,'s*****************************************')") &
        recur_num,recvdata(1)/10000.0,inter_time
end if

!write(*,*) "myid:",myid,"recv:",recv(1),'int_old',int_old_best_obj_fn_val
if(recv(1)-int_old_best_obj_fn_val /= 0) then
index = 0
best_sim_myid = recv(2)
if(myid==recv(2)) then


!广播种群最优参数信息
    do i=1,para_rank
        do j=1,g_best_arr2(i)%size
            g_best_place_vals_arr(index+j) = g_best_arr2(i)%p(j)
        end do
        index = index + g_best_arr2(i)%size
        !write(*,*) "myid,index:---->",myid,index,g_best_arr2(i)%size
    end do

    !call system('cp -f out/'//trim(obser_val_name)&
    !                        //myid_str//'.txt out/best_sim_data.txt')
    !call system("cp -f `ls | grep '[.]"//myid_str//"$'` ./out")
    !write(*,*) myid_str
    
end if

g_best_obj_fn_val = recv(1) / 10000.0
!write(*,*) "myid:---->",myid,g_best_place_vals_arr

call MPI_BCAST(g_best_place_vals_arr&
                ,files_need_modi,MPI_REAL,recv(2),mycomm,ierr)

if(myid /= recv(2)) then


!接受种群最优参数信息
    do i=1,para_rank
        do j=1,g_best_arr2(i)%size
            g_best_arr2(i)%p(j) = g_best_place_vals_arr(index+j) 
        end do
        index = index + g_best_arr2(i)%size
    end do
end if
!write(*,*) myid,":post:------->",g_best_place_vals_arr
end if

end subroutine communication_to_get_best_values
