#!/bin/bash

for k in $( seq 1 20 )
do
    mpirun -n 32 ./parall_spo
done
