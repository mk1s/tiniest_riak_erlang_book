# Tiniest Riak (Erlang) Book

## Appendix A: riak-admin top

The **riak-admin**<sup>1</sup> userland tool also provides a wrapper around the **etop**<sup>2</sup> utility, a top like tool included with Erlang.

This can help you identify determine potentially runaway processes and see their current memory utilization and message queues.

**Example:** List processes sorted by Message Queue

```
$ riak-admin top -sort msg_q

===============================================================================================================================
 'riak@127.0.0.1'                                                          07:45:30
 Load:  cpu         0               Memory:  total       66166    binary        390
        procs    1540                        processes   12214    code        10575
        runq        0                        atom          493    ets          6049

Pid                 Name or Initial Func         Time       Reds     Memory       MsgQ Current Function
-------------------------------------------------------------------------------------------------------------------------------
<6172.0.0>          init                          '-'     188842       8712          0 init:loop/1                             
<6172.3.0>          erl_prim_loader               '-'    3624399     230032          0 erl_prim_loader:loop/3                  
<6172.6.0>          error_logger                  '-'      85569       5800          0 gen_event:fetch_msg/5                   
<6172.7.0>          application_controller        '-'    8928318      55880          0 gen_server:loop/6                       
<6172.9.0>          proc_lib:init_p/5             '-'         64       3896          0 application_master:main_loop/2          
<6172.10.0>         application_master:start_     '-'         69       2640          0 application_master:loop_it/4            
<6172.11.0>         kernel_sup                    '-'       2575       6160          0 gen_server:loop/6                       
<6172.12.0>         rex                           '-'      85955       2704          0 gen_server:loop/6                       
<6172.13.0>         global_name_server            '-'         50       2784          0 gen_server:loop/6                       
<6172.14.0>         erlang:apply/2                '-'         20       2600          0 global:loop_the_locker/1                

===============================================================================================================================
```

--
<sup>1</sup> *riak-admin top* - [Official Basho Documentation](http://docs.basho.com/riak/latest/ops/running/tools/riak-admin/#top)

<sup>2</sup> *etop* - [*Official Erlang Documentation*](http://www.erlang.org/doc/man/etop.html)