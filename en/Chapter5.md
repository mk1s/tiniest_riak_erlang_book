# The Tiniest Riak (Erlang) Book

## Processes Continued ...

In the last section we saw many processes can exist within an Erlang VM. But how do we quickly and reliably interact with long running processes we know will always exist, such as the process responsible for GET operations in Riak?

Well luckily Erlang gives a mechanism to register processes with a name.

### Registered Processes

OTP allows us to create supervisor trees for 1 to many parent and child processes.  

One step further is we can actually name these processes so that we have a predictable manner in communicating with them rather than having to consult a file or data structure for what could be a transient PID.

Let's have a look at the currently running registered processes:

```
Eshell V5.9.1  (abort with ^G)
(riak@127.0.0.1)1> registered().
[riak_pipe_builder_sup,sasl_safe_sup,riak_core_vnode_sup,
 riak_core_vnode_proxy_sup,inets_sup,
 proxy_riak_pipe_vnode_1438665674247607560106752257205091097473808596992,
 riak_core_vnode_manager,
 proxy_riak_pipe_vnode_1415829711164312202009819681693899175291684651008,
 proxy_riak_pipe_vnode_1392993748081016843912887106182707253109560705024,
 proxy_riak_pipe_vnode_1370157784997721485815954530671515330927436759040,
 proxy_riak_pipe_vnode_1347321821914426127719021955160323408745312813056,
 httpd_sup,
 proxy_riak_pipe_vnode_1324485858831130769622089379649131486563188867072,
 proxy_riak_pipe_vnode_1301649895747835411525156804137939564381064921088,
 riak_core_sysmon_minder,
 proxy_riak_pipe_vnode_1278813932664540053428224228626747642198940975104,
 proxy_riak_pipe_vnode_1255977969581244695331291653115555720016817029120,
 riak_core_sup,
 proxy_riak_pipe_vnode_1233142006497949337234359077604363797834693083136,
 proxy_riak_pipe_vnode_1210306043414653979137426502093171875652569137152,
 riak_core_stats_sup,
 proxy_riak_pipe_vnode_1187470080331358621040493926581979953470445191168,
 auth,riak_core_stat_sup,
 proxy_riak_pipe_vnode_1164634117248063262943561351070788031288321245184,
 proxy_riak_pipe_vnode_1141798154164767904846628775559596109106197299200,
 riak_core_stat_calc_sup,error_logger,
 proxy_riak_pipe_vnode_1118962191081472546749696200048404186924073353216|...]
(riak@127.0.0.1)2>
```

So that yields far more useful and contextual data about currently running processes.  We now see Riak related processes for supervisors, worker processes, as well as some general Erlang VM processes.

Let's take a look at the `riak_core_vnode_manager` process:

```
Eshell V5.9.1  (abort with ^G)
(riak@127.0.0.1)1> VPID = whereis(riak_core_vnode_manager).
<0.160.0>
```

Well that was much easier than surfing traversing the 1700+ PIDs from the last chapter:

```
(riak@127.0.0.1)2> erlang:process_info(VPID).
[{registered_name,riak_core_vnode_manager},
 {current_function,{gen_server,loop,6}},
 {initial_call,{proc_lib,init_p,5}},
 {status,waiting},
 {message_queue_len,0},
 {messages,[]},
 {links,[<0.149.0>]},
 {dictionary,[{'$ancestors',[riak_core_sup,<0.148.0>]},
              {'$initial_call',{riak_core_vnode_manager,init,1}}]},
 {trap_exit,false},
 {error_handler,error_handler},
 {priority,normal},
 {group_leader,<0.147.0>},
 {total_heap_size,6765},
 {heap_size,6765},
 {stack_size,9},
 {reductions,1684027},
 {garbage_collection,[{min_bin_vheap_size,46368},
                      {min_heap_size,233},
                      {fullsweep_after,0},
                      {minor_gcs,0}]},
 {suspending,[]}]

```

So in addition to being able to spawn a bunch of processes, we can also give them meaningful and predictable names which simplifies the development and debugging of large systems.