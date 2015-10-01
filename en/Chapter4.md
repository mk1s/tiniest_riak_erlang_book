# The Tiniest Riak (Erlang) Book

## Introduction to Processes

Erlang is provides high concurrency whether you are running a single node or hundreds.  It accomplishes this by providing developers with the ability to create lightweight processes that exist solely within the VM.  

We can easily spawn and communicated with processes local to a node or residing on other nodes in a cluster.  Erlang creates a process identifier or PID for each process that can be used by other processes to monitor, send messages, or control.  

A PID is composed of the following structure `<X:Y:Z>`.  

Let's take a look at some live processes

via **riak attach**:

We can get the PID of our process:

```
Eshell V5.9.1  (abort with ^G)
(riak@127.0.0.1)1> REPLPid = self().
<0.7291.0>
```

Let's see how many total processes are currently running:

```
Eshell V5.10.3  (abort with ^G)
(riak@127.0.0.1)1> erlang:processes().
[<0.0.0>,<0.3.0>,<0.6.0>,<0.7.0>,<0.9.0>,<0.10.0>,<0.11.0>,
 <0.12.0>,<0.13.0>,<0.14.0>,<0.15.0>,<0.16.0>,<0.17.0>,
 <0.18.0>,<0.19.0>,<0.20.0>,<0.21.0>,<0.22.0>,<0.23.0>,
 <0.24.0>,<0.25.0>,<0.26.0>,<0.27.0>,<0.28.0>,<0.29.0>,
 <0.30.0>,<0.31.0>,<0.32.0>,<0.33.0>|...]

```

**Note**: REPL will truncate long output, an easy way to print the entire output of your command is by wrapping your command with `rp()`.

But for now, let's see just count how many processes are currently running:

```
Eshell V5.9.1  (abort with ^G)
(riak@127.0.0.1)1> length(erlang:processes()).
1761
```

That's a lot of processes, but what are they?  The PID on its own doesn't give us a lot of insight as to what this process actually does, its' lifetime, or current state.  Erlang provides a way to inspect a specific process if we know the PID.

But first, we should talk about using the PID type and creation:

```
Eshell V5.9.1  (abort with ^G)
(riak@127.0.0.1)1> erlang:is_pid(<0.30.0>).
* 1: syntax error before: '<'
(riak@127.0.0.1)1> erlang:is_pid("<0.30.0>").
false
```

Huh, why didn't that work?  Well in Erlang we need to build up a PID type using some special functions:

```
Eshell V5.9.1  (abort with ^G)
(riak@127.0.0.1)1> A = pid(0,30,0).
<0.30.0>
(riak@127.0.0.1)2> B = list_to_pid("<0.30.0>").
<0.30.0>
```

Now we can grab some information of our REPL process:

```
(riak@127.0.0.1)1> erlang:process_info(REPLPid).
[{current_function,{erl_eval,do_apply,6}},
 {initial_call,{erlang,apply,2}},
 {status,running},
 {message_queue_len,0},
 {messages,[]},
 {links,[<0.7290.0>]},
 {dictionary,[]},
 {trap_exit,false},
 {error_handler,error_handler},
 {priority,normal},
 {group_leader,<16886.30.0>},
 {total_heap_size,1597},
 {heap_size,1597},
 {stack_size,24},
 {reductions,5463},
 {garbage_collection,[{min_bin_vheap_size,46368},
                      {min_heap_size,233},
                      {fullsweep_after,0},
                      {minor_gcs,0}]},
 {suspending,[]}]
```

Interesting, we now have some useful metadata about this process and can even see of evidence of relationships to other processes.

Let's explore what this `group_leader` process is:

```
(riak@127.0.0.1)5> erlang:process_info(pid(0,30,0)).
[{registered_name,user},
 {current_function,{group,server_loop,3}},
 {initial_call,{group,server,3}},
 {status,waiting},
 {message_queue_len,0},
 {messages,[]},
 {links,[<0.28.0>,<0.29.0>,<0.6.0>]},
 {dictionary,[{read_mode,list},
              {kill_buffer,[]},
              {line_buffer,[]},
              {expand_fun,#Fun<group.0.129081181>},
              {echo,true},
              {user_drv,<0.29.0>}]},
 {trap_exit,true},
 {error_handler,error_handler},
 {priority,normal},
 {group_leader,<0.9.0>},
 {total_heap_size,233},
 {heap_size,233},
 {stack_size,4},
 {reductions,28},
 {garbage_collection,[{min_bin_vheap_size,46368},
                      {min_heap_size,233},
                      {fullsweep_after,0},
                      {minor_gcs,0}]},
 {suspending,[]}]

```

Woo Hoo!!! Process trees in action! 

So we can see that this process is currently executing a function called server_loop, but we also see it has a `group_leader` as well.  

Let's have a look at that PID too:

```
(riak@127.0.0.1)4> erlang:process_info(pid(0,9,0)). 
[{current_function,{application_master,main_loop,2}},
 {initial_call,{proc_lib,init_p,5}},
 {status,waiting},
 {message_queue_len,0},
 {messages,[]},
 {links,[<0.7.0>,<0.10.0>]},
 {dictionary,[{'$ancestors',[<0.8.0>]},
              {'$initial_call',{application_master,init,4}}]},
 {trap_exit,true},
 {error_handler,error_handler},
 {priority,normal},
 {group_leader,<0.9.0>},
 {total_heap_size,377},
 {heap_size,377},
 {stack_size,6},
 {reductions,64},
 {garbage_collection,[{min_bin_vheap_size,46368},
                      {min_heap_size,233},
                      {fullsweep_after,0},
                      {minor_gcs,0}]},
 {suspending,[]}]
```

And we see that this is the PID is related to the root application itself!

So we have seen that processes have PIDs and we can extract some useful information about the process.  However, in a very large complex system manually inspecting PIDs to determine their purpose would be cumbersome, so how can we improve our lives when debugging processes?
