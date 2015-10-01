# The Tiniest Riak (Erlang) Book

## TL;DR Haz Shell, Canz Script?

Typically when interacting with Riak from an Erlang perspective, we can employ one of two techniques:

* `riak attach`

* `riak escript`

### riak attach

If you have Erlang installed you can launch a VM and interact with the REPL shell immediately; simply by typing `erl`.

Riak, whether from source or package, includes a distributable version of the Erlang Runtime System(ERTS) as well as some helper tools in we can use to interact with the Virtual Machine or Riak itself.

One of these tools is `riak attach`, a tool which connects to the running Riak ERTS and provides an Erlang REPL shell.  This affords us access to all of Erlang VM as well as Riak modules and their exported functions. 

`riak attach` is a powerful and quick tool for small debugging or introspection tasks.  

Riak is a complex system and includes many subsystems.  Luckily the REPL shell also provides tab completion!

Let's give it a try, by typing `riak attach` in your shell:

```
$ riak attach
Remote Shell: Use "Ctrl-C a" to quit. q() or init:stop() will terminate the riak node.
Erlang R15B01 (erts-5.9.1) [source] [64-bit] [smp:8:8] [async-threads:0] [kernel-poll:false]

Eshell V5.9.1  (abort with ^G)
(riak@127.0.0.1)1>  erlang:system_info(otp_release).
"R15B01"
```

Just type `riak` at the prompt and hit TAB:

```
(riak@127.0.0.1)1> riak
riak                               riak_api_app                       
riak_api_pb_listener               riak_api_pb_registrar              
riak_api_pb_registration_helper    riak_api_pb_service                
riak_api_pb_sup                    riak_api_stat                      
riak_api_sup                       riak_client                        
riak_control_app                   riak_control_sup                   
riak_core                          riak_core_app                      
riak_core_bucket                   riak_core_capability               
riak_core_cinfo_core               riak_core_claim                    
riak_core_claimant                 riak_core_eventhandler_guard       
riak_core_eventhandler_sup         riak_core_gossip                   
riak_core_handoff_listener         riak_core_handoff_listener_sup     
riak_core_handoff_manager          riak_core_handoff_receiver_sup     
riak_core_handoff_sender           riak_core_handoff_sender_sup       
riak_core_handoff_sup              riak_core_node_watcher             
riak_core_node_watcher_events      riak_core_ring                     
riak_core_ring_events              riak_core_ring_handler             
riak_core_ring_manager             riak_core_send_msg                 

...

```

As you can see Riak is composed of many modules.  We can explore the metadata of these modules and see what methods are exported as well as version information:

```
(riak@127.0.0.1)1> riak_core_app:module_info().
[{exports,[{start,2},
           {prep_stop,1},
           {stop,1},
           {module_info,0},
           {module_info,1}]},
 {imports,[]},
 {attributes,[{vsn,[320905446589353904925710846419343217033]},
              {lager_records,[]},
              {behaviour,[application]}]},
 {compile,[{options,[{outdir,"ebin"},
                     debug_info,warnings_as_errors,
                     {parse_transform,lager_transform},
                     debug_info,
                     {i,"include"}]},
           {version,"4.8.1"},
           {time,{2014,12,4,17,57,6}},
           {source,"/Users/buildbot/masters/riak/riak1_4-build-osx-108-64/build/distdir/riak-1.4.12/deps/riak_core/src/riak_core_app.erl"}]}]
(riak@127.0.0.1)2> m(riak_core_app).
Module riak_core_app compiled: Date: December 4 2014, Time: 17.57
Compiler options:  [{outdir,"ebin"},
                    debug_info,warnings_as_errors,
                    {parse_transform,lager_transform},
                    debug_info,
                    {i,"include"}]
Object file: /Users/johnwhite/Downloads/riak-1.4.12/bin/../lib/riak_core-1.4.10/ebin/riak_core_app.beam
Exports: 
         module_info/0
         module_info/1
         prep_stop/1
         start/2
         stop/1
ok           
```

### riak escript

`riak attach` showed us we can easily gain access and explore a running Riak node.

For more advanced or long running tasks, it might make sense to write a script rather than pasting a bunch of Erlang snippets into the REPL shell.  

Luckily the ERTS comes with the `escript` command which accepts the path to an escript as an argument and then starts up an Erlang node to execute your script.  

Since Riak bundles its own copy of the ERTS, the `riak` command encapsulates escript and allows us to easily execute scripts using the familiar userland tools.

Let's try a simple escript:

**example1.escript**
```
-module(example1).
-compile(export_all).

main(_) ->
	io:format("Current OTP Version: ~p~n.", [erlang:system_info(otp_release)]).
              
```

Save `example1.escript` and try it out:

```
$ riak escript example1.escript 
Current OTP Version: "R15B01"
```

We have seen two examples of ways we can interact with the underlying ERTS of Riak.

In subsequent chapters we will investigate some additional concepts and caveats of these two tools.

