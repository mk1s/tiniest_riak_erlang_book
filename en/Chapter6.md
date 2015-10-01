# Tiniest Riak (Erlang) Book

## Process Creation and Message Passing

We have stated that Erlang allows one to develop highly concurrent systems that can create and run a large number of lightweight processes.

As we spawn more and more processes, ultimately we are probably going to want these processes to communicate with one another.  

Erlang utilizes message passing for Interprocess Communication(IPC).  This allows processes to communicate locally on a node or across a LAN to a another cluster member.

For example when a PUT operation is received by a Riak node, it will be coordinated to the N nodes in the preflist for that {Bucket, Key} hash (N-1 if the hash is owned by the coordinating Riak node).  

In very simplistic terms, this is ultimately accomplished using the message passing capabilities provided by Erlang.

### Process Creation

Up to this point we have focused on inspecting Riak and Erlang processes that are already running.  Sure we have started a process each time we use `riak attach` but how do we actually create a simple process?

via `riak attach`:

```
Eshell V5.9.1  (abort with ^G)
(riak@127.0.0.1)1> Pid = spawn(fun() -> io:format("Johnny Five is Alive!!~n~n", []), sleep(5000) end). 
Johnny Five is Alive!!

<0.3449.7>
```

Process creation is that simple!  

We called spawn with a simple anonymous function as an argument and it returned the PID of the new process.

### Message Passing and Process Mailboxes

So we have seen the basics on inspecting currently running processes in Erlang.  We have also created some processes of our own. 

Now lets dive a bit deeper and see how they really work especially in the scope of a concurrent distributed system.

Let's start by sending a simple message to the PID of our REPL shell:

```
Eshell V5.9.1  (abort with ^G)
(riak@127.0.0.1)1> Me = self().
<0.18154.1>
(riak@127.0.0.1)2> Me ! "Hello".
"Hello"
(riak@127.0.0.1)3> erlang:process_info(Me).
[
 ...
 {message_queue_len,1},
 {messages,["Hello"]},
 ...
]
```

So that was easy, and we see from inspecting the process that we have 1 queued message called hello.  

Let's send a few more messages:

```
(riak@127.0.0.1)4> Me ! "Are you there?".
"Are you there?"
(riak@127.0.0.1)5> Me ! "Why aren't you answering me?".
"Why aren't you answering me?"
(riak@127.0.0.1)6> erlang:process_info(Me).
[
 ...
 {message_queue_len,3},
 {messages,["Hello","Are you there?",
            "Why aren't you answering me?"]},
 ...
]
```

Huh, so the message queue is building and we can see all of those messages hanging around.  

So, how do we actually do something with these messages?

```
Eshell V5.9.1  (abort with ^G)
(riak@127.0.0.1)1> f().
ok
(riak@127.0.0.1)2> Me = self().
<0.7884.2>
(riak@127.0.0.1)3> Me ! "Hello".
"Hello"
(riak@127.0.0.1)4> erlang:process_info(Me).
[
 ...
 {message_queue_len,1},
 {messages,["Hello"]},
 ...
 ]
(riak@127.0.0.1)5> receive Msg -> io:format("Message Received: ~p~n", [Msg]) end.
Message Received: "Hello"
ok
(riak@127.0.0.1)6> erlang:process_info(Me).
[
...
 {message_queue_len,0},
 {messages,[]},
...
]
```

So in the second example by adding a receive block, we see that the message is consumed and then removed from the process mailbox.  

Let's try a more complicated example using two `riak attach` sessions.

From Session A:

```
(riak@127.0.0.1)1> ProcA = self().
<0.10529.2>
```

From Session B:

```
Eshell V5.9.1  (abort with ^G)
(riak@127.0.0.1)1> ProcB = self().
<0.10394.2>
```

From Session A:

```
B = list_to_pid("<0.10394.2>").
B ! {ProcA, "Are you there?"}.
receive 
	{From, Msg} ->
		io:format("~p responded: ~p~n", [From, Msg])
end.
```

From Session B:

```
receive 
	{From, _Msg} -> 
		io:format("**Sigh**, Yes I'm here.~n", []), 
		From ! {ProcB, "What do you want?"}
end.
```

So in the third example we demonstrated message passing between two separate processes and how we can reply back to them by supplying our PID.  

While these examples are simplistic, we can see the power and ease of Erlang's message passing.

Riak creates a lot of processes and heavily relies on message passing between local and remote processes.

**Look familiar?**
```
-module(riak_kv_util).
...

overload_reply({raw, ReqId, Pid}) ->
    Pid ! {ReqId, {error, overload}};
overload_reply(_) ->
    ok.
...

```

So in this chapter we have seen that processes are easily created and can easily communicate with one another.
