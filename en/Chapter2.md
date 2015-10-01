# The Tiniest Riak (Erlang) Book


## Application Configuration 

The Erlang Runtime System and release tools also provide an integrated application configuration subsystem that generates and references an `app.config`<sup>1</sup> file for configuration directives.  `app.config` is simply a proplist of Erlang terms that get mapped to an Erlang ETS table when Riak is starting up.

**/etc/riak/app.config**
```
[
 	...	
	{riak_core, [             
              {ring_state_dir, "./data/ring"},
              {ring_creation_size, 64},
              {http, [ {"127.0.0.1", 8098 } ]},
              {https, [{ "127.0.0.1", 8098 }]}
              ]
    },              
	...
]
```

Cool! Baked in application configuration, no more flirtation with reinventing the wheel, because we can do INI, XML, YAML, etc. so much better.  

What is even better is we have access to this information from guess where?  

**riak attach**:

```
Eshell V5.9.1  (abort with ^G)
(riak@127.0.0.1)1> application:get_env(riak_core, ring_creation_size).
{ok,64}
(riak@127.0.0.1)2> application:set_env(riak_core, my_variable, [{does, nothing}]).
ok
(riak@127.0.0.1)3> application:get_env(riak_core, my_variable).
{ok,[{does,nothing}]}
```

So this adds yet another tool to our growing toolbox, as we can actually modify certain Riak configuration options at runtime.  

Not all configuration options are dynamic, but a lot are and being able to alter settings at runtime can be useful in debugging, tuning, etc.  

Which specific Riak configuration options are dynamic is beyond the scope of this document, so hit the docs<sup>2</sup>.

The vm.args file is used specifically for initializing and tuning different settings of the Erlang VM Riak runs in.

--

<sup>1</sup> *Starting with Riak 2.0+, the new cuttlefish configuration system was introduced which provides a more simplified `sysctl` like configuration file as well as granular schema definition and validation.  However, under the hood cuttlefish transforms `riak.conf` into `app.config` and `vm.args` files.*

<sup>2</sup> [*Official Basho Documentation*](http://docs.basho.com) 
