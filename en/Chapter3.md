# The Tiniest Riak (Erlang) Book

## OTP ~~For The~~ Because I'm Impatient

The Erlang Open Telecom Platform is a book in its own right, in fact they are already written, but we'll give a lightning fast introduction.

Ericsson developed Erlang as the foundation of their new switching platform and in doing so developed and exposed some reusable patterns that grew into what we call the OTP framework.  

These patterns are referred to as behaviors, which you declare as part of your Erlang module:

```
-module(my_module).
-behavior(gen_server).

... 
ALL THE CODE!!! 
...

```

Behaviors require a set of functions that need to be defined and exported in order to satisfy their specification. 

I'll leave the details of that for your own investigations, however, some of the OTP behaviors used by Riak include:

* gen_server

* gen_fsm

* gen_event 

