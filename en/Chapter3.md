# The Tiniest Riak (Erlang) Book

## OTP ~~For The~~ Because I'm Impatient

The Erlang Open Telephony Platform is a book in its own right, in fact they are already written, but we'll give a lightning fast introduction.

Ericsson developed Erlang as the foundation for its switching platform and in doing so created some reusable patterns that are the basis of the OTP framework.  These patterns take the form of behaviors, which you define as part of your Erlang module declaration:

```
-module(my_module).
-behavior(gen_server).

... 
ALL THE CODE!!! 
...

```

Behaviors define a set of functions that need to be defined and exported in order to adhere to their specification, but we'll leave it at that and you can go investigate behavior creation on your own.

Some examples of OTP behaviors used in Riak are:

* gen_server

* gen_fsm

* gen_event 

