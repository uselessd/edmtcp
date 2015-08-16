edmtcp
=====

edmtcp is a simple Erlang port interface that makes use of DMTCP to do
per-process management of application checkpoint/restore images.

By default, DMTCP's unit of granularity is based on handling whole computations
rather than individual processes. edmtcp launches a new coordinator for every launched
binary, with its port number written to /usr/local/share/edmtcp (by default, currently
hardcoded), as well as storing all gzipped checkpoint images there.

No default interval is set, it must be set manually or checkpoints done manually.

It is currently quite rudimentary, and will be seeing significant polish. A more
sophisticated strategy for storing and rotating checkpoint images will be added,
along with more dynamic configuration.

Build
-----

    $ rebar3 compile

Example session
-----

   Erlang/OTP 18 [erts-7.0] [source] [async-threads:0] [hipe] [kernel-poll:false]

   Eshell V7.0  (abort with ^G)
   
   1> application:start(edmtcp).
   ok
   2> edmtcp:launch("/bin/cat").
   #Port<0.15755>
   3> edmtcp:status("/bin/cat").
   Coordinator:
      Host: localhost
      Port: 58546
   Status...
      NUM_PEERS=1
      RUNNING=yes
      CKPT_INTERVAL=0 (checkpoint manually)
   ok
   4> edmtcp:set_interval("/bin/cat", 100).
   Interval changed to 100
   ok
   5> edmtcp:terminate("/bin/cat", quit). %% setting second arg to [] calls --kill
   ok
   6> edmtcp:flush_share().
   ok
