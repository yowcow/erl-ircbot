erl-ircbot
==========

IRC client bot implemented over OTP application/supervisor/gen_server.


HOW TO BUILD/TEST/RELEASE/RUN
-----------------------------

    $ make clean all test release shell

HOW TO START/STOP TEST IRC SERVER
---------------------------------

    $ make -f docker.mk start stop
