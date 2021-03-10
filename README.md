file_appender
=====

An OTP application

Build
-----

    $ rebar3 compile

TASK
_____

Write file appender. It will get a file path and string to append and will append it to the file in a newline.
if in 10 seconds nothing was appended to the file, the file will be closed.
We will check the task by running it in erlang console (will run `rebar3 shell` to open it and from inside we will call your function).﻿

Remarks-
Use OTP
Add tests (we will check by running `rebar3 eunit`)
share your code in git
include a clear README file that will describe how to build and use the API
You can use https://learnyousomeerlang.com/ for some assistance  
