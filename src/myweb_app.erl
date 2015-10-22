-module(myweb_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    application:start(log4erl),
    log4erl:conf("/home/zsf/test/erl_test/myweb/priv/log4erl.conf"),
    {ok, _} = ranch:start_listener(myweb, 1, ranch_tcp, [{port, 5555}], myweb_protocol, []),
    myweb_sup:start_link().

stop(_State) ->
    ok.
