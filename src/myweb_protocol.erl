-module(myweb_protocol).
-export([start_link/4, init/4]).

-record(state, {
	  socket:: inet:socket(),
	  transport:: module(),
	  middlewares:: [module()],
	  compress:: boolean(),
	  env:: cowboy_middleware:env(),
	  onresponse = undefined :: undefined | cowboy:onresponse_fun(),
	max_empty_lines :: non_neg_integer(),
	req_keepalive = 1 :: non_neg_integer(),
	max_keepalive :: non_neg_integer(),
	max_request_line_length :: non_neg_integer(),
	max_header_name_length :: non_neg_integer(),
	max_header_value_length :: non_neg_integer(),
	max_headers :: non_neg_integer(),
	timeout :: timeout(),
	until :: non_neg_integer() | infinity
})

start_link(Ref, Socket, Transport, Opts)->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, Socket, Transport, Opts)->
    ok = ranch:accept_ack(Ref),
    Timeout = get_value(timeout, Opts, 5000),
    Until = until(Timeout),
    log4erl:info("init stat..."),
    log4erl:info("timeout is ~p", [Timeout]),
    %%loop(Socket, Transport, Opts).
    case recv(Socket, Transport, Uutil) of
	{ok, Data}->
	    OnFirstRequest = get_value(onfirstrequest, Opts, undefined),
		 case OnFirstRequest of
		      undefined -> ok;
		      _ -> OnFirstRequest(Ref, Socket, Transport, Opts)
		 end,
	    Compress = get_value(compress, Opts, false),
	    MaxEmptyLines = get_value(max_empty_lines, Opts, 5),
	    MaxHeaderNameLength = get_value(max_header_name_length, Opts, 64),
	    MaxHeaderValueLength = get_value(max_header_value_length, Opts, 4096),
	    MaxHeaders = get_value(max_headers, Opts, 100),
	    MaxKeepalive = get_value(max_keepalive, Opts, 100),
	    MaxRequestLineLength = get_value(max_request_line_length, Opts, 4096),
	    Middlewares = get_value(middlewares, Opts, [cowboy_router, cowboy_handler]),
	    Env = [{listener, Ref}|get_value(env, Opts, [])],
	    OnResponse = get_value(onresponse, Opts, undefined),
	    parse_request(Data, #state{socket=Socket, transport=Transport,
				middlewares=Middlewares, compress=Compress, env=Env,
				max_empty_lines=MaxEmptyLines, max_keepalive=MaxKeepalive,
				max_request_line_length=MaxRequestLineLength,
				max_header_name_length=MaxHeaderNameLength,
				max_header_value_length=MaxHeaderValueLength, max_headers=MaxHeaders,
				onresponse=OnResponse, timeout=Timeout, until=Until}, 0);
	{error, _} ->
	    terminate(#state{socket=Socket, transport=Transport})
    end.

recv(Socket, Transport, infinity)->
    Transport:recv(Socket, 0, infinity);
recv(Socket, Transport, Until) ->
    Timeout = Until - erlang:monotonic_time(milli_seconds),
    if Timeout<0 ->
	    {error, timeout};
       true ->
	    Transport:recv(Socket, 0, Timeout)
    end.

parse_request(<<$\n, _/bits>>, State)->
    error_terminate(400, State);
parse_request(<< $\s, _/bits >>, State) ->
    error_terminate(400, State);
parse_request(Buffer, State=#state{max_request_line_length=MaxLength,
		max_empty_lines=MaxEmpty}, ReqEmpty) ->
    ok.

match_eol(<<$\n, _/bits>>, N)->
    N;
match_eol(<<_, Rest/bits>>, N) ->
    match_eol(Rest, N+1);
match_eol(_, _) ->
    nomatch.

get_value(Key, Opts, Default)->
    case lists:keyfind(Key, 1, Opts) of
	{_, Value} -> Value;
	_ -> Default
    end.

until(infinity)->
    infinity;
until(Timeout) ->
    erlang:monotonic_time(milli_seconds) + Timeout.

loop(Socket, Transport, Opts)->
    case Transport:recv(Socket, 0, 5000) of
	{ok, Data} ->
	    log4erl:info("loop start..."),
	    log4erl:info("in one request, opts is ... ~p", [Opts]),
	    Transport:send(Socket, Data),
	    loop(Socket, Transport, Opts);
	_ ->
	    ok = Transport:close(Socket)
    end.

error_terminate(Status, State=#state{socket=Socket, transport=Transport,
		compress=Compress, onresponse=OnResponse}) ->
	error_terminate(Status, myweb_req:new(Socket, Transport,
		undefined, <<"GET">>, <<>>, <<>>, 'HTTP/1.1', [], <<>>,
		undefined, <<>>, false, Compress, OnResponse), State).

error_terminate(Status, Req, State) ->
	_ = myweb_req:reply(Status, Req),
	terminate(State).

terminate(#state{socket=Socket, transport=Transport})->
    Transport:close(Socket),
    ok.
