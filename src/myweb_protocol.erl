-module(myweb_protocol).
-export([start_link/4, init/4]).

-record(state, {
	socket:: inet:socket(),
	transport:: module(),
	middlewares:: [module()],
	compress:: boolean(),
	env:: any(),
	onresponse :: any(),
	max_empty_lines :: non_neg_integer(),
	req_keepalive = 1 :: non_neg_integer(),
	max_keepalive :: non_neg_integer(),
	max_request_line_length :: non_neg_integer(),
	max_header_name_length :: non_neg_integer(),
	max_header_value_length :: non_neg_integer(),
	max_headers :: non_neg_integer(),
	timeout :: timeout(),
	until :: non_neg_integer() | infinity
}).

start_link(Ref, Socket, Transport, Opts)->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, Socket, Transport, Opts)->
    ok = ranch:accept_ack(Ref),
    Timeout = get_value(timeout, Opts, 5000),
    Until = until(Timeout),
    log4erl:info("init stat..."),

    case recv(Socket, Transport, Until) of
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


parse_request(<<$\n, _/bits>>, State, _)->
    error_terminate(400, State);
parse_request(<< $\s, _/bits >>, State, _) ->
    error_terminate(400, State);
parse_request(Buffer, State, ReqEmpty) ->
    log4erl:info("buffer is ~p", [Buffer]),
    case match_eol(Buffer, 0) of
			nomatch->
					error_terminate(400, State);
			N ->
					log4erl:info("match eol is ~p", [N]),
					parse_method(Buffer, State, <<>>)
    end.

parse_method(<< C, Rest/bits >>, State, Sofar)->
    case C of
	$\r->error_terminate(400, State);
	$\s ->
	    log4erl:info("parse method ~p", [Sofar]),
	    parse_uri(Rest, State, Sofar);
	_ ->parse_method(Rest, State, <<Sofar/binary, C>>)
    end.

parse_uri(<<$\r, _/bits>>, State, _)->
    error_terminate(400, State);
parse_uri(<<$\s, _/bits>>, State, _) ->
    error_terminate(400, State);
parse_uri(<<"* ", Rest/bits>>, State, Methos) ->
    log4erl:info("parse uri * ~p", [Rest]);
parse_uri(Buffer, State, Method) ->
    parse_uri_path(Buffer, State, Method, <<>>).

parse_uri_path(<<C, Rest/bits>>, State, Method, Sofar)->
    log4erl:info("parse uri path ~p", [C]),
    case C of
	$\r->
	    error_terminate(400, State);
	$\s ->
	    log4erl:info("start parser version"),
	    parse_version(Rest, State, Method, Sofar, <<>>);
	$? ->
	    parse_uri_query(Rest, State, Method, Sofar, <<>>);
	_ ->
	    parse_uri_path(Rest, State, Method, <<Sofar/binary, C>>)
    end.

parse_uri_query(<<C, Rest/bits>>, S, M, P, Sofar)->
    log4erl:info("parse uri query"),
    case C of
	$\r->
	    error_terminate(400, S);
	$\s ->
	    parse_version(Rest, S, M, P, Sofar);
	_ ->
	    parse_uri_query(Rest, S, M, P, <<Sofar/binary, C>>)
    end.

parse_version(<<"HTTP/1.1\r\n", Rest/bits>>, S, M, P, Q)->
    parse_header(Rest, S, M, P, Q, 'HTTP/1.1\r\n', []);
parse_version(<<"HTTP/1.0\r\n", Rest/bits>>, S, M, P, Q) ->
    parse_header(Rest, S, M, P, Q, 'HTTP/1.0\r\n', []);
parse_version(_, State, _, _, _) ->
    error_terminate(505, State).

parse_header(<<$\r, $\n, Rest/bits>>, S, M, P, Q, V, Headers)->
    request(Rest, S, M, P, Q, V, lists:reverse(Headers));
parse_header(B, S, M, P, Q, V, Headers) ->
    request(B, S,M, P, Q, V, lists:reverse(Headers)).

request(Buffer, State=#state{transport = Transport}, M, P, Q, Version, Headers)->
	request(Buffer, State, M, P, Q, Version, Headers, <<>>, default_port(Transport:name())).

default_port(ssl) -> 443;
default_port(_) -> 80.

request(Buffer, State=#state{socket=Socket, transport=Transport,
	req_keepalive=ReqKeepalive, max_keepalive=MaxKeepalive,
	compress=Compress, onresponse=OnResponse},
		Method, Path, Query, Version, Headers, Host, Port)->
	case Transport:peername(Socket) of
		{ok, Peer}->
			Req = myweb_req:new(Socket, Transport, Peer, Method, Path,
				Query, Version, Headers, Host, Port, Buffer,
				ReqKeepalive < MaxKeepalive, Compress, OnResponse),
			execute(Req, State);
		{error, _}->
			terminate(State)
	end.

execute(Req, State=#state{middlewares = Middlewares, env=Env})->
	execute(Req, State, Env, Middlewares).

execute(Req, State, Env, [Middlewares|Tail])->
	myweb_req:reply(Req, State).

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

error_terminate(_Status, State) ->
	terminate(State).

terminate(#state{socket=Socket, transport=Transport})->
    Transport:close(Socket),
    ok.
