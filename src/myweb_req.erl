-module(myweb_req).

-export([new/14]).
-export([reply/2]).

-record(http_req, {
	%% Transport.
	socket = undefined :: any(),
	transport = undefined :: undefined | module(),
	connection = keepalive :: keepalive | close,

	%% Request.
	pid = undefined :: pid(),
	method = <<"GET">> :: binary(),
	version = 'HTTP/1.1':: any(),
	peer = undefined :: undefined | {inet:ip_address(), inet:port_number()},
	host = undefined :: undefined | binary(),
	host_info = undefined :: any(),
	port = undefined :: undefined | inet:port_number(),
	path = undefined :: binary(),
	path_info = undefined :: any(),
	qs = undefined :: binary(),
	bindings = undefined :: any(),
	headers = [] :: any(),
	meta = [] :: [{atom(), any()}],

	%% Request body.
	body_state = waiting :: any(),
	buffer = <<>> :: binary(),
	multipart = undefined :: undefined | {binary(), binary()},

	%% Response.
	resp_compress = false :: boolean(),
	resp_state = waiting :: locked | waiting | waiting_stream
		| chunks | stream | done,
	resp_headers = [] :: any(),
	resp_body = <<>> :: any(),

	%% Functions.
	onresponse = undefined :: any()
}).

new(Socket, Transport, Peer, Method, Path, Query, Version,
   Headers, Host, Port, Buffer, CanKeepalive, Compress, OnResponse)->
    Req = #http_req{socket=Socket, transport=Transport, pid=self(), peer=Peer,
		method=Method, path=Path, qs=Query, version=Version,
		headers=Headers, host=Host, port=Port, buffer=Buffer,
		resp_compress=Compress, onresponse=OnResponse},
    case CanKeepalive of
			false->
					Req#http_req{connection=close};
			true ->
					Req#http_req{connection=close}
    end.


reply(State=#http_req{transport = Transport, socket = Socket}, Req)->
	Transport:send(Socket, "this is a test").

reply(Status, Headers, Body, Req=#http_req{
		socket=Socket, transport=Transport,
		version=Version, connection=Connection,
		method=Method, resp_compress=Compress,
		resp_state=RespState, resp_headers=RespHeaders})->
    #http_req{resp_state=done, resp_headers=[], resp_body= <<>>}.
