-module(erlblog_cowboy_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

-define(DEFAULT_PORT, 9090).
-define(DEFAULT_LISTENERS, 10).


%% API.
start(_Type, _Args) ->
        Dispatch = cowboy_router:compile([
                {'_', [
                        {"/", cowboy_static, [
                                {directory, {priv_dir, erlblog, []}},
                                {file, <<"index.html">>},
                                {mimetypes, [{<<".html">>, [<<"text/html">>]}]}
                        ]},
                        {"/websocket", websockets_handler, []},
                        {"/static/[...]", cowboy_static, [
                                {directory, {priv_dir, erlblog, [<<"static">>]}},
                                {mimetypes, [{<<".js">>, [<<"application/javascript">>]}]}
                        ]}
                ]}
        ]),
    {ok, Port} = application:get_env(erlblog, cowboy_port),
    {ok, Listeners} = application:get_env(erlblog, cowboy_listeners),
    {ok, _} = cowboy:start_http(http, Listeners, [{port, Port}],
				[{env, [{dispatch, Dispatch}]}]),
    io:format("Cowboy starts on port ~p\n", [Port]).

stop(_State) ->
    ok.
