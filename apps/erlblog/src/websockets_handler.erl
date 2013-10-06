-module(websockets_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

-include("erlblog.hrl").

-define(ROOM_ID, test).
-define(USERS, users).


init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    ?PRINT(["User's pid is ", self()]),
    ok = erlblog:set(?USERS, self()),
    erlang:start_timer(1000, self(), <<"Hello!">>),
    Msgs = erlblog:getlist(?ROOM_ID),
    [erlang:start_timer(1010, self(), Msg) || Msg <- Msgs],
    {ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
    ok = erlblog:set(?ROOM_ID, Msg),
    Users = erlblog:getlist(?USERS),
    [UserPID ! {text, Msg} || UserPID <- Users],
    {reply, {text, Msg}, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}. 

websocket_info({timeout, _Ref, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};
websocket_info({text, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
    Users0 = erlblog:getlist(?USERS),
    Users1 = users_list(Users0, self()),
    ok = erlblog:setlist(?USERS, Users1),
    ok.


%% @private
users_list(Users, Pid) ->
    users_list(Users, [], Pid).

users_list([UserPID | Users0], Users1, Pid) ->
    case UserPID =:= Pid of
	true ->
	    users_list(Users0, Users1, Pid);
	_ ->
	    users_list(Users0, [UserPID | Users1], Pid)
    end;
users_list([], Users, _) ->
    Users.
