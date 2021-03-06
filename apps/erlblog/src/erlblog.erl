-module(erlblog).
-include("erlblog.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([ping/0]).
-export([getlist/1, set/2, setlist/2]).

%% Public API

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, erlblog),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, erlblog_vnode_master).

%% @doc Gets list from a random vnode to renders it 
-spec getlist(term()) -> [binary()].
getlist(Key) ->
    DocIdx = riak_core_util:chash_key({term_to_binary(static), term_to_binary(Key)}),
    [Pref] = riak_core_apl:get_apl(DocIdx, 1, erlblog),
    riak_core_vnode_master:sync_command(Pref, {getlist, Key}, erlblog_vnode_master).

%% @doc Sets a message into db
-spec set(term(), term()) -> ok.
set(Key, Msg) ->
    DocIdx = riak_core_util:chash_key({term_to_binary(static), term_to_binary(Key)}),
    [Pref] = riak_core_apl:get_apl(DocIdx, 1, erlblog),
    riak_core_vnode_master:sync_command(Pref, {set, Key, Msg}, erlblog_vnode_master).

%% @doc Sets a message into db
-spec setlist(term(), [term()]) -> ok.
setlist(Key, Msgs) ->
    DocIdx = riak_core_util:chash_key({term_to_binary(static), term_to_binary(Key)}),
    [Pref] = riak_core_apl:get_apl(DocIdx, 1, erlblog),
    riak_core_vnode_master:sync_command(Pref, {setlist, Key, Msgs}, erlblog_vnode_master).
