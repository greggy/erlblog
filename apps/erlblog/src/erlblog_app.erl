-module(erlblog_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case erlblog_sup:start_link() of
        {ok, Pid} ->
            ok = riak_core:register([{vnode_module, erlblog_vnode}]),
            ok = riak_core_ring_events:add_guarded_handler(erlblog_ring_event_handler, []),
            ok = riak_core_node_watcher_events:add_guarded_handler(erlblog_node_event_handler, []),
            ok = riak_core_node_watcher:service_up(erlblog, self()),
            %% Start cowboy sup
            ok = erlblog_cowboy_app:start(_StartType, _StartArgs),
	    ok = lager:start(),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.
