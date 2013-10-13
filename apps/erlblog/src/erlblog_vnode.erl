-module(erlblog_vnode).
-behaviour(riak_core_vnode).
-include("erlblog.hrl").

-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_coverage/4,
         handle_exit/3]).

-record(state, {
	  partition
	  , dict
	  , backup_file
	 }).

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
    process_flag(trap_exit, true),
    {ok, Backup} = application:get_env(erlblog, backup_file),
    Dict =
    	case file:read_file(Backup) of
    	    {error, _} -> 
		dict:new();
    	    {ok, Data} -> 
		Dict0 = dict:new(),
		dict:store(test, binary_to_term(Data), Dict0)
    	end,
    {ok, #state {
       partition=Partition
       , dict=Dict
       , backup_file=Backup
      }}.

handle_command({set, Key, Msg}, _Sender, #state{dict=Dict, backup_file=BFile}=State) ->
    Dict1 =
	case dict:find(Key, Dict) of
            error ->
                dict:append(Key, Msg, Dict);
            {ok, _Found} ->
		dict:append_list(Key, [Msg], Dict)
        end,
    case Key =:= test of
	true ->
	    {ok, Data} = dict:find(test, Dict1),
	    lager:info("Writing data is: ~p", [Data]),
	    ok = file:write_file(BFile, term_to_binary(Data));
	false ->
	    error
    end,
    ?PRINT({set, Msg, Dict1}),
    {reply, ok, State#state{dict=Dict1}};

handle_command({setlist, Key, Msgs}, _Sender, #state{dict=Dict0}=State) ->
    Dict1 = dict:erase(Key, Dict0),
    Dict2 = dict:store(Key, Msgs, Dict1),
    ?PRINT({setlist, Msgs, Dict2}),
    {reply, ok, State#state{dict=Dict2}};

handle_command({getlist, Key}, _Sender, #state{dict=Dict}=State) ->
    Data =
	case dict:find(Key, Dict) of
            error ->
		[];
            {ok, Found} ->
                Found
        end,
    ?PRINT({getlist, Data}),
    {reply, Data, State};

%% Sample command: respond to a ping
handle_command(ping, _Sender, State) ->
    {reply, {pong, State#state.partition}, State};
handle_command(Message, _Sender, State) ->
    ?PRINT({unhandled_command, Message}),
    {noreply, State}.

handle_handoff_command(Message, Sender, State) ->
    lager:info("Handle_handoff_command gets message: ~p from sender: ~p.", [Message, Sender]),
    {noreply, State}.

handoff_starting(TargetNode, State) ->
    lager:info("VNODE CALL ~p handoff_starting with target node: ~p.", [self(), TargetNode]),
    {true, State}.

handoff_cancelled(State) ->
    lager:info("VNODE CALL ~p handoff_cancelled executed.", [self()]),
    {ok, State}.

handoff_finished(TargetNode, State) ->
    lager:info("VNODE CALL ~p handoff_finished with target node: ~p.", [self(), TargetNode]),
    {ok, State}.

handle_handoff_data(Data, #state{dict=Dict0}=State) ->
    lager:info("VNODE CALL ~p handle_handoff_data gets data: ~p.", [self(), Data]),
    {Key, Val} = binary_to_term(Data),
    Dict1 = dict:store(Key, Val, Dict0),
    {reply, ok, State#state{dict=Dict1}}.

encode_handoff_item(_ObjectName, _ObjectValue) ->
    <<>>.

is_empty(State) ->
    lager:info("VNODE CALL ~p is_empty executed.", [self()]),
    case dict:size(State#state.dict) of
        0 -> {true, State};
        _ -> {false, State}
    end.

delete(State) ->
    lager:info("VNODE CALL ~p delete executed.", [self()]),
    {ok, State}.

handle_coverage(Req, KeySpaces, Sender, State) ->
    lager:info("VNODE CALL ~p handle_coverage request: ~p, keyspaces: ~p, sender: ~p.", [self(), Req, KeySpaces, Sender]),
    {stop, not_implemented, State}.

handle_exit(Pid, Reason, State) ->
    lager:info("VNODE CALL ~p handle_exit with pid: ~p and reason: ~p.", [self(), Pid, Reason]),
    {noreply, State}.

terminate(Reason, #state{partition=Partition}=_State) ->
    lager:info("VNODE CALL ~p terminate with reason: ~p and partition: ~p.", [self(), Reason, Partition]),
    ok.
