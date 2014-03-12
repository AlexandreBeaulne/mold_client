
-module(mold_client).

-behaviour(gen_server).

-export([start_link/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%% Client API
start_link(Addr, Port, RecoveryPort) ->
    gen_server:start_link(?MODULE, [Addr, Port, RecoveryPort], []).

%%% Server functions
init([_Addr, Port, _RecoveryPort]) ->
    {ok, _Socket} = gen_udp:open(Port, [binary, {active, true}, {reuseaddr, true}]),
    {ok, state}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({udp, _Socket, _Interface, _Port, Packet}, State) ->
    <<Name:10/binary, NS:64, C:16, Msg/binary>> = Packet,
    case Msg of
        <<>> -> % heartbeat, ignore
            {noreply, State};
        _ ->
            PrettyName = string:strip(binary_to_list(Name)),
            Pretty = [X || X <- binary_to_list(Msg), X >= 32, X < 127],
            io:format("~p (SeqID [~p] NumMsgs [~p]): ~p ~n",[PrettyName, NS, C, Pretty]),
            {noreply, State}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    %% No change planned. The function is there for the behaviour,
    %% but will not be used. Only a version on the next
    {ok, State}.

