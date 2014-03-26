
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
    {ok, FH} = file:open("mold_client.log", [write]),
    {ok, {FH, 0}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({udp, _Socket, _Interface, _Port, Packet}, {File, ExpectedSeqID}) ->
    <<Name:10/binary, NS:64, C:16, Msg/binary>> = Packet,
    case Msg of
        <<>> -> % heartbeat, ignore
            {noreply, {File, ExpectedSeqID}};
        _ ->
            case NS of
                ExpectedSeqID ->
                    ok;
                _ ->
                    Msg1= io_lib:format("~p - Dropped packet!!! Expected sequence ID ~p, received ~p~n", [erlang:localtime(), ExpectedSeqID, NS]),
                    file:write(File, Msg1)
            end,
            PrettyName = string:strip(binary_to_list(Name)),
            Pretty = [X || X <- binary_to_list(Msg), X >= 32, X < 127],
            Msg2 = io_lib:format("~p (SeqID [~p] NumMsgs [~p]): ~p ~n",[PrettyName, NS, C, Pretty]),
            file:write(File, Msg2),
            {noreply, {File, NS + C}}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    %% No change planned. The function is there for the behaviour,
    %% but will not be used. Only a version on the next
    {ok, State}.

