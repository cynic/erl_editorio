-module(erl_editorio).
-behaviour(application).
-export([getch/0, clear/0, start/2, stop/1]).
-on_load(init/0).

init() ->
    Path = filename:dirname(code:which(?MODULE)) ++ "/../priv",
    Load = erlang:load_nif(Path ++ "/erl_editorio", 0),
    case Load of
        ok -> ok;
        {error, {Reason,Text}} -> io:format("Load of shared library (erl_editorio) failed. ~p:~p~n", [Reason, Text])
    end,
    ok.

start(_Type, _Args) ->
    ok.

stop(_State) ->
    ok.

getch_internal() -> 
    erlang:nif_error(nif_not_loaded).

getch() -> 
    case getch_internal() of
        timeout ->
            getch();
        [unknown, 126] ->
            {ok, delete};
        127 ->
            {ok, backspace};
        [unknown, KeyValue] ->
            {error, unknown_input, KeyValue};
        V when erlang:is_atom(V) ->
            {ok, V};
        V ->
            {ok, [V]}
    end.

% clear_internal() ->
%     erlang:nif_error(nif_not_loaded).

clear() ->
    io:format("\e[2J\e[H\e[0;0f").