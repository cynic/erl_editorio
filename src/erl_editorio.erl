-module(erl_editorio).
-behaviour(application).
-export([start/2, stop/1]).
-export([getch/0, clear/0]).
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
    Value = getch_internal(),
    if
        Value == timeout ->
            getch();
        Value == unknown ->
            {error, unknown_input};
        true ->
            {ok, Value}
    end.

clear() ->
    erlang:nif_error(nif_not_loaded).