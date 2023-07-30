-module(erl_editorio).
-behaviour(application).
-export([start/2, stop/1]).
-export([getch/0, clear/0]).
-on_load(init/0).

init() -> 
    erlang:load_nif("./erl_editorio", 0).

start(_Type, _Args) ->
    ok.

stop(_State) ->
    ok.

getch() -> 
    erlang:nif_error(nif_not_loaded).

clear() ->
    erlang:nif_error(nif_not_loaded).