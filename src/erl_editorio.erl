-module(erl_editorio).
-export([getch/0, clear/0]).
-on_load(init/0).

init() -> 
    erlang:load_nif("./erl_editorio", 0).

getch() -> 
    erlang:nif_error(nif_not_loaded).

clear() ->
    erlang:nif_error(nif_not_loaded).