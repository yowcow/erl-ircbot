%%%-------------------------------------------------------------------
%% @doc erl_ircclient public API
%% @end
%%%-------------------------------------------------------------------

-module(erl_ircclient_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    lager:start(),
    erl_ircclient_sup:start_link().

stop(_State) ->
    ok.
