%%%-------------------------------------------------------------------
%% @doc erl_ircbot public API
%% @end
%%%-------------------------------------------------------------------

-module(erl_ircbot_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    lager:start(),
    erl_ircbot_sup:start_link().

stop(_State) ->
    ok.
