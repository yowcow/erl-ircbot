%%%-------------------------------------------------------------------
%% @doc erl_ircbot top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(erl_ircbot_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 1,
        period => 5
    },
    ChildSpec = [
        #{
            id => ircbot,
            start => {ircbot, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => worker,
            modules => [ircbot]
        }
    ],
    {ok, {SupFlags, ChildSpec}}.
