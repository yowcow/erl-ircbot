-module(ircclient).

-behavior(gen_server).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).

-export([
    start_link/0,
    stop/0,
    notice/2
]).

-define(RECONNECT_INTERVAL, 15 * 1000).

init([]) ->
    process_flag(trap_exit, true),
    {ok, SvrConf} = application:get_env(erl_ircclient, server),
    {ok, IRCConf} = application:get_env(erl_ircclient, irc),
    {ok, M, Sock} = reconnect(SvrConf, IRCConf),
    lager:info("ircclient initialized"),
    {ok, #{
        svr_conf => SvrConf,
        irc_conf => IRCConf,
        mod => M,
        sock => Sock
    }}.

terminate(_Reason, #{mod := M, sock := Sock}) ->
    process_flag(trap_exit, false),
    Ret = M:close(Sock),
    lager:info("ircclient terminated"),
    Ret.

handle_call(Req, _From, State) ->
    lager:info("handle_call: ~p", [Req]),
    {reply, ok, State}.

handle_cast({notice, Target, Msg}, #{mod := M, sock := Sock} = State) ->
    send_notice(M, Sock, Target, Msg),
    {noreply, State};
handle_cast(Req, State) ->
    lager:info("handle_cast: ~p", [Req]),
    {noreply, State}.

handle_info({tcp_closed, _} = Req, #{svr_conf := SvrConf, irc_conf := IRCConf} = State) ->
    lager:error("ircclient disconnected: tcp_closed"),
    timer:sleep(5000),
    {ok, M, Sock} = reconnect(SvrConf, IRCConf),
    lager:info("ircclient reconnected"),
    {noreply, State#{
        mod => M,
        sock => Sock
    }};
handle_info({tcp, Sock, Msg}, #{mod := M, sock := Sock} = State) ->
    Trimmed = string:trim(Msg),
    case string:split(Trimmed, " ", all) of
        %% PING :HOST
        ["PING", [_ |Host] | _] ->
            send_pong(M, Sock, Host),
            {noreply, State};
        %% _ KICK Chan
        [_, "KICK", Chan | _] ->
            send_join(M, Sock, Chan),
            {noreply, State};
        %% _ PRIVMSG
        [_, "PRIVMSG" | _] ->
            {noreply, State};
        %% _ NOTICE
        [_, "NOTICE" | _] ->
            {noreply, State};
        %% _ JOIN
        [_, "JOIN" | _] ->
            {noreply, State};
        In ->
            lager:info("handle_info: ~p~n", [Trimmed]),
            {noreply, State}
    end.

code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%=== public ===%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

notice(Target, Msg) ->
    gen_server:cast(?MODULE, {notice, Target, Msg}).


%%=== private ===%%

connect({Host, Port, false}, IRCConf) ->
    connect(gen_tcp, {Host, Port}, IRCConf).

connect(M, {Host, Port}, IRCConf) ->
    case M:connect(Host, Port, [], infinity) of
        {ok, Sock} ->
            ok = send_init(M, Sock, IRCConf),
            {ok, M, Sock};
        _ -> not_connected
    end.

reconnect(SvrConf, IRCConf) ->
    case connect(SvrConf, IRCConf) of
        {ok, _, _} = Ret -> Ret;
        _ ->
            lager:error("reconnecting after ~p ms", [?RECONNECT_INTERVAL]),
            timer:sleep(?RECONNECT_INTERVAL),
            reconnect(SvrConf, IRCConf)
    end.

send_init(M, Sock, {Nick, Chan}) ->
    ok = M:send(Sock, "NICK " ++ Nick ++ "\r\n"),
    ok = M:send(Sock, "USER " ++ Nick ++ " " ++ Nick ++ " nohost :" ++ Nick ++ "\r\n"),
    ok = send_join(M, Sock, Chan),
    ok.

send_join(M, Sock, Chan) ->
    M:send(Sock, "JOIN " ++ Chan ++ "\r\n").

send_pong(M, Sock, Host) ->
    M:send(Sock, "PONG " ++ Host ++ "\r\n").

send_notice(M, Sock, Target, Msg) ->
    M:send(Sock, "NOTICE " ++ Target ++ " :" ++ Msg ++ "\r\n").
