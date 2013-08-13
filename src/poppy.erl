%% @author Richard
%% @doc @todo In order to build an OTP application we need to construct
%% modules implementing an application functions.


-module(poppy).
-author("kuangyel2000@gmail.com").

-behaviour(application).



%% ====================================================================
%% Behavioural functions
%% ====================================================================
-export([start/2, stop/1]).



%% ====================================================================
%% start/0, start/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/apps/kernel/application.html#Module:start-2">application:start/2</a>
%% -spec start(Type :: normal | {takeover, Node} | {failover, Node}, Args :: term()) ->
%%  	{ok, Pid :: pid()}
%%	  | {ok, Pid :: pid(), State :: term()}
%%	  | {error, Reason :: term()}.
%% ====================================================================
%% start() ->
%%     application:start(sasl),
%%     application:start(?MODULE).

start(_Type, _Args) ->
    poppy_sup:start_link().



%% ====================================================================
%% stop/0, stop/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/apps/kernel/application.html#Module:stop-1">application:stop/1</a>
%% -spec stop(State :: term()) ->  Any :: term().
%% ====================================================================
%% stop() ->
%%     application:stop(?MODULE),
%%     application:stop(sasl).

stop(_State) ->
    ok.

