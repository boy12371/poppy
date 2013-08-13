%% @author Administrator
%% @doc @todo supervisor behaviour callback functions.


-module(poppy_sup).
-author("kuangyel2000@gmail.com").

-behaviour(supervisor).



%% ====================================================================
%% API functions
%% ====================================================================
-export([start_fsm/0, init/1, start_link/0]).

-define(MAX_RESTART, 5).
-define(MAX_TIME,   60).
-define(DEF_PORT, 8086).



%% ====================================================================
%% start_fsm/0 functions
%% A startup function for spawning new client connection handling FSM.
%% To be called by the TCP listener process.
%% ====================================================================
start_fsm() ->
    supervisor:start_child(poppy_fsm_sup, []).



%% ====================================================================
%% Behavioural functions 
%% ====================================================================
start_link() ->
    ListenPort = get_app_env(listen_port, ?DEF_PORT),
    supervisor:start_link(  {local, ?MODULE},
                            ?MODULE,
                            [ListenPort, poppy_fsm]
                         ).



%% ====================================================================
%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/supervisor.html#Module:init-1">supervisor:init/1</a>
%% -spec init(Args :: term()) -> Result when
%% 	Result :: {ok, {SupervisionPolicy, [ChildSpec]}} | ignore,
%% 	SupervisionPolicy :: {RestartStrategy, MaxR :: non_neg_integer(), MaxT :: pos_integer()},
%% 	RestartStrategy :: one_for_all
%% 					 | one_for_one
%% 					 | rest_for_one
%% 					 | simple_one_for_one,
%% 	ChildSpec :: {Id :: term(), StartFunc, RestartPolicy, Type :: worker | supervisor, Modules},
%% 	StartFunc :: {M :: module(), F :: atom(), A :: [term()] | undefined},
%% 	RestartPolicy :: permanent
%% 				   | transient
%% 				   | temporary,
%% 	Modules :: [module()] | dynamic.
%% ====================================================================
init([Port, Module]) ->
    {ok,
        {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
                % TCP Listener
                    % Id       = internal id
                {   poppy_server_sup,
                    % StartFun = {M, F, A}
                    {poppy_ser, start_link, [Port, Module]},
                    % Restart  = permanent | transient | temporary
                    permanent,
                    % Shutdown = brutal_kill | int() >= 0 | infinity
                    2000,
                    % Type     = worker | supervisor
                    worker,
                    % Modules  = [Module] | dynamic
                    [poppy_ser]
                },
                % Client instance supervisor
                {   poppy_fsm_sup,
                    {   supervisor,
                        start_link,
                        [   {local, poppy_fsm_sup},
                            ?MODULE,
                            [Module]
                        ]
                    },
                    % Restart  = permanent | transient | temporary
                    permanent,
                    % Shutdown = brutal_kill | int() >= 0 | infinity
                    infinity,
                    % Type     = worker | supervisor
                    supervisor,
                    % Modules  = [Module] | dynamic
                    []
                }
            ]
        }
    };
init([Module]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
                % TCP Client
                    % Id       = internal id
                {   undefined,
                    % StartFun = {M, F, A}
                    {Module, start_link, []},
                    % Restart  = permanent | transient | temporary
                    temporary,
                    % Shutdown = brutal_kill | int() >= 0 | infinity
                    2000,
                    % Type     = worker | supervisor
                    worker,
                    % Modules  = [Module] | dynamic
                    []
                }
            ]
        }
    }.



%% ====================================================================
%% Internal functions
%% ====================================================================
get_app_env(Opt, Default) ->
    case application:get_env(application:get_application(), Opt) of
        {ok, Val} ->
            Val;
        _ ->
            case init:get_argument(Opt) of
                [[Val | _]] ->
                    Val;
                error       ->
                    Default
            end
    end.

