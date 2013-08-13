%% @author Richard
%% @doc @todo The gen_tcp module is that it only exports interface to
%% a blocking accept call.


-module(poppy_ser).
-author("kuangyel2000@gmail.com").

-behaviour(gen_server).



%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/2]).
-export([   init/1, handle_call/3, handle_cast/2, handle_info/2, 
            terminate/2, code_change/3
        ]).


%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state,
            {    listener, % Listening socket
                 acceptor, % Asynchronous acceptor's internal reference
                 module    % FSM handling module
            }
       ).



%% ====================================================================
%% Func: start_link/2
%% @spec (Port::integer(), Module) -> {ok, Pid} | {error, Reason}
%
%% @doc Called by a supervisor to start the listening process.
%% @end
%% ====================================================================
start_link(Port, Module) when is_integer(Port), is_atom(Module) ->
    gen_server:start_link(  {local, ?MODULE},
                            ?MODULE,
                            [Port, Module],
                            []
                         ).



%% ====================================================================
%% Callback functions from gen_server
%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">gen_server:init/1</a>
%% -spec init(Args :: term()) -> Result when
%% 	Result :: {ok, State}
%% 			| {ok, State, Timeout}
%% 			| {ok, State, hibernate}
%% 			| {stop, Reason :: term()}
%% 			| ignore,
%% 	State :: term(),
%% 	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init([Port, Module]) ->
    process_flag(trap_exit, true),
    Opts = [    binary,
                {packet, 2},
                {reuseaddr, true},
                {keepalive, true},
                {backlog, 30},
                {active, false}
           ],
    case gen_tcp:listen(Port, Opts) of
        {ok, Listen_socket} ->
            %% Create first accepting process
            {ok, Ref} = prim_inet:async_accept(Listen_socket, -1),
            {ok, #state
                {   listener = Listen_socket,
                    acceptor = Ref,
                    module   = Module
                }
            };
        {error, Reason} ->
            {stop, Reason}
    end.



%% ====================================================================
%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_call-3">gen_server:handle_call/3</a>
%% -spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
%% 	Result :: {reply, Reply, NewState}
%% 			| {reply, Reply, NewState, Timeout}
%% 			| {reply, Reply, NewState, hibernate}
%% 			| {noreply, NewState}
%% 			| {noreply, NewState, Timeout}
%% 			| {noreply, NewState, hibernate}
%% 			| {stop, Reason, Reply, NewState}
%% 			| {stop, Reason, NewState},
%% 	Reply :: term(),
%% 	NewState :: term(),
%% 	Timeout :: non_neg_integer() | infinity,
%% 	Reason :: term().
%% ====================================================================
handle_call(Request, _From, State) ->
    {stop, {unknown_call, Request}, State}.



%% ====================================================================
%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_cast-2">gen_server:handle_cast/2</a>
%% -spec handle_cast(Request :: term(), State :: term()) -> Result when
%% 	Result :: {noreply, NewState}
%% 			| {noreply, NewState, Timeout}
%% 			| {noreply, NewState, hibernate}
%% 			| {stop, Reason :: term(), NewState},
%% 	NewState :: term(),
%% 	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast(_Msg, State) ->
    {noreply, State}.



%% ====================================================================
%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:handle_info-2">gen_server:handle_info/2</a>
%% -spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
%% 	Result :: {noreply, NewState}
%% 			| {noreply, NewState, Timeout}
%% 			| {noreply, NewState, hibernate}
%% 			| {stop, Reason :: term(), NewState},
%% 	NewState :: term(),
%% 	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info({inet_async, ListSock, Ref, {ok, CliSocket}},
            #state
                {   listener=ListSock,
                    acceptor=Ref,
                    module=Module
                } = State) ->
    try
        case set_sockopt(ListSock, CliSocket) of
            ok              -> ok;
            {error, Reason} -> exit({set_sockopt, Reason})
        end,
        %% New client connected - spawn a new process using the
        %% simple_one_for_one supervisor.
        {ok, Pid} = poppy_sup:start_fsm(),
        gen_tcp:controlling_process(CliSocket, Pid),
        %% Instruct the new FSM that it owns the socket.
        Module:set_socket(Pid, CliSocket),
        %% Signal the network driver that we are ready to accept
        %% another connection
        case prim_inet:async_accept(ListSock, -1) of
            {ok,    NewRef} -> ok;
            {error, NewRef} -> exit
                (
                    {   async_accept,
                        inet:format_error(NewRef)
                    }
                )
        end,
        {noreply, State#state{acceptor = NewRef}}
    catch exit:Why ->
        error_logger:error_msg
            (   "Error in async accept: ~p.\n",
                [Why]
            ),
        {stop, Why, State}
    end;
handle_info({   inet_async,
                ListSock,
                Ref,
                Error
            },
            #state
                {   listener = ListSock,
                    acceptor = Ref
                } = State
           ) ->
    error_logger:error_msg
        (   "Error in socket acceptor: ~p.\n",
            [Error]
        ),
    {stop, Error, State};
handle_info(_Info, State) ->
    {noreply, State}.



%% ====================================================================
%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:terminate-2">gen_server:terminate/2</a>
%% -spec terminate(Reason, State :: term()) -> Any :: term() when
%% 	Reason :: normal
%% 			| shutdown
%% 			| {shutdown, term()}
%% 			| term().
%% ====================================================================
terminate(_Reason, State) ->
    gen_tcp:close(State#state.listener),
    ok.



%% ====================================================================
%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:code_change-3">gen_server:code_change/3</a>
%% -spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
%% 	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
%% 	OldVsn :: Vsn | {down, Vsn},
%% 	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================
set_sockopt(ListSock, CliSocket) ->
    true = inet_db:register_socket(CliSocket, inet_tcp),
    case prim_inet:getopts(ListSock,
                            [   active,
                                nodelay,
                                keepalive,
                                delay_send,
                                priority,
                                tos
                            ]
                          ) of
        {ok, Opts} ->
            case prim_inet:setopts(CliSocket, Opts) of
                ok    -> ok;
                Error -> gen_tcp:close(CliSocket), Error
            end;
        Error ->
            gen_tcp:close(CliSocket), Error
    end.

