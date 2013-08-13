%% @author Richard
%% @doc @todo While poppy_ser is a generic implementation,
%% poppy_fsm is a mere stub FSM for illustrating how to write TCP 
%% servers.


-module(poppy_fsm).
-author("kuangyel2000@gmail.com").

-behaviour(gen_fsm).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0, set_socket/2]).

-export([   init/1, handle_event/3, handle_sync_event/4, handle_info/3,
            terminate/3, code_change/4
        ]).



%% ====================================================================
%% FSM States
%% ====================================================================
-export(['WAIT_FOR_SOCKET'/2, 'WAIT_FOR_DATA'/2]).



%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state,
            {   socket,    % client socket
                addr       % client address
            }
       ).

-define(TIMEOUT, 120000).


%% ====================================================================
%% API functions
%% Func: start_link/0, set_socket/2
%% @spec (Socket) -> {ok,Pid} | ignore | {error,Error}
%% @doc To be called by the supervisor in order to start the server.
%%      If init/1 fails with Reason, the function returns {error,Reason}.
%%      If init/1 returns {stop,Reason} or ignore, the process is
%%      terminated and the function returns {error,Reason} or ignore,
%%      respectively.
%% @end
%% ====================================================================
start_link() ->
    gen_fsm:start_link(?MODULE, [], []).

set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
    gen_fsm:send_event(Pid, {socket_ready, Socket}).



%% ====================================================================
%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:init-1">gen_fsm:init/1</a>
%% -spec init(Args :: term()) -> Result when
%% 	Result :: {ok, StateName, StateData}
%% 			| {ok, StateName, StateData, Timeout}
%% 			| {ok, StateName, StateData, hibernate}
%% 			| {stop, Reason}
%% 			| ignore,
%% 	StateName :: atom(),
%% 	StateData :: term(),
%% 	Timeout :: non_neg_integer() | infinity,
%% 	Reason :: term().
%% ====================================================================
init([]) ->
    process_flag(trap_exit, true),
    {ok, 'WAIT_FOR_SOCKET', #state{}}.



%% ====================================================================
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%% ====================================================================
'WAIT_FOR_SOCKET'(
                    {   socket_ready,
                        Socket
                    },
                    State
                 ) when is_port(Socket) ->
    % Now we own the socket
    inet:setopts(Socket, [{active, once}, {packet, 2}, binary]),
    {ok, {IP, _Port}} = inet:peername(Socket),
    {next_state, 'WAIT_FOR_DATA', State#state
        {   socket=Socket,
            addr=IP
        },
        ?TIMEOUT
    };
'WAIT_FOR_SOCKET'(Other, State) ->
    error_logger:error_msg
        (   "State: 'WAIT_FOR_SOCKET'. Unexpected message: ~p\n",
            [Other]
        ),
    %% Allow to receive async messages
    {next_state, 'WAIT_FOR_SOCKET', State}.

%% Notification event coming from client
'WAIT_FOR_DATA'({data, Data}, #state{socket = S} = State) ->
    ok = gen_tcp:send(S, Data),
    {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT};
'WAIT_FOR_DATA'(timeout, State) ->
    error_logger:error_msg
        (   "~p Client connection timeout - closing.\n",
            [self()]
        ),
    {stop, normal, State};
'WAIT_FOR_DATA'(Data, State) ->
    io:format("~p Ignoring data: ~p\n", [self(), Data]),
    {next_state, 'WAIT_FOR_DATA', State, ?TIMEOUT}.



%% ====================================================================
%% handle_event/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:handle_event-3">gen_fsm:handle_event/3</a>
%% -spec handle_event(Event :: term(), StateName :: atom(), StateData :: term()) -> Result when
%% 	Result :: {next_state, NextStateName, NewStateData}
%% 			| {next_state, NextStateName, NewStateData, Timeout}
%% 			| {next_state, NextStateName, NewStateData, hibernate}
%% 			| {stop, Reason, NewStateData},
%% 	NextStateName :: atom(),
%% 	NewStateData :: term(),
%% 	Timeout :: non_neg_integer() | infinity,
%% 	Reason :: term().
%% ====================================================================
handle_event(Event, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.



%% ====================================================================
%% handle_sync_event/4
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:handle_sync_event-4">gen_fsm:handle_sync_event/4</a>
%% -spec handle_sync_event(Event :: term(), From :: {pid(), Tag :: term()}, StateName :: atom(), StateData :: term()) -> Result when
%% 	Result :: {reply, Reply, NextStateName, NewStateData}
%% 			| {reply, Reply, NextStateName, NewStateData, Timeout}
%% 			| {reply, Reply, NextStateName, NewStateData, hibernate}
%% 			| {next_state, NextStateName, NewStateData}
%% 			| {next_state, NextStateName, NewStateData, Timeout}
%% 			| {next_state, NextStateName, NewStateData, hibernate}
%% 			| {stop, Reason, Reply, NewStateData}
%% 			| {stop, Reason, NewStateData},
%% 	Reply :: term(),
%% 	NextStateName :: atom(),
%% 	NewStateData :: term(),
%% 	Timeout :: non_neg_integer() | infinity,
%% 	Reason :: term().
%% ====================================================================
handle_sync_event(Event, _From, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.



%% ====================================================================
%% handle_info/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:handle_info-3">gen_fsm:handle_info/3</a>
%% -spec handle_info(Info :: term(), StateName :: atom(), StateData :: term()) -> Result when
%% 	Result :: {next_state, NextStateName, NewStateData}
%% 			| {next_state, NextStateName, NewStateData, Timeout}
%% 			| {next_state, NextStateName, NewStateData, hibernate}
%% 			| {stop, Reason, NewStateData},
%% 	NextStateName :: atom(),
%% 	NewStateData :: term(),
%% 	Timeout :: non_neg_integer() | infinity,
%% 	Reason :: normal | term().
%% ====================================================================
handle_info(    {tcp, Socket, Bin},
                StateName,
                #state{socket = Socket} = StateData
           ) ->
    % Flow control: enable forwarding of next TCP message
    inet:setopts(Socket, [{active, once}]),
    ?MODULE:StateName({data, Bin}, StateData);
handle_info(    {tcp_closed, Socket},
                _StateName,
                #state{socket = Socket, addr = Addr} = StateData
           ) ->
    error_logger:info_msg
        (   "~p Client ~p disconnected.\n",
            [self(), Addr]
        ),
    {stop, normal, StateData};
handle_info(_Info, StateName, StateData) ->
    {noreply, StateName, StateData}.


%% ====================================================================
%% terminate/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:terminate-3">gen_fsm:terminate/3</a>
%% -spec terminate(Reason, StateName :: atom(), StateData :: term()) -> Result :: term() when
%% 	Reason :: normal
%% 			| shutdown
%% 			| {shutdown, term()}
%% 			| term().
%% ====================================================================
terminate(_Reason, _StateName, #state{socket = Socket}) ->
    (catch gen_tcp:close(Socket)),
    ok.



%% ====================================================================
%% code_change/4
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_fsm.html#Module:code_change-4">gen_fsm:code_change/4</a>
%% -spec code_change(OldVsn, StateName :: atom(), StateData :: term(), Extra :: term()) -> {ok, NextStateName :: atom(), NewStateData :: term()} when
%% 	OldVsn :: Vsn | {down, Vsn},
%% 	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

