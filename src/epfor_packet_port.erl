-module(epfor_packet_port).

-export([init/1, terminate/2, handle_info/2, handle_call/3, handle_cast/2, code_change/3, start_link/3, send_command/2, start_pid_link/2]).
-behaviour(gen_server).

-record(state, {port, timeout=5000}).

start_pid_link(CmdSpec, OtherArgs) -> gen_server:start_link(?MODULE, {CmdSpec, OtherArgs}, []).

start_link(NameSpec, CmdSpec, OtherArgs) ->
  gen_server:start_link(NameSpec, ?MODULE, {CmdSpec, OtherArgs}, []).

send_command(NameSpec, Cmd) -> gen_server:call(convert_namespec(NameSpec), Cmd).

spawn_port(CmdSpec, OtherArgs) ->
	erlang:open_port({spawn, CmdSpec}, [{packet, 4}, nouse_stdio, exit_status] ++ OtherArgs).

init({CmdSpec, OtherArgs}) ->
	SpawnedPort = spawn_port(CmdSpec, OtherArgs),
	{ok, #state{port = SpawnedPort}}.

terminate(_Reason, State) ->
	case erlang:port_info(State#state.port) of
		undefined -> ok;
                _ -> erlang:port_close(State#state.port)
	end.

handle_info(Info, State) -> 
	case Info of
		{Port,{exit_status,EStatus}} -> {stop, {port_exit, Port, EStatus}, State};
		_ -> {stop, {message_not_understood, Info}, State}
	end.

handle_call(Request, _From, State) -> 
	case catch(call_port(State, Request)) of
		{ok, Data} -> {reply, Data, State};
		timeout -> {stop, timeout, State}
	end.

handle_cast(_, State) -> {noreply, State}.

call_port(State, Cmd) -> 
	erlang:port_command(State#state.port, Cmd),
	receive
		{_Port, {data, Data}} -> {ok, Data}
	after
		State#state.timeout -> timeout
	end.

code_change(_, _, _) -> {error, "Hot upgrade not supported"}.

%% @private
convert_namespec({local, Name}) -> Name;
convert_namespec(NS) -> NS.
