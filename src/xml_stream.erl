-module(xml_stream).

-include_lib("xmerl/include/xmerl.hrl").

-export([new/1,new/2,parse/2,close/1,parse_element/1]).

new(SinkPid) ->
	new(SinkPid, infinity).

new(SinkPid, MaxSize) ->
	spawn(fun() ->
		start_parser(SinkPid, MaxSize)
	end).

close(StreamPid) ->
	exit(StreamPid, normal).

parse(StreamPid, Str) ->
	StreamPid ! {parse,self(),to_list(Str)},
	receive
	{StreamPid,Result} ->
		Result
	end.

parse_element(S) ->
	{Elem,[]} = xmerl_scan:string(S, []),
	easy_element(Elem).

%%------------------------------------------------------------------------------

%%TODO: MaxSize ignored
%%TODO: errors are not sent to SinkPid

start_parser(SinkPid, _MaxSize) ->

	ContFun =
		fun(Continue, _Exception, St) ->
			From = xmerl_scan:cont_state(St),
			From ! {self(),self()},
			receive
			{parse,From1,Str} ->
				St1 = xmerl_scan:cont_state(From1, St),
				Continue(Str, St1)
			end
		end,

	EventFun =
		fun(#xmerl_event{event =started,
						 data =#xmlElement{name =Name,
										   attributes =Attrs}}, St) ->
			case xmerl_scan:event_state(St) of
			not_started ->
				SinkPid = xmerl_scan:user_state(St),
				gen_fsm:send_event(SinkPid, {xmlstreamstart,easy_name(Name),
															easy_attrs(Attrs)}),
				xmerl_scan:event_state(started, St);
			_ ->
				St
			end;
		(_, St) ->
			St
		end,

	HookFun =
		fun(#xmlElement{parents =[_]} =Elem, St) ->
			SinkPid = xmerl_scan:user_state(St),
			gen_fsm:send_event(SinkPid, {xmlstreamelement,easy_element(Elem)}),
			{Elem,St};

		(#xmlElement{name =Name,parents =[]} =Elem, St) ->
			SinkPid = xmerl_scan:user_state(St),
			gen_fsm:send_event(SinkPid, {xmlstreamend,easy_name(Name)}),
			{Elem,St};

		(Entity, St) ->
			{Entity,St}
		end,

	AccFun =
		fun(_ParsedEntity, Acc, St) ->
			{Acc,St}
		end,

	receive
	{parse,From,Str} ->
		xmerl_scan:string(Str, [{user_state,SinkPid},
								{continuation_fun,ContFun,From},
								{event_fun,EventFun,not_started},
						  		{hook_fun,HookFun},
								{acc_fun,AccFun}]),
		self()
	end.

%%------------------------------------------------------------------------------

to_list(B) when is_binary(B) ->
	binary_to_list(B);
to_list(L) ->
	L.

easy_name(Name) when is_atom(Name) ->
	atom_to_list(Name);
easy_name(Name) when is_binary(Name) ->
	Name.

easy_value(Value) when is_list(Value); is_binary(Value) ->
	value;
easy_value(Value) when is_atom(Value) ->
	atom_to_list(Value);
easy_value(Value) when is_integer(Value) ->
	integer_to_list(Value).

easy_attrs(Attrs) when is_list(Attrs) ->
	[{easy_name(Name),easy_value(Value)}
			|| #xmlAttribute{name =Name,value =Value} <- Attrs].

easy_element(#xmlText{value =Val}) ->
	{xmlcdata,Val};
easy_element(#xmlElement{name =Name,attributes =Attrs,content =Elems}) ->
	{xmlelement,easy_name(Name),
				easy_attrs(Attrs),
				[easy_element(El) || El <- Elems]}.

%%EOF
