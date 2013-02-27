
%%MK:
%%
%% This module was automatically generated by running
%% ejabberd_loglevel:ejabberd_logger_src({4,[]}).
%%
-module(ejabberd_logger).
-author('mickael.remond@process-one.net').

-export([debug_msg/4,
		 info_msg/4,
		 warning_msg/4,
		 error_msg/4,
		 critical_msg/4,
		 get/0]).

get() -> {5,[]}.

critical_msg(Module, Line, Format, Args) -> notify(error,
	"C(~p:~p:~p) : "++Format++"~n",
	[self(), Module, Line | Args]).

error_msg(Module, Line, Format, Args) -> notify(error,
	"E(~p:~p:~p) : "++Format++"~n",
	[self(), Module, Line | Args]).

warning_msg(Module, Line, Format, Args) -> notify(warning_msg,
	"W(~p:~p:~p) : "++Format++"~n",
	[self(), Module, Line | Args]).

info_msg(Module, Line, Format, Args) -> notify(info_msg,
	"I(~p:~p:~p) : "++Format++"~n",
	[self(), Module, Line | Args]).

debug_msg(Module, Line, Format, Args) -> notify(info_msg,
	"D(~p:~p:~p) : "++Format++"~n",
	[self(), Module, Line | Args]).

notify(Type, Format, Args) ->
		LoggerMsg = {Type, group_leader(), {self(), Format, Args}},
		gen_event:notify(error_logger, LoggerMsg).

%%EOF