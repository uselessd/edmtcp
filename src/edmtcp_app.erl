%%%-------------------------------------------------------------------
%% @doc edmtcp public API
%% @end
%%%-------------------------------------------------------------------

-module('edmtcp_app').

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    'edmtcp_sup':start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================