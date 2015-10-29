%% -*- mode: erlang -*-
%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 ft=erlang et
%%
%% Copyright 2015 Marc A. Paradise
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%

-module(escon_notifier).
-author('Marc A. Paradise <marc.paradise@gmail.com>').

-behaviour(gen_server).


-define(SERVER, ?MODULE).
%%
%% API
%%
-export([start_link/0, notify/2]).

%%
%% gen_server
%%
-export([init/1,
         handle_call/3,
         handle_cast/2,
         terminate/2,
         handle_info/2,
         code_change/3]).

%% API
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

notify(What, Message) ->
    gen_server:cast({notify, What, Message}).

%%
%% gen_server Function Definitions
%%
init(Args) ->
    {ok, Args}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({notify, What, Message}, State) ->
    FinalMessage = { What, Message },
    Who = escon_submanager:who_is_watching(What),
    [ Proc ! FinalMessage || Proc <- Who ],
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
