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

-module(escon).
-author('Marc A. Paradise <marc.paradise@gmail.com>').
-behaviour(gen_server).

-define(SERVER, ?MODULE).

%% API
-export([start_link/0,
         get/1,
         get_with_watch/1,
         watch/1,
         unwatch/1]).


%% gen_server
-export([init/1,
         handle_call/3,
         handle_cast/2,
         terminate/2,
         handle_info/2,
         code_change/3]).


%% API
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

% Get the requested value without registering for notifications
get(What) ->
    gen_server:call(?SERVER, {get, What, no_notify}).

% Get the requested value and be notified of changes to it.
get_with_watch(What) ->
    gen_server:call(?SERVER, {get, What, notify}).

watch(What) ->
    gen_server:cast(?SERVER, {watch, self(), What}).

unwatch(What) ->
    gen_server:cast(?SERVER, {unwatch, self(), What}).

%% gen_server
init(_) ->
  {ok, []}.

handle_call({get, What, Monitor}, From, State) ->
    {reply, get_value(What, Monitor, From), State};
handle_call({watch, What}, From, State) ->
    start_watching(From, What),
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({unwatch, Pid, all}, State) ->
    escon_submanager:unwatch_all(Pid),
    {noreply, State};
handle_cast({unwatch, Pid, What}, State) ->
    escon_submanager:unwatch(Pid, What),
    {noreply, State};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

get_value(What, no_notify, _Pid) ->
    escon_store:get(What);
get_value(What, notify, Pid) ->
    start_watching(Pid, What),
    escon_store:get(What).



start_watching(Pid, What) ->
    escon_submanager:watch(Pid, What).

