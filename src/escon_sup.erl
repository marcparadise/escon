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

-module('escon_sup').
-author('Marc A. Paradise <marc.paradise@gmail.com>').
-behaviour(supervisor).

-define(SERVER, ?MODULE).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).



start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    % We're using an ets table to track who is monitoring what.  This will allow
    % us to persist this information even in the event of a 'escon_submanager' crash.
    % Note that only escon_submanager is accessing these tables, and
    % only sequentially.
    ets:new(escon_subscribers, [set, named_table, {read_concurrency, false}, {write_concurrency, false}, public]),
    ets:new(escon_subscriptions,  [set, named_table, {read_concurrency, false}, {write_concurrency, false}, public]),

    %SupFlags = #{strategy => one_for_one,
                 %intensity => 1,
                 %period => 5},
    %ChildSpecs = [#{id => escon_notifier,
                    %start => {escon_notifier
    {ok, { {one_for_all, 0, 1}, []} }.

