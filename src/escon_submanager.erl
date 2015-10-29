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

-module('escon_submanager').
-author('Marc A. Paradise <marc.paradise@gmail.com>').
-behaviour(gen_server).

-define(SERVER, ?MODULE).
%% API
-export([start_link/0,
         watch/2,
         unwatch/2,
         unwatch_all/1,
         what_is_watched/1,
         who_is_watching/1]).


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

% Synchronous Calls
what_is_watched(Who) ->
    gen_server:call(?SERVER, {what_is_watched, Who}).
who_is_watching(What) ->
    gen_server:call(?SERVER, {who_is_watching, What}).

% Asynchronous Calls
watch(Pid, What) ->
    gen_server:cast(?SERVER, {watch, Pid, What}).

unwatch_all(Pid) ->
    gen_server:cast(?SERVER, {unwatch_all, Pid}).
unwatch(Pid, What) ->
    gen_server:cast(?SERVER, {unwatch, Pid, What}).

%%
%% gen_server Function Definitions
%%
init(_) ->
    % Restore any monitors we're supposed to have in place.
    reconstitute_monitors(ets:tab2list(escon_subscribers)),
    {ok, []}.


handle_call({what_is_watched, Pid}, _From, State) ->
    {reply, watched_events(Pid), State};
handle_call({who_is_watching, EventName}, _From, State) ->
    {reply, set:to_list(event_watchers(EventName)), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({watch, Pid, EventName}, State) ->
    watch_event(Pid, EventName),
    {noreply, State};
handle_cast({unwatch, Pid, EventName}, State) ->
    unwatch_event(Pid, EventName),
    {noreply, State};
handle_cast({unwatch_all, Pid}, State) ->
    unwatch_all_events(Pid),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    % Clear out the watches when a child dies.
    unwatch_all_events(Pid),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Internals
%%

reconstitute_monitors([{Pid, _OldRef, Subscriptions}|Tail]) ->
    ets:insert(escon_subscribers, {Pid, erlang:monitor(process, Pid), Subscriptions}),
    reconstitute_monitors(Tail);
reconstitute_monitors([]) ->
    ok.

unwatch_all_events(WatcherPid) ->
    case ets:lookup(escon_subscribers, WatcherPid) of
        [{WatcherPid, Ref, Subscriptions}] ->
            [unwatch_event0(WatcherPid, Event) || Event <- Subscriptions],
            ets:delete(escon_subscribers, WatcherPid),
            erlang:demonitor(Ref);
        [] ->
            ok
    end.

unwatch_event(WatcherPid, Event) ->
    case ets:lookup(escon_subscribers, WatcherPid) of
        [{WatcherPid, Ref, Subscriptions}] ->
            case lists:delete(Event, Subscriptions) of
                [] ->
                   % We don't need to monitor this watcher if
                   % it is no longer subscribed to anything.
                   erlang:demonitor(Ref);
                Subscriptions1 ->
                    ets:insert(escon_subscribers, {WatcherPid, Ref, Subscriptions1})
            end;
        [] ->
            ok
    end,
    unwatch_event0(WatcherPid, Event).

unwatch_event0(WatcherPid, Event) ->
    Watchers = set:del_element(WatcherPid, event_watchers(Event)),
    ets:insert(escon_subscriptions, {WatcherPid, Watchers}).


watch_event(WatcherPid, Event) ->
    Sub0  = ets:lookup(escon_subscribers, WatcherPid),
    Sub1 = updated_subscriptions(WatcherPid, Event, Sub0),
    ets:insert(escon_subscribers, Sub1),

    Watchers = set:add_element(WatcherPid, event_watchers(Event)),
    ets:insert(escon_subscriptions, {WatcherPid, Watchers}),
    ok.


updated_subscriptions(WatcherPid, Event, []) ->
    { WatcherPid, erlang:monitor(process, WatcherPid), [Event] };
updated_subscriptions(WatcherPid, Event, [{WatcherPid, Ref, Subscriptions}]) ->
    case lists:member(Event, Subscriptions) of
         true ->
             {WatcherPid, Ref, Subscriptions};
         false ->
             {WatcherPid, Ref, [Event | Subscriptions]}
     end.

event_watchers(Event) ->
  case ets:lookup(escon_subscriptions, Event) of
    [{Event, Watchers}] ->
        Watchers;
    [] ->
        set:new()
  end.

watched_events(WatcherPid) ->
    case ets:lookup(escon_subscribers, WatcherPid) of
        [{WatcherPid, _Ref, Subscriptions}] ->
            Subscriptions;
        [] ->
            []
    end.




