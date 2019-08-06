-module(cache_server).

-behavior(gen_server).

-include("cache_server.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-export([start_link/2, insert/4, lookup/2, lookup_by_date/3, clean_server/1]).
-export([init/1,  handle_cast/2, handle_call/3, stop/0, terminate/2, handle_info/2, code_change/3]).


start_link(TableName, Interval) ->
	Dropintmilisec= proplists:get_value(drop_interval, Interval)*1000,
	gen_server:start_link({local, ?MODULE}, ?MODULE, {TableName, Dropintmilisec}, []).

stop() ->
  gen_server:stop(?MODULE).


init({TableName, Dropintmilisec}) -> 
	ets:new(TableName, [set, public, named_table, {keypos, #inscache.key}]), 
	timer:apply_interval(Dropintmilisec, ?MODULE, clean_server, TableName),
{ok, {TableName, Dropintmilisec}}.


insert(TableName, Key, Value, Time) -> 
	gen_server:cast(?MODULE, {insert, TableName, Key, Value, Time}).

  handle_cast({insert, TableName, Key, Value, Time}, State) ->
	Lifetime = calendar:datetime_to_gregorian_seconds(calendar:local_time())+ Time,
	AddTime = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
	ets:insert(TableName, #inscache{key=Key, value=Value, time=Lifetime, addtime=AddTime}),
  {noreply, State}.
  
  
lookup(TableName, Key) ->
  gen_server:call(?MODULE, {lookup, TableName, Key}).

lookup_by_date(TableName, DateFrom, DateTo) -> 
	gen_server:call(?MODULE, {lookup_by_date, TableName, DateFrom, DateTo}).

  
handle_call({lookup, TableName, Key}, _From, State) ->
  Nowtime=calendar:datetime_to_gregorian_seconds(calendar:local_time()),
  List = ets:lookup(TableName, Key),
     Reply= case List of
	[{_, _, Value, Lifetime, _}] when Nowtime < Lifetime -> {ok, Value};
	[{_, _, _, Lifetime, _}] when Nowtime >= Lifetime -> undefined
     end,
  {reply, Reply, State};

 handle_call({lookup_by_date, TableName, DateFrom, DateTo}, _From, State) ->
	FromDate= calendar:datetime_to_gregorian_seconds(DateFrom),
	ToDate= calendar:datetime_to_gregorian_seconds(DateTo),
	MS= ets:fun2ms(fun(#inscache{value=Value, addtime=AddTime}) when AddTime >= FromDate andalso AddTime =< ToDate -> {ok, Value} end),
 	Reply = ets:select(TableName, MS), 
  {reply, Reply, State}.

clean_server(TableName) ->
	Nowtime=calendar:datetime_to_gregorian_seconds(calendar:local_time()),
        MS=ets:fun2ms(fun(#inscache{key =Key, value=Value, time=Lifetime}) when Nowtime >= Lifetime -> true end),
        ets:select_delete(TableName, MS).	
	

terminate(normal, _State) ->
  ok.

handle_info(_Info, State) -> {noreply, State}.

code_change(_Old, State, _Extra) -> {ok, State}.

