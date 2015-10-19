%% example code
-module(db).

-export([dump/1,
         new/1,
         lookup/2,
         insert/2,
         delete/1,
         delete/2,
         update/2]).

-define(TAB(Name), list_to_atom(binary_to_list(Name))).
-type bin_string() :: binary().

-spec dump(bin_string()) -> {ok, Json::bin_string()}|{nok, Cause::bin_string()}.
dump(Name) when is_binary(Name) ->
    {ok, jiffy:encode([{[KV]}||KV<-ets:tab2list(?TAB(Name))])}.

-spec new(bin_string()) -> ok|{nok, Cause::bin_string()}.
new(Name) when is_binary(Name) ->
    Tab = ?TAB(Name),
    Tab = ets:new(Tab, [public, named_table, {heir, whereis(ecweb_sup), undefined}]),
    ok.

-spec delete(bin_string()) -> ok|{nok, Cause::bin_string()}.
delete(Name) when is_binary(Name) ->
    Tab = ?TAB(Name),
    true = ets:delete(Tab),
    ok.

-spec delete(bin_string(), bin_string()) -> ok|{nok, Cause::bin_string()}.
delete(Name, Key) when is_binary(Name),
                       is_binary(Key) ->
    true = ets:delete(?TAB(Name), Key),
    ok.

-spec lookup(bin_string(), bin_string()) -> {ok, Json::bin_string()}|{nok, Cause::bin_string()}.
lookup(Name, Key) when is_binary(Name),
                       is_binary(Key) ->
    [Ret] = ets:lookup(?TAB(Name), Key),
    {ok, jiffy:encode({[Ret]})}.

-spec insert(bin_string(), Json::bin_string()) -> ok|{nok, Cause::bin_string()}.
insert(Name, KV) when is_binary(Name),
                      is_binary(KV) ->
    {[{Key, Value}]} = jiffy:decode(KV),
    true = ets:insert(?TAB(Name), {Key, Value}),
    ok.

-spec update(bin_string(), Json::bin_string()) -> ok|{nok, Cause::bin_string()}.
update(Name, KV) when is_binary(Name),
                      is_binary(KV) ->
    insert(Name, KV).
