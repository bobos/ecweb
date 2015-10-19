%% Feel free to use, reuse and abuse the code in this file.

%% @doc GET echo handler.
-module(route_handler).

-export([init/2]).

init(Req, Opts) ->
    Method = cowboy_req:method(Req),
    PathInfo = cowboy_req:path_info(Req),
    NewReq =
    case examine_path(PathInfo) of
        invalid_path ->
            handle_invalid_req(Req);
        {ResourceName, Key} ->
            handle_rest_api(Method, ResourceName, Key, Req)
    end,
    {ok, NewReq, Opts}.

handle_invalid_req(Req) ->
    cowboy_req:reply(400, [], <<"Invalid Path">>, Req).

handle_req(Fun, Req) ->
    case Fun() of
      {nok, Reason} ->
          cowboy_req:reply(400, [], Reason, Req);
      ok -> 
          cowboy_req:reply(200, [
              {<<"content-type">>, <<"text/plain; charset=utf-8">>}], 
              <<"ok">>, Req);
      {ok, Data} -> 
          cowboy_req:reply(200, [
              {<<"content-type">>, <<"application/json; charset=utf-8">>}], 
              Data, Req)
    end.

handle_rest_api(<<"GET">>, ResourceName, undefined, Req) ->
    handle_req(fun() -> db:dump(ResourceName) end, Req);
handle_rest_api(<<"GET">>, ResourceName, Key, Req) ->
    handle_req(fun() -> db:lookup(ResourceName, Key) end, Req);
handle_rest_api(<<"POST">>, ResourceName, undefined, Req) ->
    case cowboy_req:has_body(Req) of
        true ->
            {ok, Body, _} = cowboy_req:body(Req),
            handle_req(fun() -> db:insert(ResourceName, Body) end, Req);
        false ->
            handle_req(fun() -> db:new(ResourceName) end, Req)
    end;
handle_rest_api(<<"UPDATE">>, ResourceName, undefined, Req) ->
    handle_req(fun() -> 
        case cowboy_req:has_body(Req) of
            false -> 
                {nok, <<"missing http body">>};
            true -> 
                {ok, Body, _} = cowboy_req:body(Req),
                db:update(ResourceName, Body)
        end end, Req);
handle_rest_api(<<"DELETE">>, ResourceName, undefined, Req) ->
    handle_req(fun() -> db:delete(ResourceName) end, Req);
handle_rest_api(<<"DELETE">>, ResourceName, Key, Req) ->
    handle_req(fun() -> db:delete(ResourceName, Key) end, Req);
handle_rest_api(_, _, _, Req) ->
    cowboy_req:reply(405, Req).

examine_path([ResourceName]) ->
    {ResourceName, undefined};
examine_path([ResourceName, Key]) ->
    {ResourceName, Key};
examine_path(_) ->
    invalid_path.
