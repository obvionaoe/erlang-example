%%%-------------------------------------------------------------------
%%% @author Luís Guimarães
%%% @copyright (C) 2021
%%% @doc
%%%   A client that calls a mnesia database server.
%%% @end
%%% Created : 18. Mar 2021 18:33
%%%-------------------------------------------------------------------
-module(client).

-export([call/3]).


call(Node, Type, Req_Info) ->
  net_adm:ping(Node),
  {server_pid, Node} ! {self(), Type, Req_Info},
  receive
    Response ->
      Response
  end.
