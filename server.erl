%%%-------------------------------------------------------------------
%%% @author Luís Guimarães
%%% @copyright (C) 2021
%%% @doc
%%%   A server to interact with a mnesia database.
%%% @end
%%% Created : 18. Mar 2021 16:27
%%%-------------------------------------------------------------------
-module(server).
-export([start/0, loop/0, db_interaction/4]).
-import(db, [init_db/0, update/2, lookup/2]).

db_interaction(ServerPID, ClientPID, Type, Req_Info) ->
  {Req_Type, Info} = Req_Info,
  case Type of
    lookup ->
      ServerPID ! {ClientPID, db:lookup(Req_Type, Info)};
    update ->
      ServerPID ! {ClientPID, db:update(Req_Type, Info)}
  end.


loop() ->
  receive
    {ClientPID, Type, Req_Info} ->
      spawn(server, db_interaction, [self(), ClientPID, Type, Req_Info]),
      loop();
    {ClientPID, Response} ->
      ClientPID ! Response,
      loop();
    _ ->
      io:fwrite("Invalid request received!~n"),
      loop()
  end.


start() ->
  compile:file(db),
  db:init_db(),
  Pid = spawn(server, loop, []),
  register(server_pid, Pid),
  node().
