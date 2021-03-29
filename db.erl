%%%-------------------------------------------------------------------
%%% @author Luís Guimarães
%%% @copyright (C) 2021
%%% @doc
%%%   A db interaction module
%%% @end
%%% Created : 21. Mar 2021 19:28
%%%-------------------------------------------------------------------
-module(db).

-import(lists, [foreach/2, member/2, last/1, append/2, delete/2]).
-import(string, [tokens/2]).

-export([init_db/0, update/2, lookup/2]).

-include_lib("stdlib/include/qlc.hrl").

-record(person, {cc, name, address, phone}).
-record(book, {isbn, name, authors}).
-record(loans, {cc, books}).


% Call once to setup the database and start the server
init_db() ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  mnesia:create_table(person, [{attributes, record_info(fields, person)}]),
  mnesia:create_table(book, [{attributes, record_info(fields, book)}]),
  mnesia:create_table(loans, [{attributes, record_info(fields, loans)}]),
  mnesia:wait_for_tables([person, book, loans], 20000),
  add_default_values().


do(Q) ->
  F = fun() -> qlc:e(Q) end,
  {atomic, Val} = mnesia:transaction(F),
  Val.


update(request, {Cc, Isbn}) ->
  P = do(qlc:q([X#person.cc || X <- mnesia:table(person),
    X#person.cc =:= Cc
  ])),
  B = do(qlc:q([X || X <- mnesia:table(book),
    X#book.isbn =:= Isbn
  ])),
  if
    P =:= [] -> error;
    B =:= [] -> error;
    true -> Books = do(qlc:q([X#loans.books || X <- mnesia:table(loans),
      X#loans.cc =:= Cc
    ])),
      if
        Books =:= [] -> Row = #loans{cc = Cc, books = [Isbn]},
          F = fun() ->
            mnesia:write(Row)
              end,
          {atomic, Val} = mnesia:transaction(F),
          Val;
        true -> Member = member(Isbn, last(Books)),
          if
            Member =:= true -> error;
            true -> Row = #loans{cc = Cc, books = append(last(Books), [Isbn])},
              F = fun() ->
                mnesia:write(Row)
                  end,
              {atomic, Val} = mnesia:transaction(F),
              Val
          end
      end
  end;

update(return, {Cc, Isbn}) ->
  P = do(qlc:q([X#person.cc || X <- mnesia:table(person),
    X#person.cc =:= Cc
  ])),
  L = do(qlc:q([X || X <- mnesia:table(loans),
    X#loans.cc =:= Cc
  ])),
  B = do(qlc:q([X || X <- mnesia:table(book),
    X#book.isbn =:= Isbn
  ])),
  if
    P =:= [] -> error;
    L =:= [] -> error;
    B =:= [] -> error;
    true -> Row = #loans{cc = Cc, books = delete(Isbn, element(3, last(L)))},
      F = fun() ->
        mnesia:write(Row)
          end,
      {atomic, Val} = mnesia:transaction(F),
      Val
  end;

update(_, _) ->
  bad_request.


lookup(books, Cc) ->
  do(qlc:q([X#book.name || X <- mnesia:table(book),
    Y <- mnesia:table(loans),
    Y#loans.cc =:= Cc,
    member(X#book.isbn, Y#loans.books)
  ]));

lookup(loans, BookTitle) ->
  do(qlc:q([X#person.name || X <- mnesia:table(person),
    Y <- mnesia:table(loans),
    Z <- mnesia:table(book),
    Z#book.name =:= BookTitle,
    X#person.cc =:= Y#loans.cc,
    member(Z#book.isbn, Y#loans.books)
  ]));

lookup(requested, Isbn) ->
  List = do(qlc:q([X || X <- mnesia:table(loans),
    member(Isbn, X#loans.books)
  ])),
  if
    length(List) >= 1 -> true;
    true -> false
  end;

lookup(codes, BookTitle) ->
  do(qlc:q([X#book.isbn || X <- mnesia:table(book),
    X#book.name =:= BookTitle
  ]));

lookup(reqNum, Cc) ->
  do(qlc:q([length(X#loans.books) || X <- mnesia:table(loans),
    X#loans.cc =:= Cc
  ]));

lookup(_, _) ->
  bad_request.


default_tables() ->
  [%% The person table
    {person, 123456789, "David Bowie", "285 Lafayette Street, NYC, USA", 934423789},
    {person, 123456790, "Elton John", "1 Blythe Road, London W14 0HG, UK", 934423489},
    {person, 123456791, "Jean-Michel Jarre", "Avenue Jean-Baptiste Charcot 78380, Bougival, France", 933423489},
    %% The book table
    {book, 9780099458326, "Kafka on the Shore", "Murakami, H."},
    {book, 9781787300194, "Killing Commendatore", "Murakami, H."},
    {book, 9780061177590, "Women", "Bukowski, C."},
    {book, 9780060850523, "Brave New World", "Huxley, A."},
    {book, 9780141036144, "1984", "Orwell, G."},
    %% The loans table
    {loans, 123456789, [9780099458326, 9780061177590, 9780060850523, 9780141036144]},
    {loans, 123456790, [9780060850523, 9780141036144]},
    {loans, 123456791, [9780099458326, 9781787300194, 9780061177590, 9780060850523, 9780141036144]}
  ].


add_default_values() ->
  mnesia:clear_table(person),
  mnesia:clear_table(book),
  mnesia:clear_table(loans),
  F = fun() ->
    foreach(fun mnesia:write/1, default_tables())
      end,
  mnesia:transaction(F).
