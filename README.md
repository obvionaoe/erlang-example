# Erlang Client-Server

## Usage

### Server

To run the server:

First create a named node:
```bash
erl -sname server
```

Then in the Eshell run:

```erlang
c(server).

server:start().
```

**Note**: the db.erl file does not need to be compiled since the server:start function calls compile:file(db).

The printed atom is the **Node** that should be used in the client.

### Client

To run the client:

#### Same Node

Compile and run the client:

```erlang
c(client).

client:call(Node, Type, {Req_Type, Info}).
```

#### Different Node

First create a named node:

```bash
erl -sname client
```

Then in the Eshell run:

```erlang
c(client).

client:call(Node, Type, {Req_Type, Info}).
```

**Note**: for client:call usage examples see report.pdf