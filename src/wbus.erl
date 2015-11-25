%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%%    Wbus interface
%%% @end
%%% Created : 22 Nov 2015 by Tony Rogvall <tony@rogvall.se>

-module(wbus).

-include("../include/wbus.hrl").

-compile(export_all).

-define(NUM_RETRIES, 4).

open() ->
    open("/dev/tty.usbserial-A901HOK3", 2400). %% 2400).

open(Device, Baud) ->
    uart:open(Device, [{baud, Baud},{parity,even},{stopb,1},
			    {mode,binary},{active,false}]).

init(U) ->
    uart:break(U, 50),
    timer:sleep(50).
    %% uart:flush(U)  
	    
close(U) ->
    uart:close(U).

checksum(<<C,Rest/binary>>, Chk) ->
    checksum(Rest, C bxor Chk);
checksum(<<>>, Chk) ->
    Chk.

%% Send Wbus message

send(U, Addr, Cmd, Data, Data2) ->
    Len = byte_size(Data)+byte_size(Data2)+2,
    Header = <<Addr, Len, Cmd>>,
    Chk1 = checksum(Header, 0),
    Chk2 = checksum(Data, Chk1),
    Chk3 = checksum(Data2, Chk2),
    Message = <<Header/binary, Data/binary, Data2/binary, Chk3>>,
    io:format("sending message: header=~p, data=~w, data2=~w, check=~w\n", 
	      [Header, Data, Data2, Chk3]),
    ok = uart:send(U, Message),  %% Send message
    MessageSize = byte_size(Message),
    %% Read and check echoed Message
    case recv(U, MessageSize) of
	{ok,Message} -> ok;
	{ok,BadData} -> {error, {bad_data, BadData}};
	Error -> Error
    end.

recv(U, Len) ->
    recv_(U, Len, <<>>).

recv_(_U, 0, Acc) ->
    {ok, Acc};
recv_(U, Len, Acc) ->
    case uart:recv(U, 1, 2000) of
	{ok,<<C>>} ->
	    io:format("got ~w\n", [C]),
	    recv_(U, Len-1, <<Acc/binary, C>>);
	Error ->
	    io:format("got error ~p\n", [Error]),
	    Error
    end.

recv(U, Addr, Cmd, Skip) ->
    case wait_address(U, Addr) of
	ok ->
	    case recv(U, 2) of
		{ok, <<Len,Cmd1>>} ->
		    if Cmd1 =/= (Cmd bor 16#80) ->
			    {ok,<<>>};
		       true ->
			    Chk0 = checksum(<<Addr,Len,Cmd1>>, 0),
			    {ok,Chk1,_Data0} = recv_data(U, Skip, Chk0),
			    io:format("recv_data ~w = ~p check1=~w\n", 
				      [Skip,_Data0,Chk1]),
			    Len1 = Len - 2 - Skip,
			    {ok,Chk2,Data} = recv_data(U, Len1, Chk1),
			    io:format("recv_data ~w = ~p check2=~w\n", 
				      [Len1,Data,Chk2]),
			    case recv(U, 1) of
				{ok, <<Chk2>>} ->
				    {ok,Data};
				{ok, _} ->
				    {errro, bad_checksum};
				Error ->
				    Error
			    end
		    end;
		Error -> Error
	    end;
	Error -> 
	    Error
    end.

%% wait until we see address on wbus.
wait_address(U, Addr) ->
    case recv(U, 1) of
	{ok, <<Addr>>} ->
	    ok;
	{ok, _} ->
	    wait_address(U, Addr);
	Error ->
	    Error
    end.

recv_data(_U, 0, Chk) ->
    {ok, Chk, <<>>};
recv_data(U, Len, Chk) ->			    
    case recv(U, Len) of
	{ok,Data} ->
	    Chk1 = checksum(Data, Chk),
	    {ok, Chk1, Data};
	Error ->
	    Error
    end.

%% Send a client W-Bus request and read answer from Heater.
io_request(U, Cmd, Data, Data2, Skip) ->
    io_try_request(U, Cmd, Data, Data2, Skip, 0).

io_request_(U, Cmd, Data, Data2, Skip) ->
    init(U),
    io_try_request(U, Cmd, Data, Data2, Skip, 0).

io_try_request(U, Cmd, Data, Data2, Skip, I) when I =< ?NUM_RETRIES ->
    if I > 0 -> timer:sleep(50); true -> ok end,
    case io_do_request(U, Cmd, Data, Data2, Skip) of
	{ok, Data} ->
	    timer:sleep(30),  %% add some space for next request
	    {ok,Data};
	Error ->
	    io:format("wbus: error ~p\n", [Error]),
	    io_try_request(U, Cmd, Data, Data2, Skip, I+1)
    end;
io_try_request(_U, _Cmd, _Data, _Data2, _Skip, _I) ->
    {error, too_many_retries}.

io_do_request(U, Cmd, Data, Data2, Skip) ->
    Addr = (?WBUS_CADDR bsl 4) bor ?WBUS_HADDR,
    case send(U, Addr, Cmd, Data, Data2) of
	ok ->
	    RAddr = (?WBUS_HADDR bsl 4) bor ?WBUS_CADDR,
	    case recv(U, RAddr, Cmd, Skip) of
		{ok, Data} -> {ok,Data};
		Error -> Error
	    end;
	Error -> Error
    end.

ident(U, dev_name) ->  ident(U, ?IDENT_DEV_NAME);
ident(U, dev_id) ->  ident(U, ?IDENT_DEV_ID);
ident(U, dom_cu) ->  ident(U, ?IDENT_DOM_CU);
ident(U, dom_ht) ->  ident(U, ?IDENT_DOM_HT);
ident(U, custid) ->  ident(U, ?IDENT_CUSTID);
ident(U, serial) ->  ident(U, ?IDENT_SERIAL);
ident(U, IdentCmd) when is_integer(IdentCmd) ->
    io_request(U, ?WBUS_CMD_IDENT, <<IdentCmd>>, <<>>, 1).

get_basic_info(U) ->
    {ok, DevName} = ident(U, ?IDENT_DEV_NAME),
    {ok, DevID}   = ident(U, ?IDENT_DEV_ID),
    {ok, DomCU}   = ident(U, ?IDENT_DOM_CU),
    {ok, DomHT}   = ident(U, ?IDENT_DOM_HT),
    {ok, CustID}  = ident(U, ?IDENT_CUSTID),
    {ok, Serial}  = ident(U, ?IDENT_SERIAL),
    {ok, [{device,DevName},{id,DevID},{cu,DomCU},{ht,DomHT},
	  {custid,CustID},{serial,Serial}]}.

sensor_read(U, query_state) ->
    sensor_read(U, ?QUERY_STATE);
sensor_read(U, query_sensors) ->
    sensor_read(U, ?QUERY_SENSORS);
sensor_read(U, Idx) when is_integer(Idx) ->
    case Idx of
	0 -> {ok, <<>>};
	1 -> {ok, <<>>};
	8 -> {ok, <<>>};
	9 -> {ok, <<>>};
	13 -> {ok, <<>>};
	14 -> {ok, <<>>};
	16 -> {ok, <<>>};
	_ ->
	    case io_request_(U, ?WBUS_CMD_QUERY, <<Idx>>, <<>>, 1) of
		{ok, Values} -> {ok, decode_sensors(Idx, Values)};
		Error -> Error
	    end
    end.

decode_sensors(?QUERY_STATE, Value) ->
    [{op, binary:at(Value, ?OP_STATE)},
     {n, binary:at(Value, ?OP_STATE_N)},
     {dev, binary:at(Value, ?DEV_STATE)}];
decode_sensors(?QUERY_SENSORS, Value) ->
    [{temperature, binary:at(Value, ?SEN_TEMP)},
     {voltage, binary:at(Value, ?SEN_VOLT)},
     {flame_detect, binary:at(Value, ?SEN_FD)},
     {heat_energy, binary:at(Value, ?SEN_HE)},
     {glow_resistance, binary:at(Value, ?SEN_GPR)}];
decode_sensors(_ID, Value) ->
    [{data, Value}].

check(U, Mode) ->
    io_request_(U, ?WBUS_CMD_CHK, <<Mode,0>>, <<>>, 0).

turn_on(U, Time) -> %% default?
    turn_on(U, ?WBUS_CMD_ON, Time).

turn_on(U, park_heating, Time) ->
    turn_on(U, ?WBUS_CMD_ON_PH, Time);
turn_on(U, ventilation, Time) -> 
    turn_on(U, ?WBUS_CMD_ON_VENT, Time);
turn_on(U, supplumental_heating, Time) ->
    turn_on(U, ?WBUS_CMD_ON_SH, Time);
turn_on(U, Cmd, Time) when is_integer(Cmd) ->
    io_request_(U, Cmd, <<Time>>, <<>>, 0).

turn_off(U) ->
    io_request_(U, ?WBUS_CMD_OFF, <<>>, <<>>, 0).

fuel_prime(U,Time) ->
    io_request_(U, ?WBUS_CMD_X, <<16#03,16#00,(Time bsr 1)>>, <<>>, 0).

get_fault_count(U) ->
    io_request_(U, ?WBUS_CMD_ERR, <<?ERR_LIST>>, <<>>, 1).

clear_faults(U) ->
    io_request_(U, ?WBUS_CMD_ERR, <<?ERR_DEL>>, <<>>, 0).

get_fault(U, ErrorNumber) ->
    io_request_(U, ?WBUS_CMD_ERR, <<?ERR_READ,ErrorNumber>>, <<>>,  1).
