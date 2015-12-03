%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2015, Tony Rogvall
%%% @doc
%%%    Wbus interface
%%% @end
%%% Created : 22 Nov 2015 by Tony Rogvall <tony@rogvall.se>

-module(wbus).

-include("../include/wbus.hrl").

-compile(export_all).

-define(dbg(F,A), ok).
%% -define(dbg(F,A), io:format((F),(A))).

-define(NUM_RETRIES, 4).


open() ->
    open("/dev/tty.usbserial-A901HOK3", 2400). %% 2400).

open(Device, Baud) ->
    uart:open(Device, [{baud, Baud},{parity,even},{stopb,1},
			    {mode,binary},{active,false}]).

init(U) ->
    ?dbg("send break\n", []),
    ok = uart:break(U, 50),
    ok = timer:sleep(50),
    ok = uart:flush(U, input).
	    
close(U) ->
    uart:close(U).

%% Send Wbus message

send_message(U, Addr, Cmd, Data, Data2) ->
    Len = byte_size(Data)+byte_size(Data2)+2,
    Header = <<Addr, Len, Cmd>>,
    Chk1 = checksum(Header, 0),
    Chk2 = checksum(Data, Chk1),
    Chk3 = checksum(Data2, Chk2),
    Message = <<Header/binary, Data/binary, Data2/binary, Chk3>>,
    ?dbg("sending message: header=~p, data=~w, data2=~w, check=~w\n", 
	 [Header, Data, Data2, Chk3]),
    ok = uart:send(U, Message),  %% Send message
    MessageSize = byte_size(Message),
    %% Read and check echoed Message
    case recv(U, MessageSize) of
	{ok,Message} -> ok;
	{ok,BadData} -> {error, {bad_data, BadData}};
	Error -> Error
    end.

recv_message(U, Addr, Cmd, Skip) ->
    case wait_address(U, Addr) of
	ok ->
	    case recv(U, 2) of
		{ok, <<Len,Cmd1>>} ->
		    if Cmd1 =/= (Cmd bor 16#80) ->
			    {ok,<<>>};
		       true ->
			    Chk0 = checksum(<<Addr,Len,Cmd1>>, 0),
			    {ok,Chk1,_Data0} = recv(U, Skip, Chk0),
			    ?dbg("recv_data ~w = ~p check1=~w\n", 
				 [Skip,_Data0,Chk1]),
			    Len1 = Len - 2 - Skip,
			    {ok,Chk2,Data} = recv(U, Len1, Chk1),
			    ?dbg("recv_data ~w = ~p check2=~w\n", 
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

%% recv and checksum
recv(_U, 0, Chk) ->
    {ok, Chk, <<>>};
recv(U, Len, Chk) ->			    
    case recv(U, Len) of
	{ok,Data} ->
	    Chk1 = checksum(Data, Chk),
	    {ok, Chk1, Data};
	Error ->
	    Error
    end.

%% recv
recv(U, Len) ->
    uart:recv(U, Len, 2000).
%%    recv_(U, Len, <<>>).

-ifdef(hard_debug).
%% recv loop ( temporary loop for debugging )
recv_(_U, 0, Acc) ->
    {ok, Acc};
recv_(U, Len, Acc) ->
    case uart:recv(U, 1, 2000) of
	{ok,<<C>>} ->
	    ?dbg("recv_data got ~w\n", [C]),
	    recv_(U, Len-1, <<Acc/binary, C>>);
	Error ->
	    io:format("got error ~p\n", [Error]),
	    Error
    end.
-endif.

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



%% Send a client W-Bus request and read answer from Heater.
io_request(U, Cmd, Data, Data2, Skip) ->
    io_try_request(U, Cmd, Data, Data2, Skip, 0).

io_request_(U, Cmd, Data, Data2, Skip) ->
    init(U),
    io_try_request(U, Cmd, Data, Data2, Skip, 0).

io_try_request(U, Cmd, Data, Data2, Skip, I) when I =< ?NUM_RETRIES ->
    if I > 0 -> timer:sleep(50); true -> ok end,
    case io_do_request(U, Cmd, Data, Data2, Skip) of
	{ok, DataOut} ->
	    timer:sleep(30),  %% add some space for next request
	    {ok,DataOut};
	Error ->
	    io:format("wbus: error ~p\n", [Error]),
	    io_try_request(U, Cmd, Data, Data2, Skip, I+1)
    end;
io_try_request(_U, _Cmd, _Data, _Data2, _Skip, _I) ->
    {error, too_many_retries}.

io_do_request(U, Cmd, Data, Data2, Skip) ->
    Addr = (?WBUS_CADDR bsl 4) bor ?WBUS_HADDR,
    case send_message(U, Addr, Cmd, Data, Data2) of
	ok ->
	    RAddr = (?WBUS_HADDR bsl 4) bor ?WBUS_CADDR,
	    case recv_message(U, RAddr, Cmd, Skip) of
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
    io_request_(U, ?WBUS_CMD_IDENT, <<IdentCmd>>, <<>>, 1).

get_basic_info(U) ->
    {ok, DevName} = ident(U, ?IDENT_DEV_NAME),
    {ok, DevID}   = ident(U, ?IDENT_DEV_ID),
    {ok, DomCU}   = ident(U, ?IDENT_DOM_CU),
    {ok, DomHT}   = ident(U, ?IDENT_DOM_HT),
%%  {ok, CustID}  = ident(U, ?IDENT_CUSTID),
    {ok, Serial}  = ident(U, ?IDENT_SERIAL),
    %% {custid,CustID}
    {ok, [{device,DevName},{id,DevID},{cu,DomCU},{ht,DomHT},
	  {serial,Serial}]}.

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
    [{op, get_byte(?OP_STATE, Value)},
     {n, get_byte(?OP_STATE_N, Value)},
     {dev, get_byte( ?DEV_STATE, Value)}];
decode_sensors(?QUERY_SENSORS, Value) ->
    [{temperature, get_byte(?SEN_TEMP,Value)-50},
     {voltage, get_short(?SEN_VOLT, Value)/1000},
     {flame_detect, get_byte(?SEN_FD,Value)},
     {heat_energy, get_short(?SEN_HE, Value)},
     {glow_resistance, get_short(?SEN_GPR,Value)/1000}];
decode_sensors(_ID, Value) ->
    [{data, Value}].


get_byte(Offset, Data) ->
    case Data of
	<<_:Offset/binary, Value, _/binary>> ->
	    Value
    end.

get_short(Offset, Data) ->
    case Data of
	<<_:Offset/binary, Value:16, _/binary>> ->
	    Value
    end.

check(U, Mode) ->
    io_request_(U, ?WBUS_CMD_CHK, <<Mode,0>>, <<>>, 0).

turn_on(U, Time) -> %% default?
    turn_on(U, ?WBUS_CMD_ON, Time).

turn_on(U, park_heating, Time) ->
    turn_on(U, ?WBUS_CMD_ON_PH, Time);
turn_on(U, ventilation, Time) -> 
    turn_on(U, ?WBUS_CMD_ON_VENT, Time);
turn_on(U, supplemental_heating, Time) ->
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

checksum(<<C,Rest/binary>>, Chk) ->
    checksum(Rest, C bxor Chk);
checksum(<<>>, Chk) ->
    Chk.

%% Get error string from error code
decode_error(Err) ->
    case Err of
	?ERR_DEVICE ->
	    {error, "Defective control unit"};
	?ERR_NOSTART -> 
	    {error, "No start"};
	?ERR_FLAME -> 
	    {error, "Flame failure"};
	?ERR_VCCHIGH -> 
	    {error, "Supply voltage too high"};
	?ERR_FLAME2 -> 
	    {error, "Flame was detected prior to combustion"};
	?ERR_OH ->
	    {error, "Heating unit overheated"};
	?ERR_IL ->
	    {error, "Heating unit interlocked"};
	?ERR_SCDP ->
	    {error, "Metering pump short circuit"};
	?ERR_SCCAF ->
	    {error, "Combustion air fan short circuit"};
	?ERR_SCGP ->
	    {error, "Glow plug/flame monitor short circuit"};
	?ERR_SCCP ->
	    {error, "Circulation pump short circuit"};
	?ERR_COMAC ->
	    {error, "No comunication to air condition"};
	?ERR_SCLEDG ->
	    {error, "Green LED short circuit"};
	?ERR_SCLEDY ->
	    {error, "Yellow LED short circuit"};
	?ERR_CFG ->
	    {error, "No configuraton signal"};
	?ERR_SCSV ->
	    {error, "Solenoid valve short circuit"};
	?ERR_ECU ->
	    {error, "ECU wrong coded"};
	?ERR_WBUS ->
	    {error, "W-Bus comunication failure"};
	?ERR_SCVF ->
	    {error, "Vehicle fan relay short circuit"};
	?ERR_SCT ->
	    {error, "Temperature sensor short circuit"};
	?ERR_BLKCAF ->
	    {error, "Combustion air fan blocked"};
	?ERR_SCBAT ->
	    {error, "Battery main switch short circuit"};
	?ERR_IAFR ->
	    {error, "Invalid air flow reduction"};
	?ERR_COMCS ->
	    {error, "Comunication failure on customer specific bus"};
	?ERR_SCGP2 ->
	    {error, "Glow plug/electronic ignition short circuit"};
	?ERR_SCFD ->
	    {error, "Flame sensor short circuit"};
	?ERR_SCOH ->
	    {error, "Overheat short circuit"};
	?ERR_SCSV2 ->
	    {error, "Solenoid valve shed test short circuit"};
	?ERR_SCFS ->
	    {error, "Fuel sensor short circuit"};
	?ERR_SCNSH ->
	    {error, "Nozzle stock heating short circuit"};
	?ERR_SCOI ->
	    {error, "Operation indicator short circuit"};
	?ERR_SCFD2 ->
	    {error, "Flame indicator short circuit"};
	?ERR_RREF ->
	    {error, "Reference resistance wrong"};
	?ERR_CRASH ->
	    {error, "Crash interlock activated"};
	?ERR_NOFUEL ->
	    {error, "Car is almost out of fuel"};
	?ERR_SCFPW ->
	    {error, "Fuel pre heating short circuit"};
	?ERR_SCTPCB ->
	    {error, "PCB temperatur sensor short circuit"};
	?ERR_ECUGND ->
	    {error, "Ground contact to the ECU broken"};
	?ERR_LPV ->
	    {error, "Board net energy manager low power voltage"};
	?ERR_FPSND ->
	    {error, "Fuel priming still not done"};
	?ERR_TSDATA ->
	    {error, "Error in the radio telegram"};
	?ERR_TSSNP ->
	    {error, "Telestart still not programmed"};
	?ERR_SCP ->
	    {error, "The pressure sensor has short circuit"};
	?ERR_CIDDLE ->
	    {error, "No start from control idle period"};
	?ERR_FDSIG ->
	    {error, "Flame monitor signal invalid"};
	?ERR_DEFAULT ->
	    {error, "Default values entered"};
	?ERR_EOLSNP ->
	    {error, "EOL programming has not been carried out"};
	?ERR_SCFUSE ->
	    {error, "Thermal fuse short circuit"};
	?ERR_RELAY ->
	    {error, "Error relay box (short circuit/open circuit of heating relay)"};
	?ERR_IDDLEUI ->
	    {error, "User interface idle-Mode (no-communication)"};
	?ERR_COMUI ->
	    {error, "User interface has communication fault"};
	?ERR_COMUI2 ->
	    {error, "User interface send no defined operating mode"};
	?ERR_FAN ->
	    {error, "Heater fan status message negative"};
	?ERR_SCFAN ->
	    {error, "Heater fan status bus has short circuit to UB"};
	?ERR_TW ->
	    {error, "Temperature water sensor failure"};
	?ERR_SCTW ->
	    {error, "Temperature water sensor short circuit to UB"};
	?ERR_OHTW ->
	    {error, "Overheating water temperature sensor"};
	?ERR_OSHT ->
	    {error, "Overstepping water temperature sensor gradient"};
	?ERR_TBLOW ->
	    {error, "Overheating blow temperature sensor"};
	?ERR_OSLT ->
	    {error, "Overstepping low temperature sensor gradient"};
	?ERR_OHPCBT ->
	    {error, "Overheating printed circuit board temperature sensor"};
	?ERR_OSPCBT ->
	    {error, "Overstepping printed circuit board temp sensor gradient"};
	?ERR_TC ->
	    {error, "Cabin temperature sensor failure"};
	?ERR_OSFD ->
	    {error, "Flame detector gradient failure"};
	?ERR_ECOOL ->
	    {error, "Emergency cooling"};
	?ERR_CS1 ->
	    {error, "Customer specific fault 1"};
	?ERR_CS32 ->
	    {error, "Customer specific fault 32"};
	?ERR_EOLCHK ->
	    {error, "EOL checksum error"};
	?ERR_TEST ->
	    {error, "No start during test-run"};
	?ERR_FLAME3 ->
	    {error, "Flame failure"};
	?ERR_VCCLOW ->
	    {error, "Operating voltage too low"};
	?ERR_FDAFTER ->
	    {error, "Flame was detected after combustion"};
	?ERR_PLOCK ->
	    {error, "Heater lock-out permanent"};
	?ERR_DP ->
	    {error, "Fuel pump failure"};
	?ERR_INTCAF ->
	    {error, "Combustion air fan interruption"};
	?ERR_INTGP ->
	    {error, "Glow plug / flame monitor interruption"};
	?ERR_INTCP ->
	    {error, "Circulation pump interruption"};
	?ERR_INTLEDG ->
	    {error, "Green LED interruption"};
	?ERR_INTLEDY ->
	    {error, "Yellow LED interruption"};
	?ERR_INTSV ->
	    {error, "Solenoid valve interruption"};
	?ERR_NEUTRAL ->
	    {error, "Control unit locked or coded as neutral"};
	?ERR_REFRESH ->
	    {error, "Command refresh failure"};
	?ERR_INTT ->
	    {error, "Temperature sensor interruption"};
	?ERR_TCAF ->
	    {error, "Combustion air fan tight"};
	?ERR_OHPOS ->
	    {error, "Overheat sensor position wrong"};
	?ERR_INTGP2 ->
	    {error, "Glow plug / electronic ignition unit interruption"};
	?ERR_INTFD ->
	    {error, "Flame sensor interruption"};
	?ERR_TXSP ->
	    {error, "Setpoint transmitter invalid"};
	?ERR_IVLOW ->
	    {error, "Intelligent undervoltage detection"};
	?ERR_INTSV2 ->
	    {error, "Solenoid valve shed test interruption"};
	?ERR_INTFS ->
	    {error, "Fuel sensor interruption"};
	?ERR_INTNSH ->
	    {error, "Nozzle stock heating interruption"};
	?ERR_INTOI ->
	    {error, "Operating indicator interruption"};
	?ERR_INTFD2 ->
	    {error, "Flame indicator interruption"};
	?ERR_INTFPW ->
	    {error, "Fuel pre heating interruption"};
	?ERR_INTPCBT ->
	    {error, "PCB temperature sensor interruption"};
	?ERR_EMGR ->
	    {error, "Communication board net energy manager error"};
	?ERR_WBUSTX ->
	    {error, "Send on W-Bus not succeed"};
	?ERR_INTOHS ->
	    {error, "Overheat sensor interruption"};
	?ERR_P ->
	    {error, "The pressure sensor failure"};
	?ERR_INTFUSE ->
	    {error, "Thermal fuse interrupted"};
	?ERR_CS33 ->
	    {error, "Customer specific fault 33"};
	?ERR_CS63 ->
	    {error, "Customer specific fault 63"};
	?ERR_UNKNOWN ->
	    {error, "Unknown error code"};
	_ ->
	    {error, "Fault "++integer_to_list(Err)}
    end.
