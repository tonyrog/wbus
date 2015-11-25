%% W-Bus constant definitions
%%
%% Author: Manuel Jander
%% mjander@users.sourceforge.net
%%
%% Ported by me.

-ifndef(__WBUS_HRL__).
-define(__WBUS_HRL__, true).

%% W-Bus addresses
%% 0xf : thermo Test Software
%% 0x4 : heating device
%% 0x3 : 1533 Timer
%% 0x2 : Telestart
%%

%% address as client
-define(WBUS_CADDR, 16#f).
%% address as host
-define(WBUS_HADDR, 16#4).

%% W-Bus command refresh period in mili seconds 
-define(CMD_REFRESH_PERIOD, 20000).

%% W-Bus commands 
-define(WBUS_CMD_OFF,    16#10).   %% no data 

-define(WBUS_CMD_ON,     16#20). %% For all CMD_ON_x : 1 byte = time in minutes 
-define(WBUS_CMD_ON_PH,  16#21). %% Parking Heating 
-define(WBUS_CMD_ON_VENT,16#22). %% Ventilation 
-define(WBUS_CMD_ON_SH,  16#23). %% Supplemental Heating 
-define(WBUS_CMD_CP,     16#24). %% Circulation Pump external control 
-define(WBUS_CMD_BOOST,  16#25). %% Boost mode 

-define(WBUS_CMD_SL_RD,  16#32). %% Telestart system level read.
-define(WBUS_CMD_X_RD,   16#32). %% read something. 2 bytes 16#03 16#07, returns 16#0A 
-define(WBUS_CMD_SL_WR,  16#33). %% Telestart system level write
-define(WBUS_CMD_U1,     16#38). %% sent no bytes. answer 0B 00 00 00 00 03 BF 

%% Dataset commands 
-define(WBUS_CMD_DS_START, 16#47). %% 1 byte = 1 start bloc mode. Issued before WBUS_CMD_BAUD and CMD_DS_LSB 
-define(WBUS_CMD_DS_R,     16#03). %% 1 byte: address LSB. return 1 byte read data. 
-define(WBUS_CMD_DS_W,     16#04). %% 2 byte: address LSB and data for writing. 
-define(WBUS_CMD_DS_MSB,   16#05). %% 1 byte: address MSB. No returned data. For either read or write. 
-define(WBUS_CMD_DS_STOP,  16#09). %% 0 byte: stop block mode. 
-define(WBUS_CMD_BAUD,     16#34). %% Set block mode baudrate. 2 bytes. 
%% 16#80 16#00 38400 
%% 16#40 16#00 19200
%% 16#20 16#00 9600 
%% After issuing WBUS_CMD_DS_START and WBUS_CMD_BAUD, 
%% the host sents 16#03 <addr> <checksum>
%% and gets as response 16#03 <addr> <data> <checksum> 

%% discovered by pczepek, thank a lot ! eeprom[8] = int system_level 
-define(WBUS_TS_ERD1,     16#32). %% 2 bytes: 16#00 addr 
-define(WBUS_TS_EWR1,     16#33). %% 3 bytes: 16#00 addr data 
-define(WBUS_TS_ERD,      16#35). %% READ_EEPROM [address]. 

%% Response length 2 byte. Reads two bytes from
%% eeprom memory, from address given as parameter.
%% The eeprom is accessible from address 16#00 to 16#1f.
%% In eeprom dump I found customer ID Number,
%% registered remote fob codes, telestart system level etc. 

-define(WBUS_TS_EWR,      16#36). %% WRITE_EEPROM [address],[byte1],[byte2]
%% Write two bytes to eeprom. Address, byte1, byte2
%% as parameter. 

-define(WBUS_TS_REGR, 16#40). %% discovered by pczepek, Response length 0 byte.
%% Parameter 0-16#0f. After issuing command,
%% press remote fob OFF button to register.
%% You can register max 3 remote fob. 

-define(WBUS_CMD_X, 16#42).  %% Several commands 
-define(CMD_X_VCAL, 1).      %% Calibrate voltage. 3 bytes. 1 byte 0, 
                             %% 2 bytes voltage in mV, big endian.
-define(CMD_X_FCAL, 2).      %% Flame detector calibration. 
                             %% 3 bytes. 1 byte 0, 2 bytes mOhm big endian. 
-define(CMD_X_FP, 3).        %% Fuel prime. data, 16#03 <2 bytes time 
                             %% in seconds /2>
-define(WBUS_CMD_CHK, 16#44). %% Check current command (16#20,16#21,16#22 or 16#23) 
-define(WBUS_CMD_TEST, 16#45). %% <1 byte sub system id> <1 byte time in seconds> <2 bytes value> 
-define(WBUS_CMD_QUERY, 16#50). %% Read operational information registers 
-define(WBUS_CMD_IDENT, 16#51). %% Read device identification information registers 
-define(WBUS_CMD_OPINFO, 16#53). %% 1 byte, operational info index. 
-define(WBUS_CMD_ERR, 16#56). %% Error related commands 
-define(WBUS_CMD_CO2CAL, 16#57). %% CO2 calibration 

-define(WBUS_CMD_DATASET, 16#58). %% (Not Webasto) data set related commands 

%% 16#50 Command parameters 
%% Status flags. Bitmasks below.
%% STAxy_desc means status "x", byte offset "y", flag called 2desc" 
-define(QUERY_STATUS0, 16#02).
-define(STA00_SHR,     16#10). %%!< Supplemental heater request
-define(STA00_MS,      16#01). %%!< Main, switch
-define(STA01_S,       16#01). %%!< Summer, season 
-define(STA02_D,       16#10). %%!< Generator signal D+ 
-define(STA03_BOOST,   16#10). %%!< boost mode -
-define(STA03_AD,      16#01). %%!< auxiliary drive 
-define(STA04_T15,     16#01). %%!< ignition (terminal 15) 

-define(QUERY_STATUS1, 16#03).
-define(STA10_CF,      16#01). %%!< Combustion Fan 
-define(STA10_GP,      16#02). %%!< Glühkerze
-define(STA10_FP,      16#04). %%!< Fuel Pump
-define(STA10_CP,      16#08). %%!< Circulation Pump 
-define(STA10_VF,      16#10). %%!< Vehicle Fan Relay 
-define(STA10_NSH,     16#20). %%!< Nozzle stock heating
-define(STA10_FI,      16#40). %%!< Flame indicator

-define(QUERY_OPINFO0, 16#04). %% Fuel type, max heat time and factor for shortening ventilation time (but details are unclear) 
-define(OP0_FUEL, 0). %%!<, 16#0b).: gasoline, 16#0d: diesel, 0:neutral 
-define(OP0_TIME, 1). %%!< max heating time / 10 in minutes 
-define(OP0_FACT, 2). %%!< ventilation shortening factor (?) 

-define(QUERY_SENSORS, 16#05). %%!< Assorted sensors. 8 bytes. Byte offsets below. 
-define(SEN_TEMP, 0). %%!< Temperature with 50C offset (20C is represented by 70) 
-define(SEN_VOLT, 1). %%!< 2 bytes Spannung in mili Volt, big endian 
-define(SEN_FD,   3). %%!< 1 byte Flame detector flag 
-define(SEN_HE,   4). %%!< 2 byte, heating power, in percent or watts 
                      %% (semantic seems wrong, heating energy?) 
-define(SEN_GPR, 6).  %%!< 2 byte, glow plug resistance in mili Ohm. 

-define(QUERY_COUNTERS1, 16#06).
-define(WORK_HOURS, 0).   %%!< Working hours
-define(WORK_MIN,   2).   %%!< Working minutes
-define(OP_HOURS,   3).   %%!< Operating hours
-define(OP_MIN,     5).   %%!< Operating minute
-define(CNT_START, 6).    %%!< Start counter

-define(QUERY_STATE, 16#07).
-define(OP_STATE,   0).
-define(OP_STATE_N, 1).
-define(DEV_STATE,  2).

%% 3 more unknown bytes 
-define(WB_STATE_BO,     16#00). %% Burn out 
-define(WB_STATE_DEACT1, 16#01). %% Deactivation 
-define(WB_STATE_BOADR,  16#02). %% Burn out ADR (has something to due with hazardous substances transportation) 
-define(WB_STATE_BORAMP, 16#03). %% Burn out Ramp 
-define(WB_STATE_OFF,    16#04). %% Off state 
-define(WB_STATE_CPL,    16#05). %% Combustion process part load 
-define(WB_STATE_CFL,    16#06). %% Combustion process full load 
-define(WB_STATE_FS,     16#07). %% Fuel supply 
-define(WB_STATE_CAFS,   16#08). %% Combustion air fan start 
-define(WB_STATE_FSI,    16#09). %% Fuel supply interruption 
-define(WB_STATE_DIAG,   16#0a). %% Diagnostic state 
-define(WB_STATE_FPI,    16#0b). %% Fuel pump interruption 
-define(WB_STATE_EMF,    16#0c). %% EMF measurement 
-define(WB_STATE_DEB,    16#0d). %% Debounce 
-define(WB_STATE_DEACTE, 16#0e). %% Deactivation 
-define(WB_STATE_FDI,    16#0f). %% Flame detector interrogation 
-define(WB_STATE_FDC,    16#10). %% Flame detector cooling 
-define(WB_STATE_FDM,    16#11). %% Flame detector measuring phase 
-define(WB_STATE_FDMZ,   16#12). %% Flame detector measuring phase ZUE 
-define(WB_STATE_FAN,    16#13). %% Fan start up 
-define(WB_STATE_GPRAMP, 16#14). %% Glow plug ramp 
-define(WB_STATE_LOCK,   16#15). %% Heater interlock 
-define(WB_STATE_INIT,   16#16). %% Initialization
-define(WB_STATE_BUBLE,  16#17). %% Fuel bubble compensation 
-define(WB_STATE_FANC,   16#18). %% Fan cold start-up 
-define(WB_STATE_COLDR,  16#19). %% Cold start enrichment 
-define(WB_STATE_COOL,   16#1a). %% Cooling 
-define(WB_STATE_LCHGUP, 16#1b). %% Load change PL-FL 
-define(WB_STATE_VENT,   16#1c). %% Ventilation 
-define(WB_STATE_LCHGDN, 16#1d). %% Load change FL-PL 
-define(WB_STATE_NINIT,  16#1e). %% New initialization 
-define(WB_STATE_CTRL,   16#1f). %% Controlled operation 
-define(WB_STATE_CIDDLE, 16#20). %% Control iddle period 
-define(WB_STATE_SSTART, 16#21). %% Soft start 
-define(WB_STATE_STIME,  16#22). %% Savety time 
-define(WB_STATE_PURGE,  16#23). %% Purge 
-define(WB_STATE_START,  16#24). %% Start 
-define(WB_STATE_STAB,   16#25). %% Stabilization 
-define(WB_STATE_SRAMP,  16#26). %% Start ramp
-define(WB_STATE_OOP,    16#27). %% Out of power
-define(WB_STATE_LOCK2,  16#28). %% Interlock
-define(WB_STATE_LOCKADR, 16#29). %% Interlock ADR (Australian design rules) 
-define(WB_STATE_STABT,  16#2a). %% Stabilization time 
-define(WB_STATE_CHGCTRL, 16#2b). %% Change to controlled operation 
-define(WB_STATE_DECIS,  16#2c). %% Decision state 
-define(WB_STATE_PSFS,   16#2d). %% Prestart fuel supply 
-define(WB_STATE_GLOW,   16#2e). %% Glowing 
-define(WB_STATE_GLOWP,  16#2f). %% Glowing power control 
-define(WB_STATE_DELAY,  16#30). %% Delay lowering 
-define(WB_STATE_SLUG,   16#31). %% Sluggish fan start 
-define(WB_STATE_AGLOW,  16#32). %% Additional glowing 
-define(WB_STATE_IGNI,   16#33). %% Ignition interruption 
-define(WB_STATE_IGN,    16#34). %% Ignition 
-define(WB_STATE_IGNII,  16#35). %% Intermittent glowing 
-define(WB_STATE_APMON,  16#36). %% Application monitoring 
-define(WB_STATE_LOCKS,  16#37). %% Interlock save to memory 
-define(WB_STATE_LOCKD,  16#38). %% Heater interlock deactivation 
-define(WB_STATE_OUTCTL, 16#39). %% Output control 
-define(WB_STATE_CPCTL,  16#3a). %% Circulating pump control 
-define(WB_STATE_INITUC, 16#3b). %% Initialization uP 
-define(WB_STATE_SLINT,  16#3c). %% Stray light interrogation 
-define(WB_STATE_PRES,   16#3d). %% Prestart 
-define(WB_STATE_PREIGN, 16#3e). %% Pre-ignition 
-define(WB_STATE_FIGN,   16#3f). %% Flame ignition 
-define(WB_STATE_FSTAB,  16#40). %% Flame stabilization 
-define(WB_STATE_PH,     16#41). %% Combustion process parking heating 
-define(WB_STATE_SH,     16#42). %% Combustion process suppl. heating
-define(WB_STATE_PHFAIL, 16#43). %% Combustion failure failure heating
-define(WB_STATE_SHFAIL, 16#44). %% Combustion failure suppl. heating
-define(WB_STATE_OFFR,   16#45). %% Heater off after run
-define(WB_STATE_CID,    16#46). %% Control iddle after run
-define(WB_STATE_ARFAIL, 16#47). %% After-run due to failure
-define(WB_STATE_ARTCTL, 16#48). %% Time-controlled after-run due to failure 
-define(WB_STATE_LOCKCP, 16#49). %% Interlock circulation pump 
-define(WB_STATE_CIDPH,  16#4a). %% Control iddle after parking heating 
-define(WB_STATE_CIDSH,  16#4b). %% Control iddle after suppl. heating
-define(WB_STATE_CIDHCP, 16#4c). %% Control iddle period suppl. heating with circulation pump 
-define(WB_STATE_CPNOH,  16#4d). %% Circulation pump without heating function 
-define(WB_STATE_OV,     16#4e). %% Waiting loop overvoltage 
-define(WB_STATE_MFAULT, 16#4f). %% Fault memory update 
-define(WB_STATE_WLOOP,  16#50). %% Waiting loop 
-define(WB_STATE_CTEST,  16#51). %% Component test 
-define(WB_STATE_BOOST,  16#52). %% Boost 
-define(WB_STATE_COOL2,  16#53). %% Cooling 
-define(WB_STATE_LOCKP,  16#54). %% Heater interlock permanent 
-define(WB_STATE_FANIDL, 16#55). %% Fan iddle 
-define(WB_STATE_BA,     16#56). %% Break away 
-define(WB_STATE_TINT,   16#57). %% Temperature interrogation 
-define(WB_STATE_PREUV,  16#58). %% Prestart undervoltage 
-define(WB_STATE_AINT,   16#59). %% Accident interrogation 
-define(WB_STATE_ARSV,   16#5a). %% After-run solenoid valve 
-define(WB_STATE_MFLTSV, 16#5b). %% Fault memory update solenoid valve 
-define(WB_STATE_TCARSV, 16#5c). %% Timer-controlled after-run solenoid valve 
-define(WB_STATE_SA,     16#5d). %% Startup attempt 
-define(WB_STATE_PREEXT, 16#5e). %% Prestart extension 
-define(WB_STATE_COMBP,  16#5f). %% Combustion process 
-define(WB_STATE_TIARUV, 16#60). %% Timer-controlled after-run due to undervoltage 
-define(WB_STATE_MFLTSW, 16#61). %% Fault memory update prior switch off 
-define(WB_STATE_RAMPFL, 16#62). %% Ramp full load 
%% byte1 Operating state state number
%% byte2 Device state
-define(WB_DSTATE_STFL,  16#01). %% STFL 
-define(WB_DSTATE_UEHFL, 16#02). %% UEHFL 
-define(WB_DSTATE_SAFL,  16#04). %% SAFL
-define(WB_DSTATE_RZFL,  16#08). %% RZFL 
%%byte3,4,5: Unknown

-define(QUERY_DURATIONS0, 16#0a). %% 24 bytes 

-define(QUERY_DURATIONS1, 16#0b). %% 6 bytes
-define(DUR1_PH, 0). %% Parking heating duration, hh:m 
-define(DUR1_SH, 3). %% Supplemental heating duration hh:m 

-define(QUERY_COUNTERS2, 16#0c).
-define(STA3_SCPH, 0). %%!< 2 bytes, parking heater start counter
-define(STA3_SCSH, 2). %%!< 2 bytes, supplemtal heater start counter 
-define(STA34_FD, 16#00). %%!< Flame detected

-define(QUERY_STATUS2, 16#0f).
-define(STA2_GP, 0). %% glow plug (ignition/flame detection)
-define(STA2_FP, 1). %% fuel pump 
-define(STA2_CAF, 2). %% combustion air fan 
-define(STA2_U0, 3). %% unknown 
-define(STA2_CP, 4). %% (coolant) circulation pump 

-define(QUERY_OPINFO1, 16#11).
-define(OP1_THI, 0). %% Lower temperature threshold 
-define(OP1_TLO, 1). %% Higher temperature threshold 
-define(OP1_U0, 2).

-define(QUERY_DURATIONS2, 16#12). %% 3 bytes 
-define(DUR2_VENT, 0). %% Ventilation duration hh:m 

-define(QUERY_FPW, 16#13). %%!< Fuel prewarming. May not be available. See wbcode 
-define(FPW_R, 0). %%!< 2 bytes: Current fuel prewarming PTC resistance in mili ohm, big endian 
-define(FPW_P, 2). %%!< 2 bytes: Currently applied fuel prewarming power in watts, big endian 

%% 16#51 Command parameters 
-define(IDENT_DEV_ID, 16#01). %%!< Device ID Number 
-define(IDENT_HWSW_VER, 16#02). %%!< Hardware version (KW/Jahr), Software version, Software version EEPROM, 6 bytes 
-define(IDENT_DATA_SET, 16#03). %%!< Data Set ID Number 
-define(IDENT_DOM_CU, 16#04). %%!< Control Unit Herstellungsdatum (Tag monat jahr je ein byte) 
-define(IDENT_DOM_HT, 16#05). %%!< Heizer Herstellungsdatum (Tag monat jahr je ein byte) 
-define(IDENT_TSCODE, 16#06). %%!< Telestart code 
-define(IDENT_CUSTID, 16#07). %%!< Customer ID Number (Die VW Teilenummer als string und noch ein paar Nummern dran) + test sig 
-define(IDENT_U0, 16#08). %%!< ? 
-define(IDENT_SERIAL, 16#09). %%!< 5 bytes: Serial Number. 2 bytes Test signature. 
-define(IDENT_WB_VER, 16#0a). %%!< W-BUS version. Antwort ergibt ein byte. Jedes nibble dieses byte entspricht einer Zahl (Zahl1.Zahl2) 
-define(IDENT_DEV_NAME, 16#0b). %%!< Device Name: Als character string zu interpretieren. 
-define(IDENT_WB_CODE, 16#0c). %%!< W-BUS code. 7 bytes. This is sort of a capability bit field 

%% W-Bus code bits 
-define(WB_CODE_0,     0). %% Unknown supplemental heater feature 
-define(WB_CODE_ON,    3). %% on/off switch capability 
-define(WB_CODE_PH,    4). %% Parking heater capability 
-define(WB_CODE_SH,    5). %% Supplemental heater capability 
-define(WB_CODE_VENT,  6). %% Ventilation capability 
-define(WB_CODE_BOOST, 7). %% Boost capability 

-define(WB_CODE_ECPC,  9). %% External circulation pump control 
-define(WB_CODE_CAV,  10). %% Combustion air fan (CAV) 
-define(WB_CODE_GP,   11). %% Glow Plug (flame detector) 
-define(WB_CODE_FP,   12). %% Fuel pump (FP) 
-define(WB_CODE_CP,   13). %% Circulation pump (CP) 
-define(WB_CODE_VFR,  14). %% Vehicle fan relay (VFR) 
-define(WB_CODE_LEDY, 15). %% Yellow LED 

-define(WB_CODE_LEDG, 16). %% Green LED present 
-define(WB_CODE_ST,   17). %% Spark transmitter. Implies no Glow plug and thus no resistive flame detection 
-define(WB_CODE_SV,   18). %% Solenoid valve present (coolant circuit switching) 
-define(WB_CODE_DI,   19). %% Auxiliary drive indicator (whatever that means) 
-define(WB_CODE_D,    20). %% Generator signal D+ present 
-define(WB_CODE_CAVR, 21). %% Combustion air fan level is in RPM instead of percent 
-define(WB_CODE_22,   22). %% (ZH) 
-define(WB_CODE_23,   23). %% (ZH) 

-define(WB_CODE_CO2,  25). %% CO2 calibration 
-define(WB_CODE_OI,   27). %% Operation indicator (OI) 

-define(WB_CODE_32,   32). %% (ZH) 
-define(WB_CODE_33,   33). %% (ZH) 
-define(WB_CODE_34,   34). %% (ZH) 
-define(WB_CODE_35,   35). %% (ZH) 
-define(WB_CODE_HEW,  36). %% Heating energy is in watts
%% (or if not set in percent and the value field must be divided
%% by 2 to get the percent value) 
-define(WB_CODE_37,   37). %% (ZH) 
-define(WB_CODE_FI,   38). %% Flame indicator (FI) 
-define(WB_CODE_NSH,  39). %% Nozzle Stock heating (NSH) 

-define(WB_CODE_T15,  45). %% Ignition (T15) flag present 
-define(WB_CODE_TTH,  46). %% Temperature thresholds available, command, 16#50). index 16#11 
-define(WB_CODE_VPWR, 47). %% Fuel prewarming resistance and power can be read. 

-define(WB_CODE_SET,  57). %%, 16#02. Set value flame detector resistance (FW-SET),
%% set value combustion air fan revolutions
%% (BG-SET), set value output temperature (AT-SET)

-define(IDENT_SW_ID,   16#0d). %%!< Software ID 

%% Dataset commands are custom and proprietary to this library 
-define(DATASET_COUNT, 16#01). %% Amount of data set entries. 
-define(DATASET_READ,  16#02). %% Read given data set entry. 
-define(DATASET_WRITE, 16#03). %% Write given data set entry. 

%% 053 operational info indexes 
-define(OPINFO_LIMITS, 02).
%% 
%% data format:
%% 1 byte: no idea
%% 2 bytes: Minimum voltage threshold in milivolts
%% 4 bytes: no idea
%% 1 byte: minimum voltage detection delay (seconds)
%% 2 bytes: maximum voltage threshold in milivolts
%% 4 bytes: no idea
%% 1 byte: maximum voltage detection delay (seconds 

%%, 16#56 Error code operand 0 
-define(ERR_LIST, 1). %% send not data. answer is n, code0, counter0-1, code1, counter1-1 ... coden, countern-1 
-define(ERR_READ, 2). %% send code. answer code, flags, counter ... (err_info_t) 
-define(ERR_DEL, 3). %% send no data. answer also no data. 
-define(ERR_TS_LIST, 5). %% Telestart error list. Return 5 bytes: n, code0, counter0-1, code1, counter1-1

%% Error codes
-define(ERR_DEVICE,  16#01). %% Defective control unit
-define(ERR_NOSTART, 16#02). %% No start 
-define(ERR_FLAME,   16#03). %% Flame failure 
-define(ERR_VCCHIGH, 16#04). %% Supply voltage too high 
-define(ERR_FLAME2,  16#05). %% Flame was detected prior to combustion 
-define(ERR_OH,      16#06). %% Heating unit overheated  
-define(ERR_IL,      16#07). %% Heating unit interlocked 
-define(ERR_SCDP,    16#08). %% Metering pump short circuit 
-define(ERR_SCCAF,   16#09). %% Combustion air fan short circuit 
-define(ERR_SCGP,    16#0a). %% Glow plug/flame monitor short circuit 
-define(ERR_SCCP,    16#0b). %% Circulation pump short circuit 
-define(ERR_COMAC,   16#0c). %% No comunication to air condition 
-define(ERR_SCLEDG,  16#0d). %% Green LED short circuit 
-define(ERR_SCLEDY,  16#0e). %% Yellow LED short circuit 
-define(ERR_CFG,     16#0f). %% No configuraton signal 
-define(ERR_SCSV,    16#10). %% Solenoid valve short circuit 
-define(ERR_ECU,     16#11). %% ECU wrong coded 
-define(ERR_WBUS,    16#12). %% W-Bus comunication failure 
-define(ERR_SCVF,    16#13). %% Vehicle fan relay short circuit 
-define(ERR_SCT,     16#14). %% Temperature sensor short circuit 
-define(ERR_BLKCAF,  16#15). %% Combustion air fan blocked 
-define(ERR_SCBAT,   16#16). %% Battery main switch short circuit 
-define(ERR_IAFR,    16#17). %% Invalid air flow reduction 
-define(ERR_COMCS,   16#18). %% Comunication failure on customer specific bus 
-define(ERR_SCGP2,   16#19). %% Glow plug/electronic ignition short circuit 
-define(ERR_SCFD,    16#1a). %% Flame sensor short circuit 
-define(ERR_SCOH,    16#1b). %% Overheat short circuit 
-define(ERR_28,      16#1c). %% Fault 28 
-define(ERR_SCSV2,   16#1d). %% Solenoid valve shed test short circuit 
-define(ERR_SCFS,    16#1e). %% Fuel sensor short circuit 
-define(ERR_SCNSH,   16#1f). %% Nozzle stock heating short circuit 
-define(ERR_SCOI,    16#20). %% Operation indicator short circuit 
-define(ERR_SCFD2,   16#21). %% Flame indicator short circuit 
-define(ERR_RREF,    16#22). %% Reference resistance wrong 
-define(ERR_CRASH,   16#23). %% Crash interlock activated 
-define(ERR_NOFUEL,  16#24). %% Car is almost out of fuel 
-define(ERR_SCFPW,   16#25). %% Fuel pre heating short circuit 
-define(ERR_SCTPCB,  16#26). %% PCB temperatur sensor short circuit 
-define(ERR_ECUGND,  16#27). %% Ground contact to the ECU broken 
-define(ERR_LPV,     16#28). %% Board net energy manager low power voltage 
-define(ERR_FPSND,   16#29). %% Fuel priming still not done 
-define(ERR_TSDATA,  16#2a). %% Error in the radio telegram 
-define(ERR_TSSNP,   16#2b). %% Telestart still not programmed 
-define(ERR_SCP,     16#2c). %% The pressure sensor has short circuit 
-define(ERR_45, 16#2d). %% Fault 45 
-define(ERR_46, 16#2e). %% ... 
-define(ERR_47, 16#2f).
-define(ERR_48, 16#30).
-define(ERR_49, 16#31). %% Fault 49 
-define(ERR_CIDDLE,  16#32). %% No start from control idle period 
-define(ERR_FDSIG,   16#33). %% Flame monitor signal invalid 
-define(ERR_DEFAULT, 16#34). %% Default values entered 
-define(ERR_EOLSNP,  16#35). %% EOL programming has not been carried out 
-define(ERR_SCFUSE,  16#36). %% Thermal fuse short circuit 
-define(ERR_55, 16#37). %% Fault 55 
                 %% ... 
-define(ERR_RELAY,   16#45). %% Error relay box (short circuit/open circuit of heating relay)
-define(ERR_79, 16#4f). %% Fault 79 
-define(ERR_IDDLEUI, 16#50). %% User interface idle-Mode (no-communication) 
-define(ERR_COMUI,   16#51). %% User interface has communication fault 
-define(ERR_COMUI2,  16#52). %% User interface send no defined operating mode 
-define(ERR_FAN,     16#53). %% Heater fan status message negative 
-define(ERR_SCFAN,   16#54). %% Heater fan status bus has short circuit to UB 
-define(ERR_TW,      16#55). %% Temperature water sensor failure 
-define(ERR_SCTW,    16#56). %% Temperature water sensor short circuit to UB 
-define(ERR_OHTW,    16#57). %% Overheating water temperature sensor 
-define(ERR_OSHT,    16#58). %% Overstepping water temperature sensor gradient 
-define(ERR_TBLOW,   16#59). %% Overheating blow temperature sensor 
-define(ERR_OSLT,    16#5a). %% Overstepping low temperature sensor gradient 
-define(ERR_OHPCBT,  16#5b). %% Overheating printed circuit board temperature sensor 
-define(ERR_OSPCBT,  16#5c). %% Overstepping printed circuit board temp sensor gradient 
-define(ERR_TC,      16#5d). %% Cabin temperature sensor failure 
-define(ERR_OSFD,    16#5e). %% Flame detector gradient failure 
-define(ERR_ECOOL,   16#5f). %% Emergency cooling 
-define(ERR_CS1,  16#60). %% Customer specific fault 1 
-define(ERR_CS32, 16#7f). %% Customer specific fault 32 
-define(ERR_128, 16#80). %% Fault 128 
-define(ERR_EOLCHK,  16#81). %% EOL checksum error 
-define(ERR_TEST,    16#82). %% No start during test-run 
-define(ERR_FLAME3,  16#83). %% Flame failure 
-define(ERR_VCCLOW,  16#84). %% Operating voltage too low 
-define(ERR_FDAFTER, 16#85). %% Flame was detected after combustion 
-define(ERR_134, 16#86). %% Fault 134 
-define(ERR_PLOCK, 16#87). %% Heater lock-out permanent 
-define(ERR_DP,     16#88). %% Fuel pump failure 
-define(ERR_INTCAF, 16#89). %% Combustion air fan interruption 
-define(ERR_INTGP,  16#8a). %% Glow plug / flame monitor interruption 
-define(ERR_INTCP,  16#8b). %% Circulation pump interruption 
-define(ERR_140, 16#8c). %% Fault 140 
-define(ERR_INTLEDG, 16#8d). %% Green LED interruption 
-define(ERR_INTLEDY, 16#8e). %% Yellow LED interruption 
-define(ERR_143, 16#8f). %% Fault 143 
-define(ERR_INTSV,   16#90). %% Solenoid valve interruption 
-define(ERR_NEUTRAL, 16#91). %% Control unit locked or coded as neutral 
-define(ERR_REFRESH, 16#92). %% Command refresh failure 
-define(ERR_147, 16#93). %% Fault 147 
-define(ERR_INTT,    16#94). %% Temperature sensor interruption 
-define(ERR_TCAF,    16#95). %% Combustion air fan tight 
-define(ERR_150, 16#96). %% Fault 150 
-define(ERR_OHPOS, 16#97). %% Overheat sensor position wrong 
-define(ERR_152, 16#98). %% Fault 152 (Power supply interruption) 
-define(ERR_INTGP2,  16#99). %% Glow plug / electronic ignition unit interruption 
-define(ERR_INTFD,   16#9a). %% Flame sensor interruption 
-define(ERR_TXSP,    16#9b). %% Setpoint transmitter invalid 
-define(ERR_IVLOW,   16#9c). %% Intelligent undervoltage detection 
-define(ERR_INTSV2,  16#9d). %% Solenoid valve shed test interruption 
-define(ERR_INTFS,   16#9e). %% Fuel sensor interruption 
-define(ERR_INTNSH,  16#9f). %% Nozzle stock heating interruption 
-define(ERR_INTOI,   16#a0). %% Operating indicator interruption 
-define(ERR_INTFD2,  16#a1). %% Flame indicator interruption 
-define(ERR_162, 16#a2). %% Fault 162 
-define(ERR_163, 16#a3). %% Fault 163 
-define(ERR_164, 16#a4). %% Fault 164 
-define(ERR_INTFPW,  16#a5). %% Fuel pre heating interruption 
-define(ERR_INTPCBT, 16#a6). %% PCB temperature sensor interruption 
-define(ERR_167, 16#a7). %% Fault 167 
-define(ERR_EMGR,    16#a8). %% Communication board net energy manager error 
-define(ERR_169, 16#a9). %% Fault 169 
-define(ERR_WBUSTX,  16#aa). %% Send on W-Bus not succeed 
-define(ERR_INTOHS,  16#ab). %% Overheat sensor interruption 
-define(ERR_P,       16#ac). %% The pressure sensor failure 
-define(ERR_173, 16#ad). %% Fault 173 

-define(ERR_181, 16#b5). %% Fault 181 
-define(ERR_INTFUSE, 16#b6). %% Thermal fuse interrupted 
-define(ERR_183, 16#b7). %% Fault 183 
-define(ERR_208, 16#d0). %% Fault 208 

-define(ERR_CS33, 16#e0). %% Customer specific fault 33 
-define(ERR_CS63, 16#fe). %% Customer specific fault 63 

-define(ERR_UNKNOWN, 16#ff). %% Unknown error code 



-define(CO2CAL_READ, 1). %% 3 data bytes: current value, min value, max value. 
-define(CO2CAL_WRITE_C, 2). %% 2 data byte: new current value for cold CO2 cal. 
-define(CO2CAL_WRITE, 3). %% 1 data byte: new current value for hot CO2 cal. 

%% Component test device definitions 
-define(WBUS_TEST_CF, 1). %%!< Combustion Fan 
-define(WBUS_TEST_FP, 2). %%!< Fuel Pump 
-define(WBUS_TEST_GP, 3). %%!< Glow Plug 
-define(WBUS_TEST_CP, 4). %%!< Circulation Pump 
-define(WBUS_TEST_VF, 5). %%!< Vehicle Fan Relays 
-define(WBUS_TEST_SV, 9). %%!< Solenoid Valve 
-define(WBUS_TEST_NSH, 13). %%!< Nozzel Stock Heating 
-define(WBUS_TEXT_NAC, 14). %%!< Nozzle air compressor (not standart, POELI specific) 
-define(WBUS_TEST_FPW, 15). %%!< Fuel Prewarming

-endif.
