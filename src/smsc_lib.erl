%% -*- coding: utf-8 -*-
%% @author Andrey Andruschenko <apofiget@gmail.com>
%% @version 0.3
%% @doc Unofficial Erlang SMSC API
%% @reference <a href="http://smsc.ru/api/">SMSC API description and code examples</a>;
%% @end
-module(smsc_lib).
-author("Andrey Andruschenko <apofiget@gmail.com>").

-include("../include/smsc.hrl").

-export([init/0, send_sms/5, send_hlr/3,
         get_status/4, del_req/4, get_balance/2,
         get_oper_info/3, get_stat/4, send_ping_sms/3,
         send_flash_sms/4, get_answers/3, get_answers_after_id/3,
         get_messages/3]).

-type error_details() :: {Reason :: atom(), Object :: binary()}.

-type opts() :: [option()].
%% Send SMS options list
-type option() :: {atom(), Some :: term()}.
%% Send SMS option tuple. Options described <a href="http://smsc.ru/api/http/#send">here</a>

%% @doc Start dependence application.
%% Use it first, before all other functions.
%% @end
-spec(init() -> ok | {error, Message :: string()}).
init() ->
    try [ok,ok,ok,ok,ok] = [application:ensure_started(A) || A <- [asn1, crypto, public_key, ssl, ibrowse]] of
        _ -> ok
    catch _:_ ->
            {error, "Some dependence application not stated"}
    end.

%% @doc Send SMS.
%% Optional parameters described <a href="http://smsc.ru/api/http/#send">here</a>
%% @end
-spec(send_sms(Login :: string(), Pass :: string(), Phones :: list(), Message :: string(),
               Opts :: opts()) -> {ok, Id :: string(), Num :: integer()} | {ok, Obj :: list()} |
                                  {error, Message :: binary()} |
                                  {parser_error, Details :: error_details()}).
send_sms(_Login, _Pass, _Phones, Message, _Opts) when length(Message) > 800 ->
    {error, "Too long message"};

send_sms(Login, Pass, Phones, Message, Opts) ->
    Ops = lists:foldl(fun(K, L) -> proplists:delete(K,L) end, Opts, [login,psw,phones,mes,fmt,id]),
    Oplist = lists:concat(lists:foldl(fun({K,V}, Acc) -> ["&" ++ atom_to_list(K) ++ "=" ++ V  | Acc] end, [], Ops)),
    Request = lists:concat(["login=", Login, "&psw=", Pass, "&phones=", string:join(Phones, ";"), "&mes=", to_cp1251(Message),
                            Oplist, "&fmt=3", "&id=", uuid()]),
    get_reply(?URL ++ "send.php", Request, ?ERR_CODE).

%% @doc Send ping SMS.
%% @end
-spec(send_ping_sms(Login :: string(), Pass :: string(), Phone :: string()) ->
             {ok, Id :: string()} | {ok, Obj :: list()} | {error, Message :: binary()} |
             {parser_error, Details :: error_details()}).
send_ping_sms(Login, Pass, Phone) ->
    Request = lists:concat(["login=", Login, "&psw=", Pass, "&phones=", Phone, "&ping=1&fmt=3", "&id=", uuid()]),
    get_reply(?URL ++ "send.php", Request, ?STATUS_CODE).

%% @doc Send flash SMS.
%% @end
-spec(send_flash_sms(Login :: string(), Pass :: string(), Phones :: list(), Message :: string()) ->
             {ok, Id :: string(), Num :: integer()} | {ok, Obj :: list()} |
             {error, Message :: binary()} | {parser_error, Details :: error_details()}).
send_flash_sms(Login, Pass, Phones, Message) ->
    send_sms(Login, Pass, Phones, Message,[{flash,"1"}]).

%% @doc Send HLR request.
%% @end
-spec(send_hlr(Login :: string(), Pass :: string(), Phones :: list()) ->
             {ok, Id :: string()} | {error, Message :: binary()} |
             {parser_error, Details :: error_details()}).
send_hlr(Login, Pass, Phones) ->
    Request = lists:concat(["login=", Login, "&psw=", Pass, "&phones=", string:join(Phones, ";"), "&hlr=1&fmt=3", "&id=", uuid()]),
    get_reply(?URL ++ "send.php", Request, ?STATUS_CODE).

%% @doc Get SMS/HLR status by Id
%% @end
-spec(get_status(Login :: string(), Pass :: string(), Phones :: list(), Ids :: list()) ->
             {ok, Reply :: list()} | {error, Message :: binary()} |
             {parser_error, Details :: error_details()}).
get_status(Login, Pass, Phones, Ids) ->
    Request = lists:concat(["login=", Login, "&psw=", Pass, "&phone=", string:join(Phones, ","), "&fmt=3", "&id=", string:join(Ids, ",")]),
    case get_reply(?URL ++ "status.php", Request, ?STATUS_CODE) of
        {ok, Obj} ->
            Err = proplists:get_value(err, Obj),
            if Err == 0; Err == undefined ->
                    {ok, Obj};
               true ->
                    {error, proplists:get_value(Err, ?SMS_HRL_ERR_CODE)}
            end;
        Any -> Any
    end.

%% @doc Delete SMS/HLR by Id
%% @end
-spec(del_req(Login :: string(), Pass :: string(), Phones :: list(), Ids :: list()) ->
             {ok, Reply :: list()} | {error, Message :: binary()} |
             {parser_error, Details :: error_details()}).
del_req(Login, Pass, Phones, Ids) ->
    Request = lists:concat(["login=", Login, "&psw=", Pass, "&phone=", string:join(Phones, ","), "&fmt=3", "&id=", string:join(Ids, ","), "&del=1"]),
    case get_reply(?URL ++ "status.php", Request, ?STATUS_CODE) of
        {error, <<"OK">>} ->
            ok;
        Any -> Any
    end.

%% @doc Get account balance and currency
%% @end
-spec(get_balance(Login :: string(), Pass :: string()) ->
             {ok, Balance :: string()} | {error, Message :: binary()} |
             {parser_error, Details :: error_details()}).
get_balance(Login, Pass) ->
    Request = lists:concat(["login=", Login, "&psw=", Pass, "&cur=1&fmt=3"]),
    case get_reply(?URL ++ "balance.php", Request, ?BALANCE_CODE) of
        {ok, Obj} ->
            {ok, proplists:get_value(balance, Obj), proplists:get_value(currency, Obj)};
        Any -> Any
    end.

%% @doc Get mobile operator information
%% @end
-spec(get_oper_info(Login :: string(), Pass :: string(), Phone :: string()) ->
             {ok, Info :: list()} | {error, Message :: binary()} |
             {parser_error, Details :: error_details()}).
get_oper_info(Login, Pass, Phone) ->
    Request = lists:concat(["login=", Login, "&psw=", Pass, "&get_operator=1&fmt=3&phone=",Phone]),
    get_reply(?URL ++ "info.php", Request, ?OPER_CODE).

%% @doc Get account statistics
%% @end
-spec(get_stat(Login :: string(), Pass :: string(), Start :: string(), End :: string()) ->
             {ok, Info :: list()} | {error, Message :: binary()} |
             {parser_error, Details :: error_details()}).
get_stat(Login, Pass, Start, End) ->
    Request = lists:concat(["login=", Login, "&psw=", Pass, "&get_stat=1&fmt=3&mycur=1&start=",Start, "&end=",End]),
    get_reply(?URL ++ "get.php", Request, ?OPER_CODE).

%% @doc Get incomig sms's
%% @end
-spec(get_answers(Login :: string(), Pass :: string(), Hour :: integer()) ->
             {ok, Info :: list()} | {error, Message :: binary()} | {parser_error, Details :: error_details()}).
get_answers(_Login, _Pass, Hour) when Hour < 1 ->
    {error, <<"Hour must be more or equal that 1!">>};
get_answers(Login, Pass, Hour) ->
    Request = lists:concat(["get_answers=1&fmt=3", "&login=", Login, "&psw=", Pass, "&hour=",Hour]),
    get_reply(?URL ++ "get.php", Request, ?BALANCE_CODE).

%% @doc Get incomig sms's after given ID
%% @end
-spec(get_answers_after_id(Login :: string(), Pass :: string(), AfterId :: string()) ->
             {ok, Info :: list()} | {error, Message :: binary()} | {parser_error, Details :: error_details()}).
get_answers_after_id(Login, Pass, AfterId) ->
    Request = lists:concat(["get_answers=1&fmt=3", "&login=", Login, "&psw=", Pass, "&after_id=",AfterId]),
    get_reply(?URL ++ "get.php", Request, ?BALANCE_CODE).

%% @doc Get messages history
%% @end
-spec(get_messages(Login :: string(), Pass :: string(), Count :: integer()) ->
             {ok, Info :: list()} | {error, Message :: binary()} | {parser_error, Details :: error_details()}).
get_messages(_Login, _Pass, Count) when Count < 1 ; Count > 100 ->
    {error, <<"Count should be more or equal that 0 and less or equal that 100!">>};
get_messages(Login, Pass, Count) ->
    Request = lists:concat(["get_messages=1&fmt=3", "&login=", Login, "&psw=", Pass, "&cnt=",Count]),
    get_reply(?URL ++ "get.php", Request, ?STATUS_CODE).

%% Internals
%% Get JSON from application service
%% @hidden
get_reply(Url, Request, Err) ->
    try ibrowse:send_req(Url, [{"Content-Type", "application/x-www-form-urlencoded"}], post, Request, [{response_format, binary}]) of
        {ok, "200", _Headers, Body} -> decode_reply(Body, Err);
        Any -> {error, Any}
    catch _:Error -> {error, Error}
    end.

%% Decode JSON and parse reply
%% @hidden
decode_reply(Json, Err) ->
    try jsx:decode(Json, [{labels, atom}]) of
        Obj -> case proplists:get_value(error_code, Obj) of
                   undefined ->
                       try {ok, binary_to_list(proplists:get_value(id, Obj)), proplists:get_value(cnt, Obj)}
                       catch _:_ ->
                               {ok, Obj} end;
                   Code ->
                       {error, proplists:get_value(Code, Err)}
               end
    catch _:Reason ->
            {parser_error, {Reason, Json}}
    end.


%% Translate string to CP1251
%% @hidden
to_cp1251(Str) ->
    lists:map(
      fun(X) ->
              case X of
                  16#0402 -> 16#80; %% CYRILLIC CAPITAL LETTER DJE
                  16#0403 -> 16#81; %% CYRILLIC CAPITAL LETTER GJE
                  16#201A -> 16#82; %% SINGLE LOW-9 QUOTATION MARK
                  16#0453 -> 16#83; %% CYRILLIC SMALL LETTER GJE
                  16#201E -> 16#84; %% DOUBLE LOW-9 QUOTATION MARK
                  16#2026 -> 16#85; %% HORIZONTAL ELLIPSIS
                  16#2020 -> 16#86; %% DAGGER
                  16#2021 -> 16#87; %% DOUBLE DAGGER
                  16#20AC -> 16#88; %% EURO SIGN
                  16#2030 -> 16#89; %% PER MILLE SIGN
                  16#0409 -> 16#8A; %% CYRILLIC CAPITAL LETTER LJE
                  16#2039 -> 16#8B; %% SINGLE LEFT-POINTING ANGLE QUOTATION MARK
                  16#040A -> 16#8C; %% CYRILLIC CAPITAL LETTER NJE
                  16#040C -> 16#8D; %% CYRILLIC CAPITAL LETTER KJE
                  16#040B -> 16#8E; %% CYRILLIC CAPITAL LETTER TSHE
                  16#040F -> 16#8F; %% CYRILLIC CAPITAL LETTER DZHE
                  16#0452 -> 16#90; %% CYRILLIC SMALL LETTER DJE
                  16#2018 -> 16#91; %% LEFT SINGLE QUOTATION MARK
                  16#2019 -> 16#92; %% RIGHT SINGLE QUOTATION MARK
                  16#201C -> 16#93; %% LEFT DOUBLE QUOTATION MARK
                  16#201D -> 16#94; %% RIGHT DOUBLE QUOTATION MARK
                  16#2022 -> 16#95; %% BULLET
                  16#2013 -> 16#96; %% EN DASH
                  16#2014 -> 16#97; %% EM DASH
                  16#2122 -> 16#99; %% TRADE MARK SIGN
                  16#0459 -> 16#9A; %% CYRILLIC SMALL LETTER LJE
                  16#203A -> 16#9B; %% SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
                  16#045A -> 16#9C; %% CYRILLIC SMALL LETTER NJE
                  16#045C -> 16#9D; %% CYRILLIC SMALL LETTER KJE
                  16#045B -> 16#9E; %% CYRILLIC SMALL LETTER TSHE
                  16#045F -> 16#9F; %% CYRILLIC SMALL LETTER DZHE
                  16#040E -> 16#A1; %% CYRILLIC CAPITAL LETTER SHORT U
                  16#045E -> 16#A2; %% CYRILLIC SMALL LETTER SHORT U
                  16#0408 -> 16#A3; %% CYRILLIC CAPITAL LETTER JE
                  16#0490 -> 16#A5; %% CYRILLIC CAPITAL LETTER GHE WITH UPTURN
                  16#0401 -> 16#A8; %% CYRILLIC CAPITAL LETTER IO
                  16#0404 -> 16#AA; %% CYRILLIC CAPITAL LETTER UKRAINIAN IE
                  16#0407 -> 16#AF; %% CYRILLIC CAPITAL LETTER YI
                  16#0406 -> 16#B2; %% CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
                  16#0456 -> 16#B3; %% CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
                  16#0491 -> 16#B4; %% CYRILLIC SMALL LETTER GHE WITH UPTURN
                  16#0451 -> 16#B8; %% CYRILLIC SMALL LETTER IO
                  16#2116 -> 16#B9; %% NUMERO SIGN
                  16#0454 -> 16#BA; %% CYRILLIC SMALL LETTER UKRAINIAN IE
                  16#0458 -> 16#BC; %% CYRILLIC SMALL LETTER JE
                  16#0405 -> 16#BD; %% CYRILLIC CAPITAL LETTER DZE
                  16#0455 -> 16#BE; %% CYRILLIC SMALL LETTER DZE
                  16#0457 -> 16#BF; %% CYRILLIC SMALL LETTER YI
                  16#0410 -> 16#C0; %% CYRILLIC CAPITAL LETTER A
                  16#0411 -> 16#C1; %% CYRILLIC CAPITAL LETTER BE
                  16#0412 -> 16#C2; %% CYRILLIC CAPITAL LETTER VE
                  16#0413 -> 16#C3; %% CYRILLIC CAPITAL LETTER GHE
                  16#0414 -> 16#C4; %% CYRILLIC CAPITAL LETTER DE
                  16#0415 -> 16#C5; %% CYRILLIC CAPITAL LETTER IE
                  16#0416 -> 16#C6; %% CYRILLIC CAPITAL LETTER ZHE
                  16#0417 -> 16#C7; %% CYRILLIC CAPITAL LETTER ZE
                  16#0418 -> 16#C8; %% CYRILLIC CAPITAL LETTER I
                  16#0419 -> 16#C9; %% CYRILLIC CAPITAL LETTER SHORT I
                  16#041A -> 16#CA; %% CYRILLIC CAPITAL LETTER KA
                  16#041B -> 16#CB; %% CYRILLIC CAPITAL LETTER EL
                  16#041C -> 16#CC; %% CYRILLIC CAPITAL LETTER EM
                  16#041D -> 16#CD; %% CYRILLIC CAPITAL LETTER EN
                  16#041E -> 16#CE; %% CYRILLIC CAPITAL LETTER O
                  16#041F -> 16#CF; %% CYRILLIC CAPITAL LETTER PE
                  16#0420 -> 16#D0; %% CYRILLIC CAPITAL LETTER ER
                  16#0421 -> 16#D1; %% CYRILLIC CAPITAL LETTER ES
                  16#0422 -> 16#D2; %% CYRILLIC CAPITAL LETTER TE
                  16#0423 -> 16#D3; %% CYRILLIC CAPITAL LETTER U
                  16#0424 -> 16#D4; %% CYRILLIC CAPITAL LETTER EF
                  16#0425 -> 16#D5; %% CYRILLIC CAPITAL LETTER HA
                  16#0426 -> 16#D6; %% CYRILLIC CAPITAL LETTER TSE
                  16#0427 -> 16#D7; %% CYRILLIC CAPITAL LETTER CHE
                  16#0428 -> 16#D8; %% CYRILLIC CAPITAL LETTER SHA
                  16#0429 -> 16#D9; %% CYRILLIC CAPITAL LETTER SHCHA
                  16#042A -> 16#DA; %% CYRILLIC CAPITAL LETTER HARD SIGN
                  16#042B -> 16#DB; %% CYRILLIC CAPITAL LETTER YERU
                  16#042C -> 16#DC; %% CYRILLIC CAPITAL LETTER SOFT SIGN
                  16#042D -> 16#DD; %% CYRILLIC CAPITAL LETTER E
                  16#042E -> 16#DE; %% CYRILLIC CAPITAL LETTER YU
                  16#042F -> 16#DF; %% CYRILLIC CAPITAL LETTER YA
                  16#0430 -> 16#E0; %% CYRILLIC SMALL LETTER A
                  16#0431 -> 16#E1; %% CYRILLIC SMALL LETTER BE
                  16#0432 -> 16#E2; %% CYRILLIC SMALL LETTER VE
                  16#0433 -> 16#E3; %% CYRILLIC SMALL LETTER GHE
                  16#0434 -> 16#E4; %% CYRILLIC SMALL LETTER DE
                  16#0435 -> 16#E5; %% CYRILLIC SMALL LETTER IE
                  16#0436 -> 16#E6; %% CYRILLIC SMALL LETTER ZHE
                  16#0437 -> 16#E7; %% CYRILLIC SMALL LETTER ZE
                  16#0438 -> 16#E8; %% CYRILLIC SMALL LETTER I
                  16#0439 -> 16#E9; %% CYRILLIC SMALL LETTER SHORT I
                  16#043A -> 16#EA; %% CYRILLIC SMALL LETTER KA
                  16#043B -> 16#EB; %% CYRILLIC SMALL LETTER EL
                  16#043C -> 16#EC; %% CYRILLIC SMALL LETTER EM
                  16#043D -> 16#ED; %% CYRILLIC SMALL LETTER EN
                  16#043E -> 16#EE; %% CYRILLIC SMALL LETTER O
                  16#043F -> 16#EF; %% CYRILLIC SMALL LETTER PE
                  16#0440 -> 16#F0; %% CYRILLIC SMALL LETTER ER
                  16#0441 -> 16#F1; %% CYRILLIC SMALL LETTER ES
                  16#0442 -> 16#F2; %% CYRILLIC SMALL LETTER TE
                  16#0443 -> 16#F3; %% CYRILLIC SMALL LETTER U
                  16#0444 -> 16#F4; %% CYRILLIC SMALL LETTER EF
                  16#0445 -> 16#F5; %% CYRILLIC SMALL LETTER HA
                  16#0446 -> 16#F6; %% CYRILLIC SMALL LETTER TSE
                  16#0447 -> 16#F7; %% CYRILLIC SMALL LETTER CHE
                  16#0448 -> 16#F8; %% CYRILLIC SMALL LETTER SHA
                  16#0449 -> 16#F9; %% CYRILLIC SMALL LETTER SHCHA
                  16#044A -> 16#FA; %% CYRILLIC SMALL LETTER HARD SIGN
                  16#044B -> 16#FB; %% CYRILLIC SMALL LETTER YERU
                  16#044C -> 16#FC; %% CYRILLIC SMALL LETTER SOFT SIGN
                  16#044D -> 16#FD; %% CYRILLIC SMALL LETTER E
                  16#044E -> 16#FE; %% CYRILLIC SMALL LETTER YU
                  16#044F -> 16#FF; %% CYRILLIC SMALL LETTER YA
                  <<0:8,T/binary>> -> T; %% ALL OTHER
                  Other   -> Other
              end
      end, Str).

%% Generate message ID
%% @hidden
uuid() ->
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:rand_bytes(16),
    lists:concat(io_lib:format("~8.16.0b~4.16.0b4~3.16.0b~4.16.0b~12.16.0b",
                               [A, B, C band 16#0fff, D band 16#3fff bor 16#8000, E])).
