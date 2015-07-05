-module(ws_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
  erlang:register(browser,self()),
	{ok, Req, undefined_state}.

websocket_handle({text, <<"init">>}, Req, State) ->
  {ok,IPS} = conTest_mngr:get_nodes(),
  Nodes = lists:map(fun(IP) -> 
                        ip_status_to_json(IP,"\"null\"")
                    end,IPS) ,
  BinNodes = erlang:list_to_binary(io_lib:format("~p",[Nodes])),
	{reply, {text, <<"init ",BinNodes/binary>>}, Req, State};

websocket_handle({text, <<"remove ",BinaryIP/binary>>}, Req, State) ->
  IP = erlang:binary_to_list(BinaryIP),
  conTest_mngr:delete_node(IP),
	{reply, {text, <<"remove ",BinaryIP/binary>>}, Req, State};

websocket_handle({text, <<"add ",BinaryIP/binary>>}, Req, State) ->
  IP = erlang:binary_to_list(BinaryIP),
  conTest_mngr:start_node(IP),
  Node = erlang:list_to_binary(ip_status_to_json(IP,"\"null\"")),
	{reply, {text, <<"add ",Node/binary>>}, Req, State};

websocket_handle({text, <<"testall">>}, Req, State) ->
  {ok,Ret} = conTest_mngr:solicit_all(),
  Nodes = lists:map(fun({IP,RawData}) -> 
                        io:format("~p~n",[RawData]),
                        case RawData of 
                          unknown_host ->
                            ip_status_to_json(IP,"\"null\"");
                          _-> 
                            Data = status_to_json(RawData),
                            ip_status_to_json(IP,Data) 
                        end
                    end, 
                    Ret),
  BinNodes = erlang:list_to_binary(io_lib:format("~p",[Nodes])), 
  
	{reply, {text,<<"testall ",BinNodes/binary>> }, Req, State};

websocket_handle({text, <<"test ",BinaryIP/binary>>}, Req, State) ->
  IP = erlang:binary_to_list(BinaryIP),
  Node = case conTest_mngr:solicit_node(IP) of 
           {ok,Ret} ->
             Data = status_to_json(Ret),
             erlang:list_to_binary(ip_status_to_json(IP,Data));
           {error,_} ->
             erlang:list_to_binary(ip_status_to_json(IP,"\"null\""))
         end,
  {reply, {text,<<"test ",Node/binary>> }, Req, State};


websocket_handle({text, Msg}, Req, State) ->
	{reply, {text, << "That's what she said! ", Msg/binary >>}, Req, State};

websocket_handle(_Data, Req, State) ->
	{ok, Req, State}.

websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.
status_to_json({X1,X2,X3,X4,X5,X6,X7,X8}) ->
  "{\"" ++ X1++ "\":\" "++X2++"\",\""++ X3 ++ "\" :\"" ++X4++"\" ,\""++ X5 ++ "\" : \""++X6++"\", \""++X7++"\" :\""++ X8++"\"}".

ip_status_to_json(IP,Data) ->
  "{\"ip\": \""++IP++"\", \"data\":"++Data++"}".

