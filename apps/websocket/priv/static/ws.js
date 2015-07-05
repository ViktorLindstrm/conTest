var nodes = [];
var websocket;
$(document).ready(init);
function init() {
  $('#server').val("ws://" + window.location.host + "/websocket");
  if(!("WebSocket" in window)){  
    $('#status').append('<p><span style="color: red;">websockets are not supported </span></p>');
    $("#navigation").hide();  
  } else {
    $('#status').append('<p><span style="color: green;">websockets are supported </span></p>');
    connect();
  };
  $("#connected").hide(); 	
  $("#content").hide(); 	
};
function connect() {
  wsHost = $("#server").val()
    websocket = new WebSocket(wsHost);
  showScreen('<b>Connecting to: ' +  wsHost + '</b>'); 
  websocket.onopen = function(evt) { onOpen(evt) }; 
  websocket.onclose = function(evt) { onClose(evt) }; 
  websocket.onmessage = function(evt) { onMessage(evt) }; 
  websocket.onerror = function(evt) { onError(evt) }; 
};  
function disconnect() {
  websocket.close();
}; 
function toggle_connection(){
  if(websocket.readyState == websocket.OPEN){
    disconnect();
  } else {
    connect();
  };
};
function addNode() {
  if(websocket.readyState == websocket.OPEN){
    txt = $("#send_txt").val();
    if(txt != "") {
      websocket.send("add "+txt);
      showScreen('sending: ' + txt); 
    }else {
      console.log("error");
    }
  } else {
    showScreen('websocket is not connected'); 
  }
};
function sendTest(ip) {
  if(websocket.readyState == websocket.OPEN){
    websocket.send("test "+ip);
  } else {
    showScreen('websocket is not connected'); 
  }
};
function testAll() {
  if(websocket.readyState == websocket.OPEN){
    console.log("testing all");
    websocket.send("testall");
  } else {
    showScreen('websocket is not connected'); 
  }
};
function removeNode(ip) {
  if(websocket.readyState == websocket.OPEN){
    websocket.send("remove "+ip);
    var i = 0;
    while( nodes[i].ip != ip ) {
      i++;
    }
    nodes.splice(i,1);
    updateNodes();
  } else {
    showScreen('websocket is not connected'); 
  }
};
function onOpen(evt) { 
  websocket.send("init");
  showScreen('<span style="color: green;">CONNECTED </span>'); 
  $("#connected").fadeIn('slow');
  $("#content").fadeIn('slow');
};  
function onClose(evt) { 
  showScreen('<span style="color: red;">DISCONNECTED </span>');
};  
function onMessage(evt) { 
  console.log(evt);
  if(evt.data.substr(0,3) == "add"){
    var node = jQuery.parseJSON(evt.data.substr(4,evt.data.length));
    nodes.push(node);
    updateNodes();
  }else if(evt.data.substr(0,4) == "init"){
    var allNodes = jQuery.parseJSON(evt.data.substr(5,evt.data.length));
    for( j = 0; j < allNodes.length; j++) {
      var node = jQuery.parseJSON(allNodes[j]);
      console.log(node);
      nodes.push(node);
    }
    updateNodes();
  }else if(evt.data.substr(0,7) == "testall"){
    var allNodes = jQuery.parseJSON(evt.data.substr(8,evt.data.length));
    for( j = 0; j < allNodes.length; j++) {
      n = jQuery.parseJSON(allNodes[j]);
      updateNode(n.ip,n.data);
    }
    updateNodes();
  }else if(evt.data.substr(0,4) == "test"){
    var node = jQuery.parseJSON(evt.data.substr(5,evt.data.length));
    updateNode(node.ip,node.data);
    updateNodes();
  } else{
    showScreen('<span style="color: blue;">RESPONSE: ' + evt.data+ '</span>'); 
  }
};  
function updateNode(ip,data){
  i = 0;
  while( nodes[i].ip != ip ) {
    i++;
  }
  nodes[i].data = data;
}
function onError(evt) {
  showScreen('<span style="color: red;">ERROR: ' + evt.data+ '</span>');
};
function showScreen(txt) { 
  $('#output').prepend('<p>' + txt + '</p>');
};
function updateNodes() {
  var htmlString = "<table>";
  for(i = 0; i < nodes.length; i++){
    node = nodes[i];
    htmlString = htmlString + "<tr>";
    htmlString = htmlString + "<td>"+node.ip+"</td>";

    if(node.data != "null"){
      var info = node.data;
      if(info.packets_transmitted.trim() == info.received.trim()){
        htmlString = htmlString + "<td>ok</td>";
      }else{
        htmlString = htmlString + "<td>no connection</td>";
      }
    } else {
      htmlString = htmlString + "<td>no data</td>";
    }

    htmlString = htmlString + '<td><button type="button" onclick="sendTest(\''+node.ip+'\');">test node</button></td>';
    htmlString = htmlString + '<td><button type="button" onclick="removeNode(\''+node.ip+'\');">remove node</button></td>';
    htmlString = htmlString + "</tr>";
  }
  htmlString = htmlString+"</table>";
  $('#nodes').html(htmlString);
}
function clearScreen() 
{ 
  $('#output').html("");
};
