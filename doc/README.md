

# btune - Gproc-based cluster-wide messaging #

__Version:__ 0.3.1-1-g192527c

__Authors:__ erlanger.

This application implements a simple cluster-wide message broadcasting/listening
library.


### <a name="News">News</a> ###

Version 0.3 includes the [get_pid](https://github.com/erlanger/btune/blob/master/doc/btune.md#get_pid-2) function.


### <a name="Introduction">Introduction</a> ###

It is meant to be a specific type of publish/subscribe model adapted to erlang's
messaging and node structure.


### <a name="Important_definitions">Important definitions</a> ###



<dt>key</dt>



<dd> A <code>key</code> is any erlang <code>term()</code> that identifies
        a desire to listen to messages. In the case of<code>get_pid(...)</code> it identifies a server
        that has been registered <code>{via,grpoc,{n,l,Key}}</code>.</dd>



<dt>cluster</dt>



<dd><p> A cluster is defined as all the nodes connected to
        the current node. In order to have a dynamic cluster,
        where nodes can be automatically discovered you can use<code>nodefinder</code> at <a href="http://github.com/erlanger/nodefinder" target="_top"><tt>http://github.com/erlanger/nodefinder</tt></a>
        to enable automatic discovery of nodes. It is very easy,
        just include the <code>nodefinder</code> app in your release, and
it's done. No code to write.</p>With the use of <code>nodefinder</code> a cluster is dynamically managed
        and all nodes on the LAN having the same erlang cookie and running<code>nodefinder</code> are automatically added so that <code>btune</code> is always 
        connected to all the nodes.</dd>




### <a name="Where">Where</a> ###

* Project's repository: [`https://github.com/erlanger/btune`](https://github.com/erlanger/btune)

* Git clone command: `git clone https://github.com/erlanger/btune.git`



### <a name="Features">Features</a> ###

1. Simple API

1. It is based on [gproc](https://github.com/uwiger/gproc), so its foundation is solid.

1. Broadcast messages cluster-wide without having to know about Pids.

1. Listen to messages without having to know about registered servers.

1. Match keys with a pattern similar to ets:match().



### <a name="Docs">Docs</a> ###
Click on the `btune` module below to get to the documentation.

## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="btune.md" class="module">btune</a></td></tr></table>

