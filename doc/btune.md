

# Module btune #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-key">key()</a> ###


<pre><code>
key() = term()
</code></pre>

Key is any `term()`, however in order to use the `plisteners(...)`
functions the key needs to be a tuple.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#bcast-2">bcast/2</a></td><td>Send <code>Msg</code> to processes registered with <code>Key</code> in all connected nodes.</td></tr><tr><td valign="top"><a href="#count-1">count/1</a></td><td>Returns true if there are no listeners for keys that match the given <code>Pattern</code>,
otherwise returns the number of matches for <code>Pattern</code>.</td></tr><tr><td valign="top"><a href="#get_pid-1">get_pid/1</a></td><td>Returns the <code>pid()</code> of the first process matching <code>Key</code>
in the cluster, but it does not include any listeners.</td></tr><tr><td valign="top"><a href="#get_pid-2">get_pid/2</a></td><td>  Same as <a href="#get_pid-1"><code>get_pid/1</code></a> but with the a specified timeout.</td></tr><tr><td valign="top"><a href="#listen-1">listen/1</a></td><td>Adds the calling process to the list of listeners of<code>Key</code>.</td></tr><tr><td valign="top"><a href="#match-1">match/1</a></td><td>match keys againts <code>Pattern</code> as in <a href="ets.md#match-2"><code>ets:match/2</code></a>.</td></tr><tr><td valign="top"><a href="#match-2">match/2</a></td><td>Same as <a href="#match-1"><code>match/1</code></a> but with a specific timeout in milliseconds.</td></tr><tr><td valign="top"><a href="#match_rnd-1">match_rnd/1</a></td><td>Like <a href="#match-1"><code>match/1</code></a> but returns one random entry.</td></tr><tr><td valign="top"><a href="#reg-1">reg/1</a></td><td>Registers the calling process with <code>Key</code>.</td></tr><tr><td valign="top"><a href="#reg-2">reg/2</a></td><td>Registers the calling process with <code>Key</code> and <code>Value</code>.</td></tr><tr><td valign="top"><a href="#unlisten-1">unlisten/1</a></td><td>Removes the calling process from the list of listeners of<code>Key</code>.</td></tr><tr><td valign="top"><a href="#unreg-1">unreg/1</a></td><td>Unregisters the <code>Key</code> that was previously registered by the
calling process.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="bcast-2"></a>

### bcast/2 ###

<pre><code>
bcast(Key::<a href="#type-key">key()</a>, Msg::any()) -&gt; any()
</code></pre>
<br />

Send `Msg` to processes registered with `Key` in all connected nodes.

<a name="count-1"></a>

### count/1 ###

<pre><code>
count(Pattern::term()) -&gt; true | integer()
</code></pre>
<br />

Returns true if there are no listeners for keys that match the given `Pattern`,
otherwise returns the number of matches for `Pattern`.

The `Pattern` is the same as in [`match/1`](#match-1).

<a name="get_pid-1"></a>

### get_pid/1 ###

<pre><code>
get_pid(Key::<a href="#type-key">key()</a>) -&gt; [{pid(), Value::term()}]
</code></pre>
<br />

Equivalent to [`get_pid(Key, DEFAULT_TIMEOUT)`](#get_pid-2).

Returns the `pid()` of the first process matching `Key`
in the cluster, but it does not include any listeners.

The `pid()` returned is that of the first process in the cluster
that was registered with a gproc `{n,l,Key}`. This is the
case when using `{via,gproc,{n,l,Key}}` in any of the gen_xxx
behaviours. By first, it is meant the node that responds first.

btune always uses gproc keys of the form `{p,l,Key}` when you
call `btune:listen(...)`, but many times the listener wants to
send a message back to a server. Servers can be registered using
`{via,gproc,{n,l,Key}}` in any of the gen_xxx `start_link(...)`
functions, and this function will help the listener to get the
pid of that sever in the cluster.

<a name="get_pid-2"></a>

### get_pid/2 ###

<pre><code>
get_pid(Key::<a href="#type-key">key()</a>, Timeout::integer()) -&gt; pid()
</code></pre>
<br />

 Same as [`get_pid/1`](#get_pid-1) but with the a specified timeout.

<a name="listen-1"></a>

### listen/1 ###

<pre><code>
listen(Key::<a href="#type-key">key()</a>) -&gt; true
</code></pre>
<br />

Adds the calling process to the list of listeners of`Key`.

<a name="match-1"></a>

### match/1 ###

<pre><code>
match(Pattern::term()) -&gt; [{<a href="#type-key">key()</a>, pid(), Value::term()}]
</code></pre>
<br />

match keys againts `Pattern` as in [`ets:match/2`](ets.md#match-2).
The easiest way to use this function is to think of
pattern matching in erlang. Simply use a '_' if you
don't want to match on that value and an actual `term()`
if you want to match.

For example `{'_',hello,'_'}` will match
`{ "hi",hello, "it is me" }`, but not
`{ "hi",hey, "it is me"}`.
Here is an example:

```
      n1@host> btune:listen({mykey,param1,param2}).
      true
      ...
      n2@host> btune:listen({mykey,param3,param4}).
      true
      ...
      n1@host> btune:match({mykey,'_','_'}).
      [{{mykey,param1,param2},<0.43,0>,undefined},
         {{mykey,param3,param4},<3332.43.0>,undefined}]
```

If there is no match an empty list `[]` is returned.

A default timeout of 3 seconds is presumed.
Use [`match/2`](#match-2) if you want a different one.

<a name="match-2"></a>

### match/2 ###

<pre><code>
match(Pattern::term(), Timeout::integer()) -&gt; [{<a href="#type-key">key()</a>, pid(), Value::term()}]
</code></pre>
<br />

Same as [`match/1`](#match-1) but with a specific timeout in milliseconds.

<a name="match_rnd-1"></a>

### match_rnd/1 ###

<pre><code>
match_rnd(Pattern::term()) -&gt; {<a href="#type-key">key()</a>, pid(), Value::term()}
</code></pre>
<br />

Like [`match/1`](#match-1) but returns one random entry.
If there is no match a `no_match` error is raised.

<a name="reg-1"></a>

### reg/1 ###

<pre><code>
reg(Key::<a href="#type-key">key()</a>) -&gt; true
</code></pre>
<br />

Registers the calling process with `Key`.

<a name="reg-2"></a>

### reg/2 ###

<pre><code>
reg(Key::<a href="#type-key">key()</a>, Value::term()) -&gt; true
</code></pre>
<br />

Registers the calling process with `Key` and `Value`.

<a name="unlisten-1"></a>

### unlisten/1 ###

<pre><code>
unlisten(Key::<a href="#type-key">key()</a>) -&gt; true
</code></pre>
<br />

Removes the calling process from the list of listeners of`Key`.

<a name="unreg-1"></a>

### unreg/1 ###

<pre><code>
unreg(Key::<a href="#type-key">key()</a>) -&gt; true
</code></pre>
<br />

Unregisters the `Key` that was previously registered by the
calling process.

