

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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#bcast-2">bcast/2</a></td><td>Send <code>Msg</code> to all connected nodes.</td></tr><tr><td valign="top"><a href="#listen-1">listen/1</a></td><td>Adds the calling process to the list of listeners of<code>Key</code>.</td></tr><tr><td valign="top"><a href="#match-1">match/1</a></td><td>match keys againts <code>Pattern</code> as in <a href="ets.md#match-2"><code>ets:match/2</code></a>.</td></tr><tr><td valign="top"><a href="#match-2">match/2</a></td><td>Same as <a href="#pmatch-1"><code>pmatch/1</code></a> but with a specific timeout in milliseconds.</td></tr><tr><td valign="top"><a href="#unlisten-1">unlisten/1</a></td><td>Removes the calling process from the list of listeners of<code>Key</code>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="bcast-2"></a>

### bcast/2 ###


<pre><code>
bcast(Key::<a href="#type-key">key()</a>, Msg::any()) -&gt; any()
</code></pre>
<br />

Send `Msg` to all connected nodes.
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
match(Pattern::tuple()) -&gt; [<a href="#type-key">key()</a>]
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
      [{mykey,param1,param2},{mykey,param3,param4}]
```


A default timeout of 3 seconds is presumed.
Use [`match/2`](#match-2) if you want a different one.
<a name="match-2"></a>

### match/2 ###


<pre><code>
match(Pattern::tuple(), Timeout::integer()) -&gt; [<a href="#type-key">key()</a>]
</code></pre>
<br />

Same as [`pmatch/1`](#pmatch-1) but with a specific timeout in milliseconds.
<a name="unlisten-1"></a>

### unlisten/1 ###


<pre><code>
unlisten(Key::<a href="#type-key">key()</a>) -&gt; true
</code></pre>
<br />

Removes the calling process from the list of listeners of`Key`.
