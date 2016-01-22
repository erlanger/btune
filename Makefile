.PHONY: doc eunit
eunit:
	rebar eunit

doc:
	rebar get-deps co edown edoc
