REBAR := rebar3

all:
	$(REBAR) compile

shell:
	$(REBAR) shell

test:
	$(REBAR) eunit

release:
	$(REBAR) release

clean:
	rm -rf _build

.PHONY: all shell test release clean
