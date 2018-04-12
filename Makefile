.PHONY: all test

REBAR=./rebar

all: compile

compile: $(REBAR)
	@$(REBAR) compile

clean: $(REBAR)
	@$(REBAR) clean

test: compile
	@$(REBAR) xref eunit

