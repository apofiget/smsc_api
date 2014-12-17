REBAR = `which rebar`

compile:
	@(if test ! -d "deps"; then mkdir deps ; fi)
	@$(REBAR) get-deps
	@$(REBAR) compile

clean:
	@$(REBAR) clean
	@rm -f erl_crash.dump
	@rm -rf ./doc

doc:
	@$(REBAR) doc

start:
	ERL_LIBS=deps erl -pa ebin deps/*/ebin -sname smsc

.PHONY: compile
