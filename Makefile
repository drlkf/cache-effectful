deps: ## Install the dependencies
	@cabal build all --only-dependencies

build: ## Build the project
	@cabal build all

clean: ## Remove compilation artifacts
	@cabal clean all

test: ## Run the test suite
	@cabal test all

lint: ## Run the code linter (HLint)
	@find test src -name "*.hs" | xargs -P $(PROCS) -I {} hlint --refactor-options="-i" --refactor {}

style: ## Run the code stylers
	@cabal-gild --mode=format --io=cache-effectful.cabal
	@fourmolu -q --mode inplace test src

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.* ?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

PROCS := $(shell nproc)

.PHONY: all $(MAKECMDGOALS)

.DEFAULT_GOAL := help
