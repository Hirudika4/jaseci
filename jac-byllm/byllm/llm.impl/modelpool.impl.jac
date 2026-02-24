"""ModelPool implementation — uses LiteLLM Router for in-process fallback and load-balancing."""
import from byllm.mtir { MTRuntime }
import from typing { Generator }

"""Allow pool() call syntax, storing call_params and returning self."""
impl ModelPool.__call__(**kwargs: object) -> ModelPool {
    self.call_params = kwargs;
    return self;
}

"""Initialize the ModelPool by building a LiteLLM Router in-process.

Constructs the router model list from the provided BaseLLM models, then
creates a litellm.Router with the requested routing strategy. No subprocess,
no proxy server, no startup latency — the Router handles everything in-process.
"""
impl ModelPool.postinit -> None {
    import from litellm { Router }
    if len(self.models) == 0 {
        raise ValueError("ModelPool requires at least one model.") ;
    }
    model_list: list = [];
    if self.strategy == "fallback" {
        # Each model gets a unique router name; fallbacks chain from primary.
        fallback_names: list = [];
        for (i, model) in enumerate(self.models) {
            name = "pool-primary" if i == 0 else f"pool-fallback-{i}";
            litellm_params: dict = {
                "model": model.model_name,
                "api_key": model.api_key or model.config.get("api_key", "") or None,

            };
            base_url = (
                model.config.get("base_url")
                or model.config.get("host")
                or model.config.get("api_base")
            );
            if base_url {
                litellm_params["api_base"] = base_url;
            }
            model_list.append(
                {
                    "model_name": name,
                    "litellm_params": litellm_params,
                    "model_info": {"id": f"pool-fallback-id-{i}"},

                }
            );
            if i > 0 {
                fallback_names.append(name);
            }
        }
        self._router = Router(
            model_list=model_list,
            fallbacks=[{"pool-primary": fallback_names}],
            num_retries=self.num_retries,
            timeout=self.timeout,
        );
        self._primary_model_name = "pool-primary";
    } else {
        # Load-balancing strategies: all models share the same router name.
        for (i, model) in enumerate(self.models) {
            litellm_params: dict = {
                "model": model.model_name,
                "api_key": model.api_key or model.config.get("api_key", "") or None,

            };
            base_url = (
                model.config.get("base_url")
                or model.config.get("host")
                or model.config.get("api_base")
            );
            if base_url {
                litellm_params["api_base"] = base_url;
            }
            model_list.append(
                {
                    "model_name": "pool-model",
                    "litellm_params": litellm_params,
                    "model_info": {"id": f"pool-model-id-{i}"},

                }
            );
        }
        self._router = Router(
            model_list=model_list,
            routing_strategy=self.strategy,
            num_retries=self.num_retries,
            timeout=self.timeout,
        );
        self._primary_model_name = "pool-model";
    }
    # Update model_name so BaseLLM.make_model_params passes the right router alias.
    self.model_name = self._primary_model_name;
    # Propagate verbose setting from any pool model to BaseLLM.log_info.
    verbose = any(m.config.get("verbose", False) for m in self.models);
    if verbose {
        self.config["verbose"] = True;
    }
    logger.info(
        f"ModelPool: LiteLLM Router initialized "
        f"with strategy '{self.strategy}' and {len(self.models)} models."
    );
}

"""Make a direct model call without streaming via the LiteLLM Router.

The router receives the same model alias used at init time ("pool-primary"
or "pool-model") and handles fallback / load-balancing transparently.
"""
impl ModelPool.model_call_no_stream(params: dict) -> dict {
    if self._router is None {
        raise RuntimeError("ModelPool router not initialized.") ;
    }
    # cost-based-routing requires acompletion() — its deployment selection
    # only runs on the async path (async_filter_deployments).
    if self.strategy == "cost-based-routing" {
        import asyncio;
        response = asyncio.run(self._router.acompletion(**params));
    } else {
        response = self._router.completion(**params);
    }
    return response.model_dump();
}

"""Make a streaming model call via the LiteLLM Router."""
impl ModelPool.model_call_with_stream(params: dict) -> Generator[str, None, None] {
    if self._router is None {
        raise RuntimeError("ModelPool router not initialized.") ;
    }
    # cost-based-routing requires acompletion() on the async path.
    # Collect chunks from the async stream and return them as a list
    # so the sync caller can iterate normally.
    if self.strategy == "cost-based-routing" {
        import asyncio;
        async def _collect_stream -> list {
            result = await self._router.acompletion(stream=True, **params);
            chunks = [];
            async for chunk in result {
                chunks.append(chunk);
            }
            return chunks;
        }
        return asyncio.run(_collect_stream());
    } else {
        return self._router.completion(stream=True, **params);
    }
}

"""Release the LiteLLM Router (no-op cleanup — nothing to terminate)."""
impl ModelPool.shutdown -> None {
    self._router = None;
}
