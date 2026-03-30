"""ModelPool implementation — uses LiteLLM Router for in-process fallback and load-balancing."""
import from byllm.mtir { MTRuntime }
import from typing { Generator }
import concurrent.futures;
import asyncio;

"""Initialize the ModelPool by building a LiteLLM Router in-process.

Constructs the router model list from the provided BaseLLM models, then
creates a litellm.Router with the requested routing strategy. No subprocess,
no proxy server, no startup latency — the Router handles everything in-process.
"""
impl ModelPool.postinit -> None {
    import from litellm { Router }
    if len(self.models) == 0 {
        raise ValueError("ModelPool requires at least one model.");
    }
    is_fallback = self.strategy == "fallback";
    model_list: list = [];
    fallback_names: list = [];
    for (i, model) in enumerate(self.models) {
        # Determine router alias: fallback uses unique names; load-balancing shares one.
        name = (
            ("pool-primary" if i == 0 else f"pool-fallback-{i}")
                if is_fallback
                else "pool-model"
        );
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
                "model_info": {"id": f"pool-model-id-{i}"},

            }
        );
        if is_fallback and i > 0 {
            fallback_names.append(name);
        }
    }
    if is_fallback {
        self._router = Router(
            model_list=model_list,
            fallbacks=[{"pool-primary": fallback_names}],
            num_retries=self.num_retries,
            timeout=self.timeout,
        );
        self._primary_model_name = "pool-primary";
    } else {
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

For most strategies, router.completion() works synchronously.
For cost-based-routing, the sync get_available_deployment() has no handler
for that strategy, so we must use acompletion(). To remain safe inside
frameworks that already have a running event loop (FastAPI, Jupyter, etc.),
we run acompletion() via a ThreadPoolExecutor so asyncio.run() always gets
a fresh event loop in a dedicated thread.
"""
impl ModelPool.model_call_no_stream(params: dict) -> dict {
    if self._router is None {
        raise RuntimeError("ModelPool router not initialized.");
    }
    response: dict;
    if self.strategy == "cost-based-routing" {
        
        coro = self._router.acompletion(**params);  # type: ignore
        with concurrent.futures.ThreadPoolExecutor(max_workers=1) as pool {
            response = pool.submit(asyncio.run, coro).result();  # type: ignore
        }
    } else {
        response = self._router.completion(**params);  # type: ignore
    }
    return response;
}

"""Make a streaming model call via the LiteLLM Router.

For cost-based-routing, acompletion(stream=True) is used and chunks are
collected synchronously via a ThreadPoolExecutor (same reasoning as
model_call_no_stream). All other strategies use router.completion(stream=True).
"""
impl ModelPool.model_call_with_stream(params: dict) -> Generator[str, None, None] {
    if self._router is None {
        raise RuntimeError("ModelPool router not initialized.");
    }
    if self.strategy == "cost-based-routing" {
        
        async def _collect_stream -> list {
            result = await self._router.acompletion(stream=True, **params);
            chunks = [];
            async for chunk in result {
                chunks.append(chunk);
            }
            return chunks;
        }
        with concurrent.futures.ThreadPoolExecutor(max_workers=1) as pool {
            return pool.submit(asyncio.run, _collect_stream()).result();  # type: ignore
        }
    }
    return self._router.completion(stream=True, **params);  # type: ignore
}

"""Release the LiteLLM Router (no-op cleanup — nothing to terminate)."""
impl ModelPool.shutdown -> None {
    self._router = None;
}
