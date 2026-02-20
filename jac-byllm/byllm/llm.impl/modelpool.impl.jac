"""ModelPool implementation — spawns LiteLLM proxy for fallback and load-balancing."""
import subprocess;
import socket;
import time;
import tempfile;
import atexit;
import os;
import yaml;
import from byllm.mtir { MTRuntime }

"""Allow pool() call syntax, storing call_params and returning self."""
impl ModelPool.__call__(**kwargs: object) -> ModelPool {
    self.call_params = kwargs;
    return self;
}

"""Initialize the ModelPool by spawning a LiteLLM proxy server.

Generates a proxy config from the provided models, starts the LiteLLM
proxy server on a free port, waits for it to be ready, and creates an
internal Model that routes all calls through the proxy.
"""
impl ModelPool.postinit -> None {
    if len(self.models) == 0 {
        raise ValueError("ModelPool requires at least one model.") ;
    }
    # Find a free port if not specified
    if self.port == 0 {
        self._assigned_port = self._find_free_port();
    } else {
        self._assigned_port = self.port;
    }
    # Generate and write proxy config to a temp YAML file
    config = self._generate_proxy_config();
    config_file = tempfile.NamedTemporaryFile(
        mode='w', suffix='.yaml', prefix='litellm_pool_', delete=False
    );
    yaml.dump(config, config_file, default_flow_style=False);
    config_file.close();
    self._config_path = config_file.name;
    # Start the LiteLLM proxy server as a subprocess
    self._process = subprocess.Popen(
        [
            'litellm', '--config', self._config_path,
            '--host', self.host, '--port', str(self._assigned_port)
        ],
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    );
    # Wait for the proxy server to be ready
    if not self._wait_for_ready() {
        self.shutdown();
        raise RuntimeError(
            "LiteLLM proxy server failed to start. "
            "Ensure litellm proxy dependencies are installed: pip install 'litellm[proxy]'"
        ) ;
    }
    # Determine the model name to use for proxy requests
    model_name = "pool-primary" if self.strategy == "fallback" else "pool-model";
    # Create a Model that connects through the proxy
    base_url = f"http://{self.host}:{self._assigned_port}";
    # Propagate verbose setting from any pool model to the internal proxy model
    verbose = any(m.config.get("verbose", False) for m in self.models);
    self._model = Model(
        model_name=model_name,
        api_key="sk-pool",
        config={"proxy": True, "base_url": base_url, "verbose": verbose},
    );
    logger.info(
        f"ModelPool: LiteLLM proxy started on {base_url} "
        f"with strategy '{self.strategy}' and {len(self.models)} models."
    );
    # Register cleanup on process exit
    atexit.register(self.shutdown);
}

"""Invoke the LLM through the proxy server.

Delegates to the internal Model which routes the call through the
LiteLLM proxy. The proxy handles fallback, load-balancing, retries,
and cost-based routing transparently.
"""
impl ModelPool.invoke(mt_run: MTRuntime) -> object {
    if self._model is None {
        raise RuntimeError("ModelPool proxy not initialized.") ;
    }
    # Pass call_params to the internal model
    if self.call_params {
        self._model.call_params = self.call_params;
    }
    return self._model.invoke(mt_run);
}

"""Generate the LiteLLM proxy config from the provided models.

For 'fallback' strategy: each model gets a unique name with fallback
chain configured in router_settings.
For load-balancing strategies ('simple-shuffle', 'cost-based-routing',
'latency-based-routing'): all models share the same name and the router
distributes calls across them.
"""
impl ModelPool._generate_proxy_config -> dict {
    model_list: list = [];
    if self.strategy == "fallback" {
        # Each model gets a unique name; fallbacks chain from primary
        fallback_names: list = [];
        for (i, model) in enumerate(self.models) {
            name = "pool-primary" if i == 0 else f"pool-fallback-{i}";
            litellm_params: dict = {"model": model.model_name};
            api_key = model.api_key or model.config.get("api_key", "");
            if api_key {
                litellm_params["api_key"] = api_key;
            }
            base_url = (
                model.config.get("base_url")
                or model.config.get("host")
                or model.config.get("api_base")
            );
            if base_url {
                litellm_params["api_base"] = base_url;
            }
            model_list.append({
                "model_name": name,
                "litellm_params": litellm_params,
            });
            if i > 0 {
                fallback_names.append(name);
            }
        }
        config = {
            "model_list": model_list,
            "router_settings": {
                "num_retries": self.num_retries,
                "timeout": self.timeout,
                "fallbacks": [{"pool-primary": fallback_names}],
            },
        };
    } else {
        # For load-balancing strategies, all models share the same name
        for (i, model) in enumerate(self.models) {
            litellm_params: dict = {"model": model.model_name};
            api_key = model.api_key or model.config.get("api_key", "");
            if api_key {
                litellm_params["api_key"] = api_key;
            }
            base_url = (
                model.config.get("base_url")
                or model.config.get("host")
                or model.config.get("api_base")
            );
            if base_url {
                litellm_params["api_base"] = base_url;
            }
            model_list.append({
                "model_name": "pool-model",
                "litellm_params": litellm_params,
            });
        }
        config = {
            "model_list": model_list,
            "router_settings": {
                "routing_strategy": self.strategy,
                "num_retries": self.num_retries,
                "timeout": self.timeout,
            },
        };
    }
    return config;
}

"""Find a free port on the local machine."""
impl ModelPool._find_free_port -> int {
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM);
    sock.bind(('', 0));
    port = sock.getsockname()[1];
    sock.close();
    return port;
}

"""Wait for the proxy server to become ready.

Polls the proxy port until it accepts connections or the timeout expires.
Also checks if the proxy process has exited unexpectedly.
"""
impl ModelPool._wait_for_ready(timeout: float = 30.0) -> bool {
    start = time.time();
    while time.time() - start < timeout {
        # Check if the proxy process died
        if self._process is not None and self._process.poll() is not None {
            stderr = (
                self._process.stderr.read().decode("utf-8")
                if self._process.stderr else ""
            );
            logger.error(
                f"ModelPool: Proxy process exited with code "
                f"{self._process.returncode}. Stderr: {stderr}"
            );
            return False;
        }
        try {
            sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM);
            sock.settimeout(1);
            result = sock.connect_ex((self.host, self._assigned_port));
            sock.close();
            if result == 0 {
                # Give the server a moment to fully initialize
                time.sleep(1.0);
                return True;
            }
        } except Exception {
            _ = None;
        }
        time.sleep(0.5);
    }
    return False;
}

"""Shut down the proxy server and clean up resources."""
impl ModelPool.shutdown -> None {
    if self._process is not None {
        self._process.terminate();
        try {
            self._process.wait(timeout=5);
        } except Exception {
            self._process.kill();
        }
        self._process = None;
    }
    if self._config_path and os.path.exists(self._config_path) {
        try {
            os.unlink(self._config_path);
        } except Exception {
            _ = None;
        }
        self._config_path = "";
    }
}