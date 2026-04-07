"""Test ModelPool streaming fallback path.

Verifies that when the primary model fails before yielding any chunks,
the fallback model takes over and streams its response cleanly.

This is a regression test for the stream_with_fallbacks infinite loop fix:
our _fallback_gen calls Router._completion() per model with fallbacks=[]
to prevent LiteLLM Router from recursing back into the primary model when
a streaming call fails mid-flight.
"""
import from byllm.lib { MockLLM, ModelPool }
import from litellm.exceptions { RateLimitError }
import types;

glob model1 = MockLLM(model_name="gpt-3.5-turbo", config={"outputs": []});
glob model2 = MockLLM(model_name="gpt-4", config={"outputs": []});
glob pool = ModelPool(models=[model1, model2], strategy="fallback");

"""Stream a short answer to the given question."""
def stream_answer(question: str) -> str by pool(stream=True);

def make_chunk(text: str) -> object {
    delta = types.SimpleNamespace(content=text);
    choice = types.SimpleNamespace(delta=delta);
    return types.SimpleNamespace(choices=[choice]);
}

glob _models_tried: list = [];

def mock_completion(model: str, messages: list, stream: bool,
                    fallbacks: list, **_kwargs: object) -> object {
    _models_tried.append(model);
    if model == "pool-primary" {
        raise RateLimitError(
            message="Primary quota exceeded",
            model="gpt-3.5-turbo",
            llm_provider="openai",
        );
    }
    def _gen() -> object {
        yield make_chunk("stream_fallback_");
        yield make_chunk("success");
    }
    return _gen();
}

with entry {
    pool._router._completion = mock_completion;  # type: ignore

    result_stream = stream_answer("test question");
    output = "".join(list(result_stream));

    print(f"output:{output}");
    print(f"primary_tried:{'pool-primary' in _models_tried}");
    print(f"fallback_tried:{'pool-fallback-1' in _models_tried}");
    print(f"order_correct:{_models_tried.index('pool-primary') < _models_tried.index('pool-fallback-1')}");
}
