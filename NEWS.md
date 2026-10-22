# ggruf 0.1.1

* Fixed `server_start()` failing on `~` paths: the model path and
  `bin` are now expanded before being handed to the child process
  (caught in real end-to-end testing with llama.cpp b11796 and
  SmolLM2-360M-Instruct).

# ggruf 0.1.0

* First release: `find_gguf_models()` and `gguf_dirs()` (Find),
  `gguf_metadata()`, `gguf_info()`, `gguf_chat_template()` and
  `is_gguf()` (Inspect), and `llama_bin()`, `llamacpp_version()`,
  `llama_server_cmd()`, `server_start()`, `server_stop()`,
  `server_is_running()`, `server_health()` and `server_models()`
  (Serve).
