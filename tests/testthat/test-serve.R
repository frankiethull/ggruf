test_that("server_start/serve/stop works against a fake llama-server", {
  skip_on_cran()
  python <- Sys.which("python3")
  skip_if_not(nzchar(python), "python3 not available")

  # a tiny stand-in for llama-server: /health and /v1/models only
  script <- tempfile("fake-llama-server-", fileext = ".py")
  writeLines(c(
    "import json, sys",
    "from http.server import BaseHTTPRequestHandler, HTTPServer",
    "port = 8080",
    "args = sys.argv[1:]",
    "if '--port' in args: port = int(args[args.index('--port') + 1])",
    "class H(BaseHTTPRequestHandler):",
    "    def do_GET(self):",
    "        if self.path == '/health':",
    "            body, code = {'status': 'ok'}, 200",
    "        elif self.path == '/v1/models':",
    "            body, code = {'object': 'list', 'data': [{'id': 'test-goat', 'object': 'model', 'owned_by': 'llamacpp'}]}, 200",
    "        else:",
    "            body, code = {'error': 'not found'}, 404",
    "        raw = json.dumps(body).encode()",
    "        self.send_response(code)",
    "        self.send_header('Content-Type', 'application/json')",
    "        self.send_header('Content-Length', str(len(raw)))",
    "        self.end_headers()",
    "        self.wfile.write(raw)",
    "    def log_message(self, *a):",
    "        pass",
    "HTTPServer(('127.0.0.1', port), H).serve_forever()"
  ), script)

  wrapper <- tempfile("fake-llama-server-")
  writeLines(c("#!/bin/sh", paste("exec", shQuote(python), shQuote(script), '"$@"')), wrapper)
  Sys.chmod(wrapper, "0755")

  model <- tempfile(fileext = ".gguf")
  write_gguf_fixture(model)

  port <- 18099
  srv <- server_start(model, port = port, bin = wrapper, timeout = 20)
  on.exit(try(server_stop(srv), silent = TRUE), add = TRUE)

  expect_s3_class(srv, "ggruf_server")
  expect_true(server_is_running(srv))
  expect_true(server_health(srv))

  models <- server_models(srv)
  expect_equal(models$id, "test-goat")

  server_stop(srv)
  expect_false(server_is_running(srv))
  expect_false(server_health(srv))
})

test_that("server_start expands tilde paths for the child process", {
  skip_on_cran()
  python <- Sys.which("python3")
  skip_if_not(nzchar(python), "python3 not available")

  # files must live under $HOME for "~" paths to point at them
  home <- path.expand("~")
  dir <- file.path(home, "ggruf-tilde-test")
  dir.create(dir, showWarnings = FALSE)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)

  script <- file.path(dir, "fake_server.py")
  writeLines(c(
    "import json, sys",
    "from http.server import BaseHTTPRequestHandler, HTTPServer",
    "port = 8080",
    "args = sys.argv[1:]",
    "if '--port' in args: port = int(args[args.index('--port') + 1])",
    "class H(BaseHTTPRequestHandler):",
    "    def do_GET(self):",
    "        body = {'status': 'ok'} if self.path == '/health' else {'object': 'list', 'data': [{'id': 'tilde-goat', 'object': 'model', 'owned_by': 'llamacpp'}]}",
    "        raw = json.dumps(body).encode()",
    "        self.send_response(200)",
    "        self.send_header('Content-Type', 'application/json')",
    "        self.send_header('Content-Length', str(len(raw)))",
    "        self.end_headers()",
    "        self.wfile.write(raw)",
    "    def log_message(self, *a):",
    "        pass",
    "HTTPServer(('127.0.0.1', port), H).serve_forever()"
  ), script)
  wrapper <- file.path(dir, "fake-llama-server")
  writeLines(c("#!/bin/sh", paste("exec", shQuote(python), shQuote(script), '"$@"')), wrapper)
  Sys.chmod(wrapper, "0755")

  model <- file.path(dir, "model.gguf")
  write_gguf_fixture(model)

  tilde <- function(p) sub(home, "~", p, fixed = TRUE)
  port <- 18098
  srv <- server_start(tilde(model), port = port, bin = tilde(wrapper), timeout = 20)
  on.exit(try(server_stop(srv), silent = TRUE), add = TRUE)

  expect_true(server_health(srv))
  expect_false(grepl("~", srv$model, fixed = TRUE))
  server_stop(srv)
  expect_false(server_is_running(srv))
})
