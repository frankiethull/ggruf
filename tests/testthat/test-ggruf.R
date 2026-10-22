test_that("is_gguf checks magic bytes", {
  f <- tempfile(fileext = ".gguf")
  write_gguf_fixture(f)
  expect_true(is_gguf(f))

  not <- tempfile()
  writeLines("just text", not)
  expect_false(is_gguf(not))
  expect_false(is_gguf(file.path(tempdir(), "does-not-exist.gguf")))
})

test_that("find_gguf_models finds fixtures by magic, not extension", {
  dir <- tempfile("ggruf-models-")
  dir.create(dir)
  write_gguf_fixture(file.path(dir, "model.gguf"))
  write_gguf_fixture(file.path(dir, "sha256-abc123"))
  writeLines("nope", file.path(dir, "notes.txt"))

  res <- find_gguf_models(dirs = dir, include_ollama = FALSE)
  mine <- res[res$path %in% file.path(dir, c("model.gguf", "sha256-abc123")), ]
  expect_equal(nrow(mine), 2)
  expect_true(all(c("name", "path", "size_mb", "modified", "source") %in% names(res)))
})

test_that("gguf_dirs reports existence", {
  dir <- tempfile("ggruf-dirs-")
  dir.create(dir)
  res <- gguf_dirs(dirs = dir)
  row <- res[res$dir == dir, ]
  expect_equal(nrow(row), 1)
  expect_true(row$exists)
})

test_that("gguf_metadata parses scalars, strings and arrays", {
  f <- tempfile(fileext = ".gguf")
  write_gguf_fixture(f)
  meta <- gguf_metadata(f)

  expect_equal(meta[["general.name"]], "TestGoat")
  expect_equal(meta[["llama.context_length"]], 8192)
  expect_equal(meta[["llama.rope.freq_base"]], 500000)
  expect_true(meta[["tokenizer.ggml.add_bos_token"]])
  expect_equal(meta[["tokenizer.ggml.tokens"]], "<array: STRING x 3>")
  expect_equal(attr(meta, "version"), 3)
  expect_equal(attr(meta, "tensor_count"), 42)
})

test_that("gguf_metadata rejects non-GGUF files", {
  f <- tempfile()
  writeLines("hello", f)
  expect_error(gguf_metadata(f), "Not a GGUF")
})

test_that("gguf_info summarises a model", {
  f <- tempfile(fileext = ".gguf")
  write_gguf_fixture(f)
  info <- gguf_info(f)
  expect_equal(nrow(info), 1)
  expect_equal(info$name, "TestGoat")
  expect_equal(info$architecture, "llama")
  expect_equal(info$quant, "MOSTLY_Q4_K_M")
  expect_equal(info$context_length, 8192)
  expect_equal(info$layers, 32)
  expect_equal(info$tensor_count, 42)
  expect_true(info$has_chat_template)
})

test_that("gguf_chat_template extracts the template", {
  f <- tempfile(fileext = ".gguf")
  write_gguf_fixture(f)
  expect_match(gguf_chat_template(f), "for m in messages", fixed = TRUE)
})

test_that("llama_server_cmd builds a command", {
  cmd <- llama_server_cmd("my model.gguf", port = 9000, n_gpu_layers = 99,
                          bin = "llama-server")
  expect_match(cmd, "llama-server", fixed = TRUE)
  expect_match(cmd, "-ngl", fixed = TRUE)
  expect_match(cmd, "99", fixed = TRUE)
  expect_match(cmd, "--port", fixed = TRUE)
  expect_match(cmd, "9000", fixed = TRUE)
})

test_that("server_health is FALSE for a dead port and never errors", {
  expect_false(server_health(port = 59999))
  res <- server_models(port = 59999)
  expect_equal(nrow(res), 0)
})
