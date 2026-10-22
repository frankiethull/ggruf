# Build small synthetic GGUF files for tests (header + metadata only).

raw_u32 <- function(v) {
  as.raw(c(v %% 256, (v %/% 256) %% 256, (v %/% 65536) %% 256,
           (v %/% 16777216) %% 256))
}

raw_u64 <- function(v) {
  out <- raw(8)
  for (i in 0:7) {
    out[i + 1] <- as.raw((v %/% 256^i) %% 256)
  }
  out
}

gguf_str <- function(s) {
  b <- charToRaw(enc2utf8(s))
  c(raw_u64(length(b)), b)
}

gguf_kv <- function(key, type, value) {
  out <- c(gguf_str(key), raw_u32(type))
  out <- c(out, switch(as.character(type),
    "4" = raw_u32(value),                                   # UINT32
    "6" = writeBin(value, raw(), size = 4, endian = "little"),
    "7" = as.raw(if (value) 1 else 0),                      # BOOL
    "8" = gguf_str(value),                                  # STRING
    "9" = {                                                 # ARRAY of STRING
      parts <- c(raw_u32(8), raw_u64(length(value)))
      for (s in value) parts <- c(parts, gguf_str(s))
      parts
    },
    stop("fixture type not supported: ", type)
  ))
  out
}

write_gguf_fixture <- function(path, tensor_count = 42) {
  kvs <- list(
    gguf_kv("general.name", 8, "TestGoat"),
    gguf_kv("general.architecture", 8, "llama"),
    gguf_kv("general.file_type", 4, 15),
    gguf_kv("llama.context_length", 4, 8192),
    gguf_kv("llama.embedding_length", 4, 4096),
    gguf_kv("llama.block_count", 4, 32),
    gguf_kv("llama.attention.head_count", 4, 32),
    gguf_kv("llama.rope.freq_base", 6, 500000),
    gguf_kv("tokenizer.chat_template", 8, "{% for m in messages %}{{ m.content }}{% endfor %}"),
    gguf_kv("tokenizer.ggml.add_bos_token", 7, TRUE),
    gguf_kv("tokenizer.ggml.tokens", 9, c("<unk>", "<s>", "</s>"))
  )
  blob <- c(
    charToRaw("GGUF"),
    raw_u32(3),                       # version
    raw_u64(tensor_count),
    raw_u64(length(kvs))
  )
  for (kv in kvs) blob <- c(blob, kv)
  writeBin(blob, path)
  invisible(path)
}
