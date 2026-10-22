#' Read GGUF metadata from a model file
#'
#' @description
#' Parses the GGUF header in pure R and returns the metadata key/value
#' pairs stored in the file: model name, architecture, quantisation,
#' training context length, tokenizer settings, and so on. No model is
#' loaded and no external tool is needed, the file is only read up to
#' the end of its metadata section.
#'
#' Scalar values come back as R values. Array values (for example the
#' tokenizer vocabulary) are not materialised; they are summarised as
#' a string like `"<array: STRING x 32000>"`.
#'
#' @param path path to a GGUF file.
#' @returns A named list of metadata values, with attributes `version`
#'   (GGUF format version), `tensor_count`, and `metadata_count`.
#' @export
#' @examples
#' \dontrun{
#' meta <- gguf_metadata("~/models/llama-3.2-3b-q4_k_m.gguf")
#' meta[["general.name"]]
#' meta[["llama.context_length"]]
#' }
gguf_metadata <- function(path) {
  if (!file.exists(path)) {
    stop("File not found: ", path, call. = FALSE)
  }
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)

  magic <- readBin(con, "raw", n = 4L)
  if (length(magic) != 4L || !all(magic == charToRaw("GGUF"))) {
    stop("Not a GGUF file: ", path, call. = FALSE)
  }
  version <- .gguf_read_u32(con)
  if (version >= 3L) {
    tensor_count <- .gguf_read_u64(con)
    kv_count <- .gguf_read_u64(con)
  } else {
    tensor_count <- .gguf_read_u32(con)
    kv_count <- .gguf_read_u32(con)
  }

  out <- list()
  for (i in seq_len(kv_count)) {
    key <- .gguf_read_string(con, capture_limit = Inf)
    if (is.na(key)) {
      stop("Could not read metadata key ", i, " in: ", path, call. = FALSE)
    }
    type <- .gguf_read_u32(con)
    out[[key]] <- .gguf_read_value(con, type)
  }

  attr(out, "version") <- version
  attr(out, "tensor_count") <- tensor_count
  attr(out, "metadata_count") <- kv_count
  out
}

#' One row summary of a GGUF model
#'
#' @description
#' Reads [gguf_metadata()] and picks out the fields people usually
#' want to know about a model file, as a one row data frame.
#'
#' @param path path to a GGUF file.
#' @returns A data frame with one row and columns `name`, `path`,
#'   `architecture`, `quant`, `file_type`, `context_length`,
#'   `embedding_length`, `layers`, `attention_heads`, `tensor_count`,
#'   `size_mb`, and `has_chat_template`.
#' @export
#' @examples
#' \dontrun{
#' gguf_info("~/models/llama-3.2-3b-q4_k_m.gguf")
#' }
gguf_info <- function(path) {
  meta <- gguf_metadata(path)
  arch <- meta[["general.architecture"]]
  arch <- if (is.null(arch)) NA_character_ else arch

  pick <- function(key) {
    v <- if (is.na(arch)) NULL else meta[[paste(arch, key, sep = ".")]]
    if (is.null(v) || length(v) != 1L) NA_real_ else as.numeric(v)
  }

  file_type <- meta[["general.file_type"]]
  file_type <- if (is.null(file_type)) NA_real_ else as.numeric(file_type)

  name <- meta[["general.name"]]
  if (is.null(name) || !nzchar(name)) {
    name <- basename(path)
  }

  info <- file.info(path)
  data.frame(
    name = name,
    path = path,
    architecture = arch,
    quant = .gguf_ftype_label(file_type),
    file_type = file_type,
    context_length = pick("context_length"),
    embedding_length = pick("embedding_length"),
    layers = pick("block_count"),
    attention_heads = pick("attention.head_count"),
    tensor_count = attr(meta, "tensor_count"),
    size_mb = round(info$size / 1024^2, 1),
    has_chat_template = !is.null(meta[["tokenizer.chat_template"]]),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

#' Extract the chat template from a GGUF model
#'
#' @description
#' Many GGUF models embed a Jinja chat template in their metadata.
#' This pulls just that string out, which is handy when debugging why
#' a model formats chat prompts the way it does.
#'
#' @param path path to a GGUF file.
#' @returns The chat template as a single string, or `NA_character_`
#'   when the model does not embed one.
#' @export
#' @examples
#' \dontrun{
#' cat(gguf_chat_template("~/models/llama-3.2-3b-q4_k_m.gguf"))
#' }
gguf_chat_template <- function(path) {
  meta <- gguf_metadata(path)
  tmpl <- meta[["tokenizer.chat_template"]]
  if (is.null(tmpl)) NA_character_ else tmpl
}

# GGUF binary reader ------------------------------------------------------

# type codes from the GGUF spec
.gguf_types <- c(
  "UINT8", "INT8", "UINT16", "INT16", "UINT32", "INT32", "FLOAT32",
  "BOOL", "STRING", "ARRAY", "UINT64", "INT64", "FLOAT64"
)
.gguf_scalar_sizes <- c(1, 1, 2, 2, 4, 4, 4, 1, NA, NA, 8, 8, 8)

#' @noRd
.gguf_read_raw <- function(con, n) {
  if (n == 0) {
    return(raw(0))
  }
  r <- readBin(con, "raw", n = n)
  if (length(r) != n) {
    stop("Unexpected end of GGUF file", call. = FALSE)
  }
  r
}

#' @noRd
.gguf_read_u32 <- function(con) {
  r <- .gguf_read_raw(con, 4L)
  sum(as.numeric(r) * c(1, 256, 65536, 16777216))
}

#' @noRd
.gguf_read_u64 <- function(con) {
  r <- .gguf_read_raw(con, 8L)
  sum(as.numeric(r) * 256^(0:7))
}

#' @noRd
.raw_to_numeric <- function(bytes, size) {
  con <- rawConnection(bytes, "rb")
  on.exit(close(con), add = TRUE)
  readBin(con, "numeric", n = 1L, size = size, endian = "little")
}

#' @noRd
.gguf_skip <- function(con, n) {
  if (n > 0) {
    seek(con, where = n, origin = "current")
  }
  invisible(NULL)
}

#' @noRd
.gguf_read_string <- function(con, capture_limit = 100000) {
  len <- .gguf_read_u64(con)
  if (len > capture_limit) {
    .gguf_skip(con, len)
    return(NA_character_)
  }
  if (len == 0) {
    return("")
  }
  rawToChar(.gguf_read_raw(con, len))
}

#' @noRd
.gguf_read_value <- function(con, type) {
  # type is 0-based in the spec
  switch(as.character(type),
    "0" = as.numeric(.gguf_read_raw(con, 1L)),
    "1" = {
      v <- as.numeric(.gguf_read_raw(con, 1L))
      if (v > 127) v - 256 else v
    },
    "2" = {
      r <- .gguf_read_raw(con, 2L)
      sum(as.numeric(r) * c(1, 256))
    },
    "3" = {
      r <- .gguf_read_raw(con, 2L)
      v <- sum(as.numeric(r) * c(1, 256))
      if (v > 32767) v - 65536 else v
    },
    "4" = .gguf_read_u32(con),
    "5" = {
      v <- .gguf_read_u32(con)
      if (v > 2147483647) v - 4294967296 else v
    },
    "6" = .raw_to_numeric(.gguf_read_raw(con, 4L), 4L),
    "7" = {
      as.logical(as.numeric(.gguf_read_raw(con, 1L)))
    },
    "8" = .gguf_read_string(con),
    "9" = {
      elem_type <- .gguf_read_u32(con)
      n <- .gguf_read_u64(con)
      .gguf_skip_array(con, elem_type, n)
      sprintf(
        "<array: %s x %.0f>",
        .gguf_types[elem_type + 1L], n
      )
    },
    "10" = .gguf_read_u64(con),
    "11" = {
      v <- .gguf_read_u64(con)
      # loses precision above 2^53, acceptable for a metadata reader
      v
    },
    "12" = .raw_to_numeric(.gguf_read_raw(con, 8L), 8L),
    stop("Unknown GGUF metadata type: ", type, call. = FALSE)
  )
}

#' @noRd
.gguf_skip_array <- function(con, elem_type, n) {
  if (n == 0) {
    return(invisible(NULL))
  }
  if (elem_type == 8L) {
    # strings: each is length-prefixed, walk the lengths
    for (i in seq_len(n)) {
      len <- .gguf_read_u64(con)
      .gguf_skip(con, len)
    }
  } else {
    size <- .gguf_scalar_sizes[elem_type + 1L]
    if (is.na(size)) {
      stop("Nested GGUF arrays are not supported", call. = FALSE)
    }
    .gguf_skip(con, size * n)
  }
  invisible(NULL)
}

#' @noRd
.gguf_ftype_label <- function(file_type) {
  if (length(file_type) == 0L || is.na(file_type)) {
    return(NA_character_)
  }
  labels <- c(
    "0" = "ALL_F32",
    "1" = "MOSTLY_F16",
    "2" = "MOSTLY_Q4_0",
    "3" = "MOSTLY_Q4_1",
    "4" = "MOSTLY_Q4_1_SOME_F16",
    "7" = "MOSTLY_Q8_0",
    "8" = "MOSTLY_Q5_0",
    "9" = "MOSTLY_Q5_1",
    "10" = "MOSTLY_Q2_K",
    "11" = "MOSTLY_Q3_K_S",
    "12" = "MOSTLY_Q3_K_M",
    "13" = "MOSTLY_Q3_K_L",
    "14" = "MOSTLY_Q4_K_S",
    "15" = "MOSTLY_Q4_K_M",
    "16" = "MOSTLY_Q5_K_S",
    "17" = "MOSTLY_Q5_K_M",
    "18" = "MOSTLY_Q6_K"
  )
  lab <- labels[as.character(as.integer(file_type))]
  if (is.na(lab)) paste0("Ftype_", as.integer(file_type)) else unname(lab)
}
