#' Find GGUF models on this machine
#'
#' @description
#' Scans the machine for GGUF model files that `llama.cpp` can load
#' directly (`llama-server -m /path/to/model.gguf`). Files are identified
#' by their GGUF magic bytes, so files without a `.gguf` extension, such
#' as Ollama's blob store, are found too.
#'
#' By default the scan covers:
#' * the `llama.cpp` download cache (`~/.cache/llama.cpp/`),
#' * the Ollama blob store (`~/.ollama/models/blobs/`, or
#'   `$OLLAMA_MODELS/blobs` when that variable is set),
#' * `~/models/`,
#' * any extra directories passed via `dirs`.
#'
#' With `include_ollama = TRUE` (default), models in the local Ollama
#' library are also resolved to their underlying GGUF blob paths by
#' reading Ollama's manifest files, so those rows carry a real model
#' `name` (for example `llama3.2:latest`) instead of a `sha256-...` blob
#' file name. This works from the filesystem alone; no server needs to
#' be running.
#'
#' @param dirs additional directories to scan, on top of the defaults.
#'   Non-existent directories are silently skipped.
#' @param include_ollama resolve Ollama library models to GGUF paths as
#'   well. Defaults to `TRUE`.
#' @param recursive scan directories recursively. Defaults to `TRUE`.
#'
#' @returns A data frame with columns `name`, `path`, `size_mb`,
#'   `modified`, and `source` (`"llama.cpp"`, `"disk"`, `"ollama-blob"`,
#'   or `"ollama"` for manifest-resolved rows). Empty data frame when
#'   nothing is found.
#' @export
#' @examples
#' \donttest{
#' find_gguf_models()
#' }
find_gguf_models <- function(dirs = NULL, include_ollama = TRUE, recursive = TRUE) {
  disk <- .scan_gguf_dirs(.default_gguf_dirs(dirs), recursive = recursive)

  ollama <- if (isTRUE(include_ollama)) {
    .ollama_gguf_models()
  } else {
    .empty_gguf_df()
  }

  res <- rbind(disk, ollama)

  # the ollama blob store is also a default scan dir: drop disk rows
  # whose path was already resolved from an ollama manifest, keeping
  # the row with the human readable name
  dup <- res$source != "ollama" & res$path %in% res$path[res$source == "ollama"]
  res <- res[!dup, , drop = FALSE]

  res <- res[order(res$name), , drop = FALSE]
  rownames(res) <- NULL
  res
}

#' Directories ggruf searches for GGUF models
#'
#' @description
#' Shows the default search directories, what each one is for, and
#' whether it exists on this machine. Handy when
#' [find_gguf_models()] does not find a model you expected: check here
#' first, then pass the right folder via `dirs`.
#'
#' @param dirs additional directories to include in the listing.
#' @returns A data frame with columns `dir`, `purpose`, and `exists`.
#' @export
#' @examples
#' gguf_dirs()
gguf_dirs <- function(dirs = NULL) {
  home <- .home_dir()
  ollama_models <- .ollama_models_dir()

  spec <- data.frame(
    dir = c(
      file.path(home, ".cache", "llama.cpp"),
      file.path(ollama_models, "blobs"),
      file.path(home, "models"),
      dirs
    ),
    purpose = c(
      "llama.cpp download cache",
      "Ollama blob store",
      "user models folder",
      rep("user supplied", length(dirs))
    ),
    stringsAsFactors = FALSE
  )
  spec <- spec[!duplicated(spec$dir), , drop = FALSE]
  spec$exists <- dir.exists(spec$dir)
  rownames(spec) <- NULL
  spec
}

#' Is this file a GGUF model?
#'
#' @description
#' Checks the first four bytes of a file for the `GGUF` magic. This is
#' the same test [find_gguf_models()] applies, exported so you can
#' check a single path yourself.
#'
#' @param path path to a file.
#' @returns `TRUE` when the file exists and starts with the GGUF
#'   magic bytes, `FALSE` otherwise.
#' @export
#' @examples
#' is_gguf(system.file("DESCRIPTION", package = "ggruf"))
is_gguf <- function(path) {
  length(path) == 1L && file.exists(path) && .is_gguf_file(path)
}

# internals ---------------------------------------------------------------

#' @noRd
.home_dir <- function() {
  home <- Sys.getenv("HOME", "~")
  if (identical(.Platform$OS.type, "windows")) {
    home <- Sys.getenv("USERPROFILE", home)
  }
  home
}

#' @noRd
.ollama_models_dir <- function() {
  base <- Sys.getenv("OLLAMA_MODELS", "")
  if (!nzchar(base)) {
    base <- file.path(.home_dir(), ".ollama", "models")
  }
  base
}

#' @noRd
.default_gguf_dirs <- function(dirs = NULL) {
  home <- .home_dir()
  candidates <- c(
    file.path(home, ".cache", "llama.cpp"),
    file.path(.ollama_models_dir(), "blobs"),
    file.path(home, "models"),
    dirs
  )
  unique(candidates[dir.exists(candidates)])
}

#' @noRd
.scan_gguf_dirs <- function(dirs, recursive = TRUE) {
  if (length(dirs) == 0L) {
    return(.empty_gguf_df())
  }

  files <- unlist(
    lapply(dirs, function(d) {
      list.files(d, full.names = TRUE, recursive = recursive, all.files = FALSE)
    }),
    use.names = FALSE
  )
  files <- files[!dir.exists(files)]
  ok <- vapply(files, .is_gguf_file, logical(1), USE.NAMES = FALSE)
  files <- files[ok]

  if (length(files) == 0L) {
    return(.empty_gguf_df())
  }

  info <- file.info(files)
  data.frame(
    name = basename(files),
    path = files,
    size_mb = round(info$size / 1024^2, 1),
    modified = info$mtime,
    source = vapply(files, .classify_source, character(1), USE.NAMES = FALSE),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

#' @noRd
.classify_source <- function(path) {
  norm <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (grepl("/.cache/llama.cpp/", norm, fixed = TRUE)) {
    "llama.cpp"
  } else if (grepl("/.ollama/models/blobs/", norm, fixed = TRUE) ||
             grepl("/.ollama/models/blobs$", norm)) {
    "ollama-blob"
  } else {
    "disk"
  }
}

#' Check GGUF magic bytes (first 4 bytes are "GGUF")
#' @noRd
.is_gguf_file <- function(path) {
  con <- tryCatch(
    suppressWarnings(file(path, "rb")),
    error = function(e) NULL
  )
  if (is.null(con)) {
    return(FALSE)
  }
  on.exit(tryCatch(close(con), error = function(e) NULL), add = TRUE)
  magic <- tryCatch(readBin(con, "raw", n = 4L), error = function(e) raw(0))
  length(magic) == 4L && all(magic == charToRaw("GGUF"))
}

#' Resolve Ollama library models to GGUF blob paths via manifest files
#' @noRd
.ollama_gguf_models <- function() {
  base <- .ollama_models_dir()
  manifests <- file.path(base, "manifests")
  blobs <- file.path(base, "blobs")
  if (!dir.exists(manifests) || !dir.exists(blobs)) {
    return(.empty_gguf_df())
  }

  # manifests/<registry>/<namespace>/<model>/<tag>
  manifest_files <- list.files(manifests, full.names = TRUE, recursive = TRUE)
  manifest_files <- manifest_files[!dir.exists(manifest_files)]

  rows <- lapply(manifest_files, function(mf) {
    rel <- sub(paste0("^", manifests, "/?"), "", mf)
    parts <- strsplit(rel, "/", fixed = TRUE)[[1]]
    if (length(parts) < 3L) {
      return(NULL)
    }
    model <- paste(utils::tail(parts, 2L), collapse = ":")
    manifest <- tryCatch(
      jsonlite::fromJSON(mf, simplifyVector = FALSE),
      error = function(e) NULL
    )
    if (is.null(manifest) || is.null(manifest$layers)) {
      return(NULL)
    }
    weight_layer <- Filter(
      function(l) identical(l$mediaType, "application/vnd.ollama.image.model"),
      manifest$layers
    )
    if (length(weight_layer) == 0L) {
      return(NULL)
    }
    digest <- sub("^sha256:", "sha256-", weight_layer[[1]]$digest)
    blob_path <- file.path(blobs, digest)
    if (!file.exists(blob_path) || !.is_gguf_file(blob_path)) {
      return(NULL)
    }
    info <- file.info(blob_path)
    data.frame(
      name = model,
      path = blob_path,
      size_mb = round(info$size / 1024^2, 1),
      modified = info$mtime,
      source = "ollama",
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) {
    return(.empty_gguf_df())
  }
  do.call(rbind, rows)
}

#' Empty GGUF result data frame (consistent columns)
#' @noRd
.empty_gguf_df <- function() {
  data.frame(
    name = character(0),
    path = character(0),
    size_mb = numeric(0),
    modified = as.POSIXct(character(0)),
    source = character(0),
    stringsAsFactors = FALSE
  )
}
