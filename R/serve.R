#' Find a llama.cpp binary
#'
#' @description
#' Looks for a `llama.cpp` binary (by default `llama-server`) in the
#' usual places: the `PATH`, the directory in the `LLAMACPP_DIR` env
#' var (its `build/bin` subdir is checked too), a local build at
#' `~/llama.cpp/build/bin`, Homebrew, and the standard system dirs.
#'
#' @param name binary name, for example `"llama-server"` or
#'   `"llama-cli"`.
#' @returns The full path to the binary, or `NA_character_` when it
#'   cannot be found.
#' @export
#' @examples
#' llama_bin()
llama_bin <- function(name = "llama-server") {
  on_path <- Sys.which(name)
  if (nzchar(on_path)) {
    return(unname(on_path))
  }

  home <- .home_dir()
  roots <- c(
    Sys.getenv("LLAMACPP_DIR", ""),
    file.path(home, "llama.cpp"),
    file.path(home, ".local"),
    "/opt/homebrew",
    "/usr/local",
    "/usr"
  )
  subs <- c("build/bin", "bin", "")
  for (root in roots[nzchar(roots)]) {
    for (sub in subs) {
      cand <- file.path(root, sub, name)
      if (file.exists(cand) && file.access(cand, 1L) == 0L) {
        return(cand)
      }
    }
  }
  NA_character_
}

#' Version of a llama.cpp binary
#'
#' @param bin path to a llama.cpp binary, or `NULL` to find
#'   `llama-server` with [llama_bin()].
#' @returns The first line of the binary's `--version` output, or
#'   `NA_character_` when the binary cannot be found or run.
#' @export
#' @examples
#' \donttest{
#' llamacpp_version()
#' }
llamacpp_version <- function(bin = NULL) {
  if (is.null(bin)) {
    bin <- llama_bin("llama-server")
  }
  if (length(bin) == 1L && !is.na(bin)) {
    bin <- path.expand(bin)
  }
  if (length(bin) == 0L || is.na(bin)) {
    return(NA_character_)
  }
  res <- tryCatch(
    processx::run(
      bin, "--version", error_on_status = FALSE, timeout = 10,
      env = .spawn_env()
    ),
    error = function(e) NULL
  )
  if (is.null(res)) {
    return(NA_character_)
  }
  out <- trimws(c(res$stdout, res$stderr))
  out <- out[nzchar(out)]
  if (length(out) == 0L) {
    return(NA_character_)
  }
  head <- out[1]
  if (.looks_like_loader_error(head)) {
    NA_character_
  } else {
    head
  }
}

.looks_like_loader_error <- function(line) {
  # e.g. "/opt/llama-server: /usr/lib/.../libc.so.6: version
  # `GLIBC_2.38' not found (required by ...)"
  grepl(
    "version `.*' not found|symbol lookup error|cannot execute binary file|exec format error|permission denied",
    line,
    ignore.case = TRUE
  )
}

.env_cache <- new.env(parent = emptyenv())

.processx_has_pdeathsig <- function() {
  cached <- .env_cache$has_pdeathsig
  if (!is.null(cached)) {
    return(cached)
  }
  ok <- tryCatch(
    utils::compareVersion(
      as.character(utils::packageVersion("processx")), "3.9.0"
    ) >= 0,
    error = function(e) FALSE
  )
  .env_cache$has_pdeathsig <- ok
  ok
}

#' Environment for child processes
#'
#' R's own environment is a bad starting point for `llama-server`: the
#' `R` wrapper script or the GUI/IDE sets a `LD_LIBRARY_PATH` that can
#' shadow the libc a nix-built llama.cpp needs (system GLIBC wins the
#' lookup and the binary dies on launch), and R-only variables from
#' `.Renviron` (say `CUDA_VISIBLE_DEVICES`) leak into the child. Spawn
#' children with the environment of a plain login shell instead, and
#' fall back to R's env minus the loader variables when that fails.
#' @noRd
.spawn_env <- function() {
  if (!identical(.Platform$OS.type, "unix")) {
    return(c("current", LD_LIBRARY_PATH = "", LD_PRELOAD = ""))
  }
  cached <- .env_cache$env
  if (!is.null(cached)) {
    return(cached)
  }
  sh <- Sys.which("bash")
  if (!nzchar(sh)) {
    sh <- Sys.which("sh")
  }
  env <- NULL
  if (nzchar(sh)) {
    # a minimal env for the capture shell itself: the shell then
    # rebuilds its own environment from /etc/profile, ~/.profile and
    # ~/.bashrc, without any of R's (or the IDE's) inherited variables
    # leaking in. A login shell (`-l`) covers system defaults, an
    # interactive shell (`-i`) covers ~/.bashrc exports; merge both,
    # with the interactive one winning.
    base <- c(
      PATH = "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games",
      HOME = path.expand("~"),
      TERM = "dumb"
    )
    env <- .capture_env(sh, c("-lc", "env"), base)
    if (grepl("bash$", sh)) {
      env <- .merge_env(env, .capture_env(sh, c("-ic", "env"), base))
    }
  }
  if (is.null(env)) {
    env <- c("current", LD_LIBRARY_PATH = "", LD_PRELOAD = "")
  }
  .env_cache$env <- env
  env
}

.capture_env <- function(sh, flags, base) {
  res <- tryCatch(
    processx::run(
      basename(sh), c(flags, "env"),
      error_on_status = FALSE, timeout = 10, env = base
    ),
    error = function(e) NULL
  )
  if (!is.null(res) && nzchar(res$stdout)) {
    .parse_env_text(res$stdout)
  } else {
    NULL
  }
}

.merge_env <- function(a, b) {
  if (is.null(a)) {
    return(b)
  }
  if (is.null(b)) {
    return(a)
  }
  keep <- !(names(a) %in% names(b))
  structure(c(unname(a[keep]), b), names = c(names(a)[keep], names(b)))
}

.parse_env_text <- function(x) {
  parts <- strsplit(x, "\n", fixed = TRUE)[[1]]
  parts <- parts[grepl("^[A-Za-z_][A-Za-z0-9_]*=", parts)]
  if (length(parts) == 0L) {
    return(NULL)
  }
  key <- sub("=.*$", "", parts)
  parts <- parts[!grepl("^PROCESSX_", key)]
  key <- sub("=.*$", "", parts)
  if (length(parts) == 0L) {
    return(NULL)
  }
  res <- sub("^[^=]*=", "", parts)
  ans <- unname(res)
  names(ans) <- key
  ans
}

#' Build a llama-server command
#'
#' @description
#' Builds the exact `llama-server` command for serving a model, as a
#' string you can paste into a terminal. To actually start the server
#' from R, use [server_start()] instead.
#'
#' @param model path to a GGUF file, or a one row data frame from
#'   [find_gguf_models()].
#' @param host host to bind. Defaults to `"127.0.0.1"`.
#' @param port port to serve on. Defaults to `8080`.
#' @param ctx_size context size (`-c`). `NULL` keeps the llama.cpp
#'   default.
#' @param n_gpu_layers layers to offload to the GPU (`-ngl`). `NULL`
#'   keeps the llama.cpp default.
#' @param threads CPU threads (`-t`). `NULL` keeps the default.
#' @param extra_args extra command line arguments, as a character
#'   vector.
#' @param bin path to `llama-server`, or `NULL` to use [llama_bin()].
#' @returns The command, as a single string.
#' @export
#' @examples
#' llama_server_cmd("model.gguf", n_gpu_layers = 99)
llama_server_cmd <- function(model, host = "127.0.0.1", port = 8080,
                             ctx_size = NULL, n_gpu_layers = NULL,
                             threads = NULL, extra_args = character(),
                             bin = NULL) {
  if (is.null(bin)) {
    bin <- llama_bin("llama-server")
    if (is.na(bin)) bin <- "llama-server"
  }
  args <- .server_args(
    model = model, host = host, port = port, ctx_size = ctx_size,
    n_gpu_layers = n_gpu_layers, threads = threads,
    extra_args = extra_args
  )
  paste(c(.shq(bin), .shq(args)), collapse = " ")
}

#' Start a llama-server
#'
#' @description
#' Starts `llama-server` as a background process serving one model,
#' and (by default) waits until the server answers its health check.
#' This is the piece you would otherwise kick off in a terminal just
#' to have a local endpoint for tools like ellmer's
#' `chat_llamacpp()`.
#'
#' Keep the returned object around: pass it to [server_stop()] when
#' you are done. The server is also killed when the object is garbage
#' collected or the R session ends, and if R dies without running any
#' cleanup (a hard crash, say), the kernel SIGTERMs it on Linux and
#' processx's companion supervisor process does the same elsewhere, so
#' no stray server outlives the session.
#'
#' @param model path to a GGUF file, or a one row data frame from
#'   [find_gguf_models()].
#' @param host host to bind. Defaults to `"127.0.0.1"`.
#' @param port port to serve on. Defaults to `8080`.
#' @param ctx_size context size (`-c`). `NULL` keeps the llama.cpp
#'   default.
#' @param n_gpu_layers layers to offload to the GPU (`-ngl`), `99` is
#'   the usual "everything" value. `NULL` keeps the default.
#' @param threads CPU threads (`-t`). `NULL` keeps the default.
#' @param extra_args extra command line arguments, as a character
#'   vector.
#' @param bin path to `llama-server`, or `NULL` to find it with
#'   [llama_bin()].
#' @param wait wait for the health check to pass before returning.
#' @param timeout seconds to wait for the server to come up when
#'   `wait = TRUE`. Large models can take a while to load.
#' @param env environment for the `llama-server` child process: either
#'   `NULL` (the default, use the environment of a plain login shell,
#'   which avoids R-only settings like a wrapper's `LD_LIBRARY_PATH` or
#'   `.Renviron`'s `CUDA_VISIBLE_DEVICES` poisoning the run), a named
#'   character vector replacing the child environment entirely, or a
#'   vector whose unnamed element is `"current"` plus named overrides.
#' @returns An object of class `ggruf_server` holding the process, the
#'   base URL, and the log file path.
#' @export
#' @examples
#' \dontrun{
#' models <- find_gguf_models()
#' srv <- server_start(models[1, ], n_gpu_layers = 99)
#' srv$url
#' server_stop(srv)
#' }
server_start <- function(model, host = "127.0.0.1", port = 8080,
                         ctx_size = NULL, n_gpu_layers = NULL,
                         threads = NULL, extra_args = character(),
                         bin = NULL, wait = TRUE, timeout = 60,
                         env = NULL) {
  if (is.null(bin)) {
    bin <- llama_bin("llama-server")
  }
  if (length(bin) == 1L && !is.na(bin)) {
    bin <- path.expand(bin)
  }
  if (length(bin) == 0L || is.na(bin)) {
    stop(
      "Could not find llama-server. Install llama.cpp ",
      "(for example `brew install llama.cpp`), build it, or pass ",
      "its path via `bin`.",
      call. = FALSE
    )
  }

  model_path <- .model_path(model)

  # A stray server from an earlier run (or started elsewhere) owns the
  # port: llama-server would fail to bind, then the health check below
  # would be answered by *that* server while our child is dead. Say so
  # up front instead.
  if (isTRUE(server_health(NULL, host, port))) {
    stop(
      "Port ", port, " already answers /health on ", host, ". Another ",
      "llama-server (or something else) is probably still running there, ",
      "e.g. one left over from an earlier session. Stop it with ",
      "`pkill -f llama-server`, or point `server_start()` at a free port ",
      "(or a stray you want to keep with `server = ", sprintf('"http://%s:%d"', host, port),
      " in `server_health()`/`server_models()`).",
      call. = FALSE
    )
  }

  args <- .server_args(
    model = model, host = host, port = port, ctx_size = ctx_size,
    n_gpu_layers = n_gpu_layers, threads = threads,
    extra_args = extra_args
  )

  if (is.null(env)) {
    env <- .spawn_env()
  }
  if (!is.null(env) && !is.character(env)) {
    stop("`env` must be NULL or a character vector of NAME=VALUE entries.", call. = FALSE)
  }

  log <- tempfile("ggruf-llama-server-", fileext = ".log")

  # crash-safe cleanup: on Linux the kernel SIGTERMs llama-server the
  # moment the R process dies (even a hard crash), so no stray server
  # can outlive the session and hold the port hostage. Elsewhere, and
  # as a fallback, processx's companion `supervisor` binary does the
  # same by watching the R process from outside.
  spawn_args <- list(
    command = bin, args = args, stdout = log, stderr = "2>&1",
    cleanup = TRUE, env = env
  )
  proc <- NULL
  if (Sys.info()[["sysname"]] == "Linux" && .processx_has_pdeathsig()) {
    proc <- tryCatch(
      do.call(processx::process$new, c(spawn_args, linux_pdeathsig = TRUE)),
      error = function(e) NULL
    )
  }
  if (is.null(proc)) {
    proc <- tryCatch(
      do.call(processx::process$new, c(spawn_args, supervise = TRUE)),
      error = function(e) NULL
    )
  }
  if (is.null(proc)) {
    proc <- do.call(processx::process$new, spawn_args)
  }

  srv <- structure(
    list(
      process = proc,
      pid = proc$get_pid(),
      model = model_path,
      host = host,
      port = port,
      url = sprintf("http://%s:%d", host, port),
      log = log,
      bin = bin
    ),
    class = "ggruf_server"
  )

  if (isTRUE(wait)) {
    deadline <- proc.time()[["elapsed"]] + timeout
    repeat {
      if (!proc$is_alive()) {
        stop(
          "llama-server exited before becoming healthy.\n",
          paste(.log_tail(log), collapse = "\n"),
          call. = FALSE
        )
      }
      if (isTRUE(server_health(srv))) {
        break
      }
      if (proc.time()[["elapsed"]] > deadline) {
        stop(
          "Timed out after ", timeout, "s waiting for llama-server at ",
          srv$url, ". Log tail:\n",
          paste(.log_tail(log), collapse = "\n"),
          call. = FALSE
        )
      }
      Sys.sleep(0.5)
    }
  }
  srv
}

#' Stop a llama-server started with server_start()
#'
#' @param server a `ggruf_server` object from [server_start()].
#' @returns `TRUE` invisibly when the process was killed or was
#'   already finished.
#' @export
server_stop <- function(server) {
  if (!inherits(server, "ggruf_server")) {
    stop("`server` must be a ggruf_server from server_start().", call. = FALSE)
  }
  if (server$process$is_alive()) {
    server$process$kill()
    server$process$wait(5000)
  }
  invisible(TRUE)
}

#' Is a ggruf_server still running?
#'
#' @param server a `ggruf_server` object from [server_start()].
#' @returns `TRUE` when the server process is alive.
#' @export
server_is_running <- function(server) {
  inherits(server, "ggruf_server") && isTRUE(server$process$is_alive())
}

#' @export
print.ggruf_server <- function(x, ...) {
  cat(
    "llama-server (", if (server_is_running(x)) "running" else "stopped",
    ")\n", sep = ""
  )
  cat("  url:   ", x$url, "\n", sep = "")
  cat("  model: ", x$model, "\n", sep = "")
  cat("  pid:   ", x$pid, "\n", sep = "")
  cat("  log:   ", x$log, "\n", sep = "")
  invisible(x)
}

#' Check the health of a llama-server
#'
#' @param server a `ggruf_server` from [server_start()], or `NULL`
#'   together with `host`/`port` to probe a server started elsewhere.
#' @param host host when `server` is `NULL`. Defaults to
#'   `"127.0.0.1"`.
#' @param port port when `server` is `NULL`. Defaults to `8080`.
#' @returns `TRUE` when the server answers `/health` with status ok,
#'   `FALSE` otherwise. Never errors.
#' @export
#' @examples
#' server_health()
server_health <- function(server = NULL, host = "127.0.0.1", port = 8080) {
  url <- paste0(.server_url(server, host, port), "/health")
  res <- .http_get(url)
  if (is.null(res) || res$status != 200L) {
    return(FALSE)
  }
  body <- tryCatch(
    jsonlite::fromJSON(rawToChar(res$content)),
    error = function(e) NULL
  )
  isTRUE(body$status == "ok")
}

#' Models loaded on a llama-server
#'
#' @param server a `ggruf_server` from [server_start()], or `NULL`
#'   together with `host`/`port` to query a server started elsewhere.
#' @param host host when `server` is `NULL`. Defaults to
#'   `"127.0.0.1"`.
#' @param port port when `server` is `NULL`. Defaults to `8080`.
#' @returns A data frame of loaded models (at least an `id` column),
#'   or an empty data frame when the server cannot be reached.
#' @export
#' @examples
#' \donttest{
#' server_models()
#' }
server_models <- function(server = NULL, host = "127.0.0.1", port = 8080) {
  url <- paste0(.server_url(server, host, port), "/v1/models")
  res <- .http_get(url)
  if (is.null(res) || res$status != 200L) {
    return(data.frame(id = character(0), stringsAsFactors = FALSE))
  }
  parsed <- tryCatch(
    jsonlite::fromJSON(rawToChar(res$content)),
    error = function(e) NULL
  )
  if (is.null(parsed) || is.null(parsed$data)) {
    return(data.frame(id = character(0), stringsAsFactors = FALSE))
  }
  data <- parsed$data
  data$meta <- NULL
  as.data.frame(data, stringsAsFactors = FALSE)
}

# internals ---------------------------------------------------------------

#' Only quote when the value actually needs it (spaces, specials)
#' @noRd
.shq <- function(x) {
  needs <- grepl("[^A-Za-z0-9_@%+=:,./-]", x)
  x[needs] <- shQuote(x[needs], type = "sh")
  x
}

#' @noRd
.model_path <- function(model, check_exists = TRUE) {
  if (is.data.frame(model)) {
    if (nrow(model) != 1L || !"path" %in% names(model)) {
      stop(
        "`model` data frame must have exactly one row and a `path` column.",
        call. = FALSE
      )
    }
    return(path.expand(model$path))
  }
  if (!is.character(model) || length(model) != 1L) {
    stop("`model` must be a single path or a one row find_gguf_models() result.",
         call. = FALSE)
  }
  if (check_exists && !file.exists(model)) {
    stop("Model file not found: ", model, call. = FALSE)
  }
  # child processes get the literal string: expand "~" for them
  path.expand(model)
}

#' @noRd
.server_args <- function(model, host, port, ctx_size, n_gpu_layers,
                         threads, extra_args) {
  args <- c("-m", .model_path(model, check_exists = FALSE), "--host", host, "--port", as.character(port))
  if (!is.null(ctx_size)) {
    args <- c(args, "-c", as.character(ctx_size))
  }
  if (!is.null(n_gpu_layers)) {
    args <- c(args, "-ngl", as.character(n_gpu_layers))
  }
  if (!is.null(threads)) {
    args <- c(args, "-t", as.character(threads))
  }
  c(args, extra_args)
}

#' @noRd
.server_url <- function(server, host, port) {
  if (!is.null(server)) {
    if (inherits(server, "ggruf_server")) {
      return(server$url)
    }
    if (is.character(server) && length(server) == 1L) {
      return(sub("/$", "", server))
    }
    stop("`server` must be a ggruf_server or a base URL.", call. = FALSE)
  }
  sprintf("http://%s:%d", host, port)
}

#' @noRd
.http_get <- function(url, timeout = 5) {
  tryCatch(
    curl::curl_fetch_memory(url, handle = curl::new_handle(timeout = timeout)),
    error = function(e) NULL
  )
}

#' @noRd
.log_tail <- function(log, n = 15L) {
  if (!file.exists(log)) {
    return("(no log)")
  }
  lines <- readLines(log, warn = FALSE)
  utils::tail(lines, n)
}
