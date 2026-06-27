# Format a number of seconds into a short, human-readable string.
#
# Picks the unit (ms, s, min, h, days) based on the magnitude so the
# progress messages stay compact regardless of how long a loop runs.
#
# @param secs Numeric. Elapsed time in seconds.
# @return A length-one character string.
# @keywords internal
# @noRd
.tl_format_time <- function(secs) {
  if (secs < 1) {
    return(sprintf("%.2f ms", secs * 1000))
  }
  if (secs < 60) {
    return(sprintf("%.2f s", secs))
  }
  if (secs < 3600) {
    return(sprintf("%.2f min", secs / 60))
  }
  if (secs < 86400) {
    return(sprintf("%.2f h", secs / 3600))
  }
  days <- floor(secs / 86400)
  hours <- (secs %% 86400) / 3600
  sprintf("%d days %.1f h", as.integer(days), hours)
}


# Validate and coerce the `each` argument shared by all loop wrappers.
#
# @param each The value supplied by the user.
# @return A length-one positive integer.
# @keywords internal
# @noRd
.tl_check_each <- function(each) {
  each <- suppressWarnings(as.integer(each))
  if (length(each) != 1L || is.na(each) || each < 1L) {
    stop("`each` must be a single positive integer.", call. = FALSE)
  }
  each
}


# Validate and coerce the `cores` argument.
#
# @param cores The value supplied by the user.
# @return A length-one positive integer.
# @keywords internal
# @noRd
.tl_check_cores <- function(cores) {
  cores <- suppressWarnings(as.integer(cores))
  if (length(cores) != 1L || is.na(cores) || cores < 1L) {
    stop("`cores` must be a single positive integer.", call. = FALSE)
  }
  cores
}


# Build a progress tracker as a pair of closures sharing private state.
#
# The tracker measures wall-clock time with proc.time(). It is driven by
# the iteration counter only, so it does not care *where* in the loop body
# an iteration ends (e.g. via `next` or `break`): timing is always the
# elapsed time since the previous printed line.
#
# @param total Integer total number of iterations, or NA when unknown
#   (`while`/`repeat`). Enables the ETA when known.
# @param each Integer. Print a line every `each` iterations.
# @return A list with `update(i)` (print on a checkpoint) and `flush(i)`
#   (print whatever happened since the last line, used at the very end).
# @keywords internal
# @noRd
.tl_make_tracker <- function(total, each) {
  fmt <- .tl_format_time
  start_total <- proc.time()
  start_batch <- start_total
  last_printed <- 0L

  print_line <- function(i) {
    now <- proc.time()
    batch_secs <- as.numeric((now - start_batch)["elapsed"])
    total_secs <- as.numeric((now - start_total)["elapsed"])
    n_batch <- i - last_printed

    if (is.na(total)) {
      msg <- sprintf(
        "[iter %d] | last %d: %s | total: %s",
        i, n_batch, fmt(batch_secs), fmt(total_secs)
      )
    } else {
      msg <- sprintf(
        "[%d/%d] | last %d: %s | total: %s",
        i, total, n_batch, fmt(batch_secs), fmt(total_secs)
      )
      if (i < total && i > 0L) {
        eta_secs <- total_secs / i * (total - i)
        msg <- paste0(msg, " | ETA: ", fmt(eta_secs))
      }
    }

    message(msg)
    start_batch <<- now
    last_printed <<- i
  }

  list(
    update = function(i) {
      if (i %% each == 0L) print_line(i)
    },
    flush = function(i) {
      if (i > last_printed) print_line(i)
    }
  )
}


# Run a loop specification sequentially, mutating the caller's environment.
#
# Reproduces a plain `for`/`while`/`repeat`: the body is evaluated in the
# calling scope, `next`/`break` work, and assignments leak out as usual.
#
# @param loop A `tl_loop` specification.
# @param body The captured loop body (a language object).
# @return `NULL`, invisibly.
# @keywords internal
# @noRd
.tl_run_seq <- function(loop, body) {
  inner_body <- bquote({
    .tl_i <- .tl_i + 1L
    .(body)
    .tl_track$update(.tl_i)
  })

  if (loop$type == "for") {
    inner <- call("for", as.name(loop$var), quote(.tl_seq), inner_body)
    total <- length(loop$seq)
    seq_assign <- bquote(.tl_seq <- .(loop$seq))
    cleanup <- bquote(rm(.tl_seq, .tl_i, .tl_track))
  } else if (loop$type == "while") {
    inner <- call("while", loop$cond, inner_body)
    total <- NA_integer_
    seq_assign <- NULL
    cleanup <- bquote(rm(.tl_i, .tl_track))
  } else {
    inner <- call("repeat", inner_body)
    total <- NA_integer_
    seq_assign <- NULL
    cleanup <- bquote(rm(.tl_i, .tl_track))
  }

  code <- bquote({
    .(seq_assign)
    .tl_i <- 0L
    .tl_track <- .(.tl_make_tracker)(total = .(total), each = .(loop$each))
    tryCatch(
      .(inner),
      finally = {
        .tl_track$flush(.tl_i)
        .(cleanup)
      }
    )
  })

  eval(code, envir = loop$envir)
  invisible(NULL)
}


# Run a `for` specification in parallel and return the collected results.
#
# Each iteration runs in a separate process (via the future framework), so
# this is functional: the body's return value is collected and side effects
# on the calling environment are not visible. A progress bar with elapsed
# time is shown through 'progressr'.
#
# @param loop A `tl_loop` specification of type "for".
# @param body The captured loop body (a language object).
# @return A list of results, one per element of the sequence.
# @keywords internal
# @noRd
.tl_run_par <- function(loop, body) {
  if (loop$type != "for") {
    stop("Parallel execution (%dopar%) is only available for for_t() loops.",
         call. = FALSE)
  }

  seq_val <- loop$seq
  n <- length(seq_val)

  # Turn the body into a genuine closure `function(<var>) <body>` enclosed in
  # the caller's environment. This lets the future framework detect the
  # variables and packages the body needs and ship them to the workers, while
  # lexical scope resolves anything else from the caller.
  fmls <- as.pairlist(stats::setNames(alist(x = ), loop$var))
  loop_fun <- eval(call("function", fmls, body), envir = loop$envir)

  oplan <- future::plan()
  on.exit(future::plan(oplan), add = TRUE)
  future::plan(future::multisession, workers = loop$cores)

  progressr::with_progress({
    p <- progressr::progressor(steps = n)
    wrapped <- function(.x) {
      .res <- loop_fun(.x)
      p()
      .res
    }
    future.apply::future_lapply(
      seq_val, wrapped,
      future.seed = TRUE,
      future.chunk.size = 1L
    )
  })
}


#' Set up a `for` loop with progress reporting
#'
#' @description
#' Describes a `for` loop to be run with [`%do%`][grapes-do] (sequential) or
#' [`%dopar%`][grapes-do] (parallel). Use the natural `var = sequence` form:
#'
#' ```r
#' for_t(i = 1:n, each = 10) %do% { ... }
#' for_t(i = 1:n, cores = 4) %dopar% { ... }
#' ```
#'
#' With `%do%` it behaves exactly like a base `for` loop (same scope, same
#' `next`/`break`) while printing progress and an estimated time to completion
#' (ETA). With `%dopar%` the iterations run in parallel and the loop returns a
#' list of results (see [`%do%`][grapes-do] for the difference in behaviour).
#'
#' @param ... A single named argument of the form `var = sequence`, giving the
#'   loop variable and the values to iterate over.
#' @param each Print an update every `each` iterations. Defaults to `1`. Used
#'   only by `%do%`.
#' @param cores Number of parallel workers to use with `%dopar%`. Defaults to
#'   `1`. The cluster is created and torn down automatically and works on all
#'   platforms, including Windows.
#'
#' @return A loop specification (class `tl_loop`) to be piped into [`%do%`][grapes-do] or
#'   [`%dopar%`][grapes-do].
#' @seealso [`%do%`][grapes-do], [`%dopar%`][grapes-do], [while_t()], [repeat_t()]
#' @export
#'
#' @examples
#' # sequential: behaves like a normal for loop, with live progress + ETA
#' for_t(i = 1:5) %do% {
#'   Sys.sleep(0.01)
#' }
#'
#' # build up a result in the calling scope, exactly like base `for`
#' total <- 0
#' for_t(i = 1:10, each = 5) %do% {
#'   total <- total + i
#' }
#' total
#'
#' # parallel: returns a list of results (no side effects), with a progress bar
#' \donttest{
#' squares <- for_t(i = 1:8, cores = 2) %dopar% {
#'   i^2
#' }
#' }
for_t <- function(..., each = 1L, cores = 1L) {
  dots <- match.call(expand.dots = FALSE)[["..."]]
  nms <- names(dots)
  if (length(dots) != 1L || is.null(nms) || !nzchar(nms[1L])) {
    stop("Use the form: for_t(var = sequence) %do% { ... }", call. = FALSE)
  }

  structure(
    list(
      type = "for",
      var = nms[1L],
      seq = eval(dots[[1L]], envir = parent.frame()),
      each = .tl_check_each(each),
      cores = .tl_check_cores(cores),
      envir = parent.frame()
    ),
    class = "tl_loop"
  )
}


#' Set up a `while` loop with progress reporting
#'
#' @description
#' Describes a `while` loop to be run with [`%do%`][grapes-do]:
#'
#' ```r
#' while_t(x < 100, each = 10) %do% { ... }
#' ```
#'
#' It behaves like a base `while` loop while printing the iteration count and
#' elapsed time. No ETA is shown because the number of iterations is unknown in
#' advance. Parallel execution is not available for `while` loops.
#'
#' @param cond The condition checked before each iteration.
#' @param each Print an update every `each` iterations. Defaults to `1`.
#'
#' @return A loop specification (class `tl_loop`) to be piped into [`%do%`][grapes-do].
#' @seealso [`%do%`][grapes-do], [for_t()], [repeat_t()]
#' @export
#'
#' @examples
#' x <- 0
#' while_t(x < 30, each = 10) %do% {
#'   x <- x + 1
#' }
while_t <- function(cond, each = 1L) {
  structure(
    list(
      type = "while",
      cond = substitute(cond),
      each = .tl_check_each(each),
      envir = parent.frame()
    ),
    class = "tl_loop"
  )
}


#' Set up a `repeat` loop with progress reporting
#'
#' @description
#' Describes a `repeat` loop to be run with [`%do%`][grapes-do]:
#'
#' ```r
#' repeat_t(each = 10) %do% { ...; if (done) break }
#' ```
#'
#' It behaves like a base `repeat` loop while printing the iteration count and
#' elapsed time. As with `repeat`, the body must contain a `break` to stop.
#' Parallel execution is not available for `repeat` loops.
#'
#' @param each Print an update every `each` iterations. Defaults to `1`.
#'
#' @return A loop specification (class `tl_loop`) to be piped into [`%do%`][grapes-do].
#' @seealso [`%do%`][grapes-do], [for_t()], [while_t()]
#' @export
#'
#' @examples
#' x <- 0
#' repeat_t(each = 10) %do% {
#'   x <- x + 1
#'   if (x >= 30) break
#' }
repeat_t <- function(each = 1L) {
  structure(
    list(
      type = "repeat",
      each = .tl_check_each(each),
      envir = parent.frame()
    ),
    class = "tl_loop"
  )
}


#' Run a timed loop
#'
#' @description
#' `%do%` runs the loop set up by [for_t()], [while_t()] or [repeat_t()]
#' **sequentially**, exactly like the corresponding base loop: the body is
#' evaluated in the calling scope (so assignments and `<-` updates are visible
#' afterwards), `next` and `break` work as usual, and progress is printed while
#' it runs. It returns `NULL` invisibly.
#'
#' `%dopar%` runs a [for_t()] loop **in parallel** across `cores` workers. Each
#' iteration runs in its own process, so the body must be self-contained and
#' return a value: `%dopar%` collects these and returns them as a list, and
#' changes to the calling environment are *not* visible. A progress bar with
#' elapsed time is shown. Parallel execution is only available for `for` loops.
#'
#' @section Customising the parallel progress bar:
#' The parallel progress bar is produced with the \pkg{progressr} package, so its
#' appearance is fully under your control: pick a handler (and its options) once,
#' before running the loop, and every `%dopar%` call will use it. For example:
#'
#' ```r
#' # choose a different style
#' progressr::handlers("progress")   # or "cli", "txtprogressbar", "rstudio", ...
#'
#' # customise the format (tokens like :percent, :elapsed, :eta, :bar)
#' progressr::handlers(progressr::handler_progress(
#'   format = "Working :percent | elapsed :elapsed | eta :eta"
#' ))
#'
#' for_t(i = 1:1000, cores = 4) %dopar% slow_function(i)
#' ```
#'
#' See [progressr::handlers()] for the full list of handlers and options. If you
#' do not set anything, a plain text progress bar is used.
#'
#' @param loop A loop specification from [for_t()], [while_t()] or [repeat_t()].
#' @param expr The loop body.
#'
#' @return `%do%` returns `NULL` invisibly (called for its side effects).
#'   `%dopar%` returns a list with one element per iteration.
#' @seealso [for_t()], [while_t()], [repeat_t()], [progressr::handlers()]
#' @name grapes-do
#' @examples
#' for_t(i = 1:3) %do% {
#'   Sys.sleep(0.01)
#' }
#'
#' \donttest{
#' for_t(i = 1:6, cores = 2) %dopar% {
#'   i * 10
#' }
#'
#' # customise the progress bar before running
#' progressr::handlers("txtprogressbar")
#' for_t(i = 1:6, cores = 2) %dopar% {
#'   Sys.sleep(0.05)
#'   i * 10
#' }
#' }
NULL

#' @rdname grapes-do
#' @export
`%do%` <- function(loop, expr) {
  if (!inherits(loop, "tl_loop")) {
    stop("The left-hand side of %do% must be a for_t()/while_t()/repeat_t() call.",
         call. = FALSE)
  }
  .tl_run_seq(loop, substitute(expr))
}

#' @rdname grapes-do
#' @export
`%dopar%` <- function(loop, expr) {
  if (!inherits(loop, "tl_loop")) {
    stop("The left-hand side of %dopar% must be a for_t() call.", call. = FALSE)
  }
  .tl_run_par(loop, substitute(expr))
}


#' @export
print.tl_loop <- function(x, ...) {
  cat(sprintf("<timeloops %s-loop spec> - pipe it into %%do%% or %%dopar%%\n",
              x$type))
  invisible(x)
}
