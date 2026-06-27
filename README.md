
<!-- README.md is generated from README.Rmd. Please edit that file -->

# timeloops

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/giovannitinervia9/timeloops/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/giovannitinervia9/timeloops/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/giovannitinervia9/timeloops/graph/badge.svg)](https://app.codecov.io/gh/giovannitinervia9/timeloops)
<!-- badges: end -->

Self-reporting `for`, `while` and `repeat` loops for R. They behave like
the loops you already know, but print how far they have got, how long
they have been running, and — for `for` loops — an estimated time to
completion (ETA). `for` loops can also be run in parallel, on every
platform (Windows included), with a live progress bar and **no cluster
setup required**.

Handy for long-running loops where you want to know whether to wait or
go make a coffee.

## Installation

``` r
# install.packages("remotes")
remotes::install_github("giovannitinervia9/timeloops")
```

## Usage

You set up a loop with `for_t()` / `while_t()` / `repeat_t()` and run it
with the `%do%` (sequential) or `%dopar%` (parallel) operator.

### Sequential — `%do%`

Behaves exactly like the base loop: same scope, same `next`/`break`, and
your assignments are visible afterwards.

``` r
library(timeloops)

for_t(i = 1:100) %do% {
  Sys.sleep(0.02)
}
#> [1/100] | last 1: 20.00 ms | total: 20.00 ms | ETA: 1.98 s
#> [2/100] | last 1: 20.00 ms | total: 40.00 ms | ETA: 1.96 s
#> ...

# report only every 10th iteration (the last one is always shown)
for_t(i = 1:100, each = 10) %do% {
  Sys.sleep(0.02)
}

# while / repeat (no ETA: the number of iterations is unknown)
x <- 0
while_t(x < 50, each = 10) %do% {
  x <- x + 1
}

repeat_t(each = 10) %do% {
  x <- x + 1
  if (x >= 100) break
}
```

### Parallel — `%dopar%`

Runs a `for_t()` loop across `cores` workers. Just set `cores`; the
cluster is created and removed for you and works on Windows too.

``` r
results <- for_t(i = 1:1000, cores = 4) %dopar% {
  slow_function(i)
}
```

Because each iteration runs in a separate process, parallel loops are
**functional**: the body must *return* a value, and `%dopar%` collects
these and returns them as a list. Side effects on the calling
environment are not visible (this is a hard limit of multiprocessing in
R, not a quirk of the package). A progress bar with elapsed time is
shown while it runs.

|  | `%do%` (sequential) | `%dopar%` (parallel) |
|----|----|----|
| Scope | mutates the caller (like base `for`) | isolated workers, no side effects |
| Result | `NULL` (invisible) | list of returned values |
| Progress | live messages + ETA | progress bar + elapsed time |
| Loops | `for` / `while` / `repeat` | `for` only |

### Customising the parallel progress bar

The parallel progress bar comes from the
[`progressr`](https://progressr.futureverse.org/) package, so you can
style it however you like. Set a handler once, before the loop, and
every `%dopar%` call will use it:

``` r
# pick a different style
progressr::handlers("progress")   # or "cli", "txtprogressbar", "rstudio", ...

# or customise the format (tokens like :percent, :elapsed, :eta, :bar)
progressr::handlers(progressr::handler_progress(
  format = "Working :percent | elapsed :elapsed | eta :eta"
))

for_t(i = 1:1000, cores = 4) %dopar% {
  slow_function(i)
}
```

See `?progressr::handlers` for the full list. If you set nothing, a
plain text progress bar is used.

## Arguments

- `var = sequence` — the loop variable and the values to iterate over
  (`for_t`).
- `cond` — the condition (`while_t`).
- `each` — print an update every `each` iterations (default `1`; `%do%`
  only).
- `cores` — number of parallel workers (default `1`; used by `%dopar%`).

Progress is written to the message stream, so you can silence it with
`suppressMessages()`.
