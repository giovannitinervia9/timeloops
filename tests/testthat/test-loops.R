# Helper: run code with progress messages silenced.
quiet <- function(x) suppressMessages(x)

test_that("for_t() returns a loop spec, not a result", {
  spec <- for_t(i = 1:5)
  expect_s3_class(spec, "tl_loop")
  expect_equal(spec$type, "for")
  expect_equal(spec$var, "i")
  expect_equal(spec$seq, 1:5)
})

test_that("%do% runs the body once per element and mutates the caller scope", {
  acc <- integer()
  quiet(for_t(i = 1:5) %do% {
    acc <- c(acc, i)
  })
  expect_equal(acc, 1:5)
})

test_that("%do% leaves no internal variables behind", {
  quiet(for_t(i = 1:3) %do% {})
  expect_false(any(c(".tl_i", ".tl_seq", ".tl_track") %in% ls(all.names = TRUE)))
})

test_that("namespaced calls in the body do not break the loop (regression)", {
  out <- numeric(3)
  expect_no_error(quiet(for_t(i = 1:3) %do% {
    out[i] <- stats::rnorm(1)
  }))
  expect_false(anyNA(out))
})

test_that("$-method calls in the body do not break the loop", {
  e <- new.env()
  e$f <- function() 1
  expect_no_error(quiet(for_t(i = 1:2) %do% {
    e$f()
  }))
})

test_that("break stops the loop and is still timed", {
  n <- 0L
  expect_message(
    for_t(i = 1:100) %do% {
      n <- n + 1L
      if (i == 4L) break
    },
    "\\[4/100\\]"
  )
  expect_equal(n, 4L)
})

test_that("next skips the rest of the body without losing the count", {
  hits <- integer()
  quiet(for_t(i = 1:6) %do% {
    if (i %% 2L == 0L) next
    hits <- c(hits, i)
  })
  expect_equal(hits, c(1L, 3L, 5L))
})

test_that("%do% prints an ETA before the end but not on the last line", {
  msgs <- capture_messages(for_t(i = 1:3) %do% {})
  expect_true(any(grepl("ETA", msgs)))
  expect_false(grepl("ETA", msgs[length(msgs)]))
})

test_that("each controls how often a line is printed", {
  msgs <- capture_messages(for_t(i = 1:10, each = 5) %do% {})
  expect_equal(length(msgs), 2L)
})

test_that("while_t() captures its condition and runs until it is false", {
  x <- 0L
  expect_no_error(quiet(while_t(x < 4) %do% {
    x <- x + 1L
  }))
  expect_equal(x, 4L)
})

test_that("while_t prints no ETA", {
  x <- 0L
  msgs <- capture_messages(while_t(x < 3) %do% {
    x <- x + 1L
  })
  expect_false(any(grepl("ETA", msgs)))
})

test_that("repeat_t loops until break", {
  x <- 0L
  quiet(repeat_t() %do% {
    x <- x + 1L
    if (x >= 5L) break
  })
  expect_equal(x, 5L)
})

test_that("empty loops print nothing", {
  expect_silent(for_t(i = integer(0)) %do% {})
  expect_silent(while_t(FALSE) %do% {})
})

test_that("each and cores are validated", {
  expect_error(for_t(i = 1:3, each = 0), "positive integer")
  expect_error(for_t(i = 1:3, each = -1), "positive integer")
  expect_error(for_t(i = 1:3, each = c(1, 2)), "positive integer")
  expect_error(for_t(i = 1:3, cores = 0), "positive integer")
})

test_that("for_t() rejects a malformed call", {
  expect_error(for_t(1:3), "var = sequence")
})

test_that("operators reject a non-spec left-hand side", {
  expect_error(1:3 %do% {}, "for_t")
  expect_error(1:3 %dopar% {}, "for_t")
})

test_that("%dopar% is rejected for while/repeat loops", {
  expect_error(while_t(TRUE) %dopar% {}, "only available for for_t")
})

test_that(".tl_format_time picks sensible units", {
  expect_match(timeloops:::.tl_format_time(0.5), "ms$")
  expect_match(timeloops:::.tl_format_time(5), "s$")
  expect_match(timeloops:::.tl_format_time(120), "min$")
  expect_match(timeloops:::.tl_format_time(7200), "h$")
})

test_that("%dopar% returns one result per element, in order", {
  skip_on_cran()
  res <- suppressMessages(
    for_t(i = 1:6, cores = 2) %dopar% {
      i^2
    }
  )
  expect_type(res, "list")
  expect_equal(unlist(res), (1:6)^2)
})

test_that("%dopar% body can use variables from the calling scope", {
  skip_on_cran()
  k <- 10
  res <- suppressMessages(
    for_t(i = 1:4, cores = 2) %dopar% {
      i + k
    }
  )
  expect_equal(unlist(res), (1:4) + 10)
})
