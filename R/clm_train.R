# Split a model formula into the location and `nominal` formulas that
# `ordinal::clm()` expects when the parallel regression assumption is relaxed.
# Every predictor moves to `nominal`, leaving an intercept-only location
# formula, so that `clm()` has no aliased coefficients to drop. An offset has no
# coefficient to vary across thresholds, so it stays in the location formula.
clm_formulas <- function(formula, data, call = rlang::caller_env()) {
  term_info <- terms(formula, data = data)
  term_labels <- attr(term_info, "term.labels")
  offset <- attr(term_info, "offset")

  if (length(term_labels) == 0L) {
    cli::cli_abort(
      "{.code parallel_reg = FALSE} needs at least one predictor to make
       non-parallel, but {.arg formula} has none.",
      call = call
    )
  }

  loc <- formula
  loc[[3]] <- if (length(offset)) {
    as.list(attr(term_info, "variables"))[[offset + 1L]]
  } else {
    1
  }

  nominal <- reformulate(term_labels, env = rlang::f_env(formula))

  list(formula = loc, nominal = nominal)
}

# Fit `ordinal::clm()`, translating `parallel_reg` into the `nominal` formula.
# This is done here rather than in `translate()` because the formula can only be
# split once `data` is known, and because `make_form_call()` overwrites any
# location formula that `translate()` sets.
clm_train <- function(
  formula,
  data,
  weights = NULL,
  parallel_reg = NULL,
  ...,
  call = rlang::caller_env()
) {
  # capture before forcing the promises, so that the rebuilt call below records
  # engine arguments as the user wrote them rather than as their values
  dot_exprs <- rlang::enexprs(...)
  dots <- list(...)

  formulas <- if (isFALSE(parallel_reg)) {
    clm_formulas(formula, data, call = call)
  } else {
    list(formula = formula)
  }

  args <- c(formulas, list(data = data), dots)
  if (!is.null(weights)) {
    args$weights <- weights
  }

  res <- do.call(ordinal::clm, args)

  # `do.call()` inlines the data into the recorded call. Rebuild it so that
  # printing the fit shows the formulas and engine arguments that were used.
  res$call <- rlang::call2(
    "clm",
    !!!formulas,
    data = rlang::sym("data"),
    !!!dot_exprs,
    .ns = "ordinal"
  )

  res
}
