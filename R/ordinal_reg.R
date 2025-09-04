#' Ordinal regression
#'
#' @description
#' `ordinal_reg()` defines a generalized linear model that predicts an ordinal
#' outcome. This function can fit classification models.
#'
#' \Sexpr[stage=render,results=rd]{parsnip:::make_engine_list("ordinal_reg")}
#'
#' More information on how \pkg{parsnip} is used for modeling is at
#' \url{https://www.tidymodels.org/}.
#'
#' @param mode A single character string for the prediction outcome mode. The
#'   only possible value for this model is "classification".
#' @param engine A single character string specifying what computational engine
#'  to use for fitting. Possible engines are listed below. The default for this
#'  model is `"polr"`.
#' @param ordinal_link The ordinal link function.
#' @param odds_link The odds or probability link function.
#' @param penalty A non-negative number representing the total
#'  amount of regularization (specific engines only).
#' @param mixture A number between zero and one (inclusive) denoting the
#'  proportion of L1 regularization (i.e. lasso) in the model.
#'
#'  * `mixture = 1` specifies a pure lasso model,
#'  * `mixture = 0`  specifies a ridge regression model, and
#'  * `0 < mixture < 1` specifies an elastic net model, interpolating lasso and ridge.
#'
#'  Available for specific engines only.
#'
#' @templateVar modeltype ordinal_reg
#'
#' @template spec-details
#'
#' @details Ordinal regression models include cumulative, sequential, and
#' adjacent structures.
#'
#' @template spec-references
#'
#' @seealso \Sexpr[stage=render,results=rd]{parsnip:::make_seealso_list("ordinal_reg")}
#'
#' @examplesIf !parsnip:::is_cran_check()
#' show_engines("ordinal_reg")
#'
#' ordinal_reg(mode = "classification")
#'
#' @keywords internal
#' @export
ordinal_reg <-
  function(
    mode = "classification",
    ordinal_link = NULL, odds_link = NULL,
    engine = "polr"
  ) {

    if (mode != "classification") {
      rlang::abort("`mode` should be 'classification'")
    }

    args <- list(
      ordinal_link = enquo(ordinal_link),
      odds_link = enquo(odds_link),
      penalty = enquo(penalty),
      mixture = enquo(mixture)
    )

    parsnip::new_model_spec(
      "ordinal_reg",
      args = args,
      eng_args = NULL,
      mode = mode,
      user_specified_mode = ! missing(mode),
      method = NULL,
      engine = engine,
      user_specified_engine = ! missing(engine)
    )
  }

# ------------------------------------------------------------------------------

#' @method update ordinal_reg
#' @rdname parsnip_update
#' @export
update.ordinal_reg <-
  function(
    object,
    parameters = NULL,
    ordinal_link = NULL, odds_link = NULL,
    fresh = FALSE, ...
  ) {

    args <- list(
      ordinal_link = enquo(ordinal_link),
      odds_link = enquo(odds_link),
      penalty = enquo(penalty),
      mixture = enquo(mixture)
    )

    update_spec(
      object = object,
      parameters = parameters,
      args_enquo_list = args,
      fresh = fresh,
      cls = "ordinal_reg",
      ...
    )
  }

# ------------------------------------------------------------------------------

# TODO:
# * `check_args()` if necessary
# * `translate()` if warranted
