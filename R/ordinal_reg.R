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
  function(mode = "classification", engine = "polr") {

    if (mode != "classification") {
      rlang::abort("`mode` should be 'classification'")
    }

    args <- list(
      ordinal_link = enquo(ordinal_link)
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
  function(object,
           parameters = NULL,
           fresh = FALSE, ...) {

    args <- list()

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
