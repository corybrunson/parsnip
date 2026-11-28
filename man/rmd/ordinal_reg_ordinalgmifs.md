

For this engine, there is a single mode: classification

## Tuning Parameters



This model has 2 tuning parameters:

- `ordinal_link`: Ordinal Link (type: character, default: logistic)

- `odds_link`: Odds Link (type: character, default: cumulative)

## Translation from parsnip to the original package

The **ordered** extension package is required to fit this model.


``` r
library(ordered)

ordinal_reg(
  ordinal_link = character(0),
  odds_link = character(0)
) |>
  set_engine("ordinalgmifs", x = c("Cont")) |>
  # "classification" is the only mode
  translate()
```

```
## Ordinal Regression Model Specification (classification)
## 
## Main Arguments:
##   ordinal_link = character(0)
##   odds_link = character(0)
## 
## Engine-Specific Arguments:
##   x = c("Cont")
## 
## Computational engine: ordinalgmifs 
## 
## Model fit template:
## ordered::ordinalgmifs_wrapper(formula = missing_arg(), data = missing_arg(), 
##     weights = missing_arg(), link = character(0), probability.model = character(0), 
##     x = c("Cont"))
```

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.


The `x` engine argument names penalized predictor columns after preprocessing.
For factor predictors, use the names of the generated indicator columns.

`ordinalgmifs::ordinalgmifs()` centers and scales penalized predictors by
default; set the native `scale` argument through `set_engine()` to change this.

## Case weights


The underlying model implementation does not allow for case weights. 

## Other notes

Some combinations of `probability.model` and `link` are rejected, while others
warn and continue using the link supported by `ordinalgmifs`. The engine
translates the standardized `odds_link` and `ordinal_link` arguments but does
not check model and link compatibility in advance.

## Prediction types


``` r
parsnip:::get_from_env("ordinal_reg_predict") |>
  dplyr::filter(engine == "ordinalgmifs") |>
  dplyr::select(mode, type)
```

```
## # A tibble: 2 x 2
##   mode           type 
##   <chr>          <chr>
## 1 classification class
## 2 classification prob
```

## References

- Archer KJ, Hou J, Zhou Q, Ferber K, Layne G. 2014. Ordinal Regression for High-Dimensional Data: The Generalized Monotone Incremental Forward Stagewise Method. _Journal of Statistical Software_ 60(13):1-31. \doi{10.18637/jss.v060.i13}.
