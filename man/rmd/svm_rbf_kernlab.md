


For this engine, there are multiple modes: classification and regression

## Tuning Parameters



This model has 3 tuning parameters:

- `cost`: Cost (type: double, default: 1.0)

- `rbf_sigma`: Radial Basis Function sigma (type: double, default: see below)

- `margin`: Insensitivity Margin (type: double, default: 0.1)

There is no default for the radial basis function kernel parameter. kernlab estimates it from the data using a heuristic method. See [kernlab::sigest()]. This method uses random numbers so, without setting the seed before fitting, the model will not be reproducible. 

## Translation from parsnip to the original package (regression)


```r
svm_rbf(
  cost = double(1),
  rbf_sigma = double(1), 
  margin = double(1)
) %>%  
  set_engine("kernlab") %>% 
  set_mode("regression") %>% 
  translate()
```

```
## Radial Basis Function Support Vector Machine Specification (regression)
## 
## Main Arguments:
##   cost = double(1)
##   rbf_sigma = double(1)
##   margin = double(1)
## 
## Computational engine: kernlab 
## 
## Model fit template:
## kernlab::ksvm(x = missing_arg(), data = missing_arg(), C = double(1), 
##     epsilon = double(1), kernel = "rbfdot", kpar = list(sigma = ~double(1)))
```

## Translation from parsnip to the original package (classification)


```r
svm_rbf(
  cost = double(1),
  rbf_sigma = double(1)
) %>% 
  set_engine("kernlab") %>% 
  set_mode("classification") %>% 
  translate()
```

```
## Radial Basis Function Support Vector Machine Specification (classification)
## 
## Main Arguments:
##   cost = double(1)
##   rbf_sigma = double(1)
## 
## Computational engine: kernlab 
## 
## Model fit template:
## kernlab::ksvm(x = missing_arg(), data = missing_arg(), C = double(1), 
##     kernel = "rbfdot", prob.model = TRUE, kpar = list(sigma = ~double(1)))
```

The `margin` parameter does not apply to classification models. 

Note that the `"kernlab"` engine does not naturally estimate class probabilities. To produce them, the decision values of the model are converted to probabilities using Platt scaling. This method fits an additional model on top of the SVM model. When fitting the Platt scaling model, random numbers are used that are not reproducible or controlled by R's random number stream.   

## Preprocessing requirements


Factor/categorical predictors need to be converted to numeric values (e.g., dummy or indicator variables) for this engine. When using the formula method via \\code{\\link[=fit.model_spec]{fit()}}, parsnip will convert factor columns to indicators.


Predictors should have the same scale. One way to achieve this is to center and 
scale each so that each predictor has mean zero and a variance of one.

## Case weights


The underlying model implementation does not allow for case weights. 

## Examples 

The "Fitting and Predicting with parsnip" article contains [examples](https://parsnip.tidymodels.org/articles/articles/Examples.html#svm-rbf-kernlab) for `svm_rbf()` with the `"kernlab"` engine.

## References

 - Lin, HT, and R Weng. ["A Note on Platt’s Probabilistic Outputs for Support Vector Machines"](https://www.csie.ntu.edu.tw/~cjlin/papers/plattprob.pdf)
 
 - Karatzoglou, A,  Smola, A,  Hornik, K, and A Zeileis. 2004. ["kernlab - An S4 Package for Kernel Methods in R."](https://www.jstatsoft.org/article/view/v011i09), _Journal of Statistical Software_. 
 
 - Kuhn, M, and K Johnson. 2013. _Applied Predictive Modeling_. Springer.
