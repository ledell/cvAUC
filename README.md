# cvAUC

The `cvAUC` R package provides a computationally efficient means of estimating confidence intervals (or variance) of cross-validated Area Under the ROC Curve (AUC) estimates.  

In binary classification problems, the [AUC](https://en.wikipedia.org/wiki/Receiver_operating_characteristic#Area_under_the_curve) is commonly used to evaluate the performance of a prediction model. Often, it is combined with [cross-validation](http://en.wikipedia.org/wiki/Cross-validation_%28statistics%29) in order to assess how the results will generalize to an independent data set. In order to evaluate the quality of an estimate for cross-validated AUC, we obtain an estimate of its variance. 

For massive data sets, the process of generating a single performance estimate can be computationally expensive. Additionally, when using a complex prediction method, the process of cross-validating a predictive model on even a relatively small data set can still require a large amount of computation time. Thus, in many practical settings, the [bootstrap](https://en.wikipedia.org/wiki/Bootstrapping_%28statistics%29) is a computationally intractable approach to variance estimation.  As an alternative to the bootstrap, a computationally efficient [influence curve](http://www.jstor.org/stable/2285666) based approach to obtaining a variance estimate for cross-validated AUC can be used.  

The primary functions of the package are `ci.cvAUC` and `ci.pooled.cvAUC`, which report cross-validated AUC and compute confidence intervals for cross-validated AUC estimates based on influence curves for [i.i.d.](https://en.wikipedia.org/wiki/Independent_and_identically_distributed_random_variables) and [pooled repeated measures data](http://en.wikipedia.org/wiki/Pooled_variance), respectively.  One benefit to using influence curve based confidence intervals is that they require much less computation time than bootstrapping methods.  The utility functions, `AUC` and `cvAUC`, are simple wrappers for functions from the [ROCR](http://cran.r-project.org/web/packages/ROCR/index.html) package. 

Erin LeDell, Maya L. Petersen & Mark J. van der Laan, "Computationally Efficient Confidence Intervals for Cross-validated Area Under the ROC Curve Estimates."  (*In Review*)
- Preprint: [https://biostats.bepress.com/ucbbiostat/paper304](https://biostats.bepress.com/ucbbiostat/paper304)


## Install cvAUC

You can install:

-   the latest released version from CRAN with

    ``` r
    install.packages("cvAUC")
    ```

-   the latest development version from GitHub with

    ``` r
    if (packageVersion("devtools") < 1.6) {
      install.packages("devtools")
    }
    devtools::install_github("ledell/cvAUC")
    ```

## Using cvAUC
 
Here is a quick demo of how you can use the package.  In this example we do the following:
- Load an i.i.d. data set with a binary outcome.
- We will use 10-fold cross-validation, so we need to divide the indices randomly into 10 folds.  In this step, we [stratify](http://en.wikipedia.org/wiki/Stratified_sampling) the folds by the outcome variable.  Stratification is not necessary, but is commonly performed in order to create validation folds with similar distributions.  This information is stored in a 10-element list called `folds`.  Below, the function that creates the folds is called `.cvFolds`.
- For the v<sup>th</sup> iteration of the cross-validation (CV) process, fit a model on the training data (i.e. observations in folds `{1,...,10}\v`) and then using this saved fit, generate predicted values for the observations in the v<sup>th</sup> validation fold.  The `.doFit` function below does this procedure.  In this example, we the [Random Forest](http://en.wikipedia.org/wiki/Random_forest) algorithm.
- Next, this function is applied across all folds to generate predicted values for each validation fold.  
- These predicted values is stored in vector called `predictions`.  
- Lastly, we use the `ci.cvAUC` function to calculate CV AUC and to generate a 95% confidence interval for this CV AUC estimate.


First, we define a few utility functions:
```r
.cvFolds <- function(Y, V){
  # Create CV folds (stratify by outcome)	
  Y0 <- split(sample(which(Y==0)), rep(1:V, length=length(which(Y==0))))
  Y1 <- split(sample(which(Y==1)), rep(1:V, length=length(which(Y==1))))
  folds <- vector("list", length=V)
  for (v in seq(V)) {folds[[v]] <- c(Y0[[v]], Y1[[v]])}  	
  return(folds)
}

.doFit <- function(v, folds, train){
  # Train & test a model; return predicted values on test samples
  set.seed(v)
  ycol <- which(names(train)==y)
  params <- list(x = train[-folds[[v]],-ycol],
                 y = as.factor(train[-folds[[v]],ycol]),
                 xtest = train[folds[[v]],-ycol])
  fit <- do.call(randomForest, params)
  pred <- fit$test$votes[,2]
  return(pred)
}
```

This function will execute the example:
```r
iid_example <- function(train, y = "V1", V = 10, seed = 1) {
  
  # Create folds
  set.seed(seed)
  folds <- .cvFolds(Y = train[,c(y)], V = V)
  
  # Generate CV predicted values
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  predictions <- foreach(v = 1:V, .combine="c", 
    .packages=c("randomForest")) %dopar% .doFit(v, folds, train)
  stopCluster(cl)
  predictions[unlist(folds)] <- predictions

  # Get CV AUC and 95% confidence interval
  runtime <- system.time(res <- ci.cvAUC(predictions = predictions, 
                                         labels = train[,c(y)],
                                         folds = folds, 
                                         confidence = 0.95))
  print(runtime)
  return(res)
}
```

Load a sample binary outcome training set into R:
```r
train_csv <- "http://www.stat.berkeley.edu/~ledell/data/higgs_10k.csv"
train <- read.table(train_csv, sep=",")
```


Run the example:
```r
library(randomForest)
library(doParallel)
library(cvAUC)

res <- iid_example(train = train, y = "V1", V = 10, seed = 1)
print(res)

# $cvAUC
# [1] 0.7813759
#
# $se
# [1] 0.004534395
# 
# $ci
# [1] 0.7724886 0.7902631
# 
# $confidence
# [1] 0.95
```

## cvAUC Performance

For the example above (10,000 observations), it took ~0.2 seconds to calculate the cross-validated AUC and the influence curve based confidence intervals.  This was benchmarked on a 2.3 GHz Intel Core i7 processor using `cvAUC` package version 1.1.0. 

For bigger (i.i.d.) training sets, here are a few rough benchmarks:
- 100,000 observations: ~0.5 seconds 
- 1 million observations: ~13.0 seconds 

