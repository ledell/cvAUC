\name{ci.cvAUC}
\alias{ci.cvAUC}
\title{
Confidence Intervals for Cross-validated Area Under the ROC Curve (AUC) Estimates
}
\description{
This function calculates influence curve based confidence intervals for cross-validated area under the ROC curve (AUC) estimates.
}
\usage{
ci.cvAUC(predictions, labels, label.ordering = NULL, folds = NULL, confidence = 0.95)
}
\arguments{
  \item{predictions}{
 A vector, matrix, list, or data frame containing the predictions.
}
  \item{labels}{
A vector, matrix, list, or data frame containing the true class labels. Must have the same dimensions as \code{predictions}.
}
  \item{label.ordering}{
The default ordering of the classes can be changed by supplying a vector containing the negative and the positive class label (negative label first, positive label second).
}
  \item{folds}{
If specified, this must be a vector of fold ids equal in length to \code{predictions} and \code{labels}, or a list of length V (for V-fold cross-validation) of vectors of indexes for the observations contained in each fold.  The \code{folds} argument must only be specified if the \code{predictions} and \code{labels} arguments are vectors.
}
  \item{confidence}{
A number between 0 and 1 that represents confidence level.
}
}
\details{
See the documentation for the \code{\link[ROCR]{prediction}} function in the ROCR package for details on the \code{predictions}, \code{labels} and \code{label.ordering} arguments. 
}
\value{
A list containing the following named elements:
\item{cvAUC}{Cross-validated area under the curve estimate.}
\item{se}{Standard error.}
\item{ci}{A vector of length two containing the upper and lower bounds for the confidence interval.}
\item{confidence}{A number between 0 and 1 representing the confidence.}

}
\references{

LeDell, Erin; Petersen, Maya; van der Laan, Mark. Computationally efficient confidence intervals for cross-validated area under the ROC curve estimates. Electron. J. Statist. 9 (2015), no. 1, 1583--1607. doi:10.1214/15-EJS1035. \url{http://projecteuclid.org/euclid.ejs/1437742107}.

M. J. van der Laan and S. Rose. Targeted Learning: Causal Inference for Observational and Experimental Data. Springer Series in Statistics. Springer, first edition, 2011.

Tobias Sing, Oliver Sander, Niko Beerenwinkel, and Thomas Lengauer. ROCR: Visualizing classifier performance in R. Bioinformatics, 21(20):3940-3941, 2005.
}
\author{
Erin LeDell \email{oss@ledell.org}

Maya Petersen \email{mayaliv@berkeley.edu}

Mark van der Laan \email{laan@berkeley.edu}
}

\seealso{
\code{\link[ROCR]{prediction}}, \code{\link[ROCR]{performance}},
\code{\link{cvAUC}}, \code{\link{ci.pooled.cvAUC}}
}
\examples{
# This i.i.d. data example does the following:

# Load a data set with a binary outcome. For the i.i.d. case we use a simulated data set of 
# 500 observations, included with the package, of graduate admissions data. 
# 
# Divide the indices randomly into 10 folds, stratifying by outcome.  Stratification is not 
# necessary, but is commonly performed in order to create validation folds with similar 
# distributions.  Store this information in a list called folds.
# 
# Define a function to fit a model on the training data and to generate predicted values 
# for the observations in the validation fold, for a single iteration of the cross-validation 
# procedure.  We use a logistic regression fit.
# 
# Apply this function across all folds to generate predicted values for each validation fold.  
# The concatenated version of these predicted values is stored in vector called predictions.  
# The outcome vector, Y, is the labels argument. 


iid_example <- function(data, V = 10){

  .cvFolds <- function(Y, V){  #Create CV folds (stratify by outcome)
    Y0 <- split(sample(which(Y==0)), rep(1:V, length = length(which(Y==0))))
    Y1 <- split(sample(which(Y==1)), rep(1:V, length = length(which(Y==1))))
    folds <- vector("list", length=V)
    for (v in seq(V)) {folds[[v]] <- c(Y0[[v]], Y1[[v]])}		
    return(folds)
  }
  .doFit <- function(v, folds, data){  #Train/test glm for each fold
    fit <- glm(Y~., data = data[-folds[[v]],], family = binomial)
    pred <- predict(fit, newdata = data[folds[[v]],], type = "response")
    return(pred)
  }
  folds <- .cvFolds(Y = data$Y, V = V)  #Create folds
  predictions <- unlist(sapply(seq(V), .doFit, folds = folds, data = data))  #CV train/predict
  predictions[unlist(folds)] <- predictions  #Re-order pred values
  # Get CV AUC and confidence interval
  out <- ci.cvAUC(predictions = predictions, labels = data$Y, 
                  folds = folds, confidence = 0.95)
  return(out)
}


# Load data
library(cvAUC)
data(admissions)

# Get performance
set.seed(1)
out <- iid_example(data = admissions, V = 10) 


# The output is given as follows:
# > out
# $cvAUC
# [1] 0.9046473
# 
# $se
# [1] 0.01620238
# 
# $ci
# [1] 0.8728913 0.9364034
#
# $confidence
# [1] 0.95
}
