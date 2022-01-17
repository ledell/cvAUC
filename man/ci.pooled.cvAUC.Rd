\name{ci.pooled.cvAUC}
\alias{ci.pooled.cvAUC}
\title{Confidence Intervals for Cross-validated Area Under the ROC Curve (AUC) Estimates for Pooled Repeated Measures Data 
}
\description{
This function calculates influence curve based confidence intervals for cross-validated area under the curve (AUC) estimates, for a pooled repeated measures data set.
}
\usage{
ci.pooled.cvAUC(predictions, labels, label.ordering = NULL, 
  folds = NULL, ids, confidence = 0.95)
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
  \item{ids}{
 A vector, matrix, list, or data frame containing cluster or entity ids.  All observations from the same entity (i.e. patient) that have been pooled must have the same id.  Must have the same dimensions as 'predictions'. 
}
  \item{confidence}{
A number between 0 and 1 that represents confidence level.
}
}
\details{
See the documentation for the \code{\link[ROCR]{prediction}} function in the ROCR package for details on the \code{predictions}, \code{labels} and \code{label.ordering} arguments. 

In pooled repeated measures data, the clusters (not the individual observations) are the independent units.  Each observation has a corresponding binary outcome.  This data structure arises often in clinical studies where each patient is measured, and an outcome is recorded, at various time points.  Then the observations from all patients are pooled together.  See the Examples section below for more information. 
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
\code{\link{cvAUC}}, \code{\link{ci.cvAUC}}
}


\examples{
# This example is similar to the ci.cvAUC example, with the excpection that
# this is a pooled repeated measures data set.  The example uses simulated
# data that contains multiple time point observations for 500 patients, 
# each observation having a binary outcome.
# 
# The cross-validation folds are stratified by ids that have at least one 
# positive outcome.  All observations belonging to one patient are
# contained within the save CV fold.


pooled_example <- function(data, ids, V = 10){

  .cvFolds <- function(Y, V, ids){
    #Stratify by outcome & id
    classes <- tapply(1:length(Y), INDEX = Y, FUN = split, 1)
    ids.Y1 <- unique(ids[classes$`1`])  #ids that contain an observation with Y==1
    ids.noY1 <- setdiff(unique(ids), ids.Y1)  #ids that have no Y==1 obvervations
    ids.Y1.split <- split(sample(length(ids.Y1)), rep(1:V, length = length(ids.Y1)))
    ids.noY1.split <- split(sample(length(ids.noY1)), rep(1:V, length = length(ids.noY1)))  
    folds <- vector("list", V)
    for (v in seq(V)){
      idx.Y1 <- which(ids \%in\% ids.Y1[ids.Y1.split[[v]]])
      idx.noY1 <- which(ids \%in\% ids.noY1[ids.noY1.split[[v]]])
      folds[[v]] <- c(idx.Y1, idx.noY1)
    }
    return(folds)
  }
  .doFit <- function(v, folds, data){  #Train/test glm for each fold
    fit <- glm(Y~., data = data[-folds[[v]],], family = binomial)
    pred <- predict(fit, newdata = data[folds[[v]],], type = "response")
    return(pred)
  }
  folds <- .cvFolds(Y = data$Y, ids = ids, V = V)  #Create folds
  predictions <- unlist(sapply(seq(V), .doFit, folds = folds, data = data))  #CV train/predict
  predictions[unlist(folds)] <- predictions  #Re-order fold indices
  out <- ci.pooled.cvAUC(predictions = predictions, labels = data$Y, 
                         folds = folds, ids = ids, confidence = 0.95)
  return(out)
}


# Load data
library(cvAUC)
data(adherence)


# Get performance
set.seed(1)
out <- pooled_example(data = subset(adherence, select=-c(id)), 
                      ids = adherence$id, V = 10)


# The output is given as follows:
# > out
# $cvAUC
# [1] 0.8648046
#
# $se
# [1] 0.01551888
#
# $ci
# [1] 0.8343882 0.8952211
#
# $confidence
# [1] 0.95


}
