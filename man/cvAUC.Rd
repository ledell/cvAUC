\name{cvAUC}
\alias{cvAUC}
\title{Cross-validated Area Under the ROC Curve (AUC)}
\description{
This function calculates cross-validated area under the ROC curve (AUC) esimates.  For each fold, the empirical AUC is calculated, and the mean of the fold AUCs is the cross-validated AUC estimate.  The area under the ROC curve is equal to the probability that the classifier will score a randomly drawn positive sample higher than a randomly drawn negative sample.  This function is a simple wrapper for the AUC functionality inside the ROCR package.
}
\usage{
cvAUC(predictions, labels, label.ordering = NULL, folds = NULL)
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
}
\details{
If \code{predictions} and \code{labels} are provided as vectors and \code{folds} is \code{NULL}, then this function will return AUC (not cross-validated).   See the documentation for the \code{\link[ROCR]{prediction}} function in the ROCR package for details on the \code{predictions}, \code{labels} and \code{label.ordering} arguments. 
}
\value{
\item{perf}{An object of class 'performance' from the \code{ROCR} package.  Can be used to plot the ROC curve.}
\item{fold.AUC}{A vector containing the AUC estimate for each fold.}
\item{cvAUC}{Cross-validated area under the curve.}
}
\references{
Tobias Sing, Oliver Sander, Niko Beerenwinkel, and Thomas Lengauer. ROCR: Visualizing classifier performance in R. Bioinformatics, 21(20):3940-3941, 2005.
}
\author{
Erin LeDell \email{ledell@berkeley.edu}
}

\seealso{
\code{\link[ROCR]{prediction}}, \code{\link[ROCR]{performance}}, \code{\link{ci.cvAUC}}, \code{\link{ci.pooled.cvAUC}}
}
\examples{
# Example of how to get CV AUC and plot the curve.
data(ROCR.hiv)
attach(ROCR.hiv)

out <- cvAUC(hiv.svm$predictions, hiv.svm$labels)

#Plot fold AUCs
plot(out$perf, col="grey82", lty=3, main="10-fold CV AUC")

#Plot CV AUC
plot(out$perf, col="red", avg="vertical", add=TRUE)

# See the ci.cvAUC documentation for an example 
# of how to use the `folds` argument.
}