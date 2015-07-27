\name{cvAUC-package}
\alias{cvAUC-package}
\alias{cvAUC-package}
\docType{package}
\title{Cross-Validated Area Under the ROC Curve Confidence Intervals}
\description{
This package contains various tools for working with and evaluating cross-validated area under the ROC curve (AUC) estimators.  The primary functions of the package are \code{ci.cvAUC} and \code{ci.pooled.cvAUC}, which compute confidence intervals for cross-validated AUC estimates based on influence curves of both regular i.i.d and pooled repeated measures data.  One benefit to using influence function based confidence intervals is that they require much less computation time than bootstrapping methods.  The utility function, \code{cvAUC}, which computes cross-validated AUC, is a wrapper for functions from the ROCR package.
}
\details{
\tabular{ll}{
Package: \tab cvAUC\cr
Type: \tab Package\cr
Version: \tab 1.1.1\cr
Date: \tab 2015-07-28\cr
License: \tab Apache License (== 2.0)\cr
}
See the help files for the following functions for more information:

\code{\link{cvAUC}}, \code{\link{ci.cvAUC}}, \code{\link{ci.pooled.cvAUC}}
}
\author{
Erin LeDell, Maya Petersen, Mark van der Laan

Maintainer: Erin LeDell <oss@ledell.org>
}
\references{

LeDell, Erin; Petersen, Maya; van der Laan, Mark. Computationally efficient confidence intervals for cross-validated area under the ROC curve estimates. Electron. J. Statist. 9 (2015), no. 1, 1583--1607. doi:10.1214/15-EJS1035. \url{http://projecteuclid.org/euclid.ejs/1437742107}.

M. J. van der Laan and S. Rose. Targeted Learning: Causal Inference for Observational and Experimental Data. Springer Series in Statistics. Springer, first edition, 2011.

Tobias Sing, Oliver Sander, Niko Beerenwinkel, and Thomas Lengauer. ROCR: Visualizing classifier performance in R. Bioinformatics, 21(20):3940-3941, 2005.
}

\note{
This work was supported by the Doris Duke Charitable Foundation Grant No. 2011042
}

\seealso{
\url{http://cran.r-project.org/web/packages/ROCR/index.html}
}
