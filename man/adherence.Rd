\name{adherence}
\alias{adherence}
\docType{data}
\title{Data set: Simulated Pooled Repeated Measures Data}
  
\description{
  This is a simulated pooled repeated measures data set of patient medication adherence summaries with a binary outcome.  The independent units are the patients, and each patient has one or more measurements made at different time points in treatment, each associated with a binary outcome that could represent a diagnostic test result.  This data set is meant to be used with the \code{ci.pooled.cvAUC} function.
}

\usage{data(adherence)}

\format{A data frame.  The first column, \code{id}, is the patient id.  Columns 2-5 represent medication adherence (as a percentage) averaged over the past 2, 7, 14, 21 and 28 days, respectively.  The last column is a binary outcome that could represent a test result.}

\seealso{

\code{\link{ci.pooled.cvAUC}}

}
\keyword{datasets}