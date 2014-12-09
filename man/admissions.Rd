\name{admissions}
\alias{admissions}
\docType{data}
\title{Data set: Simulated Admissions Data with Binary Outcome}
  
\description{
  This is a simulated data set that represents admissions information for a graduate program in the sciences.  The binary outcome is 1 for admitted and 0 for not admitted.  This data set is meant to be used with the \code{ci.cvAUC} function.}  

\usage{data(admissions)}

\format{A data frame. The five predictor variables are: \code{quant}, \code{verbal}, \code{gpa}, \code{toptier} and \code{research}.  We can treat \code{quant} and \code{verbal}, which represent quantitative and verbal GRE scores, as continuous variables.  The binary indicator variables, \code{toptier} and \code{research}, indicate whether the application is coming from a ``top tier'' institution and whether or not they have prior research experience.  The binary indicator, \code{Y}, represents admitted \code{(Y=1)} vs. not admitted \code{(Y=0)}.
}

\keyword{datasets}