\name{mcmcgg-package}
\alias{mcmcgg-package}
\alias{mcmcgg}
\docType{package}
\title{
Simple functions for visualizing mcmc chains using ggplot2
}
\description{
A set of simple functions for the visualization of MCMC output data, 
specifically from OpenBUGS/WinBUGS, for assessing convergence, history and 
auto-correlation.  
}
\details{
\tabular{ll}{
Package: \tab mcmcgg\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-12-16\cr
License: \tab GPL v2\cr
}
See the vignette and man pages for \code{abstractParam}, \code{acf.gg}, 
\code{bgr.gg} and \code{history.gg}
for information on how to use this package. 
}
\author{
Maintainer: Jacob Simmering <jacob@jacobsimmering.com>
}
\keyword{ package }
\seealso{
\code{\link{abstractParam}}
\code{\link{history.gg}}
\code{\link{acf.gg}}
\code{\link{bgr.gg}}
}
\examples{
data(exampleChain1)
data(exampleChain2)
alpha <- c(abstractParam(1, exampleChain1), abstractParam(1, exampleChain2))
history.gg(alpha, 1000, 2)
acf.gg(alpha, 1000, 2)
bgr.gg(alpha, 1000, 2, seq(10, 1000, 10))
}
