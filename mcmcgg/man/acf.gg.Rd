\name{acf.gg}
\alias{acf.gg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to produce a ggplot2 visualization of the ACF for MCMC data
}
\description{
A function to produce a ggplot2 visualization of the ACF for MCMC data
}
\usage{
acf.gg(x, iter, nchain)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
A vector of length (\code{iter} * \code{nchain}) of the iteration values ordered
by chain number and then by iteration number. 
}
  \item{iter}{
A vector of length 1 with the number of iterations per chain. 
}
  \item{nchain}{
A vector of length 1 with the number of chains.
}
}

\value{
A ggplot2 object with the plot of the acf plot of the parameter passed in 
argument \code{x} for lags 0 to 100.
}

\author{
Jacob Simmering
}
\seealso{
\code{\link{abstractParam}}
\code{\link{history.gg}}
\code{\link{bgr.gg}}
}
\examples{
data(exampleChain1)
data(exampleChain2)
alpha <- c(abstractParam(1, exampleChain1), abstractParam(1, exampleChain2))
acf.gg(alpha, 1000, 2)
  }