\name{bgr.gg}
\alias{bgr.gg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to calculate the BGR diagnostic for MCMC convergence with ggplot2
}
\description{
A function to calculate the BGR diagnostic for MCMC convergence with ggplot2}
\usage{
bgr.gg(x, iter, nchain, T, qtype = 7)
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
  \item{T}{
A vector of the 2 times iteration numbers to consider splitting on. E.g., to 
assess convergence at iteration 500, use a T = 1000. Can pass multiple T values,
\code{seq()} is especially useful, but be aware of the increased compute time 
required. The function is currently rather crude and inefficient.  
}
  \item{qtype}{
A vector of length one that defines the method to be used by \code{quantile()} 
for calculating the order statistic, defaults to \code{type = 7}. See the 
documentation under \code{quantile()} for more information. None exactly match
the method used by OpenBUGS which is not clearly documented by appears to be 
simply ordering the values and selecting the desired quantile without any 
corrections for ties or other problems. Regardless of the type uses, it tracks
very near to the OpenBUGS reported values.
}
}

\value{
A ggplot2 object with the plot of the bgr plot of the parameter passed in 
argument \code{x} evaluated at the points passed in \code{T}.
}

\author{
Jacob Simmering
}
\seealso{
\code{\link{abstractParam}}
\code{\link{history.gg}}
\code{\link{acf.gg}}
}
\examples{
data(exampleChain1)
data(exampleChain2)
alpha <- c(abstractParam(1, exampleChain1), abstractParam(1, exampleChain2))
bgr.gg(alpha, 1000, 2, seq(10, 1000, 10))
  }
