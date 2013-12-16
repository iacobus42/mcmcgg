\name{history.gg}
\alias{history.gg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to produce a ggplot2 of a (set of) mcmc chain(s) history
}
\description{
A function to produce a ggplot2 of a (set of) mcmc chain(s) history
}
\usage{
history.gg(x, iter, nchain, thin = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  A vector of length \code{iter} * \code{nchain} with each element being a 
  iterate from the mcmc. The expected ordering is by chain (e.g., obs 1 from 
  chain 1, 2 from 1,..., \code{iter} from 1, 1 from chain 2, ...). 
}
  \item{iter}{
  A vector of length 1 giving the number of iterations in a complete chain
}
  \item{nchain}{
  A vector of length 1 giving the number of chains
}
  \item{thin}{
  A vector of length 1 giving a thinning parameter. Defaults to 1 which is no
  thinning. For \code{thin} > 1, the plot contains only every \code{thin}^th 
  observation. Useful for chains with large numbers of iterations. 
}
}
\value{
A ggplot2 object with the plot of the history of the parameter passed in 
argument \code{x}.
}
\author{
Jacob Simmering, Jessica Orth
}
\note{
See \code{abstractParam()} for details on extracting a parameter from the 
coda data files. 
}

\seealso{
\code{\link{abstractParam}}
\code{\link{bgr.gg}}
\code{\link{acf.gg}}
}
\examples{
data(exampleChain1)
data(exampleChain2)
alpha <- c(abstractParam(1, exampleChain1), abstractParam(1, exampleChain2))
history.gg(alpha, 1000, 2)
}
