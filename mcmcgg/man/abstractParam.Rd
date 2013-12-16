\name{abstractParam}
\alias{abstractParam}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A function to extract a single parameter of interest from a CODA chain
}
\description{
A function to extract a single parameter of interest from a CODA chain.
}
\usage{
abstractParam(paramNumber, chain)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{paramNumber}{
The index of the parameter to draw from the coda object. This is easy to get 
using the \code{dimnames(<coda object>)[[2]]} command and extracting the 
matching index from the output.
}
  \item{chain}{
A coda object from which to draw the parameter.
}
}

\value{
A vector with the parameter values by iteration number for the chain defined by
the coda object. 
}

\author{
Jacob Simmering
}

\examples{
data(exampleChain1)
data(exampleChain2)
alpha <- c(abstractParam(1, exampleChain1), abstractParam(1, exampleChain2))
}