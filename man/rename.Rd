\name{rename}
\alias{rename}
\title{
Rename (some of) the elements in a vector, list, or data frame.
}
\description{
The names of the object \code{x} are replaced by the names given in the
list \code{replace}.
}
\usage{
rename(x, replace)
}
\arguments{
  \item{x}{
A vector, list, or data frame.
}
  \item{replace}{
A named vector or list, where the names of the elements correspond to
the elements in the supplied object which are to be changed. The values
contain the new names.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  Returns the supplied object \code{x} with new names.
}
\author{
Ludo Pagie
}
\examples{
dt <- list(var1=1:3, var2=letters[1:3])
rename(dt, list(var1='el1', var2='el2'))

}
\keyword{ misc }
