\name{writeFunction}
\alias{writeFunction}
\title{
Write the code of a function as text file.
}
\description{
Write the code of a function as text file.
}
\usage{
writeFunction(FUN, fname = NULL)
}
\arguments{
  \item{FUN}{
The function to be written.
}
  \item{fname}{
Character; (optional) filename.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  No value is returned.
}
\author{
Ludo Pagie
}
\examples{
MyFun <- function(x) { x^2 }
writeFunction(MyFun)
}
\keyword{ misc }
