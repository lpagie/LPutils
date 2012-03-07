\name{wideScreen}
\alias{wideScreen}
\title{
Changes the \code{width} option.
}
\description{
The \code{width} option determines the number of columns used in
printing objects (vectors, matrices, etc). This function takes the width
of the terminal in which the Rsession runs and adjusts the option
accordingly. Specifying the \code{ncols} argument allows specifying the
new width.
}
\usage{
wideScreen(ncols)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ncols}{
Numeric specifying new value for \code{width}.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  No value is returned.
}
\author{
Wouter Meuleman
}
\examples{
  wideScreen()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }
