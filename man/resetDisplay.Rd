\name{resetDisplay}
\alias{resetDisplay}
\title{
Sets the environment variable DISPLAY.
}
\description{
When working with R in a screen session the DISPLAY environment variable
may change after reconnecting to the session. This prohibits using the
graphical devices in R. To reset the variable vlue in R first recover
the correct value in the terminal (eg env | grep -i display) and reset
the variable in the active Rsession.
}
\usage{
resetDisplay(dis)
}
\arguments{
  \item{dis}{
Numeric specifying the new value for the DISPLAY variable.
}
}
\details{
}
\value{
  No value is returned.
}
\author{
Ludo Pagie
}
\examples{
# not run:
resetDisplay(11)
}
\keyword{ misc }
