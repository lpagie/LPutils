\name{signString}
\alias{signString}
\title{
Add a substring with initials and date to a string.
}
\description{
Adding this substring can be used to sign filenames with initials and
date. 
}
\usage{
signString(str, is.filename = T, initials = "LP")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{str}{
Character string, to be signed.
}
  \item{is.filename}{
Logical; if TRUE the character string is expected to have a filename
extension. The initials/date is placed just before the extension. If
false the substring is simply added to the end of the string.
}
  \item{initials}{
Character string; your initials.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
 Character string with inserted substring.
}
\author{
Ludo Pagie
}
\examples{
filename <- 'annot.gff'
signString(filename)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ misc }
