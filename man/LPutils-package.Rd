\name{LPutils-package}
\alias{LPutils-package}
\alias{LPutils}
\docType{package}
\title{
General utilities.
~~ package title ~~
}
\description{
General utilities; sign character strings with initials/date, reset
DISPLAY environment variable, change width option for printing, renaming
list elements, and writing function objects as tex files.
~~ A concise (1-5 lines) description of the package ~~
}
\details{
\tabular{ll}{
Package: \tab LPutils\cr
Type: \tab Package\cr
Version: \tab 0.0.1\cr
Date: \tab 2012-03-02\cr
License: \tab GPL \cr
}
A small collection of some general utilities to make working with R just
a little bit simpler. 
~~ An overview of how to use the package, including the most important ~~
~~ functions ~~
}
\author{
Ludo Pagie and Wouter Meuleman.

Maintainer: Ludo Pagie <ludo.pagie@gmail.com>
~~ The author and/or maintainer of the package ~~
}
\keyword{ package }
\examples{
# rename
dt <- list(var1=1:3, var2=letters[1:3])
rename(dt, list(var1='el1', var2='el2'))

# resetDisplay
# not run:
resetDisplay(11)

# signString
filename <- 'annot.gff'
signString(filename)

# wideScreen
wideScreen()

# writeFunction
MyFun <- function(x) { x^2 }
writeFunction(MyFun)
}
