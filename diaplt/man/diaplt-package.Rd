\name{diaplt-package}
\alias{diaplt-package}
\alias{diaplt}
\docType{package}
\title{
Beads Summary Plot
}
\description{
Visualize one-factor data frame.  Look over a data frame
with many numeric columns and a factor column.
Beads plot consists of diamonds of each factor of each 
data series.  A diamond indicates average and range.
}
\details{
\tabular{ll}{
Package: \tab diaplt\cr
Type: \tab Package\cr
Version: \tab 1.2.1\cr
Date: \tab 2013-11-12\cr
License: \tab MIT | \href{http://paidforeveryone.wordpress.com}{paid4e}: Copyright (c) 2013, Shinichiro Tomizono\cr
}
This package contains beadsplot function.
}
\author{
Shinichiro Tomizono

Maintainer: Shinichiro Tomizono <cowares@gmail.com>
}
\references{
Beads Summary Plot of Ranges: \url{http://tomizonor.wordpress.com/2013/11/12/beads-plot/}
}
\keyword{ package }
\keyword{ hplot }
\seealso{
  \code{\link{beadsplot}}.
}
\examples{
beadsplot(Species~., iris)
}
