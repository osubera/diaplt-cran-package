\name{beadsplot}
\alias{beadsplot}
\alias{beadsplot.default}
\alias{beadsplot.data.frame}
\alias{beadsplot.numeric}
\alias{beadsplot.character}
\alias{beadsplot.formula}
\title{
Beads Summary Plot
}
\description{
Visualize one-factor data frame.  Look over a data frame
with many numeric columns and a factor column.
Beads plot consists of diamonds of each factor of each 
data series.  A diamond indicates average and range.
}
\usage{
beadsplot(x, ...)

\method{beadsplot}{default}(x, index=NULL, horizontal=FALSE,
               col=NULL, sheer=NULL, shading=NA, shading.angle=NA,
               bw=0.2, lwd=1, lwd.center=lwd, 
               legend=TRUE, label.factor=TRUE, label.range=TRUE, 
               drift.label.factor=c(0.2,2), drift.label.range=c(0,0),
               S=min, E=mean, N=max, summary.labels=NULL,
               plot=TRUE, verbose=FALSE, ...)

\method{beadsplot}{data.frame}(x, index=NULL, horizontal=FALSE,
               col=NULL, sheer=NULL, shading=NA, shading.angle=NA,
               bw=0.2, lwd=1, lwd.center=lwd, 
               legend=TRUE, label.factor=TRUE, label.range=TRUE, 
               drift.label.factor=c(0.2,2), drift.label.range=c(0,0),
               S=min, E=mean, N=max, summary.labels=NULL,
               plot=TRUE, verbose=FALSE, ...)

\method{beadsplot}{numeric}(x, index=NULL, horizontal=FALSE,
               col=NULL, sheer=NULL, shading=NA, shading.angle=NA,
               bw=0.2, lwd=1, lwd.center=lwd, 
               legend=TRUE, label.factor=TRUE, label.range=TRUE, 
               drift.label.factor=c(0.2,2), drift.label.range=c(0,0),
               S=min, E=mean, N=max, summary.labels=NULL,
               plot=TRUE, verbose=FALSE, ...)

\method{beadsplot}{character}(x, data, horizontal=FALSE,
               col=NULL, sheer=NULL, shading=NA, shading.angle=NA,
               bw=0.2, lwd=1, lwd.center=lwd, 
               legend=TRUE, label.factor=TRUE, label.range=TRUE, 
               drift.label.factor=c(0.2,2), drift.label.range=c(0,0),
               S=min, E=mean, N=max, summary.labels=NULL,
               plot=TRUE, verbose=FALSE, ...)

\method{beadsplot}{formula}(formula, data, horizontal=FALSE,
               col=NULL, sheer=NULL, shading=NA, shading.angle=NA,
               bw=0.2, lwd=1, lwd.center=lwd, 
               legend=TRUE, label.factor=TRUE, label.range=TRUE, 
               drift.label.factor=c(0.2,2), drift.label.range=c(0,0),
               S=min, E=mean, N=max, summary.labels=NULL,
               plot=TRUE, verbose=FALSE, ...)
}
\arguments{
  \item{x}{
data frame, 
contains observation columns 
(for \code{beadsplot.default}, that is,  = \code{beadsplot.data.frame}). \cr
numeric vector, 
as a single observation data (for \code{beadsplot.numeric}). \cr
character, as a column name of factor 
(for \code{beadsplot.character}). 
  }
  \item{index}{
factor to separate rows. 
default (\code{NULL}) is to plot without factors. \cr
character vector, or data frame with a single column,
as a factor to separate rows 
(for \code{beadsplot.default} and \code{beadsplot.numeric}). 
  }
  \item{formula}{
formula, a model formula, eg. \code{factor ~ obs1 + obs2 + obs3} 
(for \code{beadsplot.formula}). 
  }
  \item{data}{
data.frame, contains variables in formula
(for \code{beadsplot.formula}), 
or observation columns with a factor column 
(for \code{beadsplot.character}). \cr
  }
  \item{horizontal}{
if TRUE is given, diamond beads are plotted horizontally. 
default is FALSE.
  }
  \item{col}{
character vector, as colors by factor. 
default is \code{NULL} to select colors automatically.
  }
  \item{sheer}{
numeric vector of four items, as trasparencies, 
that is, alpha levels of colors. 
each item must be between \code{0} and \code{1}, 
\code{1} is for solid colors. 
items are in sequence of legend, label, border and diamond. 
default is \code{NULL}, same as \code{c(1, 0.9, 0.6, 0.2)}, 
legneds use solid colors, labels almost solid, borders sheer, 
and diamods are most sheer. 
  }
  \item{shading}{
numeric vector, as shading density to draw inside of beads. 
default is \code{NA} to draw automatic, usually no shadings. 
the density value means lines per inch. 
the generator depends the vector length. 
    \describe{
      \item{1}{a sigle value is used to all densities}
      \item{2}{values are generated between the pair}
      \item{k}{when k is number of factor levels, values are used to each level}
    }
logical value TRUE has a special meaning to enable shading with automatic densities.  
  }
  \item{shading.angle}{
numeric vector, as shading angle to draw inside of beads. 
default is \code{NA} to draw automatic, usually no shadings. 
the angle value means degree of line direction to horizon. 
the generator depends the vector length with same manner of density.
  }
  \item{bw}{
numeric value, as half of bead width relative to series distance,
default is \code{0.2}, is 40\% width.
  }
  \item{lwd}{
numeric value, as line width of bead border,
default is \code{1}. 
set \code{NULL} to disable border line. 
  }
  \item{lwd.center}{
numeric value, as line width of bead center,
default is \code{lwd}, same width as border.
set \code{NULL} to disable center line. 
  }
  \item{legend}{
logical value, to control the display of legned located at top. 
set FALSE to disable the legend.
default is TRUE.
  }
  \item{label.factor}{
logical value, to control the display of labels located at each bead. 
set FALSE to disable the labels.
default is TRUE. \cr
character vector, as alternative labels can be also specified. 
  }
  \item{label.range}{
logical value, to control the display of whole range values. 
set FALSE to disable showing range values. 
default is TRUE.
  }
  \item{drift.label.factor}{
numeric vector, as \code{(value, cycle)} 
to give small drifts to factor label location.
default is \code{c(0.2, 2)}, to generate \code{0.2, 0, 0.2, 0, ...}.
  }
  \item{drift.label.range}{
numeric vector, as \code{(value, cycle)} 
to give small drifts to range label location.
default is \code{c(0, 0)}, to generate no drifts. 
  }
  \item{S}{
function, to calculate summary for bottom vertex. 
default is \code{min}.
  }
  \item{E}{
function, to calculate summary for center diagonal.
default is \code{mean}.
  }
  \item{N}{
function, to calculate summary for top vertex. 
default is \code{max}.
  }
  \item{summary.labels}{
character vector, as names of summary functions S, E and N.
eg. \code{c('Minimum','Mean','Maximum')}.
default is \code{NULL}, no explicit names.
  }
  \item{plot}{
if FALSE is given, it disable to plot and print a summary.  
default is TRUE.  
  }
  \item{verbose}{
if TRUE is given, it print verbose debugging information. 
default is FALSE.  
  }
  \item{\dots}{
plot parameters and scale parameters are acceptable.  
  }
}
\section{Scale parameters}{
  \describe{
    \item{scale.range}{
Numeric value with default=1.
Width between center and border grids.
NULL turns off scaling.
    }
    \item{scale.mean}{
Numeric value with default=0.
Location of center grid.
NULL turns off centering.
    }
    \item{scale.log}{
Logical value with default=FALSE.
TRUE enables log10 scaling.
    }
    \item{scale.data.center}{
Numeric vector with default=NULL.
Give center value vector from outside.
See section Details and Examples. 
    }
    \item{scale.data.border}{
Numeric matrix with default=NULL.
Give border value matrix from outside.
See section Details and Examples. 
    }
    \item{scale.grid.center}{
Character value with default=NA.
Color of center grid. 
NULL turns off drawing the grid.
    }
    \item{scale.grid.border}{
Character value with default=NA.
Color of border grids. 
NULL turns off drawing the grids.
    }
    \item{cex.axis}{
Numeric value with default=1.
Font size of grid label.
    }
  }
}
\details{
This function is designed to visualize an overview of a data frame
with one factor. 
Such as, soil chemical components of several sites. \cr
When you have recommended values or critical limits for upper and lower 
which you want to compare with the data, scale parameter arguments 
\code{scale.data.center} and \code{scale.data.border} can be used. 
Eg. setting critical values of pH, Nitrogen and Phosphorus 
and draw horizontal grids as these critical values. 
}
\value{
A summary list is explicitly printed when plot=FALSE is given, 
and is invisibly returned when plot=TRUE.  
  \item{scaled}{
    scaled summary of three-dimensional array (series, factors, summaries)
    \describe{
      \item{series}{ each column of data }
      \item{factors}{ each index }
      \item{summaries}{ S/min, E/mean, N/max }
    }
    for one-dimensional vector data, series=1 is used.
    for one-level index=NULL, factors=1 is used.
  }
  \item{raw}{
    unscaled summary of three-dimensional array (series, factors, summaries)
    data structure is same as scaled item.
  }
  \item{scale}{
    scaling parameters
  }
}
\references{
Beads Summary Plot of Ranges: \url{http://tomizonor.wordpress.com/2013/11/12/beads-plot/}
}
\author{
Shinichiro Tomizono
}

\seealso{
  \code{\link{range}},
  \code{\link{min}},
  \code{\link{max}},
  \code{\link{mean}}.
}
\examples{
# iris data, by Species
beadsplot(Species~., iris)
beadsplot('Species', iris)
beadsplot(iris[1:4], iris[5])
beadsplot(iris[1:4], iris[,5])

# iris data, Petal.Length by Species
beadsplot(iris[,'Petal.Length'], iris[,'Species'], drift.label.factor=-0.2)

# horizontal
beadsplot(Species~., iris, horizontal=TRUE)

# color, sheer and shading
beadsplot(Species~., iris, 
          col=c('tomato1','tomato4','tomato3'), sheer=c(1,0.3,0.6,0.05))
beadsplot(Species~., iris, bw=0.5, label.factor=FALSE,
          sheer=c(1,0.9,0.6,0.4), lwd=NULL)
beadsplot(Species~., iris, shading=c(3,5))

# labels
beadsplot(Species~., iris, label.factor=FALSE)
beadsplot(Species~., iris, label.factor=c('a','b','c'), 
          drift.label.factor=0.05)
beadsplot(Species~., iris, legend=FALSE)

# scale grids
## disable grids
beadsplot(Species~., iris, scale.grid.center=FALSE, scale.grid.border=FALSE)
## color grids
beadsplot(Species~., iris, scale.grid.center='red', scale.grid.border='tan')

# scaling
beadsplot(Species~., iris, scale.range=50, scale.mean=50)
## no centering by mean
beadsplot(Species~., iris, scale.mean=NULL)
## plot raw values
beadsplot(Species~., iris, scale.range=NULL, scale.mean=NULL)
beadsplot(Species~., iris, scale.range=NULL, scale.mean=NULL, scale.log=TRUE)
## custom value scaling

### critical matrix as ranges of versicolor,
###   in sequeence of Sep.Len, Sep.Wid, Pet.Len, Pet.Wid
criticals <- matrix(c(4.9,7.0, 2.0,3.4, 3.0,5.1, 1.0,1.8), nrow=2, ncol=4)
beadsplot(Species~., iris, scale.data.border=criticals)

### recommend vector as mean of versicolor, in sequeence of 
###            S.Len, S.Wid, P.Len, P.Wid
recommend <- c(5.936, 2.770, 4.260, 1.326)
beadsplot(Species~., iris, scale.data.border=criticals, scale.data.center=recommend)
### when both of center and border is specified,
### center is dominance, border is adjusted to indicate  
### the width between lower and upper grids. 
### in this case, criticals can be simplified as follows,
### criticals <- matrix(c(0,2.1, 0,1.4, 0,2.1, 0,0.8), nrow=2, ncol=4)
### because differences each are used. 

### recommend vector as mean of setosa, in sequeence of 
###             S.Len, S.Wid, P.Len, P.Wid
recommend2 <- c(5.006, 3.428, 1.462, 0.246)
beadsplot(Species~., iris, scale.data.border=criticals, scale.data.center=recommend2)
### center grid is on setosa mean, 
### lower and upper grids indicate versicolor range width.

beadsplot(Species~., iris, scale.data.center=recommend2)
### center grid is on setosa mean, 
### lower and upper grids indicate whole range width.

# using median and IQR
q1 <- function(x) fivenum(x)[2]
q3 <- function(x) fivenum(x)[4]
q2 <- median
beadsplot(Species~., iris, S=q1, E=q2, N=q3)

# graphic parameters
beadsplot(Species~., iris, cex=0.7, cex.axis=0.7)

# print summary
beadsplot(Species~., iris, plot=FALSE)
}
\keyword{ hplot }
