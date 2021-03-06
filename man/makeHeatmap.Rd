% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeHeatmap.R
\name{makeHeatmap}
\alias{makeHeatmap}
\title{Add gradient background color to ztable}
\usage{
makeHeatmap(
  z,
  palette = "Reds",
  mycolor = NULL,
  rows = NULL,
  cols = NULL,
  changeColor = TRUE,
  reverse = FALSE,
  margin = 0
)
}
\arguments{
\item{z}{An object of class ztable}

\item{palette}{Name of color palette}

\item{mycolor}{user defined color vectors}

\item{rows}{columns to make heatmap}

\item{cols}{columns to make heatmap}

\item{changeColor}{Logical. Whether of not change font color automatically}

\item{reverse}{If true, reverse the font color}

\item{margin}{An integer. Choices are one of 0,1 and 2. 0(default), heatmap for all numeric data.
1 ; rowwise heatmap, 2: columnwise heatmap.}
}
\description{
Add gradient background color to ztable
}
\examples{
require(magrittr)
ztable(head(mtcars)) \%>\% makeHeatmap()
\dontrun{
ztable(head(mtcars)) \%>\% makeHeatmap(palette="YlOrRd",cols=c(1,4,6),margin=2)
ztable(head(mtcars)) \%>\% makeHeatmap(rows=c(1,3,5),margin=1)
require(moonBook)
x=table(acs$smoking,acs$Dx)
ztable(x) \%>\% makeHeatmap
ztable(x) \%>\% makeHeatmap(palette="Blues")
ztable(x) \%>\% makeHeatmap(mycolor=gradientColor(low="yellow",mid="orange",high="red"))
}
}
