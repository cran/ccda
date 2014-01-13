\name{plotccda.q95}
\alias{plotccda.q95}
\title{ CCDA density drawer }
\description{ The function plotccda.q95 draws the simulated density for the randomly coded datasets. }
\usage{
plotccda.q95(x,pl="max")
}
\arguments{
  \item{x}{ The output list of ccda.main which has to include the RCDP output! (Set return.RCDP=TRUE while running ccda.main). }
  \item{pl}{ "max" if the grouping with the highest difference value is considered or the number of the grouping for which the plot is made.}
  
}
\seealso{
\code{\link{ccda.main}}, \code{\link{plotccda.results}}, \code{\link{plotccda.cluster}}
}
\examples{

result<-ccda.main(iris[,1:4] , iris[,5],  500, c("setosa","versicolor",
"virginica"), "proportions", return.RCDP=TRUE)
plotccda.q95(result)
plotccda.q95(result, pl=2)

}
