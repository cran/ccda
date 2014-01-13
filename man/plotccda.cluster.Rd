\name{plotccda.cluster}
\alias{plotccda.cluster}
\title{ Plot of the basic grouping }
\description{ The function plotccda.cluster draws the dendrogram for the basic grouping using hierarchical clustering for the averages with Ward's method (as used in ccda.main). }
\usage{
plotccda.cluster(x)
}
\arguments{
  \item{x}{ The output list of ccda.main. }
  
}
\seealso{
\code{\link{ccda.main}}, \code{\link{plotccda.results}},
\code{\link{plotccda.q95}}
}
\examples{

result<-ccda.main(iris[,1:4] , iris[,5],  500, c("setosa","versicolor",
"virginica"),"proportions",return.RCDP=FALSE)
plotccda.cluster(result)

}
