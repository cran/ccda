\name{plotccda.results}
\alias{plotccda.results}
\title{ Plot of the results of ccda.main }
\description{ Plots the summarized results of CCDA for all possible groupings based on the output of ccda.main. }
\usage{
plotccda.results(x)

}
\arguments{
  \item{x}{ The output list of ccda.main. }
    
}
\seealso{
\code{\link{ccda.main}}, \code{\link{plotccda.cluster}},
\code{\link{plotccda.q95}}
}
\examples{

result<-ccda.main(iris[,1:4] , iris[,5],  500, c("setosa","versicolor",
"virginica"),"proportions", return.RCDP=FALSE)
plotccda.results(result)

}
