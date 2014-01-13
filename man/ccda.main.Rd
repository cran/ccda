\name{ccda.main}
\alias{ccda.main}
\title{ Combined Cluster and Discriminant Analysis }
\description{ Classification into homogeneous groups using combined cluster and discriminant analysis (CCDA).}
\usage{ 
ccda.main(dataset, names_vector, nr, nameslist, 
prior = "proportions",return.RCDP=FALSE) }
\arguments{
  \item{dataset}{ Contains only the dataset as a matrix (without labels). }
  \item{names_vector}{ Contains labels (names of sample origins) for each individual observation. }
  \item{nr}{ Number of randomly coded datasets (RCD) investigated. }
  \item{nameslist}{ Contains the names of sample origins as a list. }
  \item{prior}{ A specified method that can be either "proportions" (in the case of different group sizes) or "equal" (in the case of equal group sizes). If unspecified, "proportions" is used as the default.}
  \item{return.RCDP}{A logical value indicating whether the method should return the percentages for the randomly coded datasets as a matrix. Not returned, unless set to "TRUE".}
}
\details{
ccda.main determines the basic grouping (Step I). For this it uses hierarchical clustering with Ward's method for the averages of the measured variables. Step II, the core cycle then runs for every one of the obtained groupings. For a suggestion on the number of randomly coded datasets investigated (nr), see Appendix in Kovacs et al., 2014. It should be noted that nr has a linear influence on the amount of time needed for computing.

Step III, the evaluation of the results is left to the user based on the output of ccda.main. Based on these outputs, the function plot.ccda.result helps the decision regarding further division.

The subgroups component of the output contains the grouping with the highest corresponding difference value. The iterative further investigation of these subgroups is required in order to obtain homogeneous groups as a final result. One should stop when the highest difference value is reached when every sampling location belongs to the same group.
}
\value{
  \item{nameslist }{Returns the input nameslist.}
  \item{q95 }{The 95 \% quantiles of the ratios of correctly classified cases by LDA for the randomly coded datasets.}
  \item{ratio }{Ratios of correctly classified cases by LDA for each coded dataset.}
  \item{difference }{Ratio-q95.}
  \item{sub_groups }{Suggestion for subdivision according to the maximal difference value.}
  \item{RCDP }{Percentages for the randomly coded datasets as a matrix.}
}
\references{
Jozsef Kovacs, Solt Kovacs, Norbert Magyar, Peter Tanos, Istvan Gabor Hatvani, Angela Anda (2014): Classification into homogeneous groups using combined cluster and discriminant analysis (CCDA). Environmental Modelling & Software. DOI: http://dx.doi.org/10.1016/j.envsoft.2014.01.010
}
\seealso{
\code{\link{percentage}}, \code{\link{plotccda.results}}, \code{\link{plotccda.q95}}, 
\code{\link{plotccda.cluster}}
}
\examples{

ccda.main(iris[,1:4] , iris[,5],  500, c("setosa","versicolor","virginica"),
"proportions",return.RCDP=FALSE)

}
