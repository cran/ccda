\name{percentage}
\alias{percentage}
\title{ Calculation of the ratio of correctly classified cases by linear discriminant analysis }
\description{ Extracts the ratio of correctly classified cases from the output of lda. }
\usage{
percentage(dataset, starting_vector, prior)
}
\arguments{
  \item{dataset}{ Contains only the dataset as a matrix (without labels). }
  \item{starting_vector}{ A vector specifying the class for each observation. }
  \item{prior}{ A specified method that can be either "proportions" (in the case of different group sizes) or "equal" (in the case of equal group sizes). }
}
\value{
  \item{perctg }{The ratio of correctly classified cases by lda for the input grouping.}
}
\seealso{
\code{\link{ccda.main}}
}
