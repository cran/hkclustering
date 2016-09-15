\name{centroidssummary}
\alias{centroidssummary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Returns Centroids summary
}
\description{
Returns Centroid summary
}
\usage{
centroidssummary(clustereddata)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{clustereddata}{
original dataframe
}
}
\value{
centroid
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{
Kaloyan S, \email{kaloyanS@profusion.com}
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (clustereddata) 
{
    colnames(clustereddata)[(length(clustereddata))] <- "cluster_number"
    centroids <- aggregate(clustereddata, by = list(clustereddata$cluster_number), 
        FUN = mean)
    clustereddata$counts <- 1
    centroids <- cbind(centroids, aggregate(counts ~ cluster_number, 
        data = clustereddata, FUN = sum))
    centroids <- centroids[, c((length(df) + 2), 2:(length(df) + 
        1), (length(df) + 4))]
    return(centroids)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ centroids }% use one of  RShowDoc("KEYWORDS")
\keyword{ summary }% __ONLY ONE__ keyword per line
