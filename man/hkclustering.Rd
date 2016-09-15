\name{hkclustering}
\alias{hkclustering}

\title{
Clustering function
}
\description{
Takes a dataframe and the number of initial clusters and performs  kmeans and a hierarchical clustering on the dataframe. The function outputs a dataframe as the clustered data
}
\usage{
hkclustering(df, numbk, t)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
Original dataframe to cluster
}
  \item{numbk}{
The number of initial clusters for the kmeans algorithm
}
  \item{t}{
Number of iterations to find the centroids
}
}

\author{
Kaloyan S, \email{kaloyanS@profusion.com}
}


%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

 
a<-runif(500, min=3.5, max=2000)
b<-runif(500, min=1.5, max=2000)
df = data.frame(a, b) 

#Specifying  4 clusters
results.hkclust<-hkclustering(df,4,100)
centroidssummary(results.hkclust)
with(results.hkclust, pairs(results.hkclust[,1:2], col=c(1:10)[results.hkclust[,3]]))

## The function is currently defined as
function (df, numbk, t) 
{
    scaled.df <- scale(df)
    rm(.Random.seed, envir = globalenv())
    temp <- kmeans(scaled.df, numbk)
    c <- temp$centers
    c <- temp$centers
    for (i in 2:t) {
        rm(.Random.seed, envir = globalenv())
        temp <- kmeans(scaled.df, numbk)
        c <- rbind(c, temp$centers)
    }
    cr <- as.data.frame(c, row.names = F)
    d <- dist(cr, method = "euclidean")
    fit <- hclust(d, method = "centroid")
    cr$clusnumber <- cutree(fit, k = numbk)
    centroids1 <- aggregate(cr, by = list(cr$clusnumber), FUN = mean)
    centr <- centroids1[, c(2:(length(df) + 1))]
    final <- kmeans(scaled.df, centr)
    clustereddata <- cbind(df, final$cluster)
    colnames(clustereddata)[(length(df) + 1)] <- "cluster_number"
    return(clustereddata)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ kmeans_clustering }% use one of  RShowDoc("KEYWORDS")
\keyword{ hierarchical_clustering }% __ONLY ONE__ keyword per line
