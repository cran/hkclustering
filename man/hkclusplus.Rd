\name{hkclusplus}
\alias{hkclusplus}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
hkclusplus
}
\description{
Takes a dataframe  and
performs  kmeans and a hierarchical clustering on the dataframe using teh gap statistic to calculate the initial number of centroids.
The function outputs a dataframe as the clustered data
}
\usage{
hkclusplus(df, t)
}

\arguments{
  \item{df}{
original dataframe
}
  \item{t}{
Number of iterations to find the centroids
}
}
\author{
Kaloyan S <kaloyanS@profusion.com>
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

a<-runif(500, min=3.5, max=2000)
b<-runif(500, min=1.5, max=2000)
df = data.frame(a, b) 

#Let the Gap statistic to find the clusters
results.hkplus<-hkclusplus(df,100)
centroidssummary(results.hkplus)
with(results.hkplus, pairs(results.hkplus[,1:2], col=c(1:7)[results.hkplus[,3]]))

## The function is currently defined as
function (df, t) 
{
    library(cluster)
    scaled.df <- scale(df)
    numbk <- which.max(clusGap(scaled.df, FUN = kmeans, K.max = 8, 
        B = 200)$Tab[, 3])
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
\keyword{ hierarchical_clustering }% use one of  RShowDoc("KEYWORDS")
\keyword{ gap_statistic }% __ONLY ONE__ keyword per line
