library(stats)

centroidssummary <-
function(clustereddata){
  
  colnames(clustereddata)[(length(clustereddata))] <-"cluster_number"
  
  centroids            <-stats::aggregate(clustereddata, by=list(clustereddata$cluster_number), FUN = mean)
  
  clustereddata$counts <-1
  
  centroids            <-cbind(centroids,stats::aggregate(counts~cluster_number, data=clustereddata, FUN = sum))
  
  centroids            <-centroids[,c(
						(length(df)+2),
						2:(length(df)+1),
						(length(df)+4)
  )]
  
  return(centroids)
  
}
