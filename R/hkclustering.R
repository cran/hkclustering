library(stats)

hkclustering <-
function(df,numbk,t){
  
  scaled.df <- scale(df)
  
  rm(.Random.seed, envir=globalenv())
  temp<-stats::kmeans(scaled.df,numbk)
    
  c  <-temp$centers
  
  for (i in 2:t){
    rm(.Random.seed, envir=globalenv())
    temp <-stats::kmeans(scaled.df,numbk)
    c    <-rbind(c,temp$centers)
  }
  
  
  cr     <-as.data.frame(c,row.names = F)
  
  d      <- stats::dist(cr, method = "euclidean")
  fit    <- stats::hclust(d, method="centroid") 
    
  cr$clusnumber <- stats::cutree(fit, k=numbk)
  
  centroids1    <-stats::aggregate(cr, by=list(cr$clusnumber), FUN = mean)
    
  centr         <-centroids1[,c(2:(length(df)+1))]
  final         <-stats::kmeans(scaled.df,centr)
  
  clustereddata <-cbind(df,final$cluster)
  
  colnames(clustereddata)[(length(df)+1)] <-"cluster_number"
  
  return(clustereddata)
  
}
