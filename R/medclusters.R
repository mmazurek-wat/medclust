#' @title medclust
#'
#' @description Clustering  time series
#'
#' @param  Data wide data set (time points in columns)
#'
#' @return Clustered series data
#'
#' @examples
#'
#' @export MDCluster

setClass("MDCluster", slots=list(labels = "vector", means = "matrix"))


find_clusters <- function(data,k){
  nums<-sapply(data, is.numeric)
  data_imp<-apply(r[5:ncol(r)],2, Hmisc::impute, mean)
  tseries.clustered = kmeans(data_imp, centers=k, iter.max=200)
  mdc<-new("MDCluster")
  mdc@labels<-tseries.clustered$cluster
  mdc@means<-tseries.clustered$centers
  return(mdc)
}

makeRegular<-function(zoo_series){
  pthzoo_regular<-llply(zoo_series, function(x){ zoo(as.ts(x)) } )
  pthzoo_imp<-llply(pthzoo_regular, function(x) {na.approx(x, na.rm=FALSE, rule=2)})
  pyhzoo_norm<-
  return (pthzoo_imp)

}







