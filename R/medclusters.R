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


library(plyr)
library(zoo)
library(dplyr)
library(clusterSim)



find_clusters <- function(data,k){
  nums<-sapply(data, is.numeric)
  data_imp<-apply(r[5:ncol(r)],2, Hmisc::impute, mean)
  tseries.clustered = kmeans(data_imp, centers=k, iter.max=200)
  mdc<-new("MDCluster")
  mdc@labels<-tseries.clustered$cluster
  mdc@means<-tseries.clustered$centers
  return(mdc)
}



find_clusters_ts <- function(zoo_list, k){

  mdc<-new("MDCluster")
  return(mdc)
}




 makeRegular<-function(zoo_series){
   pthzoo_regular<-llply(zoo_series, function(x){ zoo(as.ts(x)) } )
   pthzoo_imp<-llply(pthzoo_regular, function(x) {na.approx(x, na.rm=FALSE, rule=2)})
   return (pthzoo_imp)

 }




 #' makes tabular form from zoo series, each observation comes as a row, each variable and point as  columns
 makeTabFromZooList<-function(zoo_list){


   #add column PID with id  value passed as parameters
   # add column SEQ_ID  from rowname
   add.id<-function(df, id){
     df$PID = id
     df$SEQ_ID = rownames(df)
     return (df)
   }




   list_of_df<-lapply(zoo_list, data.frame)
   list_of_df_ext<-lapply( seq_along(list_of_df),
                      function(y,n,i){ add.id( y[[i]],  n[[i]]) } ,
                      y = list_of_df,
                      n = names(list_of_df)
                   )
   df <- ldply(list_of_df_ext, data.frame)

   return (df)

 }



 #' makes tabular form from zoo series, each observation comes as a row, each variable and point as  columns
 #'
 makeZooListFromTab<-function(tab, id_colname='PID', seq_id_colname='SEQ_ID'){
   zoo_list<-by(tab, tab$PID, function(x)  zoo( x[,- which(names(x) %in% c("PID", "SEQ_ID"))], x$SEQ_ID))
   return(zoo_list)
 }



 normalize<-function(zoo_series){
   df<-makeTabFromZooList(zoo_series)
   df.meta<-df[,c('PID', 'SEQ_ID')]
   df.data<-df[,- which(names(df) %in% c("PID", "SEQ_ID"))]
   df_norm<-data.Normalization (df.data,type="n4",normalization="column")
   df_out<-cbind(df.meta, df_norm)
   zoo_series_norm<-makeZooListFromTab(df_out)
   return(zoo_series_norm)
 }


