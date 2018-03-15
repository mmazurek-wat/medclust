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

setClass("MDCluster", slots=list(labels = "vector", centroids = "list",  distance="vector", size="vector", variance="vector", cluster_distance="matrix" ))



library(plyr)
library(zoo)
library(dplyr)
library(clusterSim)
library(dtwclust)



find_clusters_tab <- function(data,k){
  nums<-sapply(data, is.numeric)
  data_imp<-apply(r[5:ncol(r)],2, Hmisc::impute, mean)
  clus = kmeans(data_imp, centers=k, iter.max=200)
  mdc<-new("MDCluster")
  mdc@labels<-clus@cluster
  mdc@centroids<-clus@centers


  return(mdc)
}



find_clusters_ts <- function(zoo_list, k=2){
  clus<-dtwclust::tsclust(zoo_list, type="partitional", k )
  mdc<-new("MDCluster")
  mdc@labels<-clus@cluster
  mdc@centroids<-clus@centroids
  mdc@distance<-clus@distance
  mdc@size=clus@clusinfo[,1]
  mdc@variance=clus@clusinfo[,2]

  #distance from point to cluster center
  mdc@distance=clus@cldist

  # calculation cluster centroid distances
  centr<-clus@centroids
  n=length(centr)
  m=matrix(nrow=n, ncol=n)
  for(i in seq(1,length(centr))){
    for(j in seq(1,length(centr))){
      m[i,j]=dtw_basic(centr[[i]], centr[[j]])
    }
  }

  mdc@cluster_distance<-m
  return(mdc)
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
   zoo_list<-by(tab, tab$PID, function(x)  zoo( x[,- which(names(x) %in% c("PID", "SEQ_ID"))], as.numeric(x$SEQ_ID)))
   return(zoo_list)
 }





 preprocess<-function(zoo_series){

   zoo_series_regular<-llply(zoo_series, function(x){ zoo(as.ts(x)) } )
   zoo_series_imputed<-llply(zoo_series_regular, function(x) {na.approx(x, na.rm=FALSE, rule=2)})

   df<-makeTabFromZooList(zoo_series_imputed)
   df.meta<-df[,c('PID', 'SEQ_ID')]
   df.data<-df[,- which(names(df) %in% c("PID", "SEQ_ID"))]
   df_norm<-data.Normalization (df.data,type="n4",normalization="column")
   df_out<-cbind(df.meta, df_norm)
   zoo_series_norm<-makeZooListFromTab(df_out)
   return(zoo_series_norm)
 }


 showClusterStats<-function(mdc) {

   df.clust<-data.frame(n=mdc@size,var=mdc@variance )

   df.clust$cluster_id <- as.factor(rownames(df.clust))
   df.clust<-df.clust[order(-df.clust$n),]


   # visualize clusters
   w <- df.clust$n
   pos <- 0.5 * (cumsum(w) + cumsum(c(0, w[-length(w)])))
   min.var <- max(df.clust$var)


   blank_theme <- theme_minimal()+
     theme(
       axis.title.x = element_blank(),
       axis.title.y = element_blank(),
       panel.border = element_blank(),
       panel.grid=element_blank(),
       axis.ticks = element_blank(),
       axis.text.y = element_blank(),
       plot.title=element_text(size=14, face="bold")
     )


   theme_infog <- theme_classic() +
     theme(
       axis.line = element_blank(),
       axis.title = element_blank(),
       axis.ticks = element_blank(),
       axis.text.y = element_blank())


   p <-ggplot(df.clust, aes(x = pos))
   p<- p+ geom_bar(aes(x = pos, y = min.var*1.2, width = w), color = "black",  stat = "identity", fill = "grey90")
   p<- p + geom_bar(aes(y = var, fill=var), width = w,   stat = "identity")
   #p<- p+  geom_text(aes(y = min.var*1.2,  label = paste("n=",n)), size=3)
   p<- p + scale_fill_gradient(low = "#132B43", high = "#56B1F7", name = "Within variance")
   p<- p + coord_polar(theta = "x")
   p<- p+  scale_x_continuous(labels = paste("ID:",  df.clust$cluster_id, "\nn=" , df.clust$n), breaks = pos)
   p<-p+theme(legend.position="none")


   p<-p+blank_theme
   print(p)

 }

