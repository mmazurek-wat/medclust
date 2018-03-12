

library(medclust)
library(plyr)
library(zoo)
data(pthzoo)
data(pthseries)

library(clusterSim)

colnames<-colnames(pthzoo[[1]])
pthzoo_regular<-llply(pthzoo, function(x){ zoo(as.ts(x)) } )
pthzoo_imp<-llply(pthzoo_regular, function(x) {na.approx(x, na.rm=FALSE, rule=2)})



df<-data.frame(
  matrix(unlist(llply(pthzoo_imp, function(x) { apply(x,2, max) })), nrow =length(pthzoo_imp), byrow=TRUE)
)


df_norm<-data.Normalization (df,type="n7",normalization="column")
df

lodf<-llply(pthzoo_imp, function(x) unlist(x))
lodf<-llply(pthzoo_imp, function(x) unlist(x))


tc.df<-lapply(pthzoo_imp, data.frame)

add.id<-function(data, id){
  data$PID = id
  data$time.seq = rownames(data)
  data
}



tc.df.1<-lapply(seq_along(tc.df), function(y,n,i){ add.id(y[[i]],n[[i]]) } , y = tc.df, n=names(tc.df))

pthseries_l %>% dplyr::group_by(PID) %>% do(zoo(.))


pthzoo<-by(pthseries_l, pthseries_l$PID, function(x)  zoo(x[,3:ncol(x)], x$time.seq))
z1<-lzoo[[1]]
z1
time(z1)
z1





library(rlist)
tc.df.2<-list.rbind(tc.df.1)


pthzoo1<-by(tc.df.2, tc.df.2$PID, function(x)  zoo(x[,3:ncol(x)], x$time.seq))
z1<-pthzoo1[[1]]
z1
time(z1)
z1






tc.df.2



pthzoo_norm<-llply(pthzoo_imp, function(x) {(x-lb)/(ub-lb)})


pthzoo_imp$`555866`
pthzoo_norm$`555866`

pthzoo_imp$'1153003'
pthzoo_regular$'1153003'

library(dtwclust)
clust<-tsclust(pthzoo_imp)

plot(clust)


seq1=c(1,2,3,4,5,5,6)
seq2=c(5,4,3,2,1,1,1)
seq3=c(3,3,2,2,1,1,4)
seq4=c(3,2,1,2,3,2,1)


seq1A=-seq1/2
seq2A=-seq2*2
seq3A=-seq3
seq4A=2-seq4



z1=zoo(seq1)
z2=zoo(seq2)

z3=zoo(seq3)
z4=zoo(seq4)

tz=list(z1,z2,z3,z4)

clust<-tsclust(tz, type="hierarchical")

clust<-tsclust(tz, type="fuzzy", centroid="fcmdd")
clust<-tsclust(tz, type="fuzzy", centroid="fcm")



clust<-tsclust(tz, type="partitional", centroid="median")
clust<-tsclust(tz, type="partitional", centroid="shape") #shape exracion
clust<-tsclust(tz, type="partitional", centroid="pam") #partitioning around medoids
clust<-tsclust(tz, type="partitional", centroid="mean")
plot(clust)



z1=zoo( matrix(c(seq1, seq1A), ncol=2, byrow=FALSE), order.by = seq(1,7))
z2=zoo( matrix(c(seq2, seq2A), ncol=2, byrow=FALSE), order.by = seq(1,7))
z3=zoo( matrix(c(seq3, seq3A), ncol=2, byrow=FALSE), order.by = seq(1,7))
z4=zoo( matrix(c(seq4, seq4A), ncol=2, byrow=FALSE), order.by = seq(1,7))


plot(z1)

tz=list(z1,z2,z3,z4)

clust<-tsclust(tz, type="hierarchical")

clust<-tsclust(tz, type="fuzzy", centroid="fcmdd")
clust<-tsclust(tz, type="fuzzy", centroid="fcm")



clust<-tsclust(tz, type="partitional", centroid="median")
clust<-tsclust(tz, type="partitional", centroid="shape") #shape exracion
clust<-tsclust(tz, type="partitional", centroid="pam") #partitioning around medoids


#wyswietlenie centroidow

clust@centroids


clust@cluster


clust@clusinfo











library(cluster)

df=data.frame(matrix(c(seq1, seq2, seq3, seq4), nrow=4 , byrow=TRUE))
plot(seq1,seq2,seq3)
pam(df, k=2)

plot(clust)

clust@cldist

library(pdc)
# calculate codebook from sine-wave
pr<-c(1,-1,1,0,0,0)
cb <- codebook(pr,m=3, normalize=FALSE)
cb
barplot(cb,xlab="Permutation Distribution")
sample(pr, 9, replace=FALSE)


