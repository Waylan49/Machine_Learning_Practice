## Hierarchical Cluster Analysis
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms
library(purrr)

df<-USArrests
df<-na.omit(df)
df<-scale(df)

# hclust [in stats package] and agnes [in cluster package] for 
# agglomerative hierarchical clustering (HC)

# diana [in cluster package] for divisive HC

#### Agglomerative Hierarchical Clustering
d<-get_dist(x = df)
hc1<-hclust(d, method="complete")
plot(hc1, hang=-1)


### with the agnes function you can also get the agglomerative 
### coefficient, which measures the amount of clustering structure 
### found (values closer to 1 suggest strong clustering structure).
hc2<-agnes(x = d, method="complete")

m<-c("complete", "single", "ward", "weighted")
names(m)<-c("complete", "single", "ward", "weighted")
ac<-function(method){
        agnes(x=d, method=method)$ac
}

map_dbl(m, ac)

hc3<-agnes(x=d, method="ward")
plot(hc3, hang=-1) ### pltree(hc3)

#### Divisive Hierarchical Clustering
hc4<-diana(x = df)
pltree(hc4, hang=-1)

hc5<-hclust(d, method = "ward.D2")
sub_grp<-cutree(hc5, k = 4)

USArrests<-USArrests %>% mutate(cluster=sub_grp)

## draw the dendrogram with a border around the 4 clusters
plot(hc5, hang=-1)
rect.hclust(hc5, k=4, border=2:5)

fviz_cluster(list(data=df, cluster=sub_grp))

## Determining Optimal Clusters








