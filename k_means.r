## K-means Cluster Analysis

df<-USArrests
df<-na.omit(df)
df<-scale(df)

library(factoextra) # clustering algorithms & visualization
library(tibble)
distance<-get_dist(df)
fviz_dist(distance)

k2<-kmeans(x = df, centers = 2, nstart = 25)
fviz_cluster(object = k2, data = df)

## Alternatively, you can use standard pairwise scatter plots 
## to illustrate the clusters compared to the original variables.

df %>% as.tibble() %>%
        mutate(cluster=k2$cluster, state=rownames(df)) %>%
        ggplot(aes(UrbanPop, Murder, color=factor(cluster)))+
        geom_text(aes(label=state))

## it is often advantageous to use several different values of k and 
## examine the differences in the results.

k3<-kmeans(df, centers = 3, nstart = 25)
k4<-kmeans(df, centers = 4, nstart = 25)
k5<-kmeans(df, centers = 5, nstart = 25)

library(gridExtra)

p2<-fviz_cluster(object = k2, data = df)
p3<-fviz_cluster(object = k3, data = df)
p4<-fviz_cluster(object = k4, data = df)
p5<-fviz_cluster(object = k5, data = df)

grid.arrange(p2, p3, p4, p5, ncol=2)


## Determining Optimal Clusters
### Elbow Method

set.seed(123)
fviz_nbclust(df, kmeans, method = "wss")
fviz_nbclust(df, kmeans, method = "silhouette")

final<-kmeans(x = df, centers = 4, nstart = 25)
print(final)
fviz_cluster(final,df)

