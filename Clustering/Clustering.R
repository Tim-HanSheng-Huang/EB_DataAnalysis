
if(!require(cluster)){install.packages("cluster")} 
if(!require(profvis)){install.packages("profvis")}
if(!require(devtools)){install.packages("devtools")}
#devtools::install_github("kassambara/factoextra")
library(cluster)
library(profvis)
library(devtools)

if(!require(factoextra)){install.packages("factoextra")}
if(!require(fpc)){install.packages("fpc")}
if(!require(NbClust)){install.packages("NbClust")}
library(factoextra)
library(NbClust)
library(fpc)

animals<-read.csv("E:/E_Business/Homework/clustering/animals.csv")

# NA imputation
# Fill the missing value by mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
animals[,5]<-factor(animals[,5])
animals[,6]<-factor(animals[,6]) 
mode.data <- animals
mode.5 <- getmode(animals[,5])
mode.6 <- getmode(animals[,6])

na.rows <- is.na(mode.data[, 5])
animals[na.rows, 5] <- mode.5
na.rows <- is.na(mode.data[, 6]) 
animals[na.rows, 6] <- mode.6

# HAC
head(animals)
animals[,5]<-as.numeric(animals[,5])
animals[,6]<-as.numeric(animals[,6])
AnimalsScaled=as.data.frame(scale(animals[,-1]))
AnimalsScaled$Name=animals[1]
hc=hclust(dist(AnimalsScaled, method="euclidean"), method="ward.D2")
hc
plot(hc, hang = -0.01, cex = 0.7)
hc2=hclust(dist(AnimalsScaled), method="single")
plot(hc2, hang = -0.01, cex = 0.7) 
plot(hc)
rect.hclust(hc,k=4,border="red") 

# k-means
rownames(animals)=animals$X
animals$X=NULL
AnimalsScaled=as.data.frame(scale(animals))
set.seed(100)
kmFit = kmeans(AnimalsScaled, 3)
kmFit
aggregate(AnimalsScaled, by=list(cluster=kmFit$cluster),mean)
fviz_cluster(kmFit, data = AnimalsScaled)
fviz_nbclust(AnimalsScaled, kmeans, method = "wss")
geom_vline(xintercept=3, linetype = 2) 
pamFit <- pam(AnimalsScaled, 3)

pamFit$medoids
fviz_cluster(pamFit)
claraFit<-clara(AnimalsScaled,3,samples=5)
claraFit$medoids 
fviz_cluster(claraFit)
nb <- NbClust(AnimalsScaled, distance = "euclidean", min.nc = 2,max.nc = 9, method = "ward.D2", index ="all")
fviz_nbclust(nb) + theme_minimal()
km.res = kmeans(AnimalsScaled, 3) 
sil.km <- silhouette(km.res$cluster, dist(AnimalsScaled))

# silhouette analysis
si.sum <- summary(sil.km)
si.sum$clus.avg.widths  
si.sum$avg.width
si.sum$clus.sizes
fviz_silhouette(sil.km)
pam.res <- pam(AnimalsScaled, 3)
dd <- dist(AnimalsScaled, method ="euclidean")
pam_stats <- cluster.stats(dd,  pam.res$cluster)
pam_stats$within.cluster.ss
pam_stats$clus.avg.silwidths
pam_stats$dunn
pam_stats$dunn2
res.stat <- cluster.stats(dd, km.res$cluster, pam.res$cluster) 
res.stat$corrected.rand
res.stat$vi
par(mfrow = c(1,1))
pam.res$medoids
plot(pam.res, which.plots=2, main="")

