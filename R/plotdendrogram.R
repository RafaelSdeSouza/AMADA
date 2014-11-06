#  R package GRAD file R/plotdendrogram.R
#  Copyright (C) 2014  R COIN
#
#This program is free software: you can redistribute it and/or modify
#it under the terms of the GNU General Public License version 3 as published by
#the Free Software Foundation.

#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU General Public License for more details.

#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#

# This function performs  find optimal number of clusterings in the correlation matrix and plot them   as a dendrogram

#' @title Dendrogram of a hierarchical clustering from a correlation matrix
#' @param corr Correlation Matrix 
#' @param type logical 
#' @return Dendrogram colored by clusters 
#' @import ape phytools squash fpc stats 
#'@examples
#'  data(iris)
#'  cor1<-Corr_MIC(iris[,1:4],method="pearson")
#'  plotdendrogram(cor1,type="p")
#'  
#' @usage plotdendrogram(corr)
#' 
#' @author Rafael S. de Souza
#' 
#' @keywords misc
#' @export 

plotdendrogram<-function(corr,type=c("phylogram", "cladogram", "fan")){
type <- match.arg(type)
rownames(corr)<-colnames(corr)
dissimilarity<-1-abs(corr)
dist_matrix<-as.dist(dissimilarity)
kp<-round(nrow(corr)-1)/3
# Number of clusters 
nc <- pamk(dist_matrix,krange=2:kp,criterion="ch",diss=T)$nc
# Ordering clusters

hc_MIC<-hclust(dist_matrix,method="average")
clust<-cutree(hc_MIC, nc)



#extract groups
#groupclu<-function(x,cluster){
 # f_temp<-which(cluster==x)
#}

#Biggroup<-lapply(seq(1:nc),groupclu,cluster=clust)

# Dendrogram
tree<-as.phylo(hc_MIC)

# Plot configuration

labelColors = c("blue3","darkgreen","darkmagenta","orange3","khaki4","lightslategray","red3","darkcyan","violet")

distValuesPerId=data.frame(tree$tip.label,labelColors[as.data.frame(clust)$clust])


edge.tip.labels=tree$tip.label[tree$edge[,2]]
edge.rows=charmatch(edge.tip.labels,distValuesPerId[,1])

cols=distValuesPerId[edge.rows,2]
cols=as.character(cols)
cols[which(is.na(cols))]<-"black"
my.cols<-c(cols,"grey60","grey60")
#my.cols=ifelse(is.na(cols),'black',cols)

#line type
#line<-rep(1,ncol(corr))
#line[which(my.cols=="black")]<-2
plot(tree,type = type,
     edge.color = my.cols,show.node.label=TRUE,root.edge = T,no.margin = F,label.offset = 0.025,
     edge.width =5,cex=1.25,tip.color = labelColors[clust],direction="d")
#axisPhylo(2,las=1,col="black",lwd = 2)

}
