#  R package GRAD file R/plotgraph.R
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
#' @title Network visualization of a hierarchical clustering from a correlation matrix
#' @param corr Correlation Matrix 
#' @return qgraph object  
#' @import qgraph squash fpc stats
#'@examples
#'  data(iris)
#'  cor1<-Corr_MIC(iris[,1:4],method="pearson")
#'  plotgraph(cor1)
#'  
#' @usage plotgraph(corr)
#' 
#' @author Rafael S. de Souza
#' 
#' @keywords misc
#' @export 
 
plotgraph<-function(corr,layout=c("spring","circular")){
  layout <- match.arg(layout)
  
  rownames(corr)<-colnames(corr)
  dissimilarity<-1-abs(corr)
  dist_matrix<-as.dist(dissimilarity)
  kp<-round(nrow(corr)-1)/3
  # Number of clusters 
  #nc <- pamk(dist_matrix,krange=1:kp,criterion="asw",critout=TRUE,usepam=T)$nc
  nc <- pamk(dist_matrix,krange=2:kp,criterion="ch",diss=T)$nc
  # Ordering clusters
  
  hc_MIC<-hclust(dist_matrix,method="average")
  clust<-cutree(hc_MIC, nc)
  
  
  
  #extract groups
  groupclu<-function(x,cluster){
    f_temp<-which(cluster==x)
  }
  
  Biggroup<-lapply(seq(1:nc),groupclu,cluster=clust)
  
  # Format data to be used as input from qgraph
  M<-as.matrix(corr)
  # Color scheme
  col_q<-colorRampPalette(c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#ffff33","#a65628","#f781bf"))
  
  qgraph(M,layout=layout,type="factorial",legend=FALSE,vsize=7.75,groups=Biggroup,
         label.scale=F,label.cex=1,edge.color = "black",labels=rownames(corr),
         transparency = F,bg=F,diag=F,color=col_q(nc),mar=c(2,2,2,2),vTrans=235,arrows=T)
  
}
