#  R package GRAD file R/Nightingale.R
#  Copyright (C) 2014  COIN
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
#' @title PCA  visualization of a hierarchical clustering from a correlation matrix
#'  @description \code{Nightingale} computes Robust PCA projections of 
#' variables and plot them in polar coordinates.
#' @param corr Correlation Matrix 
#' @param npcs number of principal components 
 #' @param PCAmethod a string with one of the following values:  \code{PCA}, \code{RPCA}
#' @return ggplot object  
#'@import ggplot2 pcaPP ggthemes  reshape mvtnorm
#'@examples
#'  data(iris)
#'  require(mvtnorm)
#'  cor1<-Corr_MIC(iris[,1:4],PCAmethod="pearson")
#'  Nightingale(cor1)
#'  
#' @usage Nightingale(corr, npcs= 4,PCAmethod = c("PCA","RPCA"))
#' 
#' @author Rafael S. de Souza
#' 
#' @keywords misc
#' @details The program is a simple alteration of PCAproj() that computes a desired number of robust
#'  principal components using the grid search algorithm in a sphere. 
#' @export 
Nightingale<-function(corr,npcs=4,PCAmethod=c("PCA","RPCA")){
names<-rownames(corr)


PCAmethod <- match.arg(PCAmethod)
if(!is.matrix(corr)&!is.data.frame(corr))
  stop("Need a matrix or data frame!")

if(PCAmethod=="PCA"){
  PC<-PCAproj(corr, npcs, scale="sd",center="mean",method="sd","sphere",maxit = 2000)
}
if(PCAmethod=="RPCA"){
  PC<-PCAproj(corr, npcs, scale="mad",center="median",method="mad","sphere",maxit = 2000)  
}
#PC<-PCAproj(corr, npcs, scale=NULL,method="sd","sphere",maxit = 1000)

tree.data<-as.data.frame(abs(PC$loadings[,]))

for(i in 1:npcs){
  tree.data[,i]<-data.frame(abs(tree.data[,i])/sum(abs(tree.data[,i])))
}

if(npcs>1){
PCA_tree<-melt(PC$loadings[,])
colnames(PCA_tree)<-c("Parameter","PC","var")
PCA_tree$var<-abs(PCA_tree$var)
}else{
  PCA_tree<-melt(PC$loadings[,])
  PCA_tree$Parameter<-rownames(PCA_tree)
  PCA_tree$PC<-rep("Comp.1",nrow(PCA_tree))
  PCA_tree<-PCA_tree[,c("Parameter","PC","value")]
  colnames(PCA_tree)<-c("Parameter","PC","var")
 PCA_tree$var<-abs(PCA_tree$var)}
#PCA_tree$PC<-revalue(as.factor(PCA_tree$PC), c("Comp.1"="PC1","Comp.2"="PC2","Comp.3"= "PC3","Comp.4"="PC4","Comp.5"="PC5","Comp.6"="PC6"))

PCA_tree$Parameter<-as.factor(PCA_tree$Parameter)
levels(PCA_tree$PC)
#PCA_tree$var<-PCA_tree$var^2

Parameter=NULL# Usuless command to avoind R CDM check note
p3<-ggplot(PCA_tree,aes(x=Parameter,y=100*var,fill=Parameter,group=PC))
p4<-p3+layer(geom="bar",position="dodge",stat="identity")+
  coord_cartesian(ylim=c(0, 1))+labs(title="")+
  theme_economist_white(gray_bg = F, base_family = "serif",
                        base_size = 11, horizontal = T)+
  scale_color_stata()+
  theme(panel.grid.major.y = element_line(color="gray70",size = 0.25,linetype="dashed"),panel.grid.major.x = element_line(color="gray70",size = 0.5,linetype="dashed"),axis.line.x = element_blank(),legend.direction="horizontal",legend.position="none", 
        legend.text.align=0,legend.key = element_blank(),
        axis.text.y = element_text( hjust = 1),axis.text.x = element_text( vjust = 0),legend.title = element_blank(),
        text = element_text(size=13))+
  xlab("")+ylab("Variable contribution to PC (%)")+
  facet_wrap(~PC)+coord_polar()+guides(fill=guide_legend(nrow =6))+
  scale_x_discrete(breaks = names, labels=names)
return(p4)
}
