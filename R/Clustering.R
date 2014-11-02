#  R package AMADA file R/Clustering.R
#  Copyright (C) 2014  Rafael S. de Souza
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

# This function displays K-means clustering  in the PCA feature space 

#' @title Clustering analysis
#' @param x A matrix or dataframe with objects in the rows and features in the columns 
#' @param method a string with one of the following values:  \code{PCA}, \code{RPCA} 
#' @return plot object 
#' @import mclust pcaPP MASS NbClust  mvtnorm fpc
#'@examples
#' require(mvtnorm)
#'  data(iris)
#'  Clustering(iris[,-5],method="PCA")
#'  
#' @usage Clustering(x, method = c("PCA","RPCA"))
#' 
#' @author Rafael S. de Souza
#' 
#' @keywords misc
#' @export 

# Clustering  Function

Clustering<-function(x,method=c("PCA","RPCA"))
{
  npctot<-ncol(x)-1
  if(npctot>=4){
    npcs<-4
  }else(npcs<-npctot)
  
  method <- match.arg(method)
  if(!is.matrix(x)&!is.data.frame(x))
    stop("Need a matrix or data frame!")
  
  if(method=="PCA"){
    PC<-PCAproj(x, npcs, scale="sd",center="mean",method="sd","sphere",maxit = 2000)
  }
  if(method=="RPCA"){
    PC<-PCAproj(x, npcs, scale="mad",center="median",method="mad","sphere",maxit = 2000)  
  }
  
  pca<-PC$scores
  vote<-NbClust(pca,method="complete",index="all")
  tt<-table(vote$Best.n[1,])
  nc.best<-as.numeric(names(tt[which.max(tt)]))
  
  
  km <- Mclust(pca, nc.best)
  plot(km,what="classification")
}  

