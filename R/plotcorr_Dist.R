#  R package GRAD file R/plotcorrDist.R
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

# This function performx MIC and Spearman  correlation  analysis into a tabular dataset

#' @title Plot correlation matrix
#' @param x a correlation  matrix 
#' @param labels a list with variables names, default = NULL
#' @return A plot of the Correlation Matrix
#' @import  corrplot squash
#'@examples
#'  data(iris)
#'  cor1<-Corr_MIC(iris[,1:4],method="pearson")
#'  plotcorrDist(cor1)
#' @usage plotcorrDist(x,labels=NULL)
#' 
#' @author Rafael S. de Souza
#' 
#' @keywords misc
#' @details The program is a simple alteration of distogram(). 
#' @export 

# Plot Correlation Matrix

plotcorrDist<-function(x,labels=NULL){
  if(!is.matrix(x)&!is.data.frame(x))
    stop("Need a matrix or data frame!")
  
  if(is.null(labels)){  
    labels <- colnames(x)
  }  
  
  
  MIC_dis <- 1 - abs(x)
  MIC_distance <- as.dist(MIC_dis)
  map1<-distogram(MIC_distance,diag = F,cex=1,colFn = bluered,labels=labels,  key = FALSE,xpos=-2,asp=12/13)
  vkey(map1, title = "d(x,y)",side = 2)
  
}
