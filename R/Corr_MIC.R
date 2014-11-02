#  R package GRAD file R/Corr_MIC.R
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

# This function estimate Pearson,  Spearman, and   MIC   correlation  analysis into a tabular dataset

#' @title Non-linear correlation
#' @param x A matrix or dataframe with objects in the rows and features in the columns 
#' @param method a string with one of the following values:  \code{pearson}, \code{spearman},\code{MIC} 
#' @param scale  a boolean indicating if the variables should be scaled.  
#' @return Correlation Matrix
#' @import minerva MASS corrplot squash stats
#'@examples
#'  data(iris)
#'  cor1<-Corr_MIC(iris[,1:4],method="pearson")
#'  
#' @usage Corr_MIC(x, method = c("pearson","spearman", "MIC"), scale = "")
#' 
#' @author Rafael S. de Souza
#' 
#' @keywords misc
#' @details The program is a simple alteration of cor() and MIC from package Minerva.  
#' @export 
#Script to calculate Pearson, Spearman or MIC correlation function for a Fisher like matrix 

# Correlation Function
Corr_MIC<-function(x,method=c("pearson","spearman","MIC"),scale="")

{
  method <- match.arg(method)
  if(!is.matrix(x)&!is.data.frame(x))
    stop("Need a matrix or data frame!")

## pearson
 if(method=="pearson"){
   cor_data<-cor(x,method="pearson",use="complete.obs")
 }

## spearman
if(method=="spearman"){
  cor_data<-cor(x,method="spearman",use="complete.obs")
} 

 ## MIC
     if(method=="MIC"){
       cor_data<-mine(x,var.thr=1e-25)$MIC
     }
   

  return(cor_data)
}  



