\name{CosmoGrad}
\alias{CosmoGrad}
\docType{package}
\title{
Correlation analysis of multidimensional datasets 
}
\description{
User-friedly interface to perfom linear and non-linear correlation analysis in multidimensional datasets. The code makes use of hierarchical clustering models to find sets of variables highly correlated. The code additionally provides a Shiny application providing a simple user interface. 
}
\details{
\tabular{ll}{
Package: \tab CosmoGrad\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2014-06-07\cr
License: \tab GPL-3\cr
}
The CosmoGrad package aims to provide an user-friendly interface to enable the correlation analysis of multidimensional datasets. This particular usefull for a fast exploration and visualization of astronomical catalogs.  The present version Pearson, Spearman and Maximum Information correlation analysis. 

The package includes also a series of  plotting functions to facilitate the visual analysis of the dataset.  Four examples of the implemented visual tests  can be seen in the figures bellow:

%\if{html}{\figure{merged-Diag-Plots.jpg}{merged-Diag-Plots}}
%\if{latex}{\figure{merged-Diag-Plots.pdf}{options: width=5.3in}}

Additionally, the code is also accompanied by a \pkg{Shiny} application that can be hosted locally or deployed by the user at a webserver. This application allows the user to run the correlation analysis, to configure many parameters of the code visually and experiment with the results. It also allows the user  to upload its own data files (the expected format can be found at the application's help tab). 
%A screenshot of this application can be seen in the next figure.

%\if{html}{\figure{CosmoPhotoz-Shiny.jpg}{CosmoPhotoz-Shiny-Plots}}
%\if{latex}{\figure{CosmoPhotoz-Shiny.pdf}{options: width=5.3in}}

%To run the graphical interface locally, it is sufficient to call: 

%\code{runApp(paste(find.package("CosmoPhotoz"),"/glmPhotoZ-2/",sep=""))}

%Finally, a short tutorial on how to use the package can be found at: 

%--------------- http://rafaelsdesouza.github.io/CosmoPhotoz/ -----------------

}
\author{
Rafael S. de Souza

Maintainer: Rafael S. de Souza <rafael.2706@gmail.com>
}

\keyword{ package }
\seealso{
  \code{\link{plotgraph}}, 
  \code{\link{sunburstPCA}}, 
  \code{\link{Corr_MIC}}
}
\examples{
\dontrun{
# Load the data
data(iris)

# Run the analysis
dat1<-Corr_MIC(iris[,-5],method="Pearson")

# Create a correlation matrix plot
plotcorrDist(data1)
}
}
