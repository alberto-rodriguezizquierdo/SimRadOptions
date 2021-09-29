#test libraries

rm(list=ls())

libInstalling <- c('dplyr','XML','tidyverse', 'SimRAD', 'seqinr', 'data.table', 'phylotools', 'biomartr', 'stringr')

is.installed <- function(paquete) is.element(paquete, installed.packages())

for (packages in libInstalling){
  if (!is.installed(packages)){

    install.packages(packages)
  }
  print (packages)
  library(packages,character.only=TRUE)
}


#Setting Working directories
root                  <- "C:/Users/aro312/OneDrive - University of Kentucky/Documents/SimRadOptions/"
setwd(root)
rDirectory            <- 'R/'
myDirectoryR          <- paste0(root,rDirectory)


listFiles <- list.files(myDirectoryR, pattern = ".R")
for (rscripts in listFiles){
  source(paste0(myDirectoryR,rscripts))
}


debug(airenApp(root))
