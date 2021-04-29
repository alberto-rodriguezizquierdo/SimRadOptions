#test libraries

rm(list=ls())

libInstalling <- c('dplyr','XML','tidyverse', 'SimRAD', 'seqinr')

is.installed <- function(paquete) is.element(paquete, installed.packages())

for (packages in libInstalling){
  if (!is.installed(packages)){

    install.packages(packages)
  }
  print (packages)
  library(packages,character.only=TRUE)
}


#Setting Working directories
root                  <- "C:/Users/David/Desktop/SimRadOptions/"
setwd(root)
rDirectory            <- 'R/'
myDirectoryR          <- paste0(root,rDirectory)


listFiles <- list.files(myDirectoryR, pattern = ".R")
for (rscripts in listFiles){
  source(paste0(myDirectoryR,rscripts))
}


debug(SimRADoptionsApp(root))
