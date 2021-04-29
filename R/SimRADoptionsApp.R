#' @name SimRADoptionsApp
#' @param root
#' @import dplyr, SimRAD, seqinr, XML
#' @author Alberto Rodriguez-Izquierdo, 2021

SimRADoptionsApp <- function(root){


#  library(logging)

#  logging::loginfo('####--------Starting app---------------####',logger = logs)

  configFile <- getConfigFile(root)
  
  ########---------------Processing data------------------########

  resultsRestriction <- restrictionSimulation(configFile)
  
  
  outputGeneration(resultsRestriction,root, configFile)
  
  ####---------------------output-------------------####


  print('App finished successfully!')
}



