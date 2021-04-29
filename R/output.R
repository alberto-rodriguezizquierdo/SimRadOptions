#' @name outputGeneration
#' @param results
#' @param root
#' @param configFile
#'
#'
#' @author Alberto Rodriguez-Izquierdo, 2021
#'


outputGeneration <- function(results,root, configFile){
  
  dirOutput         <- paste0(root, 'output/',configFile$output$outputDir)
  
  if (!dir.exists(dirOutput)){

    dir.create(dirOutput)

  }else{

    unlink(dirOutput, recursive = TRUE)

    dir.create(dirOutput)

  }
  
  write.table(results, file=dirOutput, sep=';')


}
