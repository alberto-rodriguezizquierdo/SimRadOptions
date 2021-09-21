#' @name SimRADoptionsApp
#' @param root
#' @import dplyr, SimRAD, seqinr, XML
#' @author Alberto Rodriguez-Izquierdo, 2021

SimRADoptionsApp <- function(root){

  
  configFile <- getConfigFile(root)

  #########---------Restriction Enzyme simulation----------#########
  
  resultsRestriction <- restrictionSimulation(dnaseq = paste0(configFile$data$dataPath,'/',configFile$data$genome), 
                                              enzyme_db = paste0(configFile$data$dataPath,'/',configFile$data$enzyme_db), 
                                              min.size = configFile$param$min.size, 
                                              max.size = configFile$param$max.size, 
                                              use_finding = configFile$parameters$finding_enzyme$use_finding, 
                                              use_combination = configFile$parameters$combination$use_combination,
                                              enzyme_selection = configFile$parameters$combination$enzyme_selection,
                                              use_replicate = configFile$parameters$replicate_enzyme$use_replicate,
                                              nb_repeat=configFile$parameters$replicate_enzyme$nb_repeat,
                                              use_output = configFile$output$use_output,
                                              outputDir = configFile$output$outputDir,
                                              wd=root)

  
    
  if (isTRUE(configFile$parameters$random_genome_fragmentation$use_fragmentation)){
    
    for(x in 1:configFile$parameters$random_genome_fragmentation$nb_repeat){
      
      eval(parse(text=paste0('results', x,' <- randomGenomeFragmentation(configFile$data$genome, configFile$parameters$min_size, configFile$parameters$max_size, configFile$parameters$random_genome_fragmentation$nb_fragments)')))
      
      eval(parse(text=paste0("write.fasta(as.list(results",x,"$fragmentRes), results",x,"$V1, file.out=paste0(root,'/output/random_genome_fragmentation/results',x,'.fasta'))")))
      
      eval(parse(text=paste0('rm(results',x,')')))
      
    }
    
  }
  
  if(isTRUE(configFile$parameters$calculatePosition$use_calculate)){
    
    calculatePositionFragment(count_dir=configFile$parameters$calculatePosition$outputPath,
                              alignment_path=configFile$parameters$calculatePosition$alignment_path,
                              gffPath=configFile$parameters$calculatePosition$gffFile,
                              category=configFile$parameters$calculatePosition$category)
    
  }
  
  ########---------------Processing data------------------########

  resultsRestriction <- restrictionSimulation(configFile)
  
  
  outputGeneration(resultsRestriction,root, configFile)
  
  ####---------------------output-------------------####


  print('App finished successfully!')
}



