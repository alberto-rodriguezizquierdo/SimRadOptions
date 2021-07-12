#' @name restrictionSimulation
#' @param configFile
#' @return results


restrictionSimulation <- function(configFile){
  
  #defining parameters
  
  dnaseq      <- paste0(configFile$data$dataPath,'/',configFile$data$genome)#'C:/Users/David/Downloads/VITVvi_vCabSauv08_v1.1.fasta'
  
  load(file = paste0(configFile$data$dataPath,'/',configFile$data$enzyme_db))
  
  enzyme.db   <- data.frame(lapply(enzyme.db, as.character), stringsAsFactors = FALSE)
  
  min.size    <- configFile$param$min.size
  
  max.size    <- configFile$param$max.size
  
  #Starting restriction simulation
  if (isTRUE(configFile$parameters$finding_enzyme$use_finding)){
    for (enzymes in 1:nrow(enzyme.db)){
      
      row <- enzyme.db[enzymes,]
      
      sequences <- as.character(row$restriction.site.sequences)
      
      eval(parse(text=paste0('simseq', row$enzyme,'.dig <- insilico.digest(ref.DNAseq(dnaseq), cut_site_5prime1="', sequences,'", cut_site_3prime1="", verbose=TRUE)')))
      
      eval(parse(text=paste0('size.select',row$enzyme,' <- size.select (simseq', row$enzyme,'.dig,min.size = min.size, max.size = max.size, graph = FALSE, verbose= TRUE)'))) 
      
      if (eval(parse(text=paste0('!length(size.select', row$enzyme,') == 0')))){
        
        eval(parse(text=paste0('graph_generator(simseq',row$enzyme,'.dig, row$enzyme, min.size, max.size, configFile)')))
        
      }
      
      #building exiting file
      
      if (!exists('results')){
        
        #rm(list=ls())
        results <- data.frame(row)
        
        eval(parse(text=paste0('All_digestions_count <- length(simseq', row$enzyme,'.dig)')))
        
        eval(parse(text=paste0('Sel_digestions_count <- length(size.select', row$enzyme,')')))
        
        results <- cbind (results, All_digestions_count)
        
        results <- cbind (results, Sel_digestions_count)
        
        #eval(parse(text=paste0('results$Number_valid_Rest_fragments <- cbind(results, length(size.select',row$enzyme,'))'))) 
        
        
      }else{
        
        results1 <- data.frame(row)
        
        eval(parse(text=paste0('All_digestions_count <- length(simseq', row$enzyme,'.dig)')))
        
        eval(parse(text=paste0('Sel_digestions_count <- length(size.select', row$enzyme,')')))
        
        results1 <- cbind (results1, All_digestions_count)
        
        results1 <- cbind (results1, Sel_digestions_count)
        
        results <- rbind (results, results1)
        
      }
      
      #cleaning memory
      
      eval(parse(text=paste0('rm(simseq',row$enzyme,'.dig)')))
      
      eval(parse(text=paste0('rm(size.select', row$enzyme,')')))
    }
    
    results[order(-results$Sel_digestions_count),]
    
    
  }else if(isTRUE(configFile$parameters$combination$use_combination)){
    
    for (x in configFile$parameters$combination$enzyme_selection){
      
      if (exists('enzyme_selectiondb')==FALSE){
        
        enzyme_selectiondb <- filter(enzyme.db, enzyme == x)
        
      }else{
        
        enzyme_selectiondb_x <- filter(enzyme.db, enzyme == x)
        
        enzyme_selectiondb <- rbind (enzyme_selectiondb, enzyme_selectiondb_x)
        
      }
      
    }
    
    for (enzymes in configFile$parameters$combination$enzyme_selection){
      
      for (enzymes_2 in configFile$parameters$combination$enzyme_selection){
        
        if (!enzymes_2 == enzymes){
          
          row1 <- filter(enzyme_selectiondb, enzyme == enzymes)
          
          row2 <- filter(enzyme_selectiondb, enzyme == enzymes_2)
          
          sequences1 <- as.character(row1$restriction.site.sequences)
          
          sequences2 <- as.character(row2$restriction.site.sequences)
          
          eval(parse(text=paste0('simseq', row1$enzyme,'vs', row2$enzyme,'.dig <- insilico.digest(ref.DNAseq(dnaseq), cut_site_5prime1="', sequences1,'", cut_site_3prime1="", cut_site_5prime2="', sequences2,'", cut_site_3prime2="", verbose=TRUE)')))
          
          eval(parse(text=paste0('size.select',row1$enzyme,'vs',row2$enzyme,' <- size.select (simseq', row1$enzyme,'vs',row2$enzyme,'.dig,min.size = min.size, max.size = max.size, graph = FALSE, verbose= TRUE)'))) 
          
          if (eval(parse(text=paste0('!length(size.select', row1$enzyme,'vs',row2$enzyme,') == 0')))){
            
            eval(parse(text=paste0('graph_generator(simseq',row1$enzyme,'vs',row2$enzyme,'.dig, "', row1$enzyme,'vs', row2$enzyme,'", min.size, max.size, configFile)')))
            
          }
          
          
          #building exiting file
          
          if (!exists('results')){
            
            #rm(list=ls())
            results <- data.frame(row1)
            
            results <- cbind(results, data.frame(row2))
            
            eval(parse(text=paste0('All_digestions_count <- length(simseq', row1$enzyme,'vs', row2$enzyme,'.dig)')))
            
            eval(parse(text=paste0('Sel_digestions_count <- length(size.select', row1$enzyme,'vs',row2$enzyme,')')))
            
            results <- cbind (results, All_digestions_count)
            
            results <- cbind (results, Sel_digestions_count)
            
            #eval(parse(text=paste0('results$Number_valid_Rest_fragments <- cbind(results, length(size.select',row$enzyme,'))'))) 
            
            
          }else{
            
            results1 <- data.frame(row1)
            
            results1 <- cbind(results1, data.frame(row2))
            
            eval(parse(text=paste0('All_digestions_count <- length(simseq', row1$enzyme,'vs', row2$enzyme,'.dig)')))
            
            eval(parse(text=paste0('Sel_digestions_count <- length(size.select', row1$enzyme,'vs', row2$enzyme,')')))
            
            results1 <- cbind (results1, All_digestions_count)
            
            results1 <- cbind (results1, Sel_digestions_count)
            
            results <- rbind (results, results1)
            
          }
          
          #cleaning memory
          
          eval(parse(text=paste0('rm(simseq',row1$enzyme,'vs', row2$enzyme,'.dig)')))
          
          eval(parse(text=paste0('rm(size.select', row1$enzyme,'vs', row2$enzyme,')')))
          
          }
        }
      }
    
    results[order(-results$Sel_digestions_count),]
    
  }else if (isTRUE(configFile$parameters$replicate_enzyme$use_replicate)){
    
    for(nrepeat in 1:configFile$parameters$replicate_enzyme$nb_repeat){
      
      enzymes <- configFile$parameters$replicate_enzyme$enzyme_selection
      
      row <- filter(enzyme.db, enzyme == enzymes)
      
      sequences <- as.character(row$restriction.site.sequences)
      
      eval(parse(text=paste0('simseq', row$enzyme,'.dig <- insilico.digest(ref.DNAseq(dnaseq), cut_site_5prime1="', sequences,'", cut_site_3prime1="", verbose=TRUE)')))
      
      eval(parse(text=paste0('size.select',row$enzyme,' <- size.select (simseq', row$enzyme,'.dig,min.size = min.size, max.size = max.size, graph = FALSE, verbose= TRUE)'))) 
      
      if (eval(parse(text=paste0('!length(size.select', row$enzyme,') == 0')))){
        
        eval(parse(text=paste0('graph_generator(simseq',row$enzyme,'.dig, row$enzyme, min.size, max.size, configFile, nrepeat)')))
        
      }
      
      #building exiting file
      
      if (!exists('results')){
        
        #rm(list=ls())
        results <- data.frame(row)
        
        eval(parse(text=paste0('All_digestions_count <- length(simseq', row$enzyme,'.dig)')))
        
        eval(parse(text=paste0('Sel_digestions_count <- length(size.select', row$enzyme,')')))
        
        results <- cbind (results, All_digestions_count)
        
        results <- cbind (results, Sel_digestions_count)
        
        #eval(parse(text=paste0('results$Number_valid_Rest_fragments <- cbind(results, length(size.select',row$enzyme,'))'))) 
        
        
      }else{
        
        results1 <- data.frame(row)
        
        eval(parse(text=paste0('All_digestions_count <- length(simseq', row$enzyme,'.dig)')))
        
        eval(parse(text=paste0('Sel_digestions_count <- length(size.select', row$enzyme,')')))
        
        results1 <- cbind (results1, All_digestions_count)
        
        results1 <- cbind (results1, Sel_digestions_count)
        
        results <- rbind (results, results1)
        
      }
      
      #cleaning memory
      
      eval(parse(text=paste0('rm(simseq',row$enzyme,'.dig)')))
      
      eval(parse(text=paste0('rm(size.select', row$enzyme,')')))
    }
    
    results[order(-results$Sel_digestions_count),]
    
    }
  
  return(results)
}


#' @name graph_generator
#' @param sequences
#' @param name_enzyme
#' @param min.size
#' @param max.size

graph_generator <- function(sequences, name_enzyme, min.size, max.size, configFile, nrepeat){
  
  if (!exists('nrepeat')){
    
    nrepeat <- NULL
    
  }
  
  ssel <- sequences[width(sequences) < max.size & width(sequences) > min.size]

  dirOutput <- paste0(root,'output/',configFile$output$outputDir,'/', name_enzyme)
  
  if (!dir.exists(paste0(root,'output/',configFile$output$outputDir))){
    
    dir.create(paste0(root,'output/',configFile$output$outputDir))
    
    }
  
  if (!dir.exists(dirOutput)){
    
    dir.create(dirOutput)
    
  }
  
  if(!isTRUE(configFile$parameters$replicate_enzyme$use_replicate)){
    
    unlink(dirOutput, recursive = TRUE)
    
    dir.create(dirOutput)
    
  }
  
  tiff(filename=paste0(dirOutput, '/', name_enzyme,nrepeat,'_histogram.tiff'), units= 'in', width=5, height= 5, res=300)
  
  bk <- hist(width(sequences), breaks = length(sequences)/20, 
             plot = FALSE)$breaks
  
  hist(width(sequences), 
       border = "grey75", 
       col = "grey75", 
       breaks = bk,
       main = name_enzyme, 
       xlab = "Locus size (bp)", 
       ylab = "Number of loci")
  
  hist(width(ssel), 
       border = "red", 
       col = "red", 
       add = TRUE, 
       breaks = bk)
  
  text(mean(c(min.size, max.size)), 
       max(hist(width(ssel),breaks = bk, plot = FALSE)$counts), 
       pos = 4, 
       labels = paste(length(ssel)," loci between ", min.size, " and ", max.size, " bp", sep = ""), 
       col = "red", 
       cex = 0.9, 
       font = 2)
  
  dev.off()
  
  
  sequences_df <- data.frame(ssel)
  sequences_df$ID <- seq.int(nrow(sequences_df))
  
  write.fasta(as.list(sequences_df$ssel), sequences_df$ID, file.out=paste0(dirOutput, '/', name_enzyme,nrepeat, '.fasta'))
}
