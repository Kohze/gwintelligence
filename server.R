library(shiny)
library(ggplot2)
library(magrittr)
library(dplyr)
library(stringi)
options(shiny.maxRequestSize=30*1024^2) 

shinyServer(function(input, output) {

  output$table  <- renderDataTable({
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    

    testAllele = function(x){
      col = c[x,]
      col2 = d[which(levels(droplevels(col$rsID)) == levels(droplevels(d$V1))),]
      print("----")
      print(col$Alt)
      print(col2$V4)
      result = paste0("Target: ", toString(grepl(toString(col$Alt), toString(col2$V4))), " Reference:", toString(grepl(toString(col$Ref), toString(col2$V4))))
      e$myGene[x] <<- col2$V4
      e$SNP[x] <<- levels(droplevels(col$rsID))
      e$reference[x] <<- toString(col$Ref)
      e$target[x] <<- toString(col$Alt)
      print(result)
      print("----")
      incProgress(1/n, detail = paste("Step ", x, "/", nrow(d)))
      step = step + 1
    }
    
    withProgress(message = 'Processing Genes', value = 0, {
      myGenes = read.csv(inFile$datapath, header=F, sep = '\t', skip = 20)
      sniekersSNP = read.csv("./data/sniekers-SNP-data.txt", header = T, sep = '\t')
      b = Reduce(intersect, list(sniekersSNP$rsID,myGenes$V1))
      
      c = sniekersSNP[sniekersSNP$rsID %in% b,]
      d <<- myGenes[myGenes$V1 %in% b,]
      d$V4 = tolower(d$V4)
      
      n = nrow(d)
      step = 0
      e <<- data.frame("SNP" = rep(0, nrow(d)))
      e$myGene = "none"
      e$target = "none"
      e$reference = "none"
      sapply(1:nrow(d), testAllele)
      totalTargetCounts <- e %>% 
                            rowwise() %>% 
                            mutate(Matches = grepl(target, myGene)) 
      
      totalRefCounts <- e %>% 
        rowwise() %>% 
        mutate(Matches = grepl(reference, myGene)) 
      
      totalOther = totalRefCounts$Matches + totalTargetCounts$Matches
      totalOther = sum(totalOther == 0)
      
      totalTargetCounts = sum(totalTargetCounts$Matches)
      totalRefCounts = sum(totalRefCounts$Matches)
      
      print(sum(totalTargetCounts))
      dataCollection <<- data.frame(number = c(totalTargetCounts, totalRefCounts, totalOther, 33), type = c("targets", "references", "other", "total"))
      output$summaryPlot <- renderPlot({
        df1 <- dataCollection %>% arrange(desc(number))
        
        ggplot(data=df1, aes(x=type, y=number, fill = type)) +
          geom_bar(stat="identity")+
          scale_colour_gradient2()+
          coord_flip()+
          ylim(0, nrow(e))+
          geom_text(aes(label=round(number/33, digits = 3)), nudge_y = 0.8, size=3.5) +
          scale_x_discrete(limits = df1$type)+
          theme_classic()
      })
      return(e)
    })
  })

})
