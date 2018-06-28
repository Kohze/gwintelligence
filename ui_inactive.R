options(shiny.maxRequestSize=30*1024^2) 

library(shiny)

shinyUI(fluidPage(

  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "GWintelligence Analyzer: Upload RAW .txt File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")
      ),
      "Analyses 33 significant SNPs indicated in Sniekers et al. 2017",
      br(),
      "Takes a 23andMe raw file as input. Data is not stored."
      
    ),

    mainPanel(
      plotOutput(outputId = "summaryPlot"),
      dataTableOutput('table')
    )
  )
))
