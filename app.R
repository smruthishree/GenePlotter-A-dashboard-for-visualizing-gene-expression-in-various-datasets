# Loading required libraries --------------------------------------------------
options(rsconnect.max.bundle.size = 5 * 1024^3)

#library(shiny)
library(ggfortify)
library(cluster)
library(dplyr)
library(tibble)
library(ggplot2)
library(DT)
library(shinythemes)
library(rsconnect)
library(jsonlite)
library(ggpubr)
library(stringr)
library(reshape2)
library(reshape)
library(gplots)
library(tidyverse)
library(magrittr)
library(DESeq2)
library(glue)
library(tidyr)
library(BiocManager)
options(repos = BiocManager::repositories())
#library(rstatix)

#rsconnect::appDependencies() check and reinstall

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.16")

BiocManager::install("AnnotationDbi", force = TRUE)

# Data Import -----------------------------------------------------------------

rnaSeqData <- read.table("rnaSeqData_new.txt", sep = "\t", header = TRUE)
sigData <- read.table("SIG_IBDdata2.txt", sep = "\t", header = TRUE)
tileData <- read.table("TILE_IBDdata2.txt", sep = "\t", header = TRUE)

CambridgeData <- read.table("CambridgeData_new2.txt", sep = "\t", header = TRUE)
sc_segment <-read.table("cambridge_sc.txt", sep = "\t", header = TRUE)
ti_segment <-read.table("cambridge_ti.txt", sep = "\t", header = TRUE)

#rnaSeqData <- read.csv2("~/Ikmb/Data/trialbatch1.csv", sep = ",")


# Shiny UI --------------------------------------------------------------------
ui <- fluidPage(theme = shinytheme("cyborg"), #flatly
                br(),
                br(),
                br(),
                
                
                sidebarLayout(
                  sidebarPanel(width = 3,
                               
                               selectInput(inputId = "filter1",
                                           label = h4("Select Dataset"),
                                           choices = list( "",
                                                           "IBD Biopsies RNA-Seq dataset",
                                                           "Cambridge dataset"
                                                           
                                           ),
                                           multiple = FALSE,
                                           selected = NULL),
                               br(),
                               
                               
                               # selectizeInput(inputId = "filter2",
                               #                label = h4("Select Gene"),
                               #                choices = list("" = "", genes = sort(unique(rnaSeqData$var_name))),
                               #                selected = NULL,
                               #                multiple = FALSE),
                               
                               conditionalPanel(condition = "input.filter1 == 'IBD Biopsies RNA-Seq dataset'",
                                                selectizeInput(inputId = "filter2",
                                                            label = h4("Select Gene"),
                                                            choices = unique(rnaSeqData$GeneName),
                                                            selected = "NOD2"),
                                                br(),
                                                
                               ),
                               conditionalPanel(condition = "input.filter1 == 'Cambridge dataset'",
                                                selectizeInput(inputId = "filter3",
                                                            label = h4("Select Gene"),
                                                            choices = unique(CambridgeData$GeneName),
                                                            selected = NULL),
                                                br(),
                               ),
                               
                               
                               # selectizeInput(inputId = "filter2",
                               #                label = h4("Select Gene"),
                               #                choices = sort(unique(rnaSeqData$var_name)),
                               #                options = list(create = TRUE),
                               #                selected = NULL,
                               #                multiple = FALSE
                               # ),
                               # 
                               
                               
                               br(),
                               br(),
                               
                               downloadButton('downloadtable', 'Download data'),
                               br(),
                               br(),
                               br(),
                               
                               #downloadButton('downloadsig', 'Download SIG plot'),
                               conditionalPanel(
                                 condition = "input.filter1 == 'IBD Biopsies RNA-Seq dataset'",
                                 downloadButton('downloadsig', 'Download SIG plot'),
                                 br(),
                                 br(),
                                 br(),
                                 downloadButton('downloadtile', 'Download TILE plot'),
                               ),
                               conditionalPanel(
                                 condition = "input.filter1 == 'Cambridge dataset'",
                                 downloadButton('downloadsc', 'Download SC plot'),
                                 br(),
                                 br(),
                                 br(),
                                 downloadButton('downloadti', 'Download TI plot')
                                 
                               ),
                               
                               
                  ),
                  
                  
                  mainPanel(width = 9,
                            tabsetPanel(
                              tabPanel(" Data", DT::dataTableOutput("table")), #tableOutput("RNASeq_data")),
                              tabPanel("Visualization", 
                                       br(),
                                       
                                       conditionalPanel(condition = "input.filter1 == 'IBD Biopsies RNA-Seq dataset'",
                                                        
                                                        
                                                        column(
                                                          width = 5, height = 13,
                                                          plotOutput("plot1",width="450px",height="900px")
                                                          #style = "margin-right: 1px;" # add margin to the right of this column
                                                          
                                                        ),
                                                        
                                                        column(
                                                          width = 5, height = 13,
                                                          plotOutput("plot2", width="450px",height="900px")
                                                          #style = "margin-left: 1px;" # add margin to the left of this column
                                                          
                                                        ),
                                       ),
                                       conditionalPanel(condition = "input.filter1 == 'Cambridge dataset'",
                                                        column(
                                                          width = 5, height = 13,
                                                          plotOutput("plot3", width="450px",height="900px")
                                                          #style = "margin-left: 1px;" # add margin to the left of this column
                                                          
                                                        ),
                                                        column(
                                                          width = 5, height = 13,
                                                          plotOutput("plot4", width="450px",height="900px")
                                                          #style = "margin-left: 1px;" # add margin to the left of this column
                                                          
                                                        )
                                       )
                                       
                              ),
                              
                              
                              #plotOutput("plot1",width="400px",height="400px"), plotOutput("plot2",width="400px",height="400px")) #,
                            )
                            
                            
                  )
                )
)



# Shiny Server ----------------------------------------------------------------

server <- function(input, output, session) {
  
  
  
  
  #data with selected filter
  filteredData <- reactive({
    rnaSeqData %>%
      filter(GeneName == input$filter2)
  })
  
  # output$RNASeq_data <- DT::renderDataTable({
  #   filteredData() %>% select(-Var2, -Sex, -Diagnosis,-Age)
  # })
  # 
  
  Sig_Filtereddata <- reactive({
    sigData %>%
      filter(var_name == input$filter2)
  })
  
  
  
  Tile_Filtereddata <- reactive({
    tileData %>%
      filter(var_name == input$filter2)
  })
  
  
  #data with selected filter
  filteredData1 <- reactive({
    CambridgeData %>%
      filter(GeneName == input$filter3)
  })
  
  # output$CambridgeData <- DT::renderDataTable({
  #   filteredData1()
  # })
  
  SC_Filtereddata <- reactive({
    sc_segment %>%
      filter(genenames == input$filter3)
  })
  
  
  
  TI_Filtereddata <- reactive({
    ti_segment %>%
      filter(genenames == input$filter3)
  })
  
  
  # create a reactive expression to select the data based on the filter1 input
  selected_data <- reactive({
    if (input$filter1 == "IBD Biopsies RNA-Seq dataset") {
      return(filteredData())
    } else if (input$filter1 == "Cambridge dataset") {
      return(filteredData1()) 
    } else {
      return(NULL)
    }
  })
  
  # display the selected data as a table using DT
  output$table <- DT::renderDataTable({
    if (!is.null(selected_data())) {
      DT::datatable(selected_data())
    }
  })
  
  downloaddata <- reactive({
    switch(input$filter1,
           "IBD Biopsies RNA-Seq dataset" = filteredData(),
           "Cambridge dataset" = filteredData1())
  })
  
  
  observeEvent(input$filter2, {
    updateSelectizeInput(session, "filter2", selected = NULL)
  })
  
  observeEvent(input$filter3, {
    updateSelectizeInput(session, "filter3", selected = NULL)
  })
  
  
  
  
  output$plot1 <- renderPlot({
    if (input$filter2 == "") { #is.null(input$filter2 ||
      message <- "Please select a gene"
      #plot.new()
      text(x = 0.5, y = 0.5, labels = message, cex = 2)
    } else {
      ggplot(Sig_Filtereddata(), aes(x=Condition, y=value)) +
        geom_boxplot(aes(fill=Diagnosis)) + geom_point() +
        facet_wrap(~var_name, scales = "free_y") +
        xlab("") + ylab("Normalized counts") +
        scale_x_discrete(limits=c("Ctrl_SIG_nb", "DCtrl_SIG_nb", "DCtrl_SIG_b", "CD_SIG_nb", "CD_SIG_b", "UC_SIG_nb", "UC_SIG_b")) + 
        stat_compare_means(label = "p.adj", comparisons = list(c("Ctrl_SIG_nb", "DCtrl_SIG_nb"), c("Ctrl_SIG_nb", "DCtrl_SIG_b"), c("Ctrl_SIG_nb", "CD_SIG_nb"), 
                                                               c("Ctrl_SIG_nb", "CD_SIG_b"), c("Ctrl_SIG_nb", "UC_SIG_nb"), c("Ctrl_SIG_nb", "UC_SIG_b"), 
                                                               c("DCtrl_SIG_nb", "DCtrl_SIG_b"), c("CD_SIG_b", "CD_SIG_nb"), c("UC_SIG_b", "UC_SIG_nb")), method = "wilcox.test",exact=FALSE) + 
        theme_classic() +
        theme(axis.text = element_text(size = 14, color = "black"), axis.title = element_text(size = 20), axis.text.x = element_text(angle = 45, hjust = 1)) 
    }
  }) 
  
  
  output$plot2 <- renderPlot({
    if (input$filter2 == "") { #is.null(input$filter2 ||
      message <- "Please select a gene"
      #plot.new()
      text(x = 0.5, y = 0.5, labels = message, cex = 2)
    } else {
      ggplot(Tile_Filtereddata(), aes(x=Condition, y=value)) +
        geom_boxplot(aes(fill=Diagnosis)) + geom_point() +
        facet_wrap(~var_name, scales = "free_y") +
        xlab("") + ylab("Normalized counts") +
        scale_x_discrete(limits=c("Ctrl_TILE_nb", "DCtrl_TILE_nb", "DCtrl_TILE_b", "CD_TILE_nb", "CD_TILE_b", "UC_TILE_nb", "UC_TILE_b"))+
        stat_compare_means(label = "p.adj", comparisons = list(c("Ctrl_TILE_nb", "DCtrl_TILE_nb"), c("Ctrl_TILE_nb", "DCtrl_TILE_b"), c("Ctrl_TILE_nb", "CD_TILE_nb"), 
                                                               c("Ctrl_TILE_nb", "CD_TILE_b"), c("Ctrl_TILE_nb", "UC_TILE_nb"), c("Ctrl_TILE_nb", "UC_TILE_b"), 
                                                               c("DCtrl_TILE_nb", "DCtrl_TILE_b"), c("CD_TILE_nb", "CD_TILE_b"), c("UC_TILE_nb", "UC_TILE_b")), method = "wilcox.test") + 
        theme_classic() +
        theme(axis.text = element_text(size = 14, color = "black"), axis.title = element_text(size = 20), axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  
  output$plot3 <- renderPlot({
    if (input$filter3 == "") { #is.null(input$filter2 ||
      message <- "Please select a gene"
      #plot.new()
      text(x = 0.5, y = 0.5, labels = message, cex = 2)
    } else {
      ggplot(SC_Filtereddata(), aes(x=Condition, y=value)) +
        geom_boxplot(aes(fill=TYPE)) + geom_point() +
        facet_wrap(~genenames, scales = "free_y") +
        xlab("") + ylab("Normalized counts")+
        scale_x_discrete(limits=c("CTRL_SC_Uninf", "CD_SC_Uninf", "CD_SC_Inf", "UC_SC_Uninf", "UC_SC_Inf")) + 
        stat_compare_means(label = "p.adj", comparisons = list(c("CTRL_SC_Uninf", "CD_SC_Uninf"), 
                                                               c("CTRL_SC_Uninf", "CD_SC_Inf"), c("CTRL_SC_Uninf", "UC_SC_Uninf"), c("CTRL_SC_Uninf", "UC_SC_Inf"), 
                                                               c("CD_SC_Uninf", "CD_SC_Inf"), c("UC_SC_Uninf", "UC_SC_Inf")), method = "wilcox.test") +
        theme_classic() +
        theme(axis.text = element_text(size = 14, color = "black"), axis.title = element_text(size = 20), axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  
  output$plot4 <- renderPlot({
    if (input$filter3 == "") { #is.null(input$filter2 ||
      message <- "Please select a gene"
      #plot.new()
      text(x = 0.5, y = 0.5, labels = message, cex = 2)
    } else {
      ggplot(TI_Filtereddata(), aes(x=Condition, y=value)) +
        geom_boxplot(aes(fill=TYPE)) + geom_point() +
        facet_wrap(~genenames, scales = "free_y") +
        xlab("") + ylab("Normalized counts")+
        scale_x_discrete(limits=c("CTRL_TI_Uninf", "CD_TI_Uninf", "CD_TI_Inf", "UC_TI_Uninf", "UC_TI_Inf")) +
        stat_compare_means(label = "p.adj", comparisons = list(c("CTRL_TI_Uninf", "CD_TI_Uninf"),  
                                                               c("CTRL_TI_Uninf", "CD_TI_Inf"), c("CTRL_TI_Uninf", "UC_TI_Uninf"), c("CTRL_TI_Uninf", "UC_TI_Inf"), 
                                                               c("CD_TI_Uninf", "CD_TI_Inf"), c("UC_TI_Uninf", "UC_TI_Inf")), method = "wilcox.test") + 
        theme_classic() +
        theme(axis.text = element_text(size = 14, color = "black"), axis.title = element_text(size = 20), axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
  
  
  
  
  sigplot <- function(data) {
    ggplot(data, aes(x=Condition, y=value)) +
      geom_boxplot(aes(fill=Diagnosis)) + geom_point() +
      facet_wrap(~var_name, scales = "free_y") +
      xlab("") + ylab("Normalized counts") +
      scale_x_discrete(limits=c("Ctrl_SIG_nb", "DCtrl_SIG_nb", "DCtrl_SIG_b", "CD_SIG_nb", "CD_SIG_b", "UC_SIG_nb", "UC_SIG_b")) +
      stat_compare_means(label = "p.adj", comparisons = list(c("Ctrl_SIG_nb", "DCtrl_SIG_nb"), c("Ctrl_SIG_nb", "DCtrl_SIG_b"), c("Ctrl_SIG_nb", "CD_SIG_nb"),
                                                             c("Ctrl_SIG_nb", "CD_SIG_b"), c("Ctrl_SIG_nb", "UC_SIG_nb"), c("Ctrl_SIG_nb", "UC_SIG_b"),
                                                             c("DCtrl_SIG_nb", "DCtrl_SIG_b"), c("CD_SIG_b", "CD_SIG_nb"), c("UC_SIG_b", "UC_SIG_nb")), method = "wilcox.test") +
      theme_classic() +
      theme(axis.text = element_text(size = 12, color = "black"), axis.title = element_text(size = 16), axis.text.x = element_text(angle = 45, hjust = 1)) 
  }
  
  #make change for p-values
  
  tileplot <- function(data) {
    ggplot(data, aes(x=Condition, y=value)) +
      geom_boxplot(aes(fill=Diagnosis)) + geom_point() +
      facet_wrap(~var_name, scales = "free_y") +
      xlab("") + ylab("Normalized counts") +
      scale_x_discrete(limits=c("CD_TILE_b","CD_TILE_nb","Ctrl_TILE_nb", "DCtrl_TILE_b", "DCtrl_TILE_nb", "UC_TILE_b", "UC_TILE_nb")) +
      stat_compare_means(label = "p.adj", comparisons = list(c("CD_TILE_b", "CD_TILE_nb"), c("CD_TILE_b", "Ctrl_TILE_nb"), c("CD_TILE_b", "DCtrl_TILE_b"),
                                                             c("CD_TILE_b", "DCtrl_TILE_nb"), c("CD_TILE_b", "UC_TILE_b"), c("CD_TILE_b", "UC_TILE_nb"),
                                                             c("CD_TILE_nb", "Ctrl_TILE_nb"), c("DCtrl_TILE_b", "DCtrl_TILE_nb"), c("UC_TILE_b", "UC_TILE_nb")), method = "wilcox.test") +
      theme_classic() +
      theme(axis.text = element_text(size = 12, color = "black"), axis.title = element_text(size = 16), axis.text.x = element_text(angle = 45, hjust = 1)) 
  }
  
  
  scplot <- function(data) {
    ggplot(data, aes(x=Condition, y=value)) +
      geom_boxplot(aes(fill=TYPE)) + geom_point() +
      facet_wrap(~genenames, scales = "free_y") +
      xlab("") + ylab("Normalized counts") +
      scale_x_discrete(limits=c("CTRL_SC_Uninf", "CD_SC_Uninf", "CD_SC_Inf", "UC_SC_Uninf", "UC_SC_Inf")) + 
      stat_compare_means(label = "p.adj", comparisons = list(c("CTRL_SC_Uninf", "CD_SC_Uninf"), 
                                                             c("CTRL_SC_Uninf", "CD_SC_Inf"), c("CTRL_SC_Uninf", "UC_SC_Uninf"), c("CTRL_SC_Uninf", "UC_SC_Inf"), 
                                                             c("CD_SC_Uninf", "CD_SC_Inf"), c("UC_SC_Uninf", "UC_SC_Inf")), method = "wilcox.test") +
      theme_classic() +
      theme(axis.text = element_text(size = 12, color = "black"), axis.title = element_text(size = 16), axis.text.x = element_text(angle = 45, hjust = 1)) 
  }
  
  tiplot <- function(data) {
    ggplot(data, aes(x=Condition, y=value)) +
      geom_boxplot(aes(fill=TYPE)) + geom_point() +
      facet_wrap(~genenames, scales = "free_y") +
      xlab("") + ylab("Normalized counts")+
      scale_x_discrete(limits=c("CTRL_TI_Uninf", "CD_TI_Uninf", "CD_TI_Inf", "UC_TI_Uninf", "UC_TI_Inf")) +
      stat_compare_means(label = "p.adj", comparisons = list(c("CTRL_TI_Uninf", "CD_TI_Uninf"),  
                                                             c("CTRL_TI_Uninf", "CD_TI_Inf"), c("CTRL_TI_Uninf", "UC_TI_Uninf"), c("CTRL_TI_Uninf", "UC_TI_Inf"), 
                                                             c("CD_TI_Uninf", "CD_TI_Inf"), c("UC_TI_Uninf", "UC_TI_Inf")), method = "wilcox.test") + 
      theme_classic() +
      theme(axis.text = element_text(size = 12, color = "black"), axis.title = element_text(size = 16), axis.text.x = element_text(angle = 45, hjust = 1)) 
  }
  
  
  
  # output$downloadtable <- downloadHandler(filename = function() {
  #   paste("filteredData",input$filter2, ".txt")
  # },
  # 
  # content = function(file) {
  #   
  #   write.table(filteredData(), file,  sep = "\t", quote=FALSE, row.names = FALSE)
  # })
  
  output$downloadtable <- downloadHandler(
    filename = function() {
      paste(input$filter1, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(downloaddata(), file, row.names = FALSE)
    }
  )
  
  
  output$downloadsig <- downloadHandler(filename = function() {
    paste("SIGplot",input$filter2, ".pdf")
  },
  content = function(file) {
    SIGplot <- sigplot(Sig_Filtereddata())
    ggsave(file, plot = SIGplot, width=5, height =8)
  })
  
  ## download TILE Plot
  
  output$downloadtile <- downloadHandler(filename = function() {
    paste("TILEplot",input$filter2, ".pdf")
  },
  content = function(file) {
    
    # Generate SIG plot and save it to the file
    TILEplot <- tileplot(Tile_Filtereddata())  # <-- function to generate SIG plot
    ggsave(file, plot = TILEplot,  width=5, height =8)  # <-- save plot to file
  })
  
  
  output$downloadsc <- downloadHandler(filename = function() {
    paste("SCplot",input$filter3, ".pdf")
  },
  content = function(file) {
    SCplot <- scplot(SC_Filtereddata())
    ggsave(file, plot = SCplot, width=5, height =8)
  })
  
  ## download TILE Plot
  
  output$downloadti <- downloadHandler(filename = function() {
    paste("TIplot",input$filter3, ".pdf")
  },
  content = function(file) {
    
    # Generate SIG plot and save it to the file
    TIplot <- tiplot(TI_Filtereddata())  # <-- function to generate SIG plot
    ggsave(file, plot = TIplot,  width=5, height =8)  # <-- save plot to file
  })
  
}


# Run the application ---------------------------------------------------------
shinyApp(ui = ui, server = server)


