# Loading required libraries --------------------------------------------------

library(shiny)
library(ggfortify)
library(cluster)
library(dplyr)
library(tibble)
library(ggplot2)
library(shinythemes)
library(DT)

# Data Import -----------------------------------------------------------------

rnaSeqData <- read.csv2("~/Data/count.csv", sep = ",")


# Shiny UI --------------------------------------------------------------------
ui <- fluidPage(theme = shinytheme("flatly"),
                br(),
                titlePanel(h1("Gene Expression")),
                br(),
                br(),
                
                
                sidebarLayout(
                  sidebarPanel(width = 3,
                               
                               selectInput(inputId = "filter1",
                                           label = h4("Select Data"),
                                           choices = list( "",
                                                           "Biopsies RNA-Seq Data"
                                                           
                                           ),
                                           multiple = FALSE,
                                           
                               ),
                               br(),
                               
                               
                               selectInput(inputId= "filter2", 
                                           label = h4("Select Gene"), 
                                           choices = sort(unique(rnaSeqData$var_name)),
                                           multiple = FALSE,
                               ),
                               
                               br(),
                               br(),
                               
                               downloadButton('downloadtable', 'Download data'),
                               br(),
                               br(),
                               br(),
                               
                               downloadButton('downloadsig', 'Download SIG plot'),
                               
                               br(),
                               br(),
                               br(),
                               
                               downloadButton('downloadtile', 'Download TILE plot'),
                               
                               br(),
                               br(),
                               
                  ),
                  mainPanel(width = 9,
                            tabsetPanel(
                              tabPanel(" Data", tableOutput("RNASeq_data")),
                              tabPanel("Visualization")
                            )
                            # DT::dataTableOutput("mytable"),
                            
                  )
                )
)






# Shiny Server ----------------------------------------------------------------
#
server <- function(input, output, session) {
  
  filteredData <- reactive({
    rnaSeqData %>%
      filter(var_name == input$filter2)
  })
  
  
  
  output$RNASeq_data <- renderTable({
    rnaSeqData %>%
      filter(var_name == input$filter2) %>% head(20)
    
  })
  
  
  output$downloadtable <- downloadHandler(filename = function() {
    paste("filteredData",input$filter2, ".txt")
  },
  content = function(file) {
    
    write.table(filteredData(), file,  sep = "\t", quote=FALSE, row.names = FALSE)
  }
  )
  
  
  ## download SIG Plot
  
  # output$downloadsig <- downloadHandler(filename = function() {
  #   paste("SIGplot", ".pdf")
  # },
  # content = function(file) {
  #   
  #   # Generate SIG plot and save it to the file
  #   SIGplot <- generateSIGplot(filteredData())  # <-- function to generate SIG plot
  #   ggsave(file, plot = SIGplot)  # <-- save plot to file
  # }
  # )
  
  
  
}


# Run the application ---------------------------------------------------------
shinyApp(ui = ui, server = server)



