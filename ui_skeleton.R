library(shiny)
library(shinydashboard)

ui <- dashboardPage( skin = "black",
  dashboardHeader(title = "BFU+/CDU Recruitment data",
                  titleWidth = 300),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Consents & Demographics", tabName = "widgets", icon = icon("th")),
      menuItem("Anamnesis", tabName = "widgets", icon = icon("th")),
      menuItem("Clinical visits", tabName = "widgets", icon = icon("th")),
      menuItem("Endoscopy", tabName = "widgets", icon = icon("th")),
      menuItem("Biosampling", tabName = "widgets", icon = icon("th")),
      menuItem("Week 14 summary", tabName = "widgets", icon = icon("th"))
    )
  ),
  dashboardBody(
    box(width = 8)
  )
)


server <- function (input,output){

}


shinyApp(ui, server)
