library(shiny)
shinyUI(fluidPage(
  titlePanel("LINEAR REGRESSION"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file","Upload the file"),
      helpText("Default: max 5MB"),
      tags$hr(),
      h5(helpText("Select the read.table parameters below")),
                       checkboxInput(inputId = 'header', label = 'Header', value = TRUE),
                       checkboxInput(inputId = "stringAsFactors", "stringAsFactors", TRUE),
                       br(), 
      # conditionalPanel(condition="input.tabselected!=10",
      #                  selectInput("dataset", "select the desired dataset", choices=ls('package:datasets'), 
      #                              selected = "mtcars")
      # ),
      # conditionalPanel(condition="input.tabselected==7",
      #                  helpText("Perform ANOVA"),
      #                  actionButton("on", label = "On"),
      #                  actionButton("off", label = "Off")
      # ),      
      conditionalPanel(condition="input.tabselected==9",
                       sliderInput("slider1", label = "Threshold", min = 50, 
                                   max = 100, value = 75)
      ),      
      br(),
                       radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),
                       br(),
      conditionalPanel(condition="input.tabselected!=4",
                       uiOutput("dependent"),
                       uiOutput("sb")
                       )
                       )
      
    ,
    mainPanel(
      uiOutput("tb")
      )
    )
  ))