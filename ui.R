library(shiny)
shinyUI(fluidPage(
  titlePanel("DESITION MAKING: LOGISTIC MODEL"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file","Upload the file"), # fileinput() function is used to get the file upload contorl option
      helpText("Default max. file size is 5MB"),
      tags$hr(),
      h5(helpText("Select the read.table parameters below")),
                       checkboxInput(inputId = 'header', label = 'Header', value = TRUE),
                       checkboxInput(inputId = "stringAsFactors", "stringAsFactors", TRUE),
                       br(),
                       radioButtons(inputId = 'sep', label = 'Separator', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),
                       br(),
                       radioButtons(inputId = 'logOption', label = 'Formula', choices = c(Logit='logit',Probit='probit'), selected = 'logit'),
                       br(),
      conditionalPanel(condition="input.tabselected!=4",
                       uiOutput("dependent"),
                       uiOutput("factors"),
                       uiOutput("sb")
                       )
                       )
      
    ,
    mainPanel(
      uiOutput("tb")
      )
    )
  ))