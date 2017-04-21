library(shiny)
library(caTools)
library(ROCR)
require(corrplot)
library(effects)

shinyServer(function(input,output,session){

  # Read File  
  datasource <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()} 
    full <- read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors, na.strings=c(""))
    r <- na.omit(full)
    r <- updateData(r)
    r
  })
  
  updateData <- function(data) { 
    cols <- sapply(data, is.logical)
    data[,cols] <- lapply(data[,cols], as.numeric)
    data
  }
  
  data <- reactive({
    if(is.null(input$independent)){return(datasource())} 
    selected <- datasource()[input$independent]
  })
  
  ##### Filter Functions #####
  
  output$dependent <- renderUI({
    file1 <- input$file
    if(is.null(file1)){return()} 
   factors <- names(Filter(is.factor, as.data.frame(datasource()) ))
   logics <- getBinary(datasource())
   select <- c(factors, logics)
   selectInput("dependent", "Dependent Variable:", select)
  })
  
  getBinary <- function(data) { 
    x <- apply(data,2,function(x) { all(na.omit(x) %in% 0:1) })
    names(Filter(isTRUE, x))
  }
  
  output$factors <- renderUI({
    if(is.null(input$dependent)){return()}
    if(sapply(datasource()[input$dependent], is.factor) == T){
      facs <- datasource()[input$dependent]
      selectInput("level", "Target:", unique(facs))
    }
  })
  
  bin <- reactive({
    if(is.null(input$dependent)){return ()}
    d <- datasource()[input$dependent]
    target <- input$level
    code <- ifelse(d == target, 1, 0)
    code <- as.logical(code) 
  })
  
  # Independent variable input
  output$sb <- renderUI({
    file1 <- input$file
    if(is.null(file1)){return()} 
    checkboxGroupInput("independent", "Independent Variables:",names(datasource())[!names(datasource()) %in% input$dependent],names(datasource())[!names(datasource()) %in% input$dependent])    
  })
  
  # Outputs datasen in table formart
  output$filedf <- renderTable({
    if(is.null(datasource())){return ()}
    input$file
  })
  
  # Summary
  output$sum <- renderTable({
    if(is.null(datasource())){return ()}
    summary(datasource())
    
     })
  
  # Outputs Data
  output$table <- renderTable({
    if(is.null(datasource())){return ()}
    datasource()
  })
  
  # Structure
  output$str <- renderPrint({
    if(is.null(datasource())){return ()}
    else
    str(datasource())
  })
  
  # SELECTED VARIABLES
  output$content <- renderTable({
    if(is.null(input$dependet))
      #return()
    selected()
  })
  
  ############################ PLOTS ####################################### 
  
  ### Correlation Plot

  output$corrplot <- renderPlot({
    if(is.null(data())){return ()}
    nums <- names(Filter(is.numeric, as.data.frame(data()) ))
    if(is.null(nums)){return ()}
    corrplot(cor(data()[nums]), method = "number")
  })
  
  output$boxplot <- renderPlot({
    if(is.null(data())){return ()}
    nums <- names(Filter(is.numeric, as.data.frame(data()) ))
    if(is.null(nums)){return ()}
    boxplot(cor(data()[nums]), col = cm.colors(length(nums))) 
  })
  
  ### 
  output$effects <- renderPlot({
    if(is.null(datasource())){return ()}
    plot(allEffects(runRegression()))
  })
  
  ### Regression Plot
  output$graph <- renderPlot({
    if(is.null(data())){return ()}
    else
    plot(data(), col = terrain.colors(4))
  })
  
  output$model1 <- renderPlot({
    if(is.null(data())){return ()}
    else
      plot(runRegression(), which=1, col = terrain.colors(4), pch = 16)
  })
  
  output$model2 <- renderPlot({
    if(is.null(data())){return ()}
    else
      plot(runRegression(), which=2, col = terrain.colors(4), pch = 16)
  })
  
  output$model3 <- renderPlot({
    if(is.null(data())){return ()}
    else
      plot(runRegression(), which=3, col = terrain.colors(4), pch = 16)
  })
  
  output$model4 <- renderPlot({
    if(is.null(data())){return ()}
    else
      plot(runRegression(), which=4, col = terrain.colors(4), pch = 16)
  })
  
  ############################ DATA PREP ####################################### 
  

  
  ######################## MODEL (FUNCTIONS) ###################################
  
  runRegression <- reactive({
    if(is.null(data())){return ()}
    df <- datasource()
    if(sapply(df[input$dependent], is.factor) == T){
    df[input$dependent] <- bin()
    glm(as.formula(paste(input$dependent," ~ ",paste(input$independent,collapse="+"))),data=df, family = 'binomial')
    }
    else{
      glm(as.formula(paste(input$dependent," ~ ",paste(input$independent,collapse="+"))),data=df, family = 'binomial')
    }
})
  
  output$regTab <- renderPrint({
      summary(runRegression())
  })
  
  #########################  LOGISTIC MODEL ###########################
  
  output$glm <- renderPrint({
    if(is.null(data())){return ()}
    else
    logic <- data()[ , logic]
  })
  
  #######################################################################
  #########################  MACHINE LEARNING ###########################
  #######################################################################

  output$split <- renderPrint({
    if(is.null(data())){return ()}
    set.seed(1000)
    d <- datasource()
    
    if(sapply(d[input$dependent], is.factor) == T){
      d[input$dependent] <- bin()
    }    

    split = sample.split(d[[input$dependent]], SplitRatio = 0.75)
    # Split up the data using subset
    train = subset(d, split==TRUE)
    test = subset(d, split==FALSE)
    trainModel <- glm(as.formula(paste(input$dependent," ~ ",paste(input$independent,collapse="+"))),data=train, family = 'binomial')
    # Predictions on the test set
    predictTest = predict(trainModel, type="response", newdata=test)
    predictTest
    # Confusion matrix with threshold of 0.5
    table(test[[input$dependent]], t <- predictTest > 0.5)
  }) 

  # PREDICTION MODEL
  
  output$pm <- renderTable({
    if(is.null(data())){return ()}
    set.seed(1000)
    d <- datasource()
    if(sapply(d[input$dependent], is.factor) == T){
      d[input$dependent] <- bin()
    }    
    split = sample.split(d[[input$dependent]], SplitRatio = 0.75)
    train = subset(d, split==TRUE)
    test = subset(d, split==FALSE)
    trainModel <- glm(as.formula(paste(input$dependent," ~ ",paste(input$independent,collapse="+"))), data=train)
    predictTest = predict(trainModel, type="response", newdata=test)
    predictTest
  }) 
  output$acc <- renderPrint({
    if(is.null(data())){return ()}
    set.seed(1000)
    d <- datasource()
    if(sapply(d[input$dependent], is.factor) == T){
      d[input$dependent] <- bin()
    }
    split = sample.split(d[[input$dependent]], SplitRatio = 0.75)
    train = subset(d, split==TRUE)
    test = subset(d, split==FALSE)
    trainModel <- glm(as.formula(paste(input$dependent," ~ ",paste(input$independent,collapse="+"))), data=train)
    predictTest = predict(trainModel, type="response", newdata=test)
    predictTest
    # Confusion matrix with threshold of 0.5
     table(test[[input$dependent]], t <- predictTest > 0.5)
    ROCRpred = prediction(predictTest, test[[input$dependent]])
    as.numeric(performance(ROCRpred, "auc")@y.values)
  }) 
  
  ## ROC GRAPH
  
  output$roc <- renderPlot({
    # Randomly split the data into training and testing sets
    if(is.null(data())){return ()}
    set.seed(1000)
    d <- datasource()
    if(sapply(d[input$dependent], is.factor) == T){
      d[input$dependent] <- bin()
    }
    split = sample.split(d[[input$dependent]], SplitRatio = 0.75)
    train = subset(d, split==TRUE)
    test = subset(d, split==FALSE)
    trainModel <- glm(as.formula(paste(input$dependent," ~ ",paste(input$independent,collapse="+"))), data=train)
    predictTest = predict(trainModel, type="response", newdata=test)
    predictTest
    # Confusion matrix with threshold of 0.5
    table(test[[input$dependent]], t <- predictTest > 0.5)
    # Test set AUC
    ROCRpred = prediction(predictTest, test[[input$dependent]])
    as.numeric(performance(ROCRpred, "auc")@y.values)
    # Prediction function
    ROCRpred = prediction(predictTrain, qualityTrain$PoorCare)
    # Performance function
    ROCRperf = performance(ROCRpred, "tpr", "fpr")
    # Plot ROC curve
    plot(ROCRperf)
    # Add colors
    plot(ROCRperf, colorize=TRUE)
    # Add threshold labels 
    plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
    
  }) 
  
  output$sdata <- renderPrint({
    if(is.null(data())){return ()}
  })
  
  #######################################################################
  #######################################################################
  ############ LOG FILE ########
  
  logfilename <-   "./log.txt"
  sourceFile <- logfilename
 
  logwriter <- observe({
    # Invalidate this observer every second (1000 milliseconds)
    invalidateLater(1000, session)
    # Clear log file if more than 10 entries
    if (file.exists(logfilename) &&
        length(readLines(logfilename)) > 10) {
      unlink(logfilename)
    }
    # Add an entry to the log file
    cat(as.character(Sys.time()), '\n', file = logfilename,
        append = T)
  })
  
  # When the client ends the session, suspend the observer and
  # remove the log file.
  session$onSessionEnded(function() {
    logwriter$suspend()
    unlink(logfilename)
  })
  
  # ============================================================
  # This part of the code monitors the file for changes once per
  # 0.5 second (500 milliseconds).
  fileReaderData <- reactiveFileReader(500, session,
                                       logfilename, readLines)
  output$fileReaderText <- renderText({
    # Read the text, and make it a consistent number of lines so
    # that the output box doesn't grow in height.
    text <- fileReaderData()
    length(text) <- 14
    text[is.na(text)] <- ""
    paste(text, collapse = '\n')
  })
  
  
  # ============================================================
  # This part of the code monitors the file for changes once
  # every four seconds.
  
  # readFile <- function(){
  #   file1 <- input$file
  #   if(is.null(file1)){
  #     sourceFile <- "./log.txt"}
  #   else{
  #     sourceFile <- file1$datapath
  #   }
  # }
  # 
  # pollData <- reactivePoll(1000000, session,
  #                          # This function returns the time that the logfile was last
  #                          # modified
  #                          checkFunc = function() {
  #                            if (file.exists(readFile()))
  #                              if(file.info(readFile())$mtime[1] > file.info(readFile())$mtime[1] - 1000){
  #                                file.info(readFile())$mtime[1]
  #                              }
  #                            else
  #                              ""
  #                          },
  #                          # This function returns the content of the logfile
  #                          valueFunc = function() {
  #                            readLines(logfilename)
  #                          }
  # )
  # output$pollText <- renderText({
  #   # Read the text, and make it a consistent number of lines so
  #   # that the output box doesn't grow in height.
  #   text <- pollData()
  #   length(text) <- 14
  #   text[is.na(text)] <- ""
  #   paste(text, collapse = '\n')
  # })
  # 
  
  
  #######################################################################
  ###########################  OUPUTS  ##################################  
  
  # the following renderUI is used to dynamically generate the tabsets when the file is loaded. Until the file is loaded, app will not show the tabset.
  output$tb <- renderUI({
    if(is.null(data()))
    h5("No file")
     # h5("image", tags$img(src='image.png', heigth=200, width=200))
    else
      tabsetPanel(tabPanel("About", verbatimTextOutput("str"), plotOutput("graph")),
                  tabPanel("Data", tableOutput("table")),
                  tabPanel("Summary", tableOutput("sum")),
                  tabPanel("Corr ", plotOutput("corrplot")), 
                  tabPanel("Box ", plotOutput("boxplot")), 
                  tabPanel("Effects ", plotOutput("effects")), 
                  tabPanel("Regression", verbatimTextOutput("regTab"), plotOutput("model1"), plotOutput("model2"), plotOutput("model3"), plotOutput("model4")),
                  tabPanel("Prediction", verbatimTextOutput("split"), verbatimTextOutput("acc"), plotOutput("roc")),
                  tabPanel("Auto", tableOutput("filedf"), verbatimTextOutput("fileReaderText"))
                  )
  })
})