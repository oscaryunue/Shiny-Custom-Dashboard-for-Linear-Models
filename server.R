library(shiny)
library(caTools)
library(ROCR)
require(corrplot)
library(effects)
library(DT)
library(canvasXpress)
library(gvlma)
library(car)
library(usdm)
library(psych)
library(plotrix)




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
    req(input$file)
    file1 <- input$file
    if(is.null(file1)){return()} 
    numerics <- names(Filter(is.numeric, as.data.frame(datasource()) ))
    logics <- getBinary(datasource())
    select <- c(numerics, logics)
    selectInput("dependent", "Dependent", select)
  })
  
  getBinary <- function(data) { 
    if(is.null(datasource())){return()} 
    x <- apply(data,2,function(x) { all(na.omit(x) %in% 0:1) })
    names(Filter(isTRUE, x))
  }

  bin <- reactive({
    if(is.null(input$dependent)){return ()}
    d <- datasource()[input$dependent]
    target <- input$level
    code <- ifelse(d == target, 1, 0)
    code <- as.logical(code) 
  })
  
  output$sb <- renderUI({
    # file1 <- input$file
    numerics <- names(Filter(is.numeric, as.data.frame(datasource()) ))
    logics <- getBinary(datasource())
    select <- c(numerics, logics)
    file1 <- datasource()[select]
    if(is.null(file1)){return()} 
    checkboxGroupInput("independent", "Independent Variables:",names(datasource())[!names(datasource()) %in% input$dependent],names(file1)[!names(file1) %in% input$dependent])    
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
  output$table = DT::renderDataTable(
    datasource(), options = list(lengthChange = FALSE)
  )
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
  
  ############################ DESCRIPTIVE ####################################### 
  
  output$describe <- renderTable(colnames = T, rownames = TRUE, na = "NA",striped = T, hover = T, bordered = T,{
    if(is.null(datasource())){return ()}
    describe(datasource())
  })
  
  output$anova <- renderPrint({
    if(is.null(datasource())){return ()}
    aov(runRegression())
  })
  
  output$durbinWatsonTest <- renderPrint({
    if(is.null(datasource())){return ()}
    durbinWatsonTest(runRegression())
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


  cols <- sapply(iris, is.factor)
  iris[,cols]
  
  
    output$model5 <- renderCanvasXpress({
      if(is.null(datasource())){return()} 
    #  isolate({
        file1 <- input$file
        full <- read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors, na.strings=c(""))
        full <- na.omit(full)
        ds <- full 
        data1 <- sapply(ds, is.numeric)
        data <- t(ds[data1])
        if(length(data1) > 2){
        factor <- names(Filter(is.factor, as.data.frame(ds) ))
        logical <- names(Filter(is.logical, as.data.frame(ds) ))
        dependent <- c(factor, logical)
        varAnnot <- as.matrix(ds[dependent[1]])
        colnames(varAnnot) <- dependent[1]
        canvasXpress(t(data), varAnnot=varAnnot, graphType='Scatter3D', colorBy=dependent[1])
        }
  #    })
      
      

    })
  
  ############################ DATA PREP ####################################### 
  
    output$diagnostic <- renderTable(colnames = T, rownames = TRUE, na = "NA",striped = T, hover = T, bordered = T,{
      if(is.null(datasource())){return()} 
      
      #diagnostic()
      invisible(gvmodel <- gvlma.lm(runRegression())) 
      invisible(r <- summary(gvmodel))
       r 
    })
    
    diagnostic <- function() {
      g <- gvlma.lm(runRegression())
     # r <- summary(g)
      return(g)
    }
  
  ######################## MODEL (FUNCTIONS) ###################################
  
  runRegression <- reactive({
    if(is.null(data())){return ()}
    df <- datasource()
    if(sapply(df[input$dependent], is.numeric) == T){
    lm(as.formula(paste(input$dependent," ~ ",paste(input$independent,collapse="+"))),data=df)
    }
    else{
      df[input$dependent] <- bin()
    lm(as.formula(paste(input$dependent," ~ ",paste(input$independent,collapse="+"))),data=df)
    }
})
    
    runAnova <- reactive({
      if(is.null(data())){return ()}
      df <- datasource()
      if(sapply(df[input$dependent], is.numeric) == T){
        aov(as.formula(paste(input$dependent," ~ ",paste(input$independent,collapse="+"))),data=df)
      }
      else{
        df[input$dependent] <- bin()
        aov(as.formula(paste(input$dependent," ~ ",paste(input$independent,collapse="+"))),data=df)
      }
    })
    
    output$aovTab <- renderPrint({
      summary(runAnova())
    })
    
    output$tukey <- renderPrint({
      TukeyHSD(x=runAnova(), input$dependent, conf.level=0.95)
    })
  
    
  output$regTab <- renderPrint({
      summary(runRegression())
  })
  
  #########################   MODEL #####################################
  

  output$vif <- renderTable(colnames = T, rownames = TRUE, na = "NA",striped = T, hover = T, bordered = T,{
    if(is.null(datasource())){return()} 
    numerics <- names(Filter(is.numeric, as.data.frame(datasource()) ))
    logics <- getBinary(datasource())
    select <- c(numerics, logics)
    vif(datasource()[select])
  })
  
  output$pplot <- renderPlot({
    if(is.null(datasource())){return()} 
    gvmodel <- gvlma.lm(runRegression()) 
    par(mfrow=c(2,2))
    plot(gvmodel, onepage = FALSE)
  })
  

  output$linearPlot <- renderPlot({
  
    if(is.null(datasource())){return ()}
    # df <- datasource()
    # if(sapply(df[input$dependent], is.logical) == T){
    #   df[input$dependent] <- bin()
    #   }
   formula <- as.formula(paste(input$dependent," ~ ",paste(input$independent,collapse="+")))    
   # formula <- as.formula(paste(datasource()[input$dependent]," ~ ",paste(datasource()[input$independent],collapse="+")))    
    # scatterplot( 
    #  iris$Sepal.Width ~ iris$Sepal.Length,
    # #  formula,
    #  # cars$dist ~ cars$speed,
    #   pch=16,
    #   col= cm.colors(), 
    #   main="Linear model",
    #   xlab = "input$dependent",
    #   ylab = "input$independent")
    # 
    scatterplot(formula, reg.line=lm,data=datasource())
    
  })
  

  output$predictionT <- renderPlot({
    slices <- c(input$slider1, 100-input$slider1) 
    lbls <- c(paste("Training Set: ", input$slider1), paste("Testing Set: ", 100-input$slider1))
    pie3D(slices,
          col=c("blue","red"),
          labels=lbls,
          explode=0.1,
          main="Prediction Test")
  }) 

  
  #######################################################################
  #########################  MACHINE LEARNING ###########################
  #######################################################################


  
  output$split <- renderPrint({
    if(is.null(data())){return ()}
    set.seed(1000)
    d <- datasource()
   # datasource()$description <- NULL
    if(sapply(d[input$dependent], is.logical) == T){
      d[input$dependent] <- bin()
    }    

    split = sample.split(d[[input$dependent]], SplitRatio = input$slider1)
    # Split up the data using subset
    train = subset(d, split==TRUE)
    test = subset(d, split==FALSE)
    trainModel <- lm(as.formula(paste(input$dependent," ~ ",paste(input$independent,collapse="+"))),data=train)
    # Predictions on the test set
    predictTest = predict(trainModel, type="response", newdata=test)
    predictTest
    # Confusion matrix with threshold of 0.5
    table(test[[input$dependent]],  predictTest > 0.5)
  }) 

  # PREDICTION MODEL
  
  output$pm <- renderPrint({
    if(is.null(data())){return ()}
    set.seed(1000)
    d <- datasource()
    if(sapply(d[input$dependent], is.numeric) == T){
      d[input$dependent] <- bin()
    }    
    split = sample.split(d[[input$dependent]], SplitRatio = input$slider1)
    train = subset(d, split==TRUE)
    test = subset(d, split==FALSE)
    trainModel <- lm(as.formula(paste(input$dependent," ~ ",paste(input$independent,collapse="+"))), data=train)
    predictTest = predict(trainModel, newdata=test)
    summary(trainModel)
  }) 
  
  output$sumPred <- renderPrint({
    if(is.null(data())){return ()}
    set.seed(1000)
    d <- datasource()
    if(sapply(d[input$dependent], is.numeric) == T){
      d[input$dependent] <- bin()
    }    
    split = sample.split(d[[input$dependent]], SplitRatio = input$slider1)
    train = subset(d, split==TRUE)
    test = subset(d, split==FALSE)
    summary(lm(as.formula(paste(input$dependent," ~ ",paste(input$independent,collapse="+"))), data=train))
    
  }) 
  
  output$acc <- renderPrint({
    if(is.null(data())){return ()}
    set.seed(1000)
    d <- datasource()
    if(sapply(d[input$dependent], is.logical) == T){
      d[input$dependent] <- bin()
    }
    split = sample.split(d[[input$dependent]], SplitRatio = input$slider1)
    train = subset(d, split==TRUE)
    test = subset(d, split==FALSE)
    trainModel <- lm(as.formula(paste(input$dependent," ~ ",paste(input$independent,collapse="+"))), data=train)
    predictTest = predict(trainModel, type="response", newdata=test)
    
    # Confusion matrix with threshold of 0.5
    cor(predictTest, test[[input$dependent]])

  }) 
  
  
  output$pm <- renderPrint({
    if(is.null(data())){return ()}
    set.seed(1000)
    d <- datasource()
    if(sapply(d[input$dependent], is.logical) == T){
      d[input$dependent] <- bin()
    }
    split = sample.split(d[[input$dependent]], SplitRatio = input$slider1)
    train = subset(d, split==TRUE)
    test = subset(d, split==FALSE)
    trainModel <- lm(as.formula(paste(input$dependent," ~ ",paste(input$independent,collapse="+"))), data=train)
    predictTest = predict(trainModel, type="response", newdata=test)
    summary(predictTest)
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
    if(is.null(data())){
  #  h5("No file"),
     h5(" ", tags$img(src='title.png', heigth=600, width=600))}
    else
      tabsetPanel(tabPanel("Config", value=1,  h5("Variables"), verbatimTextOutput("str"), plotOutput("graph")),
                  tabPanel("Data",value=2, DT::dataTableOutput('table')),
               #   tabPanel("3d",value=4, canvasXpressOutput("model5")),
                  tabPanel("Descriptive ",value=5,h5("Variables"), tableOutput("describe") ,h5("Correlation: Independent Variables"), plotOutput("corrplot"), h5("Variance: Independent Variables"), plotOutput("boxplot")), 
                #  tabPanel("Box ",value=6, plotOutput("boxplot")), 
                  tabPanel("Effects ",value=7, h5("Effects: Dependent ~ Independent variables"), plotOutput("effects"),
                           h5("ANOVA"), h5("Post hoc test: Tukey-Krammer")), 
                  tabPanel("Model",value=8,  verbatimTextOutput("regTab"), plotOutput("model1"), plotOutput("model2"), plotOutput("model3"), plotOutput("model4")),
                  #tabPanel("Prediction",value=9, verbatimTextOutput("split"),  verbatimTextOutput("acc"),  verbatimTextOutput("pm"), plotOutput("pplot")), #  plotOutput("roc")),
               tabPanel("Disgnostics", value=10, h5("Multicolinearity"),tableOutput("vif"),h5("Diagnostics: Threshold = 0.05"), tableOutput("diagnostic"), h5("Test for Autocorrelated Errors: Durbin Watson Test")  ), 
               
                tabPanel("Prediction",value=9, plotOutput("predictionT") ,h5("Accuracy"), verbatimTextOutput("acc"),h5("Base model"),  plotOutput("pplot")), 
                  tabPanel("Auto",value=11, h5("File path"),tableOutput("filedf"), h5("Observer: inspect datasource every = 0.5s"),verbatimTextOutput("fileReaderText")),
                  id = "tabselected"
                  )
  })
})