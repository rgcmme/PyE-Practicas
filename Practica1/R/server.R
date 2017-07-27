library(shiny)

shinyServer(function(input, output,session) {
  library(aplpack)
  library(e1071)
  library(tools)
  library(corrplot)
  library(DT)
  library(car)
  
  dsname <- "../data/pelis1997.2.RData"
  load(dsname)
  data_default <- pelis1997.2
  numeric_cols = sapply(1:ncol(data_default),function(x) is.numeric(data_default[,x]))
  data_default <- data_default[,numeric_cols]
  varname <- colnames(data_default)[1]
    
  rv <- reactiveValues(data0=data_default,data1=data_default,data=data_default,maxd=nrow(data_default),dsname = dsname,varname=varname)
  
  observeEvent(input$fileD,{
    
    file <- input$fileD$datapath
    rv$dsname <- input$fileD$name
    ext <- file_ext(input$fileD$name)
    if (ext == "rds"){
      rv$data <- readRDS(file)
    } else if (ext == "csv"){
      rv$data <- read.csv(file,header = T)
    } else if (ext == "RData"){
      env <- new.env()
      nm <- load(file, env)[1]
      rv$data <- env[[nm]]
    } else {
      rv$data <- NULL
    }
    numeric_cols = sapply(1:ncol(rv$data),function(x) is.numeric(rv$data[,x]))
    rv$data <- rv$data[,numeric_cols]
    rv$data0 <- rv$data
    rv$data1 <- rv$data
    rv$maxd <- nrow(rv$data)
    rv$varname <- colnames(rv$data)[1]
  })
  
  observeEvent(input$data_rows_all,{
    rv$data <- rv$data1[input$data_rows_all,]  
    rv$maxd <- nrow(rv$data)
  })
  
  
  output$selectVars <- renderUI({ 
    checkboxGroupInput(inputId = "selvars",label = "Selecionar variables a analizar",choices=colnames(rv$data0),selected = colnames(rv$data0))
  })
  
  observeEvent(input$selvars,{
    if (!(rv$varname%in%input$selvars)){
      rv$varname <- input$selvars[1]
    }
    
    rv$data <- rv$data0[,input$selvars]
    rv$data1 <- rv$data
  })
  
  observeEvent(input$svar1,{
    rv$varname <- input$svar1
  })
  observeEvent(input$svar2,{
    rv$varname <- input$svar2
  })
  observeEvent(input$svar3,{
    rv$varname <- input$svar3
  })
  observeEvent(input$svar4,{
    rv$varname <- input$svar4
  })
  observeEvent(input$svar5,{
    rv$varname <- input$svar5
  })
  
  
  # input$maxstar <- nrow(rv$data)
  output$data <- DT::renderDataTable(datatable(rv$data1,filter = 'top'))
  output$star <- renderPlot({
    if (is.null(input$n)){
      stars(rv$data, key.loc = c(12, 1.5), draw.segments=T)
    } else{
      stars(rv$data[1:input$n,], key.loc = c(12, 1.5), draw.segments=T)
    }
  })
  output$star_plot <- renderUI({
    plotOutput(outputId="star",width = "auto",height = session$clientData$output_star_width)
  })
  output$corrmat <- renderPlot({
    corrplot(cor(rv$data), method="number")})
  
  output$spm <- renderPlot({
    spm(rv$data, diagonal="histogram", smooth=FALSE)
  })
  
  output$sln <- renderUI({
    sliderInput(inputId="n",label="Número de instancias",value=rv$maxd,min=1,max=rv$maxd,step=1)
  })
  
  output$sivar1 <- renderUI({
    selectInput(inputId="svar1",label="Variable a analizar:",choices=colnames(rv$data),selected = rv$varname)
  })
  output$sivar2 <- renderUI({
    selectInput(inputId="svar2",label="Variable a analizar:",choices=colnames(rv$data),selected = rv$varname)
  })
  output$sivar3 <- renderUI({
    selectInput(inputId="svar3",label="Variable a analizar:",choices=colnames(rv$data),selected = rv$varname)
  })
  output$sivar4 <- renderUI({
    selectInput(inputId="svar4",label="Variable a analizar:",choices=colnames(rv$data),selected = rv$varname)
  })
  output$sivar5 <- renderUI({
    selectInput(inputId="svar5",label="Variable a analizar:",choices=colnames(rv$data),selected = rv$varname)
  })
  
  output$stemleaf <- renderPrint({
    stem.leaf(rv$data[,rv$varname])
  })
  output$hist <- renderPlot({
    hist(rv$data[,rv$varname], freq=T, main="", xlab=rv$varname, col=3)
  })
  output$boxp <- renderPlot({
    boxplot(rv$data[,rv$varname], col=3)
  })  
  output$resum <- renderText({
    # df <- data.frame(do.call(rbind,lapply(1:ncol(rv$data), function(x) 
    # c(summary(rv$data[,x]),round(sd(rv$data[,x]),3),round(sd(rv$data[,x])/mean(rv$data[,x]),3),round(skewness(rv$data[,x]),3),round(kurtosis(rv$data[,x]),3)))))
    # df <- data.frame(c(rv$varname,summary(rv$data[,rv$varname]),round(sd(rv$data[,rv$varname]),3),round(sd(rv$data[,rv$varname])/mean(rv$data[,rv$varname]),3),round(skewness(rv$data[,rv$varname]),3),round(kurtosis(rv$data[,rv$varname]),3)))
    summ <- summary(rv$data[,rv$varname])
    paste0("<br>","<br>",
    "<b>Mínimo</b>: ",summ[[1]],"<br>","<br>",
    "<b>1er Cuartil</b>: ",summ[[2]],"<br>","<br>",
    "<b>Mediana</b>: ",summ[[3]],"<br>","<br>",
    "<b>Media</b>: ",summ[[4]],"<br>","<br>",
    "<b>3er Cuartil</b>: ",summ[[5]],"<br>","<br>",
    "<b>Máximo</b>: ",summ[[6]],"<br>","<br>",
    "<b>Desviación típica</b>: ",round(sd(rv$data[,rv$varname]),3),"<br>","<br>",
    "<b>Desviación típica/Media</b>: ",round(sd(rv$data[,rv$varname])/mean(rv$data[,rv$varname]),3),"<br>","<br>",
    "<b>Apuntamiento</b>: ",round(skewness(rv$data[,rv$varname]),3),"<br>","<br>",
    "<b>Curtosis</b>: ",round(kurtosis(rv$data[,rv$varname])))
    # ,"Min.","1st Qu.","Median","Mean","3rd Qu.","Max.","Sd","Sd/Mean","Skewness","Kurtosis")
    # df
  })
  output$freq <- renderDataTable({
    var <- cut(rv$data[,rv$varname], breaks=7)
    df<-cbind(Freq=table(var), FreqC=cumsum(table(var)), Rel=round(prop.table(table(var)),2), RelC=round(cumsum(prop.table(table(var))),2))
    df<-cbind(Intervalo= rownames(df),df)
    df
  })
  output$dset <- renderUI({
    h3(strong('Datos seleccionados: '),rv$dsname, align = "center")
  })
  

})