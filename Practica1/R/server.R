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
  # Store all values (numeric + text)
  data_default_allValues <- data_default
  varname_allValues <- colnames(data_default_allValues)[1]
  
  rv_allValues <- reactiveValues(data0=data_default_allValues,data1=data_default_allValues,data=data_default_allValues,maxd=nrow(data_default_allValues),dsname = dsname,varname=varname_allValues)
  
  observeEvent(input$fileD,{
    file <- input$fileD$datapath
    rv_allValues$dsname <- input$fileD$name
    
    ext <- file_ext(input$fileD$name)
    if (ext == "rds"){
      rv_allValues$data <- readRDS(file)
    } else if (ext == "csv"){
      rv_allValues$data <- read.csv(file,header = T)
      
    } else if (ext == "RData"){
      env <- new.env()
      nm <- load(file, env)[1]
      rv_allValues$data <- env[[nm]]
    } else {
      rv_allValues$data <- NULL
    }
    
    #Store all values (only for main table)
    rv_allValues$data0 <- rv_allValues$data
    rv_allValues$data1 <- rv_allValues$data
    rv_allValues$maxd <- nrow(rv_allValues$data)
    rv_allValues$varname <- colnames(rv_allValues$data)[1]
  })
  
  observeEvent(input$data_rows_all,{
    rv_allValues$data <- rv_allValues$data1[input$data_rows_all,]  
    rv_allValues$maxd <- nrow(rv_allValues$data)
  })
  
  
  output$selectVars <- renderUI({ 
    #In the main table all values will appear (numeric + text)
    checkboxGroupInput(inputId = "selvars",label = "Seleccionar variables a analizar",choices=colnames(rv_allValues$data0),selected = colnames(rv_allValues$data0))
  })
  
  observeEvent(input$selvars,{
    if (!(rv_allValues$varname%in%input$selvars)){
      rv_allValues$varname <- input$selvars[1]
    }
    
    rv_allValues$data <- rv_allValues$data0[,input$selvars]
    rv_allValues$data1 <- rv_allValues$data
  })
  
  observeEvent(input$svar1,{
    rv_allValues$varname <- input$svar1
  })
  observeEvent(input$svar2,{
    rv_allValues$varname <- input$svar2
  })
  observeEvent(input$svar3,{
    rv_allValues$varname <- input$svar3
  })
  observeEvent(input$svar4,{
    rv_allValues$varname <- input$svar4
  })
  observeEvent(input$svar5,{
    rv_allValues$varname <- input$svar5
  })
  observeEvent(input$svar6,{
    rv_allValues$varname <- input$svar6
  })
  observeEvent(input$nclasses,{
    rv_allValues$nclasses <- input$nclasses
  })
  
  output$data <- DT::renderDataTable(datatable(rv_allValues$data1,filter = 'top'))
  
  output$star <- renderPlot({
    numeric_cols <- sapply(rv_allValues$data, is.numeric)
    numeric_data <- rv_allValues$data[,numeric_cols]
    if (is.null(input$n)){
      stars(numeric_data, key.loc = c(12, 1.5), draw.segments=T)
    } else{
      stars(numeric_data[1:input$n,], key.loc = c(12, 1.5), draw.segments=T)
    }
  })
  
  output$star_plot <- renderUI({
    plotOutput(outputId="star",width = "auto",height = session$clientData$output_star_width)
  })
  
  #For the analysis only numeric values are used 
  output$corrmat <- renderPlot({
    numeric_cols <- sapply(rv_allValues$data, is.numeric)
    numeric_data <- rv_allValues$data[,numeric_cols]
    corrplot(cor(numeric_data), method="number")})
  
  output$spm <- renderPlot({
    numeric_cols <- sapply(rv_allValues$data, is.numeric)
    numeric_data <- rv_allValues$data[,numeric_cols]
    spm(numeric_data, diagonal="histogram", smooth=FALSE)
  })
  
  output$sln <- renderUI({
    sliderInput(inputId="n",label="Número de instancias",value=rv_allValues$maxd,min=1,max=rv_allValues$maxd,step=1)
  })
  
  output$sivar1 <- renderUI({
    numeric_cols <- subset(names(rv_allValues$data), sapply(rv_allValues$data, is.numeric))
    selected <- ifelse(rv_allValues$varname %in% numeric_cols, rv_allValues$varname, numeric_cols[1])
    selectInput(inputId="svar1",label="Variable a analizar:",choices=numeric_cols,selected = selected)
  })
  
  output$sivar2 <- renderUI({
    numeric_cols <- subset(names(rv_allValues$data), sapply(rv_allValues$data, is.numeric))
    selected <- ifelse(rv_allValues$varname %in% numeric_cols, rv_allValues$varname, numeric_cols[1])
    selectInput(inputId="svar2",label="Variable a analizar:",choices=numeric_cols,selected = selected)
  })
  
  output$sivar3 <- renderUI({
    numeric_cols <- subset(names(rv_allValues$data), sapply(rv_allValues$data, is.numeric))
    selected <- ifelse(rv_allValues$varname %in% numeric_cols, rv_allValues$varname, numeric_cols[1])
    selectInput(inputId="svar3",label="Variable a analizar:",choices=numeric_cols,selected = selected)
  })
  
  output$sivar4 <- renderUI({
    numeric_cols <- subset(names(rv_allValues$data), sapply(rv_allValues$data, is.numeric))
    selected <- ifelse(rv_allValues$varname %in% numeric_cols, rv_allValues$varname, numeric_cols[1])
    selectInput(inputId="svar4",label="Variable a analizar:",choices=numeric_cols,selected = selected)
  })
  
  output$sivar5 <- renderUI({
    numeric_cols <- subset(names(rv_allValues$data), sapply(rv_allValues$data, is.numeric))
    selected <- ifelse(rv_allValues$varname %in% numeric_cols, rv_allValues$varname, numeric_cols[1])
    selectInput(inputId="svar5",label="Variable a analizar:",choices=numeric_cols,selected = selected)
  })

  output$sivar6 <- renderUI({
    numeric_cols <- subset(names(rv_allValues$data), sapply(rv_allValues$data, is.numeric))
    selected <- ifelse(rv_allValues$varname %in% numeric_cols, rv_allValues$varname, numeric_cols[1])
    selectInput(inputId="svar6",label="Variable a analizar:",choices=numeric_cols,selected = selected)
  })

  output$num_classes <- renderUI({
    sliderInput(inputId="nclasses",label="Número de clases",value=50,min=2,max=rv_allValues$maxd,step=1)
  })
  
  output$stemleaf <- renderPrint({
    stem.leaf(rv_allValues$data[,rv_allValues$varname])
  })
  
  output$hist <- renderPlot({
    hist(rv_allValues$data[,rv_allValues$varname], freq=T, main="", xlab=rv_allValues$varname, col=3)
  })
  
  output$boxp <- renderPlot({
    boxplot(rv_allValues$data[,rv_allValues$varname], col=3)
  })
  
  output$resum <- renderText({
    numeric_cols <- subset(names(rv_allValues$data), sapply(rv_allValues$data, is.numeric))
    numeric_data <- rv_allValues$data[,numeric_cols]
    # df <- data.frame(do.call(rbind,lapply(1:ncol(rv_numeric$data), function(x) 
    # c(summary(rv_numeric$data[,x]),round(sd(rv_numeric$data[,x]),3),round(sd(rv_numeric$data[,x])/mean(rv_numeric$data[,x]),3),round(skewness(rv_numeric$data[,x]),3),round(kurtosis(rv_numeric$data[,x]),3)))))
    # df <- data.frame(c(rv_numeric$varname,summary(rv_numeric$data[,rv_numeric$varname]),round(sd(rv_numeric$data[,rv_numeric$varname]),3),round(sd(rv_numeric$data[,rv_numeric$varname])/mean(rv_numeric$data[,rv_numeric$varname]),3),round(skewness(rv_numeric$data[,rv_numeric$varname]),3),round(kurtosis(rv_numeric$data[,rv_numeric$varname]),3)))
    summ <- summary(numeric_data[,rv_allValues$varname])
    paste0("<br>","<br>",
           "<b>Mínimo</b>: ",summ[[1]],"<br>","<br>",
           "<b>1er Cuartil</b>: ",summ[[2]],"<br>","<br>",
           "<b>Mediana</b>: ",summ[[3]],"<br>","<br>",
           "<b>Media</b>: ",summ[[4]],"<br>","<br>",
           "<b>3er Cuartil</b>: ",summ[[5]],"<br>","<br>",
           "<b>Máximo</b>: ",summ[[6]],"<br>","<br>",
           "<b>Desviación típica</b>: ",round(sd(numeric_data[,rv_allValues$varname]),3),"<br>","<br>",
           "<b>Desviación típica/Media</b>: ",round(sd(numeric_data[,rv_allValues$varname])/mean(numeric_data[,rv_allValues$varname]),3),"<br>","<br>",
           "<b>Asimetría</b>: ",round(skewness(numeric_data[,rv_allValues$varname]),3),"<br>","<br>",
           "<b>Curtosis</b>: ",round(kurtosis(numeric_data[,rv_allValues$varname])))
    # ,"Min.","1st Qu.","Median","Mean","3rd Qu.","Max.","Sd","Sd/Mean","Skewness","Kurtosis")
    # df
  })
  
  output$freq <- renderDataTable({
    numeric_cols <- subset(names(rv_allValues$data), sapply(rv_allValues$data, is.numeric))

    if (!(rv_allValues$varname %in% numeric_cols)) {
      rv_allValues$varname <- numeric_cols[1]
    }
    
    var <- cut(rv_allValues$data[,rv_allValues$varname], breaks=7)
    df<-cbind(Freq=table(var), FreqC=cumsum(table(var)), Rel=round(prop.table(table(var)),2), RelC=round(cumsum(prop.table(table(var))),2))
    df<-cbind(Intervalo= rownames(df),df)
    df
  })
  
  output$dset <- renderUI({
    h3(strong('Datos seleccionados: '),rv_allValues$dsname, align = "center")
  })
  
  output$accum <- renderPlot({
    numeric_cols <- sapply(rv_allValues$data, is.numeric)
    numeric_data <- rv_allValues$data[,numeric_cols]

    d <- as.data.frame(table(x = cut(numeric_data[,rv_allValues$varname], breaks=rv_allValues$nclasses)))
    plot(
         cumsum(prop.table(d$Freq)),
         ylim=c(0,1),
         xlab=rv_allValues$varname,
         xaxt="n",
         ylab="Frecuencia relativa acumulada")
    axis(side=1, at=seq(1,length(d$x)), labels=levels(d$x))
    xspline(d$x,
          cumsum(prop.table(d$Freq)), col="blue", lwd=1)
  })
  
  
  
  
})
