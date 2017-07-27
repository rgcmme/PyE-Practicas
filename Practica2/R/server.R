library(shiny)

shinyServer(function(input, output) {
  library(aplpack)
  library(e1071)
  library(tools)
  library(corrplot)
  library(DT)
  library(car)
  library(MASS)
  library(TeachingDemos)
  

  load("../data/datos_R.RData")
  data_default <- datos
  numeric_cols <- sapply(1:ncol(data_default),function(x) is.numeric(data_default[,x]))
  data_default <- data_default[,numeric_cols]
  data <- data_default[,1]
  distname <- 'Normal'
  varname <- colnames(data_default)[1]
  fd <- fitdistr(data,"normal")
  data2=rnorm(length(data),mean=fd$estimate[1],sd=fd$estimate[2])
  
  rv <- reactiveValues(data0=data_default,data=data,varname = varname,distname=distname,fd=fd)
  
  get_df_var<- function(data,varname){
    lst <- list(data[,varname])
    names(lst) <- varname
    return(as.data.frame(lst))
  }
  output$data <- DT::renderDataTable(datatable(get_df_var(rv$data0,rv$varname),filter = 'top'))
  
  observeEvent(input$fileD,{
    
    file <- input$fileD$datapath
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
    rv$varname <- colnames(rv$data0)[1]
    rv$data <- rv$data[,1]
    resc <- compute_fd(rv$data,rv$distname)
    rv$fd <- resc$fd
    rv$data2 <- resc$data2
  })

  # Fit distribution
  compute_fd<- function(data,distname){
    if (distname=="Exponencial"){
      fd <- tryCatch(fitdistr(data,"exponential"),error=function(e){NULL})
      data2=rexp(length(data),rate=fd$estimate[1])
    } else if (distname=="Normal"){
      fd <- tryCatch(fitdistr(data,"normal"),error=function(e){NULL})
      data2=rnorm(length(data),mean=fd$estimate[1],sd=fd$estimate[2])
    } else if (distname=="Gamma"){
      fd <- tryCatch(fitdistr(data,"gamma"),error=function(e){NULL})
      if (!is.null(fd)){
        data2=rgamma(length(data),shape=fd$estimate[1],rate=fd$estimate[2])
      } else{
        showModal(modalDialog(
          title = "Error en ajuste de datos a la distribución",
          paste0("No se pudo ajustar la discribución Gamma a la variable ",rv$varname)
        ))
        data2 = c()
      }
    }
    return(list(fd=fd,data2=data2))
  }
  
  output$selectVars <- renderUI({ 
    radioButtons(inputId = "selvars",label = "Selecionar variable a analizar",choices=colnames(rv$data0),selected = rv$varname)
    #checkboxGroupInput(inputId = "selvars",label = "Selecionar variables a analizar",choices=colnames(rv$data0),selected = colnames(rv$data0))
  })
  
  output$selectVars1 <- renderUI({ 
    radioButtons(inputId = "selvars1",label = "Selecionar variable a analizar",choices=colnames(rv$data0),selected = rv$varname)
    #checkboxGroupInput(inputId = "selvars",label = "Selecionar variables a analizar",choices=colnames(rv$data0),selected = colnames(rv$data0))
  })
  
  output$selectVars2 <- renderUI({ 
    radioButtons(inputId = "selvars2",label = "Selecionar variable a analizar",choices=colnames(rv$data0),selected = rv$varname)
    #checkboxGroupInput(inputId = "selvars",label = "Selecionar variables a analizar",choices=colnames(rv$data0),selected = colnames(rv$data0))
  })
  

  
  observeEvent(input$data_rows_all,{
    rv$data <- rv$data0[input$data_rows_all,rv$varname]  
    resc <- compute_fd(rv$data,rv$distname)
    rv$fd <- resc$fd
    rv$data2 <- resc$data2
  })
  
  observeEvent(input$selvars,{
    rv$data <- rv$data0[,input$selvars]
    rv$varname <- input$selvars[1]
    rv$data1 <- rv$data
    resc <- compute_fd(rv$data,rv$distname)
    rv$fd <- resc$fd
    rv$data2 <- resc$data2
  })
  observeEvent(input$selvars1,{
    rv$data <- rv$data0[,input$selvars1]
    rv$varname <- input$selvars1[1]
    rv$data1 <- rv$data
    resc <- compute_fd(rv$data,rv$distname)
    rv$fd <- resc$fd
    rv$data2 <- resc$data2
  })
  
  observeEvent(input$selvars2,{
    rv$data <- rv$data0[,input$selvars2]
    rv$varname <- input$selvars2[1]
    rv$data1 <- rv$data
    resc <- compute_fd(rv$data,rv$distname)
    rv$fd <- resc$fd
    rv$data2 <- resc$data2
  })
  
  
  observeEvent(input$dist,{
    rv$distname <- input$dist
    resc <- compute_fd(rv$data,rv$distname)
    rv$fd <- resc$fd
    rv$data2 <- resc$data2
  })

  observeEvent(input$dist2,{
    rv$distname <- input$dist2
    resc <- compute_fd(rv$data,rv$distname)
    rv$fd <- resc$fd
    rv$data2 <- resc$data2
  })
  
  output$selectDist <- renderUI({ 
    radioButtons(inputId = "dist",label = "Selecionar distribución",choices=c("Normal","Gamma","Exponencial"), selected = rv$distname)
    #checkboxGroupInput(inputId = "selvars",label = "Selecionar variables a analizar",choices=colnames(rv$data0),selected = colnames(rv$data0))
  })
  output$selectDist2 <- renderUI({ 
    radioButtons(inputId = "dist2",label = "Selecionar distribución",choices=c("Normal","Gamma","Exponencial"),selected = rv$distname)
  })
  
  
  output$param_p <- renderText({
    
    if (is.null(rv$fd)){
      return(NULL)
    }
    
    if (is.numeric(rv$data)){
      res <- "Unknown distribution"
      if (rv$distname=="Exponencial"){
        res <- paste0("<br>Distribución <b>",rv$distname,"</b> con parámetro <b>\u03BB</b>=",round(1/rv$fd$estimate,5))
        test <- ks.test(rv$data,"pexp",rv$fd$estimate,exact=TRUE)
        res <- paste0(res,"<br><br> <b>H<sub>0</sub></b> = Los datos provienen de una distribución <b>",rv$distname,"</b>")
        res <- paste0(res,"<br><br> <b>p-valor</b> Kolmogorov-Smirnov (<b>H<sub>0</b>) = ",format(test$p.value))
      } else if (rv$distname=="Normal"){
        res <- paste0("Distribución <b>",rv$distname,"</b> con parámetros <b>\u03BC</b>=",round(rv$fd$estimate[1],5)," y <b>\u03C3</b>=",round(rv$fd$estimate[2],5))
        test <- ks.test(rv$data,"pnorm",rv$fd$estimate[1],rv$fd$estimate[2],exact=TRUE)
        res <- paste0(res,"<br><br> <b>H<sub>0</sub></b> = Los datos provienen de una distribución <b>",rv$distname,"</b>")
        res <- paste0(res,"<br><br> <b>p-valor</b> Kolmogorov-Smirnov (<b>H<sub>0</b>) = ",format(test$p.value))
      } else if (rv$distname=="Gamma"){
        res <- paste0("Distribución <b>",rv$distname,"</b> con parámetros <b>\u03BB</b>=",round(1/rv$fd$estimate[2],5)," y <b>p</b>=",round(rv$fd$estimate[1],5))
        test <- ks.test(rv$data,"pnorm",rv$fd$estimate[1],rv$fd$estimate[2],exact=TRUE)
        res <- paste0(res,"<br><br> <b>H<sub>0</sub></b> = Los datos provienen de una distribución <b>",rv$distname,"</b>")
        res <- paste0(res,"<br><br> <b>p-valor</b> Kolmogorov-Smirnov (<b>H<sub>0</b>) = ",format(test$p.value))
      }
    } else{
      res <- "The selected variable must be numeric"
    }
    res
  })
  
  output$param_p2 <- renderText({
    if (is.null(rv$fd)){
      return(NULL)
    }
    
    if (is.numeric(rv$data)){
      res <- "Unknown distribution"
      if (rv$distname=="Exponencial"){
        res <- paste0("<br>Distribución <b>",rv$distname,"</b> con parámetro <b>\u03BB</b>=",round(1/rv$fd$estimate,5))
      } else if (rv$distname=="Normal"){
        res <- paste0("Distribución <b>",rv$distname,"</b> con parámetros <b>\u03BC</b>=",round(rv$fd$estimate[1],5)," y <b>\u03C3</b>=",round(rv$fd$estimate[2],5))
      } else if (rv$distname=="Gamma"){
        res <- paste0("Distribución <b>",rv$distname,"</b> con parámetros <b>\u03BB</b>=",round(1/rv$fd$estimate[2],5)," y <b>p</b>=",round(rv$fd$estimate[1],5))
      }
    } else{
      res <- "The selected variable must be numeric"
    }
    res
  })
  
  
  output$hist_plot <- renderPlot({
    if (is.null(rv$fd)){
      return(NULL)
    }
    if (rv$distname=="Gamma"){
      fd <- rv$fd
      hist(rv$data, freq=F, main="Histograma de la variable", xlab=rv$varname,ylab = "Frecuencia")
      ## Superpongo la función de densidad del modelo elegido, en la función curve uso dnorm, dgamma, dexp según sea mi modelo.
      density <- function(x){dgamma(x,fd$estimate[1],fd$estimate[2])}
      curve(density,from=0,to=max(rv$data)*2,add=TRUE,col=2)
      legend("topright", legend=c("Gamma"), col = 2, lty = 1)
    }
    else if (rv$distname=="Normal"){
      fd <- rv$fd
      hist(rv$data, freq=F, main="Histograma de la variable", xlab=rv$varname,ylab = "Frecuencia")
      ## Superpongo la función de densidad del modelo elegido, en la función curve uso dnorm, dgamma, dexp según sea mi modelo.
      density <- function(x){dnorm(x,fd$estimate[1],fd$estimate[2])}
      curve(density,from=0,to=max(rv$data)*2,add=TRUE,col=3)
      legend("topright", legend=c("Normal"), col = 3, lty = 1)
    } else if (rv$distname=="Exponencial"){
      fd <- rv$fd
      hist(rv$data, freq=F, main="Histograma de la variable", xlab=rv$varname,ylab = "Frecuencia")
      ## Superpongo la función de densidad del modelo elegido, en la función curve uso dnorm, dgamma, dexp según sea mi modelo.
      density <- function(x){dexp(x,fd$estimate[1])}
      curve(density,from=0,to=max(rv$data)*2,add=TRUE,col=4)
      legend("topright", legend=c("Exponencial"), col = 4, lty = 1)
    }
  })
  
  output$hist_plot1 <- renderPlot({
    if (is.null(rv$fd)){
      return(NULL)
    }
    
    hist(rv$data, freq=F, main="Histograma: datos originales", xlab=rv$varname,ylab = "Frecuencia")
  })
  output$hist_plot2 <- renderPlot({
    if (is.null(rv$fd)){
      return(NULL)
    }
    
    hist(rv$data2, freq=F, main="Histograma: datos generados", xlab=rv$varname,ylab = "Frecuencia")
  })
  
  
  
  
  output$int_mean <- renderText({
    if (is.null(rv$fd)){
      return(NULL)
    }
    
    res <- t.test(rv$data, conf.level=input$nconf_mean)
    paste0("<b>Intervalo</b>: [",round(res$conf.int[[1]],2),",",round(res$conf.int[[2]],2),"]<br><br>")
  })
  output$int_var <- renderText({
    if (is.null(rv$fd)){
      return(NULL)
    }
    
    res <- sigma.test(rv$data, conf.level=input$nconf_var)
    paste0("<b>Intervalo</b>: [",round(res$conf.int[[1]],2),",",round(res$conf.int[[2]],2),"]<br><br>")
  })

  
  output$int_mean_diff <- renderText({
    if (is.null(rv$fd)){
      return(NULL)
    }
    
    res <- t.test(rv$data,rv$data2, mu=0, alternative="two.sided",paired=FALSE,var.equal=TRUE,conf.level=input$nconf_mean_diff)
    res2 <- paste0("<b>p-valor</b>: ",round(res$p.value,2),"")
    res2 <- paste0(res2, "<br><b>Intervalo</b>: [",round(res$conf.int[[1]],2),",",round(res$conf.int[[2]],2),"]")
    res2
  })
  output$int_var_coeff <- renderText({
    if (is.null(rv$fd)){
      return(NULL)
    }
    
    res <- var.test(rv$data,rv$data2, ratio=1, alternative="two.sided",conf.level=input$nconf_var_coeff)
    res2 <- paste0("<b>p-valor</b>: ",round(res$p.value,2),"")
    res2 <- paste0(res2, "<br><b>Intervalo</b>: [",round(res$conf.int[[1]],2),",",round(res$conf.int[[2]],2),"]")
    res2
    
  })
  
  
  
})