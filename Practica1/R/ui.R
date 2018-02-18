# ui.R

shinyUI(fluidPage(
  uiOutput("dset"),
  tabsetPanel(
    tabPanel("Datos",
             verticalLayout(
             fileInput("fileD", "Elegir fichero de datos (.csv, .rds o .RData)",accept = c(".csv",".rds",".RData")),
             htmlOutput("selectVars"),
             DT::dataTableOutput(outputId="data"))),
    tabPanel("1. Análisis multivariante",
      tabsetPanel(
        tabPanel("1.1 Diagrama de estrellas",
          sidebarLayout(
            sidebarPanel( 
              uiOutput("sln")
              ),
            mainPanel(
              uiOutput("star_plot")
            )
        )),
        tabPanel("1.2 Matriz de gráficos de dispersión",
                 verticalLayout(
                 h3("Matriz de gráficos de dispersión:"),
                plotOutput(outputId="spm"),
                 h3("Matriz de correlación:"),
                 plotOutput(outputId="corrmat"))
                 ))),
    tabPanel("2. Análisis Univariante",
      tabsetPanel(
        tabPanel("2.1 Tabla de frecuencias",verticalLayout(uiOutput("sivar1"),DT::dataTableOutput(outputId="freq"))),
        tabPanel("2.2 Resumen estadístico",verticalLayout(uiOutput("sivar2"),htmlOutput(outputId="resum"))),
        tabPanel("2.3. Diagrama de tallo-hojas",verticalLayout(uiOutput("sivar3"),verbatimTextOutput("stemleaf"))),
        tabPanel("2.4. Histograma",verticalLayout(uiOutput("sivar4"),plotOutput(outputId="hist"))),
        tabPanel("2.5. Boxplot",verticalLayout(uiOutput("sivar5"),plotOutput(outputId="boxp")))
      )
    ))
))
