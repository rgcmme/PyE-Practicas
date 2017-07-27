# ui.R

shinyUI(fluidPage(
  tabsetPanel(
    tabPanel("Datos",
             verticalLayout(
               fileInput("fileD", "Elegir fichero de datos (.csv o .rds)",accept = c(".csv",".rds")),
               htmlOutput("selectVars1"),
               DT::dataTableOutput(outputId="data"))
    ),
    tabPanel("Estimación del modelo",
             verticalLayout(
             splitLayout(htmlOutput("selectVars"),htmlOutput("selectDist")),
             htmlOutput("param_p"),
             plotOutput("hist_plot")),
             fluidRow(
               column(5,
               verticalLayout(p(strong("Intervalo de confianza para la media")),sliderInput("nconf_mean","Nivel de confianza",0,1,0.95,step=0.01),htmlOutput("int_mean"))),
               column(5,verticalLayout(p(strong("Intervalo de confianza para la varianza")),sliderInput("nconf_var","Nivel de confianza",0,1,0.95,step=0.01),htmlOutput("int_var")))
             )
             ),
    tabPanel("Simulación y contraste de hipótesis",
             verticalLayout(
               splitLayout(htmlOutput("selectVars2"),htmlOutput("selectDist2")),
               htmlOutput("param_p2"),
               splitLayout(plotOutput("hist_plot1"),plotOutput("hist_plot2")),
               fluidRow(
                 column(5,
                        verticalLayout(p(strong("Contraste para la media")),sliderInput("nconf_mean_diff","Nivel de confianza",0,1,0.95,step=0.01),htmlOutput("int_mean_diff"))),
                 column(5,verticalLayout(p(strong("Contraste para la varianza")),sliderInput("nconf_var_coeff","Nivel de confianza",0,1,0.95,step=0.01),htmlOutput("int_var_coeff")))
               )
               
               )
     )
  )
))
