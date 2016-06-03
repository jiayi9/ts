library(shiny)

ui <- fluidPage(
  
  tabsetPanel(
    tabPanel("MA(1)",
             uiOutput("ex1"),

             numericInput("theta1",HTML("&theta;:"),value=0.1,step=0.1),
             numericInput("int_ma_1","INTEGRATION",value=0,step=1,min=0,max=3),
             
             plotOutput("MA1")
             
             
             ),
    tabPanel("AR(1)",
             uiOutput("ex2"),
             numericInput("phi1",HTML("&phi;:"),value=0.1,step=0.1),
             
             numericInput("int_ar_1","INTEGRATION",value=0,step=1,min=0,max=3),
             plotOutput("AR1")
             
             
    ),  
    tabPanel("ARMA(1,1)",
             uiOutput("ex3"),
             numericInput("phi2",HTML("&phi;:"),value=0.1,step=0.1),
             numericInput("theta2",HTML("&theta;:"),value=0.1,step=0.1),
             
             numericInput("int_arma_11","INTEGRATION",value=0,step=1,min=0,max=3),
             plotOutput("ARMA")
             
             
    ),
    tabPanel("GARCH",
             uiOutput("ex4"),
             numericInput("garch_omega",HTML("&omega;:"),value=0.024,step=0.01),
             numericInput("garch_alpha",HTML("&alpha;:"),value=0.1,step=0.01),
             numericInput("garch_beta",HTML("&beta;:"),value=0.1,step=0.01),
             
             
             plotOutput("GARCH",height ="600px")
             
             
    )
    
  )
  
  
)