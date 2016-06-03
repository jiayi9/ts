library(shiny)
library(tseries)
library(forecast)
library(changepoint)

server <- function(input, output) {
  withMathJax()
  output$MA1 = renderPlot({
    
    layout( matrix(c(1,1,2,1,1,3), 2, 3, byrow=T) )
    par(oma = c(2,2,4,2) + 0.1, mar = c(4,1,1,1) + 0.1 )
    X = arima.sim(n = 1000,list(order=c(0,input$int_ma_1,1),ma=c(input$theta1)))
    
    mvalue = cpt.mean(X)
    plot(mvalue)

    acf(X)
    text(30,1,"ACF")
    pacf(X,ylim=c(min(pacf(X,plot = FALSE)$acf),1))
    text(30,1,"PACF")
    layout( matrix(1, 1, 1) )
  })

  
  output$ex1 <- renderUI({
    withMathJax(helpText('$$ 
                         X_t = \\varepsilon_t + \\theta_1 \\varepsilon_{t-1}
                         
                         $$'))
  })
  
  output$AR1 = renderPlot({
    
    layout( matrix(c(1,1,2,1,1,3), 2, 3, byrow=T) )
    par(oma = c(2,2,4,2) + 0.1, mar = c(4,1,1,1) + 0.1 )
    X = arima.sim(n = 1000,list(order=c(1,input$int_ar_1,0),ar=c(input$phi1)))
    
    mvalue = cpt.mean(X)
    plot(mvalue)
    
    acf(X)
    text(30,1,"ACF")
    pacf(X,ylim=c(min(pacf(X,plot = FALSE)$acf),1))
    text(30,1,"PACF")
    
    layout( matrix(1, 1, 1) )
  })
  
  output$ex2 <- renderUI({
    withMathJax(helpText('$$
  X_t - \\varphi X_{t-1}= \\varepsilon_t

                         $$'))
  })
  
  
  output$ARMA = renderPlot({
    
    layout( matrix(c(1,1,2,1,1,3), 2, 3, byrow=T) )
    par(oma = c(2,2,4,2) + 0.1, mar = c(4,1,1,1) + 0.1 )
    X = arima.sim(n = 1000,list(order=c(1,input$int_arma_11,1),ar=c(input$phi2),ma=c(input$theta2)))
    
    mvalue = cpt.mean(X)
    plot(mvalue)
    
    acf(X)
    text(30,1,"ACF")
    pacf(X,ylim=c(min(pacf(X,plot = FALSE)$acf),1))
    text(30,1,"PACF")
    
    layout( matrix(1, 1, 1) )
  })
  
  output$ex3 <- renderUI({
    withMathJax(helpText('$$
                         X_t - \\varphi X_{t-1}= \\varepsilon_t + \\theta_1 \\varepsilon_{t-1}
                         
                         $$'))
  })
  
  output$GARCH = renderPlot({
    
    library(fGarch)
    
    theta = c(input$garch_omega,input$garch_alpha,input$garch_beta)
    spec <- garchSpec(model = list(omega=theta[1], alpha=theta[2], beta=theta[3]),  
                      cond.dist="norm")
    
    X    <- garchSim(spec, n = 1000, extended=FALSE)
    layout( matrix(c(1,1,2,1,1,3,4,4,5,4,4,6), 4, 3, byrow=T) )
    par(oma = c(2,2,4,2) + 0.1, mar = c(4,1,1,1) + 0.1 )
    
    plot(X)
    acf(X)
    text(29,1,"ACF of  X")
    pacf(X,ylim=c(min(pacf(X,plot = FALSE)$acf),1))
    text(29,1,"PACF of  X")
    
    X2 = X^2
    
    plot(X2,col="blue")
    acf(X2)
#    text(30,1,paste("ACF",expression(X^2)))
    
    text(29,1,expression(paste("ACF of ",X^2),sep=" "))
    
    pacf(X2,ylim=c(min(pacf(X2,plot = FALSE)$acf),1))

    text(29,1,expression(paste("PACF of ",X^2),sep=" "))
    
    layout( matrix(1, 1, 1) )
    
  })

  output$ex4 <- renderUI({
    div(
    withMathJax(helpText('$$
                           \\epsilon_t \\sim\\mathcal{N}(0, \\sigma^2_t) \\
                         $$')),
    withMathJax(helpText('$$
 \\sigma_t^2=\\omega + \\alpha \\epsilon_{t-1}^2 + \\beta \\sigma_{t-1}^2                         
                         $$'))
    )
  })




}

#X_t = \\varepsilon_t + \\theta_1 \\varepsilon_{t-1} + \\cdots + \\theta_q \\varepsilon_{t-q} \\,

