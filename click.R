library(shiny)

ui <- fluidPage(
  plotOutput("plot", click = "plot_click"),
  verbatimTextOutput("info"),
  plotOutput("distPlot")
)

server <- function(input, output) {
  output$plot <- renderPlot({
    par(pty = 's')
    plot(c(-1, 1), c(-1, 1), pch = NA, asp = 1)
    abline(0, 1, col = 'red')
  }, res = 96)

  output$info <- renderPrint({
    req(input$plot_click)  # Ensure plot_click is available
    a <- round(input$plot_click$x, 2)
    b <- round(input$plot_click$y, 2)
    cat("[", a, ", ", b, "]", sep = "")
  })

  output$distPlot <- renderPlot({
    req(input$plot_click)  # Ensure plot_click is available
    a <- round(input$plot_click$x, 2)
    b <- round(input$plot_click$y, 2)
    x <- seq(from = -1, to = 1, by = 0.01)
    f <- function(x){x^4 + a*x^2 + b*x}
    x <- seq(from = -1, to=1, by=0.01)
    plot(x,f(x), type="l")
    
    jj <- polyroot(c(b,2*a,0,4))
    rr <- Re(jj[abs(Im(jj)) < 1e-9])
    if(length(rr) == 1){
        points(rr[1],f(rr[1]),col="red",pch=16)
    } else if (length(rr) == 2) {
        points(rr[1],f(rr[1]), col="red",pch=16)
        points(rr[2],f(rr[2]), col="red",pch=16)
    } else if (length(rr) == 3){
        rr <- sort(rr)
        points(rr[1],f(rr[1]), col="red",pch=16)
        points(rr[2],f(rr[2]), col="blue",pch=16)
        points(rr[3],f(rr[3]), col="red",pch=16)
    } else {
        print("should have 1, 2 or 3 real roots")
        stop()
    }
  })
}


shinyApp(ui, server)
