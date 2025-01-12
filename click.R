library("shiny")

ui <- fluidPage(
  plotOutput("plot", hover = "plot_hover"),
  verbatimTextOutput("info"),
  plotOutput("distPlot")
)

server <- function(input, output) {
  output$plot <- renderPlot({
    par(pty = 's')
    plot(c(-1, 1), c(-1, 1), pch = NA, asp = 1, xlab='a', ylab='b', main='control')
    x <- seq(from=0, to=1, by=0.01)
    points(-6*x^2, -8*x^3, type="l")
    points(-6*x^2, +8*x^3, type="l")
  }, res = 96)

  output$info <- renderPrint({
    req(input$plot_hover)
    a <- round(input$plot_hover$x, 2)
    b <- round(input$plot_hover$y, 2)
    cat("[", a, ", ", b, "]", sep = "")
  })

  output$distPlot <- renderPlot({
    req(input$plot_hover)  # Ensure plot_hover is available
    a <- round(input$plot_hover$x, 2)
    b <- round(input$plot_hover$y, 2)
    x <- seq(from = -1, to = 1, by = 0.01)
    f <- function(x){x^4 + a*x^2 + b*x}
    x <- seq(from = -1, to=1, by=0.01)
    plot(x,f(x), type="l")
    
    jj <- polyroot(c(b, 2*a, 0, 4))
    rr <- Re(jj[abs(Im(jj)) < 1e-9])
    pp <- function(x, ...){points(x, f(x), pch=16, cex=4, ...)}
    
    if(length(rr) == 1){
        pp(rr[1],col="red")
    } else if (length(rr) == 2){
        pp(rr[1], col="red")
        pp(rr[2], col="red")
    } else if (length(rr) == 3){
        rr <- sort(rr)
        pp(rr[1], col="red")
        pp(rr[2], col="blue")
        pp(rr[3], col="red")
    } else {
        print("should have 1, 2 or 3 real roots")
        stop()
    }
  })
}

shinyApp(ui, server)
