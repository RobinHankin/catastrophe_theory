## Only run examples in interactive R sessions
if (interactive()) {
  options(device.ask.default = FALSE)

  ui <- fluidPage(
    sliderInput("a", "splitting variable a:",
                min = -1, max = 1, value = 0, step  = 0.01
                ),
    sliderInput("b", "other variable b:",
                min = -1, max = 1, value = 0, step  = 0.01
                ),
    plotOutput("distPlot")
  )

  # Server logic
  server <- function(input, output) {
    output$distPlot <- renderPlot({
      a <- input$a
      b <- input$b
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

  # Complete app with UI and server components
  shinyApp(ui, server)
}
