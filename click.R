library("shiny")
ui <- fluidPage(
  plotOutput("plot", click = "plot_click"),
  verbatimTextOutput("info")
)

server <- function(input, output) {
  output$plot <- renderPlot({
    par(pty='s')
    plot(c(-1,1), c(-1,1), pch=NA, asp=1)
    abline(0,1,col='red')
  }, res = 96)

  output$info <- renderPrint({
    req(input$plot_click)
    a <- round(input$plot_click$x, 2)
    b <- round(input$plot_click$y, 2)
    cat("[", a, ", ", b, "]", sep = "")
      })

  output$distPlot <- renderPlot({
    a <- round(input$plot_click$x, 2)
    b <- round(input$plot_click$y, 2)
    x <- seq(from=-1, to=1, by=0.01)
      plot(x,a + b*x)
  })
}

shinyApp(ui,server)
