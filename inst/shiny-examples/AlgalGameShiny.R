library(shiny)
library(AlgalGame)
library(ggplot2)

parms_list <- AlgalGame::show_parms()
parms_name <- setNames(parms_list[, 2], parms_list[, 1])

# grid_name  <- c(Ttot = 2, dt = 1e-3, L = 3, N = 60)

# this function used to generate numericInput paramter list
makelist <- function(i, obj, min=NA, max=NA, step=NA, width=NULL) {
  list(inputId=names(obj[i]), label=names(obj[i]),
       value=unname(obj[i]), min=min, max=max, step=step,
       width=width)
}

parms_input <- lapply(1:length(parms_name), makelist, obj = parms_name, width = 100)
# parms_grid  <- lapply(1:length(grid_name), makelist, obj = grid_name, width = 100)

server <- function(input, output) {
  pp <- eventReactive(input$run, {
      # algal running function starts from here
      # qplot(input$Rin, input$h)
      run_model(
        Ttot = input$Ttot,
        dt = input$dt,
        L = input$L,
        N = input$N,
        inputs = as.list(input)
      )$gplot
  })

  output$AG <- renderPlot({
    pp()
  })

}


ui <- fluidPage(
  headerPanel("Algal Game"),
  sidebarLayout(
    sidebarPanel(
      h4("Click `Run model` once all parameters are set"),
      actionButton("run", "Run model"),
      h4("Grid setting"),
      numericInput("Ttot", "Ttot", 2, width = 100),
      numericInput("dt", "dt", 1e-3, width = 100),
      numericInput("L", "L", 3, width = 100),
      numericInput("N", "N", 60, width = 100),
      h4("Parameters"),
      lapply(parms_input, function(x) do.call("numericInput", x))
    ),
    mainPanel(
      h3("Simulation results"),
      plotOutput("AG")
    )
  )
)


shinyApp(ui = ui, server = server)