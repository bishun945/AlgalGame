library(shiny)
library(shinyBS)
library(AlgalGame)
library(ggplot2)


# Prerequisite ------------------------------------------------------------

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

inline_numericInput <- function(inputId, label, value, ...) {
  list(
    div(style = "display: inline-block; vertical-align: sub; width: 20%",
        strong(paste0(label, ": "))),
    div(style = "display: inline-block; vertical-align: sub; width: 45%",
        numericInput(inputId, label = NULL, value = value, ...)),
    br()
  )
}

# UI settings -------------------------------------------------------------

ui <- fluidPage(

  # import shinyjs
  shinyjs::useShinyjs(),

  # custom css sytle
  tags$head(
    tags$style(HTML("
                    .form-group {
                      margin-bottom: 2px;
                    }
                    .form-control {
                      height: 25px;
                    }
                    "))
  ),

  # title
  titlePanel("Algal Game Shiny"),

  # side part (tow panles include sidebar + main)
  sidebarLayout(

    sidebarPanel(

      h4("Control", align = "left"),
      actionButton("run", "Run", icon = icon("caret-right")),
      shinyjs::disabled(downloadButton("download", "Download")),
      # shinyBS to add Tooltip
      bsTooltip("run", "Enable to download after clicking this", "right"),

      tags$hr(),
      h4("Grid setting", align = "left"),
      inline_numericInput("Ttot", "Ttot", 2, width = 100, min = 0.00, step = 0.01),
      inline_numericInput("dt", "dt", 1e-3, width = 100, min = 0, step = 1e-4),
      inline_numericInput("L", "L", 3, width = 100, min = 0.00, step = 0.25),
      inline_numericInput("N", "N", 60, width = 100, min = 1, step = 1),

      bsTooltip("Ttot", "Total running time [d]", "right"),
      bsTooltip("dt", "Time step [d]", "right"),
      bsTooltip("L", "Water column length [m]", "right"),
      bsTooltip("N", "Grid number", "right"),

      tags$hr(),
      h4("Parameters", align = "left"),
      lapply(parms_input, function(x) do.call("inline_numericInput", x))
      # lapply(parms_input, inline_numericInput)
    ),

    mainPanel(

      tabsetPanel(

        tabPanel(
          "Simulation Result",
          plotOutput("AG", height = "600px")
        ),

        tabPanel(
          "Parameter Description",
          DT::DTOutput("parms_df"),
          p("The parameters above are default settings.")
        ),

        tabPanel(
          "References",
          tags$div(
            tags$ul(
              br(),
              tags$li(a("Algal Game manuscript",
                        href = "https://doi.org/10.4319/lo.2001.46.8.1998")),
              br(),
              tags$li(a("Related slide (in Chinese)",
                        href = "https://bishun945.github.io/presentation20201216/"))
            )
          )
        )

      )

    )

  )

)



# Server running ----------------------------------------------------------

server <- function(input, output) {

  # running

  pp <- eventReactive(input$run, {
    showModal(modalDialog("Running ...", footer = NULL))
    # algal running function starts from here
    # qplot(input$Rin, input$h)
    inputs <- reactiveValuesToList(input)[parms_list$Param]
    res <- run_model(
      Ttot = input$Ttot,
      dt = input$dt,
      L = input$L,
      N = input$N,
      inputs = inputs
    )
    removeModal()
    return(res)
  })

  # dt <- eventReactive(input$run, {
  #   parms_list
  # })

  # output

  output$AG <- renderPlot({
    (pp())$gplot
  })

  output$parms_df <- DT::renderDT(
    parms_list
  )

  output$download <-
    downloadHandler("AlgalGameSimulation.csv", content = function(file) {
      write.csv((pp())$out, file)
    })

  # the download button only enable when run is clicked
  observeEvent(input$run, {
    if (is.null(input$run)) {
      shinyjs::disable("download")
    } else {
      shinyjs::enable("download")
    }
  })

}




shinyApp(ui = ui, server = server)
