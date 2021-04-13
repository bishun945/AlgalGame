#' @export
#' @aliases run_shiny
run_Shiny <- function() {

  appPath <- system.file("shiny-examples", "AlgalGameShiny.R", package = "AlgalGame")

  if(appPath == "") {

    stop("Could not find example path. Try re-installing `AlgalGame`.", call. = FALSE)

  }

  shiny::runApp(appPath, display.mode = "normal")

}
