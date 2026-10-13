#' Launch the Phase I Trial Simulation Dashboard
#'
#' This function launches the local interactive Shiny application included within the package.
#'
#' @importFrom shiny runApp
#' @export
#' @examples
#' \dontrun{
#' run_dashboard()
#' }
run_dashboard <- function() {
  app_dir <- system.file("shiny-app", package = "Phase1DesignDecision")
  if (app_dir == "") {
    stop("Could not find shiny-app directory. Try re-installing `Phase1DesignDecision`.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
}
