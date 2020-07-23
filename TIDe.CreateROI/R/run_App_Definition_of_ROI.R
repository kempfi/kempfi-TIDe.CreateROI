#' run_App_Definition_of_ROI
#'
#' @description This code launches an app which shows a userinterface for the definition of a stable
#' region of interest.
#' @export
run_App_Definition_of_ROI <- function() {
  appDir <- system.file("App_Definition_of_ROI", package = "TIDe.CreateROI")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `TIDe.CreateROI`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
