#' create_fit_exp_decreasing
#'
#' @description This function creates an exponential decreasing fit for measurement data and then
#' selects a stable region within a margin.
#'
#' @details The lowest value of the exponential fit is taken as final value, then the stable region is defined
#' as the region [alph_start, alph_end]. The \code{alph_start} value is given by where the exponential
#' decreasing fit crosses the margin around the lowest final value. The function is fitted with
#' drc::EX.3 and is given by:
#' \eqn{
#' f(x) = A + (B-A)(e^{-x/C})
#' }
#'
#' @param meancpa Sum of the rows from \code{mfpga_bin} and divided by alpha packet size.
#' @param mfpga_bin Matrix with each row corresponding to one alpha, each column to an ionization
#' @param margin Margin for the calculation of fit, default is 0.01 (one percent).
#' @param meas_no Measurement number.
#' @param alpha_packet_size Number of alpha particles grouped into one data point, default 100.
#'
#' @return List with entries \code{alph_start}, \code{alph_end}, \code{curve_fit_param_A},
#' \code{curve_fit_param_B},\code{curve_fit_param_C} and \code{CpT_plot}
#' @export
#'
#' @importFrom drc drm
#' @importFrom drc EXD
#' @importFrom shiny renderPlot
#' @importFrom graphics plot
#' @importFrom graphics curve
#' @importFrom graphics abline
#' @importFrom stats coef
#'
#' @examples \dontrun{
#' result_fit1 <- create_fit_exp_decreasing(meancpa = roi_values$meas$meancpa,
#'                                          mfpga_bin = roi_values$meas$mfpga_bin,
#'                                          margin = input$roi_margin,
#'                                          meas_no =  input$roi_meas_no,
#'                                          alpha_packet_size = input$roi_alpha_packet_size)
#' roi_values$alph_start <- result_fit1$alph_start
#' roi_values$alph_end <- result_fit1$alph_end
#'
#' roi_values$curve_fit_param_A <- result_fit1$curve_fit_param_A
#' roi_values$curve_fit_param_B <- result_fit1$curve_fit_param_B
#' roi_values$curve_fit_param_C <- result_fit1$curve_fit_param_C
#' roi_values$curve_shape <- 1
#'
#' output$roi_plot_CpT <- result_fit1$CpT_plot
#' }
create_fit_exp_decreasing <- function(meancpa,
                               mfpga_bin,
                               margin,
                               meas_no,
                               alpha_packet_size
){
  meancpa <- meancpa
  margin <- margin
  alpha_packet_size <- alpha_packet_size
  mfpga_bin <- mfpga_bin
  i <- meas_no

  x = seq(1:(length(meancpa)))
  y = meancpa
  cat("\nCurve Shape 1: Exponential decrease.")
  data <- data.frame(x=seq(1:length(meancpa)), y=meancpa)

  #Fit exponential sinking curve to data using drc package
  fit1<-drc::drm(y~x, data=data, fct=drc::EXD.3(fixed = c(NA, NA, NA), names = c("c", "d", "e")))
  coef_fit1 <- stats::coef(fit1)
  myfit <- function(x){
    y <- coef_fit1[1] +(coef_fit1[2]-coef_fit1[1])*exp(-x/coef_fit1[3])
    return(y)
  }
  cat("\n The fit coefficients are: ", coef_fit1)

  #Define stable endpoint of exponential decreasing fit
  y_stable <- min(myfit(x))

  x_alph_start <- 1
  x_alph_end <- length(meancpa) #set alpha_end to last alpha
  for (j in seq(from=2, to=length(meancpa))){
    if(  (myfit(x[j])<(1+margin)*y_stable)){
      x_alph_start <- j
      break()
    }
  }
  alph_end = nrow(mfpga_bin) #set alpha end to Total No of alphas
  alph_start <- ceiling(x_alph_start*alpha_packet_size)


  cat("\nMargin: ", margin, "Alpha Start: ",x_alph_start, "Alpha End: ",x_alph_end)
  cat("\nValues Alpha Start: ",alph_start, "Alpha End: ",alph_end)

  # output$out_CpT_plot <- renderPlot({
    CpT_plot <- shiny::renderPlot({

    graphics::plot(y ~ x, data=data,
         main = paste("Meas",meas_no , ": Exponential Decrease"),
         xlab = paste("Consecutive Means (Alpha Packet Size = ",
                      alpha_packet_size, ")", sep = ""),
         ylab = "Mean Counts per Alpha")
    graphics::abline(a = y_stable, b= 0, col="red", lw=2)
    graphics::abline(a = (1+margin)*y_stable, b= 0, col="orange", lw=2)
    graphics::abline(a = (1-margin)*y_stable, b= 0, col="orange", lw=2)
    graphics::curve(myfit(x), from=1, to=length(meancpa), add=TRUE, col="Black", lw=2)
    graphics::abline(v = x_alph_start, col="limegreen", lw="2")
    graphics::abline(v = floor(x_alph_end), col="limegreen", lw=2)
  })




  results <- list("alph_start" = alph_start,
                  "alph_end" = alph_end,
                  "curve_fit_param_A" = coef_fit1[1],
                  "curve_fit_param_B" = coef_fit1[2],
                  "curve_fit_param_C" = coef_fit1[3],
                  "CpT_plot" = CpT_plot
                  )


  return(results)
}





