#' create_fit_exp_increasing
#'
#' @description This function creates an exponential increasing fit for measurement data and then
#' selects a stable region within a margin.
#'
#' @details The highest value of the exponential fit is taken as final value, then the stable region is defined
#' as the region [alph_start, alph_end]. The \code{alph_start} value is given by where the exponential
#' decreasing fit crosses the margin around the highest final value. The function is
#' fitted with drc::AR.3 and is given by:
#' \eqn{
#' f(x) = A + (B-A)(1- e^{-x/C})
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
#' @importFrom drc AR
#' @importFrom shiny renderPlot
#' @importFrom graphics plot
#' @importFrom graphics curve
#' @importFrom graphics abline
#' @importFrom stats coef
#'
#' @examples \dontrun{
#' result_fit3 <- create_fit_exp_increasing(meancpa = roi_values$meas$meancpa,
#'                                          mfpga_bin = roi_values$meas$mfpga_bin,
#'                                          margin = input$roi_margin,
#'                                          meas_no =  input$roi_meas_no,
#'                                          alpha_packet_size = input$roi_alpha_packet_size)
#' roi_values$alph_start <- result_fit3$alph_start
#' roi_values$alph_end <- result_fit3$alph_end
#'
#' roi_values$curve_fit_param_A <- result_fit3$curve_fit_param_A
#' roi_values$curve_fit_param_B <- result_fit3$curve_fit_param_B
#' roi_values$curve_fit_param_C <- result_fit3$curve_fit_param_C
#' roi_values$curve_shape <- 3
#'
#' output$roi_plot_CpT <- result_fit3$CpT_plot
#' }
create_fit_exp_increasing <- function(meancpa,
                               mfpga_bin,
                               margin,
                               meas_no,
                               alpha_packet_size
                               ){
  cat("\nCurve Shape 3: Exponential increase")
  meancpa <- meancpa
  margin <- margin
  alpha_packet_size <- alpha_packet_size
  mfpga_bin <- mfpga_bin
  i <- meas_no

  x = seq(1:(length(meancpa)))
  y = meancpa
  data <- data.frame(x=seq(1:(length(meancpa))), y=meancpa)

  #Fit exponential increasing curve to data using drc package
  fit3<-drc::drm(y~x, data=data, fct=drc::AR.3(fixed = c(NA, NA, NA), names = c("c", "d", "e")))
  coef_fit3 <- stats::coef(fit3)
  myfit3 <- function(x){
    y <- coef_fit3[1] +(coef_fit3[2]-coef_fit3[1])*(1-exp(-x/coef_fit3[3]))
    return(y)
  }
  y_stable <- max(myfit3(x))
  x_alph_start <- 1
  x_alph_end <- length(meancpa) #set alpha_end to last alpha
  for (j in seq(from=2, to=length(meancpa))){
    if(  (myfit3(x[j])>(1-margin)*y_stable)){
      x_alph_start <- j
      break()
    }
  }
  cat("\n The fit coefficients are: ", coef_fit3)

  CpT_plot <- shiny::renderPlot({
    graphics::plot(y ~ x, data=data,
         main = paste("Meas",meas_no , ": Exponential Increase"),
         xlab = paste("Consecutive Means (Alpha Packet Size = ",
                      alpha_packet_size, ")", sep = ""),
         ylab = "Mean Counts per Alpha")
    graphics::abline(a = y_stable, b= 0, col="red", lw=2)
    graphics::abline(a = (1+margin)*y_stable, b= 0, col="orange", lw=2)
    graphics::abline(a = (1-margin)*y_stable, b= 0, col="orange", lw=2)
    graphics::curve(myfit3(x), from=1, to=length(meancpa), add=TRUE, col="Black", lw=2)
    graphics::abline(v = x_alph_start, col="limegreen", lw="2")
    graphics::abline(v = floor(x_alph_end), col="limegreen", lw=2)
  })

  alph_end = nrow(mfpga_bin) #set alpha end to Total No of alphas
  alph_start <- ceiling(x_alph_start*alpha_packet_size)


  cat("\nMargin: ", margin, "Alpha Start: ",x_alph_start, "Alpha End: ",x_alph_end)
  cat("\nValues Alpha Start: ",alph_start, "Alpha End: ",alph_end)

  results <- list("alph_start" = alph_start,
                  "alph_end" = alph_end,
                  "curve_fit_param_A" = coef_fit3[1],
                  "curve_fit_param_B" = coef_fit3[2],
                  "curve_fit_param_C" = coef_fit3[3],
                  "CpT_plot" = CpT_plot
  )


  return(results)


}
