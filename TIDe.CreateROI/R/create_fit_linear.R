#' create_fit_linear
#'
#' @description This function creates a linear fit for measurement data and then
#' selects a stable region within a margin.
#'
#' @details The whole measurement region is taken as ROI. There is code that could be commented in to take
#' the margin into account. The function is fitted with stats::lm and is given by:
#' \eqn{
#' f(x) = A + Bx
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

#' @importFrom stats lm
#' @importFrom shiny renderPlot
#' @importFrom graphics plot
#' @importFrom graphics curve
#' @importFrom graphics abline
#' @importFrom stats coef
#'
#' @examples \dontrun{
#' result_fit2 <- create_fit_linear(meancpa = roi_values$meas$meancpa,
#'                                          mfpga_bin = roi_values$meas$mfpga_bin,
#'                                          margin = input$roi_margin,
#'                                          meas_no =  input$roi_meas_no,
#'                                          alpha_packet_size = input$roi_alpha_packet_size)
#' roi_values$alph_start <- result_fit2$alph_start
#' roi_values$alph_end <- result_fit2$alph_end
#'
#' roi_values$curve_fit_param_A <- result_fit2$curve_fit_param_A
#' roi_values$curve_fit_param_B <- result_fit2$curve_fit_param_B
#' roi_values$curve_fit_param_C <- result_fit2$curve_fit_param_C
#' roi_values$curve_shape <- 1
#'
#' output$roi_plot_CpT <- result_fit2$CpT_plot
#' }
create_fit_linear <- function(meancpa,
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
  data <- data.frame(x=seq(1:length(meancpa)), y=meancpa)

  #Fit linear curve to data
  fit2<-stats::lm(y~x, data=data)
  coef_fit2 <- stats::coef(fit2)
  myfit2 <- function(x){
    y <- coef_fit2[1] +coef_fit2[2]*x
    return(y)
  }
  x_alph_start <- 1
  x_alph_end <- length(meancpa) #set alpha_end to last alpha
  y_stable <- (myfit2(mean(c(x_alph_start,length(meancpa)))))

  # if(coef_fit2[2]>0){
  #   print("Linear increasing curve")
  #   #select alph_start
  #   for (j in seq(from=2, to=length(meancpa))){
  #     # cat("j: ", j, "\n x_alph_start: ", x_alph_start,"\n")
  #     if(  (myfit2(x[j])>(1-margin)*y_stable)){
  #       x_alph_start <- j
  #       break()
  #     }
  #   }
  #   #select alph end
  #   for (j in seq(from=2, to=length(meancpa))){
  #     #print(j)
  #     # cat("j: ", j, "\n x_alph_start: ", x_alph_start,"\n")
  #     if(  (myfit2(x[j])>(1+margin)*y_stable)){
  #       x_alph_end <- j
  #       break()
  #     }
  #     #Correction for if x_alph_end would lie outside of bounds.
  #     if(x_alph_end > length(counts_pro_alpha)/alpha_packet_size){
  #       x_alph_end <- floor(length(counts_pro_alpha)/alpha_packet_size)
  #     }
  #   }
  # }else if (coef_fit2[2]<0){
  #   print("Linear decreasing curve")
  #   #select alph_start
  #   for (j in seq(from=2, to=length(meancpa))){
  #     # cat("j: ", j, "\n x_alph_start: ", x_alph_start,"\n")
  #     if(  (myfit2(x[j])<(1+margin)*y_stable)){
  #       print("abs(myfit(x[j])-y_stable))")
  #       x_alph_start <- j
  #       break()
  #     }
  #   }
  #   #select alph end
  #   for (j in seq(from=2, to=length(meancpa))){
  #     print(j)
  #     # cat("j: ", j, "\n x_alph_start: ", x_alph_start,"\n")
  #     if(  (myfit2(x[j])<(1-margin)*y_stable)){
  #       print("abs(myfit(x[j])-y_stable))")
  #       x_alph_end <- j
  #       break()
  #     }
  #     #Correction for if x_alph_end would lie outside of bounds.
  #     if(x_alph_end > length(counts_pro_alpha)/alpha_packet_size){
  #       x_alph_end =length(counts_pro_alpha) #set alpha end to Total No of alphas
  #
  #     }
  #
  #   }
  # }

  CpT_plot <- shiny::renderPlot({
    graphics::plot(y ~ x, data=data,
         main = paste("Meas",meas_no , ": Linear"),
         xlab = paste("Consecutive Means (Alpha Packet Size = ",
                      alpha_packet_size, ")", sep = ""),
         ylab = "Mean Counts per Alpha")
    graphics::abline(a = y_stable, b= 0, col="red", lw=2)
    graphics::abline(a = (1+margin)*y_stable, b= 0, col="orange", lw=2)
    graphics::abline(a = (1-margin)*y_stable, b= 0, col="orange", lw=2)
    graphics::curve(myfit2(x), from=1, to=length(meancpa), add=TRUE, col="Black", lw=2)
    graphics::abline(v = x_alph_start, col="limegreen",lty=2, lw="2")
    graphics::abline(v = floor(x_alph_end), col="limegreen",lty=2, lw=2)
    graphics::abline(v = 1, col="limegreen", lw="2")
    graphics::abline(v = floor(length(meancpa)), col="limegreen", lw=2)
  })

  alph_end <-floor(x_alph_end*alpha_packet_size)

  cat("\nCurve Shape 2: Linear, stable")
  cat("\nMargin: ", margin, "Alpha Start: ",x_alph_start, "Alpha End: ",x_alph_end)
  cat("\nValues Alpha Start: ",alph_start, "Alpha End: ",alph_end)


  #Manually overwrite to take full measurement
  alph_start <- 1
  alph_end <- nrow(mfpga_bin) #set alpha_end to last alpha
  cat("Alpha end linear fit: ", alph_end)

  results <- list("alph_start" = alph_start,
                  "alph_end" = alph_end,
                  "curve_fit_param_A" = coef_fit2[1],
                  "curve_fit_param_B" = coef_fit2[2],
                  "curve_fit_param_C" = 0,
                  "CpT_plot" = CpT_plot
  )


  return(results)
}
