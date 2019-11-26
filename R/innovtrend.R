#' @title Innovative Trend Analysis
#'
#' @description Innovative trend analysis method proposed by Zekai Sen (2011) is implemented in this function.
#'
#' @importFrom  graphics abline legend lines plot text
#'
#' @importFrom stats plot.ts ts cor sd
#'
#'
#' @usage innovtrend(x)
#'
#' @param  x  - Time series data vector
#'
#' @return  S  - Slope of the trend
#'
#' D  - Trend indicator
#'
#' CLlower90  - Lower Cofidence Limit at 90 percent
#' CLupper90  - Uppler Cofidence Limit at 90 percent
#'
#' CLlower95  - Lower Cofidence Limit at 95 percent
#' CLupper95  - Uppler Cofidence Limit at 95 percent
#'
#' CLlower99  - Lower Cofidence Limit at 99 percent
#' CLupper99  - Uppler Cofidence Limit at 99 percent
#'
#' @references Şen Z (2011) Innovative Trend Analysis Methodology. J Hydrol Eng 17:1042–1046. <doi: 10.1061/(ASCE)HE.1943-5584.0000556>.
#'
#' @details If the data points lay on 1:1 line, there is no trend in the data. If the data points exist in the top triangle, it is indicative of positive trend. If the data lies in the bottom triangle, it indicates negative trend in the data.
#'
#' @examples x<-c(Nile)
#' innovtrend(x)
#'
#' @export

innovtrend <-  function(x) {

  # Initialize the test Parameters

  # Time-Series Vector

  x = x
  xbar = mean(x)

  # Length of time-series

  n <- length(x)

  # First half of time-series arranged in ascending order

  fh <- sort(x[1:(n/2)])


  # Second half of time-series arranged in ascending order

  sh <- sort(x[((n/2)+1):n])


  # Calculating Trend Slope

  ts = (2*(mean(sh)-mean(fh)))/n


  # Plotting first half versus second half of the series


  plot(fh, sh, pch = 1, main = "Innovative Trend Analysis", xlab = "First half of the series", ylab = "Second half of the series",xlim = c(min(x),max(x)),ylim = c(min(x),max(x)))


  #plotting 1:1 no trend line
  abline(a=0,b=1, lty = 1, lwd = 1)->nt



  # Slope of the trend is calculated according to Sen 2015
  S <- (2*(mean(sh)-mean(fh)))/length(x)

  # Trend indicator calculation
  D <- mean((sh-fh)*10/mean(fh))


  # Calculating slope standard deviation
  ssd  <- (2*sqrt(2))*sd(x)*sqrt(1-cor(sh,fh))/length(x)/sqrt(length(x))

  # Confidence limits (CL) of the trend slope at 90 percent

  CLlower90 <- 0 - 1.645*ssd
  CLupper90 <- 0 + 1.645*ssd

  # Confidence limits (CL) of the trend slope at 95 percent

  CLlower95 <- 0 - 1.96*ssd
  CLupper95 <- 0 + 1.96*ssd



  # Confidence limits (CL) of the trend slope at 99 percent

  CLlower99 <- 0 - 2.576*ssd
  CLupper99 <- 0 + 2.576*ssd


  # Adding 5 percent lower and upper confidence limits

  ten_LB <- fh-0.05*mean(fh)
  ten_UB <- fh+0.05*mean(fh)
  lines(fh, ten_LB, lty=2)
  lines(fh, ten_UB, lty=2)
  legend("bottomright",legend=c("No Trend","\u00B1 5percentline"),lty = c(1,2))


  #Adding 10 percentlower and upper confidence limits

  ten_LB <- fh-0.1*mean(fh)
  ten_UB <- fh+0.1*mean(fh)
  lines(fh, ten_LB, lty=3)
  lines(fh, ten_UB, lty=3)
  legend("bottomright",legend=c("No Trend","\u00B1 5percentline", "\u00B1 10percentline"),lty = c(1,2,3))

  return(c("Trend Slope" = S,
           "Trend Indicator"= D,
           "Lower Cofidence Limit at 90percent" = CLlower90,
           "Upper Cofidence Limit at 90percent" = CLupper90,
           "Lower Cofidence Limit at 95percent" = CLlower95,
           "Upper Cofidence Limit at 95percent" = CLupper95,
           "Lower Cofidence Limit at 99percent" = CLlower99,
           "Upper Cofidence Limit at 99percent" = CLupper99))

  }


