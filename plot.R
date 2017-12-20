plot.var.select <- function(x, ...){
  if (!inherits(x, "var.select"))
    stop("Object must be class 'var.select'")
  data <- x$data.frame
  var_num <- data[,1]
  adj_r_square <- data[,2]
  aic <- data[,3]
  bic <- data[,4]
  opar <- par(no.readonly=TRUE)
  par(mfrow=c(1,3))
  plot(var_num, adj_r_square, col="red")
  plot(var_num, aic, col="blue")
  plot(var_num, bic, col="green")
  par(opar)
  
  
}