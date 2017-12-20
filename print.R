print.var.select <- function(x, ...){
  if (!inherits(x, "var.select"))
    stop("Object must be class 'var.select'")
  x$data.frame
  
}