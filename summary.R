summary.var.select <- function(x, ...){
  if (!inherits(x, "var.select"))
    stop("Object must be class 'var.select'")
  num_models <- nrow(x$data.frame)
  if (num_models > 10){
    bottom_ten_index <- num_models - 10
    cat("Top 10 models by adjusted R-squared:\n")
    print(x$data.frame[1:10,])
    cat("\nBottom 10 models by adjusted R-squared:\n")
    x$data.frame[bottom_ten_index:num_models,]
  }
  else print(x)
  
}