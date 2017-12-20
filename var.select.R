var.select <- function(data, variable.indexes=NULL, response.index=1, max.num.var=NULL,
                       method=c("R-square","AIC","BIC")){
  # check arguments
  if (missing(data) || class(data) != "data.frame")
    stop("'data' is missing or incorrect")
  if (class(variable.indexes) != "numeric" && !is.null(variable.indexes))
    stop("'variable.indexes' must be NULL or numeric")
  if (class(response.index) != "numeric" || length(response.index) > 1)
    stop("'response.index' must be numeric with length 1")
  if (length(variable.indexes) >= ncol(data))
    stop("'variable.indexes' and 'response.index' exceed number of columns in data")
  if (class(max.num.var) != "numeric" && !is.null(max.num.var))
    stop("'max.num.var' must be NULL or numeric")
  if (max.num.var >= ncol(data) && !is.null(max.num.var))
    stop("'max.num.var' must be less than or equal to the number of variables")
  
  method <- match.arg(method)
  
  # intialize objects
  selection <- list()
  response <- data[,response.index]
  if (is.null(variable.indexes))
    variable.indexes <- 1:ncol(data)
    variable.indexes <- variable.indexes[-response.index]
  variables <- data[,variable.indexes]
  p <- length(variables)
  if (is.null(max.num.var))
    max.num.var = length(variables)
  
  # calculate number of combinations
  combinations <- 0
  for (k in 1:p) combinations <- choose(p,k) + combinations
  selection$combinations <- combinations
  
  # find combinations of variable indexes
  selection$indexes <- list()
  indexes.created <- 0
  for (i in 1:max.num.var){
    indexes <- combn(p,i)
    for (index in 1:choose(p, i)){
      selection$indexes[[(index+indexes.created)]] <- indexes[,index]
    }
    indexes.created <- indexes.created + choose(p, i)
  }
  model_number <- length(selection$indexes)
  
  # find names of variables for each model
  selection$names <- list()
  for (indexes in 1:length(selection$indexes)){
    selection$names[[indexes]] <- names(variables)[selection$indexes[[indexes]]]
  }
  
  # create generalized linear model for each combination of variables
  selection$adj.r.square <- numeric(length=(model_number))
  selection$AIC <- numeric(length=(model_number))
  selection$BIC <- numeric(length=(model_number))
  for (model in 1:length(selection$indexes)){
    fit <- lm(response ~ ., data = as.data.frame(variables[,selection$indexes[[model]]]))
    # calculate criterion for model
    selection$adj.r.square[[model]] <- summary(fit)$adj.r.squared
    selection$AIC[[model]] <- AIC(fit)
    selection$BIC[[model]] <- BIC(fit)
  }
  
  # create summary data frame
  selection$model <- numeric(length=(model_number))
  selection$var.number <- numeric(length=(model_number))
  selection$data.frame <- data.frame(matrix(NA, nrow=model_number, ncol=4))
  for (value in 1:length(selection$indexes)){
    selection$model[[value]] <- value
    selection$var.number[[value]] <- length(selection$indexes[[value]])
  }
  selection$data.frame[,1] <- (selection$var.number)
  selection$data.frame[,2] <- (selection$adj.r.square)
  selection$data.frame[,3] <- (selection$AIC)
  selection$data.frame[,4] <- (selection$BIC)
  names(selection$data.frame) <- c("Num_Variables", "Adj_R_Square", "AIC", "BIC")
  for (row in 1:model_number) row.names(selection$data.frame)[row] <- paste("model ", row, sep="")
  selection$data.frame <- selection$data.frame[order(-selection$data.frame$Adj_R_Square),]
  class(selection) <- c("var.select", "list")
  return(selection)
}