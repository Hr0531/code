reg <- function(filename, formula, line = FALSE){
  extension <- tail(strsplit(filename,"\\.")[[1]], n = 1)
  if(extension == "csv"){
    x <- read.csv(filename)
  }else if(extension == "xlsx"){
    x <- read.xlsx(filename)
  }else{
    return()
  }
  fit <- lm(formula = formula, data = x)
  result <- summary(fit)
  plot(x, bty = "l", tck = 0.02, las = 1)
  if(line){
    abline(fit)
  }
  return(result)
}