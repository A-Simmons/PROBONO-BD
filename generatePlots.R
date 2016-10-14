generateLogLogPlots <- function(x,y) {
  
  x <- x %>% log() %>% as.matrix()
  y <- y %>% log() %>% as.matrix()
  
  lm.r <- lm(y~x)
  
  sum <- summary(lm.r)
  resid <- resid(lm.r)
  
  qplot(x, y)
  
  loglog = function(x){
    lm.r$coefficients[1] + lm.r$coefficients[2]*x
  }
  
  actual = function(x){
    exp(lm.r$coefficients[1])*x^lm.r$coefficients[2]
  }
  
  plot(NA, xlim=c(min(x),max(x)), ylim=c(min(y),max(y)), ylab="value")
  points(x, y, pch=15)
  curve(loglog, from = min(x), to = max(x), add=TRUE)
  
  x <- exp(x)
  y <- exp(y)
  plot(NA, xlim=c(min(x),max(x)), ylim=c(min(y),max(y)), ylab="value")
  points(x, y, pch=15)
  curve(actual, from = min(x), to = max(x), add=TRUE)
  

}