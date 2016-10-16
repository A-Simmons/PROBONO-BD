# Function that pulls out the data
retrieveDataLM <- function(data, CLASS, xy) {
  names <- c("STORE", "oz", "p", "deal")
  if (xy == "x")
    temp <- data %>% filter(.,class==CLASS) %>% select(.,STORE, oz_X, pX, deal_X)
  else
    temp <- data %>% filter(class==CLASS) %>% select(STORE, oz_Y, pY, deal_Y)
  colnames(temp) <- names
  name <- if (xy == "x") "X" else "Y"
  name <- if (CLASS == "organic") paste(name,"O",sep="") else paste(name,"I",sep="") 
  return(data.frame(type=rep(name,nrow(temp)),temp))
}

# Function that plots the loglog and actual curves
filteredLM <- function(data, TYPE, log=FALSE, include_Deal=FALSE) {
  response <- data %>% filter(type==TYPE) %>% select(oz) %>% {if (log) log(.) else (.)} %>% as.matrix()
  ind <- data %>% filter(type==TYPE) %>% select(p) %>% {if (log) log(.) else (.)} %>% as.matrix()
  deal <- data %>% filter(type==TYPE) %>% select(deal) %>% {if (include_Deal) (.) else (.)*0} %>% as.matrix()
  return(lm(response~ind+deal))
}

plotStats <- function(lm) {
  results <- data.frame(p=lm$model$ind, oz=lm$model$response, resid=resid(lm))
  results <- mutate(results, loglogSol=lm$coefficients[1] + lm$coefficients[2]*p + if (is.na(lm$coefficients[3])) 0 else lm$coefficients[3]*lm$model$deal, sol=exp(lm$coefficients[1])*p^lm$coefficients[2])
  ggplot(results) + geom_point(aes(x=p, y=oz)) + geom_line(aes(x=p, y=loglogSol)) 
}
# Function that plots the loglog and actual curves

loglog = function(lm.r, x){
  lm.r$coefficients[1] + lm.r$coefficients[2]*x
}

actual = function(lm.r, x){
  exp(lm.r$coefficients[1])*x^lm.r$coefficients[2]
}

# Function that generates the inear model
lm_data <- retrieveDataLM(data, CLASS="organic", xy="x")
lm_data <- rbind(lm_data,retrieveDataLM(data, CLASS="nonorganic", xy="x"))
lm_data <- rbind(lm_data,retrieveDataLM(data, CLASS="organic", xy="y"))
lm_data <- rbind(lm_data,retrieveDataLM(data, CLASS="nonorganic", xy="y"))

# Create linear models
XO_lm_with_deal <- filteredLM(lm_data, TYPE="XO", log=TRUE, include_Deal=TRUE)
XO_lm_without_deal <- filteredLM(lm_data, TYPE="XO", log=TRUE, include_Deal=FALSE)
YO_lm_with_deal <- filteredLM(lm_data, TYPE="YO", log=TRUE, include_Deal=TRUE)
YO_lm_without_deal <- filteredLM(lm_data, TYPE="YO", log=TRUE, include_Deal=FALSE)
XI_lm_with_deal <- filteredLM(lm_data, TYPE="XI", log=TRUE, include_Deal=TRUE)
XI_lm_without_deal <- filteredLM(lm_data, TYPE="XI", log=TRUE, include_Deal=FALSE)
YI_lm_with_deal <- filteredLM(lm_data, TYPE="YI", log=TRUE, include_Deal=TRUE)
YI_lm_without_deal <- filteredLM(lm_data, TYPE="YI", log=TRUE, include_Deal=FALSE)



