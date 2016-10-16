# Function that pulls out the data
retrieveDataCPED <- function(data, Q_class, Q_brand) {
  tempQ <- data %>% filter(., class==Q_class) %>% select(., STORE, matches(paste("oz",Q_brand,sep="_")))
  colnames(tempQ) <- c("STORE", "oz")
  tempP_Org <- data %>% filter(., class=="organic") %>% select(., pX, pY)
  colnames(tempP_Org) <- c("p_X_org", "p_Y_org")
  tempP_NonOrg <- data %>% filter(., class=="nonorganic") %>% select(., pX, pY)
  colnames(tempP_NonOrg) <- c("p_X_nonorg", "p_Y_nonorg")
  return(data.frame(tempQ, tempP_Org, tempP_NonOrg))
}

# Function that plots the loglog and actual curves
crossPEDLinearReg <- function(data, TYPES, log=TRUE) {
  Q <- data %>% select(oz) %>% {if (log) log(.) else (.)} %>% as.matrix()
  pXOrg <- data %>% select(p_X_org) %>% {if (log) log(.) else (.)} %>% {if ("xOrg" %in% TYPES) (.) else (.)*0} %>% as.matrix()
  pYOrg <- data %>% select(p_Y_org) %>% {if (log) log(.) else (.)} %>% {if ("yOrg" %in% TYPES) (.) else (.)*0}  %>% as.matrix()
  pXNonOrg <- data %>% select(p_X_nonorg) %>% {if (log) log(.) else (.)} %>% {if ("xNonOrg" %in% TYPES) (.) else (.)*0}  %>% as.matrix()
  pYNonOrg <- data %>% select(p_Y_nonorg) %>% {if (log) log(.) else (.)} %>% {if ("yNonOrg" %in% TYPES) (.) else (.)*0}  %>% as.matrix()
  return(lm(Q~pXOrg+pYOrg+pXNonOrg+pYNonOrg))
}


crossXOrg <- retrieveDataCPED(data, "organic", "X")
crossYOrg <- retrieveDataCPED(data, "organic", "Y")
crossXNonOrg <- retrieveDataCPED(data, "nonorganic", "X")
crossYNonOrg <- retrieveDataCPED(data, "nonorganic", "Y")

lmXOrg <- crossPEDLinearReg(crossXOrg, c("xOrg"))
summary(lmXOrg)

lmXOrg <- crossPEDLinearReg(crossXOrg, c("yOrg"))
summary(lmXOrg)

lmXOrg <- crossPEDLinearReg(crossXOrg, c("xNonOrg"))
summary(lmXOrg)

lmXOrg <- crossPEDLinearReg(crossXOrg, c("yNonOrg"))
summary(lmXOrg)