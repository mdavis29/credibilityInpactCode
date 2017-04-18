#' @title singleVarImpactCode 
#' @param mydata a data frame
#' @catCol name of a grouped catagory in the mydata
#' @classCol outcome name use to create impact codes
#' @family a glm family 
#' @description creates a multi level intercept model for a single catagory using glmer from lmer4

singleVarImpactCode<-function(mydata, catCol = 'MSDRGName', classCol = 'UHC_ReadmissionFlag', family = guassian){
  newCatCol<-paste('(1 | ' ,paste(catCol, ')', sep = ''), sep = '' )
  f<-as.formula(paste(classCol, newCatCol, sep = '~'))
  fit<-glmer(f, data  = mydata[,c(catCol, classCol)], family = family)
  return(fit)
}