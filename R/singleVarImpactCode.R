#' @title singleVarImpactCode 
#' @param mydata a data frame
#' @param catCol name of a grouped catagory in the mydata
#' @param classCol outcome name use to create impact codes
#' @family a glm family, either 'poisson', 'binomial', or 'gaussian'
#' @description creates a multi level intercept model for a single catagory using glmer from lmer4

singleVarImpactCode<-function(mydata, catCol = 'MSDRGName', classCol = 'UHC_ReadmissionFlag', fam = 'gaussian'){
  newCatCol<-paste('(1 | ' ,paste(catCol, ')', sep = ''), sep = '' )
  f<-as.formula(paste(classCol, newCatCol, sep = '~'))
  if ( fam == 'gaussian'){
    fit<-lmer(f, data  = mydata[,c(catCol, classCol)])
  }
  if (fam == 'binomial'){
    fit<-glmer(f, data  = mydata[,c(catCol, classCol)], family = binomial )
  }
  if (fam == 'poisson'){
    fit<-glmer(f, data  = mydata[,c(catCol, classCol)], family = poisson )
  }  
  return(fit)
}