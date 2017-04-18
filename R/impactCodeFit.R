#' @title impactCodeFit 
#' @param mydata a data frame
#' @catCols names of a grouped catagories in the mydata
#' @classCol outcome name use to create impact codes
#' @family a glm family 
#' @description creates a multi level intercept model for a single catagory using glmer from lmer4
#' @export
impactCodeFit<-function(mydata, catCols , classCol , family = guassian){
  catCols<-unique(catCols)
  output<-list()
  for ( i in 1:(length(catCols))){
    print(paste('now building', catCols[i]))
    fit.temp<-singleVarImpactCode(mydata,catCol = catCols[i],  classCol=classCol, family = family)
    output[catCols[i]]<-fit.temp                
    }
  return(output)
}
  
  