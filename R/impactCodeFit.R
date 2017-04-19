#' @title impactCodeFit 
#' @param mydata a data frame
#' @param catCols names of a grouped catagories in the mydata
#' @param classCol outcome name use to create impact codes
#' @param family a glm family 
#' @return a list of intercept only multi level models to be used to transform new data
#' @description creates a multi level intercept model for a single catagory using glmer from lmer4
#' @export
impactCodeFit<-function(mydata, catCols , classCol , fam = 'guassian'){
  catCols<-unique(catCols)
  output<-list()
  for ( i in 1:(length(catCols))){
    print(paste('now building', catCols[i]))
    fit.temp<-singleVarImpactCode(mydata,catCol = catCols[i],  classCol=classCol, fam = fam)
    output[catCols[i]]<-fit.temp                
    }
  return(output)
}
  
  