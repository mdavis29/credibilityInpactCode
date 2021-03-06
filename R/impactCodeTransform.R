#' @title impactCodeTransform
#' @param newData a data frame
#' @param impactCodeFit a model list generated from impactCodeFit
#' @family a glm family 
#' @description applies a list from impactCodeFit transformations to unseen data 
#' @return a data frame with the catagory levels replaced with credibility wieghted averages 
#' @export
impactCodeTransform<-function( impactCodes, newData){
  output<-NULL
  for(i in 1:(length(impactCodes))){
    preds<-predict(impactCodes[[i]],newData, type = 'response', allow.new.levels = TRUE)
    output<-cbind(output, preds)
    }
  colnames(output)<-paste(names(impactCodes), 'code', sep = '_')
  return(output)
}