#' A surveyCleanR function
#'
#' This function allows you to deal with missing data in a column and quantify variables.
#' @param df Required Dataframe object.
#' @keywords dataframe survey 
#' @export
#' @examples
#' vectorize_df(df_survey)

vectorize_df <- function(df) {
  
  df_vect = data.frame(index=rep(0,nrow(df)))
  
  for (i in 1:ncol(df)) {
    
    df_vect = vectorize_col(df_vect,df[,i],colnames(df)[i])
    
  }
  
  return(df_vect)
  
}