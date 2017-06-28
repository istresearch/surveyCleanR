#' A surveyCleanR function
#'
#' This function creates the df of unique strata in the dataset.
#' @param df Required Dataframe object.
#' @param strata_cols Required vector of colnames that define strata
#' @keywords dataframe survey strata
#' @export
#' @examples
#' get_strata_df(df_survey, c(col1,col2))

get_strata_df <- function(df, strata_cols) {
  
  strata_subset <- df[, strata_cols]
  
  if (length(strata_cols) == 1) {
    
    strata_subset = unique(strata_subset)
    strata_subset = strata_subset[!is.na(strata_subset) & as.character(strata_subset) != '']
    
    return(strata_subset)
    
  }
  
  else{
    
    #replace empty strings with NAs
    sapply(1:nrow(strata_subset), function(x) {
      
      for (col in 1:ncol(strata_subset)) {
        
        if (trimws(as.character(strata_subset[x,col]),which = "both") == '') {
          strata_subset[x,col] <- NA
        }
      }
      
      
      
    })
    
    #get rows with no NA values
    strata_subset <- strata_subset[complete.cases(strata_subset),]
    
    #get just unique rows
    strata_subset <- unique(strata_subset)
    print(strata_subset)
    
    return(strata_subset)
    
  }
  
}