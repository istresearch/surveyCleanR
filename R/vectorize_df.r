library(plyr)

#' A surveyCleanR function
#'
#' This function allows you to deal with missing data in a column and quantify variables.
#' @param df Required Dataframe object.
#' @param strata_cols Optional vector of colnames that define strata
#' @keywords dataframe survey 
#' @export
#' @import plyr
#' @examples
#' vectorize_df(df_survey)

vectorize_df <- function(df, strata_cols=NA) {
  
  #create strata
  if (!is.na(strata_cols)) {
    
    strata_df = get_strata_df(df,strata_cols)
    
  }
  
  df_vect = data.frame(index=rep(0,nrow(df)))
  
  if (is.na(strata_cols)) {
    
    for (i in 1:ncol(df)) {
      
      df_vect = vectorize_col(df_vect,df[,i],colnames(df)[i])
      
    }
    
  }
  
  else {
    
    df_vect <- NA
    
    if (length(strata_cols) == 1) {
      
      for (stratum in strata_df) {
        
        df_subset <- df[which(df[,strata_cols[1]] == stratum),]
        df_subset_vect = data.frame(index=rep(0,nrow(df_subset)))
        
        for (i in 1:ncol(df_subset)) {
          
          df_subset_vect = vectorize_col(df_subset_vect,df_subset[,i],colnames(df_subset)[i])
          
        }
        
        if (is.na(df_vect)) {
          
          df_vect <- df_subset_vect
          
        }
        
        else {
          
          df_vect <- rbind(df_vect,df_subset_vect)
          
        }
        
      }
      
    }
    
    else {
      
      df_vect <- NA
      
      for (subset in 1:nrow(strata_df)) {
        
        subset_row <- strata_df[subset,]
        #print(subset_row)
        
        df_subset <- df[sapply(df[,strata_cols],function(x) { 
            
              if (x == subset_row) {
                return(TRUE)
              }
              else {
                return(FALSE)
              }
           }),]
        print(dim(df_subset))
        df_subset_vect = data.frame(index=rep(0,nrow(df_subset)))
        
        if (nrow(df_subset) > 0) {
          
          for (i in 1:ncol(df_subset)) {
            
            df_subset_vect = vectorize_col(df_subset_vect,df_subset[,i],colnames(df_subset)[i])
            
          }
          
          if (is.na(df_vect)) {
            
            df_vect <- df_subset_vect
            
          }
          
          else {
            
            df_vect <- rbind(df_vect,df_subset_vect)
            
          }
          
        }
        
      }
      
      #df_vect <- vectorize_df(df_vect)
      
    }
    
  }
  
  return(df_vect)
  
}