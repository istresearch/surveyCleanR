#' A surveyCleanR function
#'
#' This function allows you to deal with missing data in a column and quantify variables.
#' @param df Required Dataframe object.
#' @param col_to_vect Required column/vector object.
#' @param colname Required column name
#' @keywords dataframe survey column
#' @export
#' @examples
#' vectorize_col(dataframe,column_in_frame,name_of_column)

vectorize_col <- function(df, col_to_vect, colname) {
  
  if (is.numeric(col_to_vect)) {
    #cat(colname)
    df[,colname] <- col_to_vect
    
    mean_col <- mean(df[,colname],na.rm = TRUE)
    
    if (!is.na(df)) {
      df[,colname] <- sapply(1:nrow(df), function(x) {
        if (is.na(df[,colname][x])) {
          return(mean_col)
        }
        else {
          return(df[,colname][x])
        }
      
      })
    }
    else {
      return(NULL)
    }
  }
  
  else if (is.character(col_to_vect)) {
    #cat('character detected\n')
    
    if (length(unique(col_to_vect)) == 2) {
      df[,colname] = ifelse(col_to_vect == unique(col_to_vect)[1],1,0)
    }
    
    else if (length(unique(col_to_vect)) <= 30) {
      
      for (level in unique(col_to_vect)) {
        
        colname_paste <- paste0(colname, as.character(level))
        
        df[,colname_paste] <- ifelse(col_to_vect == level,1,0)
        
        df[is.na(col_to_vect),colname_paste] <- NA
        
        mean_col <- sum(df[,colname_paste],na.rm = TRUE)/
          length(df[,colname_paste])
        
        df[is.na(df[,colname_paste]),colname_paste] <- mean_col
        
        
      }
      
    }
    
  }
  
  return(df)
  
}