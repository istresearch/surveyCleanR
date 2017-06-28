# surveyCleanR

This is an R package for basic survey data cleanup. It makes it easier to run regression models and it accounts for missing data.

## Installation

require('devtools')
install_github('istresearch/surveyCleanR')

## Usage

df_numeric <- vectorize_df(df, strata_cols = c(col1,col2,col3))

## Future Directions

- Random assignment of dichotomous or categorical variables.
- Have this random assignment occur within a stratum.
