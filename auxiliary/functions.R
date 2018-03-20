#remove $
remove_dollarsign <- function(el){
  #check if the element begins with a dollar sign
  if(grepl('^[$]', el)){
    
    #chop off the dollar sign with substr()
    substring <- substr(el, 2, nchar(el))  
    
    #substitute nothing for commas and convert to numeric
    return(as.numeric(gsub(",", "", substring)))
  }
  
  #if it doesn't have a dollar sign in front just convert it to numeric
  return(as.numeric(el))
}

is_dollarsign <- function(col){
  return(any(grepl("\\$", col)))
}

#function which returns names and indices of columns within a dataframe that include strings with dollar signs.
find_dollarsign <- function(df){
  return(
    apply(df, 2, is_dollarsign)
  )
}

rm_dol_df <- function(df){
  columns_to_convert <- which(find_dollarsign(df))
  for(col in columns_to_convert){
    
    #print(wkbk %>% select(col))
    df[col] <- apply(df %>% select(col), 1, FUN = remove_dollarsign)  
    
  }
  
  return(df)
}

#Turns any cells which hold an element in na_chars into NA.  
make_na <- function(df, na_chars){
  data.frame(apply(df, 2, function(x) replace(x, x %in% na_chars, NA)))
}