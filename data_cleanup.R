source("requirements.R")

wkbk <- read_csv("~/microtest_outcomes/Data/Workbook no macros.csv")

wkbk[wkbk == "DK" | wkbk == "Dk" | wkbk == "$-" | wkbk == "RF"] <- NA

wkbk %>% mutate(survey.totalsales = as.numeric(survey.totalsales)) %>% 
              filter(survey.totalsales < 100000) %>% 
              select(survey.totalsales) %>% 
              pluck(1) %>%
              hist()

##ZERO INFLATED##

wkbk[grepl('^[$][0-9]', wkbk)] 

loaners <- wkbk %>% filter(wkbk$program == "LN")
learners <- wkbk %>% filter(wkbk$program == "EDU")
completed <- wkbk %>% filter(!grepl("Could", survey.status))

wkbk$survey.totalsales <- as.numeric(wkbk$survey.totalsales)

#remove $
remove_sign <- function(el){
  if(grepl('^[$]', el)){
    print(substr(el, 2, nchar(el)))
    return(as.numeric(gsub(",", "",substr(el, 2, nchar(el)))))
  }
  return(as.numeric(el))
}

apply(wkbk %>% select(intake.gross.biz.sales), 1, FUN = remove_sign)

wkbk %>% mutate(intake.gross.biz.sales = apply(wkbk$intake.gross.biz.sales, 0, remove_sign))

