source("requirements.R")
source("auxiliary/functions.R")

wkbk_2011 <- read_excel("Data/2011_no_macros.xlsx")
wkbk_2012 <- read_excel("Data/2012_no_macros.xlsx")
wkbk_2013 <- read_excel("Data/2013_no_macros.xlsx")
wkbk_2014 <- read_excel("Data/2014_no_macros__.xlsx")
wkbk_2015 <- read_excel("Data/2015_no_macros.xlsx")
wkbk_2016 <- read_excel("Data/2016_no_macros.xlsx")
wkbk_2017 <- read_csv("Data/2017_no_macros.csv")

wkbk_list <- list(wkbk_2017, wkbk_2016, wkbk_2015, wkbk_2014, wkbk_2013, wkbk_2012, wkbk_2011)

#replace missing values with NA for all workbooks
na_chars <- c("DK", "Dk", "$-", "RF")
wkbk_list <- map(wkbk_list, make_na, na_chars = na_chars)
wkbk_list <- lapply(wkbk_list, rm_dol_df)

year <- 2017
for(i in 1:length(wkbk_list)){
  wkbk_list[[i]]$year <- as.character(year)
  year <- year - 1
}

############CLEAN 2017###########
#use which and grepl to get start and end points
start = which(grepl("How.well.would.you.say", colnames(wkbk_list[[1]])))
end = which(grepl("contractorpayment", colnames(wkbk_list[[1]])))

#shift the section of the row one to the right
wkbk_list[[1]][143,(start-1):end] <- wkbk_list[[1]][143, start:(end+1)]

columns_to_keep <- c(1:5, 7, 10, 16,17, 21:25, 32, 43:44, 98, 100,102, 129)
wkbk_list[[1]] <- wkbk_list[[1]] %>% select(columns_to_keep)

completed_2017 <- wkbk_list[[1]] %>% filter(!grepl("Could", survey.status))

###################################

#######CLEAN 2016#########

columns_to_keep <- c(1:5, 8, 12, 18, 19, 23:27, 36, 47, 49, 105, 107, 109, 137)
wkbk_list[[2]] <- wkbk_list[[2]] %>% select(columns_to_keep)

##########################

##########CLEAN 2015#########
columns_to_keep <- c(1:5, 7, 11,  17,18, 22:26, 35, 48, 50, 103, 105, 107, 135)
wkbk_list[[3]] <- wkbk_list[[3]] %>% select(columns_to_keep)

#############################

##########CLEAN 2014##########
columns_to_keep <- c(1:5, 7, 11,  17,18, 22:26, 33, 46, 48, 101, 103, 105, 133)
wkbk_list[[4]] <- wkbk_list[[4]] %>% select(columns_to_keep)

############CLEAN 2013########
columns_to_keep <- c(1:5, 7, 14, 20,21, 25, 28:30, 42, 53, 66, 68, 121, 123, 125, 153)
wkbk_list[[5]] <- wkbk_list[[5]] %>% select(columns_to_keep)
##############################


#####Clean 2012########
wkbk_list[[6]] <- wkbk_list[[6]] %>%
  mutate(Status.of.the.client.s.survey = ifelse(is.na(Health.insurance.) & row_number() > 102,"Could not complete survey","Client Completed Survey"))

columns_to_keep <- c(1:5, 7, 14, 20,21, 25, 28:30, 42, 53, 66, 68, 121, 123, 125, 153)
wkbk_list[[6]] <- wkbk_list[[6]] %>% select(columns_to_keep)
##################

########Clean 2011##########
wkbk_list[[7]] <- wkbk_list[[7]] %>% select(columns_to_keep)
##############3



############2016##############
#completed <- completed %>% mutate(biz.survive = ifelse(survey.biz16 == "Yes", 1, 0 )) %>% 
#  group_by(race) %>% 
#  mutate(intake.hhincome = replace(intake.hhincome, is.na(intake.hhincome), mean(intake.hhincome, na.rm = TRUE))) %>% 
#  select(race, gender, intake.hhincome, biz.survive, survey.biz.at.intake)

#remove $
#remove_sign <- function(el){
#  if(grepl('^[$]', el)){
#    print(substr(el, 2, nchar(el)))
#    return(as.numeric(gsub(",", "",substr(el, 2, nchar(el)))))
#  }
#  return(as.numeric(el))
#}

#apply(wkbk %>% select(intake.gross.biz.sales), 1, FUN = remove_sign)

#wkbk <- wkbk %>% mutate(intake.gross.biz.sales = apply(wkbk$intake.gross.biz.sales, 0, remove_sign))

#write.csv(wkbk, "Data/workbook_allcols.csv")

#logit_mod <- glm(biz.survive ~ race + gender + survey.biz.at.intake + intake.hhincome,data = completed, family = binomial(link = "logit"))
