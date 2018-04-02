source("requirements.R")
source("auxiliary/functions.R")


### LOAD ALL SHEETS AND STORE IN LIST ###

wkbk_list <- list(read_csv("Data/2017_no_macros.csv"), 
                  read_excel("Data/2016_no_macros.xlsx"),
                  read_excel("Data/2015_no_macros.xlsx"), 
                  read_excel("Data/2014_no_macros.xlsx"),
                  read_excel("Data/2013_no_macros.xlsx"),
                  read_excel("Data/2012_no_macros.xlsx"),
                  read_excel("Data/2011_no_macros.xlsx"))

#replace missing values with NA for all workbooks and remove dollar signs
na_chars <- c("DK", "Dk", "$-", "RF")
wkbk_list <- map(wkbk_list, replace_all, na_chars = na_chars, replace = NA)
wkbk_list <- map(wkbk_list, rm_dol_df)

#add year variable to analysis
year <- 2017
for(i in 1:length(wkbk_list)){
  wkbk_list[[i]]$year <- as.character(year)
  year <- year - 1
}

###################################
######## CLEAN EACH WORKBOOK ######
###################################

############CLEAN 2017###########
#use which and grepl to get start and end points
start = which(grepl("How.well.would.you.say", colnames(wkbk_list[[1]])))
end = which(grepl("contractorpayment", colnames(wkbk_list[[1]])))

#shift the section of the row one to the right
wkbk_list[[1]][143,(start-1):end] <- wkbk_list[[1]][143, start:(end+1)]

columns_to_keep <- c(1:5, 7, 10, 16,17, 21:25, 32, 43:44, 98, 100,102, 129)
wkbk_list[[1]] <- wkbk_list[[1]] %>% select(columns_to_keep)

############ CLEAN 2016 ###########

columns_to_keep <- c(1:5, 8, 12, 18, 19, 23:27, 36, 47, 49, 105, 107, 109, 137)
wkbk_list[[2]] <- wkbk_list[[2]] %>% select(columns_to_keep)


############ CLEAN 2015 ###########
columns_to_keep <- c(1:5, 7, 11,  17,18, 22:26, 35, 48, 50, 103, 105, 107, 135)
wkbk_list[[3]] <- wkbk_list[[3]] %>% select(columns_to_keep)


############ CLEAN 2014 ###########
columns_to_keep <- c(1:5, 7, 11,  17,18, 22:26, 33, 46, 48, 101, 103, 105, 133)
wkbk_list[[4]] <- wkbk_list[[4]] %>% select(columns_to_keep)


############ CLEAN 2013 ###########
columns_to_keep <- c(1:5, 7, 14, 20,21, 25, 28:30, 42, 53, 66, 68, 121, 123, 125, 153)
wkbk_list[[5]] <- wkbk_list[[5]] %>% select(columns_to_keep)


############ CLEAN 2012 ###########
wkbk_list[[6]] <- wkbk_list[[6]] %>%
  mutate(Status.of.the.client.s.survey = ifelse(is.na(`Health insurance?`) & row_number() > 102,"Could not complete survey","Client Completed Survey"))

columns_to_keep <- c(1:5, 7, 14, 20,21, 25, 28:30, 42, 53, 66, 68, 121, 123, 125, 153)
wkbk_list[[6]] <- wkbk_list[[6]] %>% select(columns_to_keep)


############ CLEAN 2011 ###########
#columns to keep same as previous
wkbk_list[[7]] <- wkbk_list[[7]] %>% select(columns_to_keep)


############ AGGREGATE #########

col_names <- c("ID", "program", "intake date", "gender", "minority", "loan_amount", "intake_biz", "intake_sales", "intake_draw",
               "intake_outemploy", "intake_hhincome", "intake_hhsize", "intake.pubass", "survey_cmplt", "survey_biz", "survey_sales",
               "survey_draw", "survey_outemploy", "survey_outincome", "survey_pubass", "year")

numeric_cols <- c("loan_amount", "intake_sales", "intake_draw",
                  "intake_hhincome", "intake_hhsize", "survey_sales",
                  "survey_draw", "survey_outincome")

character_cols <- c("ID", "program", "intake date", "gender", "minority","intake_biz",
                    "intake_outemploy", "intake.pubass", "survey_cmplt", "survey_biz", "survey_outemploy", "survey_pubass", "year")

#give same column names to all dataframes
for(i in 1:length(wkbk_list)){
  colnames(wkbk_list[[i]]) <- col_names
}

##use lapply to refactor the columns of each dataframe in the list, then bind all dataframes in the list
aggregate_df <- bind_rows(lapply(wkbk_list, refactor_cols, numeric_cols = numeric_cols, character_cols = character_cols))

#standardize values in character columns
#change minority columns to "Yes" or "No" and clean up loan program factor levels.
non_minority <- c("White/Caucasian", "White.Caucasian", "No")

aggregate_df <- aggregate_df %>%
  mutate(minority = ifelse(minority %in% non_minority, "No", ifelse(is.na(minority), NA, "Yes"))) %>%
  mutate(program = gsub("LOAN", "LN", program),
         program = replace(program, program == "edu", "EDU")) %>%
  replace_all("NA", NA) %>%
  replace_all(c("yes", "YES"), "Yes") %>%
  replace_all(c("no", "NO"), "No") %>%
  replace_all("male", "Male") %>%
  replace_all(c("LN and IDA", "IDA, LN", "LN, EDU"), "LN", cols = "program") %>%
  replace_all("IDA, EDU", "IDA", cols = "program")


#aggregate_complete_survey <- aggregate_df %>%
  #filter(!grepl("Could", survey_cmplt), !is.na(survey_cmplt))

aggregate_df <- aggregate_df %>% mutate_at(character_cols, as.factor) %>%
  mutate(loan_amount = ifelse(is.na(loan_amount), 0, loan_amount),
         intake_sales = ifelse(is.na(intake_sales), 0, intake_sales),
         intake_draw = ifelse(is.na(intake_draw), 0, intake_draw),
         intake_hhincome = ifelse(is.na(intake_hhincome), mean(intake_hhincome), intake_hhincome),
         intake_hhincome)
