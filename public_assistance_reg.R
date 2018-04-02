source("data_cleanup.R")

test <- read_excel("Data/2016_no_macros.xlsx")

survcmplt_pa <- wkbk %>% filter(intake.pubass == "Yes" & survey.status == "Client completed survey") %>%
  mutate(movedoff = ifelse(survey.pubass == "No", 1, 0),
         intake.hhsize = as.numeric(intake.hhsize))

completed <- completed %>% mutate(movedoff = ifelse(intake.pubass == "Yes" & survey.pubass == "No", 1, 0),
                                  intake.hhsize = as.numeric(intake.hhsize))

####IMPUTE USING AMELIA#####

#complete data#
completed_subselect <- completed %>% select(-ends_with("wage"))

idvars = completed_subselect %>% select(-intake.biz, -intake.hhincome, -intake.hhsize, -intake.pubass, -Dollars.loans, -survey.pubass, -gender, -race) %>% 
  colnames()

noms = c("intake.pubass", "survey.pubass", "gender", "race", "intake.biz")

imputed <- amelia(completed_subselect %>% as.data.frame(), idvars = idvars, noms = noms, ords = "intake.hhsize", bounds = c(22,0,99999))

imputed$imputations[c(3,4,5)] <- map(imputed$imputations[c(3,4,5)], ~mutate(.x, movedoff = ifelse(intake.pubass == "Yes" & survey.pubass == "No", 1, 0)))

#only public assistance#

survcmplt_pa <- survcmplt_pa %>% 
  select(-ends_with("wage"), -(52:97), -`Please specify other`, -intake.pubass) 

idvars = survcmplt_pa %>% select(-intake.biz, -intake.hhincome, -intake.hhsize, -gender, -race, -intake.outsideemployment) %>% 
  colnames()

noms = c("gender", "race", "intake.biz", "intake.outsideemployment")

imputed <- amelia(survcmplt_pa %>% as.data.frame(), idvars = idvars, noms = noms, ords = "intake.hhsize")

imputed$imputations[c(3,4,5)] <- map(imputed$imputations[c(3,4,5)], ~mutate(.x, movedoff = ifelse(intake.pubass == "Yes" & survey.pubass == "No", 1, 0)))

#########################################

for(i in 1:length(imputed$imputations)){
  print(i)
  imputed$imputations[[i]] <- imputed$imputations[[i]] %>% mutate(movedoff = ifelse(intake.pubass == "Yes" & survey.pubass == "No", 1, 0))
}

logmod <- glm(movedoff ~ intake.hhincome + intake.hhsize + survey.biz.at.intake, data = survcmplt_pa, family = binomial(link = "logit"))

logmod_complete = glm(movedoff ~ intake.hhincome +intake.hhsize + intake.biz + intake.outsideemployment + race + gender, data = imputed$imputations$imp5, family = binomial(link = "logit"))
summary(logmod_complete)


logit_mod <- glm(biz.survive ~ race + gender + survey.biz.at.intake + intake.hhincome + Dollars.loans, data = completed, family = binomial(link = "logit"))

####Current Problem: Non-Invertible Matrices when running amelia, causes problems when creating response variable####

####Impute Complete DF####

idvars = aggregate_df %>% select(-intake_hhincome, -intake_hhsize, -intake_biz, -gender, - minority, -loan_amount) %>% 
  colnames()

noms= c("gender", "minority", "intake_biz")

imputed <- amelia(as.data.frame(aggregate_df), idvars = idvars, noms = noms)

survcmplt_pa <- aggregate_df %>% filter(intake.pubass == "Yes") %>%
  mutate(movedoff = ifelse(survey_pubass == "No", 1, 0))

surv_biz_start <- aggregate_df %>% filter(intake_biz == "No") %>%
  mutate(biz_start= ifelse(survey_biz == "Yes", 1, 0))

logmod_pa <- glm(movedoff ~ intake_hhincome + intake_hhsize + intake_biz + gender + minority + loan_amount, data = survcmplt_pa, family = binomial(link = "logit"))
logmod_biz <- glm(biz_start ~ intake_hhincome + intake_hhsize + intake_outemploy + gender + minority + loan_amount, data = surv_biz_start, family = binomial(link = "logit"))
