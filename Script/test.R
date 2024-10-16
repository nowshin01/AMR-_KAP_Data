# Load Packages
library(tidyverse)
library(dplyr)
library(tidyr)
library(gtsummary)
library(gt)
library(easystats)
library(readxl)
library(ggplot2)
library(MASS)
library(janitor)
library(likert)
#Load Datasets
data<- read_excel("E:/CHIRAL internship/R Programming/AMR-_KAP_Data/AMR_KAP_Data.xlsx")
getwd()
data1<-read_excel("E:/CHIRAL internship/R Programming/AMR-_KAP_Data/Graph.xlsx")

#Descriptive Analysis
data |> 
  dplyr::select(1:10) |> 
  tbl_summary() |> 
  as_gt() |> 
  gtsave("Table/table1.docx") 
  
data |> 
  dplyr::select(41:49) |> 
  tbl_summary() |> 
  as_gt() |> 
  gtsave("Table/table2.docx")

data<-data |> 
  mutate(`Knowledge Level`= case_when(
    `Percentage of Knowledge Score`>=80~"Good",
    `Percentage of Knowledge Score`>=50~"Moderate",
    `Percentage of Knowledge Score`<50~"Poor"
  ))
data<-data |> 
  mutate(`Attitude Level`= case_when(
    `Percentage of Attitude Score` >=80~"Positive",
    `Percentage of Attitude Score`>=50~"Uncertain",
    `Percentage of Attitude Score`<50~"Negative"
  ))
data<-data |> 
  mutate(`Practise Level`= case_when(
    `Percentage of Practise Score`>=80~1,# Good
    `Percentage of Practise Score`<80~0# misuse
    
  ))


data |> 
  dplyr::select(69:71) |> 
  tbl_summary() |> 
  as_gt() |> 
  gtsave("Table/table3.docx")

data <- data |> 
  mutate(`Knowledge Level` = as.factor(`Knowledge Level`))# Ensure that the dependent variable is ordered
data$`Knowledge Level` <- factor(data$`Knowledge Level`, ordered = TRUE)
ordinal_model <- polr(`Knowledge Level` ~ `Parent’s age (years)` + `Parent’s sex` + 
                        `Parent’s education level` + `Employment status` + `Family type` + 
                        `Your average household income per month (BDT)` + `Child’s sex` + 
                        `Child’s age (years)` + `Number of children` + 
                        `Who is the leading child caregiver at home?`, 
                      data = data, Hess = TRUE)
ordinal_model |> 
  tbl_regression() |>
  add_global_p() |> 
  bold_p(t = 0.05) |> 
  as_gt() |> 
  gtsave("Table/table4.docx")
# Count the number of duplicated rows
sum(duplicated(data))
data <- data |> 
  mutate(`Attitude Level`  = as.factor(Attitude Level` ))# Ensure that the dependent variable is ordered
data$`Attitude Level`  <- factor(data$`Attitude Level` , ordered = TRUE)

ordinal_model <- polr(`Attitude Level`  ~ `Parent’s age (years)` + `Parent’s sex` + 
                        `Parent’s education level` + `Employment status` + `Family type` + 
                        `Your average household income per month (BDT)` + `Child’s sex` + 
                        `Child’s age (years)` + `Number of children`,  
                      data = data, Hess = TRUE)
ordinal_model |> 
  tbl_regression() |>
  add_global_p() |> 
  bold_p(t = 0.05) |> 
  as_gt() |> 
  gtsave("Table/table5.docx")

mv_model<-lm(`Practise Level`~ `Parent’s age (years)` + `Parent’s sex` + 
               `Parent’s education level` + `Employment status` + `Family type` + 
               `Your average household income per month (BDT)` + `Child’s sex` + 
               `Child’s age (years)` + `Number of children` + 
               `Attitude Level`+ `Knowledge Level` , family=binomial(link = "logit"), 
             data = data)
mv_model |> 
  tbl_regression() |> 
  bold_p(t=0.05) |> 
  as_gt() |> 
  gtsave("Table/table6.docx")

# Clean column names
data1 <- data1 %>% clean_names()

knowledge_cols <- data1 %>%
  dplyr::select(all_of(c(
    "antibiotic_kills_the_bacteria_yes", 
    "amoxicillin_is_an_antibiotic_yes", 
    "azithromycin_is_an_antibiotic_yes", 
    "paracetamol_is_an_antibiotic_no", 
    "antibiotic_kills_the_virus_no", 
    "antibiotics_used_to_treat_diarrhoea_yes",
    "antibiotics_are_useful_for_flu_and_cough_no",
    "antibiotic_resistant_bacteria_are_difficult_to_treat_yes",
    "misuse_of_antibiotics_can_lead_to_antibiotic_resistant_bacteria_yes",
    "antibiotics_can_cause_allergic_reactions_yes"
    
    
  )))
df1<-data.frame(knowledge_cols)
df2<-mutate_if(df1,is.character,as.factor)
glimpse(df2)
df3<-likert(df2)
plot(df3)

Attitude_cols <- data1 %>%
  dplyr::select(all_of(c(
    "i_will_see_another_doctor_if_the_first_one_has_not_been_prescribed_antibiotics_disagree",
    "i_am_not_satisfied_if_the_doctor_does_not_prescribe_an_antibiotic_to_me_disagree",
    "antibiotics_are_safe_and_hence_can_be_used_commonly_disagree",
    "sick_child_is_given_antibiotics_even_there_is_no_indication_disagree",
    "antibiotics_can_improve_fever_in_children_disagree",
    "a_child_with_cold_is_given_antibiotics_disagree",
    "i_stop_antibiotics_when_my_child_condition_improves_disagree",
    "i_reusing_the_same_antibiotics_for_similar_symptoms_disagree",
    "leftover_antibiotics_are_good_to_keep_at_home_in_case_i_might_need_them_for_my_child_later_on_disagree",
    "doctors_often_take_time_to_inform_parents_how_antibiotics_should_be_used_for_their_children_disagree"
  )))
Attitude_cols <- data1 %>%
  dplyr::select(all_of(c(
    "i_will_see_another_doctor_if_the_first_one_has_not_been_prescribed_antibiotics_disagree",
    "i_am_not_satisfied_if_the_doctor_does_not_prescribe_an_antibiotic_to_me_disagree",
    "antibiotics_are_safe_and_hence_can_be_used_commonly_disagree",
    "sick_child_is_given_antibiotics_even_there_is_no_indication_disagree",
    "antibiotics_can_improve_fever_in_children_disagree",
    "a_child_with_cold_is_given_antibiotics_disagree",
    "i_stop_antibiotics_when_my_child_condition_improves_disagree",
    "i_reusing_the_same_antibiotics_for_similar_symptoms_disagree",
    "leftover_antibiotics_are_good_to_keep_at_home_in_case_i_might_need_them_for_my_child_later_on_disagree",
    "doctors_often_take_time_to_inform_parents_how_antibiotics_should_be_used_for_their_children_disagree"
  )))
df4<-data.frame(Attitude_cols)
df5<-mutate_if(df1,is.character,as.factor)
df6<-likert(df5)
plot(df6)

Practice_cols <- data1 %>%
  dplyr::select(all_of(c("i_give_my_children_antibiotics_no",
                         "i_check_expiring_date_of_antibiotic_before_giving_to_children_yes",
                         "i_seek_medical_advice_before_giving_antibiotic_to_my_children_yes",
                         "i_give_my_children_antibiotics_when_they_get_cough_no",
                         "i_like_to_take_antibiotic_from_pharmacy_instead_of_taking_from_doctor_no",
                         "my_child_should_complete_a_given_dose_even_he_improve_after_2_dose_yes"
  )))
df7<-data.frame(Practice_cols)
df8<-mutate_if(df7,is.character,as.factor)
df9<-likert(df8)
plot(df9)
