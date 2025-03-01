#### Creating Study 5 Dataset ####
### SSK ###

##### Long Form #####
study1 <- read.csv("Data_Processed_Study1.csv")
study2 <- read.csv("Data_Processed_Study2.csv")
study3 <- read.csv("Data_Processed_Study3.csv")
study4 <- read.csv("Data_Processed_Study4.csv")

study1$study <- 1

study1 <- study2[-c(1,4:7)]

study1 <- 
  dplyr::rename(study1, 
                patient=condition,
                age_Self=age,
                gender_Self=gender,
                ladder_Self=ladder,
                income_Self=income,
                race_Self=race,
                politics_Self=politics,
                healthLit_Patient=healthLiteracyDave,
                insurance_Patient=insuranceDave,
                ed_Own=edOwn,
                ed_Mom=edMom,
                ed_Dad=edDad,
                ed_Patient=edDave)

study2 <- study2 %>% 
  filter(Condition=="Control")

study2$study <- 2

study2$manipulationCheck <- as.character(study2$manipulationCheck)

study2 <- 
  dplyr::rename(study2,
                politics_Self=politics_Own,
                patient=patientSESRace)

study2$gender_Self <- as.factor(study2$gender_Self)
levels(study2$gender_Self) <- c("Male", "Female", "Trans", "Non-Conform", "Other")

study12 <- bind_rows(study1, study2)

study3$study <- 3

study3$patient <- case_when(study3$Condition == "BlackHighMale" ~ "Black High SES",
                            study3$Condition == "BlackHighFemale" ~ "Black High SES",
                            study3$Condition == "BlackLowMale" ~ "Black Low SES",
                            study3$Condition == "BlackLowFemale" ~ "Black Low SES",
                            study3$Condition == "WhiteHighMale" ~ "White High SES",
                            study3$Condition == "WhiteHighFemale" ~ "White High SES",
                            study3$Condition == "WhiteLowMale" ~ "White Low SES",
                            study3$Condition == "WhiteLowFemale" ~ "White Low SES",
                            TRUE ~ "NA")

study3$conditionSES <- case_when(study3$SES == "High" ~ "High SES",
                                 study3$SES == "Low" ~ "Low SES",
                                 TRUE ~ "NA")

study3 <- 
  dplyr::rename(study3,
                conditionRace=Race,
                politics_Self=politics_Own)

study3$gender_Self <- as.factor(study3$gender_Self)
levels(study3$gender_Self) <- c("Male", "Female", "Trans", "Non-Conform", "Other")

study3_male <- study3 %>% 
  filter(Gender=="Male")

data_male <- bind_rows(study12, study3_male)

data <- bind_rows(study12, study3)

study4$study <- 4

study4 <- study4[-c(1,1:3)]

study4$conditionRace <- "White"

study4$conditionSES <- case_when(study4$SES == "High" ~ "High SES",
                                 study4$SES == "Low" ~ "Low SES",
                                 TRUE ~ "NA")

study4$gender_Self <- as.factor(study4$gender_Self)
levels(study4$gender_Self) <- c("Male", "Female", "Trans", "Non-Conform", "Other")

data_FINAL <- bind_rows(data, study4)

write.csv(data_FINAL, "Data_Processed_Study5.csv")

##### Wide Form ####
study1_wide <- read.csv("Data_Processed_Study1-Wide.csv")
study2_wide <- read.csv("Data_Processed_Study2-Wide.csv")
study3_wide <- read.csv("Data_Processed_Study3-Wide.csv")
study4_wide <- read.csv("Data_Processed_Study4-Wide.csv")

study1_wide$study <- 1

study1_wide <- 
  dplyr::rename(study1_wide, 
                patient=condition,
                age_Self=age,
                gender_Self=gender,
                ladder_Self=ladder,
                income_Self=income,
                race_Self=race,
                politics_Self=politics,
                healthLit_Patient=healthLiteracyDave,
                insurance_Patient=insuranceDave,
                ed_Own=edOwn,
                ed_Mom=edMom,
                ed_Dad=edDad,
                ed_Patient=edDave)

study2_wide <- study2_wide %>% 
  filter(Condition == "Control")

study2_wide$study <- 2

study2_wide$manipulationCheck <- as.character(study2_wide$manipulationCheck)

study2_wide <- 
  dplyr::rename(study2_wide,
                politics_Self=politics_Own,
                patient=patientSESRace)

study2_wide$gender_Self <- as.factor(study2_wide$gender_Self)
levels(study3_wide$gender_Self) <- c("Male", "Female", "Trans", "Non-Conform", "Other")

study12_wide <- bind_rows(study1_wide, study2_wide)

study3_wide$study <- 3

study3_wide$patient <- case_when(study3_wide$Condition == "BlackHighMale" ~ "Black High SES",
                                 study3_wide$Condition == "BlackHighFemale" ~ "Black High SES",
                                 study3_wide$Condition == "BlackLowMale" ~ "Black Low SES",
                                 study3_wide$Condition == "BlackLowFemale" ~ "Black Low SES",
                                 study3_wide$Condition == "WhiteHighMale" ~ "White High SES",
                                 study3_wide$Condition == "WhiteHighFemale" ~ "White High SES",
                                 study3_wide$Condition == "WhiteLowMale" ~ "White Low SES",
                                 study3_wide$Condition == "WhiteLowFemale" ~ "White Low SES",
                                 TRUE ~ "NA")

study3_wide$conditionSES <- case_when(study3_wide$SES == "High" ~ "High SES",
                                      study3_wide$SES == "Low" ~ "Low SES",
                                      TRUE ~ "NA")

study3_wide <- 
  dplyr::rename(study3_wide,
                conditionRace=Race,
                politics_Self=politics_Own)

study3_wide$gender_Self <- as.factor(study3_wide$gender_Self)
levels(study3_wide$gender_Self) <- c("Male", "Female", "Trans", "Non-Conform", "Other")

study4_wide_male <- study3_wide %>% 
  filter(Gender=="Male")

data_wide_male <- bind_rows(study12_wide, study3_wide_male)

data_wide <- bind_rows(study12_wide, study3_wide)

study4_wide$study <- 4

study4_wide$conditionRace <- "White"

study4_wide$conditionSES <- case_when(study4_wide$SES == "High" ~ "High SES",
                                      study4_wide$SES == "Low" ~ "Low SES",
                                      TRUE ~ "NA")

study4_wide$gender_Self <- as.factor(study4_wide$gender_Self)
levels(study4_wide$gender_Self) <- c("Male", "Female", "Trans", "Non-Conform", "Other")

data_wide_FINAL <- bind_rows(data_wide, study4_wide)

write.csv(data_wide_FINAL, "Data_Processed_Study5-Wide.csv")
