#### Healthcare Intervention #### 
##### Data Cleaning ##### 

library(tidyverse)

data <- read.csv("Data-Raw_20241022.csv")

# removing tests + unassigned participants 
data <- data[-c(1:7), ] %>% 
  filter(Condition != "")

# removing unfinished participants 
data <- data %>% 
  filter(Finished == 1)

# removing failed patient name manipulation check 
table(factor(data$manipCheckName, labels = c("Dave", "Mark", "Noah")))

data <- data %>% 
  filter(manipCheckName == 1)

# removing unnecessary variables 
data <- data[, -c(23:26,
                28:35,
                37:44,
                46:49,
                51:58,
                60:63,
                66:69,
                72:75,
                77:84,
                86:89,
                91:94,
                96:103,
                105:108,
                111:114,
                141:152,
                156:159,
                175:178)]

# removing irrelevant roles
data <- data %>% filter(screener2 != 2 |
                        screener2 != 6 |
                        screener2 != 7)

# changing variable types
data$manipCheck2 <- as.numeric(data$manipCheck2)
data$manipCheck1 <- as.numeric(data$manipCheck1)
data$severe <- as.numeric(data$severe)
data$treatmentDose_1_1 <- as.numeric(data$treatmentDose_1_1)
data$sdoh1 <- as.numeric(data$sdoh1)
data$sdoh2 <- as.numeric(data$sdoh2)
data$culturalCompetancy1 <- as.numeric(data$culturalCompetancy1)
data$culturalCompetancy2 <- as.numeric(data$culturalCompetancy2)
data$PFI1_1 <- as.numeric(data$PFI1_1)
data$PFI1_2 <- as.numeric(data$PFI1_2)
data$PFI1_3 <- as.numeric(data$PFI1_3)
data$PFI1_4 <- as.numeric(data$PFI1_4)
data$PFI1_5 <- as.numeric(data$PFI1_5)
data$PFI1_6 <- as.numeric(data$PFI1_6)
data$PFI2_1 <- as.numeric(data$PFI2_1)
data$PFI2_2 <- as.numeric(data$PFI2_2)
data$PFI2_3 <- as.numeric(data$PFI2_3)
data$PFI2_4 <- as.numeric(data$PFI2_4)
data$PFI3_1 <- as.numeric(data$PFI3_1)
data$PFI3_2 <- as.numeric(data$PFI3_2)
data$PFI3_3 <- as.numeric(data$PFI3_3)
data$PFI3_4 <- as.numeric(data$PFI3_4)
data$PFI3_5 <- as.numeric(data$PFI3_5)
data$PFI3_6 <- as.numeric(data$PFI3_6)
data$treatmentDose_2_1 <- as.numeric(data$treatmentDose_2_1)

# recoding variables 
data <- data %>% 
  mutate(manipCheck2 = case_match(manipCheck2, 
                                      1 ~ 6,
                                      2 ~ 5,
                                      3 ~ 4,
                                      4 ~ 3,
                                      5 ~ 2,
                                      6 ~ 1)) %>% 
  mutate(manipCheck1 = case_match(manipCheck1, 
                                  1 ~ 6,
                                  2 ~ 5,
                                  3 ~ 4,
                                  4 ~ 3,
                                  5 ~ 2,
                                  6 ~ 1)) %>% 
  mutate(severe = case_match(severe, 
                             1 ~ 0,
                             2 ~ 1,
                             3 ~ 2,
                             4 ~ 3,
                             5 ~ 4,
                             6 ~ 5,
                             7 ~ 6,
                             8 ~ 7,
                             9 ~ 8,
                             10 ~ 9,
                             11 ~ 10))

## treatment variables ##
data <- 
  data %>% 
  separate(treatment_1, 
           into = c('treat1_1','treat2_1'),
           sep = ',')

data <- 
  data %>% 
  separate(treatment_2, 
           into = c('treat1_2','treat2_2'),
           sep = ',')

data$treat1_1 <- as.numeric(data$treat1_1)
data$treatRecode1_1 <- dplyr::recode(data$treat1_1, `1` = 4, `2` = 4, `4` = 3, `5` = 2, `6` = 2, `7` = 1, `8` = 1)

data$treat2_1 <- as.numeric(data$treat2_1)
data$treatRecode2_1 <- dplyr::recode(data$treat2_1, `1` = 4, `2` = 4, `4` = 3, `5` = 2, `6` = 2, `7` = 1, `8` = 1)

data$treatSum_1 <- data$treatRecode1_1 + data$treatRecode2_1
data$treatAvg_1 <- ((data$treatRecode1_1 + data$treatRecode2_1)/2)

data <- 
  data %>% 
  relocate(c(treatSum_1,treatAvg_1), .after=treat2_1)

data$treat1_2 <- as.numeric(data$treat1_2)

data$treatRecode1_2 <- dplyr::recode(data$treat1_2, `1` = 4, `2` = 4, `4` = 3, `5` = 2, `6` = 2, `7` = 1, `8` = 1)

data$treat2_2 <- as.numeric(data$treat2_2)

data$treatRecode2_2 <- dplyr::recode(data$treat2_2, `1` = 4, `2` = 4, `4` = 3, `5` = 2, `6` = 2, `7` = 1, `8` = 1)

data$treatSum_2 <- data$treatRecode1_2 + data$treatRecode2_2
data$treatAvg_2 <- ((data$treatRecode1_2 + data$treatRecode2_2)/2)

data <- 
  data %>% 
  relocate(c(treatSum_2, treatAvg_2), .after=treat2_2)

data <- 
  data %>% 
  relocate(moveForwardOpen, .after=notes_2)

### write wide data ###
write.csv(data, "Data-Clean_Wide_20250212.csv")

### re-shape to long form ###
data$ResponseId <- as.factor(data$ResponseId)

longData <- 
  data %>%
  pivot_longer(
    cols = treat1_1:notes_2,
    names_to = c(".value", "strike"),
    names_sep = "_",
    values_drop_na = TRUE)

longData$strike <- as.factor(longData$strike)

### save long data ###
write_csv(longData, "Data-Clean_Long_20250212.csv")
