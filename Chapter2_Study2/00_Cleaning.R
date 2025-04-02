#### Healthcare Intervention #### 
##### Data Cleaning ##### 

library(tidyverse)

data <- read.csv("Data-Raw_20250225.csv")

# removing tests + unassigned participants 
data <- data[-c(1:10), ] %>% 
  filter(Condition != "")

# removing unfinished participants 
data <- data %>% 
  filter(Finished == 1)

# removing failed patient name manipulation check 
# table(factor(data$manipCheckName, labels = c("Dave", "Mark", "Noah")))

# data <- data %>% 
#   filter(manipCheckName == 1)

# removing unnecessary variables 
data <- data[, -c(10:17,
                  24:27,
                  29:32,
                  34:41,
                  43:46,
                  48:51,
                  54:61,
                  63:66,
                  67:75,
                  77:80,
                  82:86,
                  88:95,
                  97:100,
                  103:106,
                  136:147,
                  151:154,
                  170:173)]

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
write.csv(data, "Data-Clean_Wide_20250225.csv")

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
write_csv(longData, "Data-Clean_Long_20250225.csv")
