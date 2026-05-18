
####  title: "FFCW Data processing"
#### author: "Yayouk & Muna"
#### date: "2023-06-19"



############################# Load Packages #############################
install.packages("haven")
install.packages("psych")
install.packages("ltm")

library(haven)
library(psych)
library(ltm)


############################# Baseline #############################

############################# Read in data #######################
baseline <- read_dta("/Volumes/MPRG-Biosocial/Projects/03_data/003_FFCW/FFCWS Public data/Baseline/FF_wave1_2020v2.dta")
colnames(baseline)

############################# Process Data #############################
baseline_select <- baseline[,c("idnum",
                               "cm1bsex")]
colnames(baseline_select)

describe(baseline_select$cm1bsex)
table(baseline_select$cm1bsex)

#set all missings to NA
baseline_select$cm1bsex <- ifelse(baseline_select$cm1bsex < 0, NA, baseline_select$cm1bsex)

table(baseline_select$cm1bsex)
#1=boy, 2=girl
#1    2 
#2556 2341


############################# Age 15 #############################

############################# Read in data #######################

#here we read in the FFCW dataset with year 15 data and select the variables of interest
age15 <- read_dta("/Volumes/MPRG-Biosocial/Projects/03_data/003_FFCW/FFCWS Public data/Year 15/FF_wave6_2020v2.dta")

############################# Process Data #############################

#select items of interest for the Mental Health Scales
age15_select <- age15[,c( "idnum",
                          "p6b36",
                          "p6b40", 
                          "p6b52",
                          "p6b53", 
                          "p6b54",
                          "p6b68", 
                          "p6b65", 
                          "p6b66", 
                          "p6b35", 
                          "p6b37", 
                          "p6b38", 
                          "p6b39",
                          "p6b41",
                          "p6b42", 
                          "p6b43",
                          "p6b44", 
                          "p6b45", 
                          "p6b57",
                          "p6b59", 
                          "p6b49",
                          "p6b50",
                          "p6b51",
                          "p6b60", 
                          "p6b61",
                          "p6b62", 
                          "p6b63", 
                          "p6b64",
                          "p6b67")]
                          
#double check if selection went allright
colnames(age15_select)
describe(age15_select)


#check how missings are coded across CBCL items
# Create a function to run table() for each column
run_table <- function(column) {
  table_result <- table(column)
  return(table_result)
}

# Apply the function to each column using lapply()
result_list15 <- lapply(age15_select, run_table)
result_list15

#present missings are -9 (not in wave), -2 (dont know), -1 (refuse to answer)
#and answers are coded 1= not true, 2=sometimes true, 3=often true
#so we recode all items so that -9, -2, and -1 = NA

# Replace specific values with NA
age15_select[age15_select < 0] <- NA
describe(age15_select)

#double check if it min for all items is >0
#it is, so recoded correctly
#all items have positive values, so all negative values have been properly recoded
describe(age15_select)

#### Create Internalizing Age 15 Score ####

#Create sumscore for parent-reported Internalizing problems age 15
#omit those with missings (na.rm=FALSE)

age15_select$Int15 <- rowMeans(age15_select[, c("p6b36",
                                     "p6b40",
                                     "p6b52",
                                     "p6b53", 
                                     "p6b54",
                                     "p6b68", 
                                     "p6b65",
                                     "p6b66")], na.rm=FALSE) 

#check if data is in line with expectations
# Min score should be 1, Max score should be 3. 
# data is in line with expectations
describe(age15_select$Int15)  

#Have a look at the data
#strong skew to the left, as most people do not show any problems
hist(age15_select$Int15)
table(age15_select$Int15)

#check how many missings
age15_select$missing_Int15 <- rowSums(is.na(age15_select[c("p6b36",
                                                 "p6b40",
                                                 "p6b52",
                                                 "p6b53", 
                                                 "p6b54",
                                                 "p6b68", 
                                                 "p6b65",
                                                 "p6b66")]))
#<100 have 1, or 2 missings
#0    1    2    3    8 
#3516   51   11    2 1318 
table(age15_select$missing_Int15)

#create a log score
age15_select$Int15_log <- log(age15_select$Int15 + 1)  

#still looks somewhat skewed
hist(age15_select$Int15_log)  
describe(age15_select$Int15_log)

#the Internalizing scale at age 15 consists of 8 items, in line with codebook see p.49
#check cronbach alpha
#.787
CA_15_int <- age15_select[,c("p6b36",
                               "p6b40",
                               "p6b52",
                               "p6b53", 
                               "p6b54",
                               "p6b68", 
                               "p6b65",
                               "p6b66")]
cronbach.alpha(CA_15_int, na.rm=TRUE)  



#### Create Externalizing Age 15 Score ####

age15_select$Ext15 <- rowMeans(age15_select[,c(
                              "p6b35", 
                              "p6b37", 
                              "p6b38", 
                              "p6b39", 
                              "p6b41",
                              "p6b42", 
                              "p6b43", 
                              "p6b44", 
                              "p6b45", 
                              "p6b57", 
                              "p6b59", 
                              "p6b49", 
                              "p6b50", 
                              "p6b51", 
                              "p6b60", 
                              "p6b61", 
                              "p6b62", 
                              "p6b63", 
                              "p6b64",
                              "p6b67")], na.rm=FALSE) 

#check how many missings
age15_select$missing_Ext15 <- rowSums(is.na(age15_select[c("p6b35", 
                                                           "p6b37", 
                                                           "p6b38", 
                                                           "p6b39", 
                                                           "p6b41",
                                                           "p6b42", 
                                                           "p6b43", 
                                                           "p6b44", 
                                                           "p6b45", 
                                                           "p6b57", 
                                                           "p6b59", 
                                                           "p6b49", 
                                                           "p6b50", 
                                                           "p6b51", 
                                                           "p6b60", 
                                                           "p6b61", 
                                                           "p6b62", 
                                                           "p6b63", 
                                                           "p6b64",
                                                           "p6b67")]))
#<100 miss 1 or 2 items
table(age15_select$missing_Ext15)

#check if data is in line with expectations
# data is in line with expectations
describe(age15_select$Ext15)

#Have a look at the data
#strong skew to the left, as most people do not show any problems
hist(age15_select$Ext15)
table(age15_select$Ext15)

#create a log score
age15_select$Ext15_log <- log(age15_select$Ext15 + 1)  

#still looks somewhat skewed
hist(age15_select$Ext15_log)  
describe(age15_select$Ext15_log)

#check cronbach alpha
#.885
CA_15_ext <- age15_select[,c("p6b35", 
                              "p6b37", 
                              "p6b38", 
                              "p6b39", 
                              "p6b41",
                              "p6b42", 
                              "p6b43", 
                              "p6b44", 
                              "p6b45", 
                              "p6b57", 
                              "p6b59", 
                              "p6b49", 
                              "p6b50", 
                              "p6b51", 
                              "p6b60", 
                              "p6b61", 
                              "p6b62", 
                              "p6b63", 
                              "p6b64",
                              "p6b67")]
cronbach.alpha(CA_15_ext, na.rm=TRUE) 



#### Coding Muna
#### Smoking,  Race, Skintone, Depressive/Anxiety symptoms age 15 ######

age15_cov <- age15[,c("idnum",
                      "k6d40", #smoke child
                      "p6h74", #smoke parent
                      "ck6ethrace", #race
                      "h6a8", #skintone
                      "cp6yagem", #age in months
                      "k6d2c", #depressive symptoms
                      "k6d2n", 
                      "k6d2s", 
                      "k6d2x", 
                      "k6d2ac",
                      "k6d2d", #anxiety symptoms
                      "k6d2j",
                      "k6d2t", 
                      "k6d2ag", 
                      "k6d2ai", 
                      "k6d2ak",
                      "p6d32", #stress parenting 
                      "p6d33", 
                      "p6d34", 
                      "p6d35",
                      "k6c17", #parent-child relationship
                      "k6c18",
                      "k6e10", #ever stopped by police
                      "k6e16", #know someone stopped by police
                      "o6a1", #neighborhood conditions
                      "o6a2",
                      "o6a3", 
                      "o6a4",
                      "o6a5", 
                      "o6a9",
                      "ck6bmip")] #bmi
                      
describe(age15_cov)

# Set negative values in the dataframe to NA
age15_cov[age15_cov< 0] <- NA

#no negative values anymore in the descriptives
describe(age15_cov)

#### Smoking age 15 ####
#we code smoking as following:
#Self-reported adolescents smoking andcaregiver smokingwill be counted as true if they reported smoking 
#at any measurement occasion (1=ever smoked, 0=never smoked).

#we use the following items:
#Ever smoked an entire cigarette	15	Child	Child	k6d40	1=yes, 2=no
#Ever regularly smoked cigarettes?	15	Primary Caregiver	PCG	p6h74	1=yes, 2=no

#check smoking
table(age15_cov$k6d40)
table(age15_cov$p6h74)

# Create the eversmoke column
age15_cov$eversmoke <- ifelse(age15_cov$p6h74 == 1 | age15_cov$k6d40 == 1, 1, 0)

#so we created a scale with  0=never smoked, 1=ever smoked
table(age15_cov$eversmoke)
table(age15_cov$eversmoke, age15_cov$k6d40)
table(age15_cov$eversmoke, age15_cov$p6h74)


####  Age 15 in months ####
names(age15_cov)[names(age15_cov) == "cp6yagem"] <- "Age15_months"


#### Race and Skincolour Age 15  ####

### Youth self-description of race/ethnicity: 1 = "White",2 = "Black/Af. American only, non-hispanic", 3 = "Hispanic/Latino", 4 ="Other only, non-hispanic",5 = "Multi-racial, non-hispanic"
#check race
table(age15_cov$ck6ethrace)

# Create dummy variables for each race group
age15_cov$race_white <- ifelse(age15_cov$ck6ethrace == 1, 1, 0)
age15_cov$race_black <- ifelse(age15_cov$ck6ethrace == 2, 1, 0)
age15_cov$race_hispanic <- ifelse(age15_cov$ck6ethrace == 3, 1, 0)
age15_cov$race_other <- ifelse(age15_cov$ck6ethrace == 4, 1, 0)
age15_cov$race_multi <- ifelse(age15_cov$ck6ethrace == 5, 1, 0)

### Depressive symptoms age 15 ####

#recode so that higher scores = more depressive symptoms

#reverse code some of the variables
# Reverse code variable k6d2c
age15_cov$k6d2c_reversed <- 5 - age15_cov$k6d2c

# View the reversed variable
table(age15_cov$k6d2c_reversed,age15_cov$k6d2c)


# Reverse code variable k6d2n
age15_cov$k6d2n_reversed <- 5 - age15_cov$k6d2n
# View the reversed variable
table(age15_cov$k6d2n_reversed,age15_cov$k6d2n)


# Reverse code variable k6d2x
age15_cov$k6d2x_reversed <- 5 - age15_cov$k6d2x
# View the reversed variable
table(age15_cov$k6d2x_reversed,age15_cov$k6d2x)


# Reverse code variable k6d2ac
age15_cov$k6d2ac_reversed <- 5 - age15_cov$k6d2ac

# View the reversed variable
table(age15_cov$k6d2ac_reversed,age15_cov$k6d2ac)

describe(age15_cov$k6d2ac_reversed)
describe(age15_cov$k6d2s)
describe(age15_cov$k6d2x_reversed)
describe(age15_cov$k6d2ac_reversed)

### create the meanscore for depressive symptoms

selected_vars <- c("k6d2c_reversed", "k6d2n_reversed", "k6d2s", "k6d2x_reversed", "k6d2ac_reversed")


# Compute the scale total score: rowMeans() function is used to compute the mean score for each row, considering the missing values (na.rm = TRUE).
#later we set those with >2 to missing in line with the codebook
age15_cov$scale_cesd <- rowMeans(age15_cov[selected_vars], na.rm = TRUE)

#sample size =3437 when creating meanscore including NA
describe(age15_cov$scale_cesd)

# Identify cases with more than 2 missing values and set them to NA
age15_cov$scale_cesd[rowSums(is.na(age15_cov[selected_vars])) > 2] <- NA

#sample size =3436 when creating meanscore excluding >2 missings
describe(age15_cov$scale_cesd)


# View the total score variable
describe(age15_cov$scale_cesd)

# Create a subset of the data with the 5 items
subset_data <- age15_cov[, c("k6d2c_reversed", "k6d2n_reversed", "k6d2s", "k6d2x_reversed", "k6d2ac_reversed")]

describe(subset_data)

#checking reliability of the scale with Cronbach Alpha
# Install and load the 'ltm' package
install.packages("ltm")
library(ltm)

#cronbach alpha = .759
cronbach.alpha(subset_data, na.rm=TRUE)

#check distribution depression score
describe(age15_cov$scale_cesd)
hist(age15_cov$scale_cesd)

#create a log score
age15_cov$cesd_log <- log(age15_cov$scale_cesd + 1)  

#still looks somewhat skewed
hist(age15_cov$cesd_log)  
describe(age15_cov$cesd_log)


### Anxiety symptoms scale age 15 ####

summary(age15_cov[c("k6d2d", "k6d2j", "k6d2t", "k6d2ag", "k6d2ai", "k6d2ak")])


#recode so that higher scores = more anxiety symptoms

#reverse code some of the variables
# Reverse code variable k6d2d
age15_cov$k6d2d_reversed <- 5 - age15_cov$k6d2d

# View the reversed variable
table(age15_cov$k6d2d_reversed,age15_cov$k6d2d)
table(age15_cov$k6d2d)
table(age15_cov$k6d2d_reversed)

# Reverse code variable k6d2j
age15_cov$k6d2j_reversed <- 5 - age15_cov$k6d2j
# View the reversed variable
table(age15_cov$k6d2j_reversed,age15_cov$k6d2j)

# Reverse code variable k6d2t
age15_cov$k6d2t_reversed <- 5 - age15_cov$k6d2t
# View the reversed variable
table(age15_cov$k6d2t_reversed,age15_cov$k6d2t)

# Reverse code variable k6d2ag
age15_cov$k6d2ag_reversed <- 5 - age15_cov$k6d2ag
# View the reversed variable
table(age15_cov$k6d2ag_reversed,age15_cov$k6d2ag)

# Reverse code variable k6d2ai
age15_cov$k6d2ai_reversed <- 5 - age15_cov$k6d2ai
# View the reversed variable
table(age15_cov$k6d2ai_reversed,age15_cov$k6d2ai)

# Reverse code variable k6d2ak
age15_cov$k6d2ak_reversed <- 5 - age15_cov$k6d2ak
# View the reversed variable
table(age15_cov$k6d2ak_reversed,age15_cov$k6d2ak)


selected_vars <- c("k6d2d_reversed", "k6d2j_reversed", "k6d2t_reversed", "k6d2ag_reversed", "k6d2ai_reversed", "k6d2ak_reversed")

# Compute the scale total score: rowMeans() function is used to compute the mean score for each row, considering the missing values (na.rm = TRUE).
age15_cov$scale_anxt <- rowMeans(age15_cov[selected_vars], na.rm = TRUE)

#sample size =3437 when creating meanscore including NA
describe(age15_cov$scale_anxt)

# Identify cases with more than 2 missing values and set them to NA
age15_cov$scale_anxt[rowSums(is.na(age15_cov[selected_vars])) > 2] <- NA

#sample size =3436 when creating meanscore excluding >2 missings
describe(age15_cov$scale_anxt)


# View the total score variable
describe(age15_cov$scale_anxt)
head(age15_cov$scale_anxt)
hist(age15_cov$scale_anxt)


# Create a subset of the data with the 6 items
subset_data <- age15_cov[, c("k6d2d", "k6d2j", "k6d2t", "k6d2ag", "k6d2ai", "k6d2ak")]
describe(subset_data)

#cronbach alpha = .76
cronbach.alpha(subset_data, na.rm=TRUE)

#create a log score
age15_cov$scale_anxt_log <- log(age15_cov$scale_anxt + 1)  

#still looks somewhat skewed
hist(age15_cov$scale_anxt_log)  
describe(age15_cov$scale_anxt_log)

################################################# PARENTING ################################################################

#Parenting Stress AGE 15 - Variables: "p6d32", "p6d33", "p6d34", "p6d35"


# Display variable summaries using the summary() function
summary(age15_cov[c("p6d32", "p6d33", "p6d34", "p6d35")])

### Reverse Code items - the higher  - the more stressed parent is

# Reverse code variable p6d32
age15_cov$p6d32_reversed <- 5 - age15_cov$p6d32

# View the reversed variable
table(age15_cov$p6d32_reversed,age15_cov$p6d32)
table(age15_cov$p6d32_reversed)


# Reverse code variable p6d33
age15_cov$p6d33_reversed <- 5 - age15_cov$p6d33
# View the reversed variable
table(age15_cov$p6d33_reversed,age15_cov$p6d33)

# Reverse code variable p6d34
age15_cov$p6d34_reversed <- 5 - age15_cov$p6d34
# View the reversed variable
table(age15_cov$p6d34_reversed,age15_cov$p6d34)
table(age15_cov$p6d34_reversed)

# Reverse code variable p6d35
age15_cov$p6d35_reversed <- 5 - age15_cov$p6d35

# View the reversed variable
table(age15_cov$p6d35_reversed,age15_cov$p6d35)
table(age15_cov$p6d35_reversed)


### SCORING: Items can be averaged to create a scale for aggravation in parenting.

# Create a vector of the selected variables
selected_vars <- c("p6d32_reversed", "p6d33_reversed", "p6d34_reversed", "p6d35_reversed")

# Calculate the mean score
age15_cov$parent_stress_age15 <- rowMeans(age15_cov[selected_vars], na.rm = FALSE)

#sample size =3579 when creating meanscore including NA
describe(age15_cov$parent_stress_age15)

#sample size =3577 when creating meanscore excluding >1 missing
describe(age15_cov$parent_stress_age15)
hist(age15_cov$parent_stress_age15)


### Cronbach's Alpha - Scale– Aggravation in Parenting

# Create a subset of the data with the 4 items
subset_data <- age15_cov[, c("p6d32_reversed", "p6d33_reversed", "p6d34_reversed", "p6d35_reversed")]
describe(subset_data)

#cronbach alpha = .681
cronbach.alpha(subset_data, na.rm=TRUE)


####### Child-Caregiver relationship Age 15####

#Child-Caregiver relationship (Child responding about mother) AGE 15 - Variables: "k6c17", "k6c18"

# Reverse code variable k6c17 - the higher - the more close to parent
age15_cov$k6c17_reversed <- 5 - age15_cov$k6c17
# View the reversed variable
table(age15_cov$k6c17_reversed,age15_cov$k6c17)

# Reverse code variable k6c18 - the higher - the more sahre ideas with parent 
age15_cov$k6c18_reversed <- 5 - age15_cov$k6c18
# View the reversed variable
age15_cov$k6c18_reversed

table(age15_cov$k6c18_reversed,age15_cov$k6c18)
table(age15_cov$k6c18_reversed)
table(age15_cov$k6c17_reversed)

# Calculate the mean of the variables
age15_cov$Close15 <- rowMeans(age15_cov[, c("k6c17_reversed", "k6c18_reversed")], na.rm = FALSE)
describe(age15_cov$Close15)

############# POLICE INTERACTION age 15 #######

### DIRECT POLICeE STOPS (n=918): Relevant question: E10. Ever been stopped by the police? - "k6e10" - RECODE 1=1 and 2=0 !!!
# Yes = 1, No = 2 - Variables (Teen stopped by police): "k6e10"

table(age15_cov$k6e10)

# Recode variable k6e10 to have Yes = 1, No = 0
age15_cov$k6e10 <- ifelse(age15_cov$k6e10 == 1, 1, 0)
table(age15_cov$k6e10)

### VICARIOUS STOPS
### k6e16" (know someone) - # RECODE TO Yes = 1, No = 0

table(age15_cov$k6e16)

# Recode variable k6e16 to have Yes = 1, No = 0
age15_cov$k6e16 <- ifelse(age15_cov$k6e16 == 1, 1, 0)
table(age15_cov$k6e16)


######## NEIGHBORHOOD CONDITIONS age 15 ######


# Higher number (4) worse conditions of the neighborhood 
# Neighborhood conditions AGE 15 - variables: "o6a1", "o6a2", "o6a3", "o6a4", "o6a5", "o6a9"

# Calculate the mean score of the selected items
mean_score_ngh15 <- rowMeans(age15_cov[, c("o6a1", "o6a2", "o6a3", "o6a4", "o6a5", "o6a9")], na.rm = FALSE)

# Add the mean score as a new column in the dataset
age15_cov$mean_score_ngh15 <- mean_score_ngh15

# Print the mean score
describe(age15_cov$mean_score_ngh15)

# Create a subset of the data with the 4 items
subset_data <- age15_cov[, c("o6a1", "o6a2", "o6a3", "o6a4", "o6a5", "o6a9")]
describe(subset_data)

#cronbach alpha = .79
cronbach.alpha(subset_data, na.rm=TRUE)

##### BMI ######

#Variable AGE 15 "ck6bmip"
describe(age15_cov$ck6bmip)







############################# Age 9 #############################

############################# Read in data #######################

#here we read in the FFCW dataset with year 15 data and select the variables of interest
age9 <- read_dta("/Volumes/MPRG-Biosocial/Projects/03_data/003_FFCW/FFCWS Public data/Year 9/FF_wave5_2020v2.dta")

############################# Process Data #############################

#select items of interest
age9_select <- age9[,c("idnum",
                        "p5q3c",
                        "p5q3o",
                        "p5q3r",
                        "p5q3s",
                        "p5q3t",
                        "p5q3u",
                        "p5q3v",
                        "p5q3aj",
                        "p5q3bc",
                        "p5q3bn",
                        "p5q3cf",
                        "p5q3cg",
                        "p5q3ch",
                        "p5q3ci",
                        "p5q3cn",
                        "p5q3co",
                        "p5q3cq",
                        "p5q3cw",
                        "p5q3b",
                        "p5q3x",
                        "p5q3aa",
                        "p5q3al",
                        "p5q3ap",
                        "p5q3bi",
                        "p5q3bm",
                        "p5q3br",
                        "p5q3bs",
                        "p5q3bz",
                        "p5q3ca",
                        "p5q3cj",
                        "p5q3cp",
                        "p5q3cr",
                        "p5q3ct",
                        "p5q3cx",
                        "p5q3cy",
                        "p5q3m",
                        "p5q3ab",
                        "p5q3ac",
                        "p5q3ad",
                        "p5q3ae",
                        "p5q3af",
                        "p5q3ah",
                        "p5q3aq",
                        "p5q3av",
                        "p5q3ax",
                        "p5q3bq",
                        "p5q3ck",
                        "p5q3db",
                        "p5q3as",
                        "p5q3au",
                        "p5q3aw",
                        "p5q3az",
                        "p5q3bb1",
                        "p5q3bb2",
                        "p5q3bb3",
                        "p5q3bb4",
                        "p5q3bb5",
                        "p5q3bb6",
                        "p5q3bb7",
                        "p5q3e",
                        "p5q3ao",
                        "p5q3bk",
                        "p5q3bo",
                        "p5q3bu",
                        "p5q3cu",
                        "p5q3cv",
                        "p5q3da")]
colnames(age9_select)                        


#check how missings are coded across CBCL items
# Create a function to run table() for each column
run_table <- function(column) {
  table_result <- table(column)
  return(table_result)
}


# Apply the function to each column using lapply()
result_list9 <- lapply(age9_select, run_table)
result_list9


#present missings are -9 (not in wave),-3 (missing), -2 (dont know), -1 (refuse to answer)
#and answers are coded 1= not true, 2=sometimes true, 3=often true
#so we recode all items so that -9, -2, and -1 = NA

# Replace specific values with NA
age9_select[age9_select < 0] <- NA

#double check if it min for all items is >0
#it is, so recoded correctly
describe(age9_select)

#### Create Internalizing Age 9 Score ####
#Total internalizing includes all items from anxious/depressed scale, all items from the somatic complaints scale and all items from the withdrawn/depressed scale.
#consists of 32 items

age9_select$Int9 <- rowMeans(age9_select[,c("p5q3m",
                                             "p5q3ab",
                                             "p5q3ac",
                                             "p5q3ad",
                                             "p5q3ae",
                                             "p5q3af",
                                             "p5q3ah",
                                             "p5q3aq",
                                             "p5q3av",
                                             "p5q3ax",
                                             "p5q3bq",
                                             "p5q3ck",
                                             "p5q3db",
                                             "p5q3as",
                                             "p5q3au",
                                             "p5q3aw",
                                             "p5q3az",
                                             "p5q3bb1",
                                             "p5q3bb2",
                                             "p5q3bb3",
                                             "p5q3bb4",
                                             "p5q3bb5",
                                             "p5q3bb6",
                                             "p5q3bb7",
                                             "p5q3e",
                                             "p5q3ao",
                                             "p5q3bk",
                                             "p5q3bo",
                                             "p5q3bu",
                                             "p5q3cu",
                                             "p5q3cv",
                                             "p5q3da")], na.rm=FALSE) 

#check how many missings
age9_select$missing_Int9 <- rowSums(is.na(age9_select[c("p5q3m",
                                                           "p5q3ab",
                                                           "p5q3ac",
                                                           "p5q3ad",
                                                           "p5q3ae",
                                                           "p5q3af",
                                                           "p5q3ah",
                                                           "p5q3aq",
                                                           "p5q3av",
                                                           "p5q3ax",
                                                           "p5q3bq",
                                                           "p5q3ck",
                                                           "p5q3db",
                                                           "p5q3as",
                                                           "p5q3au",
                                                           "p5q3aw",
                                                           "p5q3az",
                                                           "p5q3bb1",
                                                           "p5q3bb2",
                                                           "p5q3bb3",
                                                           "p5q3bb4",
                                                           "p5q3bb5",
                                                           "p5q3bb6",
                                                           "p5q3bb7",
                                                           "p5q3e",
                                                           "p5q3ao",
                                                           "p5q3bk",
                                                           "p5q3bo",
                                                           "p5q3bu",
                                                           "p5q3cu",
                                                           "p5q3cv",
                                                           "p5q3da")]))
# n=218 have 1, n=42 have 2 missing
#the FFCW guide:
#It should be noted that scale scores are only calculated for participants with responses to each variable in the scale.
#When a participant responds with don’t know, refuse, or missing to any variable on a given scale, their scale score will be missing.
table(age9_select$missing_Int9)

#check if data is in line with expectations
#min 1 max 3
# data is in line with expectations
describe(age9_select$Int9)

#Have a look at the data
#strong skew to the left, as most people do not show any problems
hist(age9_select$Int9)
table(age9_select$Int9)

#create a log score
age9_select$Int9_log <- log(age9_select$Int9 + 1)  

#still looks somewhat skewed
hist(age9_select$Int9_log)  
describe(age9_select$Int9_log)

#check cronbach alpha
#.877
CA_9_int <- age9_select[,c("p5q3m",
                           "p5q3ab",
                           "p5q3ac",
                           "p5q3ad",
                           "p5q3ae",
                           "p5q3af",
                           "p5q3ah",
                           "p5q3aq",
                           "p5q3av",
                           "p5q3ax",
                           "p5q3bq",
                           "p5q3ck",
                           "p5q3db",
                           "p5q3as",
                           "p5q3au",
                           "p5q3aw",
                           "p5q3az",
                           "p5q3bb1",
                           "p5q3bb2",
                           "p5q3bb3",
                           "p5q3bb4",
                           "p5q3bb5",
                           "p5q3bb6",
                           "p5q3bb7",
                           "p5q3e",
                           "p5q3ao",
                           "p5q3bk",
                           "p5q3bo",
                           "p5q3bu",
                           "p5q3cu",
                           "p5q3cv",
                           "p5q3da")]
cronbach.alpha(CA_9_int, na.rm=TRUE) 



#### Create Externalizing score Age 9 Score ####
#Total externalizing includes all items from the aggressive behavior scale and all items from the rule-breaking behavior scale.
#35 items

age9_select$Ext9 <- rowMeans(age9_select[,c("p5q3c",
                                           "p5q3o",
                                           "p5q3r",
                                           "p5q3s",
                                           "p5q3t",
                                           "p5q3u",
                                           "p5q3v",
                                           "p5q3aj",
                                           "p5q3bc",
                                           "p5q3bn",
                                           "p5q3cf",
                                           "p5q3cg",
                                           "p5q3ch",
                                           "p5q3ci",
                                           "p5q3cn",
                                           "p5q3co",
                                           "p5q3cq",
                                           "p5q3cw",
                                           "p5q3b",
                                           "p5q3x",
                                           "p5q3aa",
                                           "p5q3al",
                                           "p5q3ap",
                                           "p5q3bi",
                                           "p5q3bm",
                                           "p5q3br",
                                           "p5q3bs",
                                           "p5q3bz",
                                           "p5q3ca",
                                           "p5q3cj",
                                           "p5q3cp",
                                           "p5q3cr",
                                           "p5q3ct",
                                           "p5q3cx",
                                           "p5q3cy")], na.rm=FALSE)

#check if data is in line with expectations
# min=1, max=3
# data is in line with expectations
describe(age9_select$Ext9)

#Have a look at the data
#strong skew to the left, as most people do not show any problems
hist(age9_select$Ext9)


#create a log score
age9_select$Ext9_log <- log(age9_select$Ext9 + 1)  

#still looks somewhat skewed
hist(age9_select$Ext9_log)  
describe(age9_select$Ext9_log)
colnames(age9_select)

#cronbach alpha .91
CA_9_ext <- age9_select[,c("p5q3c",
                           "p5q3o",
                           "p5q3r",
                           "p5q3s",
                           "p5q3t",
                           "p5q3u",
                           "p5q3v",
                           "p5q3aj",
                           "p5q3bc",
                           "p5q3bn",
                           "p5q3cf",
                           "p5q3cg",
                           "p5q3ch",
                           "p5q3ci",
                           "p5q3cn",
                           "p5q3co",
                           "p5q3cq",
                           "p5q3cw",
                           "p5q3b",
                           "p5q3x",
                           "p5q3aa",
                           "p5q3al",
                           "p5q3ap",
                           "p5q3bi",
                           "p5q3bm",
                           "p5q3br",
                           "p5q3bs",
                           "p5q3bz",
                           "p5q3ca",
                           "p5q3cj",
                           "p5q3cp",
                           "p5q3cr",
                           "p5q3ct",
                           "p5q3cx",
                           "p5q3cy")]
cronbach.alpha(CA_9_ext, na.rm=TRUE) 



#### variables Muna age 9 ####

age9_cov <- age9[,c("idnum",
                    "cm5b_age", #age in months
                    "p5k1a", #parenting stress
                    "p5k1b",
                    "p5k1c",
                    "p5k1d",
                    "k5a2e", 
                    "k5a2f",
                    "o5a1", 
                    "o5a2", 
                    "o5a3", 
                    "o5a4", 
                    "o5a5", 
                    "o5a9",
                    "ch5bmip",
                    "p5h17", #puberty items
                    "p5h17a", 
                    "p5h17b", 
                    "p5h23",
                    "p5h19", 
                    "p5h19a",
                    "p5h21", 
                    "p5h22")]
                     

#process data
describe(age9_cov)   


# Replace specific values with NA
# Set negative values in the dataframe to NA
age9_cov[age9_cov< 0] <- NA
describe(age9_cov) 

# Rename a column in the age9_cov dataframe
colnames(age9_cov)[colnames(age9_cov) == "cm5b_age"] <- "Age9_months"

#### Parenting Stress age 9 ####

### Reverse Code items - the higher  - the more stressed parent is
table(age9_cov$p5k1a)

# Reverse code variable p5k1a
age9_cov$p5k1a_reversed <- 5 - age9_cov$p5k1a

# View the reversed variable
table(age9_cov$p5k1a_reversed, age9_cov$p5k1a)

# Reverse code variable p5k1b
age9_cov$p5k1b_reversed <- 5 - age9_cov$p5k1b

# View the reversed variable
table(age9_cov$p5k1b_reversed, age9_cov$p5k1b)

table(age9_cov$p5k1c)
# Reverse code variable p5k1c
age9_cov$p5k1c_reversed <- 5 - age9_cov$p5k1c

# View the reversed variable
table(age9_cov$p5k1c_reversed, age9_cov$p5k1c)

table(age9_cov$p5k1d)
# Reverse code variable m5k2d
age9_cov$p5k1d_reversed <- 5 - age9_cov$p5k1d
# View the reversed variable
table(age9_cov$p5k1d_reversed, age9_cov$p5k1d)

# Create a vector of the selected variables
selected_vars <- c("p5k1a_reversed", "p5k1b_reversed", "p5k1c_reversed", "p5k1d_reversed")

# Calculate the mean score
age9_cov$parent_stress_age9 <- rowMeans(age9_cov[selected_vars], na.rm = FALSE)
describe(age9_cov$parent_stress_age9)

####### Child-Caregiver relationship age 9#######

#Child-Caregiver relationship (Child responding about mother) AGE 9 - Variables: "k5a2e", "k5a2f"
describe(age9_cov$k5a2e)
describe(age9_cov$k5a2f)

# Reverse code variable k5a2e - the higher - the more close to parent
age9_cov$k5a2e_reversed <- 5 - age9_cov$k5a2e
# View the reversed variable
table(age9_cov$k5a2e_reversed,age9_cov$k5a2e)

# Reverse code variable k5a2f - the higher - the more share idea with parent 
age9_cov$k5a2f_reversed <- 5 - age9_cov$k5a2f
# View the reversed variable
table(age9_cov$k5a2f_reversed, age9_cov$k5a2f)

describe(age9_cov$k5a2e_reversed)
describe(age9_cov$k5a2f_reversed)

# Calculate the mean of the variables
age9_cov$Close9 <- rowMeans(age9_cov[, c("k5a2e_reversed", "k5a2f_reversed")], na.rm = FALSE)
describe(age9_cov$Close9)

####### Neighborhoodcondition age 9#######


# Calculate the mean score of the selected items
mean_score_ngh9 <- rowMeans(age9_cov[, c("o5a1", "o5a2", "o5a3", "o5a4", "o5a5", "o5a9")], na.rm = FALSE)

# Add the mean score as a new column in the dataset
age9_cov$mean_score_ngh9 <- mean_score_ngh9

# Print the mean score
describe(mean_score_ngh9)


# Create a subset of the data with the 4 items
subset_data <- age9_cov[, c("o5a1", "o5a2", "o5a3", "o5a4", "o5a5", "o5a9")]
describe(subset_data)

#cronbach alpha = .79
cronbach.alpha(subset_data, na.rm=TRUE)

### BMI age 9 ####

#BMI at age 9
describe(age9_cov$ch5bmip)









############################# Biodata #############################

############################# Read in data #######################

#here we read in the FFCW dataset with year 15 data and select the variables of interest
biodata <- read_dta("/Volumes/MPRG-Biosocial/Projects/03_data/003_FFCW/FFCWS Public data/Biomarker Files/biomarker_final_pub.dta")

#check correlations between PC corrected and non PC corrected phenoage and grim
#for grim correlations around .8 , for pheno correlations around .5/.6
cor.test(biodata$k5me_grim, biodata$k5me_pcgrim)
cor.test(biodata$k5me_phenoage, biodata$k5me_pcphenoage)
cor.test(biodata$k5mk_grim, biodata$k5mk_pcgrim)
cor.test(biodata$k5mk_phenoage, biodata$k5mk_pcphenoage)

cor.test(biodata$k6me_grim, biodata$k6me_pcgrim)
cor.test(biodata$k6me_phenoage, biodata$k6me_pcphenoage)
cor.test(biodata$k6mk_grim, biodata$k6mk_pcgrim)
cor.test(biodata$k6mk_phenoage, biodata$k6mk_pcphenoage)


############################# Process Data #############################

#select items of interest in the biodata file
biodata_select <- biodata[,c("idnum",
                             "k5me_poam45",
                             "k5mk_poam45",
                             "k6me_poam45",
                             "k6mk_poam45",
                             "k5me_pcgrim",
                             "k5mk_pcgrim",
                             "k6me_pcgrim",
                             "k6mk_pcgrim",
                             "k5me_pcphenoage",
                             "k5mk_pcphenoage", 
                             "k6me_pcphenoage",
                             "k6mk_pcphenoage", 
                             "k5me_immune",
                             "k5mk_immune",
                             "k6me_immune",
                             "k6mk_immune",
                             "k5me_epithelial",
                             "k5mk_epithelial",
                             "k6me_epithelial",
                             "k6mk_epithelial",
                             "k5me_batch",
                             "k5mk_batch",
                             "k6me_batch",
                             "k6mk_batch",
                             "k5me_age",
                             "k5mk_age",
                             "k6me_age",
                             "k6mk_age",
                             "k5mk_fib",
                             "k5me_fib",
                             "k6mk_fib",
                             "k6me_fib",
                             "k5mk_ic",
                             "k5me_ic",
                             "k6mk_ic",
                             "k6me_ic")]

colnames(biodata_select)
table(biodata_select$k5me_batch)
table(biodata_select$k5mk_batch)
table(biodata_select$k6me_batch)
table(biodata_select$k6mk_batch)

#In the FFCW, some are assayed on the EPIC, some are assayed on the 450K:
#me_* = Illumina Infinium MethylationEPIC (EPIC) array variables 
#mk_*=Illumina Infinium Human Methylation450K (450K) array variables 

#check if any of these have missings coded as -9 etc
#minimal value for each column >0, so no need to recode missings
describe(biodata_select)

#### residualize DNAm clocks for technical artifacts ####
## see "Trey answer to residualizing clocks" in folder what to use for residuals
#so the Clocks should be regressed out for for cell type, one of the epidish immune cells or epithelial cells and also the epidish fibroblasts
#GrimAge and Pheno are also regressed out for age so you create age acceleration

#below is check, dont need to run
### check ccorrelations cellcounts, if correlation is high, only residualize for one cell type
#correlation is -1

#cor.test(biodata_select$k5me_immune, biodata_select$k5me_epithelial)
#cor.test(biodata_select$k5mk_immune, biodata_select$k5mk_epithelial)
#cor.test(biodata_select$k6me_immune, biodata_select$k6me_epithelial)
#cor.test(biodata_select$k6mk_immune, biodata_select$k6mk_epithelial)

#checkcell <- biodata_select[,c("k5me_immune",
#                               "k5me_epithelial",
#                               "k5mk_immune",
#                               "k5mk_epithelial",
#                               "k6me_immune",
#                               "k6me_epithelial",
#                               "k6mk_immune",
#                               "k6mk_epithelial")]
#they together add op to zero
#So we will only regress them out for one of the two celltypes
#checkcell$k5me_summ <- rowSums(checkcell[,c("k5me_immune","k5me_epithelial")], na.rm=FALSE)                     
#checkcell$k6mk_summ <- rowSums(checkcell[,c("k6mk_immune","k6mk_epithelial")], na.rm=FALSE)                                 
#describe(checkcell$k5me_summ)
#describe(checkcell$k6mk_summ)


#example script laurel
#https://gitlab.gwdg.de/mpib/biosocial/gsoep-dnam/-/blob/main/3_MethylationScores_SOEP.R
#reg = lm (Horvath ~ as.factor(Array ) + as.factor(Slide)   , data=dat) ; 
#dat$Horvath_res = scale(residuals(reg))

### Age 9 EPIC

Epic_age9 <- biodata_select[,c("idnum",
                               "k5me_poam45",
                               "k5me_pcgrim",
                               "k5me_pcphenoage",
                               "k5me_immune",
                               "k5me_batch",
                               "k5me_age",
                               "k5me_fib",
                               "k5me_ic")]

describe(Epic_age9)
describe(Epic_age9$k5me_age)
cor.test(Epic_age9$k5me_poam45, Epic_age9$k5me_fib)
cor.test(Epic_age9$k5me_poam45, Epic_age9$k5me_ic)

# Filter rows where the value in column x is not NA
#otherwise we cannot attach the new variables to the dataframe cause number of rows do not line up
Epic_age9 <- Epic_age9[complete.cases(Epic_age9$k5me_poam45), ]

regPace <- lm(k5me_poam45 ~ k5me_fib + k5me_ic, data=Epic_age9)
Epic_age9$DunedinPACE_9_res_EPIC <- residuals(regPace) 
Epic_age9$DunedinPACE_9_res_EPIC_std <- scale(residuals(regPace)) 

#does the same as when entering center, and scale in the formula
#Epic_age9$DunedinPACE_9_res_EPIC_std1 <- scale(residuals(regPace)) 
#Epic_age9$DunedinPACE_9_res_EPIC_std <- scale(residuals(regPace, center = TRUE, scale = TRUE)) 
regGrim <- lm(k5me_pcgrim ~ k5me_fib + k5me_ic + k5me_age , data=Epic_age9)
Epic_age9$PCGrim_9_res_EPIC <-residuals(regGrim) 
Epic_age9$PCGrim_9_res_EPIC_std <- scale(residuals(regGrim)) 

regPheno <- lm(k5me_pcphenoage ~ k5me_fib + k5me_ic + k5me_age, data=Epic_age9)
Epic_age9$PCPheno_9_res_EPIC <- residuals(regPheno)
Epic_age9$PCPheno_9_res_EPIC_std <- scale(residuals(regPheno))

# Add a new column with value 1 so we later know this was assayed on EPIC
Epic_age9$Epic9 <- 1

#descriptives
colnames(Epic_age9)
describe(Epic_age9)



### Age 9 450k

F50K_age9 <- biodata_select[,c("idnum",
                               "k5mk_poam45",
                               "k5mk_pcgrim",
                               "k5mk_pcphenoage",
                               "k5mk_immune",
                               "k5mk_batch",
                               "k5mk_age",
                               "k5mk_fib",
                               "k5mk_ic")]

describe(F50K_age9)


# Filter rows where the value in column x is not NA
#otherwise we cannot attach the new variables to the dataframe cause number of rows do not line up
F50K_age9 <- F50K_age9[complete.cases(F50K_age9$k5mk_poam45), ]

regPace <- lm(k5mk_poam45 ~ k5mk_fib + k5mk_ic, data=F50K_age9)
F50K_age9$DunedinPACE_9_res_F50K <- residuals(regPace) 
F50K_age9$DunedinPACE_9_res_F50K_std <- scale(residuals(regPace)) 

regGrim <- lm(k5mk_pcgrim ~ k5mk_fib + k5mk_ic + k5mk_age, data=F50K_age9)
F50K_age9$PCGrim_9_res_F50K <-residuals(regGrim) 
F50K_age9$PCGrim_9_res_F50K_std <- scale(residuals(regGrim))

regPheno <- lm(k5mk_pcphenoage ~ k5mk_fib + k5mk_ic + k5mk_age, data=F50K_age9)
F50K_age9$PCPheno_9_res_F50K <- residuals(regPheno)
F50K_age9$PCPheno_9_res_F50K_std <- scale(residuals(regPheno))

# Add a new column with value 2 so we later know this was assayed on 450K
F50K_age9$F50K9 <- 2
describe(F50K_age9)

### Age 15 EPIC

Epic_age15 <- biodata_select[,c("idnum",
                               "k6me_poam45",
                               "k6me_pcgrim",
                               "k6me_pcphenoage",
                               "k6me_immune",
                               "k6me_batch",
                               "k6me_age",
                               "k6me_fib",
                               "k6me_ic")]

describe(Epic_age15)

# Filter rows where the value in column x is not NA
#otherwise we cannot attach the new variables to the dataframe cause number of rows do not line up
Epic_age15 <- Epic_age15[complete.cases(Epic_age15$k6me_poam45), ]

regPace <- lm(k6me_poam45 ~ k6me_fib + k6me_ic, data=Epic_age15)
Epic_age15$DunedinPACE_15_res_EPIC <- residuals(regPace) 
Epic_age15$DunedinPACE_15_res_EPIC_std <- scale(residuals(regPace)) 

regGrim <- lm(k6me_pcgrim ~ k6me_fib + k6me_ic + k6me_age, data=Epic_age15)
Epic_age15$PCGrim_15_res_EPIC <-residuals(regGrim) 
Epic_age15$PCGrim_15_res_EPIC_std <- scale(residuals(regGrim)) 

regPheno <- lm(k6me_pcphenoage ~ k6me_fib + k6me_ic + k6me_age, data=Epic_age15)
Epic_age15$PCPheno_15_res_EPIC <- residuals(regPheno)
Epic_age15$PCPheno_15_res_EPIC_std <- scale(residuals(regPheno))

# Add a new column with value 1 so we later know this was assayed on EPIC
Epic_age15$Epic15 <- 1

#descriptives
colnames(Epic_age15)
describe(Epic_age15)


### Age 15 450k

F50K_age15 <- biodata_select[,c("idnum",
                               "k6mk_poam45",
                               "k6mk_pcgrim",
                               "k6mk_pcphenoage",
                               "k6mk_immune",
                               "k6mk_batch",
                               "k6mk_age",
                               "k6mk_fib",
                               "k6mk_ic")]

describe(F50K_age15)


# Filter rows where the value in column x is not NA
#otherwise we cannot attach the new variables to the dataframe cause number of rows do not line up
F50K_age15 <- F50K_age15[complete.cases(F50K_age15$k6mk_poam45), ]

regPace <- lm(k6mk_poam45 ~ k6mk_fib + k6mk_ic, data=F50K_age15)
F50K_age15$DunedinPACE_15_res_F50K <- residuals(regPace) 
F50K_age15$DunedinPACE_15_res_F50K_std <- scale(residuals(regPace)) 

regGrim <- lm(k6mk_pcgrim ~ k6mk_fib + k6mk_ic + k6mk_age, data=F50K_age15)
F50K_age15$PCGrim_15_res_F50K <-residuals(regGrim) 
F50K_age15$PCGrim_15_res_F50K_std <- scale(residuals(regGrim)) 

regPheno <- lm(k6mk_pcphenoage ~ k6mk_fib + k6mk_ic + k6mk_age, data=F50K_age15)
F50K_age15$PCPheno_15_res_F50K <- residuals(regPheno)
F50K_age15$PCPheno_15_res_F50K_std <- scale(residuals(regPheno))

# Add a new column with value 2 so we later know this was assayed on 450K
F50K_age15$F50K15 <- 2
describe(F50K_age15)




############################# Age5 #############################

############################# Read in data #######################

#here we read in the FFCW dataset with year 5 data and select the variables of interest
Age5 <- read_dta("/Volumes/MPRG-Biosocial/Projects/03_data/003_FFCW/FFCWS Public data/Year 5/FF_wave4_2020v2.dta")

############################# Dataprocessing #######################

age5_select <- Age5[,c("idnum",
                       "p4l5", 
                       "m4b4b4",
                       "m4b4b4",
                       "p4l17",
                       "p4l18",
                       "p4l19", 
                       "p4l20",
                       "m4b4b18",
                       "m4b29a18", 
                       "m4b4b9",
                       "m4b29a9",
                       "m4b4b14",
                       "m4b29a14",
                       "p4l29", 
                       "p4l43", 
                       "p4l53", 
                       "m4b4b15",
                       "m4b29a15",
                       "p4l65",
                       "p4l25",
                       "p4l38", 
                       "p4l42", 
                       "p4l46", 
                       "p4l47",
                       "p4l52", 
                       "p4l61",
                       "m4b29a15", 
                       "m4b4b17",
                       "m4b29a17", 
                       "p4l1", 
                       "p4l2", 
                       "p4l7", 
                       "m4b4b16",
                       "m4b29a16", 
                       "p4l9", 
                       "p4l10", 
                       "p4l12", 
                       "p4l13", 
                       "p4l16", 
                       "p4l21",
                       "p4l33", 
                       "p4l40",
                       "p4l44",
                       "p4l45", 
                       "m4b4b11",
                       "m4b29a11", 
                       "m4b4b12",
                       "m4b29a12", 
                       "p4l56", 
                       "p4l56",
                       "p4l57", 
                       "m4b4b13",
                       "m4b29a13", 
                       "p4l62", 
                       "m4b4b7",
                       "m4b29a7", 
                       "p4l23", 
                       "p4l26", 
                       "p4l36", 
                       "p4l39", 
                       "p4l39", 
                       "p4l49", 
                       "p4l50",
                       "p4l54",
                       "p4l64", 
                       "p4l59",
                       "o4p1",
                       "o4p2",
                       "o4p3",
                       "o4p4",
                       "o4p5",
                       "o4p9",
                       "m4b6a",
                       "m4b6b",
                       "m4b6c",
                       "m4b6d",
                       "cm4b_age")]


#some CBCL items are twice in the same dataset, check what is going on
#so it seems the columns with 29a in it are a small sample size, maybe test case?
#we do not include them in our scale. 
table(age5_select$m4b4b15)
table(age5_select$m4b29a15) #small sample size
table(age5_select$m4b4b18)
table(age5_select$m4b29a18) #small sample size

#check how missings are coded across CBCL items
# Create a function to run table() for each column
run_table <- function(column) {
  table_result <- table(column)
  return(table_result)
}

# Apply the function to each column using lapply()
result_list5 <- lapply(age5_select, run_table)
result_list5

#reported missings
#-9   -6   -2   -1
# Set negative values in the dataframe to NA
age5_select[age5_select < 0] <- NA

## all missings are set to 0
## score 
## recode so 1= not at all, 2=sometimes, 3=often, just like other scales of CBCL
#instead of 0, 1, 2
colnames(age5_select)

# Specify the column indices to update
columns_plusone <- 2:67  

# Add 1 to every row in the specified columns
for (col_index in columns_plusone) {
  age5_select[, col_index] <- age5_select[, col_index] + 1
}


#before the +1
#$m4b6d #the same after +1
#column
#1    2    3    4 
#717 1617  768  945 

#$o4p1
#column #the same after +1
#1    2    3    4 
#1233  540  237   69 

#$p4l50
#column #score 1, 2, 3 after +1
#0    1    2 
#2895   68   10

#$p4l39
#column #score 1, 2, 3 after +1
#0    1    2 
#2953   16    7 


### create Internalizing Score Age 5 #####

age5_select$Int5 <- rowMeans(age5_select[, c("p4l5",
                                            "m4b4b4",
                                            "p4l17",
                                            "p4l18",
                                            "p4l19",
                                            "p4l20",
                                            "m4b4b18",
                                            "m4b4b9",
                                            "m4b4b14",
                                            "p4l29",
                                            "p4l43",
                                            "p4l53",
                                            "m4b4b15",
                                            "p4l65",
                                            "p4l25",
                                            "p4l38",
                                            "p4l42",
                                            "p4l46",
                                            "p4l47",
                                            "p4l52",
                                            "p4l61",
                                            "m4b4b17")], na.rm=FALSE)
describe(age5_select$p4l5)
describe(age5_select$m4b4b4)
table(age5_select$p4l5, age5_select$m4b4b4)

#22 items
#is correct

#check how many missings
age5_select$missing_Int5 <- rowSums(is.na(age5_select[c("p4l5",
                                                        "m4b4b4",
                                                        "p4l17",
                                                        "p4l18",
                                                        "p4l19",
                                                        "p4l20",
                                                        "m4b4b18",
                                                        "m4b4b9",
                                                        "m4b4b14",
                                                        "p4l29",
                                                        "p4l43",
                                                        "p4l53",
                                                        "m4b4b15",
                                                        "p4l65",
                                                        "p4l25",
                                                        "p4l38",
                                                        "p4l42",
                                                        "p4l46",
                                                        "p4l47",
                                                        "p4l52",
                                                        "p4l61",
                                                        "m4b4b17")]))
# n=73 have 1, n=187  have 6 missing
#the FFCW guide:not clear about missings
table(age5_select$missing_Int5)

describe(age5_select$Int5)
hist(age5_select$Int5)

#create a log score
age5_select$Int5_log <- log(age5_select$Int5 +1)  

#still looks somewhat skewed
hist(age5_select$Int5_log)  
describe(age5_select$Int5_log)



#check cronbach alpha
install.packages("ltm")
library(ltm)

#.75
cronbachdata <- age5_select[,c("p4l5",
                        "m4b4b4",
                        "p4l17",
                        "p4l18",
                        "p4l19",
                        "p4l20",
                        "m4b4b18",
                        "m4b4b9",
                        "m4b4b14",
                        "p4l29",
                        "p4l43",
                        "p4l53",
                        "m4b4b15",
                        "p4l65",
                        "p4l25",
                        "p4l38",
                        "p4l42",
                        "p4l46",
                        "p4l47",
                        "p4l52",
                        "p4l61",
                        "m4b4b17")]
cronbach.alpha(cronbachdata, na.rm=TRUE)  

### create Externalizing Score Age 5 #####

age5_select$Ext5 <- rowMeans(age5_select[, c("p4l1",
                                             "p4l2",
                                             "p4l7",
                                             "m4b4b16",
                                             "p4l9",
                                             "p4l10", 
                                             "p4l12" ,
                                             "p4l13", 
                                             "p4l16", 
                                             "p4l21",
                                             "p4l33", 
                                             "p4l40", 
                                             "p4l45", 
                                             "m4b4b11",
                                             "m4b4b12",
                                             "p4l57",
                                             "p4l56", 
                                             "m4b4b13",
                                             "p4l59",
                                             "p4l62", 
                                             "m4b4b7",
                                             "p4l23", 
                                             "p4l26", 
                                             "p4l36", 
                                             "p4l39", 
                                             "p4l44",
                                             "p4l49", 
                                             "p4l50", 
                                             "p4l54",
                                             "p4l64")], na.rm=FALSE)

 



#30 items minimum should be 1, max should be 3
describe(age5_select$Ext5)
hist(age5_select$Ext5)

#create a log score
age5_select$Ext5_log <- log(age5_select$Ext5 + 1)  

#still looks somewhat skewed
hist(age5_select$Ext5_log)  
describe(age5_select$Ext5_log)

#.87
cronbachdata_ext_5 <- age5_select[,c("p4l1",
                                     "p4l2",
                                     "p4l7",
                                     "m4b4b16",
                                     "p4l9",
                                     "p4l10", 
                                     "p4l12" ,
                                     "p4l13", 
                                     "p4l16", 
                                     "p4l21",
                                     "p4l33", 
                                     "p4l40", 
                                     "p4l45", 
                                     "m4b4b11",
                                     "m4b4b12",
                                     "p4l57",
                                     "p4l56", 
                                     "m4b4b13",
                                     "p4l59",
                                     "p4l62", 
                                     "m4b4b7",
                                     "p4l23", 
                                     "p4l26", 
                                     "p4l36", 
                                     "p4l39", 
                                     "p4l44",
                                     "p4l49", 
                                     "p4l50", 
                                     "p4l54",
                                     "p4l64")]
cronbach.alpha(cronbachdata_ext_5, na.rm=TRUE) 


#### parenting stress and neighborhood conditions at age 5 ####

#parenting stress
#m4b6a, m4b6b, m4b6c, m4b6d

age5_cov <- age5_select[,c("idnum",
                           "o4p1",
                           "o4p2",
                           "o4p3",
                           "o4p4",
                           "o4p5",
                           "o4p9",
                           "m4b6a",
                           "m4b6b",
                           "m4b6c",
                           "m4b6d",
                           "cm4b_age")]
describe(age5_cov)


# Reverse code variable m4b6a
age5_cov$m4b6a_reversed <- 5 - age5_cov$m4b6a
# View the reversed variable
table(age5_cov$m4b6a_reversed, age5_cov$m4b6a)

# Reverse code variable m4b6b
age5_cov$m4b6b_reversed <- 5 - age5_cov$m4b6b

# View the reversed variable
table(age5_cov$m4b6b_reversed, age5_cov$m4b6b)

# Reverse code variable m4b6c
age5_cov$m4b6c_reversed <- 5 - age5_cov$m4b6c
# View the reversed variable
table(age5_cov$m4b6c_reversed, age5_cov$m4b6c)

# Reverse code variable m4b6d
age5_cov$m4b6d_reversed <- 5 - age5_cov$m4b6d
# View the reversed variable
table(age5_cov$m4b6d_reversed, age5_cov$m4b6d)

age5_cov$parent_stress_age5 <- rowMeans(age5_cov[, c("m4b6a_reversed",
                                                         "m4b6b_reversed",
                                                         "m4b6c_reversed",
                                                         "m4b6d_reversed")], na.rm=FALSE)

describe(age5_cov$m4b6a_reversed)
describe(age5_cov$m4b6b_reversed)
describe(age5_cov$m4b6c_reversed)
describe(age5_cov$m4b6d_reversed)
describe(age5_cov$parent_stress_age5)


cronbachdata_stress_5 <- age5_cov[,c("m4b6a_reversed",
                                     "m4b6b_reversed",
                                     "m4b6c_reversed",
                                     "m4b6d_reversed")]
cronbach.alpha(cronbachdata_stress_5, na.rm=TRUE)


#### neighborhood conditions age 5 ####

#Create Mean Score of items set missings na.rm = FALSE): the higher number - the bad/ worse conditions are in the neighborhood

# Calculate the mean score of the selected items
mean_score_ngh5 <- rowMeans(age5_cov[, c("o4p1", "o4p2", "o4p3", "o4p4", "o4p5", "o4p9")], na.rm = FALSE)

# Add the mean score as a new column in the dataset
age5_cov$mean_score_ngh5 <- mean_score_ngh5

# Print the mean score
describe(age5_cov$mean_score_ngh5)


############################# Age3 #############################

############################# Read in data #######################

#here we read in the FFCW dataset with year 15 data and select the variables of interest
Age3 <- read_dta("/Volumes/MPRG-Biosocial/Projects/03_data/003_FFCW/FFCWS Public data/Year 3/FF_wave3_2020v2.dta")

############################# Process data #######################

age3_select <- Age3[,c("idnum",
                       "p3m3",
                       "p3m16",
                       "p3m19", 
                       "p3m22",
                       "p3m25",
                       "p3m32", 
                       "p3m42",
                       "p3m46", 
                       "p3m1", 
                       "p3m2",
                       "p3m9", 
                       "p3m29", 
                       "p3m31", 
                       "p3m35", 
                       "p3m36", 
                       "p3m50", 
                       "p3m2c", 
                       "p3m5", 
                       "p3m6", 
                       "p3m6b", 
                       "p3m7",
                       "p3m13",
                       "p3m14",
                       "p3m18", 
                       "p3m21", 
                       "p3m21a", 
                       "p3m23", 
                       "p3m26a", 
                       "p3m28", 
                       "p3m30",
                       "p3m33", 
                       "p3m39", 
                       "p3m41", 
                       "p3m44", 
                       "p3m48",
                       "p3m2a",
                       "p3m3b",
                       "p3m6a",
                       "p3m6b",
                       "p3m18a",
                       "p3m21a",
                       "p3m28a", 
                       "o3p1",
                       "o3p2",
                       "o3p3",
                       "o3p4",
                       "o3p5",
                       "o3p9",
                       "ce3agefc",
                       "ch3agemo2_c",
                       "cm3b_age")]  



#check how missings are coded across CBCL items
# Create a function to run table() for each column
run_table <- function(column) {
  table_result <- table(column)
  return(table_result)
}

# Apply the function to each column using lapply()
result_list3 <- lapply(age3_select, run_table)
result_list3

#reported missings
#-9   -6   -2   -1
# Set negative values in the dataframe to NA
age3_select[age3_select < 0] <- NA



## all missings are set to 0
## score 
## recode so 1= not at all, 2=sometimes, 3=often, just like other scales of CBCL
#instead of 0, 1, 2
colnames(age3_select)

# Specify the column indices to update
columns_plusone <- 2:43  

# Add 1 to every row in the specified columns
for (col_index in columns_plusone) {
  age3_select[, col_index] <- age3_select[, col_index] + 1
}


### create Internalizing Score Age 3 #####

age3_select$Int3 <- rowMeans(age3_select[, c("p3m3",
                                            "p3m16",
                                            "p3m19",
                                            "p3m22",
                                            "p3m25",
                                            "p3m32",
                                            "p3m42",
                                            "p3m46",
                                            "p3m1",
                                            "p3m2",
                                            "p3m9",
                                            "p3m29",
                                            "p3m31",
                                            "p3m35",
                                            "p3m36",
                                            "p3m50")], na.rm=TRUE)

#check how many missings
age3_select$missing_Int3 <- rowSums(is.na(age3_select[c("p3m3",
                                                        "p3m16",
                                                        "p3m19",
                                                        "p3m22",
                                                        "p3m25",
                                                        "p3m32",
                                                        "p3m42",
                                                        "p3m46",
                                                        "p3m1",
                                                        "p3m2",
                                                        "p3m9",
                                                        "p3m29",
                                                        "p3m31",
                                                        "p3m35",
                                                        "p3m36",
                                                        "p3m50")]))

#n=509 have 1 missing
#this is probably Anxious/Depressed scale (“nervous movements or twitching” 
#which was inadvertently substituted for “nervous, high strung, tense”).
#so we allow to create a meanscore and set those with more than 1 missing to NA
table(age3_select$missing_Int3)

describe(age3_select$Int3)
age3_select$Int3[age3_select$missing_Int3 >= 2] <- NA
describe(age3_select$Int3)


#16 items, so min1, max 3 
describe(age3_select$Int3)
hist(age3_select$Int3)
#create a log score
age3_select$Int3_log <- log(age3_select$Int3 +1)  

#still looks somewhat skewed
hist(age3_select$Int3_log)  
describe(age3_select$Int3_log)

#.74
cronbachdata_int_3 <- age3_select[,c("p3m3",
                                     "p3m16",
                                     "p3m19",
                                     "p3m22",
                                     "p3m25",
                                     "p3m32",
                                     "p3m42",
                                     "p3m46",
                                     "p3m1",
                                     "p3m2",
                                     "p3m9",
                                     "p3m29",
                                     "p3m31",
                                     "p3m35",
                                     "p3m36",
                                     "p3m50")]

cronbach.alpha(cronbachdata_int_3 , na.rm=TRUE) 

### create Externalizing Score Age 3 #####

age3_select$Ext3 <- rowMeans(age3_select[, c("p3m2c", 
                                            "p3m5",
                                            "p3m6",
                                            "p3m6b", 
                                            "p3m7",
                                            "p3m13",
                                            "p3m14",
                                            "p3m18", 
                                            "p3m21", 
                                            "p3m21a", 
                                            "p3m23", 
                                            "p3m26a", 
                                            "p3m28", 
                                            "p3m30",
                                            "p3m33", 
                                            "p3m39", 
                                            "p3m41", 
                                            "p3m44", 
                                            "p3m48",
                                            "p3m2a",
                                            "p3m3b",
                                            "p3m6a",
                                            "p3m18a",
                                            "p3m28a")], na.rm=FALSE)


describe(age3_select$Ext3) 
hist(age3_select$Ext3)


#check how many missings
age3_select$missing_Ext3 <- rowSums(is.na(age3_select[c("p3m2c", 
                                                        "p3m5",
                                                        "p3m6",
                                                        "p3m6b", 
                                                        "p3m7",
                                                        "p3m13",
                                                        "p3m14",
                                                        "p3m18", 
                                                        "p3m21", 
                                                        "p3m21a", 
                                                        "p3m23", 
                                                        "p3m26a", 
                                                        "p3m28", 
                                                        "p3m30",
                                                        "p3m33", 
                                                        "p3m39", 
                                                        "p3m41", 
                                                        "p3m44", 
                                                        "p3m48",
                                                        "p3m2a",
                                                        "p3m3b",
                                                        "p3m6a",
                                                        "p3m18a",
                                                        "p3m28a")]))

#n=109 have 1 missing
#this is probably Anxious/Depressed scale (“nervous movements or twitching” 
#which was inadvertently substituted for “nervous, high strung, tense”).
#so we allow to create a meanscore and set those with more than 1 missing to NA
table(age3_select$missing_Ext3)



#create a log score
age3_select$Ext3_log <- log(age3_select$Ext3 + 1)  

#still looks somewhat skewed
hist(age3_select$Ext3_log)  
describe(age3_select$Ext3_log)

#.89
cronbachdata_ext_3 <- age3_select[,c("p3m2c", 
                                     "p3m5",
                                     "p3m6",
                                     "p3m6b", 
                                     "p3m7",
                                     "p3m13",
                                     "p3m14",
                                     "p3m18", 
                                     "p3m21", 
                                     "p3m21a", 
                                     "p3m23", 
                                     "p3m26a", 
                                     "p3m28", 
                                     "p3m30",
                                     "p3m33", 
                                     "p3m39", 
                                     "p3m41", 
                                     "p3m44", 
                                     "p3m48",
                                     "p3m2a",
                                     "p3m3b",
                                     "p3m6a",
                                     "p3m18a",
                                     "p3m28a")]

cronbach.alpha(cronbachdata_ext_3 , na.rm=TRUE) 


############################# Merge Files #############################

#Files of Interest
#baseline_select
#age3_select
#age5_select
#age9_select
#age9_cov
#age15_select
#age15_cov
#Epic_age15
#Epic_age9
#F50K_age15
#F50K_age9
#age5_cov




#### create datafiles to merge
colnames(baseline_select) #we keep all

colnames(age3_select)
age3_merge <-age3_select[,c("idnum", "Int3","Int3_log",   
                             "Ext3","Ext3_log", "cm3b_age")]

colnames(age5_select)
age5_merge <-age5_select[,c("idnum", "Int5","Int5_log",   
                            "Ext5","Ext5_log")]

colnames(age5_cov)
age5_cov_merge <- age5_cov[,c("idnum","mean_score_ngh5","parent_stress_age5")]

colnames(age9_select)
age9_merge <-age9_select[,c("idnum", "Int9","Int9_log",    
                            "Ext9","Ext9_log")]

colnames(age9_cov)
age9_cov_merge <- age9_cov[,c("idnum","Age9_months","ch5bmip","parent_stress_age9", "Close9", "mean_score_ngh9", 
                              "p5h17",  #puberty items, so we can later create puberty score
                              "p5h17a",
                              "p5h17b", 
                              "p5h23",
                              "p5h19", 
                              "p5h19a",
                              "p5h21", 
                              "p5h22")]

colnames(age15_merge)
age15_merge <-age15_select[,c("idnum", "Int15","Int15_log",    
                            "Ext15","Ext15_log")]
colnames(age15_cov)
age15_cov_merge <- age15_cov[,c("idnum",  "ck6ethrace"  ,"h6a8" , "Age15_months","eversmoke", "race_white","race_black", "race_hispanic", 
                                "race_other", "race_multi", "scale_cesd", "cesd_log", "scale_anxt", "scale_anxt_log",
                                "parent_stress_age15","Close15", "mean_score_ngh15", "ck6bmip", "k6e10", "k6e16")]



Merge_1 <- left_join(baseline_select, age3_merge, by = "idnum")
Merge_2 <- left_join(Merge_1, age5_merge, by = "idnum")
Merge_3 <- left_join(Merge_2, age9_merge, by = "idnum")
Merge_4 <- left_join(Merge_3, age9_cov_merge, by = "idnum")
Merge_5 <- left_join(Merge_4, age15_cov_merge, by = "idnum")
Merge_6 <- left_join(Merge_5, age15_merge, by = "idnum")
Merge_7 <- left_join(Merge_6, Epic_age9, by = "idnum")
Merge_8 <- left_join(Merge_7, Epic_age15, by = "idnum")
Merge_9 <- left_join(Merge_8, F50K_age9, by = "idnum")
Merge_10 <- left_join(Merge_9, F50K_age15, by = "idnum")
Merge_11 <- left_join(Merge_10, age5_cov_merge, by = "idnum")

Datafull <- Merge_11 

colnames(Datafull)

##### dataprocessing
#me_* = Illumina Infinium MethylationEPIC (EPIC) array variables 
#mk_*=Illumina Infinium Human Methylation450K (450K) array variables 
# View the updated dataframe
#n=1120 EPIC at age 15, 1143 at age 9
#n=854 450K at age 15, 828 at age 9

# create Epic vs 450K variable
colnames(Epic_age15)
colnames(Epic_age9)
colnames(F50K_age15)
colnames(F50K_age9)


#Create a new column to for assay 1=EPIC, 2=450K
Datafull$Array <- ifelse(!is.na(Datafull$Epic15) | !is.na(Datafull$Epic9), 1,
                              ifelse(!is.na(Datafull$F50K15) | !is.na(Datafull$F50K9), 2, NA))
table(Datafull$Array)

# Create clocks combining Epic & 450K
Datafull$DunedinPACE_9_raw <- ifelse(!is.na(Datafull$k5me_poam45), Datafull$k5me_poam45, Datafull$k5mk_poam45) 
Datafull$DunedinPACE_9_res <- ifelse(!is.na(Datafull$DunedinPACE_9_res_EPIC), Datafull$DunedinPACE_9_res_EPIC, Datafull$DunedinPACE_9_res_F50K)                                                             
Datafull$DunedinPACE_9_res_std <- ifelse(!is.na(Datafull$DunedinPACE_9_res_EPIC_std), Datafull$DunedinPACE_9_res_EPIC_std, Datafull$DunedinPACE_9_res_F50K_std)                                                             

Datafull$PCGrim_9_raw <- ifelse(!is.na(Datafull$k5me_pcgrim), Datafull$k5me_pcgrim, Datafull$k5mk_pcgrim)
Datafull$PCGrim_9_accell_res <- ifelse(!is.na(Datafull$PCGrim_9_res_EPIC), Datafull$PCGrim_9_res_EPIC, Datafull$PCGrim_9_res_F50K) 
Datafull$PCGrim_9_accell_res_std <- ifelse(!is.na(Datafull$PCGrim_9_res_EPIC_std), Datafull$PCGrim_9_res_EPIC_std, Datafull$PCGrim_9_res_F50K_std) 

Datafull$PCPheno_9_raw <- ifelse(!is.na(Datafull$k5me_pcphenoage ), Datafull$k5me_pcphenoage , Datafull$k5mk_pcphenoage) 
Datafull$PCPheno_9_accell_res <- ifelse(!is.na(Datafull$PCPheno_9_res_EPIC), Datafull$PCPheno_9_res_EPIC, Datafull$PCPheno_9_res_F50K) 
Datafull$PCPheno_9_accell_res_std <- ifelse(!is.na(Datafull$PCPheno_9_res_EPIC_std), Datafull$PCPheno_9_res_EPIC_std, Datafull$PCPheno_9_res_F50K_std) 

Datafull$DunedinPACE_15_raw <- ifelse(!is.na(Datafull$k6me_poam45), Datafull$k6me_poam45, Datafull$k6mk_poam45) 
Datafull$DunedinPACE_15_res <- ifelse(!is.na(Datafull$DunedinPACE_15_res_EPIC), Datafull$DunedinPACE_15_res_EPIC, Datafull$DunedinPACE_15_res_F50K)                                                             
Datafull$DunedinPACE_15_res_std <- ifelse(!is.na(Datafull$DunedinPACE_15_res_EPIC_std), Datafull$DunedinPACE_15_res_EPIC_std, Datafull$DunedinPACE_15_res_F50K_std)                                                             

Datafull$PCGrim_15_raw <- ifelse(!is.na(Datafull$k6me_pcgrim), Datafull$k6me_pcgrim, Datafull$k6mk_pcgrim)
Datafull$PCGrim_15_accell_res <- ifelse(!is.na(Datafull$PCGrim_15_res_EPIC), Datafull$PCGrim_15_res_EPIC, Datafull$PCGrim_15_res_F50K) 
Datafull$PCGrim_15_accell_res_std <- ifelse(!is.na(Datafull$PCGrim_15_res_EPIC_std), Datafull$PCGrim_15_res_EPIC_std, Datafull$PCGrim_15_res_F50K_std) 

Datafull$PCPheno_15_raw <- ifelse(!is.na(Datafull$k6me_pcphenoage ), Datafull$k6me_pcphenoage , Datafull$k6mk_pcphenoage) 
Datafull$PCPheno_15_accell_res <- ifelse(!is.na(Datafull$PCPheno_15_res_EPIC), Datafull$PCPheno_15_res_EPIC, Datafull$PCPheno_15_res_F50K) 
Datafull$PCPheno_15_accell_res_std <- ifelse(!is.na(Datafull$PCPheno_15_res_EPIC_std), Datafull$PCPheno_15_res_EPIC_std, Datafull$PCPheno_15_res_F50K_std) 

colnames(Datafull)

### datacheck of scripts below
datacheck_array <- Datafull[ ,c("Epic15",
                                "Epic9",
                                "F50K15",
                                "F50K9",
                                "PCGrim_9_res_EPIC",
                                "PCGrim_9_res_F50K",
                                "PCGrim_15_res_EPIC",
                                "PCGrim_15_res_F50K")]

# Create a new column by combining values from Column1 and Column2
datacheck_array$PCGrim_9_res_across <- ifelse(!is.na(datacheck_array$PCGrim_9_res_EPIC), datacheck_array$PCGrim_9_res_EPIC, datacheck_array$PCGrim_9_res_F50K)    


### check if processing DNAm data went allright
datacheck_DNAm <- Datafull[ ,c("k5me_poam45",
                               "k5mk_poam45",
                               "DunedinPACE_9_raw",  
                               "Array",
                               "k6me_poam45",
                               "k6mk_poam45",
                               "DunedinPACE_15_raw")]

#looks allright

#check means across array   
#sample size in line with paper Juan
# google Ethnic/Racial Disparities in DNA Methylation Age Profiles across the Lifespan, Juan Del Toro
# he describes for age 9 sample sizes of 828 and 1,143 = 1971

describe(Datafull$k5me_pcgrim) #mean 38.82, EPIC, N=1143
describe(Datafull$k5mk_pcgrim) #mean 37.99 450K, N=828
describe(Datafull$PCGrim_9_raw) #mean 38.47 across array, n=1971

describe(Datafull$k5me_poam45) #mean 1.21, EPIC, N=1143
describe(Datafull$k5mk_poam45) #mean 1.25 450K N=828
describe(Datafull$DunedinPACE_9_raw) #mean 1.22 across array, N=828

describe(Datafull$k6me_pcgrim) #mean 38.82, EPIC, N=1120
describe(Datafull$k6mk_pcgrim) #mean 37.99 450K, N=854
describe(Datafull$PCGrim_15_raw) #mean 38.47 across array, n=1974


#### create standardized mental health variables ####

describe(Datafull$Int3_log) #mean 0.85, sd=0.10
describe(Datafull$Ext3_log) #mean=0.95, sd=0.13

#create standardized age3 score
Int3data <- Datafull[,c("idnum",
                        "Int3_log")]
Int3data <- Int3data[complete.cases(Int3data$Int3_log), ]
Int3data$Int3_Std <- (Int3data$Int3_log-0.85)/0.10

Int3_merge <- Int3data[,c("idnum",
                          "Int3_Std")]

Ext3data <- Datafull[,c("idnum",
                        "Ext3_log")]

Ext3data <- Ext3data[complete.cases(Ext3data$Ext3_log), ]
Ext3data$Ext3_Std <- (Ext3data$Ext3_log-0.95)/0.13

Ext3_merge <- Ext3data[,c("idnum",
                          "Ext3_Std")]


#descriptives Internalizing Age3
#Int3_log, Mean=0.84, SD=0.10
#Ext3_log, Mean=0.95 , SD=0.13

Int5data <- Datafull[,c("idnum",
                        "Int5_log")]
                       
Int5data <- Int5data[complete.cases(Int5data$Int5_log), ]
Int5data$Int5_Std3 <- (Int5data$Int5_log-0.85)/0.10
  
Int5_merge <- Int5data[,c("idnum",
                          "Int5_Std3")]


Ext5data <- Datafull[,c("idnum",
                        "Ext5_log")]

Ext5data <- Ext5data[complete.cases(Ext5data$Ext5_log), ]
Ext5data$Ext5_Std3 <- (Ext5data$Ext5_log-0.95)/0.13

Ext5_merge <- Ext5data[,c("idnum",
                          "Ext5_Std3")]


Int9data <- Datafull[,c("idnum",
                        "Int9_log")]

Int9data <- Int9data[complete.cases(Int9data$Int9_log), ]
Int9data$Int9_Std3 <- (Int9data$Int9_log-0.85)/0.10

Int9_merge <- Int9data[,c("idnum",
                          "Int9_Std3")]


Ext9data <- Datafull[,c("idnum",
                        "Ext9_log")]

Ext9data <- Ext9data[complete.cases(Ext9data$Ext9_log), ]
Ext9data$Ext9_Std3 <- (Ext9data$Ext9_log-0.95)/0.13

Ext9_merge <- Ext9data[,c("idnum",
                          "Ext9_Std3")]

Int15data <- Datafull[,c("idnum",
                        "Int15_log")]

Int15data <- Int15data[complete.cases(Int15data$Int15_log), ]
Int15data$Int15_Std3 <- (Int15data$Int15_log-0.85)/0.10

Int15_merge <- Int15data[,c("idnum",
                          "Int15_Std3")]

Ext15data <- Datafull[,c("idnum",
                        "Ext15_log")]

Ext15data <- Ext15data[complete.cases(Ext15data$Ext15_log), ]
Ext15data$Ext15_Std3 <- (Ext15data$Ext15_log-0.95)/0.13

Ext15_merge <- Ext15data[,c("idnum",
                          "Ext15_Std3")]


colnames(Datafull)

#merge standardized variables

Datafull1 <- left_join(Datafull, Int5_merge, by = "idnum")
Datafull2 <- left_join(Datafull1, Int9_merge, by = "idnum")
Datafull3 <- left_join(Datafull2, Int15_merge, by = "idnum")
Datafull4 <- left_join(Datafull3, Ext5_merge, by = "idnum")
Datafull5 <- left_join(Datafull4, Ext9_merge, by = "idnum")
Datafull6 <- left_join(Datafull5, Ext15_merge, by = "idnum")
Datafull7 <- left_join(Datafull6, Int3_merge, by = "idnum")
Datafull8 <- left_join(Datafull7, Ext3_merge, by = "idnum")

colnames(Datafull8)

#### create SES variable ####

#description preregistration:
#Wewillcreate a familiy-level SES composite aggregating the following measures:the average of standardized parent educational attainment and standardized, 
#log-transformed household income. We will average family-level SES accross baseline, age 9 and age 15.
#Household income, reporting total income earned before taxesin the household.

#Parent-reportededucation levelat baselineand 9years
#Parents categorized education level as: “some high school or less”, “high school diploma or GED”, “some college or 2-year degree”, “Bachelor’s degree”,
#"graduate school or higher”. We will use the highest education per parent and average across parents.

#However, most children live with their mothers ;
#see ffcws.princeton.edu/sites/g/files/toruqf4356/files/documents/attrition_table_1.pdf --> only 24% of parents are married
# see prinscreen "ffcw PCG" document and see 98% mothers are primary caregivers
#we therefore only use education and income of mothers

#mother's education at baseline cm1edu
#mother's education at age 9: cm5edu
#Mother's education at age 15: cp6edu

#mother's income at baseline: cm1hhinc
#mother's income at age 9:cm5hhinc
#mother's income at age 15:cp6hhinc

#create dataset with necessary items

baseline_0 <- baseline[,c("idnum",
                          "cm1edu",
                          "cm1hhinc")]

age9_0 <- age9[,c("idnum",
                  "cm5edu",
                  "cm5hhinc")]


age15_0 <- age15[,c("idnum",
                    "cp6edu",
                    "cp6hhinc")]

merge1 <- left_join(baseline_0, age9_0, by = "idnum")
merge2 <- left_join(merge1, age15_0, by = "idnum")
data_ses <- merge2
colnames(data_ses)

describe(data_ses)
data_ses[data_ses < 0] <- NA


#check income
library(ggplot2)  # For creating plots

#baseline
#dont seem to be crazy outliers
describe(data_ses$cm1hhinc)
ggplot(data_ses, aes(y = cm1hhinc)) +
  geom_boxplot()

# Count the number of participants with values higher than 200,000
#n=0
num_participants <- sum(data_ses$cm1hhinc > 500000, na.rm = TRUE)
print(num_participants)


#age9
describe(data_ses$cm5hhinc)
ggplot(data_ses, aes(y = cm5hhinc)) +
  geom_boxplot()


# Count the number of participants with values higher than 200,000
#n=3
num_participants <- sum(data_ses$cm5hhinc > 500000, na.rm = TRUE)
print(num_participants)

#windsorize them 
# Set values higher than 500,000 to 500,000
# income of 3 partiicpants set to 500.000
# Create a new column and copy values
data_ses$cm5hhinc_winds <- data_ses$cm5hhinc
data_ses$cm5hhinc_winds[data_ses$cm5hhinc > 500000] <- 500000

describe(data_ses$cm5hhinc)
describe(data_ses$cm5hhinc_winds)

#age15
describe(data_ses$cp6hhinc)
ggplot(data_ses, aes(y = cp6hhinc)) +
  geom_boxplot()

# Count the number of participants with values higher than 200,000
#n=5
num_participants <- sum(data_ses$cp6hhinc > 500000, na.rm = TRUE)
print(num_participants)

# Set values higher than 500,000 to 500,000
# income of 5 partiicpants set to 500.000
# Create a new column and copy values
data_ses$cp6hhinc_winds <- data_ses$cp6hhinc
data_ses$cp6hhinc_winds[data_ses$cp6hhinc > 500000] <- 500000

describe(data_ses$cp6hhinc)
describe(data_ses$cp6hhinc_winds)

describe(data_ses$cm1hhinc)
describe(data_ses$cm5hhinc_winds)
describe(data_ses$cp6hhinc_winds)

data_ses$AvInc <- rowMeans(data_ses[, c("cm1hhinc","cm5hhinc_winds", "cp6hhinc_winds")], na.rm=FALSE)
#strong samplesize decrease, might be worth also checking the analysis with only baseline income
describe(data_ses$AvInc)
describe(data_ses$cm1hhinc)
cor.test(data_ses$cm1hhinc,data_ses$cm5hhinc_winds)
cor.test(data_ses$cm1hhinc, data_ses$cp6hhinc_winds)
cor.test(data_ses$cm5hhinc_winds,data_ses$cp6hhinc_winds)

## education
#1= less HS, 2= HS or equiv, 3= some coll, tech, 4= coll or grad
describe(data_ses$cm1edu)
table(data_ses$cm1edu)
describe(data_ses$cm5edu)
table(data_ses$cm5edu)
describe(data_ses$cp6edu)
table(data_ses$cp6edu)

data_ses$AvEdu <- rowMeans(data_ses[, c("cm1edu","cm5edu", "cp6edu")], na.rm=FALSE)
describe(data_ses$AvEdu)
cor.test(data_ses$cm1edu, data_ses$cm5edu)
cor.test(data_ses$cm1edu, data_ses$cp6edu)
cor.test(data_ses$cm1edu, data_ses$cp6edu)

#create ZEduInc score

# load the dplyr package
library(dplyr)

# log-transform the column and add it as a new column, see Laura Engelhardt paper
data_ses <- data_ses %>% 
  mutate(log_par_income = log(AvInc + 1))

#create Education-Income variable
hist(data_ses$log_par_income)
hist(data_ses$AvInc)

data_ses$ZEdu = scale(data_ses$AvEdu)
data_ses$ZInc = scale(data_ses$log_par_income)
data_ses$ZEduInc = rowMeans(cbind(scale(data_ses$AvEdu), scale(data_ses$log_par_income)), na.rm=F)

describe(data_ses$AvEdu)
describe(data_ses$AvInc)
describe(data_ses$ZInc)
describe(data_ses$ZEdu)
hist(data_ses$ZEduInc)

cor.test(data_ses$log_par_income, data_ses$AvEdu) #r.54**
cor.test(data_ses$AvInc, data_ses$AvEdu) #r.53**
cor.test(data_ses$ZInc, data_ses$ZEdu) #r.54**

colnames(Datafull8)
Datafull9 <- left_join(Datafull8, data_ses, by = "idnum")

#### regress DNAm out for array ####
# Filter rows where the value in column x is not NA
#otherwise we cannot attach the new variables to the dataframe cause number of rows do not line up

Clocks_9 <- Datafull9[complete.cases(Datafull9$DunedinPACE_9_raw), ]

regPace <- lm(DunedinPACE_9_res ~ factor(Array), data=Clocks_9)
Clocks_9$DunedinPACE_9_res_array <- residuals(regPace) 
Clocks_9$DunedinPACE_9_res_array_std <- scale(residuals(regPace)) 

regPhenoAge <- lm(PCPheno_9_accell_res ~ factor(Array), data=Clocks_9)
Clocks_9$PCPheno_9_accell_res_array <- residuals(regPhenoAge) 
Clocks_9$PCPheno_9_accell_res_array_std <- scale(residuals(regPhenoAge)) 

regGrimAge <- lm(PCGrim_9_accell_res ~ factor(Array), data=Clocks_9)
Clocks_9$PCGrim_9_accell_res_array <- residuals(regGrimAge) 
Clocks_9$PCGrim_9_accell_res_array_std <- scale(residuals(regGrimAge)) 

Clocks_15 <- Datafull9[complete.cases(Datafull9$DunedinPACE_15_raw), ]

regPaceAge <- lm(DunedinPACE_15_res ~ factor(Array), data=Clocks_15)
Clocks_15$DunedinPACE_15_res_array <- residuals(regPaceAge) 
Clocks_15$DunedinPACE_15_res_array_std <- scale(residuals(regPaceAge)) 

regPhenoAge <- lm(PCPheno_15_accell_res ~ factor(Array), data=Clocks_15)
Clocks_15$PCPheno_15_accell_res_array <- residuals(regPhenoAge) 
Clocks_15$PCPheno_15_accell_res_array_std <- scale(residuals(regPhenoAge)) 

regGrimAge <- lm(PCGrim_15_accell_res ~ factor(Array), data=Clocks_15)
Clocks_15$PCGrim_15_accell_res_array <- residuals(regGrimAge) 
Clocks_15$PCGrim_15_accell_res_array_std <- scale(residuals(regGrimAge)) 

Clocks_9_merge <- Clocks_9[,c("idnum","DunedinPACE_9_res_array",
                              "DunedinPACE_9_res_array_std","PCPheno_9_accell_res_array", 
                              "PCPheno_9_accell_res_array_std", "PCGrim_9_accell_res_array", "PCGrim_9_accell_res_array_std")]

Clocks_15_merge <- Clocks_15[,c("idnum","DunedinPACE_15_res_array",
                              "DunedinPACE_15_res_array_std","PCPheno_15_accell_res_array", 
                              "PCPheno_15_accell_res_array_std", "PCGrim_15_accell_res_array", "PCGrim_15_accell_res_array_std")]


Datafull10 <- left_join(Datafull9, Clocks_9_merge, by = "idnum")
Datafull11 <- left_join(Datafull10, Clocks_15_merge, by = "idnum")


#### Create Parenting Stress , Closeness and Neighborhood variables across ages ####


### Parenting stress across age 5, 9, 15 ####

describe(age5_cov$parent_stress_age5)
describe(age9_cov$parent_stress_age9)
describe(age15_cov$parent_stress_age15)

ParentstNeigh0 <- left_join(age5_cov, age9_cov, by = "idnum")
ParentstNeigh1 <- left_join(ParentstNeigh0, age15_cov, by = "idnum")

ParentstNeigh1$parent_stress_acrossages <- rowMeans(ParentstNeigh1[, c("parent_stress_age5", "parent_stress_age9","parent_stress_age15")], na.rm = FALSE)
describe(ParentstNeigh1$parent_stress_acrossages)
hist(ParentstNeigh1$parent_stress_acrossages)

### Parenting Closeness across age  9, 15 ####

describe(age9_cov$Close9)
describe(age15_cov$Close15)

datatestfile <- left_join(age9_cov, age15_cov, by = "idnum")
colnames(datatestfile)
cor.test(datatestfile$Close9, datatestfile$Close15)

ParentstNeigh1$Close_acrossages <- rowMeans(ParentstNeigh1[,c("Close9", "Close15")], na.rm = FALSE)
describe(ParentstNeigh1$Close_acrossages)


### Neighborhood conditions across age  5, 9, 15 ####
describe(age5_cov$mean_score_ngh5)
describe(age9_cov$mean_score_ngh9)
describe(age15_cov$mean_score_ngh15)

#when creating a meanscore
#very low n
#so only include age 9 to increase sample size

ParentstNeigh1$mean_score_ngh_acrossages <- rowMeans(ParentstNeigh1[,c("mean_score_ngh5", "mean_score_ngh9", "mean_score_ngh15")], na.rm = FALSE)
describe(ParentstNeigh1$mean_score_ngh_acrossages)


ParentstNeigh1_merge <- ParentstNeigh1[,c("idnum",
                                       "parent_stress_acrossages",
                                       "Close_acrossages")]
                                      
                                       

Datafull12 <- left_join(Datafull11, ParentstNeigh1_merge, by = "idnum")

#read in datafile where census data has been prepped
#see r script 20230713_Censusdata for full data prep

census_merge <- read.csv("/Volumes/MPRG-Biosocial/Projects/03_data/003_FFCW/Raffington/Neiborhood Census Computed 18072023.csv")
colnames(census_merge)

Datafull13 <- left_join(Datafull12, census_merge, by = "idnum")
colnames(Datafull13)


############ Neighborhood variables with Index data ##########

### Load Packages ####
install.packages("haven")
library(haven)


### Read in FFCW data ####
dataset1 <- read_dta("/Volumes/MPRG-Biosocial/Projects/03_data/003_FFCW/Raffington/ff_dis_seda_2019_pub.dta")
dataset2 <- read_dta("/Volumes/MPRG-Biosocial/Projects/03_data/003_FFCW/Raffington/ff_opin_cim_b9y_pub1.dta")
dataset3 <- read_dta("/Volumes/MPRG-Biosocial/Projects/03_data/003_FFCW/Raffington/ff_sch_crdc_9y15y_pub2_2019.dta")
dataset4 <- read_dta("/Volumes/MPRG-Biosocial/Projects/03_data/003_FFCW/Raffington/ffgeo6_all_pub.dta")

### check column names 

colnames(dataset1)  
colnames(dataset2) 
colnames(dataset3) 
colnames(dataset4) 

### read in column with variable names

# list of datasets
datasets <- list(dataset1, dataset2, dataset3, dataset4)

# list of variable names
var_names <- c("rgm1opin_ccvcs_race_theil_2000",
               "rgm5opin_ccvcs_race_theil_2000",
               "tm3pfbpl_cen00",
               "tm4pfbpl_cen00",
               "tm5pfbpl_cen00",
               "tp6pfbpl_acs15",
               "tm3ppuba_cen00",
               "tf4ppuba_cen00",
               "tm5pfbpl_cen00",
               "tp6ppuba_acs15")

# loop over datasets
for (i in 1:length(datasets)) {
  # loop over variable names
  for (var_name in var_names) {
    if (var_name %in% names(datasets[[i]])) {
      print(paste("Variable", var_name, "found in dataset", i))
    }
  }
}

#check in which dataset the consesus data is
#[1] "Variable rgm1opin_ccvcs_race_theil_2000 found in dataset 2"
#[1] "Variable rgm5opin_ccvcs_race_theil_2000 found in dataset 2"
#[1] "Variable tm3pfbpl_cen00 found in dataset 4"
#[1] "Variable tm4pfbpl_cen00 found in dataset 4"
#[1] "Variable tm5pfbpl_cen00 found in dataset 4"
#[1] "Variable tp6pfbpl_acs15 found in dataset 4"
#[1] "Variable tm3ppuba_cen00 found in dataset 4"
#[1] "Variable tf4ppuba_cen00 found in dataset 4"
#[1] "Variable tf5ppuba_cen00 found in dataset 4"

census1 <- dataset2[,c("idnum",
                       "rgm1opin_ccvcs_race_theil_2000",
                       "rgm5opin_ccvcs_race_theil_2000")]

census2 <- dataset4[,c("idnum",
                       "tm3pfbpl_cen00",
                       "tm4pfbpl_cen00",
                       "tm5pfbpl_cen00",
                       "tp6pfbpl_acs15",
                       "tm3ppuba_cen00",
                       "tf4ppuba_cen00",
                       "tf5ppuba_cen00",
                       "tp6ppuba_acs15")]




census_merge <- left_join(census1, census2, by = "idnum")                       


colnames(census_merge) 
describe(census_merge)

# Replace specific values with NA
census_merge[census_merge < 0] <- NA
describe(census_merge)



census_ngh <- left_join(ParentstNeigh1, census_merge, by = "idnum")  
colnames(census_ngh)

cor.test(census_ngh$mean_score_ngh5,census_ngh$tf4ppuba_cen00)
cor.test(census_ngh$mean_score_ngh9,census_ngh$tf5ppuba_cen00)
cor.test(census_ngh$mean_score_ngh15,census_ngh$tp6ppuba_acs15)

cor.test(census_ngh$tm5pfbpl_cen00,census_ngh$tf5ppuba_cen00)  #census public assistance with census poverty level, .53
cor.test(census_ngh$mean_score_ngh9,census_ngh$tf5ppuba_cen00) #census public assistance with neighborhood conditions .30
cor.test(census_ngh$mean_score_ngh9,census_ngh$tm5pfbpl_cen00) #census poverty level with neighborhood conditions .39

describe(census_ngh$tm5pfbpl_cen00)
describe(census_ngh$tf5ppuba_cen00)
describe(census_ngh$mean_score_ngh9)

#create a log score for ngh9 cause very skewed
census_ngh$mean_score_ngh9_log <- log(census_ngh$mean_score_ngh9 + 1)  

#still looks somewhat skewed
hist(census_ngh$mean_score_ngh9_log)  
hist(census_ngh$mean_score_ngh9)



census_ngh$Zpassist <- scale(census_ngh$tf5ppuba_cen00)
census_ngh$Zpoverty <- scale(census_ngh$tm5pfbpl_cen00)
census_ngh$Zngh <- scale(census_ngh$mean_score_ngh9_log)

hist(census_ngh$Zpassist)
hist(census_ngh$Zpoverty)
hist(census_ngh$Zngh)
hist(census_ngh$mean_score_ngh9)

census_ngh$Ngh9_overall <- rowMeans(census_ngh[,c("Zpassist", "Zpoverty", "Zngh")], na.rm = FALSE)
describe(census_ngh$Ngh9_overall)
hist(census_ngh$Ngh9_overall)

census_ngh$Ngh9_census <- rowMeans(census_ngh[,c("Zpassist", "Zpoverty")], na.rm = FALSE)
describe(census_ngh$Ngh9_census)
hist(census_ngh$Ngh9_census)

### neighborhood conditions
#we have three options
describe(census_ngh$Ngh9_overall)
describe(census_ngh$Ngh9_census)
describe(census_ngh$mean_score_ngh9_log)

census_merge <- census_ngh[,c("idnum",
                              "Ngh9_overall",
                              "Ngh9_census",
                              "mean_score_ngh9_log",
                              "rgm1opin_ccvcs_race_theil_2000",
                              "rgm5opin_ccvcs_race_theil_2000")]

write.csv(census_merge, file = "/Volumes/MPRG-Biosocial/Projects/03_data/003_FFCW/Raffington/Neiborhood Census Computed 18072023.csv", row.names = FALSE)

data_neibhorhood <- read.csv("/Volumes/MPRG-Biosocial/Projects/03_data/003_FFCW/Raffington/Neiborhood Census Computed 18072023.csv")

colnames(data_neibhorhood)



### Create Puberty Measure age 9 ####
# Puberty AGE 9 - variables: 
# PCG Questions: p5h17, p5h17a, p5h17b, p5h23 (4 variables)
# PCG Male-Specific Questions: p5h19, p5h19a (2 variables)
# PCG Female-Specific Questions: p5h21, p5h22 

describe(Datafull13$p5h17)  # PCG Questions
describe(Datafull13$p5h17a) 
describe(Datafull13$p5h17b) 
describe(Datafull13$p5h19)  # PCG Male-Specific Questions
describe(Datafull13$p5h19a)
describe(Datafull13$p5h21)  # PCG Female-Specific Questions
describe(Datafull13$p5h22)

table(Datafull13$p5h22)
table(Datafull13$p5h22_recoded)

# Recode p5h22 variable -  1 = 4 and 2 = 1
#this is the mences variable
Datafull13$p5h22_recoded <- ifelse(Datafull13$p5h22 == 1, 4, ifelse(Datafull13$p5h22 == 2, 1, Datafull13$p5h22))

# View the recoded table
table(Datafull13$p5h22_recoded, Datafull13$p5h22)

# load the library
library(dplyr)

#create a score seperately for boys and girls

# For boys
Datafull13 <- Datafull13 %>%
  mutate(boys_meanscore_pds = ifelse(cm1bsex == 1, 
                                 rowMeans(cbind(p5h17, p5h17a, p5h17b, p5h19, p5h19a), na.rm = FALSE), 
                                 NA))

describe(Datafull13$boys_meanscore_pds)

Datafull13 <- Datafull13 %>%
  mutate(girls_meanscore_pds = ifelse(cm1bsex == 2, 
                                     rowMeans(cbind(p5h17, p5h17a, p5h17b, p5h21, p5h22_recoded), na.rm = FALSE), 
                                     NA))

describe(Datafull13$girls_meanscore_pds)
table(Datafull13$girls_meanscore_pds)

Datafull13$Puberty <- ifelse(!is.na(Datafull13$boys_meanscore_pds), Datafull13$boys_meanscore_pds, Datafull13$girls_meanscore_pds) 

describe(Datafull13$Puberty)
describe(Datafull13$girls_meanscore_pds)
describe(Datafull13$boys_meanscore_pds)

hist(Datafull13$Puberty)




###### create dataset for analyses

colnames(Datafull13)
DataSEM <- Datafull13[,c("idnum",      #ID
                       "cm1bsex",      #Sex
                       "Age9_months",  #age 9 months
                       "Age15_months", #age 15 months
                       "Int3",         #Internalizing & Externalizing problems
                       "Int3_log",
                       "Int3_Std",
                       "Ext3",                        
                       "Ext3_log",
                       "Ext3_Std",
                       "Int5",
                       "Int5_log",
                       "Int5_Std3",
                       "Ext5",                        
                       "Ext5_log",
                       "Ext5_Std3",
                       "Int9",
                       "Int9_log",
                       "Int9_Std3",
                       "Ext9",                        
                       "Ext9_log",
                       "Ext9_Std3",
                       "Int15",
                       "Int15_log",
                       "Int15_Std3",
                       "Ext15",                        
                       "Ext15_log",
                       "Ext15_Std3",
                       "Array" ,      #array (450K or EPIC)
                       "k5me_poam45", #raw clock data (in the file so we can later cross-check)
                       "k5me_pcgrim",
                       "k5me_pcphenoage",
                       "k6me_poam45",
                       "k6me_pcgrim",
                       "k6me_pcphenoage",
                       "k5mk_poam45",
                       "k5mk_pcgrim",
                       "k5mk_pcphenoage",
                       "k6mk_poam45",
                       "k6mk_pcgrim",
                       "k6mk_pcphenoage",
                       "DunedinPACE_9_raw", #DNA-methylation
                       "DunedinPACE_9_res",
                       "DunedinPACE_9_res_std",
                       "DunedinPACE_9_res_array",
                       "DunedinPACE_9_res_array_std",
                       "PCGrim_9_raw",
                       "PCGrim_9_accell_res", 
                       "PCGrim_9_accell_res_std",
                       "PCGrim_9_accell_res_array",
                       "PCGrim_9_accell_res_array_std",
                       "PCPheno_9_raw",
                       "PCPheno_9_accell_res",
                       "PCPheno_9_accell_res_std",
                       "PCPheno_9_accell_res_array",
                       "PCPheno_9_accell_res_array_std",
                       "DunedinPACE_15_raw",
                       "DunedinPACE_15_res",
                       "DunedinPACE_15_res_std",
                       "DunedinPACE_15_res_array",
                       "DunedinPACE_15_res_array_std",
                       "PCGrim_15_raw",
                       "PCGrim_15_accell_res",
                       "PCGrim_15_accell_res_std",
                       "PCGrim_15_accell_res_array",
                       "PCGrim_15_accell_res_array_std",
                       "PCPheno_15_raw",
                       "PCPheno_15_accell_res",
                       "PCPheno_15_accell_res_std",
                       "PCPheno_15_accell_res_array",
                       "PCPheno_15_accell_res_array_std",
                       "eversmoke",  #smoking
                       "ck6ethrace", #race
                       "h6a8",       #skin colour
                       "race_white",
                       "race_black", 
                       "race_hispanic", 
                       "race_other", 
                       "race_multi",
                       "ZEduInc",    #SES
                       "parent_stress_age9", #parenting stress
                       "parent_stress_age15",
                       "parent_stress_acrossages", #parenting stress 5, 9, 15
                       "Close9",    #closeness
                       "Close15",
                       "Close_acrossages", #closeness  9 , 15
                       "scale_cesd",       #depressive symptoms age 15
                       "cesd_log",       
                       "scale_anxt",     #anxiety symptoms age 15
                       "scale_anxt_log", 
                       "k6e10", #police
                       "k6e16",
                       "Ngh9_overall",   #neighborhood conditions overall age 9
                       "Ngh9_census",    #neighborhoodcensus age 9
                       "mean_score_ngh9_log",  #neighborhood conditions census age 9
                       "rgm1opin_ccvcs_race_theil_2000",    #racial segregation age 0
                       "rgm5opin_ccvcs_race_theil_2000",   #racial segregation age 9
                       "ch5bmip",  #bmi
                       "ck6bmip",
                       "Puberty")] #Puberty score
         
colnames(DataSEM)               
                       
# Install and load the openxlsx package
install.packages("openxlsx")
library(openxlsx)
#create descriptives table
desc_DataSEM <- describe(DataSEM)  
print(desc_DataSEM)
#write.xlsx(desc_DataSEM, file = "/Volumes/MPRG-Biosocial/Projects/04_data_analysis/005_FFCW/descriptives_analysis data_12072023.xlsx", rowNames = TRUE)
write.xlsx(desc_DataSEM, file = "/Users/willems/Desktop/Projects/FFCW/analyses FFCW/descriptives_analysis data_18072023.xlsx", rowNames = TRUE)


                  

# Save a data frame as a CSV file with a specified path
write.csv(DataSEM, file = "/Volumes/MPRG-Biosocial/Projects/03_data/003_FFCW/July 2023 Processed Data/DataSEM_18072023.csv", row.names = FALSE)
datacheck <- read.csv("/Volumes/MPRG-Biosocial/Projects/03_data/003_FFCW/July 2023 Processed Data/DataSEM_18072023.csv")
colnames(datacheck)

write.csv(DataSEM, file = "/Users/willems/Desktop/Projects/FFCW/analyses FFCW/DataSEM_18072023.csv", row.names = FALSE)
datacheck <- read.csv("/Users/willems/Desktop/Projects/FFCW/analyses FFCW/DataSEM_18072023.csv")




describe(DataSEM)
colnames(DataSEM)

Data <- DataSEM
colnames(Data)

##### Descriptives total sample ########
describe(Data$Int_total)

describe(Data$Int_total)                     
describe(Data$Ext_total) 






### double check DNAm measures

dnamcheck <- Data[c("Array",
                         "k5me_poam45",
                         "k5me_pcgrim",
                         "k5me_pcphenoage",
                         "k6me_poam45",
                         "k6me_pcgrim",
                         "k6me_pcphenoage",
                         "DunedinPACE_9_raw",
                         "PCGrim_9_raw",
                         "PCPheno_9_raw",
                         "DunedinPACE_15_raw",
                         "PCGrim_15_raw",
                         "PCPheno_15_raw",
                        "k5mk_poam45",
                        "k5mk_pcgrim",
                        "k5mk_pcphenoage",
                        "k6mk_poam45",
                        "k6mk_pcgrim",
                        "k6mk_pcphenoage")]

Data$Int_total <- rowMeans(Data[, c("Int3",
                                    "Int5",
                                    "Int9",
                                    "Int15")], na.rm=FALSE)

Data$Ext_total <- rowMeans(Data[, c("Ext3",
                                    "Ext5",
                                    "Ext9",
                                    "Ext15")], na.rm=FALSE)


 


############ Correlations between variables #################
#me_* = Illumina Infinium MethylationEPIC (EPIC) array variables 
#mk_*=Illumina Infinium Human Methylation450K (450K) array variables 
colnames(Data)

FullDat_Cor   <- Data[,c("k5me_poam45",
                             "k5me_pcgrim",
                             "k5me_pcphenoage",
                             "k6me_poam45",
                             "k6me_pcgrim",
                             "k6me_pcphenoage")]

FullDat_Cor   <- Data[,c("k5mk_poam45",
                             "k5mk_pcgrim",
                             "k5mk_pcphenoage",
                             "k6mk_poam45",
                             "k6mk_pcgrim",
                             "k6mk_pcphenoage")]
                             
FullDat_Cor   <- Data[,c("DunedinPACE_9_raw",
                             "PCGrim_9_raw",
                             "PCPheno_9_raw",
                             "DunedinPACE_15_raw",
                             "PCGrim_15_raw",
                             "PCPheno_15_raw")]

FullDat_Cor   <- Data[,c( "DunedinPACE_9_res",
                          "PCGrim_9_accell_res",
                          "PCPheno_9_accell_res",
                          "DunedinPACE_15_res",
                          "PCGrim_15_accell_res",
                          "PCPheno_15_accell_res",
                         "Array")]    

FullDat_Cor   <- Data[,c( "DunedinPACE_9_raw",
                          "PCGrim_9_raw",
                          "PCPheno_9_raw",
                          "DunedinPACE_15_raw",
                          "PCGrim_15_raw",
                          "PCPheno_15_raw",
                          "Array")]   
                             

#create dataset for correlation table
FullDat_Cor   <- Data[,c(
                              "Int3",
                              "Int3_log",
                              "Ext3",                        
                              "Ext3_log",
                              "Int5",
                              "Int5_log",
                              "Ext5",                        
                              "Ext5_log",
                              "Int9",
                              "Int9_log",
                              "Ext9",                        
                              "Ext9_log",
                              "Int15",
                              "Int15_log",
                              "Ext15",                        
                              "Ext15_log",
                              "DunedinPACE_9_raw",
                              "DunedinPACE_9_res",
                              "PCGrim_9_raw",
                              "PCGrim_9_accell_res", 
                              "PCPheno_9_raw",
                              "PCPheno_9_accell_res",
                              "DunedinPACE_15_raw",
                              "DunedinPACE_15_res",
                              "PCGrim_15_raw",
                              "PCGrim_15_accell_res",
                              "PCPheno_15_raw",
                              "PCPheno_15_accell_res")]

#create dataset for correlation table
#cortables for log vs non log look the same

FullDat_Cor   <- Data[,c(
  "Int3)]",
  "Int5_log",
  "Int9_log",
  "Int15_log",
  "Ext3_log",    
  "Ext5_log", 
  "Ext9_log",
  "Ext15_log")]

FullDat_Cor   <- Data[,c(
  "Int3",
  "Ext3",
  "Int5",
  "Ext5",
  "Int9",
  "Ext9",
  "Int15",
  "Ext15")]

#create dataset for correlation table
FullDat_Cor   <- Data[,c( "Int3",
                          "Int5",
                          "Int9",
                          "Int15",
                          "DunedinPACE_9_res",
                          "PCGrim_9_accell_res",
                          "PCPheno_9_accell_res",
                          "DunedinPACE_15_res",
                          "PCGrim_15_accell_res",
                          "PCPheno_15_accell_res")]

FullDat_Cor   <- Data[,c( "Int3",
                          "Int5",
                          "Int9",
                          "Int15",
                          "DunedinPACE_9_res",
                          "PCGrim_9_accell_res",
                          "PCPheno_9_accell_res",
                          "DunedinPACE_15_res",
                          "PCGrim_15_accell_res",
                          "PCPheno_15_accell_res")]





#create dataset for correlation table
FullDat_Cor   <- Data[,c( "Int3_log",
                              "Int5_log",
                              "Int9_log",
                              "Int15_log",
                              "DunedinPACE_9_res",
                              "PCGrim_9_accell_res",
                              "PCPheno_9_accell_res",
                              "DunedinPACE_15_res",
                              "PCGrim_15_accell_res",
                              "PCPheno_15_accell_res")]

#create dataset for correlation table
FullDat_Cor   <- Data[,c( "Int3",
                          "Int5",
                          "Int9",
                          "Int15",
                          "DunedinPACE_9_res_array",
                          "PCGrim_9_accell_res_array",
                          "PCPheno_9_accell_res_array",
                          "DunedinPACE_15_res_array",
                          "PCGrim_15_accell_res_array",
                          "PCPheno_15_accell_res_array")]

#create dataset for correlation table
FullDat_Cor   <- Data[,c( "Int3_log",
                          "Int5_log",
                          "Int9_log",
                          "Int15_log",
                          "DunedinPACE_9_res_array",
                          "PCGrim_9_accell_res_array",
                          "PCPheno_9_accell_res_array",
                          "DunedinPACE_15_res_array",
                          "PCGrim_15_accell_res_array",
                          "PCPheno_15_accell_res_array")]



FullDat_Cor   <- Data[,c( "Ext3",
                              "Ext5",
                              "Ext9",
                              "Ext15",
                              "DunedinPACE_9_res",
                              "PCGrim_9_accell_res",
                              "PCPheno_9_accell_res",
                              "DunedinPACE_15_res",
                              "PCGrim_15_accell_res",
                              "PCPheno_15_accell_res")]


                              
                       
#create dataset for correlation table
FullDat_Cor   <- Data[,c( "DunedinPACE_9_raw",
                              "DunedinPACE_15_raw",
                              "PCGrim_9_raw",
                              "PCGrim_15_raw",
                              "PCPheno_9_raw",
                              "PCPheno_15_raw")]
                            

#correlations with covariates
FullDat_Cor   <- Data[,c("ck6ethrace",
                         "ZEduInc",
                         "eversmoke",
                         "DunedinPACE_9_res",
                         "PCGrim_9_accell_res",
                         "PCPheno_9_accell_res",
                         "DunedinPACE_15_res",
                         "PCGrim_15_accell_res",
                         "PCPheno_15_accell_res")]
  
#correlations with covariates
FullDat_Cor   <- Data[,c("ck6ethrace",
                         "ZEduInc",
                         "eversmoke",
                         "Int3_log",
                         "Int5_log",
                         "Int9_log",
                         "Int15_log",
                         "Ext3_log",    
                         "Ext5_log", 
                         "Ext9_log",
                         "Ext15_log")]
                     

# Install and load the `psych` package
install.packages("psych")
library(psych)
install.packages("apaTables")
library(apaTables)


# calculate correlation matrix
# calculate significance levels
cormatrix <- corr.test(FullDat_Cor, use = "pairwise.complete.obs", adjust = "none", ci = TRUE)
print(cormatrix, short=FALSE)
lowertable <- lowerCor(FullDat_Cor,use = "pairwise.complete.obs")
print(lowertable)

#safe as apa.cor table from apaTables
tablecor <- apa.cor.table(FullDat_Cor, "/Volumes/MPRG-Biosocial/Projects/04_data_analysis/005_FFCW/001_Racism DNAm/July 2023/Cor Clocks Raw.doc")  









