############### FFCW PROJECT #################
#### Title:  FFCW                         ###       
#### Goal:   Update Neighborhood var      ###      
#### Date:   25.09.2023                   ###                  
#### Author: Yayouk Willems & Muna Aikins ###      
#############################################


#Update Neighborhood-level SES variable
#Include Neighborhoodvariable across age, instead of age 9 only.

#Neighborhood-level SES consists of two measures from census data (poverty and public assistance) and from home visits

### Load Packages ####
install.packages("haven")
library(haven)
install.packages("haven")
install.packages("psych")
install.packages("ltm")

library(haven)
library(psych)
library(ltm)

#### CENSUS data ####

#### select the right variables from the right datasets ####

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

describe(census1$rgm1opin_ccvcs_race_theil_2000)
describe(census2$tm5pfbpl_cen00)


census2 <- dataset4[,c("idnum",
                       "tm3pfbpl_cen00",
                       "tm4pfbpl_cen00", #Poverty age 5
                       "tm5pfbpl_cen00", #Poverty age 9
                       "tp6pfbpl_acs15", #Poverty age 15
                       "tm3ppuba_cen00", 
                       "tf4ppuba_cen00", #public assistance age 5
                       "tf5ppuba_cen00",  #public assistance age 9
                       "tp6ppuba_acs15")]  #public assistance age 15

hist(census2$tm4pfbpl_cen00)
hist(census2$tf4ppuba_cen00)

describe(census2$tm4pfbpl_cen00)
describe(census2$tm5pfbpl_cen00)

# Replace specific values with NA
census2[census2 < 0] <- NA
describe(census2)

#### Create overall poverty and public assistance scale ####

#check correlations between poverty census measures over time
cor.test(census2$tm4pfbpl_cen00,census2$tm5pfbpl_cen00)
cor.test(census2$tm4pfbpl_cen00,census2$tp6pfbpl_acs15)
cor.test(census2$tm5pfbpl_cen00,census2$tp6pfbpl_acs15)

census2$poverty_overall <- rowMeans(census2[,c("tm4pfbpl_cen00", "tm5pfbpl_cen00", "tp6pfbpl_acs15")], na.rm = TRUE)
describe(census2$poverty_overall)
hist(census2$poverty_overall)

census2$publicass_overall <- rowMeans(census2[,c("tf4ppuba_cen00", "tf5ppuba_cen00", "tp6ppuba_acs15")], na.rm = TRUE)
describe(census2$publicass_overall)
hist(census2$publicass_overall)

cor.test(census2$tf4ppuba_cen00, census2$tf5ppuba_cen00)
cor.test(census2$tf4ppuba_cen00, census2$tp6ppuba_acs15)
cor.test(census2$tf5ppuba_cen00, census2$tp6ppuba_acs15)

#### HOME VISITS ####

#### Age 15 ####

age15 <- read_dta("/Volumes/MPRG-Biosocial/Projects/03_data/003_FFCW/FFCWS Public data/Year 15/FF_wave6_2020v2.dta")

#select items of interest at age 15
age15_cov <- age15[,c("idnum",
                      "o6a1", #neighborhood conditions
                      "o6a2",
                      "o6a3", 
                      "o6a4",
                      "o6a5", 
                      "o6a9")] #bmi

# Replace specific values with NA
age15_cov[age15_cov < 0] <- NA
describe(age15_cov)

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

#### Age 9 ####

#here we read in the FFCW dataset with year 15 data and select the variables of interest
age9 <- read_dta("/Volumes/MPRG-Biosocial/Projects/03_data/003_FFCW/FFCWS Public data/Year 9/FF_wave5_2020v2.dta")

#select items from homevisit
age9_cov <- age9[,c("idnum",
                    "o5a1", 
                    "o5a2", 
                    "o5a3", 
                    "o5a4", 
                    "o5a5", 
                    "o5a9")]
#process data
describe(age9_cov)   


# Replace specific values with NA
# Set negative values in the dataframe to NA
age9_cov[age9_cov< 0] <- NA
describe(age9_cov)


# Calculate the mean score of the selected items
mean_score_ngh9 <- rowMeans(age9_cov[, c("o5a1", "o5a2", "o5a3", "o5a4", "o5a5", "o5a9")], na.rm = FALSE)

# Add the mean score as a new column in the dataset
age9_cov$mean_score_ngh9 <- mean_score_ngh9

# Print the mean score
describe(mean_score_ngh9)

# Create a subset of the data with the 4 items
subset_data <- age9_cov[, c("o5a1", "o5a2", "o5a3", "o5a4", "o5a5", "o5a9")]
describe(subset_data)

#cronbach alpha = .78
cronbach.alpha(subset_data, na.rm=TRUE)

#### Age 5 ####
#here we read in the FFCW dataset with year 5 data and select the variables of interest
Age5 <- read_dta("/Volumes/MPRG-Biosocial/Projects/03_data/003_FFCW/FFCWS Public data/Year 5/FF_wave4_2020v2.dta")

age5_cov <- Age5[,c("idnum",
                           "o4p1",
                           "o4p2",
                           "o4p3",
                           "o4p4",
                           "o4p5",
                           "o4p9")]
# Replace specific values with NA
# Set negative values in the dataframe to NA
age5_cov[age5_cov< 0] <- NA
describe(age5_cov)

# Calculate the mean score of the selected items
mean_score_ngh5 <- rowMeans(age5_cov[, c("o4p1", "o4p2", "o4p3", "o4p4", "o4p5", "o4p9")], na.rm = FALSE)

# Add the mean score as a new column in the dataset
age5_cov$mean_score_ngh5 <- mean_score_ngh5

# Print the mean score
describe(age5_cov$mean_score_ngh5)

# Create a subset of the data with the 4 items
subset_data <- age5_cov[, c("o4p1", "o4p2", "o4p3", "o4p4", "o4p5", "o4p9")]
describe(subset_data)

#cronbach alpha = .85
cronbach.alpha(subset_data, na.rm=TRUE)

#### Merge all datafiles ####

#census2 has the census data on poverty and public assistance
#age15_cov has home visit of neighborhood conditions at age 15
#age9_cov has home visit of neighborhood conditions at age 9
#age5_cov has home visit of neighborhood conditions at age 5

library(dplyr)

Merge_a <- left_join(census2, age15_cov, by = "idnum")
Merge_b <- left_join(Merge_a, age9_cov, by = "idnum")
Merge_c <- left_join(Merge_b, age5_cov, by = "idnum")

DataNeigh_new <- Merge_c 

#### check data of all neighborhood variables ####

describe(DataNeigh_new)
hist(DataNeigh_new$poverty_overall)
hist(DataNeigh_new$publicass_overall)


hist(DataNeigh_new$mean_score_ngh15)
hist(DataNeigh_new$mean_score_ngh9)
hist(DataNeigh_new$mean_score_ngh5)

# Calculate the mean score of the selected items
NghCon_overall <- rowMeans(DataNeigh_new[, c("mean_score_ngh15", "mean_score_ngh9", "mean_score_ngh5")], na.rm = TRUE)

# Add the mean score as a new column in the dataset
DataNeigh_new$NghCon_overall <- NghCon_overall
describe(DataNeigh_new$NghCon_overall)
hist(DataNeigh_new$NghCon_overall)

#create log scores because of skewness
DataNeigh_new$NghCon_overall_log <- log(DataNeigh_new$NghCon_overall + 1)  
hist(DataNeigh_new$NghCon_overall_log)
describe(DataNeigh_new$NghCon_overall_log)

#create Zscores so we can later add all scores
DataNeigh_new$Zngh <- scale(DataNeigh_new$NghCon_overall_log)
DataNeigh_new$Zpassist <- scale(DataNeigh_new$publicass_overall)
DataNeigh_new$Zpoverty <- scale(DataNeigh_new$poverty_overall)

DataNeigh_new$Ngh_overall_NEW <- rowMeans(DataNeigh_new[,c("Zpassist", "Zpoverty", "Zngh")], na.rm = FALSE)
describe(DataNeigh_new$Ngh_overall_NEW)
hist(DataNeigh_new$Ngh_overall_NEW)

colnames(DataNeigh_new)

cor.test(DataNeigh_new$NghCon_overall,DataNeigh_new$poverty_overall)
cor.test(DataNeigh_new$NghCon_overall,DataNeigh_new$publicass_overall)
cor.test(DataNeigh_new$poverty_overall,DataNeigh_new$publicass_overall)


colnames( DataNeigh_new)
DataNeigh_Updated   <- DataNeigh_new[,c("idnum",
                                        "poverty_overall", 
                                        "publicass_overall", 
                                        "Zngh",                 
                                        "Zpassist",            
                                        "Zpoverty",
                                        "Ngh_overall_NEW")]
describe(DataNeigh_Updated)                                       

#safe data to project folder
write.csv(DataNeigh_Updated, file = "/Volumes/MPRG-Biosocial/Projects/03_data/003_FFCW/July 2023 Processed Data/DataNeigh_Updated_25092023.csv", row.names = FALSE)

