############### FFCW PROJECT #################
#### Title:  FFCW                         ###       
#### Goal:   Data exploration             ###      
#### Date:   23.05.2023                   ###                  
#### Author: Yayouk Willems & Muna Aikins ###      
#############################################


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

