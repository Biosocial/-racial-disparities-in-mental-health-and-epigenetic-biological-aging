############### FFCW PROJECT #################
#### Title:  FFCW                         ###       
#### Goal:   Add prenatal + prep Mplus    ###      
#### Date:   09.11.2023                   ###                  
#### Author: Yayouk Willems & Muna Aikins ###      
#############################################

#We will apply latent change model in Mplus, and need to standardize our variables first
#We also need to add the new neighborhood variable
#We also need to add pre-natal variables from the medical FFCW datafiles
#We need to transform the data to Mplus format


############################# 1) Load Packages #############################
install.packages("haven")
install.packages("psych")
install.packages("ltm")
install.packages("openxlsx")
install.packages("apaTables")
install.packages("dplyr")
install.packages("lfe")
install.packages("plm")
install.packages("tidyverse")


library(plm)
library(openxlsx)
library(haven)
library(psych)
library(ltm)
library(apaTables)
library(dplyr)
library(lfe)
library(readr)

### Load in the data from pre-processing
Data <- read.csv("/Volumes/MPRG-Biosocial/Projects/03_data/003_FFCW/July 2023 Processed Data/DataSEM_18072023.csv")
colnames(Data)

############################# 2) Standardize MH and DNAm to age 9 #############################

### create Mental Health (MH) and DNAm variables standardized for age 9 ####

#### create standardized mental health variables ####
#Standardize mental health age 15 to mental health age 9
#this to make the interpretation of the model easier

#see here how to standardize variables https://www.statisticshowto.com/standardized-variables/

options(digits = 5)
as.data.frame(describe(Data$Int9_log)) #mean=0.76581, sd=0.074402
as.data.frame(describe(Data$Ext9_log)) #mean=0.77465, sd=0.083087

#describe(Data$Int15_log) 
#describe(Data$Ext15_log)

Data$Int15_log_Std9 <- (Data$Int15_log-0.76581)/0.074402
describe(Data$Int15_log_Std9)
Data$Int9_log_Std9 <- (Data$Int9_log-0.76581)/0.074402
describe(Data$Int9_log_Std9)

#double check if scaling worked proparly 
#Data$Int9_log_scaled <- (Data$Int9_log-0.76581)/0.074402
#describe(Data$Int9_log_scaled)

Data$Ext15_log_Std9 <- (Data$Ext15_log-0.77465)/0.083087
describe(Data$Ext15_log_Std9)
Data$Ext9_log_Std9 <- (Data$Ext9_log-0.77465)/0.083087
describe(Data$Ext9_log_Std9)

#double check if scaling worked proparly 
#Data$Ext9_log_scaled <- (Data$Ext9_log-0.77465)/0.083087
#describe(Data$Ext9_log_scaled)

#check descriptives MH
#both internalizing and externalizing problems go up from age 9 to age 15
describe(Data$Int9_log)
describe(Data$Int15_log)
describe(Data$Int9_log_Std)
describe(Data$Int15_log_Std9)

describe(Data$Ext9_log)
describe(Data$Ext15_log)
describe(Data$Ext9_log_Std)
describe(Data$Ext15_log_Std9)


#### create standardized DNAm variables ####
#Standardize DNAm age 15 to DNAm age 9
#this to make the interpretation of the model easier

#see here how to standardize variables https://www.statisticshowto.com/standardized-variables/
options(digits = 7)

as.data.frame(describe(Data$DunedinPACE_9_res_array)) #mean=-2.929051e-18, sd=0.09835249
as.data.frame(describe(Data$PCGrim_9_accell_res_array)) #mean=1.230201e-16, sd=1.726853
as.data.frame(describe(Data$PCPheno_9_accell_res_array)) #mean=1.365388e-16, sd=3.50461

Data$DunedinPACE_15_res_array_Std9 <- (Data$DunedinPACE_15_res_array-2.929051e-18)/0.09835249
describe(Data$DunedinPACE_15_res_array_Std9)

Data$DunedinPACE_9_res_array_Std9 <- (Data$DunedinPACE_9_res_array-2.929051e-18)/0.09835249
describe(Data$DunedinPACE_9_res_array_Std9)

as.data.frame(describe(Data$DunedinPACE_15_res_array_Std9))
as.data.frame(describe(Data$DunedinPACE_9_res_array_Std9))

Data$PCGrim_15_accell_res_array_Std9 <- (Data$PCGrim_15_accell_res_array - 1.230201e-16) / 1.726853
Data$PCGrim_9_accell_res_array_Std9 <- (Data$PCGrim_9_accell_res_array - 1.230201e-16) / 1.726853

Data$PCPheno_15_accell_res_array_Std9 <- (Data$PCPheno_15_accell_res_array - 1.365388e-16) / 3.50461
Data$PCPheno_9_accell_res_array_Std9 <- (Data$PCPheno_9_accell_res_array - 1.365388e-16) / 3.50461


#test scaling
#Data$DunedinPACE_9_res_array_std9 <- (Data$DunedinPACE_9_res_array-2.929051e-18)/0.09835249
#describe(Data$DunedinPACE_9_res_array_std9)

#Data$DunedinPACE_9_res_array_std9_test <- (Data$DunedinPACE_9_res_array-(-2.929051e-18)/0.09835249)
#describe(Data$DunedinPACE_9_res_array_std9_test)                                         

#Data$DunedinPACE_9_res_array_scaled9 <- scale(Data$DunedinPACE_9_res_array)
#describe(Data$DunedinPACE_9_res_array_scaled9)

#Data$DunedinPACE_9_res_array_test2 <- Data$DunedinPACE_9_res_array - DP9_mean/DP9_sd
#describe(Data$DunedinPACE_9_res_array_test2)

# Calculate the mean and standard deviation of 
#DP9_mean <- mean(Data$DunedinPACE_9_res_array,na.rm = TRUE)
#DP9_sd <- sd(Data$DunedinPACE_9_res_array, na.rm = TRUE)
#Data$DunedinPACE_15_res_array_std9 <- (Data$DunedinPACE_15_res_array - DP9_mean)/DP9_sd
#describe(Data$DunedinPACE_15_res_array_std9)

#check if all went well
#describe(Data$DunedinPACE_9_res_array_std9)
#describe(Data$DunedinPACE_9_res_array_std9_test)
#describe(Data$DunedinPACE_9_res_array_scaled9)

#as.data.frame(describe(Data$DunedinPACE_9_res_array_std9))
#as.data.frame(describe(Data$DunedinPACE_9_res_array_std9_test))
#as.data.frame(describe(Data$DunedinPACE_9_res_array_scaled9))
#as.data.frame(describe(Data$DunedinPACE_9_res_array_std))
#as.data.frame(describe(Data$DunedinPACE_9_res_array_test2))

############################# 3) Load in new neighborhood variable #############################

DataNeigh <- read.csv("/Volumes/MPRG-Biosocial/Projects/03_data/003_FFCW/July 2023 Processed Data/DataNeigh_Updated_25092023.csv")
colnames(DataNeigh)

NeighNew <- DataNeigh[,c("idnum",
                         "Ngh_overall_NEW")]
describe(NeighNew)

############################# 4) Load in Prenatal variables #############################

HealthData <- read_dta("/Volumes/MPRG-Biosocial/Projects/03_data/003_FFCW/Medical Health Data/FFmedrecs_pub.dta")
colnames(HealthData)

#create subset with necessary variables
#chbthwtg Constructed: baby's birthweight (gm)
#chgstage Constructed: gestational age
#chdrugp Constructed: mother used drugs during pregnancy
#chsmkp Constructed: mother smoked cigarettes during pregnancy
#chalcp Constructed: mother used alcohol during pregnancy

Prenatal <- HealthData[, c("idnum",
                           "chbthwtg",
                           "chgstage",
                           "chdrugp",
                           "chsmkp",
                           "chalcp")]
describe(Prenatal)

hist(Prenatal$chbthwtg)
hist(Prenatal$chgstage)
hist(Prenatal$chdrugp) #binary variable
hist(Prenatal$chsmkp) #binary variable
hist(Prenatal$chalcp) #binary variable

describe(Prenatal$chbthwtg) #N=3651 Mean=3217.78, SD=621.99, min=470 max=5584.95
describe(Prenatal$chgstage) #N=3677 Mean=38.54, SD=2.45, min=23, max=46
table(Prenatal$chdrugp) #N=3307 no drugs during pregnancy, N=377 drugs during pregnancy
table(Prenatal$chsmkp) #N= 2917 no smoking during pregnancy, n=767 smoking during pregnancy
table(Prenatal$chalcp) #n=3402 no alcohol during pregnancy, n=282 alchol during pregnancy

#rename variable names so they make sense
colnames(Prenatal)[colnames(Prenatal) == "chbthwtg"] <- "Birthweight"
colnames(Prenatal)[colnames(Prenatal) == "chgstage"] <- "Gestage"
colnames(Prenatal)[colnames(Prenatal) == "chdrugp"] <- "Drugspreg"
colnames(Prenatal)[colnames(Prenatal) == "chsmkp"] <- "Smokepreg"
colnames(Prenatal)[colnames(Prenatal) == "chalcp"] <- "Alchlpreg"

# Remove variable labels for all variables in the dataframe
#otherwise the creation of Mplus file does not work
for (var in names(Prenatal)) {
  attr(Prenatal[[var]], "label") <- NULL
}

#set idnum as integer to merge
Prenatal$idnum <- as.integer(Prenatal$idnum)

#set prenatal variables as factor
Prenatal$Drugspreg <- as.factor(Prenatal$Drugspreg)
Prenatal$Smokepreg <- as.factor(Prenatal$Smokepreg)
Prenatal$Alchlpreg <- as.factor(Prenatal$Alchlpreg)

colnames(Prenatal)


############################# 5) Merge all datafiles #############################

Data1 <- left_join(Data, NeighNew, by = "idnum")
Data2 <- left_join(Data1, Prenatal, by = "idnum")

colnames(Data2)

#safe datafile in folder with all variables
write.csv(Data2, file = "/Volumes/MPRG-Biosocial/Projects/03_data/003_FFCW/July 2023 Processed Data/DataSEM_12102023.csv", row.names = FALSE)

############################# 6) Create correlation table #############################

DataCor <- read.csv("/Volumes/MPRG-Biosocial/Projects/03_data/003_FFCW/July 2023 Processed Data/DataSEM_12102023.csv")
colnames(DataCor)

Cortable <- DataCor[,c("Int3_log",
                       "Ext3_log",
                       "Int5_log",
                       "Ext5_log",
                       "Int9_log",
                       "Ext9_log",
                       "Int15_log",
                       "Ext15_log",
                       "cesd_log",
                       "scale_anxt_log",
                       "h6a8",
                       "ZEduInc",
                       "rgm5opin_ccvcs_race_theil_2000",
                       "Ngh_overall_NEW",
                       "parent_stress_acrossages",
                       "Close15",
                       "k6e10",
                       "k6e16",
                       "DunedinPACE_9_res_array",
                       "PCGrim_9_accell_res_array",
                       "PCPheno_9_accell_res_array",
                       "DunedinPACE_15_res_array",
                       "PCGrim_15_accell_res_array",
                       "PCPheno_15_accell_res_array",
                       "eversmoke",
                       "ck6bmip",
                       "Puberty",
                       "cm1bsex",
                       "Birthweight",
                       "Gestage",
                       "Smokepreg",
                       "Alchlpreg",
                       "Drugspreg")]


# Install and load the `psych` package
install.packages("psych")
library(psych)
install.packages("apaTables")
library(apaTables)


# calculate correlation matrix
# calculate significance levels
cormatrix <- corr.test(Cortable, use = "pairwise.complete.obs", adjust = "none", ci = TRUE)
print(cormatrix, short=FALSE)
lowertable <- lowerCor(Cortable,use = "pairwise.complete.obs")
print(lowertable)

#safe as apa.cor table from apaTables
tablecor <- apa.cor.table(Cortable, "/Users/willems/Desktop/FFCW_Dec_2023/Analyses/Cortable.xlsx")  


                       
                       
                       
                       
                       
                 


############################# 7) Prepare data for Mplus #############################
colnames(Data2)

forMplus_LCM <- Data2[,c("idnum",
                          "cm1bsex",
                          "ch5bmip",
                          "ck6bmip",
                          "eversmoke",
                          "Puberty",
                          "ck6ethrace",
                          "race_white",
                          "race_black",
                          "race_hispanic",
                          "race_other",
                          "race_multi",
                          "ZEduInc",
                          "DunedinPACE_9_res_array",
                          "PCGrim_9_accell_res_array",
                          "PCPheno_9_accell_res_array",
                          "DunedinPACE_15_res_array",
                          "PCGrim_15_accell_res_array",
                          "PCPheno_15_accell_res_array",
                          "DunedinPACE_9_res_array_Std9",
                          "DunedinPACE_15_res_array_Std9",
                          "PCGrim_9_accell_res_array_Std9",
                          "PCGrim_15_accell_res_array_Std9",
                          "PCPheno_9_accell_res_array_Std9",
                          "PCPheno_15_accell_res_array_Std9",
                          "scale_cesd" ,
                          "cesd_log",
                          "scale_anxt",
                          "scale_anxt_log",
                          "Int9_log",
                          "Ext9_log",
                          "Int15_log",
                          "Ext15_log",
                          "Int9_log_Std9",
                          "Int15_log_Std9",
                          "Ext9_log_Std9",
                          "Ext15_log_Std9",
                         "Ngh_overall_NEW",
                         "Birthweight",
                         "Gestage",
                         "Drugspreg",
                         "Alchlpreg",
                         "Smokepreg")]

forMplus_LCM$ck6ethrace <- as.factor(forMplus_LCM$ck6ethrace)


install.packages("MplusAutomation")                               
library(MplusAutomation) 


# Replace all NA values with a period (.)
#forMplus_LCM[is.na(forMplus_LCM )] <- "."

# Print the modified dataframe
#print(forMplus_LCM)

setwd("/Users/willems/Desktop/Mplus FFCW")
MplusAutomation::prepareMplusData(forMplus_LCM, filename = "forMplusLCM_updated.dat", inpfile = T)


Data2 <- read.csv("/Volumes/MPRG-Biosocial/Projects/03_data/003_FFCW/July 2023 Processed Data/DataSEM_12102023.csv")

#### Prepare Data for Mplus
colnames(Data2)

forMplus_LCM <- Data2[,c("idnum",
                         "cm1bsex",
                         "ch5bmip",
                         "ck6bmip",
                         "eversmoke",
                         "Puberty",
                         "ck6ethrace",
                         "race_white",
                         "race_black",
                         "race_hispanic",
                         "race_other",
                         "race_multi",
                         "ZEduInc",
                         "DunedinPACE_9_res_array",
                         "PCGrim_9_accell_res_array",
                         "PCPheno_9_accell_res_array",
                         "DunedinPACE_15_res_array",
                         "PCGrim_15_accell_res_array",
                         "PCPheno_15_accell_res_array",
                         "DunedinPACE_9_res_array_Std9",
                         "DunedinPACE_15_res_array_Std9",
                         "PCGrim_9_accell_res_array_Std9",
                         "PCGrim_15_accell_res_array_Std9",
                         "PCPheno_9_accell_res_array_Std9",
                         "PCPheno_15_accell_res_array_Std9",
                         "scale_cesd" ,
                         "cesd_log",
                         "scale_anxt",
                         "scale_anxt_log",
                         "Int9_log",
                         "Ext9_log",
                         "Int15_log",
                         "Ext15_log",
                         "Int9_log_Std9",
                         "Int15_log_Std9",
                         "Ext9_log_Std9",
                         "Ext15_log_Std9",
                         "Ngh_overall_NEW",
                         "Birthweight",
                         "Gestage",
                         "Drugspreg",
                         "Alchlpreg",
                         "Smokepreg")]

forMplus_LCM$ck6ethrace <- as.factor(forMplus_LCM$ck6ethrace)


install.packages("MplusAutomation")                               
library(MplusAutomation) 


# Replace all NA values with a period (.)
#forMplus_LCM[is.na(forMplus_LCM )] <- "."

# Print the modified dataframe
#print(forMplus_LCM)

setwd("/Users/willems/Desktop/Mplus FFCW")
MplusAutomation::prepareMplusData(forMplus_LCM, filename = "forMplusLCM_updated.dat", inpfile = T)


# Create a new dataset with no missing values in either of the specified columns
forMplus_LCM_DNAm <- forMplus_LCM[complete.cases(forMplus_LCM$PCGrim_9_accell_res_array_Std9) |
                                    complete.cases(forMplus_LCM$PCGrim_15_accell_res_array_Std9), ]

setwd("/Users/willems/Desktop/Mplus FFCW")
MplusAutomation::prepareMplusData(forMplus_LCM_DNAm, filename = "forMplusLCM_updated_onlyDNAm.dat", inpfile = T)


############################# 7) Prepare data for Mplus including Theil index #############################

#Muna preprocessed variables on race, parenting and policing
#racial context/ skin colour etc needs to be added to overall data file
#upload this file and merge it with updated FFCW data file

#updated FFCW file
Data2 <- read.csv("/Volumes/MPRG-Biosocial/Projects/03_data/003_FFCW/July 2023 Processed Data/DataSEM_12102023.csv")
colnames(Data2)

#Data preprocessed by Muna
DataMuna <- read.csv("/Users/willems/Desktop/Projects/FFCW/analyses FFCW/DataSEM_muna.csv")
colnames(DataMuna)

#Select variables to add to overall datafile
varMuna <- DataMuna[,c("idnum",
                       "racial_contexts",         #Theil index
                       "racial_contexts_std")]    #Theil index standardized
                                         

Data3 <- left_join(Data2, varMuna , by = "idnum")                    
colnames(Data3)

forMplus_01122023 <- Data3[,c("idnum",
                            "cm1bsex",
                            "ch5bmip",
                            "ck6bmip",
                            "eversmoke",
                            "Puberty",
                            "ck6ethrace",
                            "race_white",
                            "race_black",
                            "race_hispanic",
                            "race_other",
                            "race_multi",
                            "ZEduInc",
                            "DunedinPACE_9_res_array",
                            "PCGrim_9_accell_res_array",
                            "PCPheno_9_accell_res_array",
                            "DunedinPACE_15_res_array",
                            "PCGrim_15_accell_res_array",
                            "PCPheno_15_accell_res_array",
                            "DunedinPACE_9_res_array_Std9",
                            "DunedinPACE_15_res_array_Std9",
                            "PCGrim_9_accell_res_array_Std9",
                            "PCGrim_15_accell_res_array_Std9",
                            "PCPheno_9_accell_res_array_Std9",
                            "PCPheno_15_accell_res_array_Std9",
                            "scale_cesd" ,
                            "cesd_log",
                            "scale_anxt",
                            "scale_anxt_log",
                            "Int9_log",
                            "Ext9_log",
                            "Int15_log",
                            "Ext15_log",
                            "Int9_log_Std9",
                            "Int15_log_Std9",
                            "Ext9_log_Std9",
                            "Ext15_log_Std9",
                            "Ngh_overall_NEW",
                            "Birthweight",
                            "Gestage",
                            "Drugspreg",
                            "Alchlpreg",
                            "Smokepreg",
                            "k6e10",
                            "Close9",
                            "Close15",
                            "Close_acrossages",
                            "parent_stress_age9",
                            "parent_stress_age15",
                            "parent_stress_acrossages", 
                            "rgm1opin_ccvcs_race_theil_2000",
                            "rgm5opin_ccvcs_race_theil_2000",
                            "racial_contexts",
                            "racial_contexts_std",
                            "h6a8")]

setwd("/Users/willems/Desktop/Mplus FFCW")
MplusAutomation::prepareMplusData(forMplus_01122023, filename = "forMplus_01122023.dat", inpfile = T)                       

write.csv(Data3, file = "/Volumes/MPRG-Biosocial/Projects/03_data/003_FFCW/July 2023 Processed Data/forMplus_full_01122023.csv", row.names = FALSE)


#Create datafile only including those with DNAm data available
FFCW_OnlyDNAm <- forMplus_01122023[!is.na(forMplus_01122023$DunedinPACE_9_res_array) | !is.na(forMplus_01122023$DunedinPACE_15_res_array), ]

setwd("/Users/willems/Desktop/Mplus FFCW")
MplusAutomation::prepareMplusData(FFCW_OnlyDNAm, filename = "forMplus_DNAmOnly_01122023.dat", inpfile = T)  

write.csv(FFCW_OnlyDNAm, file = "/Volumes/MPRG-Biosocial/Projects/03_data/003_FFCW/July 2023 Processed Data/forMplus_DNAmOnly_01122023.csv", row.names = FALSE)


############################# 8) Check descriptives / correlations DNAm #############################

Data_Full <- read.csv("/Volumes/MPRG-Biosocial/Projects/03_data/003_FFCW/July 2023 Processed Data/forMplus_full_01122023.csv")

Data_DNAm <- read.csv("/Volumes/MPRG-Biosocial/Projects/03_data/003_FFCW/July 2023 Processed Data/forMplus_DNAmOnly_01122023.csv")


colnames(Data_Full)
colnames(Data_DNAm)

describe(Data_DNAm)

table(Data_DNAm$ck6ethrace)

############################# 9) Add indirect police interactions #############################

#add indirect police interactions

forMplus_20240116_IndPolice <- Data3[,c("idnum",
                              "cm1bsex",
                              "ch5bmip",
                              "ck6bmip",
                              "eversmoke",
                              "Puberty",
                              "ck6ethrace",
                              "race_white",
                              "race_black",
                              "race_hispanic",
                              "race_other",
                              "race_multi",
                              "ZEduInc",
                              "DunedinPACE_9_res_array",
                              "PCGrim_9_accell_res_array",
                              "PCPheno_9_accell_res_array",
                              "DunedinPACE_15_res_array",
                              "PCGrim_15_accell_res_array",
                              "PCPheno_15_accell_res_array",
                              "DunedinPACE_9_res_array_Std9",
                              "DunedinPACE_15_res_array_Std9",
                              "PCGrim_9_accell_res_array_Std9",
                              "PCGrim_15_accell_res_array_Std9",
                              "PCPheno_9_accell_res_array_Std9",
                              "PCPheno_15_accell_res_array_Std9",
                              "scale_cesd" ,
                              "cesd_log",
                              "scale_anxt",
                              "scale_anxt_log",
                              "Int9_log",
                              "Ext9_log",
                              "Int15_log",
                              "Ext15_log",
                              "Int9_log_Std9",
                              "Int15_log_Std9",
                              "Ext9_log_Std9",
                              "Ext15_log_Std9",
                              "Ngh_overall_NEW",
                              "Birthweight",
                              "Gestage",
                              "Drugspreg",
                              "Alchlpreg",
                              "Smokepreg",
                              "k6e10",
                              "k6e16",
                              "Close9",
                              "Close15",
                              "Close_acrossages",
                              "parent_stress_age9",
                              "parent_stress_age15",
                              "parent_stress_acrossages", 
                              "rgm1opin_ccvcs_race_theil_2000",
                              "rgm5opin_ccvcs_race_theil_2000",
                              "racial_contexts",
                              "racial_contexts_std",
                              "h6a8")]

#Create datafile only including those with DNAm data available
forMplus_20240116_IndPolice_DNAm <- forMplus_20240116_IndPolice[!is.na(forMplus_20240116_IndPolice$DunedinPACE_9_res_array) | !is.na(forMplus_20240116_IndPolice$DunedinPACE_15_res_array), ]


setwd("/Users/willems/Desktop/FFCW_Dec_2023/Analyses")
MplusAutomation::prepareMplusData(forMplus_20240116_IndPolice_DNAm, filename = "forMplus_20240116_IndPolice_DNAm.dat", inpfile = T)    
