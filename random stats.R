


library(dplyr)
library(tidyr)
library(tibble)
library(readxl)
library(writexl)
library(stringr)

# Importing as text seems to keep things from getting messed up.
data_all <- read_excel(here::here("FRIEND_dataset_with_City_12_28_20.xlsx"), col_types = "text")

# Makes new data frame for analysis.
data <- data_all

###############################################################################
# Little Clean up.
###############################################################################

# Next steps are for converting to numeric columns.
#First convert the n/a and na to actually missing.
cols_num <- c("ANYCVD", "COPD", "CABG", "MI", "PCI", "HeartFailure", "COPD",
              "ageattest", "height", "weight", "vo2_ml_kg_min", "vo2_l_min", 
              "max_hr", "max_rer")
data <- data %>% mutate_at(.vars = cols_num,
                           .funs = list(~replace(.,.=="n/a"|.=="na",NA)))
make_num <- c("ageattest", "height", "weight", "vo2_ml_kg_min", "vo2_l_min",
              "max_rer", "max_hr")
data$max_hr[data$max_hr == "??"] <- NA
data[make_num] <- sapply(data[make_num], as.numeric)

# Create age groups (ignores those <20 and >90 years old).
data$age_group <- ifelse(data$ageattest>=20 & data$ageattest<30, "20s",
                         ifelse(data$ageattest>=30 & data$ageattest<40, "30s",
                                ifelse(data$ageattest>=40 & data$ageattest<50, "40s",
                                       ifelse(data$ageattest>=50 & data$ageattest<60, "50s",
                                              ifelse(data$ageattest>=60 & data$ageattest<70, "60s",
                                                     ifelse(data$ageattest>=70 & data$ageattest<80, "70s",
                                                            ifelse(data$ageattest>=80 & data$ageattest<90, "80s",NA)))))))

#Creates columns for height and weight in SI units.
data$height_SI <- round(data$height*2.54,1)
data$weight_SI <- round(data$weight/2.2,1)

# If missing abs VO2, then calculate it.
data$vo2_l_min[is.na(data$vo2_l_min)] <- round((data$vo2_ml_kg_min[is.na(data$vo2_l_min)]/
                                                  1000*data$weight_SI[is.na(data$vo2_l_min)]), 1)


###############################################################################
# Dropping tests.
###############################################################################

# Drops those with "ANYCVD" missing.
data <- filter(data, !is.na(ANYCVD))

# Includes only those 40-79 years old.
data <- filter(data, ageattest>=30 & ageattest<90)

# Drops those not coded as M/F (so missing gender coding).
table(data$Gender)
data <- filter(data, Gender=="Male" | Gender=="Female")

# Drops those with 0 for height, weight, age, or VO2.
data <- filter(data, !(height==0))
data <- filter(data, !(weight==0))
data <- filter(data, !(ageattest==0))
data <- filter(data, !(vo2_ml_kg_min==0))

# Only looks at TM and CY data.
data <- filter(data, Mode=="TM" | Mode=="CY")

# Drops those with RER<1.0.
data <- filter(data, max_rer>=1.0)

# Only looks at USA and Canada..
data <- filter(data, Country=="USA" | Country=="CAN")


# Drops females
#data <- filter(data, Gender == "Male")

# Check outputs to make sure there aren't mistakes.
table(data$Country)
table(data$Mode)
table(data$Gender)

######## CABG data.
data_CABG <- filter(data, CABG==1)
#data_CABG <- filter(data_CABG, Gender == "Male")
data_CABG <- filter(data_CABG, !(age_group == "30s"))
# Drops those with PCI, MI (n=857) or heart failure (n=159) (keeps if values are missing).
data_CABG <- data_CABG[!(data_CABG$HeartFailure %in% 1),]
data_CABG <- data_CABG[!(data_CABG$MI %in% 1),]
data_CABG <- data_CABG[!(data_CABG$PCI %in% 1),]

######## MI data.
data_MI <- filter(data, MI==1)
#data_MI <- filter(data_MI, Gender == "Male")
data_MI <- filter(data_MI, !(age_group == "30s"))
# Drops those from Norway (since all the others are USA and Canada (and there's not a lot from Norway))
#data_MI <- filter(data_MI, !(Country=="NOR"))
# Drops those with heart failure (n=265) (keeps folks if heart failure values are missing).
data_MI <- data_MI[!(data_MI$HeartFailure %in% 1),]

########## PCI data.
data_PCI <- filter(data, PCI==1)
#data_PCI <- filter(data_PCI, Gender == "Male")
data_PCI <- filter(data_PCI, !(age_group == "30s"))

#Drops those with MI (n=2602), heart failure (n=144), CABG (n=342)(keeps if values are missing).
data_PCI <- data_PCI[!(data_PCI$MI %in% 1),]
data_PCI <- data_PCI[!(data_PCI$HeartFailure %in% 1),]
data_PCI <- data_PCI[!(data_PCI$CABG %in% 1),]

########## Heart Failure.
data_HF <- filter(data, HeartFailure==1)
#data_HF <- filter(data_HF, Gender == "Male")
data_HF <- filter(data_HF, !(age_group == "30s"))

########## ALL CVD.
dataCVD <- filter(data, HeartFailure==1 | PCI==1 | MI==1 | CABG==1)

###############################################################################
# Stats Stuff.
###############################################################################

# Two-way ANOVA for sex and age group differences in VO2_rel.
# The ":" allows for the interaction between variables to be examined.
# CVD CYCLING
CVDaovCY <- aov(vo2_l_min ~ Gender + age_group + Gender:age_group, 
                data = filter(dataCVD, Mode == "CY"))
summary(CVDaovCY)

# ANOVA shows gender and age_group are significant. 
# Only do Tukey for age_group since there are only 2 levels for gender.
TukeyHSD(CVDaovCY, which = "age_group")
# Some t-tests to find where the differences in age are for males/females.
CVDdfM <- filter(dataCVD, Mode == "CY", Gender == "Female")
t.test(CVDdfM$vo2_l_min[CVDdfM$age_group == "30s"], CVDdfM$vo2_l_min[CVDdfM$age_group == "60s"])

# Means for the sex groups.
tapply(dataCVD$vo2_ml_kg_min[dataCVD$Mode == "CY"], dataCVD$Gender[dataCVD$Mode == "CY"], mean)
# Percent difference ("men had x% greater CRF than women")
round(((18.2-14.5)/14.5)*100, 0)
# Actual difference
18.2-14.5

# CVD TREADMILL
CVDaovTM <- aov(vo2_ml_kg_min ~ Gender + age_group + Gender:age_group, 
                data = filter(dataCVD, Mode == "TM"))
summary(CVDaovTM)
# ANOVA shows gender and age_group are significant. 
# Only do Tukey for age_group since there are only 2 levels for gender.
TukeyHSD(CVDaovTM, which = "age_group")
# Means for the sex groups.
tapply(dataCVD$vo2_ml_kg_min[dataCVD$Mode == "TM"], dataCVD$Gender[dataCVD$Mode == "TM"], mean)
# Percent difference ("men had x% greater CRF than women")
round(((22.1-18.0)/18.0)*100, 0)
# Actual difference
22.1-18.0

# One-way ANOVAs for age group differences in the specific CVD categories.
data_CABG <- filter(data_CABG, Gender == "Female")
data_MI <- filter(data_MI, Gender == "Female")
data_PCI <- filter(data_PCI, Gender == "Female")
data_HF <- filter(data_HF, Gender == "Female")

# CABG Cycling.
CABGaovCY <- aov(vo2_ml_kg_min ~ age_group, data = filter(data_CABG, Mode == "CY"))
summary(CABGaovCY)

# CABG Treadmill.
CABGaovTM <- aov(vo2_ml_kg_min ~ age_group, data = filter(data_CABG, Mode == "TM"))
summary(CABGaovTM)

# MI Cycling.
MIaovCY <- aov(vo2_ml_kg_min ~ age_group, data = filter(data_MI, Mode == "CY"))
summary(MIaovCY)

# MI Treadmill.
MIaovTM <- aov(vo2_ml_kg_min ~ age_group, data = filter(data_MI, Mode == "TM"))
summary(MIaovTM)

# PCI Cycling.
PCIaovCY <- aov(vo2_ml_kg_min ~ age_group, data = filter(data_PCI, Mode == "CY"))
summary(PCIaovCY)

# PCI Treadmill.
PCIaovTM <- aov(vo2_ml_kg_min ~ age_group, data = filter(data_PCI, Mode == "TM"))
summary(PCIaovTM)

# Heart failure Cycling.
HFaovCY <- aov(vo2_ml_kg_min ~ age_group, data = filter(data_HF, Mode == "CY"))
summary(HFaovCY)

# Heart failure Treadmill.
HFaovTM <- aov(vo2_ml_kg_min ~ age_group, data = filter(data_HF, Mode == "TM"))
summary(HFaovTM)



###############################################################################
# Save the output.
###############################################################################
x <- list("CABG" = t.test_summary_CABG, "MI" = t.test_summary_MI, "PCI" = t.test_summary_PCI, 
          "HF" = t.test_summary_HF)
write_xlsx(x, path = here::here("Frandom stats results.xlsx"), col_names = T)


###############################################################################
# Random playing around.
###############################################################################
# Independent t-tests comparing RER groups (>=1.0 and >=1.1)
# Probably not technically right test but we're going for it anyway.

age_group_vec <- c("40s","50s","60s","70s")

# CABG
t.test_summary_CABG <- data.frame(age_group_vec)

for(i in 1:length(age_group_vec)){
  temp_df <- filter(data_CABG, age_group == age_group_vec[i])
  
  # Treadmill t-tests.
  temp_df_TM <- filter(temp_df, Mode == "TM")
  # Males.
  TM_RER1_0 <- temp_df_TM$vo2_ml_kg_min[temp_df_TM$max_rer >= 1.0]
  TM_RER1_1 <- temp_df_TM$vo2_ml_kg_min[temp_df_TM$max_rer >= 1.1]
  t.test_summary_CABG[i,"Male TM p.value"] <- round(t.test(TM_RER1_0, TM_RER1_1, paired = F)$p.value, 4)
  t.test_summary_CABG[i,"Male TM rer>1.0"] <- length(TM_RER1_0)
  t.test_summary_CABG[i,"Male TM rer>1.1"] <- length(TM_RER1_1)
  
  
  # Cycling t-tests.
  temp_df_CY <- filter(temp_df, Mode == "CY")
  # Male.
  CY_RER1_0 <- temp_df_CY$vo2_ml_kg_min[temp_df_CY$max_rer >= 1.0]
  CY_RER1_1 <- temp_df_CY$vo2_ml_kg_min[temp_df_CY$max_rer >= 1.1]
  t.test_summary_CABG[i,"Male CY p.value"] <- round(t.test(CY_RER1_0, CY_RER1_1, paired = F)$p.value, 4)
  t.test_summary_CABG[i,"Male CY rer>1.0"] <- length(CY_RER1_0)
  t.test_summary_CABG[i,"Male CY rer>1.1"] <- length(CY_RER1_1)
  
  rm(temp_df, temp_df_TM, temp_df_CY, TM_RER1_0, TM_RER1_1, 
     CY_RER1_0, CY_RER1_1)
}

# MI.
t.test_summary_MI <- data.frame(age_group_vec)

for(i in 1:length(age_group_vec)){
  temp_df <- filter(data_MI, age_group == age_group_vec[i])
  
  # Treadmill t-tests.
  temp_df_TM <- filter(temp_df, Mode == "TM")
  # Males.
  TM_RER1_0 <- temp_df_TM$vo2_ml_kg_min[temp_df_TM$max_rer >= 1.0]
  TM_RER1_1 <- temp_df_TM$vo2_ml_kg_min[temp_df_TM$max_rer >= 1.1]
  t.test_summary_MI[i,"Male TM p.value"] <- round(t.test(TM_RER1_0, TM_RER1_1, paired = F)$p.value, 4)
  t.test_summary_MI[i,"Male TM rer>1.0"] <- length(TM_RER1_0)
  t.test_summary_MI[i,"Male TM rer>1.1"] <- length(TM_RER1_1)
  
  
  # Cycling t-tests.
  temp_df_CY <- filter(temp_df, Mode == "CY")
  # Male.
  CY_RER1_0 <- temp_df_CY$vo2_ml_kg_min[temp_df_CY$max_rer >= 1.0]
  CY_RER1_1 <- temp_df_CY$vo2_ml_kg_min[temp_df_CY$max_rer >= 1.1]
  t.test_summary_MI[i,"Male CY p.value"] <- round(t.test(CY_RER1_0, CY_RER1_1, paired = F)$p.value, 4)
  t.test_summary_MI[i,"Male CY rer>1.0"] <- length(CY_RER1_0)
  t.test_summary_MI[i,"Male CY rer>1.1"] <- length(CY_RER1_1)
  
  rm(temp_df, temp_df_TM, temp_df_CY, TM_RER1_0, TM_RER1_1, 
     CY_RER1_0, CY_RER1_1)
}

# PCI.
t.test_summary_PCI <- data.frame(age_group_vec)

for(i in 1:length(age_group_vec)){
  temp_df <- filter(data_PCI, age_group == age_group_vec[i])
  
  # Treadmill t-tests.
  temp_df_TM <- filter(temp_df, Mode == "TM")
  # Males.
  TM_RER1_0 <- temp_df_TM$vo2_ml_kg_min[temp_df_TM$max_rer >= 1.0]
  TM_RER1_1 <- temp_df_TM$vo2_ml_kg_min[temp_df_TM$max_rer >= 1.1]
  t.test_summary_PCI[i,"Male TM p.value"] <- round(t.test(TM_RER1_0, TM_RER1_1, paired = F)$p.value, 4)
  t.test_summary_PCI[i,"Male TM rer>1.0"] <- length(TM_RER1_0)
  t.test_summary_PCI[i,"Male TM rer>1.1"] <- length(TM_RER1_1)
  
  
  # Cycling t-tests.
  temp_df_CY <- filter(temp_df, Mode == "CY")
  # Male.
  CY_RER1_0 <- temp_df_CY$vo2_ml_kg_min[temp_df_CY$max_rer >= 1.0]
  CY_RER1_1 <- temp_df_CY$vo2_ml_kg_min[temp_df_CY$max_rer >= 1.1]
  t.test_summary_PCI[i,"Male CY p.value"] <- round(t.test(CY_RER1_0, CY_RER1_1, paired = F)$p.value, 4)
  t.test_summary_PCI[i,"Male CY rer>1.0"] <- length(CY_RER1_0)
  t.test_summary_PCI[i,"Male CY rer>1.1"] <- length(CY_RER1_1)
  
  rm(temp_df, temp_df_TM, temp_df_CY, TM_RER1_0, TM_RER1_1, 
     CY_RER1_0, CY_RER1_1)
}

# HF.
t.test_summary_HF <- data.frame(age_group_vec)

for(i in 1:length(age_group_vec)){
  temp_df <- filter(data_HF, age_group == age_group_vec[i])
  
  # Treadmill t-tests.
  temp_df_TM <- filter(temp_df, Mode == "TM")
  # Males.
  TM_RER1_0 <- temp_df_TM$vo2_ml_kg_min[temp_df_TM$max_rer >= 1.0]
  TM_RER1_1 <- temp_df_TM$vo2_ml_kg_min[temp_df_TM$max_rer >= 1.1]
  t.test_summary_HF[i,"Male TM p.value"] <- round(t.test(TM_RER1_0, TM_RER1_1, paired = F)$p.value, 4)
  t.test_summary_HF[i,"Male TM rer>1.0"] <- length(TM_RER1_0)
  t.test_summary_HF[i,"Male TM rer>1.1"] <- length(TM_RER1_1)
  
  
  # Cycling t-tests.
  temp_df_CY <- filter(temp_df, Mode == "CY")
  # Male.
  CY_RER1_0 <- temp_df_CY$vo2_ml_kg_min[temp_df_CY$max_rer >= 1.0]
  CY_RER1_1 <- temp_df_CY$vo2_ml_kg_min[temp_df_CY$max_rer >= 1.1]
  t.test_summary_HF[i,"Male CY p.value"] <- round(t.test(CY_RER1_0, CY_RER1_1, paired = F)$p.value, 4)
  t.test_summary_HF[i,"Male CY rer>1.0"] <- length(CY_RER1_0)
  t.test_summary_HF[i,"Male CY rer>1.1"] <- length(CY_RER1_1)
  
  rm(temp_df, temp_df_TM, temp_df_CY, TM_RER1_0, TM_RER1_1, 
     CY_RER1_0, CY_RER1_1)
}

