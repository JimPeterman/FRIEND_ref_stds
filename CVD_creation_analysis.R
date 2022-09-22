

library(dplyr)
library(tidyr)
library(tibble)
library(readxl)
library(writexl)
library(FRIENDanalysis)

# Importing as text seems to keep things from getting messed up.
data_all <- read_excel(here::here("FRIEND_dataset_with_City_12_28_20.xlsx"), col_types = "text")

# This dataset still includes MET-test data.

data <- data_all

###############################################################################
# Little Clean up.
###############################################################################

# Next steps are for converting to numeric columns.
#First convert the n/a and na to actually missing.
cols_num <- c("ANYCVD", "COPD", "CABG", "MI", "PCI", "HeartFailure", "COPD",
              "ageattest", "height", "weight", "vo2_ml_kg_min", "vo2_l_min", 
              "max_hr", "max_rer", "max_load_watts", "BMI")
data <- data %>% mutate_at(.vars = cols_num,
                           .funs = list(~replace(.,.=="n/a"|.=="na"|
                                                   .=="Avg of 3 20 sec values",
                                                 NA)))
make_num <- c("ageattest", "height", "weight", "vo2_ml_kg_min", "vo2_l_min",
              "max_rer", "max_hr", "max_load_watts", "BMI")
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

# Includes only those 30-89 years old.
data <- filter(data, ageattest>=30 & ageattest<90)

# Drops those not coded as M/F (so missing gender coding).
table(data$Gender)
data <- filter(data, Gender=="Male" | Gender=="Female")

# Drops those with 0 for height, weight, age, or VO2.
data <- filter(data, !(height==0))
data <- filter(data, !(weight==0))
data <- filter(data, !(ageattest==0))
data <- filter(data, !(vo2_ml_kg_min==0))
data <- filter(data, !(max_hr==0))

# Only looks at TM and CY data.
data <- filter(data, Mode=="TM" | Mode=="CY")

# Drops those with RER<1.0.
data <- filter(data, max_rer>=1.0)

# Only looks at USA and Canada..
data <- filter(data, Country=="USA" | Country=="CAN")

# Check outputs to make sure there aren't mistakes.
table(data$Country)
table(data$Mode)
table(data$Gender)
table(data$Facility)

# Add in the FRIEND percentiles (based on healthy folks).
data <- mutate(data, CRFpercentile = FRIENDpercentile(vo2_ml_kg_min, ageattest, 
                                                      Gender, Mode))

###############################################################################
# Analysis.
###############################################################################


######## CABG data.
data_CABG <- filter(data, CABG==1)

# Drops those with PCI, MI (n=857) or heart failure (n=159) (keeps if values are missing).
data_CABG <- data_CABG[!(data_CABG$HeartFailure %in% 1),]
#data_CABG <- data_CABG[!(data_CABG$MI %in% 1),]
data_CABG <- data_CABG[!(data_CABG$PCI %in% 1),]

# Drops females (n=219/2102).
#data_CABG <- filter(data_CABG, Gender=="Male")
table(data_CABG$Facility)
table(data_CABG$Country)

# Summary table of mean rel VO2 by age and exercise mode.
mean_CABG <- data_CABG %>%
  select(Gender, vo2_ml_kg_min, CRFpercentile, vo2_l_min, max_load_watts, max_hr, max_rer,
         age_group, Mode, ageattest, height_SI, weight_SI, BMI) %>%
  group_by(Gender, Mode, age_group) %>%
  summarise_if(is.numeric, function(x) {paste(paste(sprintf("%.1f", round(mean(x, na.rm = T), digits = 1)),
                                                    sprintf("%.1f", round(sd(x, na.rm = T), digits = 1)), sep = " ± "))})

n_CABG <- data_CABG %>%
  select(Gender, age_group, Mode, vo2_ml_kg_min) %>%
  group_by(Gender, Mode, age_group) %>%
  summarise_if(is.numeric, function(x) {paste("(n=", length(which(!is.na(x))), ")", sep = "")})

mean_CABG$n <- n_CABG$vo2_ml_kg_min
mean_CABG <- mean_CABG %>% select("Gender", "Mode","age_group", "n", "vo2_ml_kg_min":"BMI")
mean_CABG <- data.frame(t(mean_CABG))
mean_CABG$Label <- c("Gender", "Mode", "Age Group", "n (CABG)", "VO2rel", "FRIENDperc",
                     "VO2abs", "Watts", "maxHR", "maxRER", 
                     "age", "height_SI", "weight_SI", "BMI")
mean_CABG <- mean_CABG %>% select("Label", "X1":(ncol(mean_CABG)-1))

######## MI data.
data_MI <- filter(data, MI==1)

# Drops those from Norway (since all the others are USA and Canada (and there's not a lot from Norway))
#data_MI <- filter(data_MI, !(Country=="NOR"))
# Drops those with heart failure (n=265) (keeps folks if heart failure values are missing).
data_MI <- data_MI[!(data_MI$HeartFailure %in% 1),]
# Savage had us drop those with CABG.
data_MI <- data_MI[!(data_MI$CABG %in% 1),]
# Drops females (n=602/4097).
#data_MI <- filter(data_MI, Gender=="Male")
table(data_MI$Facility)
table(data_MI$Country)

# Summary table of mean rel VO2 by age and exercise mode.
mean_MI <- data_MI %>%
  select(Gender, vo2_ml_kg_min, CRFpercentile, vo2_l_min, max_load_watts, max_hr, max_rer,
         age_group, Mode, ageattest, height_SI, weight_SI, BMI) %>%
  group_by(Gender, Mode, age_group) %>%
  summarise_if(is.numeric, function(x) {paste(paste(sprintf("%.1f", round(mean(x, na.rm = T), digits = 1)),
                                                    sprintf("%.1f", round(sd(x, na.rm = T), digits = 1)), sep = " ± "))})

n_MI <- data_MI %>%
  select(Gender, age_group, Mode, vo2_ml_kg_min) %>%
  group_by(Gender, Mode, age_group) %>%
  summarise_if(is.numeric, function(x) {paste("(n=", length(which(!is.na(x))), ")", sep = "")})

mean_MI$n <- n_MI$vo2_ml_kg_min
mean_MI <- mean_MI %>% select("Gender", "Mode","age_group", "n", "vo2_ml_kg_min":"BMI")
mean_MI <- data.frame(t(mean_MI))
mean_MI$Label <- c("Gender", "Mode", "Age Group", "n (MI)", "VO2rel", "FRIENDperc",
                   "VO2abs", "Watts", "maxHR", "maxRER", 
                   "age", "height_SI", "weight_SI", "BMI")
mean_MI <- mean_MI %>% select("Label", "X1":(ncol(mean_MI)-1))

########## PCI data.
data_PCI <- filter(data, PCI==1)

#Drops those with MI (n=2602), heart failure (n=144), CABG (n=342)(keeps if values are missing).
data_PCI <- data_PCI[!(data_PCI$MI %in% 1),]
data_PCI <- data_PCI[!(data_PCI$HeartFailure %in% 1),]
data_PCI <- data_PCI[!(data_PCI$CABG %in% 1),]
# Drops females (n=308/1747).
#data_PCI <- filter(data_PCI, Gender=="Male")
table(data_PCI$Facility)
table(data_PCI$Country)

# Summary table of mean rel VO2 by age and exercise mode.
mean_PCI <- data_PCI %>%
  select(Gender, vo2_ml_kg_min, CRFpercentile, vo2_l_min, max_load_watts, max_hr, max_rer,
         age_group, Mode, ageattest, height_SI, weight_SI, BMI) %>%
  group_by(Gender, Mode, age_group) %>%
  summarise_if(is.numeric, function(x) {paste(paste(sprintf("%.1f", round(mean(x, na.rm = T), digits = 1)),
                                                    sprintf("%.1f", round(sd(x, na.rm = T), digits = 1)), sep = " ± "))})

n_PCI <- data_PCI %>%
  select(Gender, age_group, Mode, vo2_ml_kg_min) %>%
  group_by(Gender, Mode, age_group) %>%
  summarise_if(is.numeric, function(x){paste("(n=", length(which(!is.na(x))), ")", sep = "")})

mean_PCI$n <- n_PCI$vo2_ml_kg_min
mean_PCI <- mean_PCI %>% select("Gender", "Mode","age_group", "n", "vo2_ml_kg_min":"BMI")
mean_PCI <- data.frame(t(mean_PCI))
mean_PCI$Label <- c("Gender", "Mode", "Age Group", "n (PCI)", "VO2rel", "FRIENDperc",
                    "VO2abs", "Watts", "maxHR", "maxRER", 
                    "age", "height_SI", "weight_SI", "BMI")
mean_PCI <- mean_PCI %>% select("Label", "X1":(ncol(mean_PCI)-1))

########## Heart Failure.
data_HF <- filter(data, HeartFailure==1)

# Drops females.
#data_HF <- filter(data_HF, Gender=="Male")

table(data_HF$Facility)
table(data_HF$Country)

mean_HF <- data_HF %>%
  select(Gender, vo2_ml_kg_min, CRFpercentile, vo2_l_min, max_load_watts, max_hr, max_rer,
         age_group, Mode, ageattest, height_SI, weight_SI, BMI) %>%
  group_by(Gender, Mode, age_group) %>%
  summarise_if(is.numeric, function(x) {paste(paste(sprintf("%.1f", round(mean(x, na.rm = T), digits = 1)),
                                                    sprintf("%.1f", round(sd(x, na.rm = T), digits = 1)), sep = " ± "))})

n_HF <- data_HF %>%
  select(Gender, age_group, Mode, vo2_ml_kg_min) %>%
  group_by(Gender, Mode, age_group) %>%
  summarise_if(is.numeric, function(x) {paste("(n=", length(which(!is.na(x))), ")", sep = "")})

mean_HF$n <- n_HF$vo2_ml_kg_min
mean_HF <- mean_HF %>% select("Gender", "Mode","age_group", "n", "vo2_ml_kg_min":"BMI")
mean_HF <- data.frame(t(mean_HF))
mean_HF$Label <- c("Gender", "Mode", "Age Group", "n (HF)", "VO2rel", "FRIENDperc",
                   "VO2abs", "Watts", "maxHR", "maxRER", 
                   "age", "height_SI", "weight_SI", "BMI")
mean_HF <- mean_HF %>% select("Label", "X1":(ncol(mean_HF)-1))

########## ALL CVD.
# Includes MEN and WOMEN.
dataCVD <- filter(data, HeartFailure==1 | PCI==1 | MI==1 | CABG==1)

mean_CVD <- dataCVD %>%
  select(Gender, Mode, vo2_ml_kg_min, CRFpercentile, 
         vo2_l_min, max_load_watts, max_hr, max_rer, age_group, 
         ageattest, height_SI, weight_SI, BMI) %>%
  group_by(Gender, Mode, age_group) %>%
  summarise_if(is.numeric, function(x) {paste(paste(sprintf("%.1f", round(mean(x, na.rm = T), digits = 1)),
                                                    sprintf("%.1f", round(sd(x, na.rm = T), digits = 1)), sep = " ± "))})

n_CVD <- dataCVD %>%
  select(Gender, Mode, vo2_ml_kg_min, age_group) %>%
  group_by(Gender, Mode, age_group) %>%
  summarise_if(is.numeric, function(x) {paste("(n=", length(which(!is.na(x))), ")", sep = "")})

mean_CVD$n <- n_CVD$vo2_ml_kg_min
mean_CVD <- mean_CVD %>% select("Gender", "Mode","age_group", "n", "vo2_ml_kg_min":"BMI")
mean_CVD <- data.frame(t(mean_CVD))
mean_CVD$Label <- c("Gender", "Mode", "Age Group", "n (CVD)", "VO2rel", "FRIENDperc",
                    "VO2abs", "Watts", "maxHR", "maxRER",
                    "age", "height_SI", "weight_SI", "BMI")
mean_CVD <- mean_CVD %>% select("Label", "X1":(ncol(mean_CVD)-1))

###############################################################################
# Percentiles by age and Gender.
###############################################################################

perc <- c(5, 10, 25, 50, 75, 
          90, 95)
perc_decimal <- perc/100
ages <- c("30s", "40s", "50s", "60s", "70s", "80s")

########## ALL CVD #######
# Males - Treadmill.
TM_perc_M <- data.frame(perc)
temp_df_M <- filter(dataCVD, Gender=="Male", Mode == "TM")

for(i in 1:length(ages)){
  # Age group.
  temp_df <- filter(temp_df_M, age_group == ages[i])
  TM_perc_M[,paste(ages[i], "M (TM)")] <- sprintf("%.1f", round(quantile(temp_df$vo2_ml_kg_min, perc_decimal),1))
  
  rm(temp_df)
}

# Females - Treadmill.
TM_perc_F <- data.frame(perc)
temp_df_F <- filter(dataCVD, Gender=="Female", Mode == "TM")

for(i in 1:length(ages)){
  # Age group.
  temp_df <- filter(temp_df_F, age_group == ages[i])
  TM_perc_F[,paste(ages[i], "F (TM)")] <- sprintf("%.1f", round(quantile(temp_df$vo2_ml_kg_min, perc_decimal),1))
  
  rm(temp_df)
}

# Combine the TM outputs.
TM_perc <- left_join(TM_perc_M, TM_perc_F, by = "perc")
rm(TM_perc_M, TM_perc_F, temp_df_M, temp_df_F)
#TM_perc <- data.frame(t(TM_perc))
#TM_perc <- mutate(TM_perc, Labels = row.names(TM_perc))
#TM_perc <- select(TM_perc, Labels, X1:X7)

# Males - Cycling.
CY_perc_M <- data.frame(perc)
temp_df_M <- filter(dataCVD, Gender=="Male", Mode == "CY")

for(i in 1:length(ages)){
  # Age group.
  temp_df <- filter(temp_df_M, age_group == ages[i])
  CY_perc_M[,paste(ages[i], "M (CY)")] <- sprintf("%.1f", round(quantile(temp_df$vo2_ml_kg_min, perc_decimal),1))
  
  rm(temp_df)
}

# Females - Cycling.
CY_perc_F <- data.frame(perc)
temp_df_F <- filter(dataCVD, Gender=="Female", Mode == "CY")

for(i in 1:length(ages)){
  # Age group.
  temp_df <- filter(temp_df_F, age_group == ages[i])
  CY_perc_F[,paste(ages[i], "F (CY)")] <- sprintf("%.1f", round(quantile(temp_df$vo2_ml_kg_min, perc_decimal),1))
  
  rm(temp_df)
}

CY_perc <- left_join(CY_perc_M, CY_perc_F, by = "perc")
rm(CY_perc_M, CY_perc_F, temp_df_M, temp_df_F)
#CY_perc <- data.frame(t(CY_perc))
#CY_perc <- mutate(CY_perc, Labels = row.names(CY_perc))
#CY_perc <- select(CY_perc, Labels, X1:X7)

###############################################################################
# Shapiro Wilk test for normailty of distributions.
###############################################################################

to_test <- c("all", "CABG", "MI", "PCI", "HF")
ages <- c("30s", "40s", "50s", "60s", "70s", "80s")
modes <- c("TM", "CY")
sexes <- c("Male", "Female")

SW_summary <- data.frame(to_test)

for(i in 1:length(to_test)){
  for(m in 1:length(modes)){
    for(s in 1:length(sexes)){
      for(a in 1:length(ages)){
        if (to_test[i] == "all"){
          temp_df <- dataCVD
        } else if (to_test[i] == "CABG"){
          temp_df <- data_CABG
        } else if (to_test[i] == "MI"){
          temp_df <- data_MI
        }else if (to_test[i] == "PCI"){
          temp_df <- data_PCI
        } else {
          temp_df <- data_HF
        } 
        
        temp_df <- filter(temp_df, age_group == ages[a])
        temp_df <- filter(temp_df, Gender == sexes[s])
        temp_df <- filter(temp_df, Mode == modes[m])
        
        if(length(temp_df$vo2_ml_kg_min) > 3){
          SW_summary[i,paste(sexes[s], ages[a], modes[m], sep = "")] <- 
            round(shapiro.test(temp_df$vo2_ml_kg_min)$p.value, 4) 
        } else {
          SW_summary[i,paste(sexes[s], ages[a], modes[m], sep = "")] <- NA
        }
        
        
      }
    }
  }
  
}

###############################################################################
# Quartiles for each comparison.
###############################################################################

quartiles <- c(10, 25, 50, 75, 90)
quartiles_decimal <- quartiles/100

# No 30 year olds for this comparison.
ages_v2 <- c("40s", "50s", "60s", "70s", "80s")

quartile_summary <- data.frame(quartiles)

for(q in 1:length(quartiles)){
  for(i in 1:length(to_test)){
    for(m in 1:length(modes)){
      for(s in 1:length(sexes)){
        for(a in 1:length(ages_v2)){
          if (to_test[i] == "all"){
            temp_df <- dataCVD
          } else if (to_test[i] == "CABG"){
            temp_df <- data_CABG
          } else if (to_test[i] == "MI"){
            temp_df <- data_MI
          }else if (to_test[i] == "PCI"){
            temp_df <- data_PCI
          } else {
            temp_df <- data_HF
          } 
          
          temp_df <- filter(temp_df, age_group == ages_v2[a])
          temp_df <- filter(temp_df, Gender == sexes[s])
          temp_df <- filter(temp_df, Mode == modes[m])
          
          quartile_summary[,paste(to_test[i], sexes[s], ages_v2[a], modes[m], sep = "")] <- 
            sprintf("%.1f", round(quantile(temp_df$vo2_ml_kg_min, quartiles_decimal),1))
          
        }
      }
    }
  }
}

###############################################################################
# Save the output.
###############################################################################

x <- list("CABG" = mean_CABG, "MI" = mean_MI, 
          "PCI" = mean_PCI, "HF" = mean_HF, "CVD" = mean_CVD, 
          "TMperc" = TM_perc, "CYperc" = CY_perc, "quartiles"=quartile_summary)
write_xlsx(x, path = here::here("FRIEND CVD Ref Standards_9_20_2021_.xlsx"), col_names = T)


###############################################################################
# Graphing.
###############################################################################

library(ggplot2)

###############################################################################
# Cardiac Categories.
#########################################
# CABG.
#########################################
# Just 40-89 years old (age filtered within each ggplot).
#dfCABG <- filter(data_CABG, Gender == "Male")
dfCABG <- filter(data_CABG, ageattest>=40 & ageattest<90)
dfCABG$Gender <- factor(dfCABG$Gender, levels = c("Male", "Female"))
##### CYCLING.
fig_CABGcy <- ggplot(filter(dfCABG, Mode=="CY"), 
                     aes(x = age_group, y = vo2_ml_kg_min, fill = Gender)) +
  geom_boxplot() +
  scale_x_discrete(name = "Age Group (years)", 
                   breaks = c("40s", "50s", "60s", "70s", "80s"), 
                   labels = c("40-49", "50-59", "60-69", "70-79", "80-89")) +
  # Unicode used to create the VO2max units label (find symbol on Wiki for unicode characters).
  labs(y=expression(paste("VO"["2peak"],"(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  scale_y_continuous(limits = c(0,50),
                     breaks = c(0,5,10,15,20,25,30,35,40,45,50) ) +  theme_linedraw() +
  theme(panel.grid = element_line(colour = "white")) +
  labs(title = "CABG") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("grey69", "grey96")) +
  theme(legend.position = "none")
##### TREADMILL.
fig_CABGtm <- ggplot(filter(dfCABG, Mode=="TM"), aes(x = age_group, y = vo2_ml_kg_min, fill = Gender)) +
  geom_boxplot() +
  scale_x_discrete(name = "Age Group (years)", 
                   breaks = c("40s", "50s", "60s", "70s", "80s"), 
                   labels = c("40-49", "50-59", "60-69", "70-79", "80-89")) +
  # Unicode used to create the VO2max units label (find symbol on Wiki for unicode characters).
  labs(y=expression(paste("VO"["2peak"],"(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  scale_y_continuous(limits = c(0,55),
                     breaks = c(0,5,10,15,20,25,30,35,40,45,50,55) ) +
  theme_linedraw() +
  theme(panel.grid = element_line(colour = "white")) +
  labs(title = "CABG") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("grey69", "grey96")) +
  theme(legend.position = "none")

##### CYCLING.
fig_CABGcyM <- ggplot(filter(dfCABG, Mode=="CY", Gender == "Male"), aes(x = age_group, y = vo2_ml_kg_min)) +
  geom_boxplot(fill = "grey69") +
  scale_x_discrete(name = "Age Group (years)", 
                   breaks = c("40s", "50s", "60s", "70s", "80s"), 
                   labels = c("40-49", "50-59", "60-69", "70-79", "80-89")) +
  # Unicode used to create the VO2max units label (find symbol on Wiki for unicode characters).
  labs(y=expression(paste("VO"["2peak"],"(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  scale_y_continuous(limits = c(0,50),
                     breaks = c(0,5,10,15,20,25,30,35,40,45,50) ) +  theme_linedraw() +
  theme(panel.grid = element_line(colour = "white")) +
  labs(title = "CABG") +
  theme(plot.title = element_text(hjust = 0.5))

fig_CABGcyF <- ggplot(filter(dfCABG, Mode=="CY", Gender == "Female"), aes(x = age_group, y = vo2_ml_kg_min)) +
  geom_boxplot(fill = "grey96") +
  scale_x_discrete(name = "Age Group (years)", 
                   breaks = c("40s", "50s", "60s", "70s", "80s"), 
                   labels = c("40-49", "50-59", "60-69", "70-79", "80-89")) +
  # Unicode used to create the VO2max units label (find symbol on Wiki for unicode characters).
  labs(y=expression(paste("VO"["2peak"],"(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  scale_y_continuous(limits = c(0,50),
                     breaks = c(0,5,10,15,20,25,30,35,40,45,50) ) +  theme_linedraw() +
  theme(panel.grid = element_line(colour = "white")) +
  theme(plot.background = element_rect(fill = "grey93", colour = "grey93")) +
  labs(title = "CABG") +
  theme(plot.title = element_text(hjust = 0.5))

##### TREADMILL.
fig_CABGtmM <- ggplot(filter(dfCABG, Mode=="TM", Gender == "Male"), aes(x = age_group, y = vo2_ml_kg_min)) +
  geom_boxplot(fill = "grey69") +
  scale_x_discrete(name = "Age Group (years)", 
                   breaks = c("40s", "50s", "60s", "70s", "80s"), 
                   labels = c("40-49", "50-59", "60-69", "70-79", "80-89")) +
  # Unicode used to create the VO2max units label (find symbol on Wiki for unicode characters).
  labs(y=expression(paste("VO"["2peak"],"(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  scale_y_continuous(limits = c(0,55),
                     breaks = c(0,5,10,15,20,25,30,35,40,45,50,55) ) +
  theme_linedraw() +
  theme(panel.grid = element_line(colour = "white")) +
  labs(title = "CABG") +
  theme(plot.title = element_text(hjust = 0.5))

fig_CABGtmF <- ggplot(filter(dfCABG, Mode=="TM", Gender == "Female"), aes(x = age_group, y = vo2_ml_kg_min)) +
  geom_boxplot(fill = "grey96") +
  scale_x_discrete(name = "Age Group (years)", 
                   breaks = c("40s", "50s", "60s", "70s", "80s"), 
                   labels = c("40-49", "50-59", "60-69", "70-79", "80-89")) +
  # Unicode used to create the VO2max units label (find symbol on Wiki for unicode characters).
  labs(y=expression(paste("VO"["2peak"],"(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  scale_y_continuous(limits = c(0,55),
                     breaks = c(0,5,10,15,20,25,30,35,40,45,50,55) ) +
  theme_linedraw() +
  theme(panel.grid = element_line(colour = "white")) +
  theme(plot.background = element_rect(fill = "grey93", colour = "grey93")) +
  labs(title = "CABG") +
  theme(plot.title = element_text(hjust = 0.5))

#########################################
# MI.
#########################################
# Just men and 40-89 years old.
#dfMI <- filter(data_MI, Gender == "Male")
dfMI <- filter(data_MI, ageattest>=40 & ageattest<90)
dfMI$Gender <- factor(dfMI$Gender, levels = c("Male", "Female"))
#### CYCLING.
fig_MIcy <- ggplot(filter(dfMI, Mode=="CY"), 
                   aes(x = age_group, y = vo2_ml_kg_min, fill = Gender)) +
  geom_boxplot() +
  scale_x_discrete(name = "Age Group (years)", 
                   breaks = c("40s", "50s", "60s", "70s", "80s"), 
                   labels = c("40-49", "50-59", "60-69", "70-79", "80-89")) +
  labs(y=expression(paste("VO"["2peak"],"(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  scale_y_continuous(limits = c(0,50),
                     breaks = c(0,5,10,15,20,25,30,35,40,45,50) ) +
  theme_linedraw() +
  theme(panel.grid = element_line(colour = "white")) +
  labs(title = "MI") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("grey69", "grey96")) +
  theme(legend.position = "none")
###### TREADMILL.
fig_MItm <- ggplot(filter(dfMI, Mode=="TM"), 
                   aes(x = age_group, y = vo2_ml_kg_min, fill = Gender)) +
  geom_boxplot() +
  scale_x_discrete(name = "Age Group (years)", 
                   breaks = c("40s", "50s", "60s", "70s", "80s"), 
                   labels = c("40-49", "50-59", "60-69", "70-79", "80-89")) +
  labs(y=expression(paste("VO"["2peak"],"(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  scale_y_continuous(limits = c(0,55),
                     breaks = c(0,5,10,15,20,25,30,35,40,45,50,55) ) +
  theme_linedraw() +
  theme(panel.grid = element_line(colour = "white")) +
  labs(title = "MI") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("grey69", "grey96")) +
  theme(legend.position = "none")


#### CYCLING.
fig_MIcyM <- ggplot(filter(dfMI, Mode=="CY", Gender == "Male"), aes(x = age_group, y = vo2_ml_kg_min)) +
  geom_boxplot(fill = "grey69") +
  scale_x_discrete(name = "Age Group (years)", 
                   breaks = c("40s", "50s", "60s", "70s", "80s"), 
                   labels = c("40-49", "50-59", "60-69", "70-79", "80-89")) +
  labs(y=expression(paste("VO"["2peak"],"(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  scale_y_continuous(limits = c(0,50),
                     breaks = c(0,5,10,15,20,25,30,35,40,45,50) ) +
  theme_linedraw() +
  theme(panel.grid = element_line(colour = "white")) +
  labs(title = "MI") +
  theme(plot.title = element_text(hjust = 0.5))

fig_MIcyF <- ggplot(filter(dfMI, Mode=="CY", Gender == "Female"), aes(x = age_group, y = vo2_ml_kg_min)) +
  geom_boxplot(fill = "grey96") +
  scale_x_discrete(name = "Age Group (years)", 
                   breaks = c("40s", "50s", "60s", "70s", "80s"), 
                   labels = c("40-49", "50-59", "60-69", "70-79", "80-89")) +
  labs(y=expression(paste("VO"["2peak"],"(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  scale_y_continuous(limits = c(0,50),
                     breaks = c(0,5,10,15,20,25,30,35,40,45,50) ) +
  theme_linedraw() +
  theme(panel.grid = element_line(colour = "white")) +
  theme(plot.background = element_rect(fill = "grey93", colour = "grey93")) +
  labs(title = "MI") +
  theme(plot.title = element_text(hjust = 0.5))

###### TREADMILL.
fig_MItmM <- ggplot(filter(dfMI, Mode=="TM", Gender == "Male"), aes(x = age_group, y = vo2_ml_kg_min)) +
  geom_boxplot(fill = "grey69") +
  scale_x_discrete(name = "Age Group (years)", 
                   breaks = c("40s", "50s", "60s", "70s", "80s"), 
                   labels = c("40-49", "50-59", "60-69", "70-79", "80-89")) +
  labs(y=expression(paste("VO"["2peak"],"(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  scale_y_continuous(limits = c(0,55),
                     breaks = c(0,5,10,15,20,25,30,35,40,45,50,55) ) +
  theme_linedraw() +
  theme(panel.grid = element_line(colour = "white")) +
  labs(title = "MI") +
  theme(plot.title = element_text(hjust = 0.5))

fig_MItmF <- ggplot(filter(dfMI, Mode=="TM", Gender == "Female"), aes(x = age_group, y = vo2_ml_kg_min)) +
  geom_boxplot(fill = "grey96") +
  scale_x_discrete(name = "Age Group (years)", 
                   breaks = c("40s", "50s", "60s", "70s", "80s"), 
                   labels = c("40-49", "50-59", "60-69", "70-79", "80-89")) +
  labs(y=expression(paste("VO"["2peak"],"(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  scale_y_continuous(limits = c(0,55),
                     breaks = c(0,5,10,15,20,25,30,35,40,45,50,55) ) +
  theme_linedraw() +
  theme(panel.grid = element_line(colour = "white")) +
  theme(plot.background = element_rect(fill = "grey93", colour = "grey93")) +
  labs(title = "MI") +
  theme(plot.title = element_text(hjust = 0.5))

#########################################
# PCI
#########################################
# Just men and 40-89 years old.
#dfPCI <- filter(data_PCI, Gender == "Male")
dfPCI <- filter(data_PCI, ageattest>=40 & ageattest<90)
dfPCI$Gender <- factor(dfPCI$Gender, levels = c("Male", "Female"))

####### CYCLING.
fig_PCIcy <- ggplot(filter(dfPCI, Mode=="CY"), 
                    aes(x = age_group, y = vo2_ml_kg_min, fill = Gender)) +
  geom_boxplot() +
  scale_x_discrete(name = "Age Group (years)", 
                   breaks = c("40s", "50s", "60s", "70s", "80s"), 
                   labels = c("40-49", "50-59", "60-69", "70-79", "80-89")) +
  labs(y=expression(paste("VO"["2peak"],"(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  scale_y_continuous(limits = c(0,50),
                     breaks = c(0,5,10,15,20,25,30,35,40,45,50) ) +
  theme_linedraw() +
  theme(panel.grid = element_line(colour = "white")) +
  labs(title = "PCI") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("grey69", "grey96")) +
  theme(legend.position = "none")
####### TREADMILL.
fig_PCItm <- ggplot(filter(dfPCI, Mode=="TM"), 
                    aes(x = age_group, y = vo2_ml_kg_min, fill = Gender)) +
  geom_boxplot() +
  scale_x_discrete(name = "Age Group (years)", 
                   breaks = c("40s", "50s", "60s", "70s", "80s"), 
                   labels = c("40-49", "50-59", "60-69", "70-79", "80-89")) +
  labs(y=expression(paste("VO"["2peak"],"(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  scale_y_continuous(limits = c(0,55),
                     breaks = c(0,5,10,15,20,25,30,35,40,45,50,55) ) +
  theme_linedraw() +
  theme(panel.grid = element_line(colour = "white")) +
  labs(title = "PCI") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("grey69", "grey96")) +
  theme(legend.position = "none")


####### CYCLING.
fig_PCIcyM <- ggplot(filter(dfPCI, Mode=="CY", Gender == "Male"), aes(x = age_group, y = vo2_ml_kg_min)) +
  geom_boxplot(fill = "grey69") +
  scale_x_discrete(name = "Age Group (years)", 
                   breaks = c("40s", "50s", "60s", "70s", "80s"), 
                   labels = c("40-49", "50-59", "60-69", "70-79", "80-89")) +
  labs(y=expression(paste("VO"["2peak"],"(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  scale_y_continuous(limits = c(0,50),
                     breaks = c(0,5,10,15,20,25,30,35,40,45,50) ) +
  theme_linedraw() +
  theme(panel.grid = element_line(colour = "white")) +
  labs(title = "PCI") +
  theme(plot.title = element_text(hjust = 0.5))

fig_PCIcyF <- ggplot(filter(dfPCI, Mode=="CY", Gender == "Female"), aes(x = age_group, y = vo2_ml_kg_min)) +
  geom_boxplot(fill = "grey96") +
  scale_x_discrete(name = "Age Group (years)", 
                   breaks = c("40s", "50s", "60s", "70s", "80s"), 
                   labels = c("40-49", "50-59", "60-69", "70-79", "80-89")) +
  labs(y=expression(paste("VO"["2peak"],"(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  scale_y_continuous(limits = c(0,50),
                     breaks = c(0,5,10,15,20,25,30,35,40,45,50) ) +
  theme_linedraw() +
  theme(panel.grid = element_line(colour = "white")) +
  theme(plot.background = element_rect(fill = "grey93", colour = "grey93")) +
  labs(title = "PCI") +
  theme(plot.title = element_text(hjust = 0.5))

####### TREADMILL.
fig_PCItmM <- ggplot(filter(dfPCI, Mode=="TM", Gender == "Male"), aes(x = age_group, y = vo2_ml_kg_min)) +
  geom_boxplot(fill = "grey69") +
  scale_x_discrete(name = "Age Group (years)", 
                   breaks = c("40s", "50s", "60s", "70s", "80s"), 
                   labels = c("40-49", "50-59", "60-69", "70-79", "80-89")) +
  labs(y=expression(paste("VO"["2peak"],"(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  scale_y_continuous(limits = c(0,55),
                     breaks = c(0,5,10,15,20,25,30,35,40,45,50,55) ) +
  theme_linedraw() +
  theme(panel.grid = element_line(colour = "white")) +
  labs(title = "PCI") +
  theme(plot.title = element_text(hjust = 0.5))

fig_PCItmF <- ggplot(filter(dfPCI, Mode=="TM", Gender == "Female"), aes(x = age_group, y = vo2_ml_kg_min)) +
  geom_boxplot(fill = "grey96") +
  scale_x_discrete(name = "Age Group (years)", 
                   breaks = c("40s", "50s", "60s", "70s", "80s"), 
                   labels = c("40-49", "50-59", "60-69", "70-79", "80-89")) +
  labs(y=expression(paste("VO"["2peak"],"(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  scale_y_continuous(limits = c(0,55),
                     breaks = c(0,5,10,15,20,25,30,35,40,45,50,55) ) +
  theme_linedraw() +
  theme(panel.grid = element_line(colour = "white")) +
  theme(plot.background = element_rect(fill = "grey93", colour = "grey93")) +
  labs(title = "PCI") +
  theme(plot.title = element_text(hjust = 0.5))

#########################################
# Heart Failure.
#########################################
# Just men and 40-89 years old.
#dfHF <- filter(data_HF, Gender == "Male")
dfHF <- filter(data_HF, ageattest>=40 & ageattest<90)
dfHF$Gender <- factor(dfHF$Gender, levels = c("Male", "Female"))
####### CYCLING.
fig_HFcy <- ggplot(filter(dfHF, Mode=="CY"), 
                   aes(x = age_group, y = vo2_ml_kg_min, fill = Gender)) +
  geom_boxplot() +
  scale_x_discrete(name = "Age Group (years)", 
                   breaks = c("40s", "50s", "60s", "70s", "80s"), 
                   labels = c("40-49", "50-59", "60-69", "70-79", "80-89")) +
  labs(y=expression(paste("VO"["2peak"],"(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  scale_y_continuous(limits = c(0,50),
                     breaks = c(0,5,10,15,20,25,30,35,40,45,50) ) +
  theme_linedraw() +
  theme(panel.grid = element_line(colour = "white"))+
  labs(title = "HF") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("grey69", "grey96")) +
  theme(legend.position = "none")
####### TREADMILL.
fig_HFtm <- ggplot(filter(dfHF, Mode=="TM"), 
                   aes(x = age_group, y = vo2_ml_kg_min, fill = Gender)) +
  geom_boxplot() +
  scale_x_discrete(name = "Age Group (years)", 
                   breaks = c("40s", "50s", "60s", "70s", "80s"), 
                   labels = c("40-49", "50-59", "60-69", "70-79", "80-89")) +
  labs(y=expression(paste("VO"["2peak"],"(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  scale_y_continuous(limits = c(0,55),
                     breaks = c(0,5,10,15,20,25,30,35,40,45,50,55) ) +
  theme_linedraw() +
  theme(panel.grid = element_line(colour = "white"))+
  labs(title = "HF") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("grey69", "grey96")) +
  theme(legend.position = "none")

####### CYCLING.
fig_HFcyM <- ggplot(filter(dfHF, Mode=="CY", Gender == "Male"), aes(x = age_group, y = vo2_ml_kg_min)) +
  geom_boxplot(fill = "grey69") +
  scale_x_discrete(name = "Age Group (years)", 
                   breaks = c("40s", "50s", "60s", "70s", "80s"), 
                   labels = c("40-49", "50-59", "60-69", "70-79", "80-89")) +
  labs(y=expression(paste("VO"["2peak"],"(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  scale_y_continuous(limits = c(0,50),
                     breaks = c(0,5,10,15,20,25,30,35,40,45,50) ) +
  theme_linedraw() +
  theme(panel.grid = element_line(colour = "white"))+
  labs(title = "HF") +
  theme(plot.title = element_text(hjust = 0.5))

fig_HFcyF <- ggplot(filter(dfHF, Mode=="CY", Gender == "Female"), aes(x = age_group, y = vo2_ml_kg_min)) +
  geom_boxplot(fill = "grey96") +
  scale_x_discrete(name = "Age Group (years)", 
                   breaks = c("40s", "50s", "60s", "70s", "80s"), 
                   labels = c("40-49", "50-59", "60-69", "70-79", "80-89")) +
  labs(y=expression(paste("VO"["2peak"],"(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  scale_y_continuous(limits = c(0,50),
                     breaks = c(0,5,10,15,20,25,30,35,40,45,50) ) +
  theme_linedraw() +
  theme(panel.grid = element_line(colour = "white"))+
  theme(plot.background = element_rect(fill = "grey93", colour = "grey93")) +
  labs(title = "HF") +
  theme(plot.title = element_text(hjust = 0.5))

####### TREADMILL.
fig_HFtmM <- ggplot(filter(dfHF, Mode=="TM", Gender == "Male"), aes(x = age_group, y = vo2_ml_kg_min)) +
  geom_boxplot(fill = "grey69") +
  scale_x_discrete(name = "Age Group (years)", 
                   breaks = c("40s", "50s", "60s", "70s", "80s"), 
                   labels = c("40-49", "50-59", "60-69", "70-79", "80-89")) +
  labs(y=expression(paste("VO"["2peak"],"(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  scale_y_continuous(limits = c(0,55),
                     breaks = c(0,5,10,15,20,25,30,35,40,45,50,55) ) +
  theme_linedraw() +
  theme(panel.grid = element_line(colour = "white"))+
  labs(title = "HF") +
  theme(plot.title = element_text(hjust = 0.5))

fig_HFtmF <- ggplot(filter(dfHF, Mode=="TM", Gender == "Female"), aes(x = age_group, y = vo2_ml_kg_min)) +
  geom_boxplot(fill = "grey96") +
  scale_x_discrete(name = "Age Group (years)", 
                   breaks = c("40s", "50s", "60s", "70s", "80s"), 
                   labels = c("40-49", "50-59", "60-69", "70-79", "80-89")) +
  labs(y=expression(paste("VO"["2peak"],"(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  scale_y_continuous(limits = c(0,55),
                     breaks = c(0,5,10,15,20,25,30,35,40,45,50,55) ) +
  theme_linedraw() +
  theme(panel.grid = element_line(colour = "white"))+
  theme(plot.background = element_rect(fill = "grey93", colour = "grey93")) +
  labs(title = "HF") +
  theme(plot.title = element_text(hjust = 0.5))


# Combine to get graphs.
# COMBINED Cycling.
gridExtra::grid.arrange(fig_CABGcy, fig_MIcy, fig_PCIcy, fig_HFcy, nrow = 2)
# COMBINED Treadmill. 
gridExtra::grid.arrange(fig_CABGtm, fig_MItm, fig_PCItm, fig_HFtm, nrow = 2)

# Cyling.
gridExtra::grid.arrange(fig_CABGcyM, fig_MIcyM, fig_PCIcyM, fig_HFcyM,
                        fig_CABGcyF, fig_MIcyF, fig_PCIcyF, fig_HFcyF,nrow = 2)
# Treadmill. 
gridExtra::grid.arrange(fig_CABGtmM, fig_MItmM, fig_PCItmM, fig_HFtmM, 
                        fig_CABGtmF, fig_MItmF, fig_PCItmF, fig_HFtmF,nrow = 2)

# Men Cyling.
#gridExtra::grid.arrange(fig_CABGcyM, fig_MIcyM, fig_PCIcyM, fig_HFcyM, nrow = 2)

# Men Treadmill. 
#gridExtra::grid.arrange(fig_CABGtmM, fig_MItmM, fig_PCItmM, fig_HFtmM,nrow = 2)

# Women Cyling.
#gridExtra::grid.arrange(fig_CABGcyF, fig_MIcyF, fig_PCIcyF, fig_HFcyF,nrow = 2)

# Women Treadmill. 
#gridExtra::grid.arrange(fig_CABGtmF, fig_MItmF, fig_PCItmF, fig_HFtmF,nrow = 2)

###############################################################################
### ALL CVD graphs.
# "outlier.shape = NA" to hide outliers. 
dataCVD$Gender <- factor(dataCVD$Gender, levels = c("Male", "Female"))
# CYCLING.
fig_CVD_cy <- ggplot(filter(dataCVD, Mode=="CY"), 
                     aes(x = age_group, y = vo2_ml_kg_min, fill = Gender)) +
  geom_boxplot() +
  scale_x_discrete(name = "Age Group (years)", 
                   breaks = c("30s", "40s", "50s", "60s", "70s", "80s"), 
                   labels = c("30-39", "40-49", "50-59", "60-69", "70-79", "80-89")) +
  labs(y=expression(paste("VO"["2peak"],"(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  scale_y_continuous(limits = c(0,50),
                     breaks = c(0,5,10,15,20,25,30,35,40,45,50) ) +
  theme_linedraw() +
  theme(panel.grid = element_line(colour = "white")) +
  scale_fill_manual(values = c("grey69", "grey96")) +
  theme(legend.position = "none")
# TREADMILL.
fig_CVD_tm <- ggplot(filter(dataCVD, Mode=="TM"), 
                     aes(x = age_group, y = vo2_ml_kg_min, fill = Gender)) +
  geom_boxplot() +
  scale_x_discrete(name = "Age Group (years)", 
                   breaks = c("30s", "40s", "50s", "60s", "70s", "80s"), 
                   labels = c("30-39", "40-49", "50-59", "60-69", "70-79", "80-89")) +
  labs(y=expression(paste("VO"["2peak"],"(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  scale_y_continuous(limits = c(0,60),
                     breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60) ) +
  theme_linedraw() +
  theme(panel.grid = element_line(colour = "white")) +
  scale_fill_manual(values = c("grey69", "grey96")) +
  theme(legend.position = "none")

# Men CYCLING.
fig_CVD_Mcy <- ggplot(filter(dataCVD, Mode=="CY" & Gender == "Male"), aes(x = age_group, y = vo2_ml_kg_min)) +
  geom_boxplot(fill = "grey69") +
  scale_x_discrete(name = "Age Group (years)", 
                   breaks = c("30s", "40s", "50s", "60s", "70s", "80s"), 
                   labels = c("30-39", "40-49", "50-59", "60-69", "70-79", "80-89")) +
  labs(y=expression(paste("VO"["2peak"],"(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  scale_y_continuous(limits = c(0,50),
                     breaks = c(0,5,10,15,20,25,30,35,40,45,50) ) +
  theme_linedraw() +
  theme(panel.grid = element_line(colour = "white")) +
  labs(title = "A")

# Women CYCLING
fig_CVD_Fcy <- ggplot(filter(dataCVD, Mode=="CY" & Gender == "Female"), aes(x = age_group, y = vo2_ml_kg_min)) +
  geom_boxplot(fill = "grey96") +
  scale_x_discrete(name = "Age Group (years)", 
                   breaks = c("30s", "40s", "50s", "60s", "70s", "80s"), 
                   labels = c("30-39", "40-49", "50-59", "60-69", "70-79", "80-89")) +
  labs(y=expression(paste("VO"["2peak"],"(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  scale_y_continuous(limits = c(0,50),
                     breaks = c(0,5,10,15,20,25,30,35,40,45,50) ) +
  theme_linedraw() +
  theme(panel.grid = element_line(colour = "white")) +
  labs(title = "B")


# Men TREADMILL.
fig_CVD_Mtm <- ggplot(filter(dataCVD, Mode=="TM" & Gender == "Male"), aes(x = age_group, y = vo2_ml_kg_min)) +
  geom_boxplot(fill = "grey69") +
  scale_x_discrete(name = "Age Group (years)", 
                   breaks = c("30s", "40s", "50s", "60s", "70s", "80s"), 
                   labels = c("30-39", "40-49", "50-59", "60-69", "70-79", "80-89")) +
  labs(y=expression(paste("VO"["2peak"],"(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  scale_y_continuous(limits = c(0,60),
                     breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60) ) +
  theme_linedraw() +
  theme(panel.grid = element_line(colour = "white")) +
  labs(title = "A")

# Women TREADMILL.
fig_CVD_Ftm <- ggplot(filter(dataCVD, Mode=="TM" & Gender == "Female"), aes(x = age_group, y = vo2_ml_kg_min)) +
  geom_boxplot(fill = "grey96") +
  scale_x_discrete(name = "Age Group (years)", 
                   breaks = c("30s", "40s", "50s", "60s", "70s", "80s"), 
                   labels = c("30-39", "40-49", "50-59", "60-69", "70-79", "80-89")) +
  labs(y=expression(paste("VO"["2peak"],"(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  scale_y_continuous(limits = c(0,60),
                     breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60) ) +
  theme_linedraw() +
  theme(panel.grid = element_line(colour = "white")) +
  labs(title = "B")

# COMBINED TREADMILL 
gridExtra::grid.arrange(fig_CVD_Mtm, fig_CVD_Ftm, nrow = 2)

# COMBINED CYCLING.
gridExtra::grid.arrange(fig_CVD_Mcy, fig_CVD_Fcy, nrow = 2)

# COMBINED ALL.
gridExtra::grid.arrange(fig_CVD_Mtm, fig_CVD_Mcy, fig_CVD_Ftm, fig_CVD_Fcy, nrow = 2)


# -----------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------
dfCABG <- filter(data_CABG, ageattest>=40 & ageattest<90)

CABGscatter <- ggplot(filter(dfCABG, Mode=="TM" & Gender == "Male"), 
                      aes(x = ageattest, y = vo2_ml_kg_min)) +
  geom_point(color = "grey40") +
  theme_bw() +
  labs(y=expression(paste("VO"["2peak"],"(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")")),
       x=expression(paste("Age (years)"))) +
  scale_y_continuous(limits = c(0,60),
                     breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60)) +
  geom_smooth(method = "lm", se = F) 


# -----------------------------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------------------------

fig_healthy <- ggplot(filter(data_healthy, Mode=="CY"), aes(x = age_group, y = vo2_ml_kg_min)) +
  geom_boxplot() +
  scale_x_discrete(name = "Age Groups (years)", 
                   breaks = c("40s", "50s", "60s", "70s"), 
                   labels = c("40-49", "50-59", "60-69", "70-79")) +
  labs(y=expression(paste("Healthy VO"["2max"],"(mL","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  ylim(0, 50) +
  theme_linedraw() +
  theme(panel.grid = element_line(colour = "white"))
fig_healthy


# -----------------------------------------------------------------------------------------------------

data_healthy <- filter(data_healthy, !(Facility=="MET-test"))

table(data_healthy$age_group[data_healthy$Mode=="CY"])
table(data_healthy$age_group[data_healthy$Mode=="TM"])

# -----------------------------------------------------------------------------------------------------
# For the healthy treadmill values.
data <- filter(data, !ANYCVD>0)
data <- filter(data, Mode=="TM")
# The %in% leave those with NA in cells, == function will drop those with NA.
data <- data[!(data$COPD %in% 1),]
data <- filter(data, country=="USA")
data <- filter(data, Gender=="Male")

tapply(data$vo2_ml_kg_min, data$age_group,'mean')





######## Healthy folks.
data_healthy <- data
# No CVD (if value missing they get dropped).
data_healthy <- filter(data_healthy, !ANYCVD>0)
# Drops those with COPD (but keeps if value missing).
data_healthy <- data_healthy[!(data_healthy$COPD %in% 1),]
# Keeps only USA and Canda data (since that's the CVD data locations).
data_healthy <- filter(data_healthy, Country=="USA" | Country=="CAN")
# Drops females since most data of CVD is from males.
data_healthy <- filter(data_healthy, Gender=="Male")

# Summary table of mean rel VO2 by age and exercise mode.
mean_healthy <- data_healthy %>%
  select(vo2_ml_kg_min, age_group, Mode, ageattest, height_SI, weight_SI) %>%
  group_by(Mode, age_group) %>%
  summarise_if(is.numeric, function(x) {paste(paste(sprintf("%.1f", round(mean(x, na.rm = T), digits = 1)),
                                                    sprintf("%.1f", round(sd(x, na.rm = T), digits = 1)), sep = " ± "))})
colnames(mean_healthy)[colnames(mean_healthy)=="vo2_ml_kg_min"] <- "Healthy vo2_ml_kg_min"

n_healthy <- data_healthy %>%
  select(ID, age_group, Mode) %>%
  group_by(Mode, age_group) %>%
  summarise_if(is.numeric, function(x) length(which(!is.na(x))))
colnames(n_healthy)[colnames(n_healthy)=="ID"] <- "n_healthy"

healthy_sum <- left_join(mean_healthy, n_healthy, by=c("Mode", "age_group"))
rm("mean_healthy","n_healthy")


########################

t.test(data_CABG$vo2_ml_kg_min[data_CABG$age_group=="40s" & data_CABG$Mode=="CY"], 
       data_CABG$vo2_ml_kg_min[data_CABG$age_group=="40s" & data_CABG$Mode=="TM"], paired = F)
