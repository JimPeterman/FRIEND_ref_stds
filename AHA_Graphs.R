
# Creating regression equations to predict CRF based on CVD status.
# USE THIS FOR THE AHA ABSTRACT POSTER.
# (there's a different sample for the manuscript - includes Mayo data in paper).

library(dplyr)
# library(tidyr)
# library(tibble)
library(readxl)
library(writexl)
library(FRIENDanalysis)

# Importing as text seems to keep things from getting messed up.
data_all <- read_excel(here::here("FRIEND_dataset_with_City_12_28_20.xlsx"), col_types = "text")
# This includes Mayo and UM data.
# data_all <- read_excel(here::here("FRIEND_dataset_with_City_1_14_22.xlsx"), col_types = "text")

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
data <- dplyr::filter(data, !is.na(ANYCVD))

# Includes only those 40-89 years old.
data <- dplyr::filter(data, ageattest>=40 & ageattest<90)

# Drops those not coded as M/F (so missing gender coding).
table(data$Gender)
data <- dplyr::filter(data, Gender=="Male" | Gender=="Female")

# Drops those with 0 for height, weight, age, or VO2.
data <- dplyr::filter(data, !(height==0))
data <- dplyr::filter(data, !(weight==0))
data <- dplyr::filter(data, !(ageattest==0))
data <- dplyr::filter(data, !(vo2_ml_kg_min==0))
data <- dplyr::filter(data, !(max_hr==0))

# Only looks at TM and CY data.
data <- dplyr::filter(data, Mode=="TM" | Mode=="CY")

# Drops those with RER<1.0.
data <- dplyr::filter(data, max_rer>=1.0)

# Only looks at USA and Canada..
data <- dplyr::filter(data, Country=="USA" | Country=="CAN")

# Check outputs to make sure there aren't mistakes.
table(data$Country)
table(data$Mode)
table(data$Gender)
table(data$Facility)

# Add in the FRIEND percentiles (based on healthy folks).
# data <- mutate(data, CRFpercentile = FRIENDpercentile(vo2_ml_kg_min, ageattest, 
#                                                       Gender, Mode))

###############################################################################
# Specific to this analysis:

# Add in BMI categories (to hopefully fix heteroskedasticity in models).
# (didn't help...)
data <- mutate(data, BMI_cat = ifelse(BMI < 18.5, "under", 
                                      ifelse(BMI < 25.0, "healthy", 
                                             ifelse(BMI < 30.0, "over", 
                                                    ifelse(is.na(BMI), NA, "obese")))))

make_num <- c("resting_hr", "resting_sbp", "CABG", "MI", "PCI", "HeartFailure")
data[make_num] <- sapply(data[make_num], as.numeric)

# Function to calculate CRF from de Souza e Silva pub (EJPC, 2018)
silva <- function(age, sex, weight_lbs, height_in, ex_mode){
  result_vec <- vector(mode = "numeric", length = length(sex))
  
  for(i in 1:length(sex)){
    if(sex[i] == "Male"){
      sex_ <- 1
    } else if(sex[i] == "Female"){
      sex_ <- 2
    } else {
      sex_ <- NA
    }
    
    if(ex_mode[i] == "TM"){
      mode_ <- 1
    } else if(ex_mode[i] == "CY"){
      mode_ <- 2
    } else {
      mode_ <- NA
    }
    
    CRF <- 45.2 - (.35*age[i]) - (10.9*sex_) - (.15*weight_lbs[i]) + (.68*height_in[i]) - (.46*mode_)
    result_vec[i] <- (round(CRF, 1))
    
  }
  return(result_vec)
}

data <- mutate(data, silva_CRF_pred = 
                 silva(ageattest, Gender, weight, height, Mode))

# CVD equation.
new_eq <- function(age, sex, weight_lbs, height_in, ex_mode, CABG, MI, PCI, HF){
  result_vec <- vector(mode = "numeric", length = length(sex))
  
  for(i in 1:length(sex)){
    if(sex[i] == "Male"){
      sex_ <- 1
    } else if(sex[i] == "Female"){
      sex_ <- 0
    } else {
      sex_ <- NA
    }
    
    if(ex_mode[i] == "TM"){
      mode_ <- 1
    } else if(ex_mode[i] == "CY"){
      mode_ <- 0
    } else {
      mode_ <- NA
    }
    
    CRF <- 17.03 - (.21*age[i]) + (3.60*sex_) - (.11*weight_lbs[i]) + (.12*height_in[i]) + (3.75*mode_) -
      (2.4*CABG[i]) - (0.29*MI[i]) + (.75*PCI[i]) - (3.90*HF[i])
    result_vec[i] <- (round(CRF, 1))
    
  }
  return(result_vec)
}

data <- mutate(data, new_CRF_pred = 
                    new_eq(ageattest, Gender, weight_SI, height_SI, Mode, CABG, MI, PCI, HeartFailure))


###############################################################################
# Creating the different CVD category datasets.
###############################################################################

#######
# Patrick Savage AFTER our initial submission said CVD categories were incorrect.
# So, below is the fix/recategorizations (MI and CABG had to change).
#######

##### For getting sample size of those with max watts.
#data <- filter(data, !(max_load_watts == 0))
#data <- filter(data, !(is.na(max_load_watts)))


# ######## CABG data.
# data_CABG <- filter(data, CABG==1)
# 
# # Drops those with PCI, MI (n=857) or heart failure (n=159) (keeps if values are missing).
# data_CABG <- data_CABG[!(data_CABG$HeartFailure %in% 1),]
# #data_CABG <- data_CABG[!(data_CABG$MI %in% 1),]
# data_CABG <- data_CABG[!(data_CABG$PCI %in% 1),]
# 
# ######## MI data.
# data_MI <- filter(data, MI==1)
# 
# # Drops those from Norway (since all the others are USA and Canada (and there's not a lot from Norway))
# #data_MI <- filter(data_MI, !(Country=="NOR"))
# # Drops those with heart failure (n=265) (keeps folks if heart failure values are missing).
# data_MI <- data_MI[!(data_MI$HeartFailure %in% 1),]
# # Savage had us drop those with CABG.
# data_MI <- data_MI[!(data_MI$CABG %in% 1),]
# 
# ########## PCI data.
# data_PCI <- filter(data, PCI==1)
# 
# #Drops those with MI (n=2602), heart failure (n=144), CABG (n=342)(keeps if values are missing).
# data_PCI <- data_PCI[!(data_PCI$MI %in% 1),]
# data_PCI <- data_PCI[!(data_PCI$HeartFailure %in% 1),]
# data_PCI <- data_PCI[!(data_PCI$CABG %in% 1),]
# 
# ########## Heart Failure.
# data_HF <- filter(data, HeartFailure==1)

########## ALL CVD.
# Includes MEN and WOMEN.
dataCVD <- dplyr::filter(data, HeartFailure==1 | PCI==1 | MI==1 | CABG==1)


###############################################################################
# Creating train/test datasets (80/20).
###############################################################################

# Drops individuals who don't have these key variables.
# (Only doing CVD dataset since that's the primary concern here).
var_int <- c("vo2_ml_kg_min", "ageattest", "Gender", "height_SI", "weight_SI", 
             "CABG", "MI", "PCI", "HeartFailure", "Mode")
dataCVD <- dataCVD[complete.cases(dataCVD[,var_int]),]


# Set seed to reproduce.
set.seed(80303)

# df_lst <- list("CABG"=data_CABG, "MI"=data_MI, "PCI"=data_PCI, "HF"=data_HF, "CVD"=dataCVD)
df_lst <- list("CVD"=dataCVD)

for(i in 1:length(df_lst)){
  temp_df <- df_lst[[i]]
  
  temp_df <- mutate(temp_df, new_id = 1:nrow(temp_df))
  train <- temp_df %>% sample_frac(0.8)
  test <- temp_df %>% anti_join(train, temp_df, by = "new_id")
  
  temp_name <- names(df_lst)[i]
  assign(paste("train", temp_name, sep="_"), train)
  assign(paste("test", temp_name, sep="_"), test)
}

###############################################################################
# Bland-Altman plots.
###############################################################################

library(ggplot2)
library(gridExtra)

df_test <- test_CVD

####### Start with the CVD Equation.
df_test <- mutate(df_test, avg_CVD_eq = (vo2_ml_kg_min + new_CRF_pred)/2)
df_test <- mutate(df_test, diff_CVD_eq = new_CRF_pred - vo2_ml_kg_min)

ba_CVD <- ggplot(df_test, aes(x = avg_CVD_eq, y = diff_CVD_eq)) +
  theme_classic() +
  geom_point(alpha = 0.5) +
  # mean and SD lines.
  geom_hline(yintercept = mean(df_test$diff_CVD_eq, na.rm = T), color = "red", size = 0.75) +
  geom_hline(yintercept = mean(df_test$diff_CVD_eq, na.rm = T) - (1.96 * sd(df_test$diff_CVD_eq, na.rm = T)), 
             color = "royalblue3", size = 0.75, linetype = "dashed") +
  geom_hline(yintercept = mean(df_test$diff_CVD_eq, na.rm = T) + (1.96 * sd(df_test$diff_CVD_eq, na.rm = T)), 
             color = "royalblue3", size = 0.75, linetype = "dashed") +
  # axis labels.
  labs(y= expression(paste("Predicted CRF - CRF (ml/kg/min)")),
       x=expression(paste("Average Measure (ml/kg/min)"))) +
  # x- and y-axis size.
  coord_cartesian(xlim = c(0, 60), ylim = c(-25, 30)) +
  # graph label.
  labs(title = "CVD Equation") +
  theme(plot.title = element_text(hjust = 0.5, lineheight = 0.9, size = 12, face = "bold")) +
  # print the mean.
  annotate(geom = "text", x = 55, y = mean(df_test$diff_CVD_eq, na.rm = T)+1.75, label = "Mean",
           size = 5) +
  annotate(geom = "text", x = 55, y = mean(df_test$diff_CVD_eq, na.rm = T)-1.75, 
           label = sprintf("%.1f", round(mean(df_test$diff_CVD_eq, na.rm = T), 3)),
           size = 5) +
  # print the positive SD values.
  annotate(geom = "text", x = 55, y = (sd(df_test$diff_CVD_eq, na.rm = T))*1.96+
             mean(df_test$diff_CVD_eq, na.rm = T)+1.75, label = "+1.96SD",
           size = 5) +
  annotate(geom = "text", x = 55, y = (sd(df_test$diff_CVD_eq, na.rm = T))*1.96+
             mean(df_test$diff_CVD_eq, na.rm = T)-1.75, 
           label = sprintf("%.1f", round((sd(df_test$diff_CVD_eq, na.rm = T))*1.96+mean(df_test$diff_CVD_eq, na.rm = T),1)),
           size = 5) +
  # print the negative SD values.
  annotate(geom = "text", x = 55, y = mean(df_test$diff_CVD_eq, na.rm = T)-
             (sd(df_test$diff_CVD_eq, na.rm = T))*1.96+1.75, label = "-1.96SD",
           size = 5) +
  annotate(geom = "text", x = 55, y = mean(df_test$diff_CVD_eq, na.rm = T)-
             (sd(df_test$diff_CVD_eq, na.rm = T))*1.96-1.75, 
           label = sprintf("%.1f",round(mean(df_test$diff_CVD_eq, na.rm = T)-(sd(df_test$diff_CVD_eq, na.rm = T)*1.96),1)),
           size = 5)


########### Healthy Equation
df_test <- mutate(df_test, avg_healthy_eq = (vo2_ml_kg_min + silva_CRF_pred)/2)
df_test <- mutate(df_test, diff_healthy_eq = silva_CRF_pred - vo2_ml_kg_min)

ba_healthy <- ggplot(df_test, aes(x = avg_healthy_eq, y = diff_healthy_eq)) +
  theme_classic() +
  geom_point(alpha = 0.5) +
  # mean and SD lines.
  geom_hline(yintercept = mean(df_test$diff_healthy_eq, na.rm = T), color = "red", size = 0.75) +
  geom_hline(yintercept = mean(df_test$diff_healthy_eq, na.rm = T) - (1.96 * sd(df_test$diff_healthy_eq, na.rm = T)), 
             color = "royalblue3", size = 0.75, linetype = "dashed") +
  geom_hline(yintercept = mean(df_test$diff_healthy_eq, na.rm = T) + (1.96 * sd(df_test$diff_healthy_eq, na.rm = T)), 
             color = "royalblue3", size = 0.75, linetype = "dashed") +
  # axis labels.
  labs(y= expression(paste("Predicted CRF - CRF (ml/kg/min)")),
       x=expression(paste("Average Measure (ml/kg/min)"))) +
  # x- and y-axis size.
  coord_cartesian(xlim = c(0, 60), ylim = c(-25, 30)) +
  # graph label.
  labs(title = "Healthy Equation") +
  theme(plot.title = element_text(hjust = 0.5, lineheight = 0.9, size = 12, face = "bold")) +
  # print the mean.
  annotate(geom = "text", x = 55, y = mean(df_test$diff_healthy_eq, na.rm = T)+1.75, label = "Mean",
           size = 5) +
  annotate(geom = "text", x = 55, y = mean(df_test$diff_healthy_eq, na.rm = T)-1.75, 
           label = sprintf("%.1f", round(mean(df_test$diff_healthy_eq, na.rm = T), 3)),
           size = 5) +
  # print the positive SD values.
  annotate(geom = "text", x = 55, y = (sd(df_test$diff_healthy_eq, na.rm = T))*1.96+
             mean(df_test$diff_healthy_eq, na.rm = T)+1.75, label = "+1.96SD",
           size = 5) +
  annotate(geom = "text", x = 55, y = (sd(df_test$diff_healthy_eq, na.rm = T))*1.96+
             mean(df_test$diff_healthy_eq, na.rm = T)-1.75, 
           label = sprintf("%.1f", round((sd(df_test$diff_healthy_eq, na.rm = T))*1.96+mean(df_test$diff_healthy_eq, na.rm = T),1)),
           size = 5) +
  # print the negative SD values.
  annotate(geom = "text", x = 55, y = mean(df_test$diff_healthy_eq, na.rm = T)-
             (sd(df_test$diff_healthy_eq, na.rm = T))*1.96+1.75, label = "-1.96SD",
           size = 5) +
  annotate(geom = "text", x = 55, y = mean(df_test$diff_healthy_eq, na.rm = T)-
             (sd(df_test$diff_healthy_eq, na.rm = T))*1.96-1.75, 
           label = sprintf("%.1f",round(mean(df_test$diff_healthy_eq, na.rm = T)-(sd(df_test$diff_healthy_eq, na.rm = T)*1.96),1)),
           size = 5)

# Save as 500x700.
gridExtra::grid.arrange(ba_healthy, ba_CVD, nrow = 2)

###############################################################################
# Regression fit.
###############################################################################

# Find differences between predicted and measured (for color coding graphs).
# Percent difference.
df_test <- mutate(df_test, prec_off_CVD = ((new_CRF_pred - vo2_ml_kg_min)/vo2_ml_kg_min) *100)
df_test <- mutate(df_test, prec_grp_off_CVD = ifelse(abs(prec_off_CVD)< 10, "<10%",
                                                     ifelse(abs(prec_off_CVD)<20, "<20%",
                                                            ifelse(abs(prec_off_CVD)<30, "<30%", "≥30%"))))
df_test$prec_grp_off_CVD <- factor(df_test$prec_grp_off_CVD, 
                            levels = c("<10%", "<20%", "<30%", "≥30%"))

# METS difference.
df_test <- mutate(df_test, mets_off_CVD = ((new_CRF_pred - vo2_ml_kg_min)/3.5))
df_test <- mutate(df_test, mets_grp_off_CVD = ifelse(abs(mets_off_CVD)< .5, "<0.5",
                                                     ifelse(abs(mets_off_CVD)<1, "<1",
                                                            ifelse(abs(mets_off_CVD)<2, "<2", "≥2"))))
table(df_test$mets_grp_off_CVD)
label_mets_grp_off_CVD <- c(paste("<0.5 METS\nn=", sum(df_test$mets_grp_off_CVD == "<0.5"), " (",
                                  sprintf("%.0f", sum(df_test$mets_grp_off_CVD == "<0.5")/nrow(df_test)*100), 
                                  "%)", "\n", sep=""), 
                            paste("0.5-0.99 METS\nn=", sum(df_test$mets_grp_off_CVD == "<1"), " (",
                                  sprintf("%.0f", sum(df_test$mets_grp_off_CVD == "<1")/nrow(df_test)*100), 
                                  "%)", "\n", sep=""),
                            paste("1-1.99 METS\nn=", sum(df_test$mets_grp_off_CVD == "<2"), " (",
                                  sprintf("%.0f", sum(df_test$mets_grp_off_CVD == "<2")/nrow(df_test)*100), 
                                  "%)", "\n", sep=""),
                            paste("≥2 METS\nn=", sum(df_test$mets_grp_off_CVD == "≥2"), " (",
                                  sprintf("%.0f", sum(df_test$mets_grp_off_CVD == "≥2")/nrow(df_test)*100), 
                                  "%)", "\n", sep=""))

# Better to create the correlation statement to paste here and then call it in ggplot.
rsq_label_CVD <- paste("R^2== ", sprintf("%.2f", cor(df_test$new_CRF_pred, df_test$vo2_ml_kg_min, use = "complete.obs")^2))

reg_CVD <- ggplot(df_test, aes(x = vo2_ml_kg_min, 
                    y = new_CRF_pred, color=mets_grp_off_CVD)) +
  geom_abline(intercept = 0, slope = 1, color = "grey85", size = 2) +
  scale_color_manual("Difference from\nMeasured CRF\n(Absolute METS)*",
                     values = c("≥2"="red3", "<0.5"="darkgreen",
                                "<2"="chocolate2", "<1"="gold2"),
                     labels = label_mets_grp_off_CVD) +
                     # labels = c(paste("<0.5 (n=", sum(df_test$mets_grp_off_CVD == "<0.5"), ")", sep=""), 
                     #            paste("<1 (n=", sum(df_test$mets_grp_off_CVD == "<1"), ")", sep=""),
                     #            paste("<2 (n=", sum(df_test$mets_grp_off_CVD == "<2"), ")", sep=""),
                     #            paste("≥2 (n=", sum(df_test$mets_grp_off_CVD == "≥2"), ")", sep=""))) +
  # scale_color_manual("Absolute Percent\nDifference from\nMeasured CRF",
  #                    values = c("≥30%"="red3", "<10%"="darkgreen",
  #                               "<30%"="chocolate2", "<20%"="gold2"),
  #                    labels = c(paste("<10% (n=", sum(df_test$prec_grp_off_CVD == "<10%"), ")", sep=""), 
  #                               paste("<20% (n=", sum(df_test$prec_grp_off_CVD == "<20%"), ")", sep=""),
  #                               paste("<30% (n=", sum(df_test$prec_grp_off_CVD == "<30%"), ")", sep=""),
  #                               paste("≥30% (n=", sum(df_test$prec_grp_off_CVD == "≥30%"), ")", sep=""))) +
  # Want an enclosed graph but without the gridlines
  theme_bw(base_rect_size = 2, base_line_size = 2) +
  theme(panel.grid = element_blank()) +
  geom_point(size = 3) +
  labs(x=expression(paste("Measured CRF ","(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")")),
       y=expression(paste("Predicted CRF ","(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  # scale_x_continuous(name="Measured CRF (ml/kg/min)") +
  # scale_y_continuous(name="Predicted CRF (ml/kg/min)") +
  # Rotate the new y-axis label so it reads vertically. 
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) +
  # Axis sizes.
  coord_cartesian(xlim = c(0, 50), ylim = c(0, 50)) +
  # Graph title (centered and bold).
  labs(title = "CVD Cohort Equation") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(legend.key = element_rect(color ="white")) +
  # Add in the correlation and sig values.
  annotate(geom = "text", x=5.8, y=50, label = "r = 0.65", size=6) +
  annotate(geom = "text", x=6.4, y=47, label = rsq_label_CVD, parse=T, size = 6) +
  annotate(geom = "text", x=6, y=44, label = "P < 0.05", size=6) + 
  # These are for adjusting text size and legend dot size (since the figure is huge).
  theme(text = element_text(size = 17)) +
  guides(colour = guide_legend(override.aes = list(size=7)))
  
##### Graph with Healthy Equation Prediction
# Percent difference.
df_test <- mutate(df_test, prec_off_healthy = ((silva_CRF_pred - vo2_ml_kg_min)/vo2_ml_kg_min) *100)
df_test <- mutate(df_test, prec_grp_off_healthy = ifelse(abs(prec_off_healthy)< 10, "<10%",
                                                         ifelse(abs(prec_off_healthy)<20, "<20%",
                                                                ifelse(abs(prec_off_healthy)<30, "<30%", "≥30%"))))
df_test$prec_grp_off_healthy <- factor(df_test$prec_grp_off_healthy, 
                                       levels = c("<10%", "<20%", "<30%", "≥30%"))
table(df_test$prec_grp_off_healthy)
# METS difference.
df_test <- mutate(df_test, mets_off_healthy = ((silva_CRF_pred - vo2_ml_kg_min)/3.5))
df_test <- mutate(df_test, mets_grp_off_healthy = ifelse(abs(mets_off_healthy)< 0.5, "<0.5",
                                                     ifelse(abs(mets_off_healthy)<1, "<1",
                                                            ifelse(abs(mets_off_healthy)<2, "<2", "≥2"))))
table(df_test$mets_grp_off_healthy)
label_mets_grp_off_healthy <- c(paste("<0.5 METS\nn=", sum(df_test$mets_grp_off_healthy == "<0.5"), " (",
                                  sprintf("%.0f", sum(df_test$mets_grp_off_healthy == "<0.5")/nrow(df_test)*100), 
                                  "%)", "\n", sep=""), 
                            paste("0.5-0.99 METS\nn=", sum(df_test$mets_grp_off_healthy == "<1"), " (",
                                  sprintf("%.0f", sum(df_test$mets_grp_off_healthy == "<1")/nrow(df_test)*100), 
                                  "%)", "\n", sep=""),
                            paste("1-1.99 METS\nn=", sum(df_test$mets_grp_off_healthy == "<2"), " (",
                                  sprintf("%.0f", sum(df_test$mets_grp_off_healthy == "<2")/nrow(df_test)*100), 
                                  "%)", "\n", sep=""),
                            paste("≥2 METS\nn=", sum(df_test$mets_grp_off_healthy == "≥2"), " (",
                                  sprintf("%.0f", sum(df_test$mets_grp_off_healthy == "≥2")/nrow(df_test)*100), 
                                  "%)", "\n", sep=""))


# Better to create the correlation statement to paste here and then call it in ggplot.
# rsq_label_healthy <- paste("R^2: ", sprintf("%.2f", cor(df_test$silva_CRF_pred, df_test$vo2_ml_kg_min, use = "complete.obs")^2))
rsq_label_healthy <- paste("R^2== ", sprintf("%.2f", 0.23))

reg_healthy <- ggplot(df_test, aes(x = vo2_ml_kg_min, 
                    y = silva_CRF_pred, color=mets_grp_off_healthy)) +
  geom_abline(intercept = 0, slope = 1, color = "grey85", size = 2) +
  scale_color_manual("Difference from\nMeasured CRF\n(Absolute METS)",
                     values = c("≥2"="red3", "<0.5"="darkgreen",
                                "<2"="chocolate2", "<1"="gold2"),
                     labels = label_mets_grp_off_healthy) +
                     # labels = c(paste("<0.5 (n=", sum(df_test$mets_grp_off_healthy == "<0.5"), ")", sep=""), 
                     #            paste("<1 (n=", sum(df_test$mets_grp_off_healthy == "<1"), ")", sep=""),
                     #            paste("<2 (n=", sum(df_test$mets_grp_off_healthy == "<2"), ")", sep=""),
                     #            paste("≥2 (n=", sum(df_test$mets_grp_off_healthy == "≥2"), ")", sep=""))) +
  # scale_color_manual("Absolute Percent\nDifference from\nMeasured CRF",
  #                    values = c("≥30%"="red3", "<10%"="darkgreen",
  #                               "<30%"="chocolate2", "<20%"="gold2"),
  #                    labels = c(paste("<10% (n=", sum(df_test$prec_grp_off_healthy == "<10%"), ")", sep=""), 
  #                               paste("<20% (n=", sum(df_test$prec_grp_off_healthy == "<20%"), ")", sep=""),
  #                               paste("<30% (n=", sum(df_test$prec_grp_off_healthy == "<30%"), ")", sep=""),
  #                               paste("≥30% (n=", sum(df_test$prec_grp_off_healthy == "≥30%"), ")", sep=""))) +
  # Want an enclosed graph but without the gridlines
  theme_bw(base_rect_size = 2, base_line_size = 2) +
  theme(panel.grid = element_blank()) +
  geom_point(size = 3) +
  labs(x=expression(paste("Measured CRF ","(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")")),
       y=expression(paste("Predicted CRF ","(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  # scale_x_continuous(name="Measured CRF (ml/kg/min)") +
  # scale_y_continuous(name="Predicted CRF (ml/kg/min)") +
  # Rotate the new y-axis label so it reads vertically. 
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5)) +
  # Axis sizes.
  coord_cartesian(xlim = c(0, 50), ylim = c(0, 50)) +
  # Graph title (centered and bold).
  labs(title = "Healthy Cohort Equation") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  theme(legend.key = element_rect(color ="white")) +
  # Add in the correlation and sig values.
  annotate(geom = "text", x=5.8, y=50, label = "r = 0.48", size=6) +
  annotate(geom = "text", x=6.4, y=47, label = rsq_label_healthy, parse=T, size = 6) +
  annotate(geom = "text", x=6, y=44, label = "P < 0.05", size=6) + 
  # These are for adjusting text size and legend dot size (since the figure is huge).
  theme(text = element_text(size = 17)) +
  guides(colour = guide_legend(override.aes = list(size=7)))

# Save as 1600x700
gridExtra::grid.arrange(reg_healthy, reg_CVD, nrow = 1)

###############################################################################
# Chi-square to compare distribution of those in the different MET groups
# (difference between measured and predicted CRF).
###############################################################################

chi_df <- rbind(data.frame(temp_var = df_test$mets_grp_off_healthy, group="Healthy"), 
                data.frame(temp_var = df_test$mets_grp_off_CVD, group="CVD"))
temp_tbl <- table(chi_df$group, chi_df$temp_var)
chisq.test(temp_tbl)


