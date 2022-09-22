

# Need to run the CVD_creation_analysis code to get the datasets loaded in the environment.
library(ggplot2)

data_CABG <- mutate(data_CABG, cvd_status = "CABG")
data_MI <- mutate(data_MI, cvd_status = "MI")
data_PCI <- mutate(data_PCI, cvd_status = "PCI")
data_HF <- mutate(data_HF, cvd_status = "HF")

data_healthy <- filter(data, ANYCVD == 0)
data_healthy <- mutate(data_healthy, cvd_status = "No CVD")

data_total <- bind_rows(data_CABG, data_MI, data_PCI, data_HF, data_healthy)


# data_total <- filter(data_total, ageattest>=60 & ageattest<90)
data_total$Gender <- factor(data_total$Gender, levels = c("Male", "Female"))
data_total$cvd_status <- factor(data_total$cvd_status, 
                                levels = c("No CVD", "CABG", "MI", "PCI", "HF"))

####### TREADMILL - MALES.
ggplot(filter(data_total, Mode=="TM", Gender == "Male", age_group != "30s"), 
                    aes(x = age_group, y = vo2_ml_kg_min, fill = cvd_status)) +
  geom_boxplot() +
  scale_x_discrete(name = "Age Group (years)", 
                   breaks = c("40s", "50s", "60s", "70s", "80s"), 
                   labels = c("40-49", "50-59", "60-69", "70-79", "80-89")) +
  labs(y=expression(paste("VO"["2peak"],"(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  scale_y_continuous(limits = c(0,50),
                     breaks = c(0,5,10,15,20,25,30,35,40,45,50) ) +
  theme_linedraw() +
  theme(panel.grid = element_line(colour = "white")) +
  labs(title = "Cardiorespiratory Fitness\nfor Men with Cardiovascular Disease") +
  theme(plot.title = element_text(hjust = 0.5, face="bold")) +
  labs(fill = "CVD Category") +
  scale_fill_manual(values = c("goldenrod3", "seagreen3", "orchid3", "mediumturquoise", "lightcoral")) 


####### TREADMILL - MALES.
ggplot(filter(data_total, Mode=="TM", Gender == "Male", age_group == "60s", cvd_status != "No CVD"), 
       aes(x = age_group, y = vo2_ml_kg_min, fill = cvd_status)) +
  geom_hline(aes(yintercept = 28.2), color = "darkslategrey", linetype = "twodash", size = 1) +
  geom_boxplot() +
  labs(y=expression(paste("VO"["2peak"],"(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  scale_y_continuous(limits = c(0,50),
                     breaks = c(0,5,10,15,20,25,30,35,40,45,50) ) +
  theme_linedraw() +
  theme(panel.grid = element_line(colour = "white")) +
  labs(title = "Cardiorespiratory Fitness\nfor Men 60-69 Years Old\nwith Cardiovascular Disease") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 11)) +
  labs(fill = "CVD Category") +
  scale_fill_manual(values = c("palegreen3", "plum3", "lightblue3", "lightcoral")) +
  annotate(geom = "text", x= 0.42, y = 28.2, label = "Median\nFor No CVD", size = 3, 
           hjust=0, color="darkslategrey") + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) 
  

####### TREADMILL - FEMALES.
ggplot(filter(data_total, Mode=="TM", Gender == "Female", age_group == "60s", cvd_status != "No CVD"), 
       aes(x = age_group, y = vo2_ml_kg_min, fill = cvd_status)) +
  geom_hline(aes(yintercept = 20), color = "darkslategrey", linetype = "twodash", size = 1) +
  geom_boxplot() +
  labs(y=expression(paste("VO"["2peak"],"(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  scale_y_continuous(limits = c(0,50),
                     breaks = c(0,5,10,15,20,25,30,35,40,45,50) ) +
  theme_linedraw() +
  theme(panel.grid = element_line(colour = "white")) +
  labs(title = "Cardiorespiratory Fitness\nfor Women 60-69 Years Old\nwith Cardiovascular Disease") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 11)) +
  labs(fill = "CVD Category") +
  scale_fill_manual(values = c("palegreen3", "plum3", "lightblue3", "lightcoral")) +
  annotate(geom = "text", x= 0.42, y = 20, label = "Median\nFor No CVD", size = 3, 
           hjust=0, color="darkslategrey") + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) 


data_total$vo2_ml_kg_min[data_total$Gender == "Male" & data_total$age_group == "40s" &
                           data_total$Mode == "TM" & data_total$cvd_status == "No CVD"] <- 38.8
data_total$vo2_ml_kg_min[data_total$Gender == "Male" & data_total$age_group == "40s" &
                           data_total$Mode == "TM" & data_total$cvd_status == "No CVD"] <- 33.8
data_total$vo2_ml_kg_min[data_total$Gender == "Male" & data_total$age_group == "40s" &
                           data_total$Mode == "TM" & data_total$cvd_status == "No CVD"] <- 29.4
data_total$vo2_ml_kg_min[data_total$Gender == "Male" & data_total$age_group == "40s" &
                           data_total$Mode == "TM" & data_total$cvd_status == "No CVD"] <- 25.8


my_group <- group_by(select(data_total, cvd_status, age_group, vo2_ml_kg_min), cvd_status)
vo2_mean <- summarise_all(my_group, mean)
vo2_melt <- reshape2::melt(vo2_mean, id = "cvd_status")


ggplot(data_total) +
  geom_line(aes(x = factor(age_group), y = vo2_ml_kg_min, colour = cvd_status, group = cvd_status))


######################################
# TM data for Men
######################################
ages <- c("40-49", "50-59", "60-69", "70-79")
vo2 <- c(38.8, 33.8, 29.4, 25.8)
df <- data.frame(ages, vo2)
df <- mutate(df, cvd_status = "No CVD")

# CABG
vo2 <- c(23.2, 21.5, 20.1, 17.7)
df2 <- data.frame(ages, vo2)
df2 <- mutate(df2, cvd_status = "CABG")

# MI
vo2 <- c(27.9, 26.2, 23.6, 20.4)
df3 <- data.frame(ages, vo2)
df3 <- mutate(df3, cvd_status = "MI")

# PCI
vo2 <- c(28.9, 26.9, 24.1, 20.5)
df4 <- data.frame(ages, vo2)
df4 <- mutate(df4, cvd_status = "PCI")

# HF
vo2 <- c(22.3, 20.0, 18.7, 16.9)
df5 <- data.frame(ages, vo2)
df5 <- mutate(df5, cvd_status = "Heart Failure")

df <- bind_rows(df, df2, df3, df4, df5)
df$cvd_status <- factor(df$cvd_status, 
                                levels = c("No CVD", "CABG", "MI", "PCI", "Heart Failure"))
# 
ggplot(df) +
  geom_line(size = 1, aes(x = ages, y = vo2, group = cvd_status, color = cvd_status),
            position = position_dodge(0.3)) + 
  geom_point(size = 4, shape = 21, aes(x = ages, y = vo2, group = cvd_status, fill = cvd_status),
             position = position_dodge(0.3)) +
  labs(y=expression(paste("VO"["2peak"],"(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  scale_y_continuous(limits = c(0,45),
                     breaks = c(0,5,10,15,20,25,30,35,40,45) ) +
  theme_linedraw() +
  theme(panel.grid = element_line(colour = "white")) +
  labs(title = "Average Cardiorespiratory Fitness in Men") +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold")) +
  labs(fill = "CVD Category") +
  scale_x_discrete(name = "Age Group (years)") +
  guides(fill=guide_legend("CVD Category"), color = FALSE) +
  scale_fill_manual(values = c("goldenrod3", "seagreen3", "orchid3", "mediumturquoise", "lightcoral")) +
  scale_color_manual(values = c("goldenrod3", "seagreen3", "orchid3", "mediumturquoise", "lightcoral"))

######################################
# TM data for Women
######################################
ages <- c("50-59", "60-69", "70-79")
vo2 <- c(24.2, 20.7, 18.3)
dfw <- data.frame(ages, vo2)
dfw <- mutate(dfw, cvd_status = "No CVD")

# CABG
vo2 <- c(17.3, 16.6, 14.7)
df2 <- data.frame(ages, vo2)
df2 <- mutate(df2, cvd_status = "CABG")

# MI
vo2 <- c(20.0, 19.5, 17.2)
df3 <- data.frame(ages, vo2)
df3 <- mutate(df3, cvd_status = "MI")

# PCI
vo2 <- c(19.6, 19.6, 17.5)
df4 <- data.frame(ages, vo2)
df4 <- mutate(df4, cvd_status = "PCI")

# HF
vo2 <- c(16.2, 15.7, 15.7)
df5 <- data.frame(ages, vo2)
df5 <- mutate(df5, cvd_status = "HF")

dfw <- bind_rows(dfw, df2, df3, df4, df5)
dfw$cvd_status <- factor(dfw$cvd_status, 
                        levels = c("No CVD", "CABG", "MI", "PCI", "HF"))
# 
wmn <- ggplot(dfw) +
  geom_line(size = 1, aes(x = ages, y = vo2, group = cvd_status, color = cvd_status),
            position = position_dodge(0.3)) + 
  geom_point(size = 4, shape = 21, aes(x = ages, y = vo2, group = cvd_status, fill = cvd_status),
             position = position_dodge(0.3)) +
  labs(y=expression(paste("VO"["2peak"],"(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  scale_y_continuous(limits = c(0,30),
                     breaks = c(0,5,10,15,20,25,30) ) +
  theme_linedraw() +
  theme(panel.grid = element_line(colour = "white")) +
  labs(title = "Cardiorespiratory Fitness for Women") +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold")) +
  labs(fill = "CVD Category") +
  scale_x_discrete(name = "Age Group (years)") +
  guides(fill=guide_legend("CVD Category"), color = FALSE) +
  scale_fill_manual(values = c("goldenrod3", "seagreen3", "orchid3", "mediumturquoise", "lightcoral")) +
  scale_color_manual(values = c("goldenrod3", "seagreen3", "orchid3", "mediumturquoise", "lightcoral")) 
  


gridExtra::grid.arrange(men, wmn, nrow=2)













ggplot(df) +
  geom_bar(size = 1, aes(x = ages, y = vo2, fill = cvd_status), position = "dodge", stat="identity") + 
  labs(y=expression(paste("VO"["2peak"],"(ml","\U00B7","kg"^"-1","\U00B7","min"^"-1",")"))) +
  scale_y_continuous(limits = c(0,50),
                     breaks = c(0,5,10,15,20,25,30,35,40,45,50) ) +
  theme_linedraw() +
  theme(panel.grid = element_line(colour = "white")) +
  labs(title = "Cardiorespiratory Fitness\nfor Men with Cardiovascular Disease") +
  theme(plot.title = element_text(hjust = 0.5, face="bold", size = 11)) +
  labs(fill = "CVD Category") +
  scale_x_discrete(name = "Age Group (years)") +
  guides(fill=guide_legend("CVD Category"), color = FALSE) +
  scale_fill_manual(values = c("goldenrod3", "seagreen3", "orchid3", "mediumturquoise", "lightcoral")) +
  scale_color_manual(values = c("goldenrod3", "seagreen3", "orchid3", "mediumturquoise", "lightcoral")) 
