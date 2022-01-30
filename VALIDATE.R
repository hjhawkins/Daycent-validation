#***********************************************************************************************
##Validate model BIOMASS and SOC using field data (2016-2019)

#Heidi Hawkins, 2020
#***********************************************************************************************
if(!require(pacman)){install.packages("pacman", dependencies=TRUE); library(pacman)}
p_load(Rmisc, gfcanalysis, dplyr, car, stlplus, tidyverse, tidymodels, Metrics, lubridate, multcomp, multcompView, gtable, ggthemes, data.table, lsmeans, ggplot2, gtable, ggthemes, data.table, ggpubr)

setwd("D:/Temperate_Savanna_Brotherton")
#setwd("C:/Users/01423355/Downloads/OneDrive")

#Plot theme
theme_plotCI_big <- theme(axis.text = element_text(size = 16, color="#5C5C61")) +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(colour = "#5C5C61", size=1))+ theme(panel.background = element_rect(fill = "white"))+ theme(axis.title = element_text(size = 16, colour = "#5C5C61"))
theme_plotCI_med <- theme(axis.text = element_text(size = 14, color="#5C5C61")) +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(colour = "#5C5C61", size=1))+ theme(panel.background = element_rect(fill = "white"))+ theme(axis.title = element_text(size = 14, colour = "#5C5C61"))
theme_plotCI_small <- theme(axis.text = element_text(size = 12, color="#5C5C61")) +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(colour = "#5C5C61", size=1))+ theme(panel.background = element_rect(fill = "white"))+ theme(axis.title = element_text(size = 12, colour = "#5C5C61"))
theme_plotCI_smaller <- theme(axis.text = element_text(size = 11, color="#5C5C61")) +  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(colour = "#5C5C61", size=1))+ theme(panel.background = element_rect(fill = "white"))+ theme(axis.title = element_text(size = 12, colour = "#5C5C61"))

#Load model and observed data (below file modified in excel from EXP_OUTPUT.csv" <-output from Plots.exp.R)
val <- read.csv("EXP_OUTPUT_regressions.csv")
names(val)
str(val)
val <- val %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

as_tibble(val)
na.fail(val)

#Re-order treatment names
val$treatment <- factor(val$treatment,levels = c("HAF", "HBF", "NHAF", "NHBF", "NHNF"))

#MODEL PERFORMANCE TESTS or validation  (r2, RMSE, mean error) on DETRENDED /DESEASONED data (STLPLUS)
#*****************************************************************************************************
#First select per treatment for lm 
val_haf <- val %>% 
  filter(treatment =="HAF")

val_hbf <- val %>% 
  filter(treatment =="HBF")

val_nhaf <- val %>% 
  filter(treatment =="NHAF")

val_nhbf <- val %>% 
  filter(treatment =="NHBF")

val_nhnf <- val %>% 
  filter(treatment =="NHNF")
view(val_haf)

#Now decompose the time series of modeled and measured data, join dataframe 
help(stlplus)

#Weekly decomposition where 1=Sunday
weekDays <- c("Su", "M", "Tu","W", "Th", "F", "Sa")

#HAF
names(val_haf)
HAF_stl <- stlplus(val_haf$BIO_measured, t=val_haf$date,
                    n.p=7, s.window="periodic",
                    sub.labels=weekDays, sub.start=2)

plot(HAF_stl, xlab="Date", ylab="Daily Biomass")


#extract time, trend and season 
HAF_time <- HAF_stl$time 
HAF_season <- HAF_stl$data$seasonal
HAF_trend <- HAF_stl$data$trend

class(HAF_time)
class(HAF_season)
class(HAF_trend)

#Make new df
HAF_df <- as.data.frame(cbind(HAF_time, HAF_trend))
names(HAF_df) <- c("date", "trend")
class(HAF_df)
view(HAF_df)

#Get date back into date format
HAF_df <- mutate(HAF_df, date = as.Date(date, format = "%Y-%m-%d"))
str(HAF_df)
head(HAF_df)
view(HAF_df)

#Repeat for modeled data
names(val_haf)
mHAF_stl <- stlplus(val_haf$BIO_model, t=val_haf$date,
                   n.p=7, s.window="periodic",
                   sub.labels=weekDays, sub.start=2)

plot(mHAF_stl, xlab="Date", ylab="Daily Biomass")


#extract time, trend and season 
mHAF_time <- mHAF_stl$time 
mHAF_season <- mHAF_stl$data$seasonal
mHAF_trend <- mHAF_stl$data$trend

class(mHAF_time)
class(mHAF_season)
class(mHAF_trend)

#Make new df
mHAF_df <- as.data.frame(cbind(mHAF_time, mHAF_trend))
names(mHAF_df) <- c("date", "m_trend")
class(mHAF_df)
view(mHAF_df)

#Get date back into date format
mHAF_df <- mutate(mHAF_df, date = as.Date(date, format = "%Y-%m-%d"))
str(mHAF_df)
head(mHAF_df)
view(mHAF_df)

#Join measured and modelled HAF
HAF <- right_join(HAF_df, mHAF_df, by = "date")
view(HAF)
names(HAF)

#HBF
names(val_hbf)
HBF_stl <- stlplus(val_hbf$BIO_measured, t=val_hbf$date,
                   n.p=7, s.window="periodic",
                   sub.labels=weekDays, sub.start=2)

plot(HBF_stl, xlab="Date", ylab="Daily Biomass")


#extract time, trend and season 
HBF_time <- HBF_stl$time 
HBF_season <- HBF_stl$data$seasonal
HBF_trend <- HBF_stl$data$trend

class(HBF_time)
class(HBF_season)
class(HBF_trend)

#Make new df
HBF_df <- as.data.frame(cbind(HBF_time, HBF_trend))
names(HBF_df) <- c("date", "trend")
class(HBF_df)
view(HBF_df)

#Get date back into date format
HBF_df <- mutate(HBF_df, date = as.Date(date, format = "%Y-%m-%d"))
str(HBF_df)
head(HBF_df)
view(HBF_df)

#Repeat for modeled data
names(val_hbf)
mHBF_stl <- stlplus(val_hbf$BIO_model, t=val_hbf$date,
                    n.p=7, s.window="periodic",
                    sub.labels=weekDays, sub.start=2)

plot(mHBF_stl, xlab="Date", ylab="Daily Biomass")


#extract time, trend and season 
mHBF_time <- mHBF_stl$time 
mHBF_season <- mHBF_stl$data$seasonal
mHBF_trend <- mHBF_stl$data$trend

class(mHBF_time)
class(mHBF_season)
class(mHBF_trend)

#Make new df
mHBF_df <- as.data.frame(cbind(mHBF_time, mHBF_trend))
names(mHBF_df) <- c("date", "m_trend")
class(mHBF_df)
view(mHBF_df)

#Get date back into date format
mHBF_df <- mutate(mHBF_df, date = as.Date(date, format = "%Y-%m-%d"))
str(mHBF_df)
head(mHBF_df)
view(mHBF_df)

#Join measured and modelled HBF
HBF <- right_join(HBF_df, mHBF_df, by = "date")
view(HBF)
names(HBF)

#NHAF
names(val_nhaf)
NHAF_stl <- stlplus(val_nhaf$BIO_measured, t=val_nhaf$date,
                   n.p=7, s.window="periodic",
                   sub.labels=weekDays, sub.start=2)

plot(NHAF_stl, xlab="Date", ylab="Daily Biomass")


#extract time, trend and season 
NHAF_time <- NHAF_stl$time 
NHAF_season <- NHAF_stl$data$seasonal
NHAF_trend <- NHAF_stl$data$trend

class(NHAF_time)
class(NHAF_season)
class(NHAF_trend)

#Make new df
NHAF_df <- as.data.frame(cbind(NHAF_time, NHAF_trend))
names(NHAF_df) <- c("date", "trend")
class(NHAF_df)
view(NHAF_df)

#Get date back into date format
NHAF_df <- mutate(NHAF_df, date = as.Date(date, format = "%Y-%m-%d"))
str(NHAF_df)
head(NHAF_df)
view(NHAF_df)

#Repeat for modeled data
names(val_nhaf)
mNHAF_stl <- stlplus(val_nhaf$BIO_model, t=val_nhaf$date,
                    n.p=7, s.window="periodic",
                    sub.labels=weekDays, sub.start=2)

plot(mNHAF_stl, xlab="Date", ylab="Daily Biomass")


#extract time, trend and season 
mNHAF_time <- mNHAF_stl$time 
mNHAF_season <- mNHAF_stl$data$seasonal
mNHAF_trend <- mNHAF_stl$data$trend

class(mNHAF_time)
class(mNHAF_season)
class(mNHAF_trend)

#Make new df
mNHAF_df <- as.data.frame(cbind(mNHAF_time, mNHAF_trend))
names(mNHAF_df) <- c("date", "m_trend")
class(mNHAF_df)
view(mNHAF_df)

#Get date back into date format
mNHAF_df <- mutate(mNHAF_df, date = as.Date(date, format = "%Y-%m-%d"))
str(mNHAF_df)
head(mNHAF_df)
view(mNHAF_df)

#Join measured and modelled NHAF
NHAF <- right_join(NHAF_df, mNHAF_df, by = "date")
view(NHAF)
names(NHAF)

#NHBF
names(val_nhbf)
NHBF_stl <- stlplus(val_nhbf$BIO_measured, t=val_nhbf$date,
                    n.p=7, s.window="periodic",
                    sub.labels=weekDays, sub.start=2)

plot(NHBF_stl, xlab="Date", ylab="Daily Biomass")


#extract time, trend and season 
NHBF_time <- NHBF_stl$time 
NHBF_season <- NHBF_stl$data$seasonal
NHBF_trend <- NHBF_stl$data$trend

class(NHBF_time)
class(NHBF_season)
class(NHBF_trend)

#Make new df
NHBF_df <- as.data.frame(cbind(NHBF_time, NHBF_trend))
names(NHBF_df) <- c("date", "trend")
class(NHBF_df)
view(NHBF_df)

#Get date back into date format
NHBF_df <- mutate(NHBF_df, date = as.Date(date, format = "%Y-%m-%d"))
str(NHBF_df)
head(NHBF_df)
view(NHBF_df)

#Repeat for modeled data
names(val_nhbf)
mNHBF_stl <- stlplus(val_nhbf$BIO_model, t=val_nhbf$date,
                     n.p=7, s.window="periodic",
                     sub.labels=weekDays, sub.start=2)

plot(mNHBF_stl, xlab="Date", ylab="Daily Biomass")


#extract time, trend and season 
mNHBF_time <- mNHBF_stl$time 
mNHBF_season <- mNHBF_stl$data$seasonal
mNHBF_trend <- mNHBF_stl$data$trend

class(mNHBF_time)
class(mNHBF_season)
class(mNHBF_trend)

#Make new df
mNHBF_df <- as.data.frame(cbind(mNHBF_time, mNHBF_trend))
names(mNHBF_df) <- c("date", "m_trend")
class(mNHBF_df)
view(mNHBF_df)

#Get date back into date format
mNHBF_df <- mutate(mNHBF_df, date = as.Date(date, format = "%Y-%m-%d"))
str(mNHBF_df)
head(mNHBF_df)
view(mNHBF_df)

#Join measured and modelled NHBF
NHBF <- right_join(NHBF_df, mNHBF_df, by = "date")
view(NHBF)
names(NHBF)

#NHNF
names(val_nhnf)
NHNF_stl <- stlplus(val_nhnf$BIO_measured, t=val_nhnf$date,
                    n.p=7, s.window="periodic",
                    sub.labels=weekDays, sub.start=2)

plot(NHNF_stl, xlab="Date", ylab="Daily Biomass")


#extract time, trend and season 
NHNF_time <- NHNF_stl$time 
NHNF_season <- NHNF_stl$data$seasonal
NHNF_trend <- NHNF_stl$data$trend

class(NHNF_time)
class(NHNF_season)
class(NHNF_trend)

#Make new df
NHNF_df <- as.data.frame(cbind(NHNF_time, NHNF_trend))
names(NHNF_df) <- c("date", "trend")
class(NHNF_df)
view(NHNF_df)

#Get date back into date format
NHNF_df <- mutate(NHNF_df, date = as.Date(date, format = "%Y-%m-%d"))
str(NHNF_df)
head(NHNF_df)
view(NHNF_df)

#Repeat for modeled data
names(val_nhnf)
mNHNF_stl <- stlplus(val_nhnf$BIO_model, t=val_nhnf$date,
                     n.p=7, s.window="periodic",
                     sub.labels=weekDays, sub.start=2)

plot(mNHNF_stl, xlab="Date", ylab="Daily Biomass")


#extract time, trend and season 
mNHNF_time <- mNHNF_stl$time 
mNHNF_season <- mNHNF_stl$data$seasonal
mNHNF_trend <- mNHNF_stl$data$trend

class(mNHNF_time)
class(mNHNF_season)
class(mNHNF_trend)

#Make new df
mNHNF_df <- as.data.frame(cbind(mNHNF_time, mNHNF_trend))
names(mNHNF_df) <- c("date", "m_trend")
class(mNHNF_df)
view(mNHNF_df)

#Get date back into date format
mNHNF_df <- mutate(mNHNF_df, date = as.Date(date, format = "%Y-%m-%d"))
str(mNHNF_df)
head(mNHNF_df)
view(mNHNF_df)

#Join measured and modelled NHNF
NHNF <- right_join(NHNF_df, mNHNF_df, by = "date")
view(NHNF)
names(NHNF)

#LINEAR REGRESSION (r2) per treatment
lm_HAF <- lm(m_trend ~ trend, HAF)
summary(lm_HAF) #r2=0.772, p<2e-16 ***

lm_HBF <- lm(m_trend ~ trend, HBF)
summary(lm_HBF) #r2=0.6602, p<2e-16 ***

lm_NHAF <- lm(m_trend ~ trend, NHAF)
summary(lm_NHAF) #r2=0.8091, p<2e-16 ***

lm_NHBF <- lm(m_trend ~ trend, NHBF)
summary(lm_NHBF) #r2=0.6058, p<2e-16 ***

lm_NHNF <- lm(m_trend ~ trend, NHNF)
summary(lm_NHNF) #r2=0.461, p<2e-16 ***

#LINEAR REGRESSION (r2) ALL DATA
ALL <- bind_rows(HAF, HBF,NHAF, NHBF, NHNF)
view(ALL)
lm_ALL <- lm(m_trend ~ trend, ALL)
summary(lm_ALL) #r2=0.5029, p<2e-16 ***

#***
#ROOT MEAN SQUARE ERROR(RMSE) between modelled and observed
#Gives stdev of the model prediction error. A smaller value indicates better model performance
rmse_HAF <- rmse(HAF$trend, HAF$m_trend) #first is "observed", second is "predicted"
rmse_HAF #53.50557

rmse_HBF <- rmse(HBF$trend, HBF$m_trend) #first is "observed", second is "predicted"
rmse_HBF #52.87469

rmse_NHAF <- rmse(NHAF$trend, NHAF$m_trend) #first is "observed", second is "predicted"
rmse_NHAF #72.09993

rmse_NHBF <- rmse(NHBF$trend, NHBF$m_trend) #first is "observed", second is "predicted"
rmse_NHBF #72.09993

rmse_NHNF <- rmse(NHNF$trend, NHNF$m_trend) #first is "observed", second is "predicted"
rmse_NHNF #119.5493

#***
#Mean absolute error (MAE)
mae_HAF <- mae(HAF$trend, HAF$m_trend) #first is "observed", second is "predicted"
mae_HAF #43.67555

mae_HBF <- mae(HBF$trend, HBF$m_trend) #first is "observed", second is "predicted"
mae_HBF #43.39644

mae_NHAF <- mae(NHAF$trend, NHAF$m_trend) #first is "observed", second is "predicted"
mae_NHAF #64.00783

mae_NHBF <- mae(NHBF$trend, NHBF$m_trend) #first is "observed", second is "predicted"
mae_NHBF #60.60926

mae_NHNF <- mae(NHNF$trend, NHNF$m_trend) #first is "observed", second is "predicted"
mae_NHNF #100.2721

#PLOTS
bio1 <- val %>%
  ggplot(aes(BIO_measured, BIO_model)) + 
  geom_point(color = "#0193D7") +
  geom_smooth(method = lm, color = "#0193D7", fill = "#7ECBEF") +
  xlab(expression(paste("Measured herbaceous biomass (g C ", ~m^-2, ")"))) +
  ylab(expression(paste("Modeled herbaceous biomass (g C ", ~m^-2, ")"))) +
  annotate("text", x = 20, y = 400, label = "R ^ 2 == 0.50", color="#5C5C61", size = 4, hjust = 0, parse = TRUE) +
  annotate("text", x = 20, y = 380, label = "p<0.0001", color="#5C5C61", size = 4, hjust = 0) +
  theme_plotCI_med
bio1

bio2 <- val %>%
  ggplot(aes(BIO_measured, BIO_model, color = treatment)) + 
  geom_point() +
  geom_smooth(method = lm, fill = "#7ECBEF") +
  xlab(expression(paste("Measured herbaceous biomass (g C ", ~m^-2, ")"))) +
  ylab(expression(paste("Modeled herbaceous biomass (g C ", ~m^-2, ")"))) +
  annotate("text", x = 20, y = 480, label = "All models p<0.0001", color="#5C5C61", size = 4, hjust = 0) +
  annotate("text", x = 20, y = 450, label = "R ^ 2 == 0.77 ~ (HAF)", color="#5C5C61",  size = 3.5, hjust = 0, parse = TRUE) +
  annotate("text", x = 20, y = 430, label = "R ^ 2 == 0.66 ~ (HBF)", color="#5C5C61", size = 3.5, hjust = 0, parse = TRUE) +
  annotate("text", x = 20, y = 410, label = "R ^ 2 == 0.81 ~ (NHAF)", color="#5C5C61", size = 3.5, hjust = 0, parse = TRUE) +
  annotate("text", x = 20, y = 390, label = "R ^ 2 == 0.61 ~ (NHBF)", color="#5C5C61", size = 3.5, hjust = 0, parse = TRUE) +
  annotate("text", x = 20, y = 370, label = "R ^ 2 == 0.46 ~ (NHNF)", color="#5C5C61", size = 3.5, hjust = 0, parse = TRUE) +
  theme(legend.position = "top") +
  theme_plotCI_med 
bio2

#Time series facet plot per treatment
#select columns and pivot_long
val_BIO <- val[c(1,2,13,15)]
val_BIO <- val_BIO %>% pivot_longer(
  cols = BIO_measured:BIO_model,
  names_to = "type",
  values_to = "BIO_gCm2"
)

names(val_BIO)
view(val_BIO)
str(val_BIO)
na.fail(val_BIO)

#Time series, facet wrap on treatment
bio3 <- val_BIO %>%
  ggplot(aes(date, BIO_gCm2, color = type)) + 
  geom_point() +
  #geom_smooth(method = loess, span = 0.75) +
  xlab("Date") +
  ylab(expression(paste("Modeled herbaceous biomass (g C ", ~m^-2, ")"))) +
  facet_wrap(~treatment) + 
  #scale_color_discrete(name = "Data source", labels = c("Measured", "Modeled")) +
  scale_color_manual(name = "Data source", labels = c("Measured", "Modeled"), values = c("#0193D7", "#7ECBEF")) +
  theme(legend.position = c(0.85, 0.3),legend.direction = "vertical") +
  theme_plotCI_smaller
bio3 


#SOIL ORGANIC CARBON ************************************************************************
#linear models of SOC per treatment (only one significant, too little data - use bloxplot and avova, see end)
lm_SOM_haf <- lm(SOM_model ~ SOM_measured, val_haf)
summary(lm_SOM_haf) #NS

lm_SOM_hbf <- lm(SOM_model ~ SOM_measured, val_hbf)
summary(lm_SOM_hbf) #NS

lm_SOM_nhaf <- lm(SOM_model ~ SOM_measured, val_nhaf)
summary(lm_SOM_nhaf) #NS

lm_SOM_nhbf <- lm(SOM_model ~ SOM_measured, val_nhbf)
summary(lm_SOM_nhbf) #r2=0.6899 p=0.0406

lm_SOM_nhnf <- lm(SOM_model ~ SOM_measured, val_nhnf)
summary(lm_SOM_nhnf) #NS

#***
#RMSE and MAE cannot run

#***************************************************************************************
#SOC boxplots
#first remove NAs, pivot_long
val_SOC_w <- val %>% na.omit(val)
view(val_SOC_w)

val_SOC <- val_SOC_w[c(2,14,16)]
val_SOC <- val_SOC %>% pivot_longer(
  cols = SOM_measured:SOM_model,
  names_to = "type",
  values_to = "SOM_gCm2"
)

names(val_SOC)

soc1 <- val_SOC %>%
  ggplot(aes(treatment,SOM_gCm2, fill = type)) +
  stat_boxplot(geom ='errorbar', position=position_dodge(1), width=.3, size=.2) +
  geom_boxplot(position=position_dodge(1), notch = F, outlier.size = .6, size=.3) +
  stat_summary(data = val_SOC, aes(x = treatment, y = SOM_gCm2), fun = mean, geom = "point", size = 3,  shape = 18, fill = "white", position=position_dodge(1)) +
  scale_fill_manual(name = "Data source", labels = c("Measured", "Modeled"), values = c("#0193D7", "#7ECBEF")) +
  theme(legend.position = "top") + 
  xlab("Treatment") +
  ylab(expression(paste("SOC (g C ", m^-2, ")"))) +
  theme_plotCI_med
soc1 

#ANOVA on SOC
#first remove NAs
val_anova <- val %>% na.omit(val)
view(val_anova)
names(val_anova)

anova <- lm(SOM_model ~ SOM_measured, val_anova)
summary(anova) #p = 0.00131, r2 = 0.1191

#Check assumptions of model
hist(residuals(anova), 
     col="darkgray") #normality

plot(fitted(anova), 
     residuals(anova)) #should be unbiased and homoscedastic 

#data not normally distributed but log transformation does not improve

#DATA MEANS, SEs and ANOVAs for SOC graph (ie Q.Are measured and modeled data similar for each treatment?)
names(val_SOC)
data_SEsummary1 <- summarySE(val_SOC, measurevar=c("SOM_gCm2"), groupvars=c("treatment", "type"))
data_SEsummary1

#Now do 2-way ANOVA
anova1 <- aov(SOM_gCm2 ~ treatment*type,val_SOC)
summary(anova1) #treatment significant p=2.8e-07 ***, but type (modeled or not) is not p=0.0921, ie model predicts measured, and not interaction p=0.3876
res <- anova1$residuals
hist(res, xlab="Residuals") #good
#posthoc TukeyHSD
tukey<-TukeyHSD(anova1)
tukey

#to visualise letters use multcompView, NOTE DESCENDING arrangement is essential for fitting to Tukey
data_summary <-group_by(val_SOC, treatment, type) %>%
  summarise(mean=mean(SOM_gCm2), sd=sd(SOM_gCm2)) %>%
  arrange(desc(mean))
view(data_summary)

#create compact letter display<<<<<<<<<<<<<<<<<<<<<<<<<
tukey.cld<-multcompLetters4(anova1, tukey) 
print(tukey.cld)

#now add letters to the data mean, sd table
cld<-as.data.frame.list(tukey.cld$`treatment:type`)
data_summary$Tukey<-cld$Letters
view(data_summary)

write_csv(data_summary, "data_mean_SOC.csv")

#Make sure that the order is preserved for plotting
val_SOC$treatment <- factor(val_SOC$treatment, levels = c("HAF", "HBF", "NHAF", "NHBF", "NHNF", ordered = TRUE))
val_SOC$type <- factor(val_SOC$type, levels = c("SOM_measured", "SOM_model", ordered = TRUE))
data_summary$type <- factor(data_summary$type, levels = c("SOM_measured", "SOM_model", ordered = TRUE))
data_summary$treatment <- factor(data_summary$treatment, levels = c("HAF", "HBF", "NHAF", "NHBF", "NHNF", ordered = TRUE))


#Now put the letters on the graph from the post-hoc Tukey tests and table created - note is interaction plot to go w 2-way anova
soc2 <- val_SOC %>%
  ggplot(aes(x=interaction(type, treatment), SOM_gCm2)) +
  geom_boxplot(aes(fill = type), size = .3, outlier.size = .6) +
  stat_summary(data = val_SOC, aes(x=interaction(type, treatment), SOM_gCm2), fun = mean, geom = "point", size=8, shape=21, color="black", fill="white") +
  xlab("Treatment") +
  ylab(expression(paste("SOC (g C ", m^-2, ")"))) +
  #Now put the letters on the graph from the post-hoc Tukey tests
  geom_text(data=data_summary, hjust=0.5, vjust=0.25, aes(x=interaction(type, treatment), y=mean, label = Tukey), size=4) +
  scale_fill_manual(name = "Data source", labels = c("Measured", "Modeled"), values = c("#0193D7", "#7ECBEF")) +
  #now overwrite X axis which is a mess from the interaction plot
  scale_x_discrete(labels = c("HAF", "", "HBF", "", "NHAF", "", "NHBF", "", "NHNF", "")) +
  theme(axis.text.x = element_text(hjust=-0.3)) +
  theme_plotCI_med
soc2


#***************************************************************************************

#PRINT graphs
ggsave("validate_BIOMASS1.pdf", plot = bio1, width = 14, height = 14, units = "cm",
       dpi = 300, limitsize = TRUE)
ggsave("validate_BIOMASS1.tiff", plot = bio1, width = 14, height = 14, units = "cm",
       dpi = 300, limitsize = TRUE)
ggsave("validate_BIOMASS2.pdf", plot = bio2, width = 14, height = 14, units = "cm",
       dpi = 300, limitsize = TRUE)
ggsave("validate_BIOMASS2.tiff", plot = bio2, width = 14, height = 14, units = "cm",
       dpi = 300, limitsize = TRUE)
ggsave("validate_BIOMASS3.pdf", plot = bio3, width = 18, height = 14, units = "cm",
       dpi = 300, limitsize = TRUE)
ggsave("validate_BIOMASS3.tiff", plot = bio3, width = 18, height = 14, units = "cm",
       dpi = 300, limitsize = TRUE)
ggsave("validate_SOC.pdf", plot = soc2, width = 18, height = 14, units = "cm",
       dpi = 300, limitsize = TRUE)
ggsave("validate_SOC.tiff", plot = soc2, width = 18, height = 14, units = "cm",
       dpi = 300, limitsize = TRUE)

dev.off()

