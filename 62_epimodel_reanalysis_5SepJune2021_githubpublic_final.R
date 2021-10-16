##########################################################################################
#DATA RETRIEVAL FROM COVID19.ORG FOR MAHARASHTRA INDIA AND ANALYSIS USING RECON LIBRARIES
##########################################################################################
# Project Name: COVID-19 Pandemic: Did harsh mobility restrictions save lives and cost in Maharashtra, India?
# Creator: Preshit Ambade
# Date: 25 November 2020
#################################################################################################################################

# analysis for entire maharashtra state

rm(list = ls()) #Clears everything
# install.packages(c("distcrete", "earlyR", "EpiEstim", "epitrix", "ggpubr", "gridGraphics", "incidence", "outbreaks", "projections"))


# #download state_wise_daily data
# file_state_wise_daily <- paste0("/Users/preshitambade/Downloads/raw_state_wise_daily_", 
#                                 format(Sys.time(), "%Y-%m-%d-%H-%M-%S-AZMST", tz = "MST"), ".csv")                   
# 
# 
# download.file(url='https://api.covid19india.org/csv/latest/state_wise_daily.csv',
#               destfile=file_state_wise_daily, method='curl', overwrite = FALSE)
# 
# library(readr)
# statedf<-read.csv(file_state_wise_daily,
#                   header = T, na.strings=c("","NA"))  #this file is manually created. blank cells in string variable changed to NA


setwd("/Users/preshitambade/Projects/2_MH_COVID19")


library(readr)
statedf<-read.csv("raw_state_wise_daily_2020-06-01-08-33-52-AZMST.csv",
                  header = T, na.strings=c("","NA"), stringsAsFactors = FALSE)  #this file is manually created. blank cells in string variable changed to NA


#############################################
#THIS CSV FILE CONTAINS DATA UPTO 31 MAY2020#
#############################################

#statedf<- read.csv(url('https://api.covid19india.org/csv/latest/state_wise_daily.csv'), header = T, na.strings=c("","NA"))

str(statedf, strict.width = "cut", width = 76) #to know dataframe structure
head(statedf)  #start date 14 march 2020
tail(statedf)  # end date 31 May 2020

#subsetting dataframe and keeping only MH data
new_mahadata <- subset.data.frame(statedf, select =c(Date, Status, MH))
str(new_mahadata, strict.width = "cut", width = 76) #to know dataframe structure
head(new_mahadata) #start date 14 march 2020
tail(new_mahadata) # end date 31 May 2020


#adding missing dates AT TOP OF DATAFRAME using MEDD Report- 10 May 2020
mahadata <- rbind(data.frame(Date = "13-Mar-20", Status = "Confirmed", MH = 11), new_mahadata)
mahadata <- rbind(data.frame(Date = "12-Mar-20", Status = "Confirmed", MH = 0), mahadata)
mahadata <- rbind(data.frame(Date = "11-Mar-20", Status = "Confirmed", MH = 11), mahadata)
mahadata <- rbind(data.frame(Date = "10-Mar-20", Status = "Confirmed", MH = 4), mahadata)
mahadata <- rbind(data.frame(Date = "09-Mar-20", Status = "Confirmed", MH = 3), mahadata)

head(mahadata)
str(mahadata, strict.width = "cut", width = 76) #to know dataframe structure

rm(new_mahadata)


str(mahadata, strict.width = "cut", width = 76) #to know dataframe structure


#A.creating subset only containing different status and then combining them to create final datasets

#A1. For daily incidence
mhinci <- subset.data.frame(mahadata, Status == "Confirmed", select =c(Date, MH))
head(mhinci)
colnames(mhinci)[2] <- "I"
head(mhinci)


#A2. For daily recovered
mhrecov <- subset.data.frame(mahadata, Status == "Recovered", select =c(Date, MH))
head(mhrecov)
colnames(mhrecov)[2] <- "R"
#mhrecov <-rename(mhrecov, c(MH = "R"))
head(mhrecov)


#A3. For daily deaths
mhdeceased <- subset.data.frame(mahadata, Status == "Deceased", select =c(Date, MH))
head(mhdeceased)
colnames(mhdeceased)[2] <- "D"
#mhdeceased <-rename(mhdeceased, c(MH = "D"))
head(mhdeceased)

#A4. Creating daily dataset in long shape
head(mahadata)

library(tidyverse)
mahadata_status_uncount <- uncount(mahadata, weights = MH, .remove = FALSE)
head(mahadata_status_uncount,10)

mahadata_status_uncount$MH = 1  #replace all row values with 1
head(mahadata_status_uncount)
str(mahadata_status_uncount, strict.width = "cut", width = 76) #to know dataframe structure


#changing class of date variable
class(mahadata$Date)
class(mhinci$Date)
class(mhrecov$Date)
class(mhdeceased$Date)
class(mahadata_status_uncount$Date)


library("lubridate")
mahadata$Date <- dmy(mahadata$Date)
class(mahadata$Date)

mhinci$Date <- dmy(mhinci$Date)
class(mhinci$Date)

mhrecov$Date <- dmy(mhrecov$Date)
class(mhrecov$Date)

mhdeceased$Date <- dmy(mhdeceased$Date)
class(mhdeceased$Date)

mahadata_status_uncount$Date <- dmy(mahadata_status_uncount$Date)
class(mahadata_status_uncount$Date)

str(mahadata, strict.width = "cut", width = 76) #to know dataframe structure
str(mhinci, strict.width = "cut", width = 76) #to know dataframe structure
str(mhrecov, strict.width = "cut", width = 76) #to know dataframe structure
str(mhdeceased, strict.width = "cut", width = 76) #to know dataframe structure
str(mahadata_status_uncount, strict.width = "cut", width = 76) #to know dataframe structure

head(mahadata, 10)
head(mhinci, 10)
head(mhrecov, 10)
head(mhdeceased, 10)
head(mahadata_status_uncount, 10)

#rm(mahaclean, mahadata, mahadata_status_uncount, mhinci, mhinci_uncount)


############################################################
#modeling 
#############################################################

##A. Estimate the incidence and fit the model
#using incidence 
library(outbreaks)
library(ggplot2)
library(incidence)
#install.packages("plotly")
#install.packages("ggpubr")
library(ggpubr)
#install.packages("gridGraphics")
library(grid)
library(gridGraphics)
#library(plotly)
#?incidence

########################
#1. For only incidence
########################
#plot_save <- paste0("/Users/preshitambade/Downloads/Corona-CEA/1-COVID-19_CEA_study/2-COVID-19-Model/5-Epimodel_demo/2_epimodel_results/7_epimodel_results_formanuscript_8Dec2020/")     

#1a. creating and plotting incidence object
maha_incidence_object <- as.incidence(x = mhinci[, 2], dates = mhinci$Date)

maha_incidence_object 



fig1<- plot(maha_incidence_object, color= "blue", border = "white" )+
  labs(title="Daily incidence of lab-confirmed cases, Maharashtra-India",
       subtitle = "(9 March-31 May 2020)")
fig1

ggsave(filename = "1_MH_incidenceplot.tiff") 

dev.off()  #removes plot from plot window          


######################
### A.Fitting Model###
######################

######################################
#A.1.saving different lockdown dates
######################################
#source1: https://en.wikipedia.org/wiki/COVID-19_pandemic_lockdown_in_India
#source2: https://timesofindia.indiatimes.com/travel/destinations/india-lockdown-3-0-nationwide-lockdown-extends-till-may-17-parts-of-country-will-open-but-no-rail-road-air-travel-for-now/as75493510.cms
# Nationwide lockdown:
# Phase 1: 25 March 2020 – 14 April 2020 (21 days)
# Phase 2: 15 April 2020 – 3 May 2020 (19 days)
# Phase 3: 4 May 2020 – 17 May 2020 (14 days)
# Phase 4: 18 May 2020 – 31 May 2020 (14 days)

#Maharashtra Date sequence:
#data start day-2020-03-09(1st day)
#first lockdown start-2020-03-25 (17th day from 2020-03-09)
#second lockdown start-2020-04-15 (38th day from 2020-03-09)
#third lockdown start-2020-05-04 (57th day from 2020-03-09)
#third lockdown end i.e. data end day- 2020-05-17 (70th day from 2020-03-09)
#next 30 day date = 2020-06-16 ie 16 June 2020 (total 100 days)

lock1startdate <- as.Date("2020-03-25") #lockdown-1 start date  (by mistake I considered it 24th March in previous files)
lock1enddate <- as.Date("2020-04-14") #lockdown-1 end date
lock2enddate <- as.Date("2020-05-03") #lockdown-2 end date
lock3enddate <- as.Date("2020-05-17") #lockdown-3 end date


#####################################################
#A.6.Estimating different splits across entire period
#####################################################

###########################################
#A.6.1.Estimate fit for pre-lockdown period
###########################################

lockdown0.fit <- fit(maha_incidence_object[1:16])

lockdown0.fit

fig11x <- plot(maha_incidence_object[1:16], color = "blue", border = "white") %>% add_incidence_fit(lockdown0.fit)+
  labs(title="Observed and modelled incidence of COVID-19 cases in Maharashtra-India",
       subtitle="09 March 2020 to 24 March 2020 (pre-lockdown)")
fig11x


#####################################################################################
#A.6.2.Estimate fit for incidence between first lockdown ie 25 March to 14 April 2020
#####################################################################################

lockdown1.fit <- fit(maha_incidence_object[17:37])

lockdown1.fit

fig11y <- plot(maha_incidence_object[17:37], color = "blue", border = "white") %>% add_incidence_fit(lockdown1.fit)+
  labs(title="Observed and modelled incidence of COVID-19 cases in Maharashtra-India",
       subtitle="24 March 2020 to 14 April 2020 (lockdown-1)")
fig11y


################################################################################
#A.6.3. Estimate fit for incidence For second lockdown period 
#starting from 15 April 2020 to 03 May 2020 (which includes start of lockdown-3)
###############################################################################
lockdown2.fit <- fit(maha_incidence_object[38:56])
lockdown2.fit
#plot(maha_incidence_object[38:56]) %>% add_incidence_fit(lockdown2.fit)

fig11z <- plot(maha_incidence_object[38:56], color = "blue", border = "white") %>% add_incidence_fit(lockdown2.fit)+
  labs(title="Observed and modelled incidence of COVID-19 cases in Maharashtra-India",
       subtitle="15 April 2020 to 03 May 2020 (lockdown-2)")
fig11z

################################################################################
#A.6.4. Estimate fit for incidence For third lockdown period 
#starting from 04 May 2020 to 17 May 2020 (which includes start of lockdown-3)
###############################################################################

lockdown3.fit <- fit(maha_incidence_object[57:70])
lockdown3.fit
#plot(maha_incidence_object[57:70]) %>% add_incidence_fit(lockdown3.fit)

fig11xy <- plot(maha_incidence_object[57:70], color = "blue", border = "white") %>% add_incidence_fit(lockdown3.fit)+
  labs(title="Observed and modelled incidence of COVID-19 cases in Maharashtra-India",
       subtitle="04 May 2020 to 17 May 2020 (lockdown-3)")
fig11xy


################################################################################
#A.6.5. Estimate fit for incidence For fourth lockdown period 
#starting from 18 May 2020 to 31 May 2020 (which includes start of lockdown-3)
###############################################################################

lockdown4.fit <- fit(maha_incidence_object[71:84])
lockdown4.fit
#plot(maha_incidence_object[71:84]) %>% add_incidence_fit(lockdown4.fit)

fig11xz <- plot(maha_incidence_object[71:84], color = "blue", border = "white") %>% add_incidence_fit(lockdown4.fit)+
  labs(title="Observed and modelled incidence of COVID-19 cases in Maharashtra-India",
       subtitle="18 May 2020 to 31 May 2020 (lockdown-4)")
fig11xz


#######################################################################################
#A.4.6.Estimate fit for entire period with all breakdowns (9 march 2020 to 17 May 2020)
#######################################################################################

#this plot is for entire duration
plot(maha_incidence_object) %>% add_incidence_fit(lockdown0.fit) %>% 
  add_incidence_fit(lockdown1.fit)  %>% 
  add_incidence_fit(lockdown2.fit) %>% 
  add_incidence_fit(lockdown3.fit) %>% 
  add_incidence_fit(lockdown4.fit)


lock1startdate <- as.Date("2020-03-25") #lockdown-1 start date  (by mistake I considered it 24th March in previous files)
lock1enddate <- as.Date("2020-04-14") #lockdown-1 end date
lock2enddate <- as.Date("2020-05-03") #lockdown-2 end date
lock3enddate <- as.Date("2020-05-17") #lockdown-3 end date
lock4enddate <- as.Date("2020-05-31") #lockdown-4 end date

fig12 <- plot(maha_incidence_object, color = "blue", border ="white") %>% add_incidence_fit(lockdown0.fit)  %>% 
  add_incidence_fit(lockdown1.fit)  %>% 
  add_incidence_fit(lockdown2.fit) %>% 
  add_incidence_fit(lockdown3.fit) %>% 
  add_incidence_fit(lockdown4.fit)+
  geom_vline(xintercept = c(lock1startdate,lock1enddate,lock2enddate,lock3enddate,lock4enddate), col = "red", lty = 2) +
  scale_x_date("Date",
               breaks = c(seq(from = as.Date("2020-03-09"), to = as.Date("2020-05-31"), by ="15 day"),
                          as.Date(c(lock1startdate,lock1enddate,lock2enddate,lock3enddate,lock4enddate), origin="2020-01-01")),
               labels= c("2020-03-09", "", "2020-04-09", 
                         "2020-04-23", "2020-05-09", "2020-05-31", 
                         "Begin Lock Down-1", "End Lock Down-1", 
                         "End Lock Down-2", "End Lock Down-3",
                         "End Lock Down-4"))+
  scale_y_continuous("Daily Incidence", 
                     breaks= c(0,100,500,1000,1500,2000,2500,3000))+
                    #limits=c(0,3100))+
  labs(title="Observed and modelled incidence of COVID-19 cases in Maharashtra-India \nfor different phases of lockdown (9 March - 31 May 2020)",
       subtitle="(red lines indicate different phases of lockdown)")+
  theme(axis.text.x = element_text(angle=27),
        #panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

fig12

ggsave(filename = "12_MH_daily_incidence_with_five_linearfit_plot_manuscript.tiff") 

invisible(dev.off())

#zooming on certain sections of the graph

#library(ggforce)
fig12x <- fig12 +
  ggforce::facet_zoom(xlim = c(as.Date("2020-03-09"),as.Date("2020-03-31")), ylim = c(0, 100), horizontal = FALSE) +
theme(zoom.y = element_blank(), validate = FALSE)

fig12x

ggsave(filename = "12x_Zoomed_MH_daily_incidence_with_five_linearfit_plot_manuscript.tiff") 

invisible(dev.off())


##############################################################
##B.Estimating the reproduction number from log-linear model##
##############################################################

#install.packages("distcrete")
#install.packages("epitrix")

library("distcrete")
library("epitrix")

#??epitrix


#####################################
# serial interval estimates are from:
#####################################
# I used mu=3.9 days , SD=2.85 from Rajendrakumar etal(2020)-Epidemic Landscape and Forecasting of SARS-CoV-2 in India
#Link-https://www.medrxiv.org/content/10.1101/2020.04.14.20065151v1.full.pdf

mu <- 3.9 # this is from Rajendrakumar etal(2020)
sigma <- 2.85  # this is from Rajendrakumar etal(2020)
param <- gamma_mucv2shapescale(mu, sigma / mu)  #used epitrix package here to  get shape and scale. https://www.repidemicsconsortium.org/projections/
w <- distcrete("gamma", interval = 1,
                shape = param$shape,
                scale = param$scale, w = 0)


##############################################
# B.2. estimating R0 for pre lockdown-1 period
##############################################
set.seed(1)
loglinear_prelock1_R0 <- lm2R0_sample(lockdown0.fit$model, w, n = 1000)

summary(loglinear_prelock1_R0) #to know mean, median, min. and max
#   Min.   1st Qu.  Median  Mean    3rd Qu.  Max. 
#  0.5656  1.0410  1.1374  1.1392  1.2338  1.9085

quantile(loglinear_prelock1_R0, c(0.025, 0.975)) # to know 95% credibility interval
#95% CI: 0.847874 1.451982

hist(loglinear_prelock1_R0, col = "blue", border = "white", 
     main = NULL,
     xlab = NULL,
     ylab = NULL)
fig14 <- recordPlot() 
invisible(dev.off())

fig14


##########################################
# B.3. estimating R0 for lockdown-1 period
##########################################
set.seed(1)
loglinear_lock1_R0 <- lm2R0_sample(lockdown1.fit$model, w, n = 1000)

summary(loglinear_lock1_R0) #to know mean, median, min. and max
# Min.    1st Qu. Median  Mean   3rd Qu.   Max. 
# 1.351   1.611   1.666   1.665   1.720   2.044


quantile(loglinear_lock1_R0, c(0.025, 0.975)) # to know 95% credibility interval
#95% CI: 1.501844 1.821984

hist(loglinear_lock1_R0, col = "blue", border = "white", 
     main = NULL,
     xlab = NULL,
     ylab = NULL)


fig15 <- recordPlot() 
invisible(dev.off())

fig15


##########################################
# B.4. estimating R0 for lockdown-2 period
##########################################
set.seed(1)
loglinear_lock2_R0 <- lm2R0_sample(lockdown2.fit$model, w, n = 1000)

summary(loglinear_lock2_R0) #to know mean, median, min. and max
# Min.   1st Qu.  Median  Mean   3rd Qu.   Max. 
# 1.014   1.199   1.237   1.237   1.275   1.506 

quantile(loglinear_lock2_R0, c(0.025, 0.975)) # to know 95% credibility interval
#95% CI: 1.122330 1.347608

hist(loglinear_lock2_R0, col = "blue", border = "white", 
     main = NULL,
     xlab = NULL,
     ylab = NULL)

fig16 <- recordPlot() 
invisible(dev.off())

fig16

##########################################
# B.5. estimating R0 for lockdown-3 period
##########################################
set.seed(1)
loglinear_lock3_R0 <- lm2R0_sample(lockdown3.fit$model, w, n = 1000)

summary(loglinear_lock3_R0) #to know mean, median, min. and max
# Min.    1st Qu.  Median  Mean   3rd Qu.  Max. 
# 0.895   1.084   1.118   1.118   1.153   1.389

quantile(loglinear_lock3_R0, c(0.025, 0.975)) # to know 95% credibility interval
#95% CI: 1.008143 1.231665 

hist(loglinear_lock3_R0, col = "blue", border = "white", 
     main = NULL,
     xlab = NULL,
     ylab = NULL #, xaxt ='n' # to remove numbering on x-axis
    )

fig17 <- recordPlot() 
invisible(dev.off())

fig17

##########################################
# B.6. estimating R0 for lockdown-4 period
##########################################
set.seed(1)
loglinear_lock4_R0 <- lm2R0_sample(lockdown4.fit$model, w, n = 1000)

summary(loglinear_lock4_R0) #to know mean, median, min. and max
# Min.    1st Qu.  Median  Mean   3rd Qu.  Max. 
# 0.9183  1.0318  1.0522  1.0521  1.0727  1.2108

quantile(loglinear_lock4_R0, c(0.025, 0.975)) # to know 95% credibility interval
#95% CI: 0.9865754 1.1188902 

hist(loglinear_lock4_R0, col = "blue", border = "white", 
     main = NULL,
     xlab = NULL,
     ylab = NULL)


fig18 <- recordPlot() 
invisible(dev.off())

fig18


#################################################################################################
#C.Estimating current effective Re for lockdown phases, day-to-day basis and overall infectivity
#################################################################################################
#install.packages("EpiEstim")
#data(Flu2009)
#str(Flu2009)
#head(Flu2009)

library("EpiEstim")
#??EpiEstim

#################################################################################
#C.4. Incorporating uncertainty around Serial interval and estimating daily (Re)
#################################################################################

#following start and end date setup will help to get daily instantaneous Re value
t_start <- seq(5, length(mhinci$I) - 1) #starting from day-2 will get daily Re value #five day sliding window: get this
t_end <- t_start + 1

set.seed(1) # this prevents changes in estimates by running same sample
res_daily_uncertain <- estimate_R(mhinci, 
                                  method="uncertain_si",
                                  config = make_config(list(
                                  t_start = t_start,
                                  t_end = t_end,
                                  mean_si = 3.9, std_mean_si = 1,
                                  min_mean_si = 2.97, max_mean_si = 7.5,
                                  std_si = 2.85, std_std_si = 0.5,
                                  min_std_si = 2, max_std_si = 10.9,
                                  n1 = 1000, n2 = 1000))) 

# to aovid error in bulck code run do following
try(log("not a number"), silent = TRUE)
print("Errors cant stop me")


res_daily_uncertain


plot(res_daily_uncertain, "R") +
  geom_hline(aes(yintercept = 1), color = "red", lty = 2) +
  xlab("Time (days)") +
  ylab("Reproduction number (Re)") +
  labs(title="Time varying reproduction number (Re) for COVID-19 in Maharashtra, India",
       subtitle = "(9 March-31 May 2020)",
       caption = "Note: Re is estimated using moving window of five days")    


fig24 <- recordPlot() 
invisible(dev.off())

fig24


ggsave(filename = "24_Epiestim_daily_R0_breakup_uncertain_5daylsiding_plot_manuscript.tiff")


#####################
#D.Making Projections
#####################

#lockdown dates reference:
#https://timesofindia.indiatimes.com/travel/destinations/india-lockdown-3-0-nationwide-lockdown-extends-till-may-17-parts-of-country-will-open-but-no-rail-road-air-travel-for-now/as75493510.cms
#data start day-2020-03-09(1st day)
#first lockdown start-2020-03-25 (17th day from 2020-03-09)
#second lockdown start-2020-04-15 (38th day from 2020-03-09)
#third lockdown start-2020-05-04 (57th day from 2020-03-09)
#fourth lockdown start- 2020-05-18 (71th day from 2020-03-09)
#fourth lockdown end i.e. data end date- 2020-05-31(84th dat from 2020-03-09)
#next 30 day date = 2020-06-30 ie 30 June 2020 (total 114 days)

#making projections  before the peak on "2020-05-04" to test if model is correct
#install.packages("projections")

library("projections")
#?project

##########################################################################################
#D.3. Project future cases upto 31 May 2020 based on prelockdown and lockdown1 combined R0
##########################################################################################

#D.3.A1. To get this first get the loglinear R0 for pre-lockdown and lockdown-1 period combined
set.seed(1)
lockdown01.fit <- fit(maha_incidence_object[1:37]) #1-37 days= prelockdown+lockdown-1 period
lockdown01.fit
#plot(maha_incidence_object[71:84]) %>% add_incidence_fit(lockdown01.fit)



figlock01fit <- plot(maha_incidence_object[1:37], color = "blue", border = "white") %>% add_incidence_fit(lockdown01.fit)+
  labs(title="Observed and modelled incidence of COVID-19 cases in Maharashtra-India",
       subtitle="09 March 2020 to 14 April 2020 (pre-lcokdown & lockdown-1)")

figlock01fit


#D.3.A2. serial interval estimates are from:

#Here I used mu=3.9 days , SD=2.85 from Rajendrakumar etal(2020)-Epidemic Landscape and Forecasting of SARS-CoV-2 in India
#Link-https://www.medrxiv.org/content/10.1101/2020.04.14.20065151v1.full.pdf

mu2 <- 3.9 # this is from Rajendrakumar etal(2020) 
sigma2 <- 2.85  # this is from Rajendrakumar etal(2020)
param2 <- gamma_mucv2shapescale(mu2, sigma2 / mu2)  #used epitrix package here to  get shape and scale. https://www.repidemicsconsortium.org/projections/
w2 <- distcrete("gamma", interval = 1,
               shape = param2$shape,
               scale = param2$scale, w = 0)



#D.3.A3. Now  estimating log-linear Re for pre and lockdown-1 period combined
set.seed(1)
loglinear_lock01_R0 <- lm2R0_sample(lockdown01.fit$model, w2, n = 1000)


summary(loglinear_lock01_R0)
#Results:
#Min.   1st Qu.  Median   Mean   3rd Qu.  Max. 
#1.313   1.443   1.473   1.473   1.503   1.606 


quantile(loglinear_lock01_R0, c(0.025, 0.975)) # to know 95% credibility interval
#95% CI: 1.385960 1.556861

hist(loglinear_lock01_R0, col = "blue", border = "white", main = "Distribution of R0")




#############
#Projections
#############

#D.3.A4. Test projection based on R0 for pre-and=lockdown-1 period
pred_fwd_days <- 84 # substracting total number of days from date rane object allows the projection run for first 37 days of the data
date_range <- which(get_dates(maha_incidence_object) == as.Date("2020-05-31")):(length(get_dates(maha_incidence_object)) - pred_fwd_days)

set.seed(1)
test_pred_pre_andlock1 <- project(maha_incidence_object[date_range],
                           R = median(loglinear_lock01_R0),  # Re for last phase of lockdown used to make projections for coming 30 days.
                           si = w2,
                           n_days = 37, n_sim = 1000) # we want next 37 day projections ie. total days of pre-and-lockdown-1 period


test_pred_pre_andlock1_median_counts <- test_pred_pre_andlock1 %>% 
  as.data.frame() %>%
  pivot_longer(-dates, 
               names_to="simulation", 
               values_to="incidence") %>%
  group_by(dates) %>%
  summarise(incident_cases=as.integer(median(incidence))) %>%
  mutate(data_type = "projection")


test_pred_pre_andlock1_median_counts %>%
  bind_rows(tibble(dates=get_dates(maha_incidence_object),
                   incident_cases=get_counts(maha_incidence_object),
                   data_type="observed")) %>%
  ggplot(aes(x=dates, y=incident_cases, colour=data_type)) +
  geom_point() +
  geom_line() +
  labs(x="", y="Daily incident confirmed cases",
       title=" First 37 Day Projection of incident cases in Maharashtra",
       subtitle=paste("[projection based on observed case counts \nfrom 09 March 2020 to 14 April 2020]")) +
  theme(legend.position="top", legend.title = element_blank())

fig28a <- recordPlot() 
invisible(dev.off())

fig28a

ggsave(filename = "28a_Projection_test_Upto_14April2020_plot.tiff")

invisible(dev.off())


#D.3.A5. Projecting cases for remaining period (lockdown2,3&4) based on log-linear Re from pre-and-lockdonw-1 period
set.seed(1)
pred_lock01 <- project(maha_incidence_object[1:37], #corresponds to lockdown1 period
                      R = median(loglinear_lock01_R0),
                      si = w2,
                      n_days = 47, n_sim = 1000) #this is total no. of days from beginning of lockdown-2 (38th day) upto the availability of data ie 31 May 2020 (84th day)


pred_lock01_median_counts <- pred_lock01 %>% 
  as.data.frame() %>%
  pivot_longer(-dates, 
               names_to="simulation", 
               values_to="incidence") %>%
  group_by(dates) %>%
  summarise(incident_cases=as.integer(median(incidence))) %>%
  mutate(data_type = "projection")


#Plotting below
pred_lock01_median_counts %>%
  bind_rows(tibble(dates=get_dates(maha_incidence_object),
                   incident_cases=get_counts(maha_incidence_object),
                   data_type="observed")) %>%
  ggplot(aes(x=dates, y=incident_cases, colour=data_type)) +
  geom_point() +
  geom_line() +
  #scale_y_continuous(breaks = c(0, 1000, 2000, 3000, 5000, 10000, 20000, 30000, 40000, 50000, 60000)) +
  labs(x="", y="Daily incident confirmed cases",
       title="Daily observed vs projected COVID-19 incident cases \nin Maharashtra-India",
       caption = "Note: projection is based on observed case counts from 09 March to 14 April 2020",   
       subtitle=paste("(from 9 March to 31 May 2020)")) +
  theme(legend.position="top", legend.title = element_blank())

fig28b <- recordPlot() 
invisible(dev.off())

fig28b

ggsave(filename = "28b_Projection_Upto_31May2020_basedonprelockdown1R0_plot.tiff")

invisible(dev.off())

#rm(list=setdiff(ls(), "x")) remove all but "x" object from R environment



#zooming on certain sections of the graph

#library(ggforce)


fig28c <- pred_lock01_median_counts %>%
  bind_rows(tibble(dates=get_dates(maha_incidence_object),
                   incident_cases=get_counts(maha_incidence_object),
                   data_type="observed")) %>%
  ggplot(aes(x=dates, y=incident_cases, colour=data_type)) +
  geom_point() +
  geom_line() +
  #scale_y_continuous(breaks = c(0, 1000, 2000, 3000, 5000, 10000, 20000, 30000, 40000, 50000, 60000)) +
  labs(x="", y="Daily incident confirmed cases",
       title="Daily observed vs projected COVID-19 incident cases in Maharashtra-India",
       caption = "Note: projection is based on observed case counts from 09 March to 14 April 2020",   
       subtitle=paste("(from 9 March to 31 May 2020)")) +
  theme(legend.position="top", legend.title = element_blank())



fig28x <- fig28c +
  ggforce::facet_zoom(xlim = c(as.Date("2020-04-14"),as.Date("2020-05-31")), ylim = c(0, 3000), horizontal = FALSE) +
  theme(zoom.y = element_blank(), validate = FALSE)

fig28x


ggsave(filename = "28x_ZoomProjection_Upto_31May2020_basedonprelockdown1R0_plot.tiff")

invisible(dev.off())

#############################################################################################################################################
######################################################################################################################################
######################################################################################################################################


##############################
#exporting data into csv file
##############################

## 4. Take difference of projected-actual cases

#4a. merging projected data column with original incident data
head(pred_lock01_median_counts)
colnames(pred_lock01_median_counts)[1] <- "Date"
colnames(pred_lock01_median_counts)[2] <- "Projected"

mhinci_lock01_predict_original <- merge.data.frame(mhinci,pred_lock01_median_counts, by.x = "Date", by.y = "Date", all = TRUE)

head(mhinci_lock01_predict_original)

tail(mhinci_lock01_predict_original)

#merge deceased column
mhinci_lock01_predict_original <- merge.data.frame(mhinci_lock01_predict_original,mhdeceased, by.x = "Date", by.y = "Date", all = TRUE)

head(mhinci_lock01_predict_original)

tail(mhinci_lock01_predict_original)

#merge recovered column
mhinci_lock01_predict_original <- merge.data.frame(mhinci_lock01_predict_original,mhrecov, by.x = "Date", by.y = "Date", all = TRUE)
head(mhinci_lock01_predict_original)

tail(mhinci_lock01_predict_original)

mhinci_lock01_predict_original$data_type <- NULL  # deleting unnecessary column

head(mhinci_lock01_predict_original)

tail(mhinci_lock01_predict_original)

#4b. add difference in incidences column (Difference = Projected-Actual)
mhinci_lock01_predict_original$proj_diff <- (mhinci_lock01_predict_original$Projected - mhinci_lock01_predict_original$I)

head(mhinci_lock01_predict_original)

tail(mhinci_lock01_predict_original)

cumlock01_proj_actual <- colSums(mhinci_lock01_predict_original[38:84, c(2,3,4,5,6)], na.rm = TRUE)  ## taking column summaries only for projected dates

cumlock01_proj_actual  # I=64975, projected= 631891, difference= 566916, D = 2108, R = 29070

#Note Ghosh etal(2020)-COVID-19 in India: Statewise Analysis and Prediction study show that in severe condition, the cumulative cases in Maharashtra could 
# have reached to 196,103. My estimates show that daily new cases could have reached to 67,889 cases if virus would have allowed to spread unrestrictedly. And 
# total new cases could have reached to 631,891 new cases during the projected period (15 April to 31 May 2021)




################################
#Working on the predicted values
################################

# Add column for hospitalization rates

# predicted_data <- tibble(test_pred_30day_median_counts)
# head(predicted_data)
# 
# predicted_data[, c("hospital_rates", "death_rates", "icu_rates", "los_hosp", "los_icu",
#                    "hospital_cost_pd", "icu_costpd", "incub_period", "contact_rate_pd", "total_infections",
#                    "avg_daily_wage", "avg_life_exp", "avg_deathage", "avg_daymissed", "avg_yrs_lost", "admin_expenditure")] <- 0
# 
# head(predicted_data)
# 


#predicted_data = predicted_data[c("dates", "incident_cases", "data_type")] # for deleting column-hospital_rates


#Saving median values of projected incidence in csv file
write.csv(mhinci_lock01_predict_original,
          "1_incidence_projectiond.csv",
      row.names = FALSE)

#to run entire script press: cmd+shift+enter
#shortcut to source: cmd+shift+s

quit(save = "no", status = 0, runLast = TRUE)




