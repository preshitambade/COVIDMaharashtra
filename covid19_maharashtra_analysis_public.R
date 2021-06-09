#############################################################################
#DATA RETRIEVAL FROM COVID19.ORG FOR INDIA AND ANALYSIS USING RECON LIBRARIES
#############################################################################
# Project Name: Data retrieval from COVID19.ORG for Maharashtra and analysis using RECON libraries
# Creater: Preshit Ambade
# Date: 25 November 2020
#links to follow:
#https://api.covid19india.org
#https://api.covid19india.org/csv/
#https://timchurches.github.io/blog/posts/2020-02-18-analysing-covid-19-2019-ncov-outbreak-data-with-r-part-1/
#https://timchurches.github.io/blog/posts/2020-03-01-analysing-covid-19-2019-ncov-outbreak-data-with-r-part-2/

## Open following links in browser
##https://timchurches.github.io/blog/posts/2020-02-18-analysing-covid-19-2019-ncov-outbreak-data-with-r-part-1/
##https://www.repidemicsconsortium.org/projects/
##http://statnet.org/tut/NewDCMs.html
##https://publons.com/publon/covid-19/?title=PPE%20per%20patient&sort_by=relevance
##https://mathbitsnotebook.com/Algebra2/Exponential/EXGrowthDecay.html
##https://twitter.com/maha_medd?lang=en
##https://github.com/timchurches/blog/blob/master/_posts/2020-02-18-analysing-covid-19-2019-ncov-outbreak-data-with-r-part-1/analysing-covid-19-2019-ncov-outbreak-data-with-r-part-1.Rmd
##https://api.covid19india.org/csv/
##https://api.covid19india.org
##https://timchurches.github.io/blog/posts/2020-03-18-modelling-the-effects-of-public-health-interventions-on-covid-19-transmission-part-2/
##https://timchurches.github.io/blog/posts/2020-03-10-modelling-the-effects-of-public-health-interventions-on-covid-19-transmission-part-1/
##https://github.com/timchurches/blog/blob/master/_posts/2020-03-01-analysing-covid-19-2019-ncov-outbreak-data-with-r-part-2/analysing-covid-19-2019-ncov-outbreak-data-with-r-part-2.Rmd
##https://timchurches.github.io/blog/posts/2020-03-01-analysing-covid-19-2019-ncov-outbreak-data-with-r-part-2/
##https://timchurches.github.io/blog/posts/2020-02-18-analysing-covid-19-2019-ncov-outbreak-data-with-r-part-1/
##https://timchurches.github.io/blog/
##https://rviews.rstudio.com/2020/03/05/covid-19-epidemiology-with-r/
##https://rviews.rstudio.com/2020/03/19/simulating-covid-19-interventions-with-r/
#################################################################################################################################

# analysis for entire maharashtra state

rm(list = ls()) #Clears everything
# install.packages(c("distcrete", "earlyR", "EpiEstim", "epitrix", "ggpubr", "gridGraphics", "incidence", "outbreaks", "projections"))


# #download state_wise_daily data
# file_state_wise_daily <- paste0("/Users/preshitambade/Downloads/Corona-CEA/1-COVID-19_CEA_study/2-COVID-19-Model/5-Epimodel_demo/1_epimodel_data/7_other_rawfiles/raw_state_wise_daily_", 
#                                 format(Sys.time(), "%Y-%m-%d-%H-%M-%S-AZMST", tz = "MST"), ".csv")                   
# 
# 
# download.file(url='https://api.covid19india.org/csv/latest/state_wise_daily.csv',
#               destfile=file_state_wise_daily, method='curl', overwrite = FALSE)
# 
# library(readr)
# statedf<-read.csv(file_state_wise_daily,
#                   header = T, na.strings=c("","NA"))  #this file is manually created. blank cells in string variable changed to NA

library(readr)
statedf<-read.csv("/Users/preshitambade/Downloads/Corona-CEA/1-COVID-19_CEA_study/2-COVID-19-Model/5-Epimodel_demo/1_epimodel_data/7_other_rawfiles/raw_state_wise_daily_2020-06-01-08-33-52-AZMST.csv",
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


# to add to the bottom use following
# mahadata[nrow(mahadata) +1, ] <- c("13-Mar-20", "Confirmed", 11)
# mahadata[nrow(mahadata) +1, ] <- c("12-Mar-20", "Confirmed", 0)
# mahadata[nrow(mahadata) +1, ] <- c("11-Mar-20", "Confirmed", 11)
# mahadata[nrow(mahadata) +1, ] <- c("10-Mar-20", "Confirmed", 4)
# mahadata[nrow(mahadata) +1, ] <- c("09-Mar-20", "Confirmed", 3)
# tail(mahadata,10)  #cross-check of rows are added

#converting class of variables to its originals
# mahadata$Date <- as.factor(mahadata$Date)
# mahadata$Status <- as.factor(mahadata$Status)
# mahadata$MH <- as.numeric(mahadata$MH)

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

# #reordering of data according to descending dates- 
# #link: https://stackoverflow.com/questions/6246159/how-to-sort-a-data-frame-by-date
# 
# mahadata <- mahadata[nrow(mahadata):1,]
# 
# head(mahadata,10)

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
plot_save <- paste0("/Users/preshitambade/Downloads/Corona-CEA/1-COVID-19_CEA_study/2-COVID-19-Model/5-Epimodel_demo/2_epimodel_results/7_epimodel_results_formanuscript_8Dec2020/")     

#1a. creating and plotting incidence object
maha_incidence_object <- as.incidence(x = mhinci[, 2], dates = mhinci$Date)

maha_incidence_object 



fig1<- plot(maha_incidence_object, color= "blue", border = "white" )+
  labs(title="Daily incidence of lab-confirmed cases, Maharashtra-India",
       subtitle = "(9 March-31 May 2020)")
fig1

ggsave(path = plot_save, 
       filename = "1_MH_incidenceplot.tiff") 

dev.off()  #removes plot from plot window          

#1b. Creating and plotting incidence object for all outcomes
str(mahadata_status_uncount)
head(mahadata_status_uncount)
tail(mahadata_status_uncount)

maha_incidence_status_object <- incidence(mahadata_status_uncount$Date, interval = 1, groups = mahadata_status_uncount$Status )

maha_incidence_status_object

fig2 <- plot(maha_incidence_status_object, stack = TRUE, border = "grey")+
  labs(title="Daily status of lab-confirmed cases, Maharashtra-India",
       subtitle = "(9 March-31 May 2020)")            

fig2

#this figure will go in manuscript
ggsave(path = plot_save, 
       filename = "2_MH_incidenceplot_manuscript.tiff") 


#1f. plotting cumulative incidence

#1f1. for incidence data
maha_incidence_cum <- cumulate(maha_incidence_object)

maha_incidence_cum

fig5 <- plot(maha_incidence_cum, color = "blue", border = "black")+
  labs(title="Cumulative incidence of lab-confirmed cases, Maharashtra-India",
       subtitle = "(9 March-31 May 2020)")

fig5

ggsave(path = plot_save, 
       filename = "5_MH_cumulative_incidenceplot_manuscript.tiff") 



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


#################################################
#A.2. Fitting log-linear model to incidence data
#################################################

#A.2.1. getting linear fit
incidence.fit <- fit(maha_incidence_object) #fitted model for entire time period
incidence.fit

plot(incidence.fit)

fig8 <- plot(maha_incidence_object, fit = incidence.fit, color = "blue", border = "white")+
  labs(title="Daily incidence of lab-confirmed cases and log-linear fit model, Maharashtra-India",
       subtitle = "(As of 17 May 2020)")

fig8

#Result: Daily Growth Rate: 0.08499638 (CI: 0.07882796 0.09116481)
# Case doubling time in days: 8.15502 (CI: 7.603231 8.793164)
# these estimates are for entire study period which doesn't capture the
# effect of different restrictions imposed/relaxed during different
# phases of lockdown.

# ggsave(path = plot_save, 
#        filename = "8_MH_dailyincidence_with_linearfit_plot.tiff") 
# 

#############################################################################################################
#Note: if you see growth and decline of cases in data then you can fit two separate models as follows:
#one way is to find optimal break point (typically around peak) by using optimal split function
#Another ways is find peak of incidence manually and break data around it
#However, the cases in MH are in growth phase so no need to fit separate models for growth and decay phase

# Inspite of lockdown the cases are rising in maharashtra but is the infectivity as well?
#Estimate R0 for three different phases in the state and answer the question in below section
#############################################################################################################


####################################################################################
#A.3. fitting data upto first lockdown period by splitting it by start of lockdown-1
####################################################################################
locksplit_lock1period <- fit(maha_incidence_object[1:37], split = as.Date("2020-03-24"), level = 0.95, quiet = FALSE)

locksplit_lock1period 


fig9 <- plot(maha_incidence_object[1:37], color = "gray", border = "white") %>% add_incidence_fit(locksplit_lock1period)+
  geom_vline(xintercept = lock1startdate, col = "red", lty = 2) +
  labs(title="Observed and modelled incidence of COVID-19 cases in Maharashtra-India \nfor period 9 March-14 April 2020",
       subtitle="(red line indicates lockdown-1 start date 25 March 2020)") +
  theme_bw()

fig9

# Result: It is clear from the locksplit plot graph that before start of lockdown and after have two separate growth rates.
# Before: daily growth rate: 0.03898104 CI(-0.04563312 0.1235952), doubling time(17.781647), CI (5.608204 -15.189565)
# After:  daily growth rate: 0.17954538 CI(0.14270789 0.2163829), doubling time(3.860568), CI (3.203337   4.857105)
# Clearly, the lockdown has made difference.Need to fit data separately for these two periods


##################################
#NEED TO WORK FROM HERE AND BELOW#
##################################
#Look at the daily Re during the lockdown phase and see on what date the Re started declining. The high Re, does it 
# matches with the Vande Bharat and other activities during the same time period?
# for projections can you use lockdown-1 Re to project cases for lockdown2 and 3? Dr. Pakhale suggested you can do that.



#Also data can be fit by splitting it at first lockdown end date-but this don't have any usefulness for analysis
##########################################################################################
#A.4. splitting data at the end of first lockdown (14th April 2020) for entire time period
##########################################################################################
locksplit <- fit(maha_incidence_object, split = as.Date("2020-04-14"), level = 0.95, quiet = FALSE)

locksplit


fig10 <- plot(maha_incidence_object, color = "gray", border = "white") %>% add_incidence_fit(locksplit)+
  geom_vline(xintercept = lock1enddate, col = "red", lty = 2) +
  labs(title="Observed and modelled incidence of COVID-19 cases in Maharashtra-India \nbefore and after 15 April 2020",
       subtitle="(red line indicates lockdown-1 end date 14 April 2020)") +
  theme_bw()

fig10

# ggsave(path = plot_save, 
#        filename = "10_MH_daily_incidence_with_split_linearfit_plot.tiff") 


# Result: It is clear from the locksplit plot graph that before start of lockdown and after have two separate growth rates.
# Before: daily growth rate: 0.1300595 CI(0.10770209, 0.15241699), doubling time(5.32946), CI (4.547703,  6.435782)
# After:  daily growth rate: 0.0526358 CI(0.04672014 0.05855146), doubling time(13.16874), CI (11.838257 14.836153)
# Clearly, the lockdown has made difference.Need to fit data separately for these two periods



############################################################################################
#A.5.Estimate fit for incidence after end of first lockdown ie 15 April 2020 to 31 May 2020
############################################################################################
nrow(mhinci)  #total days= 84

lockdownafter.fit <- fit(maha_incidence_object[38:84])

lockdownafter.fit
plot(maha_incidence_object[38:84]) %>% add_incidence_fit(lockdownafter.fit)


##########################################################################################
#A.5. Estimate fit for second and third lockdown period (from 15 April 2020 to 17 May 2020 )
#by splitting data at end of second lockdown(3rd May 2020) for period 
##########################################################################################
locksplit2 <- fit(maha_incidence_object[38:84], split = as.Date("2020-05-03"), level = 0.95, quiet = FALSE)

locksplit2

# Result: It is clear from the locksplit2 plot graph that before start of lockdown and after have two separate growth rates.
# Before: daily growth rate: 0.06686441 CI(0.03592039 0.09780843), doubling time(10.36646), CI (7.086784 19.29676)
# After:  daily growth rate: 0.03972636 CI(0.03067725 0.04877547), doubling time(17.44804), CI (14.210978 22.59483)
# Need to fit data separately for these two periods

plot(maha_incidence_object) %>% add_incidence_fit(locksplit2)


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

# ggsave(path = plot_save, 
#        filename = "11x_MH_daily_incidence_beforelock1_linearfit_plot.tiff") 

#Result: Daily Growth Rate: 0.03898104 (CI: -0.04563312, 0.1235952)
# Case doubling time in days: 17.78165 (CI: 5.608204, -15.18956)
#Note: it is clear from above that the pre-lockdown period data do not provide
#conclusive estimates as the CIs for R0 and doubling rates crosses zero.
# Or I can say model not fitted well?



#####################################################################################
#A.6.2.Estimate fit for incidence between first lockdown ie 25 March to 14 April 2020
#####################################################################################

# in other words estimate if growth rates are different for before and after lockdown-1 period
lockdown1.fit <- fit(maha_incidence_object[17:37])

lockdown1.fit

fig11y <- plot(maha_incidence_object[17:37], color = "blue", border = "white") %>% add_incidence_fit(lockdown1.fit)+
  labs(title="Observed and modelled incidence of COVID-19 cases in Maharashtra-India",
       subtitle="24 March 2020 to 14 April 2020 (lockdown-1)")
fig11y

# ggsave(path = plot_save, 
#        filename = "11y_MH_daily_incidence_duringlock1_linearfit_plot.tiff") 

#Result: Daily Growth Rate: 0.1792707 (CI: 0.1386052 0.2199363)
# Case doubling time in days: 3.866482 (CI: 3.151581 5.000876)
#Note: it is clear from above that the during the lockdown period the infection was actually started
# and case doubling rate was higher.



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

#Result: Daily Growth Rate: 0.06686441 (CI: 0.03592039 0.09780843)
# Case doubling time in days: 10.36646 (CI: 7.086784 19.29676)
# It is clear that the effect of lockdown-1 was only felt during lockdown-2 as 
# case growth rates were decreased in this time and case doubling time increased to 10 days.

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


#Result: Daily Growth Rate: 0.03412093 (CI: 0.003262453 0.06497941)
# Case doubling time in days: 20.31443 (CI: 10.66718 212.4619)
# It is clear that the lockdown-1 and 2 had further effect and case doubling rate 
# increased to 20 days for the third lockdown period.


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


#Result: Daily Growth Rate: 0.01521231 (CI: -0.0033892 0.03381381)
# Case doubling time in days: 45.5649 (CI: 20.49894 -204.5165)
# It is clear that the lockdown-4 had further effect and case doubling rate 
# increased to 45 days for the third lockdown period.



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

ggsave(path = plot_save, 
       filename = "12_MH_daily_incidence_with_five_linearfit_plot_manuscript.tiff") 

ggsave(path = plot_save, 
       filename = "12_MH_daily_incidence_with_five_linearfit_plot_manuscript.jpeg") 

invisible(dev.off())

#zooming on certain sections of the graph
#source: Using ggplot2, can I insert a break in the axis?
#link: https://stackoverflow.com/questions/7194688/using-ggplot2-can-i-insert-a-break-in-the-axis
#link: https://rdrr.io/cran/ggforce/man/facet_zoom.html
#link: https://stackoverflow.com/questions/52665619/how-to-change-the-position-of-the-zoomed-area-from-facet-zoom
#link: https://www.data-imaginist.com/2019/the-ggforce-awakens-again/


#library(ggforce)
fig12x <- fig12 +
  ggforce::facet_zoom(xlim = c(as.Date("2020-03-09"),as.Date("2020-03-31")), ylim = c(0, 100), horizontal = FALSE) +
theme(zoom.y = element_blank(), validate = FALSE)

fig12x

ggsave(path = plot_save, 
       filename = "12x_Zoomed_MH_daily_incidence_with_five_linearfit_plot_manuscript.tiff") 

ggsave(path = plot_save, 
       filename = "12x_Zoomed_MH_daily_incidence_with_five_linearfit_plot_manuscript.jpeg") 

invisible(dev.off())

# how to additional x-axis date ticks?
# link- https://stackoverflow.com/questions/19287096/manually-adding-labels-to-axis-on-a-date-sclaed-variable


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

#Another study: Saurabh etal(2020)	Serial interval, basic reproduction number and prediction of COVID-19 epidemic size in Jodhpur, India
# Mu = 6.75 days, SD = 3.76 days
#link-https://www.medrxiv.org/content/10.1101/2020.07.03.20146167v1.full.pdf


mu <- 3.9 # this is from Rajendrakumar etal(2020) previously used Saurabh etal(202)=6.75
sigma <- 2.85  # this is from Rajendrakumar etal(2020) previously used Saurabh etal(202)=3.76
param <- gamma_mucv2shapescale(mu, sigma / mu)
w <- distcrete("gamma", interval = 1,
                shape = param$shape,
                scale = param$scale, w = 0)


#########################################################
# B.1. Estimating R0 for overall fit (i.e. entire period)
#########################################################
# Note: Assuming linear fit for all data will give wrong estimates.
# It is clear from locksplit graph that we have two separate growth rates before and after lockdown
# But do following estimation for check

set.seed(1) # this prevents changes in estimates by running same sample
loglinear_R0 <- lm2R0_sample(incidence.fit$model, w, n = 1000)

summary(loglinear_R0) #to know mean, median, min. and max
# Min.   1st Qu.  Median  Mean   3rd Qu.    Max. 
# 1.262   1.296   1.304   1.304   1.311   1.340 

quantile(loglinear_R0, c(0.025, 0.975)) # to know 95% credibility interval
#95% CI: 1.280140, 1.326397

hist(loglinear_R0, col = "blue", border = "white", main = "Distribution of R0")
fig13 <- recordPlot() 
invisible(dev.off())

fig13

# ggsave(path = plot_save, 
#        filename = "13_epitrix_loglinear_R0_plot.tiff") 

#Conclusion: Mean R0 for entire study period remained 1.304 (95% CI: 1.280140, 1.326397 )

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

# ggsave(path = plot_save, 
#        filename = "14_epitrix_loglinear_beforelock1_R0_plot.tiff") 


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

# ggsave(path = plot_save, 
#        filename = "15_epitrix_loglinear_lock1_R0_plot.tiff") 



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

# ggsave(path = plot_save, 
#        filename = "16_epitrix_loglinear_lock2_R0_plot.tiff") 



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

# ggsave(path = plot_save, 
#        filename = "17_epitrix_loglinear_lock3_R0_plot.tiff") 




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

# ggsave(path = plot_save, 
#        filename = "18_epitrix_loglinear_lock4_R0_plot.tiff") 


#Generating one common plot for all R0 estimates & annotating it with common title
#Source-1: ggarrange and annotate : http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/
#Source-2: annotate_figure:  https://rpkgs.datanovia.com/ggpubr/reference/annotate_figure.html#arguments
#Source-3: ggarrange: https://rpkgs.datanovia.com/ggpubr/reference/ggarrange.html
#Source-4: Patchwork: https://patchwork.data-imaginist.com/articles/patchwork.html
#Source-5: Patchwork: https://patchwork.data-imaginist.com/articles/guides/assembly.html


#install.packages("ggpubr")
#library(ggpubr)

invisible(dev.off())
fig19a <- ggarrange(fig14, fig15, fig16, fig17, fig18, nrow = 3, ncol = 2, 
                    widths = c(1,1),
                    labels = c("Pre-Lockdown (9th-23rd March 2020)",
                               "Lockdown-1 (24th March-14th April 2020)",
                               "Lockdown-2 (15th April-3rd May 2020)", 
                               "Lockdown-3 (4th May-17th May 2020)", 
                               "Lockdown-4 (18th May-31st May 2020)"),
                    font.label = list(size = 12, color = "black", face = "plain", family = NULL),
                    hjust = -0.8,  # this adjust horizontal shift of label and brings it to the center,
                    vjust = 3,  # this adjust vertical shift of label and puts it just above the histo bars,
                    align = "hv"
)
fig19a

#Annotate Arranged Figure (annotate_figure) :
fig19 <- annotate_figure(fig19a,
                          top = text_grob("Estimated R0 for different phases of lockdown in Maharashtra \n(9 March-31 May 2020)", color = "black", face = "bold", size = 14),
                          left = text_grob("No. of samples drawn", color="black", rot= 90),
                          
)

fig19
# to aovid error in bulck code run do following
try(log("not a number"), silent = TRUE)
print("Errors cant stop me")


ggsave(path = plot_save, 
      filename = "19_epitrix_loglinear_R0_combined_plot.tiff") 

# to aovid error in bulck code run do following
try(log("not a number"), silent = TRUE)
print("Errors cant stop me")

invisible(dev.off())

#################################################################################################
#C.Estimating current effective Re for lockdown phases, day-to-day basis and overall infectivity
#################################################################################################
#install.packages("EpiEstim")
#data(Flu2009)
#str(Flu2009)
#head(Flu2009)

library("EpiEstim")
#??EpiEstim


#1. For incidence data

#renaming column name in mhinci dataframe
#install.packages("reshape")
# library(reshape)
# mhinci <-rename(mhinci, c(MH = "I"))

res_incidence_parametric_si <- estimate_R(mhinci, 
                                          method="parametric_si",
                                          config = make_config(list(
                                            mean_si = 3.9, 
                                            std_si = 2.85))) 

head(res_incidence_parametric_si)

plot(res_incidence_parametric_si, legend = FALSE)

fig20 <- recordPlot() 
invisible(dev.off())

fig20


# ggsave(path = plot_save, 
#        filename = "20_Epiestim_daily_R0_prametric_plot_manuscript.tiff") 


#######################################################
#C.1. Incorporating uncertainty around Serial interval
#######################################################

# From excel file:Serial Interval Estimates_24Nov2020;sheet(Serial_interval_final_studies): Mean serial interval:	min = 2.97	keep at 3.9 (estimated by Rajendrakumar etal.)	max = 7.5
# From excel file:Serial Interval Estimates_24Nov2020;sheet(Serial_interval_final_studies): SD for serial interval: min= 2	keep at 2.85 (estimated by Rajendrakumar etal.)	max = 10.9

#??EpiEstim
#?EpiEstim::make_config
# check ?EpiEstim::make_config to know more about the parameters
set.seed(1) # this prevents changes in estimates by running same sample
res_incidence_uncertain_si <- estimate_R(mhinci, 
                                         method="uncertain_si",
                                         config = make_config(list(
                                           mean_si = 3.9, std_mean_si = 1,
                                           min_mean_si = 2.97, max_mean_si = 7.5,
                                           std_si = 2.85, std_std_si = 0.5,
                                           min_std_si = 2, max_std_si = 10.9,
                                           n1 = 1000, n2 = 1000))) 
#above values are Mean, SD, Min, MAX values calculated in "excel sheet- serial interval estimates" based on reported values in the literature

# to aovid error in bulck code run do following
try(log("not a number"), silent = TRUE)
print("Errors cant stop me")

# Warning messages:
#   1: In check_config(config, method) :
#   The distribution you chose for the mean SI is not centered around
# the mean.
# 2: In check_config(config, method) :
#   The distribution you chose for the std of the SI is not centered 
# around the mean.


head(res_incidence_uncertain_si)

plot(res_incidence_uncertain_si, legend = FALSE)

fig21 <- recordPlot() 
invisible(dev.off())

fig21

# ggsave(path = plot_save, 
#        filename = "19_Epiestim_daily_R0_uncertain_plot_manuscript.tiff") 


tiff(file="/Users/preshitambade/Downloads/Corona-CEA/1-COVID-19_CEA_study/2-COVID-19-Model/5-Epimodel_demo/2_epimodel_results/7_epimodel_results_formanuscript_8Dec2020/21_Epiestim_daily_R0_uncertain_plot_manuscript.tiff",
units="in", width=5, height=4, res=300)
plot(res_incidence_uncertain_si, legend = FALSE)
invisible(dev.off())


#combine figure
#fig34

##########################################
#C.2. Incorporating lockdown in estimation
##########################################
#Note:check the following code and match it to the dataset

#lockdown dates reference:
#https://timesofindia.indiatimes.com/travel/destinations/india-lockdown-3-0-nationwide-lockdown-extends-till-may-17-parts-of-country-will-open-but-no-rail-road-air-travel-for-now/as75493510.cms
#data start day-2020-03-09(1st day)
#first lockdown start-2020-03-25 (17th day from 2020-03-09)
#second lockdown start-2020-04-15 (38th day from 2020-03-09)
#third lockdown start-2020-05-04 (57th day from 2020-03-09)
#fourth lockdown start- 2020-05-18 (71th day from 2020-03-09)
#fourth lockdown end i.e. data end date- 2020-05-31(84th dat from 2020-03-09)
#next 30 day date = 2020-06-30 ie 30 June 2020 (total 114 days)
head(mhinci)
tail(mhinci)

# Following code corresponds to the different phases of lockdown.
t_start <- c(2, 17, 38, 57, 71) # starting at 2 as conditional on the past observation.

t_end <- c(16, 37, 56, 70, 84) #need to keep changing last digit which is the last calendar day in dataset


#1. for incidence data
res_before_during_after_closure_parametric <- estimate_R(mhinci, 
                                                         method="parametric_si",
                                                         config = make_config(list(
                                                           t_start = t_start,
                                                           t_end = t_end,
                                                           mean_si = 3.9, 
                                                           std_si = 2.85)))

res_before_during_after_closure_parametric 


plot(res_before_during_after_closure_parametric, "R") +
  geom_hline(aes(yintercept = 1), color = "red", lty = 2)
#this R0 estimation looks correct so use only incidence data for such estimation

fig22 <- recordPlot() 
invisible(dev.off())

fig22


# ggsave(path = plot_save, 
#        filename = "20_Epiestim_lockdownphases_R0_breakup_parametric_plot_manuscript.tiff") 

#######################################################
#C.3. Incorporating uncertainty around Serial interval
#######################################################
set.seed(1) # this prevents changes in estimates by running same sample
res_before_during_after_closure_uncertain <- estimate_R(mhinci, 
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


res_before_during_after_closure_uncertain
#Note: values from this command are used in table-1 to show Re for different phases of lockdown.


plot(res_before_during_after_closure_uncertain, "R") +
  geom_hline(aes(yintercept = 1), color = "red", lty = 2)

fig23 <- recordPlot() 
invisible(dev.off())

fig23


ggsave(path = plot_save, 
       filename = "23_Epiestim_lockdownphases_R0_breakup_uncertain_plot_manuscript.tiff") 

#Note: I get following warning:
# Warning messages:
#   1: In check_config(config, method) :
#   The distribution you chose for the mean SI is not centered around
# the mean.
# 2: In check_config(config, method) :
#   The distribution you chose for the std of the SI is not centered 
# around the mean.

#Note:- all the examples given in tutorials use only daily incidence data for R0 estimation.
#So follow the path and dont use all status file for final analysis as it is not giving correct R0



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


ggsave(path = plot_save,
       filename = "24_Epiestim_daily_R0_breakup_uncertain_5daylsiding_plot_manuscript.tiff")
#Result: one day estimate for day 2 are not included due as no cases were reported for the day2.
# From figure, it is clear the that Re was high during first 30 days of pandemic in the state which was later brought down.
# So, for projection purposes as per discussion with Dr. Pakhale, I can use Re estimated from the data up to 14th April and
# project the cases for remaining days if the lockdown and compare it with the actual number of cases.


#Note: I get following warning:
# Warning messages:
#   1: In check_config(config, method) :
#   The distribution you chose for the mean SI is not centered around
# the mean.
# 2: In check_config(config, method) :
#   The distribution you chose for the std of the SI is not centered 
# around the mean.



####################################
#C.5. Estimating Overall Infectivity
####################################

##in earlyR package use the estimation asked to use overall_infectivity in EpiEstim library.
##so here overall infectivity is estimated as follows

#?overall_infectivity #this vignette have example to follow
##Ref. Cori, A. et al. A new framework and software to estimate time-varying reproduction numbers during epidemics (AJE 2013).

## compute overall infectivity
##res_incidence_parametric_si - object contains $si_distr so use it for following estimate

lambda <- overall_infectivity(mhinci$I, res_incidence_parametric_si$si_distr) 
par(mfrow=c(2,1))
plot(mhinci$I, type = "s", xlab = "time (days)", ylab = "incidence")
title(main = "Epidemic curve")
plot(lambda, type = "s", xlab = "time (days)", ylab = "Infectivity")
title(main = "Overall infectivity")

fig25 <- recordPlot() 
invisible(dev.off())

fig25

ggsave(path = plot_save,
       filename = "25_Epiestim_overall_infectivity_lambda_plot.tiff")

#Note: plot is not able to save
## here getting si_distr = Vector of probabilities giving the discrete distribution of the serial interval is very improtant



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
set.seed(1) # this prevents changes in estimates by running same sample
future_i <- project(maha_incidence_object, R = loglinear_R0, n_sim = 1000, si = w, n_days = 30)
#loglinear_R0 is from B.1 above

future_i

plot(future_i)

dev.off()  #removes plot from plot window    

predicted_n <- colSums(future_i) # getting column wise summary of each simulation

summary(predicted_n)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 164152  174527  176886  176891  179251  188538 

mean(predicted_n) #176890.7

hist(predicted_n, col = "darkred", border = "white",
     main = "Prediction: new cases in 30 days",
     xlab = "Total number of new cases")



# plotting incidence with projections
large_txt <- ggplot2::theme(text = ggplot2::element_text(size = 16),
                            axis.text = ggplot2::element_text(size = 12))

rotate_x_txt <- theme(axis.text.x = element_text(angle = 45,
                                                 hjust = 1))

plot(maha_incidence_object) %>%
  add_projections(future_i, c(.1, .5, .9)) +
  theme_bw() +
  scale_x_date() +
  large_txt +
  rotate_x_txt + 
  labs(title = "Short term forcasting of new cases",
       subtitle = "upto ")
## Scale for 'x' is already present. Adding another scale for 'x', which
## will replace the existing scale.


##########################
#D.1. Alternative Plotting
##########################


# 1. Using only incidence data for 30 days from last date
set.seed(1)
pred_fwd_days <- 30 #10, 15
date_range <- 1:(which(get_dates(maha_incidence_object) == as.Date("2020-05-31")) - pred_fwd_days)


test_pred <- project(maha_incidence_object[date_range],
                     R = median(loglinear_lock3_R0),  #loglinear_afterlock1_R0 #loglinear_R0
                     si = w,
                     n_days = pred_fwd_days, n_sim = 1000)


test_pred_median_counts <- test_pred %>% as.data.frame() %>% 
  pivot_longer(-dates, names_to = "simulation", values_to = "incidence") %>% 
  group_by(dates) %>% summarise(incident_cases = as.integer(median(incidence))) %>% 
  mutate(data_type = "projection")

test_pred_median_counts %>%
  bind_rows(tibble(dates=get_dates(maha_incidence_object),
                   incident_cases=get_counts(maha_incidence_object),
                   data_type="observed")) %>%
  ggplot(aes(x=dates, y=incident_cases, colour=data_type)) +
  geom_point() +
  geom_line() +
  labs(x="", y="Daily incident confirmed cases",
       title="Observed versus growth-phase projection of incident cases\nin Maharashtra",
       subtitle=paste("(projection based on observed case counts up to", 
                      #format(maha_incidence_peak - days(pred_fwd_days), "%d %B %Y"),
                      ")")) +
  theme(legend.position="top", legend.title = element_blank())


#Result: it shows the projections are in right direction

fig26 <- recordPlot() 
invisible(dev.off())

fig26

invisible(dev.off())
# ggsave(path = plot_save, 
#        filename = "24_Projection_Testing_plot.tiff") 


################################################################
#D.2. projecting for future 30 days using data from last 30 days
################################################################

set.seed(1)
pred_fwd_days <- 13 # 5, 30 #From 31st backwards, 18th May is 14th day so last 13 day data used for making projects for next 30 days i.e. upto 30th June 2020
date_range <- which(get_dates(maha_incidence_object) == as.Date("2020-05-31")):(length(get_dates(maha_incidence_object)) - pred_fwd_days)

test_pred_30day <- project(maha_incidence_object[date_range],
                           R = median(loglinear_lock4_R0),  # Re for last phase of lockdown used to make projections for coming 30 days.
                           si = w,
                           n_days = 43, n_sim = 1000) # we want next 30 day projections so 13+30=43


test_pred_30day_median_counts <- test_pred_30day %>%
  as.data.frame() %>%
  pivot_longer(-dates,
               names_to="simulation",
               values_to="incidence") %>%
  group_by(dates) %>%
  summarise(incident_cases=as.integer(median(incidence))) %>%
  mutate(data_type = "projection")


test_pred_30day_median_counts %>%
  bind_rows(tibble(dates=get_dates(maha_incidence_object),
                   incident_cases=get_counts(maha_incidence_object),
                   data_type="observed")) %>%
  ggplot(aes(x=dates, y=incident_cases, colour=data_type)) +
  geom_point() +
  geom_line() +
  labs(x="", y="Daily incident confirmed cases",
       title=" 30 Day Projection of incident cases in Maharashtra",
       subtitle=paste("[projection based on observed case counts \nfrom 18 May 2020 to 31 May 2020]")) +
  theme(legend.position="top", legend.title = element_blank())

fig27 <- recordPlot()
invisible(dev.off())

fig27

# ggsave(path = plot_save,
#        filename = "25_Projection_Upto_8June2020_plot.tiff")

invisible(dev.off())

##########################################################################################
#D.3. Project future cases upto 31 May 2020 based on prelockdown and lockdown1 combined R0
##########################################################################################

#D.3.A1. To get this first get the loglinear R0 for pre-lockdown and lockdown-1 period combined
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

#previously used Saurabh etal(2020)	Serial interval, basic reproduction number and prediction of COVID-19 epidemic size in Jodhpur, India
# Mu = 6.75 days, SD = 3.76 days
#link-https://www.medrxiv.org/content/10.1101/2020.07.03.20146167v1.full.pdf


mu2 <- 3.9 # this is from Rajendrakumar etal(2020) previously used Saurabh etal(202)=6.75
sigma2 <- 2.85  # this is from Rajendrakumar etal(2020) previously used Saurabh etal(202)=3.76
param2 <- gamma_mucv2shapescale(mu2, sigma2 / mu2)
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



#D.3.A4. Test projection based on R0 for pre-and=lockdown-1 period
set.seed(1)
pred_fwd_days <- 84 # substracting total number of days from date rane object allows the projection run for first 37 days of the data
date_range <- which(get_dates(maha_incidence_object) == as.Date("2020-05-31")):(length(get_dates(maha_incidence_object)) - pred_fwd_days)

test_pred_pre_andlock1 <- project(maha_incidence_object[date_range],
                           R = median(loglinear_lock01_R0),  # Re for last phase of lockdown used to make projections for coming 30 days.
                           si = w,
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

ggsave(path = plot_save,
       filename = "28a_Projection_test_Upto_14April2020_plot.tiff")

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
  labs(x="", y="Daily incident confirmed cases",
       title="Daily observed vs projected COVID-19 incident cases in Maharashtra-India",
       caption = "Note: projection is based on observed case counts from 09 March to 14 April 2020",   
       subtitle=paste("(from 9 March to 31 May 2020)")) +
  theme(legend.position="top", legend.title = element_blank())

fig28b <- recordPlot() 
invisible(dev.off())

fig28b

ggsave(path = plot_save,
       filename = "28b_Projection_Upto_31May2020_basedonprelockdown1R0_plot.tiff")

ggsave(path = plot_save,
       filename = "28b_Projection_Upto_31May2020_basedonprelockdown1R0_plot.jpeg")

invisible(dev.off())

#rm(list=setdiff(ls(), "x")) remove all but "x" object from R environment
###############################################################################################
# Conclusion: Use Rajendrakumar etal(2020) estimates for all the calculations as they were 
# previoulsy used in all preliminary results.
###############################################################################################


#######################################################
#Q.1: How many infections saved during lockdown period?
#######################################################

#Answer this question by:
# 1. Fit data on pre-lockdown period 
# 2. Estimate R0 using log-linear method
# 3. Project future cases for lockdown period
# 4. Take difference of projected-actual cases
# 5. Do cost-calculations
# 6. How much is cumulative incidence for the period? predicted vs actual?


## 1. Fitting upto lockdown data
# see locksplit codes for fitting section

## 2. Estimate R0 for lockdown period using log-linear method
# see log-linear estimation section

## 3. Project future cases upto 10 May 2020 based on lockdown-1 R0
# see projection section

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

cumlock01_proj_actual  # I=64975, projected= 162156, difference= 97181, D = 2108, R = 29070

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
          "/Users/preshitambade/Downloads/Corona-CEA/1-COVID-19_CEA_study/2-COVID-19-Model/5-Epimodel_demo/2_epimodel_results/7_epimodel_results_formanuscript_8Dec2020/1_incidence_projectiond_8December2020.csv",
      row.names = FALSE)

#to run entire script press: cmd+shift+enter
#shortcut to source: cmd+shift+s

quit(save = "no", status = 0, runLast = TRUE)

#link for exponential growth model: https://mathbitsnotebook.com/Algebra2/Exponential/EXGrowthDecay.html

#example Quit function
# # NOT RUN {
# ## Unix-flavour example
# .Last <- function() {
#   graphics.off() # close devices before printing
#   cat("Now sending PDF graphics to the printer:\n")
#   system("lpr Rplots.pdf")
#   cat("bye bye...\n")
# }
# quit("yes")
# # }
# 











