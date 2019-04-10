#Title: Juvenile Repeat Exposure Experiment 2018
#Project: FFAR
#Author: HM Putnam & Sam Gurr
#Edit by: Sam Gurr
#Date Last Modified: 20190409
#See Readme file for details

rm(list=ls())
## Install packages if not already in your library
if ("dplyr" %in% rownames(installed.packages()) == 'FALSE') install.packages('dplyr') 
#if ("plyr" %in% rownames(installed.packages()) == 'FALSE') install.packages('plyr') 
if ("ggplot2" %in% rownames(installed.packages()) == 'FALSE') install.packages('ggplot2') 
if ("ggpubr" %in% rownames(installed.packages()) == 'FALSE') install_github('ggpubr') 
if ("Rmisc" %in% rownames(installed.packages()) == 'FALSE') install.packages('Rmisc') 
#if ("nlme" %in% rownames(installed.packages()) == 'FALSE') install.packages('nlme') 
#if ("lme4" %in% rownames(installed.packages()) == 'FALSE') install.packages('lme4') 
if ("plotrix" %in% rownames(installed.packages()) == 'FALSE') install.packages('plotrix') 
if ("lsmeans" %in% rownames(installed.packages()) == 'FALSE') install.packages('lsmeans') 
if ("gridExtra" %in% rownames(installed.packages()) == 'FALSE') install.packages('gridExtra') 
if ("reshape" %in% rownames(installed.packages()) == 'FALSE') install.packages('reshape') 
if ("multcompView" %in% rownames(installed.packages()) == 'FALSE') install.packages('multcompView') 
#if ("tidyr" %in% rownames(installed.packages()) == 'FALSE') install.packages('tidyr') 
#if ("Rcmdr" %in% rownames(installed.packages()) == 'FALSE') install.packages('Rcmdr') 

# Load packages and pacage version/date/import/depends info
library(dplyr)          # Version 0.7.6, Packaged: 2018-06-27, Depends: R (>= 3.1.2)Imports: assertthat (>= 0.2.0), bindrcpp (>= 0.2.0.9000), glue (>=1.1.1), magrittr (>= 1.5), methods, pkgconfig (>= 2.0.1), R6(>= 2.2.2), Rcpp (>= 0.12.15), rlang (>= 0.2.0), tibble (>=1.3.1), tidyselect (>= 0.2.3), utils
#library(plyr)           # Version 1.8.4, Packaged: 2016-06-07, Depends: R (>= 3.1.0) Imports: Rcpp (>= 0.11.0)
library(ggplot2)        # Version 2.2.1, Packaged: 2016-12-30, Depends: R (>= 3.1)Imports: digest, grid, gtable (>= 0.1.1), MASS, plyr (>= 1.7.1),reshape2, scales (>= 0.4.1), stats, tibble, lazyeval
library(ggpubr)         # Version: 0.1.8 Date: 2018-08-30, Depends: R (>= 3.1.0), ggplot2, magrittrImports: ggrepel, grid, ggsci, stats, utils, tidyr, purrr, dplyr(>=0.7.1), cowplot, ggsignif, scales, gridExtra, glue, polynom
library(Rmisc)          # Version: 1.5 Packaged: 2013-10-21, Depends: lattice, plyr
#library(nlme)           # Version: 3.1-137, Date: 2018-04-07, Depends: R (>= 3.4.0) Imports: graphics, stats, utils, lattice
#library(lme4)           # Version: 1.1-17, Date/Publication: 2018-04-03, Depends: R (>= 3.2.0), Matrix (>= 1.2-1), methods, stats
library(plotrix)        # Version: 3.7-4, Date/Publication: 2018-10-03
library(lsmeans)        # Version: 2.27-62, Date/Publication: 2018-05-11, Depends: methods, R (>= 3.2)
library(gridExtra)      # Version: 2.3, Date/Publication: 2017-09-09, Imports: gtable, grid, grDevices, graphics, utils
library(reshape)        # Version: 0.8.7, Date/Publication: 2017-08-06, Depends: R (>= 2.6.1) Imports: plyr
library(multcompView)   # Version: 0.1-7, Date/Publication: 2015-07-31, Imports: grid
#library(tidyr)          # Version: 0.8.1, Date/Publication: 2018-05-18, Depends: R (>= 3.1) Imports: dplyr (>= 0.7.0), glue, magrittr, purrr, Rcpp, rlang, stringi, tibble, tidyselect
#library(Rcmdr)          # Version: 2.5-1. Date/Publication: 2018-09-11, Depends: R (>= 3.5.0), grDevices, graphics, methods, stats, utils,splines, RcmdrMisc (>= 2.5-0), car (>= 3.0-1), effects (>=4.0-3) Imports: tcltk, tcltk2 (>= 1.2-6), abind, relimp (>= 1.0-5)
library(Rmisc)
#Required Data files
# ----Conical Chemistry (APEX data)
#20180724_Apex_Data_Output.csv
#20180805_Apex_Data_Output.csv
#20180814_Apex_Data_Output.csv
# ----Heath Tray Chemistry (discrete probe data)
#Flow.rates.csv
#pH_Calibration_Files (tris data)
#Daily_Temp_pH_Sal.csv
# ----Respiration data
#Resp.pre.Exposure.csv
#Resp.30d.Exposure.csv
# ----Shell size data
#Length.30d.Exposure.csv

#set working directory--------------------------------------------------------------------------------------------------
setwd("C:/Users/samjg/Documents/My_Projects/Juvenile_geoduck_OA/RAnalysis/") #set working

### CONICAL Seawater chemistry Data - Analysis, Graphs, Tables (APEX DATA) ####

#CONTINUOUS EXPERIMENTAL APEX DATA 
#Load Apex Data 
APEX_1<-read.csv("Data/Chemistry.flow/Apex_data/20180724_Apex_Data_Output.csv", header=T, sep=",", na.string="NA", as.is=T) 
APEX_2<-read.csv("Data/Chemistry.flow/Apex_data/20180801_Apex_Data_Output.csv", header=T, sep=",", na.string="NA", as.is=T) 
APEX_3<-read.csv("Data/Chemistry.flow/Apex_data/20180805_Apex_Data_Output.csv", header=T, sep=",", na.string="NA", as.is=T) 
APEX_4<-read.csv("Data/Chemistry.flow/Apex_data/20180814_Apex_Data_Output.csv", header=T, sep=",", na.string="NA", as.is=T) 
APEX_data<- do.call("rbind", list(APEX_1, APEX_2, APEX_3, APEX_4)) # bind all data together
APEX_data$Date.Time <-as.POSIXct(APEX_data$Date.Time, format="%Y-%m-%d %H:%M:%S") #convert date format

#plot raw data
plot(APEX_data$Date.Time,APEX_data$pH_T0) # tail end is after exposure experiment
plot(APEX_data$Date.Time,APEX_data$pH_T1) # tail end is after exposure experiment
plot(APEX_data$Date.Time,APEX_data$pH_T2) # start and tail end show before and after exposure experiment
plot(APEX_data$Date.Time,APEX_data$pH_T3) # tail end is after exposure experiment
plot(APEX_data$Date.Time,APEX_data$TMP_T0)
plot(APEX_data$Date.Time,APEX_data$TMP_T1)
plot(APEX_data$Date.Time,APEX_data$TMP_T2)
plot(APEX_data$Date.Time,APEX_data$Temp_T3)
# truncate start and end
start.elim <- 72 # create a start time at noon on day 0
APEX_data <- APEX_data[-(1:start.elim),]
APEX_data <- APEX_data[-(4442:4538),] # deleted last rows post-treatment 
plot(APEX_data$Date.Time,APEX_data$pH_T0) # tail end pH increase is ommited

APEX_data$datehour <- cut(as.POSIXct(APEX_data$Date.Time),
                          format="%d-%m-%Y %H:%M:%S", breaks="hour") # create new column for datehour to aggregate data
head(APEX_data) # check this new column - all 10 minute increment data is called for the hour as "datehour"

date <- format(as.POSIXct(APEX_data$Date.Time) ,format = "%Y-%m-%d %H") # call year month and date
days <- as.Date(date) - as.Date(date[1]) # subtract from start to get number of days
days <- as.numeric(days) # convert to numeric

# pH tables and graphs for all exposures 
pH.low_t0 <- do.call(data.frame,aggregate(pH_T0 ~ datehour + days, data = APEX_data, function(x) c(mean = mean(x), se = std.error(x)))) #calculate mean and sem of each treatment by Hour
pH.low_t3 <- do.call(data.frame,aggregate(pH_T3 ~ datehour + days, data = APEX_data, function(x) c(mean = mean(x), se = std.error(x)))) #calculate mean and sem of each treatment by Hour
pH.amb_t1 <- do.call(data.frame,aggregate(pH_T1 ~ datehour + days, data = APEX_data, function(x) c(mean = mean(x), se = std.error(x)))) #calculate mean and sem of each treatment by Hour
pH.amb_t2 <- do.call(data.frame,aggregate(pH_T2 ~ datehour + days, data = APEX_data, function(x) c(mean = mean(x), se = std.error(x)))) #calculate mean and sem of each treatment by Hour

pH.low_t0$Treatment <- "Low_1" #Add treatment Information
colnames(pH.low_t0) <- c("datehour", "days", "mean", "se", "Treatment") #rename columns to generic format
pH.low_t3$Treatment <- "Low_2" #Add treatment Information
colnames(pH.low_t3) <- c("datehour", "days", "mean", "se", "Treatment") #rename columns to generic format
pH.amb_t1$Treatment <- "Ambient_1" #Add treatment Information
colnames(pH.amb_t1) <- c("datehour", "days", "mean", "se", "Treatment") #rename columns to generic format
pH.amb_t2$Treatment <- "Ambient_2" #Add treatment Information
colnames(pH.amb_t2) <- c("datehour", "days", "mean", "se", "Treatment") #rename columns to generic format
hourly.pH <- rbind(pH.low_t0, pH.low_t3, pH.amb_t1, pH.amb_t2) #bind treatment data 
hourly.pH <- hourly.pH[!is.na(hourly.pH$se), ] # ommit rows with NA for stand error
hourly.pH <- hourly.pH[!(hourly.pH$se > 0.2),] # ommit rows with high stand error (conical cleaning)
hourly.pH #view data

# subset the data
# Initial exposure pH
APEX.pH.Exp1 <- hourly.pH %>%
  filter(days <= 9) # pH APEX Exp1
#check for the transition data from elevated to ambient for supplementary figure
LowEXP_1 <- APEX.pH.Exp1 %>%  
  filter(APEX.pH.Exp1$Treatment=="Low_1") # subset a lwo treatment conical
tail(LowEXP_1, n = 40) # rows 223:228 shows transition to ambiet common garden
LowEXP_2 <- APEX.pH.Exp1 %>%  
  filter(APEX.pH.Exp1$Treatment=="Low_2") # subset a lwo treatment conical
tail(LowEXP_2, n = 40) # rows 223:228 shows transition to ambiet common garden

APEX.pH.Exp1 <- APEX.pH.Exp1[-c(223:228, 451:456), ] # ommit these data for the supplementary figure

# common garden pH
APEX.pH.commongarden <- hourly.pH %>%
  filter(days >= 10 & days <= 23) # pH APEX common garden
APEX.pH.commongarden <- APEX.pH.commongarden[-(1300:1338),] # delete the transition data after common garden
tail(APEX.pH.commongarden) # look at the end of the dataset - some data show transition to elevated
APEX.pH.commongarden <- APEX.pH.commongarden %>% 
                        filter(mean > 7.7) # delete the last few datapoints, high st.dev becasue conditions were returning to elevated

# secondary exposure pH
APEX.pH.Exp2 <- hourly.pH %>%
  filter(days >= 24 & days <= 30) # pH APEX Exp2

# Plot daily averages of pH data for the complete experiment (continuous APEX data)
APEX.pH.Exp1$datehour <- as.POSIXct(APEX.pH.Exp1$datehour, format="%Y-%m-%d %H:%M:%S") #format datehour 
Exp1.pH.Apex.FIG <- ggplot(APEX.pH.Exp1, aes(x=datehour, y=mean, group=Treatment, color=Treatment)) +#Plot average diurnal cycle of temperature data
                    #geom_line() +
                    geom_point(aes(x = datehour, y = mean, group=Treatment, color=Treatment),cex=1) + #Plot points using time as the x axis, light as the Y axis and black dots
                    geom_errorbar(aes(x=datehour, ymax=mean+se, ymin=mean-se), 
                                  position=position_dodge(0.9), data=APEX.pH.Exp1, col="black", width=0) + #set values for standard error bars and offset on the X axis for clarity
                    ggtitle("A) Exp1") + #Label the graph with the main title
                    #scale_x_date(date_minor_breaks = "1 day") +
                    #scale_x_date(breaks = APEX.pH.Exp1$datehour[seq(1, length(APEX.pH.Exp1$datehour), by = 24)]) +
                    ylim(7,8) + #Set Y axis limits
                    xlab("Time") + #Label the X Axis
                    ylab("pH (NBS)") + #Label the Y Axis
                    #scale_x_date(date_minor_breaks = "1 day") +
                    theme_bw() + #Set the background color
                    theme(axis.line = element_line(color = 'black'), #Set the axes color
                          axis.ticks.length=unit(-0.2, "cm"), #turn ticks inward
                          axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), #set margins on labels
                          axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"), angle = 90, vjust = 0.5, hjust=1), #set margins on labels
                          panel.grid.major = element_blank(), #Set the major gridlines
                          panel.grid.minor = element_blank(), #Set the minor gridlines
                          plot.background=element_blank(), #Set the plot background
                          panel.border=element_rect(size=1.25, fill = NA), #set outer border
                          plot.title=element_text(hjust=0),
                          legend.position="bottom", #set legend location
                          legend.text = element_text(size = 8), #set the legend text size
                          legend.key = element_blank(), #remove the legend background
                          legend.title = element_text(size=8, face="bold")) #Justify the title to the top left

Exp1.pH.Apex.FIG <- Exp1.pH.Apex.FIG + scale_color_manual(values=c("#009E73", "#0072B2", "#E69F00", "#D55E00")) #colorblindess color theme
Exp1.pH.Apex.FIG # view figure

APEX.pH.commongarden$datehour <- as.POSIXct(APEX.pH.commongarden$datehour, format="%Y-%m-%d %H:%M:%S") #format datehour 
CommGarden.pH.Apex.FIG <- ggplot(APEX.pH.commongarden, aes(x=datehour, y=mean, group=Treatment, color=Treatment)) +#Plot average diurnal cycle of temperature data
                            #geom_line() +
                            geom_point(aes(x = datehour, y = mean, group=Treatment), cex=1) + #Plot points using time as the x axis, light as the Y axis and black dots
                            geom_errorbar(aes(x=datehour, ymax=mean+se, ymin=mean-se), 
                                          position=position_dodge(0.9), data=APEX.pH.commongarden, col="black", width=0) + #set values for standard error bars and offset on the X axis for clarity
                            ggtitle("B) Common garden") + #Label the graph with the main title
                            #scale_x_date(breaks = APEX.pH.Exp2$hour[seq(1, length(APEX.pH.Exp2$hour), by = 24)]) +
                            ylim(7,8) + #Set Y axis limits
                            xlab("Time") + #Label the X Axis
                            ylab("pH (NBS)") + #Label the Y Axis
                            #scale_x_date(date_minor_breaks = "1 day") +
                            theme_bw() + #Set the background color
                            theme(axis.line = element_line(color = 'black'), #Set the axes color
                                  axis.ticks.length=unit(-0.2, "cm"), #turn ticks inward
                                  axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), #set margins on labels
                                  axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"), angle = 90, vjust = 0.5, hjust=1), #set margins on labels
                                  panel.grid.major = element_blank(), #Set the major gridlines
                                  panel.grid.minor = element_blank(), #Set the minor gridlines
                                  plot.background=element_blank(), #Set the plot background
                                  panel.border=element_rect(size=1.25, fill = NA), #set outer border
                                  plot.title=element_text(hjust=0),
                                  legend.position="bottom", #set legend location
                                  legend.text = element_text(size = 8), #set the legend text size
                                  legend.key = element_blank(), #remove the legend background
                                  legend.title = element_text(size=8, face="bold")) #Justify the title to the top left
CommGarden.pH.Apex.FIG <- CommGarden.pH.Apex.FIG + scale_color_manual(values=c("#009E73", "#0072B2", "#E69F00", "#D55E00")) #colorblindess color theme
CommGarden.pH.Apex.FIG # view the plot

APEX.pH.Exp2$datehour <- as.POSIXct(APEX.pH.Exp2$datehour, format="%Y-%m-%d %H:%M:%S")#format datehour 
Exp2.pH.Apex.FIG <- ggplot(APEX.pH.Exp2, aes(x=datehour, y=mean, group=Treatment, color=Treatment)) +#Plot average diurnal cycle of temperature data
                    #geom_line() +
                    geom_point(aes(x = datehour, y = mean, group=Treatment), cex=1) + #Plot points using time as the x axis, light as the Y axis and black dots
                    geom_errorbar(aes(x=datehour, ymax=mean+se, ymin=mean-se), 
                                  position=position_dodge(0.9), data=APEX.pH.Exp2, col="black", width=0) + #set values for standard error bars and offset on the X axis for clarity
                    ggtitle("C) Exp2") + #Label the graph with the main title
                    #scale_x_date(breaks = APEX.pH.Exp2$hour[seq(1, length(APEX.pH.Exp2$hour), by = 24)]) +
                    ylim(7,8) + #Set Y axis limits
                    xlab("Time") + #Label the X Axis
                    ylab("pH (NBS)") + #Label the Y Axis
                    #scale_x_date(date_minor_breaks = "1 day") +
                    theme_bw() + #Set the background color
                    theme(axis.line = element_line(color = 'black'), #Set the axes color
                          axis.ticks.length=unit(-0.2, "cm"), #turn ticks inward
                          axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), #set margins on labels
                          axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"), angle = 90, vjust = 0.5, hjust=1), #set margins on labels
                          panel.grid.major = element_blank(), #Set the major gridlines
                          panel.grid.minor = element_blank(), #Set the minor gridlines
                          plot.background=element_blank(), #Set the plot background
                          panel.border=element_rect(size=1.25, fill = NA), #set outer border
                          plot.title=element_text(hjust=0),
                          legend.position="bottom", #set legend location
                          legend.text = element_text(size = 8), #set the legend text size
                          legend.key = element_blank(), #remove the legend background
                          legend.title = element_text(size=8, face="bold")) #Justify the title to the top left
Exp2.pH.Apex.FIG <- Exp2.pH.Apex.FIG + scale_color_manual(values=c("#009E73", "#0072B2", "#E69F00", "#D55E00")) #colorblindess color theme
Exp2.pH.Apex.FIG #view graph


## Temp tables and graphs 
Temp.low_t0 <- do.call(data.frame,aggregate(TMP_T0 ~ datehour + days, data = APEX_data, function(x) c(mean = mean(x), se = std.error(x)))) #calculate mean and sem of each treatment by Hour
Temp.low_t3 <- do.call(data.frame,aggregate(Temp_T3 ~ datehour + days, data = APEX_data, function(x) c(mean = mean(x), se = std.error(x)))) #calculate mean and sem of each treatment by Hour
Temp.amb_t1 <- do.call(data.frame,aggregate(TMP_T1 ~ datehour + days, data = APEX_data, function(x) c(mean = mean(x), se = std.error(x)))) #calculate mean and sem of each treatment by Hour
Temp.amb_t2 <- do.call(data.frame,aggregate(TMP_T2 ~ datehour + days, data = APEX_data, function(x) c(mean = mean(x), se = std.error(x)))) #calculate mean and sem of each treatment by Hour

Temp.low_t0$Treatment <- "Low_1" #Add treatment Information
colnames(Temp.low_t0) <- c("datehour", "days", "mean", "se", "Treatment") #rename columns to generic format
Temp.low_t3$Treatment <- "Low_2" #Add treatment Information
colnames(Temp.low_t3) <- c("datehour", "days", "mean", "se", "Treatment") #rename columns to generic format
Temp.amb_t1$Treatment <- "Ambient_1" #Add treatment Information
colnames(Temp.amb_t1) <- c("datehour", "days", "mean", "se", "Treatment") #rename columns to generic format
Temp.amb_t2$Treatment <- "Ambient_2" #Add treatment Information
colnames(Temp.amb_t2) <- c("datehour", "days", "mean", "se", "Treatment") #rename columns to generic format
hourly.Temp <- rbind(Temp.low_t0, Temp.low_t3, Temp.amb_t1, Temp.amb_t2) #bind treatment data 
hourly.Temp <- hourly.Temp[!is.na(hourly.Temp$se), ] # ommit rows with NA for stand error
hourly.Temp <- hourly.Temp[!(hourly.Temp$se > 0.2),] # ommit rows with high stand error (conical cleaning)
hourly.Temp #view data

# subset the data
APEX.Temp.Exp1 <- hourly.Temp %>%
  filter(days <= 9) # Temp APEX Exp1

APEX.Temp.commongarden <- hourly.Temp %>%
  filter(days >= 10 & days <= 23) # Temp APEX common garden

APEX.Temp.Exp2 <- hourly.Temp %>%
  filter(days >= 24 & days <= 30) # Temp APEX Exp2

# Plot daily averages of Temp data for the complete experiment (continuous APEX data)
APEX.Temp.Exp1$datehour <- as.POSIXct(APEX.Temp.Exp1$datehour, format="%Y-%m-%d %H:%M:%S") #format datehour 
Exp1.Temp.Apex.FIG <- ggplot(APEX.Temp.Exp1, aes(x=datehour, y=mean, group=Treatment, color=Treatment)) +#Plot average diurnal cycle of temperature data
                      #geom_line() +
                      geom_point(aes(x = datehour, y = mean, group=Treatment), cex=1) + #Plot points using time as the x axis, light as the Y axis and black dots
                      geom_errorbar(aes(x=datehour, ymax=mean+se, ymin=mean-se), 
                                    position=position_dodge(0.9), data=APEX.Temp.Exp1, col="black", width=0) + #set values for standard error bars and offset on the X axis for clarity
                      ggtitle("D) Exp1") + #Label the graTemp with the main title
                      #scale_x_date(date_minor_breaks = "1 day") +
                      #scale_x_date(breaks = APEX.Temp.Exp1$datehour[seq(1, length(APEX.Temp.Exp1$datehour), by = 24)]) +
                      ylim(14,21) + #Set Y axis limits
                      xlab("Time") + #Label the X Axis
                      ylab("Temp (°C)") + #Label the Y Axis
                      #scale_x_date(date_minor_breaks = "1 day") +
                      theme_bw() + #Set the background color
                      theme(axis.line = element_line(color = 'black'), #Set the axes color
                            axis.ticks.length=unit(-0.2, "cm"), #turn ticks inward
                            axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), #set margins on labels
                            axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"), angle = 90, vjust = 0.5, hjust=1), #set margins on labels
                            panel.grid.major = element_blank(), #Set the major gridlines
                            panel.grid.minor = element_blank(), #Set the minor gridlines
                            plot.background=element_blank(), #Set the plot background
                            panel.border=element_rect(size=1.25, fill = NA), #set outer border
                            plot.title=element_text(hjust=0),
                            legend.position="bottom", #set legend location
                            legend.text = element_text(size = 8), #set the legend text size
                            legend.key = element_blank(), #remove the legend background
                            legend.title = element_text(size=8, face="bold")) #Justify the title to the top left
Exp1.Temp.Apex.FIG   <- Exp1.Temp.Apex.FIG   + scale_color_manual(values=c("#009E73", "#0072B2", "#E69F00", "#D55E00")) #colorblindess color theme
Exp1.Temp.Apex.FIG   #view graph

APEX.Temp.commongarden$datehour <- as.POSIXct(APEX.Temp.commongarden$datehour, format="%Y-%m-%d %H:%M:%S")#format datehour 
CommGarden.Temp.Apex.FIG <- ggplot(APEX.Temp.commongarden, aes(x=datehour, y=mean, group=Treatment, color=Treatment)) +#Plot average diurnal cycle of temperature data
                            #geom_line() +
                            geom_point(aes(x = datehour, y = mean, group=Treatment), cex=1) + #Plot points using time as the x axis, light as the Y axis and black dots
                            geom_errorbar(aes(x=datehour, ymax=mean+se, ymin=mean-se), 
                                          position=position_dodge(0.9), data=APEX.Temp.commongarden, col="black", width=0) + #set values for standard error bars and offset on the X axis for clarity
                            ggtitle("E) Common garden") + #Label the graTemp with the main title
                            #scale_x_date(breaks = APEX.Temp.Exp2$hour[seq(1, length(APEX.Temp.Exp2$hour), by = 24)]) +
                            ylim(14,21) + #Set Y axis limits
                            ylab("Temp (°C)") + #Label the Y Axis
                            xlab("Time") + #Label the X Axis
                            #scale_x_date(date_minor_breaks = "1 day") +
                            theme_bw() + #Set the background color
                            theme(axis.line = element_line(color = 'black'), #Set the axes color
                                  axis.ticks.length=unit(-0.2, "cm"), #turn ticks inward
                                  axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), #set margins on labels
                                  axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"), angle = 90, vjust = 0.5, hjust=1), #set margins on labels
                                  panel.grid.major = element_blank(), #Set the major gridlines
                                  panel.grid.minor = element_blank(), #Set the minor gridlines
                                  plot.background=element_blank(), #Set the plot background
                                  panel.border=element_rect(size=1.25, fill = NA), #set outer border
                                  plot.title=element_text(hjust=0),
                                  legend.position="bottom", #set legend location
                                  legend.text = element_text(size = 8), #set the legend text size
                                  legend.key = element_blank(), #remove the legend background
                                  legend.title = element_text(size=8, face="bold")) #Justify the title to the top left
CommGarden.Temp.Apex.FIG   <- CommGarden.Temp.Apex.FIG   + scale_color_manual(values=c("#009E73", "#0072B2", "#E69F00", "#D55E00")) #colorblindess color theme
CommGarden.Temp.Apex.FIG   #view graph

APEX.Temp.Exp2$datehour <- as.POSIXct(APEX.Temp.Exp2$datehour, format="%Y-%m-%d %H:%M:%S")#format datehour 
Exp2.Temp.Apex.FIG <- ggplot(APEX.Temp.Exp2, aes(x=datehour, y=mean, group=Treatment, color=Treatment)) +#Plot average diurnal cycle of temperature data
                      #geom_line() +
                      geom_point(aes(x = datehour, y = mean, group=Treatment), cex=1) + #Plot points using time as the x axis, light as the Y axis and black dots
                      geom_errorbar(aes(x=datehour, ymax=mean+se, ymin=mean-se), 
                                    position=position_dodge(0.9), data=APEX.Temp.Exp2, col="black", width=0) + #set values for standard error bars and offset on the X axis for clarity
                      ggtitle("F) Exp2") + #Label the graTemp with the main title
                      #scale_x_date(breaks = APEX.Temp.Exp2$hour[seq(1, length(APEX.Temp.Exp2$hour), by = 24)]) +
                      ylim(14,21) + #Set Y axis limits
                      ylab("Temp (°C)") + #Label the Y Axis
                      xlab("Time") + #Label the X Axis
                      #scale_x_date(date_minor_breaks = "1 day") +
                      theme_bw() + #Set the background color
                      theme(axis.line = element_line(color = 'black'), #Set the axes color
                            axis.ticks.length=unit(-0.2, "cm"), #turn ticks inward
                            axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm")), #set margins on labels
                            axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"), angle = 90, vjust = 0.5, hjust=1), #set margins on labels
                            panel.grid.major = element_blank(), #Set the major gridlines
                              panel.grid.minor = element_blank(), #Set the minor gridlines
                            plot.background=element_blank(), #Set the plot background
                            panel.border=element_rect(size=1.25, fill = NA), #set outer border
                            plot.title=element_text(hjust=0),
                            legend.position="bottom", #set legend location
                            legend.text = element_text(size = 8), #set the legend text size
                            legend.key = element_blank(), #remove the legend background
                            legend.title = element_text(size=8, face="bold")) #Justify the title to the top left
Exp2.Temp.Apex.FIG   <- Exp2.Temp.Apex.FIG   + scale_color_manual(values=c("#009E73", "#0072B2", "#E69F00", "#D55E00")) #colorblindess color theme
Exp2.Temp.Apex.FIG   #view graph

Supplem.Fig.conical.pH.temp <- grid.arrange(arrangeGrob(Exp1.pH.Apex.FIG, CommGarden.pH.Apex.FIG, Exp2.pH.Apex.FIG, left="pH", ncol=3), 
                                            arrangeGrob(Exp1.Temp.Apex.FIG, CommGarden.Temp.Apex.FIG, Exp2.Temp.Apex.FIG, 
                                                        left="Temperature", ncol=3), ncol=1)

###  HEATH TRAY Flow rate data ##################################################################
# NOTE: heath tray pairs were gravity-fed SW from conical overflow (1 conical to every heath tray pair)
# conicals were set to 1 LPM to target ~500 mLPM for each tray

flow<-read.csv("Data/Chemistry.flow/Flow.rates.csv", header=T, sep=",", na.string="NA", as.is=T) #upload file
flow # view the data

flow_summary <-flow %>% 
  summarise(mean_LPM= mean(LPM),
            max_LPM = max(LPM),
            min_LPM = min(LPM),
            sd_LPM = sd(LPM),
            SEM = ((sd(LPM))/sqrt(n())),
            count = n()) %>% # get the count by leaving n open
  arrange(desc(min_LPM)) # makes table in descending order 
flow_summary # view table

EXP1 <- subset(flow, Exp.num=="EXP1") #initial 10-day trial, subset entire dataset resp by column nsame "Exp.num" == Exp1
flow_EXP1 <- subset(EXP1, Day!=0) # ommit day 0

EXP2 <- subset(flow, Exp.num=="EXP2") #second 6-day trial, subset entire dataset resp by column nsame "Exp.num" == Exp2
flow_EXP2 <- subset(EXP2, Day!=0) # ommit day 0

flow_EXP1_2 <- rbind(flow_EXP1, flow_EXP2) # bind exp1 and 2, day 0 already ommited

# EXP1 
# flow exp by treatment
flow_EXP1.treat <- summarySE(flow_EXP1, measurevar="LPM", groupvars=c("Treatment")) # summary by treatment
flow_EXP1.treat # view table

# EXP2 summary 
# flow by treatment
flow_EXP2.treat <- summarySE(flow_EXP2, measurevar="LPM", groupvars=c("Sec.treat")) # summary by treatment
flow_EXP2.treat # view summary table

# EXP1 AND EXP2 summary 
# flow by treatment
flow_EXP_1_2.treat <- summarySE(flow_EXP1_2, measurevar="LPM", groupvars=c("Treatment")) # summary by treatment
flow_EXP_1_2.treat # view summary table

### HEATH TRAY Discrete Seawater Chemistry Tables #######################################################################

#pH Tris Calibration Curves
#Data to calculate conversion equations from mV to total scale using tris standard for pH probe
path.tris <-("Data/Chemistry.flow/pH_Calibration_Files") #set path to calibration file folder
file.names.tris<-list.files(path = path.tris, pattern = "csv$") #list all the file names with csv 
pH.cals <- data.frame(matrix(NA, nrow=length(file.names.tris), ncol=4, dimnames=list(file.names.tris,c("Date", "Intercept", "Slope","R2")))) #generate an empty 3 column dataframe with specific column names

for(i in 1:length(file.names.tris)) { # for every file in list start at the first and run this following function
  Calib.Data <-read.table(file.path(path.tris,file.names.tris[i]), header=TRUE, sep=",", na.string="NA", as.is=TRUE) #reads in the data files
  model <-lm(mVTris ~ TTris, data=Calib.Data) #runs a linear regression of mV as a function of temperature
  coe <- coef(model) #extracts the coeffecients
  R <- summary(model)$r.squared #extracts the R2
  pH.cals[i,2:3] <- coe #inserts coef in the dataframe
  pH.cals[i,4] <- R #inserts R2 in the dataframe
  pH.cals[i,1] <- substr(file.names.tris[i],1,8) #stores the file name in the Date column
}

colnames(pH.cals) <- c("Calib.Date",  "Intercept",  "Slope", "R2") #names the columns of the dataframe
pH.cals #view data - R2 shows linear strength >0.98 for pH calibration


#call cumulative spreadsheet of discrete seawater chemistry
chem<-read.csv("Data/Chemistry.flow/Daily_Temp_pH_Sal.csv", header=T, sep=",", na.string="NA", as.is=T) 
chem # view the file
chem.exp <-subset(chem, Treatment!="na") #remove na - na often set as the treatment for samples of the sump

chem.exp1 <-chem.exp[52:131,] # exposure 1 without Day 0 (20180715 - 20180724)
chem.exp1$Exposure <- "Exp1"
chem.exp1$tank.name <- substr(chem.exp1$Tank, start = 10, stop = 13) # new column for tank name without date

chem.exp2 <- chem.exp[157:204,] # exposure 2 without Day 0 (20180808 - 20180813)
chem.exp2$Exposure <- "Exp2"
chem.exp2$tank.name <- substr(chem.exp2$Tank, start = 10, stop = 13) # new column for tank name without date

chem.exp_1_2 <- rbind(chem.exp1,chem.exp2) # bind exposure 1 and 2

chem.exp_1_2_pH_table <- aggregate(chem.exp_1_2$pH, list(chem.exp_1_2$Treatment), mean)

# get the range of pH total scale in experiments 1 and 2
chem.exp_1_data <- chem.exp_1_2 %>% filter(chem.exp_1_2$Exposure=="Exp1") # exp 1
chem.exp_2_data <- chem.exp_1_2 %>% filter(chem.exp_1_2$Exposure=="Exp2") # exp 2
# experiment 1 pH table
chem.exp_1_pH_table <- chem.exp_1_data %>% 
  group_by(Treatment) %>% #group the dataset by BOTH INITIAL AND SECONDARY TREATMENT
  summarise(mean_pH= mean(pH),
            max_pH = max(pH),
            min_pH = min(pH),
            sd_pH = sd(pH),
            SEM = ((sd(pH))/sqrt(n())),
            count = n()) %>% # get the count by leaving n open
  arrange(desc(min_pH)) # makes table in descending order 
chem.exp_1_pH_table # view table

# experiment 2 pH table
chem.exp_2_pH_table <- chem.exp_2_data %>% 
  group_by(Treatment) %>% #group the dataset by BOTH INITIAL AND SECONDARY TREATMENT
  summarise(mean_pH= mean(pH),
            max_pH = max(pH),
            min_pH = min(pH),
            sd_pH = sd(pH),
            SEM = ((sd(pH))/sqrt(n())),
            count = n()) %>% # get the count by leaving n open
  arrange(desc(min_pH)) # makes table in descending order 
chem.exp_2_pH_table # view table

#plot pH dta
plot_pH <- ggplot(chem.exp_1_2, aes(x = factor(Treatment), y = pH, fill = Exposure)) +
  scale_fill_manual(values=c("white", "grey3"), labels=c("Ambient","Elevated")) +
  geom_boxplot(alpha = 0.5, # color hue
               width=0.6, # boxplot width
               outlier.size=0, # make outliers small
               position = position_dodge(preserve = "single")) + 
  geom_point(pch = 19, position = position_jitterdodge(.05), size=1) 
plot_pH # view plot

# experiment 1 pCO2 table
chem.exp_1_pCO2_table <- chem.exp_1_data %>% 
  group_by(Treatment) %>% #group the dataset by BOTH INITIAL AND SECONDARY TREATMENT
  summarise(mean_pCO2 = mean(pCO2),
            max_pCO2 = max(pCO2),
            min_pCO2 = min(pCO2),
            sd_pH = sd(pCO2),
            SEM = ((sd(pCO2))/sqrt(n())),
            count = n()) %>% # get the count by leaving n open
  arrange(desc(min_pCO2)) # makes table in descending order 
chem.exp_1_pCO2_table # view table

# experiment 2 pCO2 table
chem.exp_2_pCO2_table <- chem.exp_2_data %>% 
  group_by(Treatment) %>% #group the dataset by BOTH INITIAL AND SECONDARY TREATMENT
  summarise(mean_pCO2 = mean(pCO2),
            max_pCO2 = max(pCO2),
            min_pCO2 = min(pCO2),
            sd_pH = sd(pCO2),
            SEM = ((sd(pCO2))/sqrt(n())),
            count = n()) %>% # get the count by leaving n open
  arrange(desc(min_pCO2)) # makes table in descending order 
chem.exp_2_pCO2_table # view table
# plot pCO2 data
plot_pCO2 <- ggplot(chem.exp_1_2, aes(x = factor(Treatment), y = pCO2, fill = Exposure)) +
  scale_fill_manual(values=c("white", "grey3"), labels=c("Ambient","Elevated")) +
  geom_boxplot(alpha = 0.5, # color hue
               width=0.6, # boxplot width
               outlier.size=0, # make outliers small
               position = position_dodge(preserve = "single")) + 
  geom_point(pch = 19, position = position_jitterdodge(.05), size=1) 
plot_pCO2 # view plot


chem.common.garden <-chem.exp[132:156,] # common garden between exposure periods
chem.common.garden$Exposure <- "Common_garden" 
chem.common.garden$tank.name <- substr(chem.common.garden$Tank, start = 10, stop = 13) # new column for tank name without date

# melt - converts wide table to long
chem.long <- melt(chem.exp_1_2, id.vars=c("Date", "Tank", "Treatment", "tank.name", "Exposure")) # uses tidyr to make a long table from wide
garden.chem.long <- melt(chem.common.garden, id.vars=c("Date", "Tank", "Treatment", "tank.name", "Exposure"))

Exp1.chem.long <-subset(chem.long, Exposure == "Exp1") #separate out exposure 1 for all data
Exp2.chem.long <-subset(chem.long, Exposure == "Exp2") #separate out exposure 2 for all data

#Test for tank and treatment differences in Temperature and Total Alkalinity in Exposure 1
Exp1.Temp <-subset(Exp1.chem.long, variable=="Temperature") #separate out exposure 1 for all data
temp1.tank <- aov(value~tank.name, data=Exp1.Temp) #test the hypothesis there is no difference in temperature between tanks
temp1.tank.res <-anova(temp1.tank) #view results
par(mfrow=c(3,2)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(temp1.tank$residuals) #plot histogram of residuals
boxplot(temp1.tank$residuals) #plot boxplot of residuals
plot(temp1.tank) #display residuals versus fitter, normal QQ plot, leverage plot

temp1.trt <- aov(value ~Treatment, data=Exp1.Temp) #test the hypothesis there is no difference in temperature between treatments
temp1.trt.res <- anova(temp1.trt) #statistical results
par(mfrow=c(3,2)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(temp1.trt$residuals) #plot histogram of residuals
boxplot(temp1.trt$residuals) #plot boxplot of residuals
plot(temp1.trt) #display residuals versus fitter, normal QQ plot, leverage plot

Exp1.TA <-subset(Exp1.chem.long, variable=="TA") #separate out exposure 1 for all data
TA1.tank <- aov(value ~tank.name, data=Exp1.TA) #test the hypothesis there is no difference in total alkalinity between tanks
TA1.tank.res <- anova(temp1.trt) #statistical results
par(mfrow=c(3,2)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(TA1.tank$residuals) #plot histogram of residuals
boxplot(TA1.tank$residuals) #plot boxplot of residuals
plot(TA1.tank) #display residuals versus fitter, normal QQ plot, leverage plot

TA1.trt <- aov(value ~Treatment, data=Exp1.TA) #test the hypothesis there is no difference in total alkalinity between treatments
TA1.trt.res <- anova(temp1.trt) #statistical results
par(mfrow=c(3,2)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(TA1.trt$residuals) #plot histogram of residuals
boxplot(TA1.trt$residuals) #plot boxplot of residuals
plot(TA1.trt) #display residuals versus fitter, normal QQ plot, leverage plot

#Test for tank and treatment differences in Temperature and Total Alkalinity in Exposure 2
Exp2.Temp <-subset(Exp2.chem.long, variable=="Temperature") #separate out exposure 1 for all data
temp2.tank <- aov(value~tank.name, data=Exp2.Temp) #test the hypothesis there is no difference in temperature between tanks
temp2.tank.res <-anova(temp2.tank) #view results
par(mfrow=c(3,2)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(temp2.tank$residuals) #plot histogram of residuals
boxplot(temp2.tank$residuals) #plot boxplot of residuals
plot(temp2.tank) #display residuals versus fitter, normal QQ plot, leverage plot

temp2.trt <- aov(value ~Treatment, data=Exp2.Temp) #test the hypothesis there is no difference in temperature between treatments
temp2.trt.res <- anova(temp2.trt) #statistical results
par(mfrow=c(3,2)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(temp2.trt$residuals) #plot histogram of residuals
boxplot(temp2.trt$residuals) #plot boxplot of residuals
plot(temp2.trt) #display residuals versus fitter, normal QQ plot, leverage plot

Exp2.TA <-subset(Exp2.chem.long, variable=="TA") #separate out exposure 1 for all data
TA2.tank <- aov(value ~tank.name, data=Exp2.TA) #test the hypothesis there is no difference in total alkalinity between tanks
TA2.tank.res <- anova(temp2.trt) #statistical results
par(mfrow=c(3,2)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(TA2.tank$residuals) #plot histogram of residuals
boxplot(TA2.tank$residuals) #plot boxplot of residuals
plot(TA2.tank) #display residuals versus fitter, normal QQ plot, leverage plot

TA2.trt <- aov(value ~Treatment, data=Exp2.TA) #test the hypothesis there is no difference in total alkalinity between treatments
TA2.trt.res <- anova(temp2.trt) #statistical results
par(mfrow=c(3,2)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(TA2.trt$residuals) #plot histogram of residuals
boxplot(TA2.trt$residuals) #plot boxplot of residuals
plot(TA2.trt) #display residuals versus fitter, normal QQ plot, leverage plot

#Calculate descriptive stats by Tray
SWC.Tanks <- ddply(chem.long, c("Exposure", "tank.name", "variable"), summarise, #apply functions to sewater chem data
                   N = length(na.omit(value)), #count the sample size removing NA
                   mean = mean(value), #calculate average 
                   sem = sd(value)/sqrt(n)) #calcualte the standard error of the mean

#Calculate descriptive stats by Treatment and exposure 
SWC.Treatments <- ddply(chem.long, c("Exposure", "Treatment", "variable"), summarise,
                        N = length(na.omit(value)), #count the sample size removing NA
                        mean = mean(value), #calculate average 
                        sem = sd(value)/sqrt(N)) #calcualte the standard error of the mean

#Calculate descriptive stats by Treatment and exposure 
SWC.Treatments.all <- ddply(chem.long, c("Treatment", "variable"), summarise,
                            N = length(na.omit(value)), #count the sample size removing NA
                            mean = mean(value), #calculate average 
                            sem = sd(value)/sqrt(N)) #calcualte the standard error of the mean

#Calculate descriptive stats by Treatment and exposure for common garden period
SWC.common.garden <- ddply(garden.chem.long, c("Exposure", "Treatment", "variable"), summarise,
                           N = length(na.omit(value)), #count the sample size removing NA
                           mean = mean(value), #calculate average 
                           sem = sd(value)/sqrt(N)) #calcualte the standard error of the mean

#subset chem data for exp 1 and exp 2
Exposure1.chem <-subset(SWC.Treatments, Exposure == "Exp1") #separate out exposure 1
Exposure2.chem <-subset(SWC.Treatments, Exposure == "Exp2") #separate out exposure 2

# create tables for exp 1 , exp2 and all data
Exposure1.long <- reshape(Exposure1.chem, idvar="Treatment", direction="wide", timevar = "variable", drop = c("Exposure", "N")) #reshape data format for table layout
Exposure2.long <- reshape(Exposure2.chem, idvar="Treatment", direction="wide", timevar = "variable", drop = c("Exposure", "N")) #reshape data format for table layout
ALL.Exposure.long <- reshape(SWC.Treatments.all, idvar="Treatment", direction="wide", timevar = "variable", drop = c("Exposure", "N")) #reshape data format for table layout
CommGard.chem <- reshape(SWC.common.garden, idvar="Treatment", direction="wide", timevar = "variable", drop = c("Exposure", "N"))

# write out tables
write.table (Exposure1.long, file="C:/Users/samjg/Documents/My_Projects/Juvenile_geoduck_OA/RAnalysis/Output/TABLE.Chem.initial.10d.exposure.csv", sep=",", row.names = FALSE) #save data to output file
write.table (Exposure2.long, file="C:/Users/samjg/Documents/My_Projects/Juvenile_geoduck_OA/RAnalysis/Output/TABLE.Chem.secondary.6d.exposure.csv", sep=",", row.names = FALSE) #save data to output file
write.table (ALL.Exposure.long, file="C:/Users/samjg/Documents/My_Projects/Juvenile_geoduck_OA/RAnalysis/Output/TABLE.Chem.both.exposures.csv", sep=",", row.names = FALSE) #save data to output file
write.table (CommGard.chem, file="C:/Users/samjg/Documents/My_Projects/Juvenile_geoduck_OA/RAnalysis/Output/TABLE.Chem.common.garden.csv", sep=",", row.names = FALSE) #save data to output file

### Respiration Data - Analysis, Graphs, Models  (summarized analysis from Stats_resp_analysis.R)#############

#Load PreExposure Respiraiton Data before the exposures ("Day 0") 
respPreExposure<-read.csv("Data/Metabolic.Rates/Resp.pre.Exposure.csv", header=T, sep=",", na.string="NA", as.is=T) 
names(respPreExposure) # view the names of the data

# plot the PreExposure respiration rate 
par(mfrow=c(1,1)) #reset plot output
resp_PreExposure_plot <- ggplot(respPreExposure, aes(x = factor(respPreExposure$Date), y = respPreExposure$LpcResp_alpha0.4_all)) +
  geom_boxplot(alpha = 0.1) +
  geom_point(size = 2, shape = 21) +
  theme(text = element_text(size = 18),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none")
print(resp_PreExposure_plot + labs(y="Respiration rate rate µg O2 L-1 h-1 indiv-1", 
                                   x = "Date") + 
        ggtitle("Juvenile geoduck respirometry \ PreExposure"))

#Load Respiraiton Data for exposure 1 and 2
resp<-read.csv("Data/Metabolic.Rates/Resp.30d.Exposure.csv", header=T, sep=",", na.string="NA", as.is=T) 
names(resp) # view  names in the dataframe 

# seperate into experiment 1 and 2 for the 10-day and 6-day OA exposure
resp_EXP1 <- subset(resp, EXP.numb=="EXP1") #initial 10-day trial, subset entire dataset resp by column nsame "Exp.num" == Exp1
resp_EXP2 <- subset(resp, EXP.numb=="EXP2") #second 6-day trial, subset entire dataset resp by column nsame "Exp.num" == Exp2
resp_EXP2.0 <- subset(resp, EXP.numb=="EXP2") # same under a diff name (read why on next comment)
resp_EXP2.0 <-subset(resp_EXP2.0, Day!=0) # omit Day0 of the second exposure for graph on cumulative exposure to OA 

### EXP1 ####
# The following are condenced steps to merge a reproducible respiration rates from LoLin R ouputs.
# Test strength of automated LoLin Script for nine constants each with the "Reference" data
# Reference = each resp value computed with a visial criteria and default constants  (alpha = 0.2, untruncated); adjusted to acheive peak emp distribution  of regressions
# Automated ouputs = at nine total settings of alpha= 0.2,0.4 and0.6 at three different truncations (all, 10-20 minutes, 10-25 minutes)

# AUTOMATED RESP RATES WITH "LOLIN" PACKAGE OUTPUTS
# First step - nine LoLin outputs to reference; choose constants with least respiration values outside of 95% Loess CI with Reference
# linear regression with reference plot
plot(resp_EXP1[,1],resp_EXP1[,5], main= "Ref vs. alpha0.4_all") # linear regression with the reference 
summary(lm(resp_EXP1[,1]~resp_EXP1[,5])) #Adjusted R-squared:  0.3905

# label with row numbers  - shows values outside of CI interval in loess curve
ggplot(resp_EXP1, aes(x = resp_EXP1[,1], y = resp_EXP1[,5])) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = resp_EXP1$ID), position = position_nudge(y = -0.01)) 
# outside of loess curve = 57,52,3,2,1,76,50,96,31,5,29,17,70,72,68,56,94

# call all points outside of the CI and create a new dataframe with these values and all non LoLin outputs
newdata_resp_EXP1_ALL <- data.frame(resp_EXP1$Date, resp_EXP1[,c(1:10)]) # new dataframe with first 11 columns, Date + nine automated outputs
newdata_resp_EXP1_ALL_1 <-newdata_resp_EXP1_ALL[c(57,52,3,6,17,2,1,76,50,96,31,5,29,70,72,68,56,94), ] # only call points (rows) outside loess in first regression

# plotted all linear regression for each automated output with the reference - strongest correlation at alpha = 0.6 truncation at 10-20 minutes
plot(newdata_resp_EXP1_ALL_1[,2],newdata_resp_EXP1_ALL_1[,10]) # strongest correlation was with alpha = 0.6 truncation at 10-20 minutes 
summary(lm(newdata_resp_EXP1_ALL_1[,10]~newdata_resp_EXP1_ALL_1[,2]))# strongest relationship with Reference - adj R-squared=0.7235 

#Merge a FINAL COLUMN as "Finalresp" for both datasets both datasets 
resp_EXP1$FINALresp <- resp_EXP1$LpcResp_alpha0.4_all.csv # base alpha 0.4, no truncation 
resp_EXP1$FINALresp[c(57,52,3,6,17,2,1,76,50,96,31,5,29,70,72,68,56,94)] <- resp_EXP1$LpcResp_alpha0.6_min10.20.csv[c(57,52,3,6,17,2,1,76,50,96,31,5,29,70,72,68,56,94)] #inserted new rows from alpha = 0.6 10-20 min for points outside loess curve

# test correlation of Finalresp with the Ref
plot(resp_EXP1$Resp_individually_all.csv,resp_EXP1$FINALresp, main= "Ref vs. FINALresp") # plot the relationship
summary(lm(resp_EXP1[,1]~resp_EXP1$FINALresp)) # summarize linear model - Multiple R-squared:  0.8784,	Adjusted R-squared:  0.8771 
ggplot(resp_EXP1, aes(x = resp_EXP1[,1], y = resp_EXP1$FINALresp)) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = resp_EXP1$ID), position = position_nudge(y = -0.01)) # ggplot with loess CI

# PLOTS
#treatments and time (with PreExposure added)
colnames(respPreExposure)[1] <- "FINALresp" # rename the calc resp values in PreExposure table to match the resp_EXP1
respPreExposure$Init.treat <- c("Ambient") # name the treatement as ambient for the preexposure "day 0" data
respPreExposure$Day <- 0 # zero dafults correctly plotted position - renamed in ggplot script at "prebasal"
resp_EXP1_condensed <- resp_EXP1[,c(11,12,13,14,15,17,21)] # call the columns in respPreExposure to rbind (must be the same)
resp_EXP1_ALL <- rbind(resp_EXP1_condensed, respPreExposure) # merge the two tables for the graph

#plot by exp 1 by treatment
Exp1.Fig.resp.A <- ggplot(resp_EXP1_ALL, aes(x = factor(resp_EXP1_ALL$Day), 
                                             y = resp_EXP1_ALL$FINALresp, 
                                             fill = resp_EXP1_ALL$Init.treat)) +
                    scale_fill_manual(values=c("white","gray1"), 
                                      labels=c("Ambient","Elevated")) +
                    geom_boxplot(alpha = 0.5, # color hue
                                 width=0.6, # boxplot width
                                 outlier.size=0,
                                 position = position_dodge(preserve = "single")) +
                    geom_point(pch = 19, position = position_jitterdodge(0.01), size=1) +
                    theme_classic() +
                    stat_summary(fun.y=mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..),
                                 width = 0.6, size=0.4, linetype = "dashed", position = position_dodge(preserve = "single")) +
                    theme(legend.position = c(0.55,0.9),legend.direction="horizontal", legend.title=element_blank(), 
                          axis.line = element_line(color = 'black'), #Set the axes color
                          axis.ticks.length=unit(0.2, "cm")) + #turn ticks inward
                    geom_vline(xintercept=c(1.5), linetype="dotted", size=1) +
                    scale_x_discrete(labels = c("0",2,5,8,10))    +
                    labs(y=expression("Respiration rate"~(~µg~O[2]*hr^{-1}*mm^{-1})), x=expression("Days"),fill= "")  
Exp1.Fig.resp.B <- Exp1.Fig.resp.A + theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=13,color="black"),
                                           axis.text.y = element_text(angle = 0, hjust = 0.5, size=13,color="black"),
                                           axis.line = element_line(color = 'black'),
                                           axis.title.x = element_text(size = 14),
                                           axis.title.y = element_text(size = 14))
Exp1.Fig.resp_FINAL <-  Exp1.Fig.resp.B + scale_y_continuous(limits = c(0, 0.65), expand = c(0, 0))
Exp1.Fig.resp_FINAL

# plot just treatment (without prePreExposure)
Exp1.Fig.resp_total <- ggplot(resp_EXP1, aes(x = factor(resp_EXP1$Init.treat), y = resp_EXP1$FINALresp, fill = resp_EXP1$Init.treat)) +
  geom_boxplot(alpha = 0.1, outlier.shape = 19,
               outlier.fill = "black", outlier.size = 1, lwd=0.2) + 
  geom_point(pch = 19, position = position_jitterdodge(0.05), size=1) +
  scale_color_grey() + scale_fill_grey() + theme_classic() +
  #geom_point(aes(fill = resp_EXP1$Treat1_Treat2), size = 2, shape = 21, position = position_jitterdodge(0.15)) +
  theme(legend.position = c(.9, .9), legend.text=element_text(size=8)) +
  stat_summary(fun.y=mean, geom="point", pch="+", size=3, position = position_jitterdodge(0.01)) +
  labs(y="Standard metabolic rate µg O2 L-1 h-1 indiv-1",  x = "Treatment", fill= "") 
Exp1.Fig.resp_total

# ANALYSIS
# Two-Way anova for respiration rate under Initial OA Exposure
hist(resp_EXP1$FINALresp) # view histogram of the final respiration data for experiment 1 - negative skew
shapiro.test(resp_EXP1$FINALresp) # shaprio test shows resp values are not normally distributed
shapiro.test(sqrt(resp_EXP1$FINALresp)) # square root transform for positive skew
EXP1.resp.sqrt <- (sqrt(resp_EXP1$FINALresp)) # call the transformation for the model EXP1.resp.sqrt
resp_EXP1$Day <- as.factor(resp_EXP1$Day) # convert day to character 
EXP1.resp.aov.mod <- aov(FINALresp ~ Init.treat * Day, data = resp_EXP1) # run anova on treatment and time
anova(EXP1.resp.aov.mod) # significant effect of  treatment 
# Levene's test for homogeneity 
leveneTest(EXP1.resp.aov.mod) # p 0.2236
# Shapiro test
EXP1.resp.mod.residuals <- residuals(object = EXP1.resp.aov.mod) # call residuals from the model
shapiro.test(x = EXP1.resp.mod.residuals) #  0.09055
# post-hoc
exp1.resp.ph <- lsmeans(EXP1.resp.aov.mod, pairwise ~  Init.treat * Day)# pariwise Tukey Post-hoc test between repeated treatments
exp1.resp.ph # view post hoc summary
E1.pairs.RESP.05 <- cld(exp1.resp.ph, alpha=.05, Letters=letters) #list pairwise tests and letter display p < 0.05
E1.pairs.RESP.05 #view results
E1.pairs.RESP.1 <- cld(exp1.resp.ph, alpha=.1, Letters=letters) #list pairwise tests and letter display p < 0.1
E1.pairs.RESP.1 #view results
# plot the residuals
par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(residuals(EXP1.resp.aov.mod)) #plot histogram of residuals
shapiro.test(residuals(EXP1.resp.aov.mod)) # residuals are normal
boxplot(residuals(EXP1.resp.aov.mod)) #plot boxplot of residuals
plot(fitted(EXP1.resp.aov.mod),residuals(EXP1.resp.aov.mod)) 
qqnorm(residuals(EXP1.resp.aov.mod)) # qqplot

# explore the effect
#summary tables to calculate the percent difference between resp in treatments
sumresp_EXP1 <- summarySE(resp_EXP1, 
                          measurevar="FINALresp", 
                          groupvars=c("Date","Init.treat")) # summary table of resp with Date and treatment
sumresp_EXP1_means <- summarySE(sumresp_EXP1, 
                                measurevar="FINALresp", 
                                groupvars=c("Init.treat")) # summarize previous table for overall treatment 

percentdiff <- ((sumresp_EXP1_means[1,3] - sumresp_EXP1_means[2,3])/sumresp_EXP1_means[1,3])*100 # calculate percent difference
percentdiff # 25% lower respiration rates in low pH

### EXP2 ####
par(mfrow=c(1,1)) #set plotting configuration

# AUTOMATED RESP RATES WITH "LOLIN" PACKAGE OUTPUTS
# First step - nine LoLin outputs to reference; choose constants with least respiration values outside of 95% Loess CI with Reference
# linear regression with reference plot
# Below  alpha = 0.4, no truncation for automated ouput - yielded the least respiration values outside of Loess CI with Reference
plot(resp_EXP2[,1],resp_EXP2[,5], main= "Ref vs. alpha0.4_all") # linear regression with the reference 
summary(lm(resp_EXP2[,1]~resp_EXP2[,5])) #Adjusted R-squared:  0.8002 

# label with row numbers  - shows values outside of CI interval in loess curve
ggplot(resp_EXP2, aes(x = resp_EXP2[,1], y = resp_EXP2[,5])) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = resp_EXP2$ID), position = position_nudge(y = .01)) 
# outside of loess curve = 114, 153,155,156,150,141,105,130,152,98,188
tail(resp_EXP1) # end of exposure 1 is row #96
numberID <- (c(114,153,155,156,150,141,105,130,152,98,188)) - 96 #subtract by 96 to get the actual row number in resp_exp2
# actuall row numbers outside loess cruve = 18 57 59 60 54 45  9 34 56  2 92

# plotted all linear regression for each automated output with the reference - strongest correlation at alpha = 0.6 truncation at 10-20 minutes
newdata_resp_EXP2_ALL <- data.frame(resp_EXP2$Date , resp_EXP2[,c(1:10)]) # new dataframe with first 11 columns, Date + nine automated outputs
newdata_resp_EXP2_ALL_1 <-newdata_resp_EXP2_ALL[c(18, 57, 59, 60 ,54 ,45 , 9 ,34 ,56,  2, 92), ]  # only call points (rows) outside loess in first regression

# plotted all linear regression for each automated output with the reference - strongest correlation at alpha = 0.6 truncation at 10-20 minutes
plot(newdata_resp_EXP2_ALL_1[,2],newdata_resp_EXP2_ALL_1[,11]) # alpha = 0.6 truncation at 10-25 minutes (each resp trial was 30 mins)
summary(lm(newdata_resp_EXP2_ALL_1[,11]~newdata_resp_EXP2_ALL_1[,2]))# strongest relationship with Reference - adj R-squared=0.7169 

#Merge a FINAL COLUMN as "Finalresp" for both datasets both datasets 
resp_EXP2$FINALresp <- resp_EXP2$LpcResp_alpha0.4_all.csv # base alpha 0.4, no truncation
resp_EXP2$FINALresp[c(18, 57, 59, 60 ,54 ,45 , 9 ,34 ,56,  2, 92)] <- resp_EXP2$LpcResp_alpha0.6_min10.25.csv[c(18, 57, 59, 60 ,54 ,45 , 9 ,34 ,56,  2, 92)] #inserted new rows from alpha = 0.6 10-25 min for points outside loess curve

# test relationship between Finalresp and the Ref
plot(resp_EXP2$FINALresp,resp_EXP2$Resp_individually_all.csv, main= "Ref vs. FINALresp")
summary(lm(resp_EXP2$Resp_individually_all.csv~resp_EXP2$FINALresp)) # summarize linear model - Multiple R-squared:  0.9261,	Adjusted R-squared:  0.9253 
ggplot(resp_EXP2, aes(x = resp_EXP2$Resp_individually_all.csv, y = resp_EXP2$FINALresp)) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = resp_EXP2$ID), position = position_nudge(y = -0.01)) # ggplot with loess CI

# subset dataset for Day 0 and days 2 4 and 6
resp_EXP2_2.4.6. <- subset(resp_EXP2, Day!=0) # subset out pre-exposure measurements on day 0 (day 24 of experiment)
resp_EXP2_d0 <- subset(resp_EXP2, Day==0) # subset of just day 0 data (for the plot)

# PLOTTING
resp_EXP2_d0$Treat1_Treat2 <- resp_EXP2_d0$Init.treat # Day 0 in Exp2 still has potential carry over from 2 treatments in intial exp, not 4 treatments in Exp2
resp_EXP2_merge <- rbind(resp_EXP2_d0, resp_EXP2_2.4.6.) # merge the two tables for the graph
#plot treatments and time (PreExposure added as day 0 data)
Exp2.Fig.resp.A <- ggplot(resp_EXP2_merge, aes(x = factor(resp_EXP2_merge$Day), y = resp_EXP2_merge$FINALresp, fill = resp_EXP2_merge$Treat1_Treat2)) +
                    scale_fill_manual(values=c("white", "white", "grey80", "gray1", "gray50", "gray1"), 
                                      labels=c("Ambient","Ambient × Ambient","Ambient × Elevated","Elevated","Elevated × Ambient","Elevated × Elevated")) +
                    geom_boxplot(alpha = 0.5, # color hue
                                 width=0.6, # boxplot width
                                 outlier.size=0, # make outliers small
                                 position = position_dodge(preserve = "single")) + 
                    geom_point(pch = 19, position = position_jitterdodge(0.01), size=1) +
                    theme_classic() + ylim(0, 0.6) +
                    #geom_point(aes(fill = resp_EXP2$Treat1_Treat2), size = 1.5, shape = 21, position = position_jitterdodge(0.05)) +
                    stat_summary(fun.y=mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), 
                                 width = 0.6, size=0.4, linetype = "dashed", position = position_dodge(preserve = "single")) +
                    theme(legend.position = c(0.55,0.96), legend.direction="horizontal", legend.title=element_blank()) +
                    geom_vline(xintercept=c(1.5), linetype="dotted", size=1) +
                    scale_x_discrete(labels = c("0",2,4,6)) +
                    geom_vline(xintercept=c(1.5), linetype="dotted", size=1) +
                    labs(y=expression("Respiration rate"~(~µg~O[2]*hr^{-1}*indiv^{-1})), x=expression("Days"), fill="") 
Exp2.Fig.resp.A # view the plot
Exp2.Fig.resp.B <- 
                    Exp2.Fig.resp.A + 
                    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=13,color="black"),
                          axis.text.y = element_text(angle = 0, hjust = 0.5, size=13,color="black"),
                          axis.line = element_line(color = 'black'),
                          axis.ticks.length=unit(0.2, "cm"),
                          axis.title.x = element_text(size = 14),
                          axis.title.y = element_text(size = 14)) +
                    scale_y_continuous(limits = c(0, 0.65), expand = c(0, 0))
Exp2.Fig.resp.B # view the plot

# ANALYSIS
# Three-Way anova for respiration rate under Secondary OA Exposure
hist(resp_EXP2_2.4.6.$FINALresp) # histogram looks positive skew
shapiro.test(resp_EXP2_2.4.6.$FINALresp) # not normally distributed
hist(sqrt(resp_EXP2_2.4.6.$FINALresp)) # view historgram of squre root transformation for positive skew
shapiro.test(sqrt(resp_EXP2_2.4.6.$FINALresp)) # square root transformed is normally distributed
EXP2.resp.sqrt <- (sqrt(resp_EXP2_2.4.6.$FINALresp))
resp_EXP2_2.4.6.$Day <- as.character(resp_EXP2_2.4.6.$Day) # covert day to character to run the model
EXP2.resp.aov.mod <- aov(FINALresp ~ Init.treat*Sec.treat*Day, data = resp_EXP2_2.4.6.) # run anova on treatment and time
anova(EXP2.resp.aov.mod) # significant effect of time and marginal effect from secondary treatment
# Levene's test for homogeneity 
leveneTest(EXP2.resp.aov.mod) # p 0.4541
# Shapiro test
EXP2.resp.mod.residuals <- residuals(object = EXP2.resp.aov.mod) # call residuals from the model
shapiro.test(x = EXP2.resp.mod.residuals) # 0.5754
#post-hoc for sig affect of time
exp2.resp.day <- lsmeans(EXP2.resp.aov.mod, pairwise ~ Init.treat*Sec.treat*Day) # pariwise Tukey Post-hoc test between repeated treatments
exp2.resp.day # view post hoc summary
E2.pairs.RESP.05 <- cld(exp2.resp.day, alpha=.05, Letters=letters) #list pairwise tests and letter display p < 0.05
E2.pairs.RESP.05 #view results - no pairwise differences for the entire model
E2.pairs.RESP.1 <- cld(exp2.resp.day, alpha=.1, Letters=letters) #list pairwise tests and letter display p < 0.05
E2.pairs.RESP.1 # view results
# plot the residuals
par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(residuals(EXP2.resp.aov.mod)) #plot histogram of residuals
boxplot(residuals(EXP2.resp.aov.mod)) #plot boxplot of residuals
plot(fitted(EXP2.resp.aov.mod),residuals(EXP2.resp.aov.mod)) 
qqnorm(residuals(EXP2.resp.aov.mod)) # qqplot

# trend to days 2 -6 for resp rate
#summary table based on the anova results of a significant effect of initial treatment
EXP2_sumRESP_means <- summarySE(resp_EXP2_2.4.6., 
                                     measurevar="FINALresp", 
                                     groupvars=c("Day")) # summarize previous table for overall Treat1_Treat2 
EXP2_sumRESP_means # view the table
percentdiff.RESP_means <- ((EXP2_sumRESP_means[3,3] - EXP2_sumRESP_means[1,3])/EXP2_sumRESP_means[1,3])*100 # calculate percent difference
percentdiff.RESP_means # 31.19829% increase in resp rate from day 2 to day 6 during secondary exposure


# t-test for differences at the end of exp1 (day 10) and start of exp 2 (day 0) by treatment
exp1_d10.resp  <- subset(resp_EXP1, Date=="20180724") #  resp  on Day 10 in Exp 1
t.test(exp1_d10.resp$FINALresp~exp1_d10.resp$Init.treat) # p-value = 0.08471; t-test shows marginal difference between treatment at end of exp 1

exp2_d0.resp <- subset(resp_EXP2, Date=="20180807") # starting resp  on Day 0 in Exp 2
t.test(exp2_d0.resp$FINALresp~exp2_d0.resp$Init.treat) # p-value = 0.01469; t-test shows significant difference between treatment at the start of Exp2

#ADD MODEL RESULTS TO THE PLOT
Exp2.Fig.resp_FINAL <- 
                    Exp2.Fig.resp.B + 
                    geom_segment(aes(x = .6, y = 0.55, xend = 1.4, yend = 0.55)) +
                    annotate("text", x="0", y=0.54, label = "**", size = 4) # t-test with p < 0.05 between treatments at day 0
Exp2.Fig.resp_FINAL

# TEST FOR EFFECTS OF THE FOUR TREATMENT GROUPS  FOR ALL DAYS
# anovas for signifcant between treatments for all days in EXP2 
Resp_EXP2_d2 <- subset(resp_EXP2, Day==2)
Resp_d2_EXP2 <- aov(FINALresp~Treat1_Treat2, data=Resp_EXP2_d2)
summary(Resp_d2_EXP2) # day 2 no difference between treatments

Resp_EXP2_d4 <- subset(resp_EXP2, Day==4)
Resp_d4_EXP2 <- aov(FINALresp~Treat1_Treat2, data=Resp_EXP2_d4)
summary(Resp_d4_EXP2) # day4 no difference between treatments

Resp_EXP2_d6 <- subset(resp_EXP2, Day==6)
Resp_d6_EXP2 <- aov(FINALresp~Treat1_Treat2, data=Resp_EXP2_d6)
summary(Resp_d6_EXP2) # day 6 no difference between treatments

Resp_EXP2_ALL <- subset(resp_EXP2, Day!=0)
Resp_alldays_EXP2 <- aov(FINALresp~Treat1_Treat2, data=Resp_EXP2_ALL)
summary(Resp_alldays_EXP2) # ALL days no difference between treatments


### Growth Data - Analysis, Graphs, Models  (summarized analysis from Stats_growth_analysis.R)#######################################

#Load Size Data
size<-read.csv("Data/Shell.Length/Length.30d.Exposure.csv", header=T, sep=",", na.string="NA", as.is=T) 
size <- tibble::rowid_to_column(size, "ID") # add unique rownumber column to ID animals
# DATASETS FOR EXPOSURES 1 AND 2
size_EXP1_with_PreExposure <- subset(size, Exp.Num=="Exp1") # all Exp1 data
size_EXP1 <- subset(size_EXP1_with_PreExposure, trial!="prebasal") # Exp1 without Day 0

size_EXP1_PreExposure <- subset(size_EXP1_with_PreExposure, Day=="prebasal") # Seperate Day 0 of Exp 1 for Figure
size_EXP1_PreExposure$Day <- "0" # name day as 0 (before it was "prebasal")
size_EXP1_all <- rbind(size_EXP1_PreExposure, size_EXP1) # merge two datasets for the first size figure Exp 1

size_EXP2 <- subset(size, Exp.Num=="Exp2") # all Exp 2
size_EXP2.d0 <- subset(size_EXP2, Day==0)
size_EXP2.d0$Treat1_Treat2 <- size_EXP2.d0$Init.Trt # merge column or initial treat as treat1_treat2 for Exp2 figure
size_EXP2.0 <-subset(size_EXP2, Day!=0) # Exp 2 without day 0 
size_EXP2_all <- rbind(size_EXP2.d0, size_EXP2.0) # merge two datasets for the first size figure Exp 2
size_Exp1.T<- merge(size_EXP1, size_EXP2, by=c("tank"))  #merge to obtained combined treatments from EXP2

inital_size <- subset(size_EXP1, Date=="20180716") # get starting size of indiivduals from first measurements
StartSize <- summarySE(inital_size, measurevar="shell_size", groupvars=c("Date")) #summary table for starting shell length = 5.077962 ± 0.6622871 (mean ± SD)
end_size <- subset(size_EXP1, Date=="20180724") # view the efinal size on Day 10 of exposure 1

# plot end of exp 1 Day 10 with treatments 
length_EXP1_Day10 <- ggplot(end_size, aes(x = treatment, y = shell_size, fill = treatment)) +
  geom_boxplot(alpha = 0.1) + scale_color_grey() + scale_fill_grey() + theme_classic() +
  ylim(2,8.2) +
  geom_point(pch = 19, position = position_jitterdodge(0.1), size=1) +
  #geom_point(aes(fill = treatment), size = 0.5, shape = 21, position = position_jitterdodge(0.05)) +
  stat_summary(fun.y=mean, geom="point", pch="+", size=3, position = position_jitterdodge(0.01)) +
  theme(legend.position = c(.92, .9), legend.text=element_text(size=8)) +
  labs(y="shell length", x = "Treatment", fill="") 
length_EXP1_Day10


### EXP1 ####
#plot treatments and time (with prePreExposure)
Exp1.Fig.size.A <- ggplot(size_EXP1_all, aes(x = factor(Day), y = shell_size, fill = treatment)) +
  theme_classic() +
  scale_fill_manual(values=c("white", "grey3"), labels=c("Ambient","Elevated")) +
  geom_boxplot(alpha = 0.5, # color hue
               width=0.6, # boxplot width
               outlier.size=0, # make outliers small
               position = position_dodge(preserve = "single")) + 
  geom_point(pch = 19, position = position_jitterdodge(.05), size=1) +
  stat_summary(fun.y=mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), 
               width = 0.6, size=0.4, linetype = "dashed", position = position_dodge(preserve = "single")) +
  theme(legend.position = c(0.55,0.96), legend.direction="horizontal", legend.title=element_blank()) +
  geom_vline(xintercept=c(1.5), linetype="dotted", size=1) +
  ylim(2,8.2) + 
  scale_x_discrete(limits=c("0",2,5,8,10)) +
  labs(y=expression("Shell length"~(mm)), x=expression("Days"))
Exp1.Fig.size.B <- Exp1.Fig.size.A + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=13,color="black"),
        axis.text.y = element_text(angle = 0, hjust = 0.5, size=13,color="black"),
        axis.line = element_line(color = 'black'),
        axis.ticks.length=unit(0.2, "cm"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  scale_y_continuous(limits = c(2, 8), expand = c(0, 0))
Exp1.Fig.size.B # view the plot

# Two-Way anova for shell size under Initial OA Exposure
hist(size_EXP1$shell_size) # view histogram - has negative skew
shapiro.test(size_EXP1$shell_size) # not normally distributed
# reflect sqr root transformation to minimize negative skew
max(size_EXP1$shell_size) # max value = 6.859
EXP1.size.sqrt <- sqrt((6.859 + 1) - size_EXP1$shell_size) # reflect and square root (reflect = [(max value + 1) - x]
hist(EXP1.size.sqrt) # seems to have solved the skewness in historgram
shapiro.test(EXP1.size.sqrt) # normally distributed
# run the model with transformed data
EXP1.size.aov.mod <- aov(shell_size ~ Init.Trt * Day, data = size_EXP1) # run anova on treatment and time
anova(EXP1.size.aov.mod) # significant effect of time; no effect from treatment
# Levene's test for homogeneity 
leveneTest(EXP1.size.aov.mod) # p 0.5609
# post-hoc
TukeyHSD(EXP1.size.aov.mod, "Day") # quick method
exp1.size.ph <- lsmeans(EXP1.size.aov.mod, pairwise ~  Init.Trt * Day) # pariwise Tukey Post-hoc test
exp1.size.ph # view post hoc summary diff between days 10 and 2
E1.pairs.SIZE.05 <- cld(exp1.size.ph, alpha=.05, Letters=letters) #list pairwise tests and letter display p < 0.05
E1.pairs.SIZE.05 #view results
# what is the percent change between days 2 and day 10 (since there is no diff between treatment - group all as a mean)
EXP1_size_Day2 <- size_EXP1 %>% filter(size_EXP1$Day==2) #dataset for day 2
EXP1_size_Day10 <- size_EXP1 %>% filter(size_EXP1$Day==10) # dataset for day 10
Days_2_and_10 <- rbind(EXP1_size_Day2, EXP1_size_Day10) # rbind both datasets
# table for mean shell length days 2 and 10
EXP1_size_table <- aggregate(Days_2_and_10$shell_size, list(Days_2_and_10$Day), mean)
EXP1_percentdiff_size <- (((EXP1_size_table[1,2])-(EXP1_size_table[2,2]))/(EXP1_size_table[2,2]))*100 # 3.6 % increase in shell length days 2 - 10
# plot the residuals
par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(residuals(EXP1.size.aov.mod)) #plot histogram of residuals
boxplot(residuals(EXP1.size.aov.mod)) #plot boxplot of residuals
plot(fitted(EXP1.size.aov.mod),residuals(EXP1.size.aov.mod)) 
qqnorm(residuals(EXP1.size.aov.mod)) # qqplot


Exp1.Fig.size_FINAL <- 
  Exp1.Fig.size.B +
  geom_segment(aes(x = 1.6, y = 7.3, xend = 2.4, yend = 7.3)) +
  geom_segment(aes(x = 2.6, y = 7.3, xend = 3.4, yend = 7.3)) + 
  geom_segment(aes(x = 3.6, y = 7.3, xend = 4.4, yend = 7.3)) +
  geom_segment(aes(x = 4.6, y = 7.3, xend = 5.4, yend = 7.3)) +
  annotate("text", x="2", y=7.2, label = "a", size = 4) + # t-test with p < 0.05 between treatments at day 0
  annotate("text", x="5", y=7.2, label = "ab", size = 4) + # add text to the graphic for posthoc letters - effect of time
  annotate("text", x="8", y=7.2, label = "ab", size = 4) + # add text to the graphic for posthoc letters - effect of time
  annotate("text", x="10", y=7.2, label = "b", size = 4) # add text to the graphic for posthoc letters - effect of time
Exp1.Fig.size_FINAL

### EXP2 ####
#PLOTTING
Exp2.Fig.size.A <- ggplot(size_EXP2_all, aes(x = factor(Day), y = shell_size, fill = Treat1_Treat2)) +
  theme_classic() +
  scale_fill_manual(values=c("white", "white", "grey80", "grey3", "gray50", "grey3"), 
                    labels=c("Ambient","Ambient × Ambient","Ambient × Elevated","Elevated","Elevated × Ambient","Elevated × Elevated")) +
  geom_boxplot(alpha = 0.5, # color hue
               width=0.6, # boxplot width
               outlier.size=0, # make outliers small
               position = position_dodge(preserve = "single")) + 
  geom_point(pch = 19, position = position_jitterdodge(.05), size=1) +
  stat_summary(fun.y=mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), 
               width = 0.6, size=0.4, linetype = "dashed", position = position_dodge(preserve = "single")) +
  theme(legend.position = c(0.55,0.96), legend.direction="horizontal", legend.title=element_blank()) +
  geom_vline(xintercept=c(1.5), linetype="dotted", size=1) +
  ylim(2,8.2) + 
  scale_x_discrete(labels = c("0",2,4,6)) +
  geom_vline(xintercept=c(1.5), linetype="dotted", size=1) + 
  labs(y=expression("Shell size"~(mm)), x=expression("Days"))
Exp2.Fig.size.FINAL <- Exp2.Fig.size.A  + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=13,color="black"),
        axis.text.y = element_text(angle = 0, hjust = 0.5, size=13,color="black"),
        axis.line = element_line(color = 'black'),
        axis.ticks.length=unit(0.2, "cm"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  scale_y_continuous(limits = c(2, 8), expand = c(0, 0))
Exp2.Fig.size.FINAL # view the plot

# ANALYSIS
# t-test for differences at the end of exp1 (day 10) and start of exp 2 (day 0) by treatment
exp1_d10.size <- subset(size_EXP1, Date=="20180724") # starting size on Day 10 in Exp 1
t.test(exp1_d10.size$shell_size~exp1_d10.size$Init.Trt) # p-value = 0.6083; t-test shows no difference between treatment at the start of Exp2

exp2_d0.size <- subset(size_EXP2, Date=="20180807") # starting size on Day 0 in Exp 2
t.test(exp2_d0.size$shell_size~exp2_d0.size$Init.Trt) # p-value = 0.2531; t-test shows significant difference between treatment at the start of Exp2

# Three-Way anova for shell size under Secondary OA Exposure
boxplot(size_EXP2.0$shell_size)
hist(size_EXP2.0$shell_size) # weak negative skew
shapiro.test(size_EXP2.0$shell_size) # not normally distributed
max(size_EXP2.0$shell_size) # max value = 7.764
EXP2.size.sqrt <- log((7.764 + 1) - size_EXP2.0$shell_size) # reflect and square root transform
hist(EXP2.size.sqrt) # view histogram of transformation 
shapiro.test(EXP2.size.sqrt) # even more non normal
EXP2.size.aov.mod <- aov(shell_size ~ Init.Trt*Sec.Trt* Day, data = size_EXP2.0) # run anova on treatment and time
anova(EXP2.size.aov.mod) # significant effect fromboth inital and secondary treatment
# Levene's test for homogeneity 
leveneTest(EXP2.size.aov.mod) # p 0.8418
# shapiro test (model residuals)
shapiro.test(residuals(EXP2.size.aov.mod))
# plot the residuals
par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(residuals(EXP2.size.aov.mod)) #plot histogram of residuals
boxplot(residuals(EXP2.size.aov.mod)) #plot boxplot of residuals
plot(fitted(EXP2.size.aov.mod),residuals(EXP2.size.aov.mod))
qqnorm(residuals(EXP2.size.aov.mod)) # qqplot

# post-hoc
exp2.size.ph <- lsmeans(EXP2.size.aov.mod, pairwise ~  Init.Trt*Sec.Trt*Day)# pariwise Tukey Post-hoc test between repeated treatments
exp2.size.ph # view post hoc summary
E2.pairs.SIZE.05 <- cld(exp2.size.ph, alpha=.05, Letters=letters) #list pairwise tests and letter display p < 0.05
E2.pairs.SIZE.05 #view results
# effect of initial exposure
exp2.size.ph.initial <- lsmeans(EXP2.size.aov.mod, pairwise ~  Init.Trt*Sec.Trt* Day)# pariwise Tukey Post-hoc test between repeated treatments
exp2.size.ph.initial # view post hoc summary
EXP2_perc_diff_InitTrmt <- ((5.820324 - 5.586145) / (5.820324)) *100
EXP2_perc_diff_InitTrmt
# effect of secondary exposure
exp2.size.ph.sec <- lsmeans(EXP2.size.aov.mod, pairwise ~  Sec.Trt)# pariwise Tukey Post-hoc test between repeated treatments
exp2.size.ph.sec  # view post hoc summary
EXP2_perc_diff_SecTrmt <- ((5.796007 - 5.610461) / (5.796007)) *100
EXP2_perc_diff_SecTrmt

# Assemble output plots
figure_2 <- ggarrange(Exp1.Fig.resp_FINAL, Exp1.Fig.size_FINAL,
                      ncol = 1, nrow = 2)
figure_2 # view the figure

figure_3 <- ggarrange(Exp2.Fig.resp_FINAL, Exp2.Fig.size.FINAL,
                      ncol = 1, nrow = 2)
figure_3 # view the figure


# Saving output plots
ggsave(file="Output/Supplem.Fig.conical.pH.temp.pdf", Supplem.Fig.conical.pH.temp, width = 12, height = 8, units = c("in"))
ggsave(file="Output/Output_Figure_2.pdf", figure_2, width = 12, height = 8, units = c("in"))
ggsave(file="Output/Output_Figure_3.pdf", figure_3, width = 12, height = 8, units = c("in"))


# 157 days post experiment in common garden heathstack

# Shell Size
size_157days_postEXP<-read.csv("Data/Shell.Length/Length.157d.post.csv", header=T, sep=",", na.string="NA", as.is=T) 
size_157days_postEXP <- na.omit(size_157days_postEXP)
size_157days_postEXP # look at the data
names(size_157days_postEXP) # look at names of data

# both Treat1_Treat2
size_table_Treat1Treat2 <- do.call(data.frame,aggregate(Length_mm ~ Treat1_Treat2,
                                                        data = size_157days_postEXP, function(x) c(mean = mean(x), se = std.error(x))))
size_table_Treat1Treat2 # view the table

# analysis
size_157days_postEXP.aov.mod <- aov(Length_mm ~ Init_treat*Sec_treat, data = size_157days_postEXP) # run anova on Treat1_Treat2 and time
anova(size_157days_postEXP.aov.mod) # significant effect of initial treatment
# plot the residuals and test with levene's test
par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
# Levene's test 
leveneTest(size_157days_postEXP.aov.mod) # p = 0.4317
# post-hoc
size_157days.size <- lsmeans(size_157days_postEXP.aov.mod, pairwise ~  Init_treat*Sec_treat)# pariwise Tukey Post-hoc test between repeated treatments
size_157days.size # view post hoc summary
size157days.pairs.SIZE.05 <- cld(size_157days.size, alpha=.05, Letters=letters) #list pairwise tests and letter display p < 0.05
size157days.pairs.SIZE.05 #view results
# hist qq residual diagnostic
hist(residuals(size_157days_postEXP.aov.mod)) #plot histogram of residuals
boxplot(residuals(size_157days_postEXP.aov.mod)) #plot boxplot of residuals
plot(fitted(size_157days_postEXP.aov.mod),residuals(size_157days_postEXP.aov.mod))
qqnorm(residuals(size_157days_postEXP.aov.mod)) # qqplot

#summary table based on the anova results of a significant effect of initial treatment
sumLENGTH_means.157days <- summarySE(size_157days_postEXP, 
                             measurevar="Length_mm", 
                             groupvars=c("Init_treat")) # summarize previous table for overall Treat1_Treat2 
sumLENGTH_means.157days # view the table
percentdiff.157days <- ((sumLENGTH_means.157days[2,3] - sumLENGTH_means.157days[1,3])/sumLENGTH_means.157days[2,3])*100 # calculate percent difference
percentdiff.157days # 5.8% greater shell length from animals initally exposed to low pH in initial exp trial

# significant effect graph
size_graph_INITIAL.157days <- ggplot(size_157days_postEXP, aes(x = factor(Init_treat), y = Length_mm, fill = Init_treat)) +
  theme_classic() +
  scale_fill_manual(values=c("white", "grey3"), 
                    labels=c("Ambient", "Elevated")) +
  geom_boxplot(alpha = 0.5, # color hue
               width=0.6, # boxplot width
               outlier.size=0, # make outliers small
               position = position_dodge(preserve = "single")) + 
  geom_point(pch = 19, position = position_jitterdodge(.05), size=1) +
  stat_summary(fun.y=mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), 
               width = 0.6, size=0.4, linetype = "dashed", position = position_dodge(preserve = "single")) +
  theme(legend.position = c(0.55,0.96), legend.direction="horizontal", legend.title=element_blank()) +
  ylim(4,13) + 
  scale_x_discrete(labels = c("Ambient","Elevated")) +
  labs(y=expression("Shell size"~(mm)), x=expression("Initial treatment"))
size_graph_INITIAL.157days

#Both treatments from secnondary exposure graph
size_graph_INITIAL.SECOND.157days <- ggplot(size_157days_postEXP, aes(x = factor(Treat1_Treat2), y = Length_mm, fill = Treat1_Treat2)) +
  theme_classic() +
  scale_fill_manual(values=c("white", "grey80",  "gray50", "grey3"), 
                    labels=c("Ambient × Ambient","Ambient × Elevated","Elevated × Ambient","Elevated × Elevated")) +
  geom_boxplot(alpha = 0.5, # color hue
               width=0.6, # boxplot width
               outlier.size=0, # make outliers small
               position = position_dodge(preserve = "single")) + 
  geom_point(pch = 19, position = position_jitterdodge(.05), size=1) +
  stat_summary(fun.y=mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), 
               width = 0.6, size=0.4, linetype = "dashed", position = position_dodge(preserve = "single")) +
  theme(legend.position = c(0.55,0.96), legend.direction="horizontal", legend.title=element_blank()) +
  ylim(4,13) + 
  scale_x_discrete(labels = c("Ambient × Ambient","Ambient × Elevated","Elevated × Ambient","Elevated × Elevated")) +
  labs(y=expression("Shell size"~(mm)), x=expression("Initial×Secondary treatment"))
size_graph_INITIAL.SECOND.157days

# Respiration
# call the whole record NOTE: initial data in first 5 minutes shows higher respriration rates
# than the rest of the record due to mixing, this leads to outliers in automated analysis
# used a truncated 10-20 minute record to pull representative data after vials were thorougly mixed
# line below is the whole record
# RESP.reference.resp157days.postEXP<-read.csv("Data/Metabolic.Rates/Resp.157d.post.all.csv", header=T, sep=",", na.string="NA", as.is=T) 

# call the truncated 5-20 minute record
RESP.reference.resp157days.postEXP<-read.csv("Data/Metabolic.Rates/Resp.157d.post.trunc.csv", header=T, sep=",", na.string="NA", as.is=T) 
SIZE.reference.resp157days.postEXP<-read.csv("Data/Metabolic.Rates/Resp.157d.post.size.reference.csv", header=T, sep=",", na.string="NA", as.is=T) 
RESP.CALC.157.days.postEXP <- merge(RESP.reference.resp157days.postEXP, SIZE.reference.resp157days.postEXP, by=c("Date","SDR_position", "RUN")) # merge the individual info (size, estimate number of larvae) by common columns 
RESP.CALC.157.days.postEXP # view new file

# from visual of Lolin plots - RUN1 C1, C4, and C5 were bad data, non linear and likely an error (air bubble)
x <- RESP.CALC.157.days.postEXP[-c(25,31,33),] # x = Resp data with out these points (stated above)
JUVresp_all <- x %>% 
  filter((substr(x$Notes, 1,9)) == "juveniles") # call only resp values of juveniles

JUVresp_RUN1 <- JUVresp_all %>%  filter(JUVresp_all$RUN == 1) # call run 1
JUVresp_RUN2 <- JUVresp_all %>%  filter(JUVresp_all$RUN == 2)  # call run 2

#blanks Run1 
JUVresp_blanks_RUN1 <- JUVresp_RUN1 %>% 
  filter(JUVresp_RUN1$Tank.ID == "Blank") # call only blanks
JUVresp_blankMEANS_RUN1 <- JUVresp_blanks_RUN1 %>% 
  summarise(mean_Lpc = mean(abs(Lpc)),mean_Leq = mean(abs(Leq)), mean_Lz = mean(abs(Lz))) # summarize the blanks into a mean value

#blanks Run2
JUVresp_blanks_RUN2 <- JUVresp_RUN2 %>% 
  filter(JUVresp_RUN2$Tank.ID == "Blank") # call only blanks
JUVresp_blankMEANS_RUN2 <- JUVresp_blanks_RUN2 %>% 
  summarise(mean_Lpc = mean(abs(Lpc)),mean_Leq = mean(abs(Leq)), mean_Lz = mean(abs(Lz))) # summarize the blanks into a mean value

# resp rates Run1
JUVresp_geoduck_RUN1 <- JUVresp_RUN1 %>% 
  filter(!is.na(length_number.individuals))

JUVresp_geoduck_RUN1$Resp_rate_ug.mol <-
  ((((((abs(JUVresp_geoduck_RUN1$Lpc)) - (JUVresp_blankMEANS_RUN1$mean_Lpc))*(4/1000))*(60))*31.998)/(JUVresp_geoduck_RUN1$length_number.individuals))
JUVresp_geoduck_RUN1$Resp_rate_ug.mol

# resp rates Run2
JUVresp_geoduck_RUN2 <- JUVresp_RUN2 %>% 
  filter(!is.na(length_number.individuals))

JUVresp_geoduck_RUN2$Resp_rate_ug.mol <-
  ((((((abs(JUVresp_geoduck_RUN2$Lpc)) - (JUVresp_blankMEANS_RUN2$mean_Lpc))*(4/2000))*(60))*32.998)/(JUVresp_geoduck_RUN2$length_number.individuals))
JUVresp_geoduck_RUN2$Resp_rate_ug.mol

# merge the two datasets
JUVresp_geoduck <- rbind(JUVresp_geoduck_RUN1,JUVresp_geoduck_RUN2)
# remove the two negative values in run 2
JUVresp_geoduck <- JUVresp_geoduck %>% filter(JUVresp_geoduck$Resp_rate_ug.mol > 0) # only resp rates over zero

#summary tables for both exposures
JUVresp_table_treatments_ALL <- JUVresp_geoduck %>%
  group_by(Treatment) %>% #group the dataset by BOTH INITIAL AND SECONDARY TREATMENT
  summarise(mean_resp = mean(Resp_rate_ug.mol),
            min_resp = min(Resp_rate_ug.mol),
            sd_resp = sd(Resp_rate_ug.mol),
            SEM = ((sd(Resp_rate_ug.mol))/sqrt(n())),
            count =n()) %>% # get the count by leaving n open
  arrange(desc(min_resp)) # makes table in descending order 
JUVresp_table_treatments_ALL # view table
# table for initial exposure
JUVresp_table_treatments_INITIAL <- JUVresp_geoduck %>%
  group_by(Treat.initial) %>% #group the dataset by INITIAL TREATMENT
  summarise(mean_resp = mean(Resp_rate_ug.mol),
            min_resp = min(Resp_rate_ug.mol),
            sd_resp = sd(Resp_rate_ug.mol),
            SEM = ((sd(Resp_rate_ug.mol))/sqrt(n())),
            count =n()) %>% # get the count by leaving n open
  arrange(desc(min_resp)) # makes table in descending order 
JUVresp_table_treatments_INITIAL # view table
# table for secondary exposure
JUVresp_table_treatments_SECONDARY<- JUVresp_geoduck %>%
  group_by(Treat.Secondary) %>% #group the dataset by SECONDARY TREATMENT
  summarise(mean_resp = mean(Resp_rate_ug.mol),
            min_resp = min(Resp_rate_ug.mol),
            sd_resp = sd(Resp_rate_ug.mol),
            SEM = ((sd(Resp_rate_ug.mol))/sqrt(n())),
            count =n()) %>% # get the count by leaving n open
  arrange(desc(min_resp)) # makes table in descending order 
JUVresp_table_treatments_SECONDARY # view table

# run the two way anova 
JUVresp.mod  <- aov(Resp_rate_ug.mol~Treat.initial*Treat.Secondary, data = JUVresp_geoduck)
anova(JUVresp.mod) # anova results
par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
# Levene's test
leveneTest(JUVresp.mod) # p = 0.5534
# pairwise post hoc
# post-hoc
resp_157days.size <- lsmeans(JUVresp.mod, pairwise ~  Treat.initial*Treat.Secondary)# pariwise Tukey Post-hoc test between repeated treatments
resp_157days.size # view post hoc summary
resp157days.pairs.SIZE.05 <- cld(size_157days.size, alpha=.05, Letters=letters) #list pairwise tests and letter display p < 0.05
resp157days.pairs.SIZE.05 #view results
# hist and qq residual diagnostics
hist(residuals(JUVresp.mod)) #plot histogram of residuals
boxplot(residuals(JUVresp.mod)) #plot boxplot of residuals
plot(fitted(JUVresp.mod),residuals(JUVresp.mod))
qqnorm(residuals(JUVresp.mod)) # qqplot

library(Rmisc)
sum_JUVresp_means <- summarySE(JUVresp_geoduck, 
                               measurevar="Resp_rate_ug.mol", 
                               groupvars=c("Treat.Secondary")) # summarize previous table for overall treatment 
sum_JUVresp_means # view the table
percentdiff.JUVresp <- ((sum_JUVresp_means[2,3] - sum_JUVresp_means[1,3])/sum_JUVresp_means[2,3])*100 # calculate percent difference
percentdiff.JUVresp # 52.37839% greater respiration rate from animals under secondary exposure to elevated conditions


# significant effect graph
JUVresp_geoduck_INITIAL.157days <- ggplot(JUVresp_geoduck, aes(x = factor(Treat.initial ), y = Resp_rate_ug.mol, fill = Treat.initial )) +
  theme_classic() +
  scale_fill_manual(values=c("white", "grey3"), 
                    labels=c("Ambient", "Elevated")) +
  geom_boxplot(alpha = 0.5, # color hue
               width=0.6, # boxplot width
               outlier.size=0, # make outliers small
               position = position_dodge(preserve = "single")) + 
  geom_point(pch = 19, position = position_jitterdodge(.05), size=1) +
  stat_summary(fun.y=mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), 
               width = 0.6, size=0.4, linetype = "dashed", position = position_dodge(preserve = "single")) +
  theme(legend.position = c(0.55,0.96), legend.direction="horizontal", legend.title=element_blank()) +
  ylim(0,3) + 
  scale_x_discrete(labels = c("Ambient","Elevated")) +
  labs(y=expression("Respiration rate"~(~µg~O[2]*hr^{-1}*mm^{-1})), x=expression("Initial treatment"))
JUVresp_geoduck_INITIAL.157days # view the graph

#Both treatments from secnondary exposure graph
JUVresp_geoduck_INITIAL.SECOND.157days <- ggplot(JUVresp_geoduck, aes(x = factor(Treatment), y = Resp_rate_ug.mol, fill = Treatment)) +
  theme_classic() +
  scale_fill_manual(values=c("white",  "grey80", "gray50", "grey3"), 
                    labels=c("Ambient × Ambient","Ambient × Elevated","Elevated × Ambient","Elevated × Elevated")) +
  geom_boxplot(alpha = 0.5, # color hue
               width=0.6, # boxplot width
               outlier.size=0, # make outliers small
               position = position_dodge(preserve = "single")) + 
  geom_point(pch = 19, position = position_jitterdodge(.05), size=1) +
  stat_summary(fun.y=mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), 
               width = 0.6, size=0.4, linetype = "dashed", position = position_dodge(preserve = "single")) +
  theme(legend.position = c(0.55,0.96), legend.direction="horizontal", legend.title=element_blank()) +
  ylim(0,3) + 
  scale_x_discrete(labels = c("Ambient × Ambient","Ambient × Elevated","Elevated × Ambient","Elevated × Elevated")) +
  labs(y=expression("Respiration rate"~(~µg~O[2]*hr^{-1}*mm^{-1})),  x = "Initial×Secondary treatment", fill= "") 
JUVresp_geoduck_INITIAL.SECOND.157days # view the graph

figure_supplementary_157d <- ggarrange(size_graph_INITIAL.SECOND.157days,
                                       JUVresp_geoduck_INITIAL.SECOND.157days,
                                       ncol = 1, nrow = 2)
figure_supplementary_157d # view the figure

# Saving output plots
ggsave(file="Output/Fig.4.resp.size.157d.post.pdf", figure_supplementary_157d, width = 14, height = 8, units = c("in"))
