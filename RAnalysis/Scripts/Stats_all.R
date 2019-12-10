#Title: Juvenile Repeat Exposure Experiment 2018
#Project: FFAR
#Author: HM Putnam & Sam Gurr
#Edit by: Sam Gurr
#Date Last Modified: 20190620
#See Readme file for details

rm(list=ls())
# Install packages if not already in your library
if ("dplyr" %in% rownames(installed.packages()) == 'FALSE') install.packages('dplyr') 
if ("ggplot2" %in% rownames(installed.packages()) == 'FALSE') install.packages('ggplot2') 
if ("ggpubr" %in% rownames(installed.packages()) == 'FALSE') install_github('ggpubr') 
if ("Rmisc" %in% rownames(installed.packages()) == 'FALSE') install.packages('Rmisc') 
if ("plotrix" %in% rownames(installed.packages()) == 'FALSE') install.packages('plotrix') 
if ("lsmeans" %in% rownames(installed.packages()) == 'FALSE') install.packages('lsmeans') 
if ("gridExtra" %in% rownames(installed.packages()) == 'FALSE') install.packages('gridExtra') 
if ("reshape" %in% rownames(installed.packages()) == 'FALSE') install.packages('reshape') 
if ("multcompView" %in% rownames(installed.packages()) == 'FALSE') install.packages('multcompView') 
if ("lmtest" %in% rownames(installed.packages()) == 'FALSE') install.packages('lmtest') 
if ("car" %in% rownames(installed.packages()) == 'FALSE') install.packages('car') 
if ("ggpubr" %in% rownames(installed.packages()) == 'FALSE') install.packages('ggpubr') 


# Load packages and pacage version/date/import/depends info
library(dplyr)          # Version 0.7.6, Packaged: 2018-06-27, Depends: R (>= 3.1.2)Imports: assertthat (>= 0.2.0), bindrcpp (>= 0.2.0.9000), glue (>=1.1.1), magrittr (>= 1.5), methods, pkgconfig (>= 2.0.1), R6(>= 2.2.2), Rcpp (>= 0.12.15), rlang (>= 0.2.0), tibble (>=1.3.1), tidyselect (>= 0.2.3), utils
library(ggplot2)        # Version 2.2.1, Packaged: 2016-12-30, Depends: R (>= 3.1)Imports: digest, grid, gtable (>= 0.1.1), MASS, plyr (>= 1.7.1),reshape2, scales (>= 0.4.1), stats, tibble, lazyeval
library(ggpubr)         # Version: 0.1.8 Date: 2018-08-30, Depends: R (>= 3.1.0), ggplot2, magrittrImports: ggrepel, grid, ggsci, stats, utils, tidyr, purrr, dplyr(>=0.7.1), cowplot, ggsignif, scales, gridExtra, glue, polynom
library(Rmisc)          # Version: 1.5 Packaged: 2013-10-21, Depends: lattice, plyr
library(plotrix)        # Version: 3.7-4, Date/Publication: 2018-10-03
library(lsmeans)        # Version: 2.27-62, Date/Publication: 2018-05-11, Depends: methods, R (>= 3.2)
library(gridExtra)      # Version: 2.3, Date/Publication: 2017-09-09, Imports: gtable, grid, grDevices, graphics, utils
library(reshape)        # Version: 0.8.7, Date/Publication: 2017-08-06, Depends: R (>= 2.6.1) Imports: plyr
library(multcompView)   # Version: 0.1-7, Date/Publication: 2015-07-31, Imports: grid
library(Rmisc)
library(lmtest)
library(car)
library(ggpubr)

#Required Data files
# ----Conical Chemistry (APEX data)
#20180724_Apex_Data_Output.csv
#20180805_Apex_Data_Output.csv
#20180814_Apex_Data_Output.csv
# ----Heath Tray Chemistry (discrete probe data)
#Flow.rates.csv
#pH_Calibration_Files (tris data)
#Seawater_chemistry_table_Output_All.csv
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
# flow <- flow %>% dplyr::filter(Notes %in% "good")
flow$Day_ID <- paste(flow$Day, flow$Sample.ID, sep ="_") 

flow_summary <-flow %>% 
  dplyr::summarise(mean_LPM= mean(LPM),
            max_LPM = max(LPM),
            min_LPM = min(LPM),
            sd_LPM = sd(LPM),
            SEM = ((sd(LPM))/sqrt(n())),
            count = n()) %>% # get the count by leaving n open
  dplyr::arrange(desc(min_LPM)) # makes table in descending order 
flow_summary # view table

# exposure 1 flow rate
EXP1 <- subset(flow, Exp.num=="EXP1") #initial 10-day trial, subset entire dataset resp by column nsame "Exp.num" == Exp1
flow_EXP1 <- subset(EXP1, Day!=0) # ommit day 0
# ambient common garden
AMB <- subset(flow, Exp.num=="AMB") #ambeint common garden
# exposure 2 flow rate
EXP2 <- subset(flow, Exp.num=="EXP2") #second 6-day trial, subset entire dataset resp by column nsame "Exp.num" == Exp2
flow_EXP2 <- subset(EXP2, Day!=0) # ommit day 0

flow_EXP1_2 <- rbind(flow_EXP1, flow_EXP2) # bind exp1 and 2, day 0 already ommited

# EXP1 
# flow exp by treatment ---------------- #
flow_EXP1 # all "good and "adjusted" data - occasionally recorded multiple times a day to adjust flow 
flow_EXP1.treat <- summarySE(flow_EXP1, measurevar="LPM", groupvars=c("Day_ID", "Treatment_1_2", "Sample.ID")) # summarize by day treament and ID
flow_EXP1.treat # view table - gives mean values if recorded muliple times in that day or simply shows the single value recorded
flow_EXP1.treat.2 <- summarySE(flow_EXP1.treat, measurevar="LPM", groupvars=c("Treatment_1_2"))
flow_EXP1.treat.2 # stats by treatment
# Ambient common garden ---------------- #
# flow exp by treatment
flow_EXP1.amb <- summarySE(AMB, measurevar="LPM", groupvars=c("Treatment")) # summary by treatment
flow_EXP1.amb # view table
# EXP2 summary  ---------------- #
# flow by treatment
flow_EXP2.treat <- summarySE(flow_EXP2, measurevar="LPM", groupvars=c("Sec.treat")) # summary by treatment (did not measure multiple times a day)
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
chem<-read.csv("Output/Seawater_chemistry_table_Output_All.csv", header=T, sep=",", na.string="NA", as.is=T) 
chem # view the file
chem.exp <-subset(chem, Treatment!="na") #remove na - na often set as the treatment for samples of the sump

chem.exp1 <-chem.exp[52:131,] # exposure 1 without Day 0 (20180715 - 20180724)
chem.exp1$Exposure <- "Exp1"
chem.exp1$tank.name <- substr(chem.exp1$Tank, start = 10, stop = 13) # new column for tank name without date

chem.exp2 <- chem.exp[157:204,] # exposure 2 without Day 0 (20180808 - 20180813)
chem.exp2$Exposure <- "Exp2"
chem.exp2$tank.name <- substr(chem.exp2$Tank, start = 10, stop = 13) # new column for tank name without date

# test for tray differences --------------------------------------------------------------------------------- #
#Exp 1 ---------------------------------------- # seawater treatment (4 trays each)
chem.exp1.LOW <- chem.exp1 %>% filter(Treatment=="Low")
chem.exp1.AMBIENT <- chem.exp1 %>% filter(Treatment=="Ambient")
## TWO-way anovas to explore tray effects (4 trays in exposure 1)
summary(aov(pH~tank.name, data=chem.exp1.LOW)) # no diff in pH between FOUR trays - LOW treatment
summary(aov(TA~tank.name, data=chem.exp1.LOW)) # no diff in TA between FOUR trays - LOW treatment
summary(aov(pCO2~tank.name, data=chem.exp1.LOW)) # no diff in pCO2 between FOUR trays - LOW treatment
summary(aov(pH~tank.name, data=chem.exp1.AMBIENT)) # no diff in pH between FOUR trays - AMBIENT treatment
summary(aov(Salinity~tank.name, data=chem.exp1.AMBIENT)) # no diff in TA between FOUR trays - AMBIENT treatment
summary(aov(Temperature~tank.name, data=chem.exp1.AMBIENT)) # no diff in pCO2 between FOUR trays - AMBIENT treatment
#Exp 2 ---------------------------------------- # seawater treatment (4 trays each) NOT combined treatment effect
chem.exp2.LOW <- chem.exp2 %>% filter(Treatment=="Low")
chem.exp2.AMBIENT <- chem.exp2 %>% filter(Treatment=="Ambient")
# TWO-way anovas to explore tray effects (4 trays in exposure 1)
summary(aov(pH~tank.name, data=chem.exp2.LOW)) # no diff in pH between FOUR trays - LOW treatment
summary(aov(TA~tank.name, data=chem.exp2.LOW)) # no diff in TA between FOUR trays - LOW treatment
summary(aov(pCO2~tank.name, data=chem.exp2.LOW)) # no diff in pCO2 between FOUR trays - LOW treatment
summary(aov(pH~tank.name, data=chem.exp2.AMBIENT)) # no diff in pH between FOUR trays - AMBIENT treatment
summary(aov(Salinity~tank.name, data=chem.exp2.AMBIENT)) # no diff in TA between FOUR trays - AMBIENT treatment
summary(aov(Temperature~tank.name, data=chem.exp2.AMBIENT)) # no diff in pCO2 between FOUR trays - AMBIENT treatment
#Exp 2 ---------------------------------------- # experimental treatment (initial and seccondary exposure
# Tray ID Key:
# ambient × ambient = "H2_B", "H1_B"
# ambient × low = "H2_T", "H1_T"
# low × low = "H3_B", "H0_B"
# low × ambient = "H3_T", "H0_T"
chem.exp2.LOW.LOW <-  chem.exp2.LOW %>% dplyr::filter(tank.name %in% c("H3_B", "H0_B")) # low × low treat
chem.exp2.AMBIENT.LOW <- chem.exp2.LOW %>% dplyr::filter(tank.name %in% c("H2_T", "H1_T")) # ambient × low treat
chem.exp2.AMBIENT.AMBIENT <- chem.exp2.AMBIENT %>% dplyr::filter(tank.name %in% c("H2_B", "H1_B")) # ambient × ambient
chem.exp2.LOW.AMBIENT <- chem.exp2.AMBIENT %>% dplyr::filter(tank.name %in% c("H3_T", "H0_T")) # low × ambient
# t.tests for to verify if treatments are the same under secondary expsure...
# NOTE: we already found no tray diffs in seawater treatment (n=4 trays treatment-1); this looks at trays specific to the initial×secoandry treatment
# AMBIENT × AMBIENT
t.test(pH~tank.name, data=chem.exp2.AMBIENT.AMBIENT) # no diff in pH between TWO trays - AMBIENT × AMBIENT treatment
t.test(TA~tank.name, data=chem.exp2.AMBIENT.AMBIENT) # no diff in TA between TWO trays - AMBIENT × AMBIENT treatment
t.test(pCO2~tank.name, data=chem.exp2.AMBIENT.AMBIENT) # no diff in pCO2 between TWO trays - AMBIENT × AMBIENT treatment
t.test(Salinity~tank.name, data=chem.exp2.AMBIENT.AMBIENT) # no diff in Salinity between TWO trays - AMBIENT × AMBIENT treatment
t.test(Temperature~tank.name, data=chem.exp2.AMBIENT.AMBIENT) # no diff in Temperature between TWO trays - AMBIENT × AMBIENT treatment
# AMBIENT × Low
t.test(pH~tank.name, data=chem.exp2.AMBIENT.LOW) # no diff in pH between TWO trays - AMBIENT × Low treatment
t.test(TA~tank.name, data=chem.exp2.AMBIENT.LOW) # no diff in TA between TWO trays - AMBIENT × Low treatment
t.test(pCO2~tank.name, data=chem.exp2.AMBIENT.LOW) # no diff in pCO2 between TWO trays - AMBIENT × Low treatment
t.test(Salinity~tank.name, data=chem.exp2.AMBIENT.LOW) # no diff in Salinity between TWO trays - AMBIENT × Low treatment
t.test(Temperature~tank.name, data=chem.exp2.AMBIENT.LOW) # no diff in Temperature between TWO trays - AMBIENT × Low treatment
# Low × AMBIENT
t.test(pH~tank.name, data=chem.exp2.LOW.AMBIENT) # no diff in pH between TWO trays - Low × AMBIENT treatment
t.test(TA~tank.name, data=chem.exp2.LOW.AMBIENT) # no diff in TA between TWO trays - Low × AMBIENT treatment
t.test(pCO2~tank.name, data=chem.exp2.LOW.AMBIENT) # no diff in pCO2 between TWO trays - Low × AMBIENT treatment
t.test(Salinity~tank.name, data=chem.exp2.LOW.AMBIENT) # no diff in Salinity between TWO trays - Low × AMBIENT treatment
t.test(Temperature~tank.name, data=chem.exp2.LOW.AMBIENT) # no diff in Temperature between TWO trays - Low × AMBIENT treatment
# Low × LOW
t.test(pH~tank.name, data=chem.exp2.LOW.LOW) # no diff in pH between TWO trays - Low × LOW treatment
t.test(TA~tank.name, data=chem.exp2.LOW.LOW) # no diff in TA between TWO trays - Low × LOW treatment
t.test(pCO2~tank.name, data=chem.exp2.LOW.LOW) # no diff in pCO2 between TWO trays - Low × LOW treatment
t.test(Salinity~tank.name, data=chem.exp2.LOW.AMBIENT) # no diff in Salinity between TWO trays - Low × AMBIENT treatment
t.test(Temperature~tank.name, data=chem.exp2.LOW.AMBIENT) # no diff in Temperature between TWO trays - Low × AMBIENT treatment
# RESULTS = NO DIFFERENCES IN CARBONATE (AND SALINITY AND TEMP) CHEMSITRY BETWEEN 
# TRAYS BY SEAWATER CONDITION AND EXPERIMENTAL TREATMENT

# bind both exposures for cumulative table
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

########################
# RESPIRATION DATA #####
# - Analysis, Graphs, Models  (summarized analysis from Stats_growth_analysis.R)

#Load PreExposure Respiraiton Data before the exposures ("Day 0") 
respPreExposure<-read.csv("Data/Metabolic.Rates/Resp.pre.Exposure.csv", header=T, sep=",", na.string="NA", as.is=T) 
# load 30d exposure data
resp<-read.csv("Data/Metabolic.Rates/Resp.30d.Exposure.csv", header=T, sep=",", na.string="NA", as.is=T) 
# seperate into experiment 1 and 2 for the 10-day and 6-day OA exposure
resp_EXP1 <- subset(resp, EXP.numb=="EXP1") #initial 10-day trial, subset entire dataset resp by column nsame "Exp.num" == Exp1
resp_EXP2 <- subset(resp, EXP.numb=="EXP2") #second 6-day trial, subset entire dataset resp by column nsame "Exp.num" == Exp2
resp_EXP2.0 <- subset(resp, EXP.numb=="EXP2") # same under a diff name (read why on next comment)
resp_EXP2.0 <-subset(resp_EXP2.0, Day!=0) # omit Day0 of the second exposure for graph on cumulative exposure to OA 

par(mfrow=c(1,1)) #reset plot margins
# plot the PreExposure respiration rate 
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

tapply(respPreExposure$LpcResp_alpha0.4_all, respPreExposure$Date, mean) # mean = 0.2886166
tapply(respPreExposure$LpcResp_alpha0.4_all, respPreExposure$Date, sd) # sd = 0.16198
# RESPIRATION RATES Initial Exposure ----------------------------------------------------------------------------

# review the file "Stats_resp_analysis" the criterion used to determine the resp data used below
par(mfrow=c(1,1)) #set plotting configuration

# FIRST CURVE FIT = alpha 0.4 and no truncation - had the strongest r^2 regressed with reference AND least NUMBER OF values outside of the loess fit
plot(resp_EXP1[,1],resp_EXP1[,5], main= "Ref vs. alpha0.4_all") # linear regression with the reference 
summary(lm(resp_EXP1[,1]~resp_EXP1[,5])) #Adjusted R-squared:  0.3905
ggplot(resp_EXP1, aes(x = resp_EXP1[,1], y = resp_EXP1[,5])) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = resp_EXP1$ID), position = position_nudge(y = -0.01)) 
# values outside of loess curve 57,52,3,2,1,76,50,96,31,5,29,17,70,72,68,56,94
# create a new dataframe with these values and all non LoLin outputs
newdata_resp_EXP1_ALL <- data.frame(resp_EXP1$Date, resp_EXP1[,c(1:10)]) # new dataframe with first 11 columns, Date + nine automated outputs
newdata_resp_EXP1_ALL_1 <-newdata_resp_EXP1_ALL[c(57,52,3,6,17,2,1,76,50,96,31,5,29,70,72,68,56,94), ] # only call points (rows) outside loess in first regression
# plotted all linear regression for each automated output with the reference - strongest correlation at alpha = 0.6 truncation at 10-20 minutes
plot(newdata_resp_EXP1_ALL_1[,2],newdata_resp_EXP1_ALL_1[,10]) # strongest correlation was with alpha = 0.6 truncation at 10-20 minutes 
summary(lm(newdata_resp_EXP1_ALL_1[,10]~newdata_resp_EXP1_ALL_1[,2]))# strongest relationship with Reference - adj R-squared=0.7235 
# Merge a FINAL COLUMN as "Finalresp" for both datasets both datasets 
resp_EXP1$FINALresp <- resp_EXP1$LpcResp_alpha0.4_all.csv # base alpha 0.4, no truncation 
resp_EXP1$FINALresp[c(57,52,3,6,17,2,1,76,50,96,31,5,29,70,72,68,56,94)] <- resp_EXP1$LpcResp_alpha0.6_min10.20.csv[c(57,52,3,6,17,2,1,76,50,96,31,5,29,70,72,68,56,94)] #inserted new rows from alpha = 0.6 10-20 mins
# SECOND CURVE FIT = alpha 0.6 and truncation at 10-20 min
plot(resp_EXP1$Resp_individually_all.csv,resp_EXP1$FINALresp, main= "Ref vs. FINALresp") # plot the relationship
summary(lm(resp_EXP1[,1]~resp_EXP1$FINALresp)) # summarize linear model - Multiple R-squared:  0.8784,	Adjusted R-squared:  0.8771 
ggplot(resp_EXP1, aes(x = resp_EXP1[,1], y = resp_EXP1$FINALresp)) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = resp_EXP1$ID), position = position_nudge(y = -0.01)) # ggplot with loess CI

# ANALYSIS
# Two-Way anova for respiration rate under Initial OA Exposure
hist(resp_EXP1$FINALresp) # view histogram of the final respiration data for experiment 1 - negative skew
# model
resp_EXP1$Day <- as.factor(resp_EXP1$Day) # convert day to character 
EXP1.resp.aov.mod <- aov(FINALresp ~ Init.treat * Day, data = resp_EXP1) # run anova on treatment and time
anova(EXP1.resp.aov.mod) # significant effect of  treatment 
# Levene's test 
leveneTest(EXP1.resp.aov.mod) # p 0.2236
# Shapiro test
EXP1.resp.mod.residuals <- residuals(object = EXP1.resp.aov.mod) # call residuals from the model
shapiro.test(x = EXP1.resp.mod.residuals) # residuals are normal p-value = 0.09055
# hist qq residual diagnostic
par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(residuals(EXP1.resp.aov.mod)) #plot histogram of residuals
boxplot(residuals(EXP1.resp.aov.mod)) #plot boxplot of residuals
plot(fitted(EXP1.resp.aov.mod),residuals(EXP1.resp.aov.mod)) 
qqnorm(residuals(EXP1.resp.aov.mod)) # qqplot 

# post-hoc
exp1.resp.ph <- lsmeans(EXP1.resp.aov.mod, pairwise ~  Init.treat * Day)# pariwise Tukey Post-hoc test between repeated treatments
exp1.resp.ph # view post hoc summary
E1.pairs.RESP.05 <- cld(exp1.resp.ph, alpha=.05, Letters=letters) #list pairwise tests and letter display p < 0.05
E1.pairs.RESP.05 #view results
E1.pairs.RESP.1 <- cld(exp1.resp.ph, alpha=.1, Letters=letters) #list pairwise tests and letter display p < 0.1
E1.pairs.RESP.1 #view results

#summary tables to calculate the percent difference between treatment
sumresp_EXP1 <- summarySE(resp_EXP1, 
                          measurevar="FINALresp", 
                          groupvars=c("Date","Init.treat")) # summary table of resp with Date and treatment
sumresp_EXP1_means <- summarySE(sumresp_EXP1, 
                                measurevar="FINALresp", 
                                groupvars=c("Init.treat")) # summarize previous table for overall treatment 

percentdiff <- ((sumresp_EXP1_means[1,3] - sumresp_EXP1_means[2,3])/sumresp_EXP1_means[1,3])*100 # calculate percent difference
percentdiff # 25% lower respiration rates in low pH

# PLOTS
#treatments and time (with PreExposure added)
colnames(respPreExposure)[1] <- "FINALresp" # rename the calc resp values in PreExposure table to match the resp_EXP1
respPreExposure$Init.treat <- c("Ambient") # name the treatement as ambient for the preexposure "day 0" data
resp_EXP1_condensed <- resp_EXP1[,c(11,12,13,14,15,17,21)] # call the columns in respPreExposure to rbind (must be the same)
resp_EXP1_ALL <- rbind(resp_EXP1_condensed, respPreExposure) # merge the two tables for the graph
resp_EXP1_ALL$Day <- as.numeric(resp_EXP1_ALL$Day) # make Day numberic for the order in figure
resp_EXP1_ALL$Day[97:104] <- 0 # make pre data 0 
#typeof(respPreExposure$Day )
#resp_EXP1_ALL$Day[98:104] <- 0
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

# plot just treatment (without prePreExposure) - view the 25% difference between treatments
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

# RESPIRATION RATES Secondary Exposure ----------------------------------------------------------------------------

# review the file "Stats_resp_analysis" the criterion used to determine the resp data used below
par(mfrow=c(1,1)) #set plotting configuration

# FIRST CURVE FIT = alpha 0.4 and no truncation - had the strongest r^2 regressed with reference AND least NUMBER OF values outside of the loess fit
ggplot(resp_EXP2, aes(x = resp_EXP2[,1], y = resp_EXP2[,5])) + geom_point() + geom_smooth(method = 'loess') + geom_text(aes(label = resp_EXP2$ID), position = position_nudge(y = .01))
# values outside of loess curve 
numberID <- (c(98,105,113,114,130, 141,146,147,150,152,153,155,156,188)) - 96 # to obtain the actual order (EXP1 was 96 rows)
numberID # actuall number of ID 2  9 17 18 34 45 50 51 54 56 57 59 60 92
# subset resp_EXP2 for a new data file
newdata_resp_EXP2_ALL <- data.frame(resp_EXP2$Date , resp_EXP2[,c(1:10)])
newdata_resp_EXP2_ALL_1 <-newdata_resp_EXP2_ALL[c(2,  9, 17, 18, 34, 45, 50, 51, 54, 56, 57, 59, 60, 92), ] 
# SECOND CURVE FIT = alpha 0.6 and truncation at 10 - 25 mins
plot(newdata_resp_EXP2_ALL_1[,2],newdata_resp_EXP2_ALL_1[,11])
summary(lm(newdata_resp_EXP2_ALL_1[,11]~newdata_resp_EXP2_ALL_1[,2])) # Adjusted R-squared: 0.7145 

# assgin new  "FINAL" column base as the alpha 0.4 with no truncation as a base
head(resp_EXP2) # alpha 0.4 with no truncation = resp_EXP2[,5]
resp_EXP2$FINALresp <- resp_EXP2[,5]
# Note: FINALresp is column # 18 in resp_EXP2; as resp_EXP2[,18]
head(resp_EXP2) # alpha 0.6 10-25mins = resp_EXP2[,10]
resp_EXP2[c(2,  9, 17, 18, 34, 45, 50, 51, 54, 56, 57, 59, 60, 92),21] <- resp_EXP2[c(2,  9, 17, 18, 34, 45, 50, 51, 54, 56, 57, 59, 60, 92), 10]

# check the strength of Finalresp to the reference; RESULT: Multiple R-squared:  0.9464,	Adjusted R-squared:  0.9458 
plot(resp_EXP2[,1],resp_EXP2$FINALresp, main= "Ref vs. FINALresp")
summary(lm(resp_EXP2[,1]~resp_EXP2$FINALresp))       
ggplot(resp_EXP2, aes(x = resp_EXP2[,1], y = resp_EXP2$FINALresp)) +
  geom_point() +
  geom_smooth(method = 'loess') +
  geom_text(aes(label = resp_EXP2$ID), position = position_nudge(y = -0.01))

# subset dataset for Day 0 and days 2 4 and 6
resp_EXP2_2.4.6 <- subset(resp_EXP2, Day!=0) # subset out pre-exposure measurements on day 0 (day 24 of experiment)

# ANALYSIS
# t-test for differences at the end of exp1 (day 10) and start of exp 2 (day 0) by treatment
exp2_d0.resp <- subset(resp_EXP2, Date=="20180807") # starting resp  on Day 0 in Exp 2
t.test(exp2_d0.resp$FINALresp~exp2_d0.resp$Init.treat) # t = 2.9495, df = 18.709, p-value = 0.008326; t-test shows significant difference between treatment at the start of Exp2

# Three-way ANOVA (not transformed)
# normality test - data tranformation
hist(resp_EXP2_2.4.6$FINALresp) # positive or right skewed - need to transform the data
# model
resp_EXP2_2.4.6$Day <- as.factor(resp_EXP2_2.4.6$Day) # treat day as a character 
EXP2.ANOVA <- aov(FINALresp ~ Init.treat*Sec.treat*Day, data = resp_EXP2_2.4.6) # run anova on treatment and time
anova(EXP2.ANOVA) # no sig effects
# Levene's test 
leveneTest(EXP2.ANOVA) ## Levene's test for homogeneity 
# Shapiro test for residual variance
shapiro.test(residuals(EXP2.ANOVA)) # p-value = 0.00429; slight deviation from normality
# hist qq residual diagnostic (not transformed)
par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(residuals(EXP2.ANOVA)) # historgram normal, one oulier is apparent
boxplot(residuals(EXP2.ANOVA)) #plot boxplot of residuals - appears driven by a single outlier
plot(fitted(EXP2.ANOVA),residuals(EXP2.ANOVA)) #display residuals versus fitter, normal QQ plot, leverage plot
qqnorm(residuals(EXP2.ANOVA)) # QQ plot


# Identify whether outliers resolve slight deviation (via Shapiro-wilk)
# and if patterns in ANOVA model (or lack thereof) are stable after omission
boxplot(resp_EXP2_2.4.6$FINALresp) # view the boxplot of the shell size data
outliers.resp2 <- boxplot(resp_EXP2_2.4.6$FINALresp, plot=FALSE)$out # call outliers from the boxplot
resp.2_NO_OUTLIERS <- resp_EXP2_2.4.6[-which(resp_EXP2_2.4.6$FINALresp %in% outliers.resp2),] # remove outliers from the data, call new dataframe
boxplot(resp.2_NO_OUTLIERS$FINALresp) # view boxplot of shellsize without outliers
# Three-Way anova for shell size under Secondary OA Exposure (WITHOUT OUTLIERS)
# run the model
EXP2.resp.aov.mod_OUTLIERS_REMOVED <- aov(FINALresp ~ Init.treat*Sec.treat*Day, data = resp.2_NO_OUTLIERS) # run anova on treatment and time
anova(EXP2.resp.aov.mod_OUTLIERS_REMOVED) # no significant results
# Levene's test for homogeneity 
leveneTest(EXP2.resp.aov.mod_OUTLIERS_REMOVED) # p 0.9224
# shapiro test (model residuals)
shapiro.test(residuals(EXP2.resp.aov.mod_OUTLIERS_REMOVED)) # not normal, p-value = 0.001305
# hist qq residual diagnostic
par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(residuals(EXP2.resp.aov.mod_OUTLIERS_REMOVED)) #plot histogram of residuals
boxplot(residuals(EXP2.resp.aov.mod_OUTLIERS_REMOVED)) #plot boxplot of residuals
plot(fitted(EXP2.resp.aov.mod_OUTLIERS_REMOVED),residuals(EXP2.resp.aov.mod_OUTLIERS_REMOVED))
qqnorm(residuals(EXP2.resp.aov.mod_OUTLIERS_REMOVED)) # qqplot
# summary - visual inspection of the hitogram and boxplot of model residuals identified outliers as the 
# likely cause for devisiton from normality in shapiro wilk test. Ommision resolved normality assumpt.
# but anova results remained the same with no significant effects. 

# post-hoc
exp2.resp.ph <- lsmeans(EXP2.ANOVA.TRANS, pairwise ~  Day)# pariwise Tukey Post-hoc test between repeated treatments
exp2.resp.ph # view post hoc summary
E1.pairs.RESP.05 <- cld(exp2.resp.ph, alpha=.05, Letters=letters) #list pairwise tests and letter display p < 0.05
E1.pairs.RESP.05 # NO PAIRWISE DIFFERENCES
E1.pairs.RESP.1 <- cld(exp2.resp.ph, alpha=.1, Letters=letters) #list pairwise tests and letter display p < 0.1
E1.pairs.RESP.1 # maringal differences between elevated (day 5) and ambient (days 2 and 5)

# PLOT 
resp_EXP2_d0 <- subset(resp_EXP2, Day==0) # subset of just day 0 data (for the plot)
resp_EXP2_d0$Treat1_Treat2 <- resp_EXP2_d0$Init.treat # Day 0 in Exp2 still has potential carry over from 2 treatments in intial exp, not 4 treatments in Exp2
resp_EXP2_merge <- rbind(resp_EXP2_d0, resp_EXP2_2.4.6) # merge the two tables for the graph
#plot treatments and time (PreExposure added as day 0 data)
Exp2.Fig.resp.A <- ggplot(resp_EXP2_merge, aes(x = factor(resp_EXP2_merge$Day), y = resp_EXP2_merge$FINALresp, fill = resp_EXP2_merge$Treat1_Treat2)) +
                    scale_fill_manual(values=c("white", "white", "grey80", "gray1", "gray50", "gray1"), 
                                      labels=c("Ambient","Ambient × Ambient","Ambient × Elevated","Elevated","Elevated × Ambient","Elevated × Elevated")) +
                    geom_boxplot(alpha = 0.5, # color hue
                                 width=0.6, # boxplot width
                                 outlier.size=0, # make outliers small
                                 position = position_dodge(preserve = "single")) + 
                    geom_point(pch = 19, position = position_jitterdodge(0.01), size=1) +
                    theme_classic() + ylim(0, 0.75) +
                    #geom_point(aes(fill = resp_EXP2$Treat1_Treat2), size = 1.5, shape = 21, position = position_jitterdodge(0.05)) +
                    stat_summary(fun.y=mean, geom = "errorbar", aes(ymax = ..y.., ymin = ..y..), 
                                 width = 0.6, size=0.4, linetype = "dashed", position = position_dodge(preserve = "single")) +
                    theme(legend.position = c(0.55,0.96), legend.direction="horizontal", legend.title=element_blank()) +
                    geom_vline(xintercept=c(1.5), linetype="dotted", size=1) +
                    scale_x_discrete(labels = c("0",2,4,6)) +
                    geom_vline(xintercept=c(1.5), linetype="dotted", size=1) +
                    labs(y=expression("Respiration rate"~(~µg~O[2]*hr^{-1}*indiv^{-1})), x=expression("Days"), fill="") 
Exp2.Fig.resp.A # view the plot
Exp2.Fig.resp_FINAL <- 
                    Exp2.Fig.resp.A + 
                    theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=13,color="black"),
                          axis.text.y = element_text(angle = 0, hjust = 0.5, size=13,color="black"),
                          axis.line = element_line(color = 'black'),
                          axis.ticks.length=unit(0.2, "cm"),
                          axis.title.x = element_text(size = 14),
                          axis.title.y = element_text(size = 14)) +
                    scale_y_continuous(limits = c(0, 0.75), expand = c(0, 0))
Exp2.Fig.resp_FINAL # view the plot

#####################
### Growth Data #####
# - Analysis, Graphs, Models  (summarized analysis from Stats_growth_analysis.R)
#Load Size Data
size<-read.csv("Data/Shell.Length/Length.30d.Exposure.csv", header=T, sep=",", na.string="NA", as.is=T) 
size <- tibble::rowid_to_column(size, "ID") # add unique rownumber column to ID animals
# DATASETS FOR EXPOSURES 1 AND 2
size_EXP1_with_PreExposure <- subset(size, Exp.Num=="Exp1") # all Exp1 data
size_EXP1 <- subset(size_EXP1_with_PreExposure, trial!="prebasal") # Exp1 without Day 0

size_EXP1_PreExposure <- subset(size_EXP1_with_PreExposure, Day=="prebasal") # Seperate Day 0 of Exp 1 for Figure
tapply(size_EXP1_PreExposure$shell_size, size_EXP1_PreExposure$run, mean) # 4.255246
tapply(size_EXP1_PreExposure$shell_size, size_EXP1_PreExposure$run, sd) # 0.8479893

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
Exp1.Fig.size_FINAL <- Exp1.Fig.size.A + 
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size=13,color="black"),
        axis.text.y = element_text(angle = 0, hjust = 0.5, size=13,color="black"),
        axis.line = element_line(color = 'black'),
        axis.ticks.length=unit(0.2, "cm"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  scale_y_continuous(limits = c(2, 8), expand = c(0, 0))
Exp1.Fig.size_FINAL # view the plot

# Two-Way anova for shell size under Initial OA Exposure
hist(size_EXP1$shell_size) # view histogram
# run the model 
size_EXP1$Day <- as.factor(size_EXP1$Day) # treat day as a character 
EXP1.size.aov.mod <- aov(shell_size ~ Init.Trt * Day, data = size_EXP1) # run anova on treatment and time
anova(EXP1.size.aov.mod) # significant effect of time; no effect from treatment
# test model residuals
shapiro.test(residuals(EXP1.size.aov.mod)) # not normally distributed - must transform data
# Levene's test for homogeneity 
leveneTest(EXP1.size.aov.mod) # p 0.5609
# hist qq residual diagnostic
par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(residuals(EXP1.size.aov.mod)) #plot histogram of residuals
boxplot(residuals(EXP1.size.aov.mod)) #plot boxplot of residuals
plot(fitted(EXP1.size.aov.mod),residuals(EXP1.size.aov.mod))
qqnorm(residuals(EXP1.size.aov.mod)) # qqplot

# Two-Way anova (transformed)
# reflect sqr root transformation to minimize negative skew
max(size_EXP1$shell_size) # max value = 6.859
EXP1.size.sqrt <- sqrt((6.859 + 1) - size_EXP1$shell_size) # reflect and square root (reflect = [(max value + 1) - x]
hist(EXP1.size.sqrt) # seems to have solved the skewness in historgram
# run the model on transformed data
EXP1.size.aov.modTRANS <- aov(EXP1.size.sqrt ~ Init.Trt * Day, data = size_EXP1) # run anova on treatment and time
anova(EXP1.size.aov.modTRANS) # ANOVA results are the same as the raw data
# test model residuals
shapiro.test(residuals(EXP1.size.aov.modTRANS)) # normal p-value = 0.1394, resolved normality
# Levene's test for homogeneity 
leveneTest(EXP1.size.aov.modTRANS) # p 0.4352
# hist qq residual diagnostic
par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(residuals(EXP1.size.aov.modTRANS)) #plot histogram of residuals
boxplot(residuals(EXP1.size.aov.modTRANS)) #plot boxplot of residuals
plot(fitted(EXP1.size.aov.modTRANS),residuals(EXP1.size.aov.modTRANS)) 
qqnorm(residuals(EXP1.size.aov.modTRANS)) # qqplot
# summary - visual inspection/diagnosis of model residuals appear normal and homoskedastic 
# although assumptions did not pass shapiro-wilk;
# an example of a transformed to resolve normality had NO effect on the anova model outcome 
# keep the untransformed data considering 1) visual insepction of model residuals 2) no change to model outcome after transformation

# post-hoc
TukeyHSD(EXP1.size.aov.mod, "Day") # quick method
exp1.size.ph <- lsmeans(EXP1.size.aov.mod, pairwise ~  Day) # pariwise Tukey Post-hoc test
exp1.size.ph # view post hoc summary diff between days 10 and 2
E1.pairs.SIZE.05 <- cld(exp1.size.ph, alpha=.05, Letters=letters) #list pairwise tests and letter display p < 0.05
E1.pairs.SIZE.05 #view results
E1.pairs.SIZE.1 <- cld(exp1.size.ph, alpha=.1, Letters=letters) #list pairwise tests and letter display p < 0.05
E1.pairs.SIZE.1 #view results

# what is the percent change between days 2 and day 10 (since there is no diff between treatment - group all as a mean)
EXP1_size_Day2 <- size_EXP1 %>% filter(size_EXP1$Day==2) #dataset for day 2
EXP1_size_Day10 <- size_EXP1 %>% filter(size_EXP1$Day==10) # dataset for day 10
Days_2_and_10 <- rbind(EXP1_size_Day2, EXP1_size_Day10) # rbind both datasets
# table for mean shell length days 2 and 10
EXP1_size_table <- aggregate(Days_2_and_10$shell_size, list(Days_2_and_10$Day), mean)
EXP1_percentdiff_size <- (((EXP1_size_table[1,2])-(EXP1_size_table[2,2]))/(EXP1_size_table[2,2]))*100 # 3.6 % increase in shell length days 2 - 10

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
hist(size_EXP2.0$shell_size) # histogram
EXP2.size.aov.mod <- aov(shell_size ~ Init.Trt*Sec.Trt* Day, data = size_EXP2.0) # run anova on treatment and time
anova(EXP2.size.aov.mod) # significant effect fromboth inital and secondary treatment
# shapiro test (model residuals)
shapiro.test(residuals(EXP2.size.aov.mod)) # slight deviation, p-value = 0.002602
# Levene's test for homogeneity 
leveneTest(EXP2.size.aov.mod) # p 0.8418
# hist qq residual diagnostic
par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(residuals(EXP2.size.aov.mod)) #plot histogram of residuals
boxplot(residuals(EXP2.size.aov.mod)) #plot boxplot of residuals
plot(fitted(EXP2.size.aov.mod),residuals(EXP2.size.aov.mod))
qqnorm(residuals(EXP2.size.aov.mod)) # qqplot

# Three-Way anova (transformed) under Secondary OA Exposure
hist(size_EXP2.0$shell_size) # weak negative skew
max(size_EXP2.0$shell_size) # max value = 7.764
# transformations
EXP2.size.sqrt <- sqrt((7.764 + 1) - size_EXP2.0$shell_size) # reflect and square root (reflect = [(max value + 1) - x]
EXP2.size.log <- log((7.764 + 1) - size_EXP2.0$shell_size) # reflect log
EXP2.size.cubrt <- ((7.764 + 1) - size_EXP2.0$shell_size)^(1/3) # reflect cube root
# run the model for each and check shaprio wilk of residuals
EXP2.size.aov.SQRT <- aov(EXP2.size.sqrt ~ Init.Trt*Sec.Trt* Day, data = size_EXP2.0) # anova SQRT trans 
summary(EXP2.size.aov.SQRT) # same model effects
EXP2.size.aov.LOG <- aov(EXP2.size.log ~ Init.Trt*Sec.Trt* Day, data = size_EXP2.0) # anova LOG trans
summary(EXP2.size.aov.LOG) # same model effects
EXP2.size.aov.CBRT <- aov(EXP2.size.cubrt ~ Init.Trt*Sec.Trt* Day, data = size_EXP2.0) # anova CBRT trans
summary(EXP2.size.aov.CBRT) # same model effects
# shapiro test (model residuals)
shapiro.test(residuals(EXP2.size.aov.SQRT)) #  SQRT trans not normally distributed, p-value = 0.0002674
shapiro.test(residuals(EXP2.size.aov.LOG)) # LOG trans not normally distributed, p-value = 2.289e-11
shapiro.test(residuals(EXP2.size.aov.CBRT)) # CBRT trans not normally distributed, p-value = 2.177e-06
# not responsive to transformation - omit outliers and test data without transformation
# identify outliers
boxplot(size_EXP2.0$shell_size) # view the boxplot of the shell size data
outliers <- boxplot(size_EXP2.0$shell_size, plot=FALSE)$out # call outliers from the boxplot
size_NO_OUTLIERS <- size_EXP2.0[-which(size_EXP2.0$shell_size %in% outliers),] # remove outliers from the data, call new dataframe
boxplot(size_NO_OUTLIERS$shell_size) # view boxplot of shellsize without outliers
# Three-Way anova for shell size under Secondary OA Exposure (WITHOUT OUTLIERS)
hist(size_NO_OUTLIERS$shell_size) # weak negative skew
EXP2.size.aov.mod_OUTLIERS_REMOVED <- aov(shell_size ~ Init.Trt*Sec.Trt* Day, data = size_NO_OUTLIERS) # run anova on treatment and time
anova(EXP2.size.aov.mod_OUTLIERS_REMOVED) # same model effects
# Levene's test for homogeneity 
leveneTest(EXP2.size.aov.mod_OUTLIERS_REMOVED) # p 0.9224
# shapiro test (model residuals)
shapiro.test(residuals(EXP2.size.aov.mod_OUTLIERS_REMOVED)) # not normal, p-value = 0.001305
# hist qq residual diagnostic
par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(residuals(EXP2.size.aov.mod_OUTLIERS_REMOVED)) #plot histogram of residuals
boxplot(residuals(EXP2.size.aov.mod_OUTLIERS_REMOVED)) #plot boxplot of residuals
plot(fitted(EXP2.size.aov.mod_OUTLIERS_REMOVED),residuals(EXP2.size.aov.mod_OUTLIERS_REMOVED))
qqnorm(residuals(EXP2.size.aov.mod_OUTLIERS_REMOVED)) # qqplot
# summary - visual inspection/diagnosis of model residuals appear normal and homoskedastic 
# although assumptions did not pass shapiro-wilk...
# transformations did not resolve normality and had NO effect on the anova model outcome 

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
# shapiro test (model residuals)
shapiro.test(residuals(size_157days_postEXP.aov.mod)) #  normal residuals p-value = 0.8263
# Levene's test for homogeneity 
leveneTest(size_157days_postEXP.aov.mod) # p 0.4317
# hist qq residual diagnostic
par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(residuals(size_157days_postEXP.aov.mod)) #plot histogram of residuals
boxplot(residuals(size_157days_postEXP.aov.mod)) #plot boxplot of residuals
plot(fitted(size_157days_postEXP.aov.mod),residuals(size_157days_postEXP.aov.mod))
qqnorm(residuals(size_157days_postEXP.aov.mod)) # qqplot
# plot and test model for heteroscedasticity
plot(lm(size_157days_postEXP.aov.mod))
bptest(lm(size_157days_postEXP.aov.mod))  # Breusch-Pagan test p-value = 0.7411

# post-hoc
size_157days.size <- lsmeans(size_157days_postEXP.aov.mod, pairwise ~  Init_treat*Sec_treat)# pariwise Tukey Post-hoc test between repeated treatments
size_157days.size # view post hoc summary
size157days.pairs.SIZE.05 <- cld(size_157days.size, alpha=.05, Letters=letters) #list pairwise tests and letter display p < 0.05
size157days.pairs.SIZE.05 #view results

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

# Descriptive statistics
res <- desc_statby(size_157days_postEXP, measure.var = "Length_mm",
                   grps = c("Treat1_Treat2"))
head(res[, 1:14]) # all output
head(res[, c(1:6,9,10)]) # targetted output for boxplot stats

size_graph_INITIAL.SECOND.157days.VIOLIN <- ggviolin(size_157days_postEXP, "Treat1_Treat2", "Length_mm",
              color = "Treat1_Treat2", 
              #palette =c("#00AFBB", "#FC4E07", "#00AFBB", "#FC4E07"),
              palette =c("grey80", "grey60",  "gray35", "grey3"),
              add = "jitter", shape = "Treat1_Treat2")
size_graph_INITIAL.SECOND.157days.VIOLIN + geom_bracket(
  xmin = c("Ambient_Ambient","Low_Low", "Ambient_Low"), 
  xmax = c("Ambient_Low", "Low_Ambient", "Low_Low"),
  y.position = c(13, 13,13.2), label = c("", "", "post-hoc, P < 0.05"),
  tip.length = c(0.02)
)



# Default plot
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
x <- RESP.CALC.157.days.postEXP[-c(25,31,33),] # x = Resp data without these points (stated above)
JUVresp_all <- x %>% 
  filter((substr(x$Notes, 1,9)) == "juveniles") # call resp values 
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
  filter(!is.na(length_number.individuals)) # remove nas from the data
JUVresp_geoduck_RUN1$Resp_rate_ug.mol <-
  ((((((abs(JUVresp_geoduck_RUN1$Lpc)) - (JUVresp_blankMEANS_RUN1$mean_Lpc))*(4/1000))*(60))*31.998)/(JUVresp_geoduck_RUN1$length_number.individuals)) # subtract from the mean blank value of the run, convert units, standardize by mean individual
JUVresp_geoduck_RUN1$Resp_rate_ug.mol

# resp rates Run2
JUVresp_geoduck_RUN2 <- JUVresp_RUN2 %>% 
  filter(!is.na(length_number.individuals)) # remove nas from the data
JUVresp_geoduck_RUN2$Resp_rate_ug.mol <-
  ((((((abs(JUVresp_geoduck_RUN2$Lpc)) - (JUVresp_blankMEANS_RUN2$mean_Lpc))*(4/2000))*(60))*32.998)/(JUVresp_geoduck_RUN2$length_number.individuals)) # subtract from the mean blank value of the run, convert units, standardize by mean individual
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

# Two Way anova
JUVresp.mod  <- aov(Resp_rate_ug.mol~Treat.initial*Treat.Secondary, data = JUVresp_geoduck)
anova(JUVresp.mod) # anova results

# shapiro test (model residuals)
shapiro.test(residuals(JUVresp.mod)) #  normal residuals p-value = 0.5265
# Levene's test for homogeneity 
leveneTest(JUVresp.mod) # p = 0.5534
# hist qq residual diagnostic
par(mfrow=c(1,3)) #set plotting configuration
par(mar=c(1,1,1,1)) #set margins for plots
hist(residuals(JUVresp.mod)) #plot histogram of residuals
boxplot(residuals(JUVresp.mod)) #plot boxplot of residuals
plot(fitted(JUVresp.mod),residuals(JUVresp.mod))
qqnorm(residuals(JUVresp.mod)) # qqplot
# plot and test model for heteroscedasticity
plot(lm(JUVresp.mod))
bptest(lm(JUVresp.mod))  # Breusch-Pagan test p-value = 0.2961

# post-hoc
resp_157days.size <- lsmeans(JUVresp.mod, pairwise ~  Treat.initial*Treat.Secondary)# pariwise Tukey Post-hoc test between repeated treatments
resp_157days.size # view post hoc summary
resp157days.pairs.SIZE.05 <- cld(size_157days.size, alpha=.05, Letters=letters) #list pairwise tests and letter display p < 0.05
resp157days.pairs.SIZE.05 #view results

library(Rmisc)
sum_JUVresp_means <- summarySE(JUVresp_geoduck, 
                               measurevar="Resp_rate_ug.mol", 
                               groupvars=c("Treat.Secondary")) # summarize previous table for overall treatment 
sum_JUVresp_means # view the table
percentdiff.JUVresp <- ((sum_JUVresp_means[2,3] - sum_JUVresp_means[1,3])/sum_JUVresp_means[2,3])*100 # calculate percent difference
percentdiff.JUVresp # 52.37839% greater respiration rate from animals under secondary exposure to elevated conditions


# significant effect graph
JUVresp_geoduck_INITIAL.157days <- ggplot(JUVresp_geoduck, aes(x = factor(Treat.Secondary ), y = Resp_rate_ug.mol, fill = Treat.Secondary )) +
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

# violin plot of data
JUVresp_geoduck_INITIAL.SECOND.157days.VIOLIN <- ggviolin(JUVresp_geoduck, "Treatment", "Resp_rate_ug.mol",
                                                     color = "Treatment", 
                                                     order = c("Ambient_Ambient", "Elevated_Ambient", 
                                                               "Ambient_Elevated","Elevated_Elevated"),
                                                     #palette =c("#00AFBB", "#FC4E07", "#00AFBB", "#FC4E07"),
                                                     palette =c("grey80", "grey60",  "gray35", "grey3"),
                                                     add = "jitter", shape = "Treatment")
JUVresp_geoduck_INITIAL.SECOND.157days.VIOLIN <- add_summary(JUVresp_geoduck_INITIAL.SECOND.157days.VIOLIN, fun = "mean_se") # add mean and standard error
            
JUVresp_geoduck_INITIAL.SECOND.157days.VIOLIN + geom_bracket(
  xmin = c("Ambient_Ambient","Ambient_Elevated", "Elevated_Ambient"), 
  xmax = c("Elevated_Ambient", "Elevated_Elevated", "Ambient_Elevated"),
  y.position = c(2.5, 2.5,2.6), label = c("", "", "post-hoc, P < 0.05"),
  tip.length = c(0.02)
)


figure_supplementary_157d <- ggarrange(size_graph_INITIAL.SECOND.157days,
                                       JUVresp_geoduck_INITIAL.SECOND.157days,
                                       ncol = 1, nrow = 2)
figure_supplementary_157d # view the figure

# Saving output plots
ggsave(file="Output/Fig.4.resp.size.157d.post.pdf", figure_supplementary_157d, width = 14, height = 8, units = c("in"))
