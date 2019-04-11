# Geoduck Conditioning
# Title: SDR_LoLinR.R
# Supported by: FFAR
# This script was written by Sam J Gurr in Fall 2018
# Contact: samuel_gurr@uri.edu

rm(list=ls())

# Install packages if not already in your library
if ("plyr" %in% rownames(installed.packages()) == 'FALSE') install.packages('plyr') 

# Load packages and pacage version/date/import/depends info
library(plyr)           # Version 1.8.4, Packaged: 2016-06-07, Depends: R (>= 3.1.0) Imports: Rcpp (>= 0.11.0)


setwd("C:/Users/samjg/Documents/My_Projects/Juvenile_geoduck_OA/RAnalysis/Data/SDR_data/All_data_csv") #set working directory
main<-getwd()


# path used in the loop to each file name indicated in Table_files (matrix of filenames created below)
path<-"Cumulative_output/" #the location of all your respiration files

# make a table for all the filenames to call
table_o_files <- data.frame(matrix(nrow = 10))
table_o_files$rownumber <- c(1,2,3,4,5,6,7,8,9,10)
table_o_files$name <- c("Cumulative_resp_reference_all.csv",
                        "Cumulative_resp_alpha0.2_all.csv", "Cumulative_resp_alpha0.2_min10-20.csv", "Cumulative_resp_alpha0.2_min10-25.csv",
                        "Cumulative_resp_alpha0.4_all.csv", "Cumulative_resp_alpha0.4_min10-20.csv", "Cumulative_resp_alpha0.4_min10-25.csv",
                        "Cumulative_resp_alpha0.6_all.csv", "Cumulative_resp_alpha0.6_min10-20.csv", "Cumulative_resp_alpha0.6_min10-25.csv")
table_o_files$matrix.nrow...10. <- NULL # delete the initial row

# make a dataframe to rbind the ouput
cumulative_respCALC <- data.frame(matrix(nrow = 384, ncol = 16)) # 384 is the size of each file (16*24)
                                                      # composed of cumulative LoLin regression analysis of 16 files with
                                                      # 24 measurements (rows) each ( 24-well SDR plate)
# names here are the same order as the imported files in table_o_filenames$name
colnames(cumulative_respCALC) <- c("Resp_individually_all.csv",
                                   "LpcResp_alpha0.2_all.csv", "LpcResp_alpha0.2_min10-20.csv", "LpcResp_alpha0.2_min10-25.csv",
                                   "LpcResp_alpha0.4_all.csv", "LpcResp_alpha0.4_min10-20.csv", "LpcResp_alpha0.4_min10-25.csv",
                                   "LpcResp_alpha0.6_all.csv", "LpcResp_alpha0.6_min10-20.csv", "LpcResp_alpha0.6_min10-25.csv", # now has 10 columns for each file output
                                    "Date", "SDR_pos", "RUN", "tank", "Day_trial", "Treat1_Treat2")

# OUTSIDE FOR LOOP 
# About this loop: The following script is used to calculate and standardize respriration rate data as µg_O2_hr-1_mm.shell.length-1
# calls each target file in the data.frame created above "cumulative_respCALC"

for(i in 1:nrow(table_o_files)){ # this for loop sorts and integrated avaerge blank values for each datafile...
 resp<-read.csv(file.path(path, (table_o_files[i,2])), header=T, sep=",", na.string="NA", as.is=T) 
  resp_sorted <- resp[
    with(resp, order(resp$Date, resp$RUN)), # order the data by Date and Run
    ]
  # name a new column that will can call common blanks, calculate a mean, and integrate back into a the dataset
  resp_sorted$newname <- paste(
    resp_sorted$Date, (substr(resp_sorted$tank, 1, 4)), (substr(resp_sorted$ID, 6, 10)), sep="_") # combine date with tray and vial IDs as "newname"
  dataMEANS <- ddply(resp_sorted, "newname", summarise, mean = mean(abs(b1.Lpc))) # summarise by "newname" to get mean values of the raw output O2 data in umnol min-1
  MEANSblanks <- dataMEANS[(which(nchar(dataMEANS$newname) == 19)),] # Blank "newname" files are 19 characters - can call just blanks in this line
  resp_sorted_2 <- merge(resp_sorted,MEANSblanks, by="newname", all = TRUE, sort = T) # new resp data file with triplicate mean blank values added
  resp_sorted_2$row <- seq.int(nrow(resp_sorted_2)) # number the rows 1 - the end
  
# NOTE: now that the  mean blank values are in "resp_sorted_2", we can correct the raw µmol min-1 data to the blanks
# each respirtion "run" had three vials for each tray (10 animals per vial, n = 30 total animals)
# and three blanks seawater samples from that corresponding tray - therefore we can correct just by calling +3 rows ahead of each animal resp rate (below)

# INSIDE FOR LOOP
  for (j in 1:nrow(resp_sorted_2)){ # for each row in the dataset resp sorted_2, run the following...
  
# correct for the mean blank value alwyas three rows ahead! (this will also calculate a false resp value for blanks)
  resp_sorted_2$resprate_CORRECTED <- ((abs(resp_sorted_2$b1.Lpc)) - (resp_sorted_2$mean[(resp_sorted_2$row+3)])) # correct to blank
# standardize by the volume of the SDR vial (4 ml) adn convert to µg hr-1 mm shell size-1 (now blanks will be NA in "resprate_CALC")
   resp_sorted_2$resprate_CALC <- ((((resp_sorted_2$resprate_CORRECTED/(1000/4))*(60))*31.998)/(resp_sorted_2$number_indivs*resp_sorted_2$mean_size)) # convert and standardize
  } # close inside for loop

  # This small function eliminates a dataframe based on NA of a certain columns
  # In this case, the NA are in resprate_CALC
  desiredCols <- resp_sorted_2[,c(3,4,5,7,10,11,12,16,19)]
  completeFun <- function(resp_sorted_2,  desiredCols) {
    completeVec <- complete.cases(resp_sorted_2[, desiredCols])
    return(resp_sorted_2[completeVec, ])
  }
  # run completeFun
  resp_FINAL<-completeFun(resp_sorted_2, "resprate_CALC")
  # order the dataframe
  resp_FINAL <- resp_sorted_2[
    with(resp_sorted_2, order(resp_sorted_2$Date, resp_sorted_2$RUN, resp_sorted_2$SDR_position)),
    ]
  
  # start a cumulative data sheet for each outer for loop of resp values for each dataset
  # each filename in the outside for loop is a new row in  "cumulative_respCALC"
  cumulative_respCALC[,i] <- resp_FINAL$resprate_CALC
  print(cumulative_respCALC) # view progress of the loops in console 
  
  # write the output
  cumulative_respCALC[,11] <- resp_sorted$Date # add date
  cumulative_respCALC[,12] <- resp_sorted$SDR_position # add sdr position ID
  cumulative_respCALC[,13] <- resp_sorted$RUN # add run number
  cumulative_respCALC[,14] <- resp_sorted$tank # add tray ID
  cumulative_respCALC[,15] <- resp_sorted$Day_Trial # add the day and run
  cumulative_respCALC[,16] <- resp_sorted$Treat1_Treat2 # add treatments

  cumulative_respCALC[,c(11,12,13,14,15,16,1,2,3,4,5,6,7,8,9,10)] # re organize for clarity
} # closed OUTSIDE for loop


# This small function eliminates a datafram based on NA of a certain columns
# In this case, the NA are in resprate_CALC
desiredCols2 <- cumulative_respCALC[,c(11,12,13,14,15,16,1,2,3,4,5,6,7,8,9,10)] # we want all of the columns
completeFun <- function(cumulative_respCALC,  desiredCols2) {
  completeVec <- complete.cases(cumulative_respCALC[, desiredCols2])
  return(cumulative_respCALC[completeVec, ])
}
# eliminates all rows with NA in "LpcResp_alpha0.6_min10-25.csv"
cumulative_respCALC_FINAL <- resp_FINAL<-completeFun(cumulative_respCALC, "LpcResp_alpha0.6_min10-25.csv") # choose an example of a data file
print(cumulative_respCALC_FINAL)

write.table(cumulative_respCALC_FINAL,"All_resp_calc_and_standardized.csv",sep=",", row.names=FALSE) # write final table

