library(data.table)
library(ggplot2)
library(gridExtra)
library(plyr) 
rm(list=ls())
coulomb<-6.24151e18
ee<-1.60217733E-19
chargestate<-2

#setwd("/Volumes/ibares/AMS/AMS-Messdaten/2017/2017_07_10f_Be-7_Be-10/ssi-batch/2017_07_13_7Be_RAIN_HANNOVER/")
setwd('M://ibares/AMS/AMS-Messdaten/2017/2017_07_10f_Be-7_Be-10/ssi-batch/2017_07_13_7Be_RAIN_HANNOVER/')   
dir.create(paste(getwd(),"/tmp_ssi_results/",sep=""))
results_directory<-paste(getwd(),"/tmp_ssi_results/", sep="")

#source("/Volumes/ibares/AMS/Programme/R_Studio/use4ssi/read_all_ssi_blocks.R")
source("M://ibares/AMS/Programme/R_Studio/use4ssi/read_all_ssi_blocks.R")
all_blocks<-read.csv("tmp_ssi_results/all_samples_and_blocks.csv")

#create new empty data.frame for sample summaries
summary_ssires<-data.frame()

#start sample evaluation 
#source("/Volumes/ibares/AMS/Programme/R_Studio/use4ssi/while_evaluation.R")
source("M://ibares/AMS/Programme/R_Studio/use4ssi/while_evaluation.R")

#create final summary of machine ratios per sample
sumSample<-data.frame()
sumSample<-ddply(summary_ssires,.(sampleID), function(x)
{data.frame(w_mean_of_turns=weighted.mean(x$w_mean, 1/x$final_error), 
            error=1/sqrt(sum(1/x$final_error^2))
)})

#write csv files
write.csv(summary_ssires,paste(results_directory,"summary_per_ssires.csv",sep = "/"),row.names = FALSE)
write.csv(sumSample,paste(results_directory,"samples_machineRatios.csv",sep = "/"), row.names = FALSE)


