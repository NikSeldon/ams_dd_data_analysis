library(data.table)
library(ggplot2)
library(gridExtra)
library(plyr) 
rm(list=ls())
coulomb<-6.24151e18
ee<-1.60217733E-19
chargestate<-2

setwd("/Volumes/ibares/AMS/AMS-Messdaten/2017/2017_07_10f_Be-7_Be-10/ssi-batch/2017_07_13_7Be_RAIN_HANNOVER/")
dir.create(paste(getwd(),"/tmp_ssi_results/",sep=""))
results_directory<-paste(getwd(),"/tmp_ssi_results/", sep="")

source("/Volumes/ibares/AMS/Programme/R_Studio/use4ssi/read_all_ssi_blocks.R")
all_blocks<-read.csv("tmp_ssi_results/all_samples_and_blocks.csv")


#create new empty data.frame for sample summaries
sampleSummarySsires<-data.frame()
summary_ssires<-data.frame()
#directory<-"3"

#select a sample for evaluation
########################
sampleID<-"B1864"
sample<-all_blocks[all_blocks[,1]==sampleID,]
name<-sample$Name[1]

#some calculations##########
sample$machineRatio<-sample$CountrateIso1*ee*chargestate/sample$CurrentIso2
sample$rel_error<-1/sqrt(sample$Counts)
sample$abs_error<-sample$machineRatio*sample$rel_erro
sample$abs_error[is.na(sample$abs_error)]<-Inf
sample$EvalMachineRatio<-sample$machineRatio
sample$Eval_abs_error<-sample$EvalMachineRatio*sample$rel_erro
sample$Eval_abs_error[is.na(sample$Eval_abs_error)]<-Inf


#plot current, counts, machine ratios per block 
#############################################

ggplot(data = sample)+
  labs(title=paste(sampleID," ",name,"Current"))+
  geom_point(aes(x=Block, y=CurrentIso2/max(CurrentIso2), color=File))+
  geom_line(aes(x=Block, y=CurrentIso2/max(CurrentIso2), color=File))
ggplot(data = sample)+
  labs(title=paste(sampleID," ",name,"Counts"))+
  geom_point(aes(x=Block, y=Counts, color=File))+
  geom_line(aes(x=Block, y=Counts, color=File))
ggplot(data = sample)+
  labs(title=paste(sampleID," ",name,"machine Ratio"))+
  geom_point(aes(x=Block, y=machineRatio, color=File))+
  geom_line(aes(x=Block, y=machineRatio, color=File))

#want to remove some datapoints?
#########
#1) select a ssires file
ssires<-"22.ssires"
ggplot(data = sample[sample$File==ssires,])+
  geom_point(aes(x=Block, y=CurrentIso2/max(CurrentIso2), color=File))+
  geom_line(aes(x=Block, y=CurrentIso2/max(CurrentIso2), color=File))
#1) select a Block
remove_block<-3
remove_row<-rownames((sample[(sample$File==ssires & sample$Block==remove_block),][1]))
sample<-sample[-which(rownames(sample)==remove_row),]
##repeat these steps eventually
###############################

#Want to try some current correction??
# remove a trend in machineRatios (current dependency), 
# this will not chanhge the mean of ratios, but their standard deviation
# for next steps give your answers, using the console below

source("/Volumes/ibares/AMS/Programme/R_Studio/use4ssi/current_correction.R")


##finish current correction


sumTurn<-ddply(sample,.(File), function(x) 
{data.frame(sampleID,name,w_mean=weighted.mean(x$EvalMachineRatio, 1/x$rel_error), 
            mean=mean(x$EvalMachineRatio),
            sd=sd(x$EvalMachineRatio),
            unc=1/sqrt(sum(1/x$Eval_abs_error^2))
)})


for (i in 1:(dim(sumTurn)[1])) { 
  if(sumTurn[i,"sd"]>sumTurn[i,"unc"])
  {sumTurn[i,"final_error"]<-sumTurn[i,"sd"]
  }else{sumTurn[i,"final_error"]<-sumTurn[i,"unc"]}
}

summary_ssires<-rbind.data.frame(summary_ssires,sumTurn)




#### #Make a plot
#####
ggplot(data = sample)+
  labs(title=paste(sample_nr,"_",sample_description))+
  geom_point(aes(x=Blk, y=sample$`Iso1/Iso2`, color=res_file))+
  geom_line(aes(x=Blk, y=sample$`Iso1/Iso2`, color=res_file))+
  geom_errorbar(data=sample,
                aes(x=Blk, ymin=sample$`Iso1/Iso2`-1/sqrt(Iso1cnts)*sample$`Iso1/Iso2`,
                    ymax=sample$`Iso1/Iso2`+1/sqrt(Iso1cnts)*sample$`Iso1/Iso2`), width=.1, color="grey")+
  facet_grid(. ~ res_file)+
  geom_smooth(method = lm, data =sample, aes(x=Blk, y=sample$`Iso1/Iso2`, color=res_file))

pdf_file<-paste(results_directory,paste(sample_nr,"ratios.pdf",sep="_"),sep = "/")
ggsave(filename =pdf_file) 










sampleTmp<-sample[sample$File==ssires,]
analysis_description<-paste(sampleTmp$Sample[1],sampleTmp$Dir,sampleTmp$File ,sep = " ")
#sampleTmp<-sampleTmp[sampleTmp$Counts!=0,]
sampleTmp$ErrCounts[sampleTmp$Counts==0]<-1

n=dim(sampleTmp)[1]

#######Sample Mean Ratios
sampleTmp$MeanRatio<-mean(sampleTmp$MachineRatioIso1Iso2)
sampleTmp$ErrMeanRatio<-sd(sampleTmp$MachineRatioIso1Iso2)/sqrt(n)

#####################

  ratio_lm<-lm(data=sampleTmp, formula = MachineRatioIso1Iso2~Block)
  sampleTmp$CurrentCorrRatio<-sampleTmp$MachineRatioIso1Iso2+sampleTmp$MeanRatio-predict(ratio_lm, newdata=sampleTmp[7])
  sampleTmp$ErrCurrentCorrRatio<-sampleTmp$CurrentCorrRatio*sampleTmp$ErrMRatio/sampleTmp$CurrentCorrRatio
  
  ggplot(data = sampleTmp)+
    labs(title=analysis_description)+
    geom_point(aes(x=Block, y=MachineRatioIso1Iso2), color="grey")+
    geom_line(aes(x=Block, y=MachineRatioIso1Iso2), color="grey")+
    geom_point(aes(x=Block, y=CurrentCorrRatio), color="blue")+
    geom_line(aes(x=Block, y=CurrentCorrRatio), color="blue")+
    geom_errorbar(data=sampleTmp,aes(x=Block, ymin=CurrentCorrRatio-ErrMRatio, ymax=CurrentCorrRatio+ErrMRatio), width=.01,color="blue") +
    geom_hline(yintercept = sampleTmp$MeanRatio)+
    geom_hline(yintercept = sampleTmp$MeanRatio+2*sampleTmp$ErrMeanRatio,color="darkgrey" )+
    geom_hline(yintercept = sampleTmp$MeanRatio-2*sampleTmp$ErrMeanRatio,color="darkgrey")+
    geom_smooth(method = lm, data =sampleTmp, aes(x=Block, y=CurrentCorrRatio) )

pdf_file<-paste(results_directory,paste(sampleTmp$Sample[1],sampleTmp$Dir[1],sampleTmp$File[1],"Ratios.pdf",sep="_"))
ggsave(filename =pdf_file)  

####overwrite current correction
sampleTmp$CurrentCorrRatio<-sampleTmp$MachineRatioIso1Iso2
sampleTmp$ErrCurrentCorrRatio<-sampleTmp$CurrentCorrRatio*sampleTmp$ErrCounts

curr_ratio_lm<-lm(data=sampleTmp, formula = CurrentCorrRatio~Block)
#meanCurrentCorrRatio<-summary(curr_ratio_lm)[[4]][1]
meanCurrentCorrRatio<-mean(sampleTmp$CurrentCorrRatio)
errR<-sampleTmp$ErrCounts*sampleTmp$CurrentCorrRatio
meanCurrentCorrRatioWeigthed<-with(sampleTmp,weighted.mean(x = sampleTmp$CurrentCorrRatio, w=(errR)))
uncMeanCurrentCorrRatio<-1/sqrt(sum(1/sampleTmp$ErrCurrentCorrRatio^2))
sigmaMeanCurrentCorrRatio<-sd(sampleTmp$CurrentCorrRatio)/sqrt(n)

addError<-0 #in percentage%
chi2_crit<-qchisq(p=.95,df=(n-1))

normChi2mean<-sum((sampleTmp$CurrentCorrRatio-meanCurrentCorrRatio)^2/(sampleTmp$ErrCurrentCorrRatio^2+(addError*sampleTmp$CurrentCorrRatio)^2))/chi2_crit
normChi2wmean<-sum((sampleTmp$CurrentCorrRatio-meanCurrentCorrRatioWeigthed)^2/(sampleTmp$ErrCurrentCorrRatio^2+(addError*sampleTmp$CurrentCorrRatio)^2))/chi2_crit

while(normChi2mean>1){
  addError<-addError+0.001
  normChi2mean<-sum((sampleTmp$CurrentCorrRatio-meanCurrentCorrRatio)^2/(sampleTmp$ErrCurrentCorrRatio^2+(addError*sampleTmp$CurrentCorrRatio)^2))/chi2_crit
  normChi2wmean<-sum((sampleTmp$CurrentCorrRatio-meanCurrentCorrRatioWeigthed)^2/(sampleTmp$ErrCurrentCorrRatio^2+(addError*sampleTmp$CurrentCorrRatio)^2))/chi2_crit
}

totaluncError<-sqrt(uncMeanCurrentCorrRatio^2+(addError*meanCurrentCorrRatio)^2)/meanCurrentCorrRatio


print(paste("addError: ",addError))
print(paste("norm. Chi2mean: ", normChi2mean))
print(paste("norm. Chi2w_mean: ", normChi2wmean))
totaluncError<-sqrt(uncMeanCurrentCorrRatio^2+(addError*meanCurrentCorrRatio)^2)/meanCurrentCorrRatio

sampleResultSsires<-data.frame("ID"=sampleTmp$Sample[1],
                         "Name"=sampleTmp$Name[1],"Pos"=sampleTmp$Pos[1],
                         "File"=sampleTmp$File[1], "TimeStamp"=sampleTmp$Timestamp[1],
                         "mRatio"=meanCurrentCorrRatio,"mRatioWeighted"=meanCurrentCorrRatioWeigthed, "uncert."=uncMeanCurrentCorrRatio,
                         "sigma"=sigmaMeanCurrentCorrRatio, "normChi2mean"=normChi2mean, "normChi2wmean"=normChi2wmean,"Blocks"=n, "addError"=addError, "totaluncError"=totaluncError)

###################
#add samples to the list of summaries
sampleSummarySsires<-rbind(sampleSummarySsires,sampleResultSsires)

###############################
write.csv(sampleSummarySsires,paste(results_directory,"batch_","1866_samples_and_blocks_corrected.csv",sep = ""), row.names=F)


