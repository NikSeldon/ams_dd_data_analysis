#basically this little script will give you an overview of FSI measurement, wether it is in 
#progress or finished doesn't matter.
#You can run it line by line (ctrl+enter) or select all and hit the run button in the upper right 
#corner of the editor (assuming to run it in R-studio).

#first -all blocks in the measurement will be read from all fsires in the all_blocks_samples data.frame
#then a sample can be select for closer view
#finally all samples of the batch are processed 
#if you are familiar with R, you can deselect certain blocks

library(plyr)
library(data.table)
library(ggplot2)
rm(list=ls())
coulomb<-6.24151e18
ee<-1.60217733E-19
chargestate<-2

StandardValue<-1.70429e-12  #input of Standardvalue Be10/9

#setwd('M://ibares/AMS/AMS-Messdaten/2017/2017_09_18ff_Be10/batches-be10/2017_09_21_Be_AWI_17c/')
setwd('~/Documents/Physik/000_HZDR/ams/data_analysis/tmp/2017_09_20_Be_AWI_17f/')
dir.create(paste(getwd(),"/rough_results_dggr",sep=""))
results_directory<-paste(getwd(),"/rough_results_dggr", sep="")

#read all Blocks (select (mark line 27...and run ))
################
details<-file.info(list.files(path=getwd(),pattern=".fsires"))
details<-details[order(details$mtime),]
all.files<-as.character(rownames(details))
all_blocks_samples<-data.frame()

#i<-1
for(i in 1:length(all.files)){
  fsires<-data.frame()
  #sample_summary<-data.frame()
  
  filename<-all.files[i]
  header<-fread(paste(getwd(),filename,sep="/"),skip=0, nrows = 7,fill = T)
  time_stamp<-paste(header[2]$V5,header[2]$V6,header[2]$V7, sep = " ")
  id<-header[4]$V4
  position<-header[5]$V4
  description<-header[6]$V4
  
  
  if (!grepl("C_in_Cu",id)  &  !grepl("X00",id)){
    fsires<-fread(paste(getwd(),all.files[i],sep="/"),skip = 59, fill=TRUE)
    res_file<-rep(all.files[i],dim(fsires)[1])
    sample_id<-rep(id,dim(fsires)[1])
    sample_position<-rep(position,dim(fsires)[1])
    sample_description<-rep(description,dim(fsires)[1])
  
    fsires<-cbind(sample_id,sample_description,sample_position,res_file,fsires)
    fsires[is.na(fsires)]<-0
    
    all_blocks_samples<-rbind(all_blocks_samples,fsires)
  } # end of "if not C sample" condition
} # end of file loop

if (file.exists(paste(results_directory,"all_sample_and_blocks.csv",sep = "")))
  do.call(file.remove, list(list.files(results_directory, full.names = TRUE)))
write.csv(all_blocks_samples,paste(results_directory,"all_sample_and_blocks.csv",sep = "/"), row.names=F)





#select Sample (and fsires) to analyze 
#calculate mean
sample_list<-unique(all_blocks_samples$sample_id)
#smpl<-sample_list[2]
summary_fsires<-data.frame()

#################

#select samples for closer view on current, count rates, ratios 
#####################
sample_nr<-"B1962"
sample<-all_blocks_samples[(all_blocks_samples$sample_id==sample_nr),]
sample_description<-sample$sample_description[1]
#select certain resfile only
# fsires_file<-"10.fsires"
#sample<-all_blocks_samples[(all_blocks_samples$sample_id==sample_nr  & all_blocks_samples$res_file==fsires_file),]

#plot current (can save the plot manually...hit export in the plot window)
ggplot(data = sample)+
  geom_point(aes(x=Blk, y=Iso2cur/max(Iso2cur), color=res_file))+
  geom_line(aes(x=Blk, y=Iso2cur/max(Iso2cur), color=res_file))

#plot counts
ggplot(data = sample)+
  geom_point(aes(x=Blk, y=Iso1cnts, color=res_file))+
  geom_line(aes(x=Blk, y=Iso1cnts, color=res_file))

#plot ratios
ggplot(data = sample)+
  geom_point(aes(x=Blk, y=sample$`Iso1/Iso2`, color=res_file))+
  geom_line(aes(x=Blk, y=sample$`Iso1/Iso2`, color=res_file))

######## want remove some datapoints? ...as an example
######
#which(sample$`Iso1/Iso2`==min(sample$`Iso1/Iso2`))
#sample<-sample[-5,] #remove the 5th line of "sample" data frame
#####

#############################################################################################
#process all samples
#############################################################################################
#LOOP over all samples ...
for (smpl in sample_list){

sample_nr<-smpl
sample<-all_blocks_samples[(all_blocks_samples$sample_id==sample_nr),]
sample_description<-sample$sample_description[1]

sample$rel_error<-1/sqrt(sample$Iso1cnts)
sample$abs_error<-sample$`Iso1/Iso2`*sample$rel_erro
sample$abs_error[is.na(sample$abs_error)]<-Inf

sumTurn<-ddply(sample,.(res_file), function(x) 
        {data.frame(sample_nr,sample_description,w_mean=weighted.mean(x$`Iso1/Iso2`, 1/x$rel_error), 
              mean=mean(x$`Iso1/Iso2`),
              sd=sd(x$`Iso1/Iso2`),
              unc=1/sqrt(sum(1/x$abs_error^2))
              )})
#i<-0
for (i in 1:(dim(sumTurn)[1])) { 
if(sumTurn[i,"sd"]>sumTurn[i,"unc"])
  {sumTurn[i,"error"]<-sumTurn[i,"sd"]
    }else{sumTurn[i,"error"]<-sumTurn[i,"unc"]}
  }

summary_fsires<-rbind.data.frame(summary_fsires,sumTurn)
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


}####end a loop over samples here


#write a summary of turns
write.csv(summary_fsires,paste(results_directory,"summary_turns.csv",sep = "/"), row.names = FALSE)


######create final sample summary
sumSample<-data.frame()
  
sumSample<-ddply(summary_fsires,.(sample_nr), function(x)
  {data.frame(w_mean_of_turns=weighted.mean(x$w_mean, 1/x$error), 
              error=1/sqrt(sum(1/x$error^2))
              
)})

###finally calculate the ratios and write the csv file
c_factor<-StandardValue/sumSample[1,2]
sumSample$Be10_9<-sumSample[,2]*c_factor
sumSample$Be10_9error<-sumSample[,3]*c_factor

#write a summary of turns
write.csv(sumSample,paste(results_directory,"summary_samples.csv",sep = "/"), row.names = FALSE)

#good night and good luck!!!!!!
