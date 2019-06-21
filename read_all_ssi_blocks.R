
#setwd('M://ibares/AMS/AMS-Messdaten/2017/2017_07_10f_Be-7_Be-10/')
#setwd("/Volumes/ibares/AMS/AMS-Messdaten/2017/2017_07_10f_Be-7_Be-10/")
meas_directory<-getwd()
#dir.create(paste(getwd(),"/tmp_ssi_results/",sep=""))
#results_directory<-paste(getwd(),"/tmp_ssi_results/", sep="")

#directories<-list.dirs(path=meas_directory,recursive = F)
#dir_name<-list.dirs(path=meas_directory,recursive = F, full.names = F)
#all_samples_summary<-data.frame()


iso1<-"Be7"
iso2<-"Be9"

#directory<-1
#i<-2
all_blocks_samples<-data.frame()

#for(directory in 1:(length(directories))){
details<-file.info(list.files(path=getwd(),pattern=".ssires"))
details<-details[order(details$mtime),]
all.files<-as.character(rownames(details))
  
  for(i in 1:length(all.files)){
    ssires<-data.frame()
    #sample_summary<-data.frame()
    
    filename<-all.files[i]
    header<-fread(paste(getwd(),filename,sep="/"),skip=2, nrows = 7,fill = T)
    time_stamp<-paste(header[1]$V5,header[1]$V6,header[1]$V7)
    id<-header[5]$V4
    position<-header[6]$V4
    description<-paste(header[7]$V4,header[7]$V5,header[7]$V6,header[7]$V7)
    
    
    if (id!="C") {
      ssires<-fread(paste(getwd(),all.files[i],sep="/"),skip = 71, fill=TRUE)
      res_file<-rep(all.files[i],dim(ssires)[1])
      sample_id<-rep(id,dim(ssires)[1])
      #sample_des<-rep(des,dim(ssires)[1])
      
      ssires<-cbind(res_file,sample_id,ssires)
      
      ssires[is.na(ssires)]<-0
      ssires<-ssires[(ssires$IsotopeName==iso1 | ssires$IsotopeName==iso2),]
      sample_id<-ssires[,2][ssires$IsotopeName==iso2]
      
      block<-as.matrix(ssires[,3][ssires$IsotopeName==iso1])
      y7<-as.matrix(ssires[,5][ssires$IsotopeName==iso1])
      cps_be7<-as.matrix(ssires[,7][ssires$IsotopeName==iso1])
      lifetime_be7<-as.matrix(ssires[,6][ssires$IsotopeName==iso1])
      
      x9<-as.matrix(ssires[,3][ssires$IsotopeName==iso2])
      y9<-as.matrix(ssires[,5][ssires$IsotopeName==iso2])
      current_be9<-as.matrix(ssires[,7][ssires$IsotopeName==iso2])
      
      
      sample_id<-ssires[,2][ssires$IsotopeName==iso2]
      
      ######################################################      
      #calculate some values
      ######################################################
      
      #ratio<-cps_be7*ee*chargestate/current_be9
      y7_err<-1/sqrt(y7)
      #ratio_err<-ratio*y7_err
      #ratio_sd<-sd(ratio)
      #weighted_meanratio<-weighted.mean(x = ratio, w=(1/ratio_err^2))
      #n=length(ratio)
      
      
      sample_summary<-data.frame(sample_id,description,position,filename,time_stamp,block,lifetime_be7,y7,cps_be7,y9,current_be9)# (ratio-weighted_meanratio)^2/ratio_err^2,chi2_crit )
      colnames(sample_summary)<-c("Sample","Name","Pos","File","Timestamp","Block","LifetimeIso1", "Counts","CountrateIso1","Charge","CurrentIso2")#,"chi_test","chi-crit")
      
      
      all_blocks_samples<-rbind(all_blocks_samples,sample_summary)

      ##############################            
      
     # analysis_description<-paste(id,description,all.files[i],directory,sep = " ")
    #  
    #  png(file=paste(results_directory,paste(id,directory,"res_file",strsplit(all.files[i],"[.]")[[1]][1],"Counts_Currents.png",sep="_")))
    #  plot.new()
    #  par(mar=c(5, 4, 4, 5) + 0.1)
    #  plot(x9,current_be9/max(current_be9),xlim=c(0,max(block)), ylim = c(0,1.01), xlab=NA,ylab=NA,main = analysis_description)
    #  lines(x9,current_be9/max(current_be9),type="l",col="blue")
    #  mtext(side = 2, line = 3, 'normalized. current')
    #  #segments(nh_dc14_data_o[,1],nh_dc14_data_o[,2]-nh_dc14_data_o[,3],nh_dc14_data_o[,1],nh_dc14_data_o[,2]+nh_dc14_data_o[,3],col="blue")
    #  axis(1,xlab="Charge" ); 
    #  axis(2); box()
      
    # par(new=TRUE)
    #  plot(block,y7,axes=F,xlim=c(0,max(block)), ylim = c(min(y7),1.05*max(y7)),ylab = NA, xlab = "Sequence")
    #  axis(4, col="red",col.axis="red",las=0)
    #  lines(block,y7,type="o",col="red")
    #  
    #  mtext(side = 4, line = 3, 'Be7 Counts')
    #  dev.off()
      #################################         
      
    }
  }

write.csv(all_blocks_samples,paste(results_directory,"all_samples_and_blocks.csv",sep = ""), row.names=F)
rm(all_blocks_samples)
      