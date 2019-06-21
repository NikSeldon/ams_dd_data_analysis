while(menu(c("Yes", "No"),title = "Want to evaluate a sample?")==1){
  sampleID<-unique(all_blocks[,1])[menu(unique(all_blocks[,1]), title="please select sample?")]
  #sampleID<-readline(prompt = "please type sampleID for evaluation: ")
  if(length(sampleID)==0){"unvalid sample ID "
  }else{
    if(sampleID %in% all_blocks[,1]){
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
      
      plt<-ggplot(data = sample)+
        labs(title=paste(sampleID," ",name,"Current"))+
        geom_point(aes(x=Block, y=CurrentIso2/max(CurrentIso2), color=File))+
        geom_line(aes(x=Block, y=CurrentIso2/max(CurrentIso2), color=File))
      show(plt)
      #  }else{cat("sample not in list") 
      #    break}
      
      #ggplot(data = sample)+
      #  labs(title=paste(sampleID," ",name,"Counts"))+
      #  geom_point(aes(x=Block, y=Counts, color=File))+
      #  geom_line(aes(x=Block, y=Counts, color=File))
      #ggplot(data = sample)+
      #  labs(title=paste(sampleID," ",name,"machine Ratio"))+
      #  geom_point(aes(x=Block, y=machineRatio, color=File))+
      #  geom_line(aes(x=Block, y=machineRatio, color=File))
      
      #want to remove some datapoints?
      #########
      #1) select a ssires file
      while(menu(c("Yes", "No"),title = "Want to remove some data points")==1){
        ssires<-unique(sample$File)[menu(unique(sample$File), title="please select file?")]
        plot2<-ggplot(data = sample[sample$File==ssires,])+
          geom_point(aes(x=Block, y=CurrentIso2/max(CurrentIso2), color=File))+
          geom_line(aes(x=Block, y=CurrentIso2/max(CurrentIso2), color=File))
        show(plot2)
        remove_block<-as.numeric(readline(prompt = "please select a block (or 0): "))
        if((remove_block<1) | (remove_block>max(sample$Block)))
        {print("unvalid block number")
        }else{
          remove_row<-rownames((sample[(sample$File==ssires & sample$Block==remove_block),][1]))
          if(length(remove_row)!=0) sample<-sample[-which(rownames(sample)==remove_row),]
        }
        
        plot3<-ggplot(data = sample)+
          labs(title=paste(sampleID," ",name,"Current"))+
          geom_point(aes(x=Block, y=CurrentIso2/max(CurrentIso2), color=File))+
          geom_line(aes(x=Block, y=CurrentIso2/max(CurrentIso2), color=File))
        show(plot3)
      }
      
      
      #Want to try some current correction??
      ########
      # remove a trend in machineRatios (current dependency), 
      # this will not chanhge the mean of ratios, but their standard deviation
      #source("/Volumes/ibares/AMS/Programme/R_Studio/use4ssi/current_correction.R")
      source("'M://ibares/AMS/Programme/R_Studio/use4ssi/current_correction.R")
      
      
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
      #repeat the previous steps for all your samples of interrest
    }
  }
}