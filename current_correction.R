while(menu(c("Yes", "No"), title="Want to try some current correction?")==1){
  ssires<-unique(sample$File)[menu(unique(sample$File), title="please select file?")]
  
  sampleTmp<-sample[sample$File==ssires,]
  ratio_lm<-lm(data=sampleTmp, formula = machineRatio~Block)
  CurrentCorrRatio<-sampleTmp$machineRatio+mean(sampleTmp$machineRatio)-predict(ratio_lm, newdata=sampleTmp[6])
  ErrCurrentCorrRatio<-CurrentCorrRatio*sampleTmp$rel_error#/sampleTmp$CurrentCorrRatio
  
  plot_cc<-ggplot(data = sampleTmp)+
    labs(title=paste(sampleID," ",ssires))+
    geom_point(aes(x=Block, y=machineRatio), color="grey")+
    geom_line(aes(x=Block, y=machineRatio), color="grey")+
    geom_point(aes(x=Block, y=CurrentCorrRatio), color="blue")+
    geom_line(aes(x=Block, y=CurrentCorrRatio), color="blue")+
    geom_errorbar(data=sampleTmp,aes(x=Block, ymin=CurrentCorrRatio-ErrCurrentCorrRatio, ymax=CurrentCorrRatio+ErrCurrentCorrRatio), width=.01,color="blue")+
    geom_smooth(method = lm, data =sampleTmp, aes(x=Block, y=CurrentCorrRatio) )
  show(plot_cc)
  
  
  if(menu(c("Yes", "No"), title="Want to apply this correction?")==1){
    sample$EvalMachineRatio[sample$File==ssires]<-CurrentCorrRatio
    sample$Eval_abs_error[sample$File==ssires]<-ErrCurrentCorrRatio
    sample$Eval_abs_error[is.na(sample$Eval_abs_error)]<-Inf
    pdf_file<-paste(results_directory,paste(sampleID,ssires,"Ratios.pdf",sep="_"))
    ggsave(filename =pdf_file) 
  }
}

