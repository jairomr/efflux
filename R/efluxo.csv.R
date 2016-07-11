efluxo.csv<-function(data=NULL,name='',dir=''){
  limite<-length(data)
  if(is.data.frame(data)){
    write.csv(data,paste(dir,name,'_resutado.csv',sep = ''),row.names=F)
  }else{
    for (l in 1:limite) {
      write.csv(data[[l]],paste(dir,name,'_',l,'.csv',sep =''),row.names=F)
    }
  }
}
