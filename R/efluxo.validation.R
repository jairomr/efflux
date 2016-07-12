setwd("/home/rocha/Área de Trabalho/teste efluxo 2016/ICMAIO2016/")
efluxo.validation<-function(file=NUll){
  nomeS<-substr(file,1,nchar(file)-4)
  doc<-read.table(file,comment.char = ";")
  limite<-length(doc[,1])
  for(linha in 1:limite){
    if(linha<limite){
      if(doc[linha,1] < doc[(linha+1),1]){
        if(doc[linha,1] != doc[(linha+1),1]-1){
          print(paste("Erro falta a linha ",doc[(linha+1),1]-1))
        }
      }
    }
  }
  write.csv(doc,paste(nomeS,".efl",sep = ""))
}
efluxo.validation("Renata ICCT maio 2016.dat")
