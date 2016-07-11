efluxo.result<-function(dir=NULL,gravar=FALSE, startRange=0, endRange=9){
  arquivos<-list.files(dir)
  ##Comteraio
  docn<-0
  i=0
  RData=list()
  for (arquivo in arquivos){
    if('dat'==substr(arquivo,nchar(arquivo)-2,nchar(arquivo))){
      nomeS<-substr(arquivo,1,nchar(arquivo)-4)
      Data=list()
      docn<-docn+1
      doc<-read.table(paste(dir,nomeS,'.dat',sep=''),comment.char = ";")
      doc2<-read.csv(paste(dir,nomeS,'.csv',sep=''))
      limite<-length(doc[,1])
      for(linha in 1:limite){
        if(linha<limite){
          if(doc[linha,1] < doc[(linha+1),1]){
            i<-i+1
            reRuc<-efluxo.ruc(doc[linha-startRange,7],doc[(linha-endRange),7],doc[linha-startRange,14],doc[(linha-endRange),14],doc[(linha-endRange),18],doc2[i,6],doc2[i,9],doc2[i,10])
            reRcgr<-efluxo.rcgr(reRuc,doc2[i,9],doc2[i,2])
            reRcumol<-efluxo.rcumol(reRcgr)
            Data[[i]]<-data.frame(Plot=i,C02_10=doc[linha-startRange,7],C02_=doc[(linha-endRange),7],Tempo10=doc[linha-startRange,14],Tempo1=doc[linha-endRange,14],Atm=doc[(linha-endRange),18],Profundidade=doc2[i,2],Temp_Solo=doc2[i,3],Umidade_Solo=doc2[i,4],Umidade_Ar=doc2[i,5],Temp_Ar=doc2[i,6],Tem_Sup=doc2[i,7],Peso_seco=doc2[i,8],Volume=doc2[i,9],Area=doc2[i,10],Ruc_g_CO2_m_2_h_1=reRuc,Rc_g_CO2_m_2_h_1=reRcgr,Rc_umol_CO2=reRcumol,Local=nomeS)
          }
        }else{
          i<-i+1
          reRuc<-efluxo.ruc(doc[linha-startRange,7],doc[(linha-endRange),7],doc[linha-startRange,14],doc[(linha-endRange),14],doc[(linha-endRange),18],doc2[i,6],doc2[i,9],doc2[i,10])
          reRcgr<-efluxo.rcgr(reRuc,doc2[i,9],doc2[i,2])
          reRcumol<-efluxo.rcumol(reRcgr)
          Data[[i]]<-data.frame(Plot=i,C02_10=doc[linha-startRange,7],C02_=doc[(linha-endRange),7],Tempo10=doc[linha-startRange,14],Tempo1=doc[linha-endRange,14],Atm=doc[(linha-endRange),18],Profundidade=doc2[i,2],Temp_Solo=doc2[i,3],Umidade_Solo=doc2[i,4],Umidade_Ar=doc2[i,5],Temp_Ar=doc2[i,6],Tem_Sup=doc2[i,7],Peso_seco=doc2[i,8],Volume=doc2[i,9],Area=doc2[i,10],Ruc_g_CO2_m_2_h_1=reRuc,Rc_g_CO2_m_2_h_1=reRcgr,Rc_umol_CO2=reRcumol,Local=nomeS)
          if(gravar){
            D<-do.call('rbind',Data)
            #Remove a utima coluna antes de criar o csv
            efluxo.csv(D,nomeS,dir)
            rm(D)#limpa os dados
          }
          RData[[docn]]<-do.call('rbind',Data)
          rm(Data)#limpa os dados
          i<-0
        }
      }
    }
  }
  return(RData)
}
