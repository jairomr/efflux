efluxo.ruc<-function(c10,c1,t10,t1,atm,temperatura,volume,area){
  resultado<-(((c10-c1)/(t10-t1))*(atm/1013.25)*(273/(temperatura+273))*(44.01/22.41)*(volume/area))*(3600/1000)
  return(resultado)
}
