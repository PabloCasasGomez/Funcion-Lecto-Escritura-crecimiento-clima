library(openxlsx)
library(ggplot2)

nombre_archivos=list.files(path="D:/pablo/Desktop/Lo mas actualizado/Doctorado/Manuscrito 7 (Himalaya)/CRU")
nombre_archivos=nombre_archivos[!nombre_archivos %in% c("RCP 85","RCP 45","Graficas tendencias")]

graficas_clima=function(nombre_archivos){

for(n in nombre_archivos){
  setwd("D:/pablo/Desktop/Lo mas actualizado/Doctorado/Manuscrito 7 (Himalaya)/CRU")
  data=read.xlsx(n)
  
  matriz_clima=data.frame(nrow=119,ncol=2)
  
  if(substr(n, 1, 1)=="P"){
    
    for(i in c(1:nrow(data))){
      matriz_clima[i,1]=data[i,1]
      
      matriz_clima[i,2]=(data[i,13]+data[i,2]+data[i,3]+data[i,4]+data[i,5]+data[i,6]+data[i,7]+data[i,8]+data[i,9]+data[i,10]+data[i,11]+data[i,12])
    }
    
    grafica_final=ggplot(matriz_clima)+
      geom_line(aes(y=matriz_clima[,2],x=data[,1]),color="red")+
      geom_smooth(aes(y=matriz_clima[,2],x=data[,1]),color="blue",se=FALSE)+
      xlab("Year")+
      theme_classic()+
      ylab("P (mm)")
    ggsave(paste("D:/pablo/Desktop/Lo mas actualizado/Doctorado/Manuscrito 7 (Himalaya)/CRU/Graficas tendencias/",substr(n, 1, 20),".png",sep=""),grafica_final)
  }else{
    
    
    for(i in c(1:nrow(data))){
      matriz_clima[i,1]=data[i,1]
      
      matriz_clima[i,2]=(data[i,13]+data[i,2]+data[i,3]+data[i,4]+data[i,5]+data[i,6]+data[i,7]+data[i,8]+data[i,9]+data[i,10]+data[i,11]+data[i,12])/12
    }
    
    grafica_final=ggplot(matriz_clima)+
      geom_line(aes(y=matriz_clima[,2],x=data[,1]),color="red")+
      geom_smooth(aes(y=matriz_clima[,2],x=data[,1]),color="blue",se=FALSE)+
      xlab("Year")+
      theme_classic()+
      ylab("T (ºC)")
    ggsave(paste("D:/pablo/Desktop/Lo mas actualizado/Doctorado/Manuscrito 7 (Himalaya)/CRU/Graficas tendencias/",substr(n, 1, 20),".png"),grafica_final)
  }
  
  }
}