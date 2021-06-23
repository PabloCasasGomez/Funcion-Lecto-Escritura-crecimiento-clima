#Estamos haciendo una funcion para que lea todos los archivos de una carpeta en xlsx y que despues se modifiquen todos los archivos.

library(openxlsx)

nombre_archivos=list.files(path="D:/pablo/Desktop/Lo mas actualizado/Doctorado/Manuscrito 7 (Himalaya)/ArchivosExcel/Calculado_Incremento_BAI/Archivos_MLM_JuanCarlos")
nombre_archivos=nombre_archivos[!nombre_archivos %in% "Archivos MLM con clima"]

co2=read.xlsx("D:/pablo/Desktop/Lo mas actualizado/Doctorado/Manuscrito 7 (Himalaya)/co2.xlsx")

clima=read.xlsx("D:/pablo/Desktop/Lo mas actualizado/Doctorado/Manuscrito 7 (Himalaya)/site_details.xlsx")
clima=na.omit(clima)

#Funcion que lee los diferentes archivos de temperatura y precipitacion para los archivos de crecimiento anual
lecto_escritura_clima=function(clima,nombre_archivos,co2){
  
  for(j in c(1:nrow(clima))){

    #Lectura de archivos de crecimiento
    n=nombre_archivos[[j]]
    crecimiento=read.xlsx(paste("D:/pablo/Desktop/Lo mas actualizado/Doctorado/Manuscrito 7 (Himalaya)/ArchivosExcel/Calculado_Incremento_BAI/Archivos_MLM_JuanCarlos/",n,sep=""), colNames=TRUE,rowNames=TRUE)
    
    dimension=dim(crecimiento)
    fil=dimension[1]
    col=dimension[2]
    
    #A?adimos el valor de co2 para cada poblacion
    z=matrix(nrow = fil, ncol = 1)
    
    crecimiento[,2]=as.numeric(crecimiento[,2])
    maximo=max(crecimiento[,2])
    
    for(i in c(1:fil)){
      if(crecimiento[i,2]==co2[1,1]){
        print("\n")
        print(crecimiento[i,2])
        contador=i
        for(o in c(1:(maximo-1880))){
          z[contador,1]=co2[o,2]
          contador=contador+1
        }
      }
    }
    
    colnames(z)=c("co2")
    
    #Lectura de archivos de temperatura y precipitacion para cada archivo
    temperatura=clima$Nombre.Archivo.Temp[j]
    print(temperatura)
    
    temperatura=read.xlsx(paste("D:/pablo/Desktop/Lo mas actualizado/Doctorado/Manuscrito 7 (Himalaya)/CRU/",temperatura,".xlsx",sep=""))
    
    precipitacion=clima$Nombre.Archivo.Precp[j]
    precipitacion=read.xlsx(paste("D:/pablo/Desktop/Lo mas actualizado/Doctorado/Manuscrito 7 (Himalaya)/CRU/",precipitacion,".xlsx",sep=""))
  
    #Creacion de tablas con valores de precipitacion y temperatura
    matriz_clima=matrix(nrow=119,ncol=11)
    
    for(i in c(2:nrow(temperatura))){
      matriz_clima[i,1]=temperatura[i,1]
      matriz_clima[i,2]=(temperatura[(i-1),10]+temperatura[(i-1),11]+temperatura[(i-1),12])/3 #Taup
      matriz_clima[i,3]=(temperatura[(i-1),13]+temperatura[(i),2]+temperatura[(i),3])/3 #Twi
      matriz_clima[i,4]=(temperatura[(i),4]+temperatura[(i),5]+temperatura[(i),6])/3 #Tsp
      matriz_clima[i,5]=(temperatura[(i),7]+temperatura[(i),8]+temperatura[(i),9])/3 #Tsu
      matriz_clima[i,6]=(temperatura[(i),10]+temperatura[(i),11]+temperatura[(i),12])/3 #Tau
      
      matriz_clima[i,7]=precipitacion[(i-1),10]+precipitacion[(i-1),11]+precipitacion[(i-1),12] #Paup
      matriz_clima[i,8]=precipitacion[(i-1),13]+precipitacion[i,2]+precipitacion[i,3] #Pwi
      matriz_clima[i,9]=precipitacion[i,4]+precipitacion[i,5]+precipitacion[i,6] #Psp
      matriz_clima[i,10]=precipitacion[i,7]+precipitacion[i,8]+precipitacion[i,9] #Pau
      matriz_clima[i,11]=precipitacion[(i),10]+precipitacion[(i),11]+precipitacion[(i),12] #Pau
    }
    
    colnames(matriz_clima)=c("Year","Taup","Twi","Tsp","Tsum","Tau","Paup","Pwi","Psp","Psum","Pau")
    
    #Union de las matrices de clima con la matriz de datos original+co2
    matriz_ampliada_clima=matrix(nrow = fil, ncol = 11)
    
    for(i in c(1:fil)){
      if(crecimiento[i,2]==temperatura[1,1]){
        #print(i)
        #print("\n")
        #print(crecimiento[i,2])
        contador=i
        j=1
        while(j<nrow(temperatura)&& i<fil){
          matriz_ampliada_clima[contador,1]=matriz_clima[j,1]
          
          matriz_ampliada_clima[contador,2]=matriz_clima[j,2]
          matriz_ampliada_clima[contador,3]=matriz_clima[j,3]
          matriz_ampliada_clima[contador,4]=matriz_clima[j,4]
          matriz_ampliada_clima[contador,5]=matriz_clima[j,5]
          matriz_ampliada_clima[contador,6]=matriz_clima[j,6]
          
          matriz_ampliada_clima[contador,7]=matriz_clima[j,7]
          matriz_ampliada_clima[contador,8]=matriz_clima[j,8]
          matriz_ampliada_clima[contador,9]=matriz_clima[j,9]
          matriz_ampliada_clima[contador,10]=matriz_clima[j,10]
          matriz_ampliada_clima[contador,11]=matriz_clima[j,11]
          
          contador=contador+1
          i=i+1
          j=j+1
          print(i)
        }
      }
    }
    
    colnames(matriz_ampliada_clima)=c("Year","Taup","Twi","Tsp","Tsum","Tau","Paup","Pwi","Psp","Psum","Pau")
    
    conjunta=cbind(crecimiento,z)
    conjunta=cbind(conjunta,matriz_ampliada_clima)
    
    #Eliminamos las filas que tengan edad inferior a 3 a?os
    #Primero convertimos las filas de la tabla original de crecimiento en NA
    for(i in c(1:fil)){
      if(crecimiento[i,8]<3){
        crecimiento[i,8]=NA
      }
    }
    
    #Despues recorremos la tabla definitiva y comprobamos si la original tiene un NA para despues eliminarlo en la tabla final
    contador_filas_eliminadas=0
    for(i in c(1:fil)){
      if(is.na(crecimiento[i,8])==TRUE){
        conjunta=conjunta[-(i-contador_filas_eliminadas),]
        contador_filas_eliminadas=contador_filas_eliminadas+1
      }
    }
    
    write.xlsx(conjunta,paste("D:/pablo/Desktop/Lo mas actualizado/Doctorado/Manuscrito 7 (Himalaya)/ArchivosExcel/Calculado_Incremento_BAI/Archivos_MLM_JuanCarlos/Archivos MLM con clima/",n,sep=""))
  }
}
