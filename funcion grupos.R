dfas1<-data.frame(NULL, NULL, NULL)
while(dim(dfas1)[1]!=dim(dfas)[1]){  
  Grupos<-1:dim(dfas)[1]
  for(i in 1:numero_de_campus){
    campus<-dfas[sample(nrow(dfas),round(dim(dfas)[1]/numero_de_campus)),]### Aqui habra que hacer un ajuste de la manera de dividir los campus
    name_campus<-paste0("C", i)
    for(j in 1:numero_de_grupos){
      #if(i == 1 && j == 1){
      if(j+ (i-1)<10){
        u<-paste0("-00",j + (i-1)*numero_de_grupos , sep = "")
      }else{
        if(j+ (i-1)<100){
          u<-paste0("-0",j + (i-1)*numero_de_grupos, sep = "")
        }else{
          u<-paste0("-",  j+ (i-1)*numero_de_grupos, sep = "")
        }
      }
      name_grupo<-paste0(mod_tr, name_campus, gen_tr,ap ,  u , sep = "")
      p<-campus[sample(nrow(campus),sample(floor(dim(campus)[1]/numero_de_grupos):ceiling(dim(campus)[1]/numero_de_grupos), 1)),]
      assign(name_grupo, p)
      Grupos[seq(from = ((i-1)*round(dim(campus)[1]/numero_de_grupos)*numero_de_grupos) + ((j-1)*round(dim(campus)[1]/numero_de_grupos)+1) , to = ((i-1)*round(dim(campus)[1]/numero_de_grupos)*numero_de_grupos) + (j*round(dim(campus)[1]/numero_de_grupos)))]<-name_grupo
      if(i*j==1){
        dfas1<-p    
      }else{
        dfas1<-rbind(dfas1,p)
      }
      #dfas<-p
      #}else{
      # name_grupo<-paste0(mod_tr, name_campus, gen_tr,ap,"-00",j, sep = "")
      #p<-campus[sample(nrow(campus),round(dim(campus)[1]/numero_de_grupos)),]
      #assign(name_grupo, p)
      #Grupos[seq(from = ((i-1)*round(dim(campus)[1]/numero_de_grupos)*numero_de_grupos) + ((j-1)*round(dim(campus)[1]/numero_de_grupos)+1) , 
      #          to = ((i-1)*round(dim(campus)[1]/numero_de_grupos)*numero_de_grupos) + (j*round(dim(campus)[1]/numero_de_grupos)))]<-name_grupo
      #dfas<-rbind(dfas,p)  
      #}
    }
  }
}


cardinalidad_del_grupo<-1:numero_de_grupos
while(sum(cardinalidad_del_grupo)!=dim(dfas)[1]){
  for(i in 1:numero_de_grupos){
    cardinalidad_del_grupo[i]<-sample(floor(dim(dfas)[1]/numero_de_grupos):ceiling(dim(dfas)[1]/numero_de_grupos),1)
  }
}
cardinalidad_del_grupo<-c(0,cardinalidad_del_grupo)




#Generacion de los campuses
cardinalidad_del_campus=1:numero_de_campus
for(i in 1:numero_de_campus){
  if(i*ceiling(numero_de_grupos/numero_de_campus)>numero_de_grupos){
    cardinalidad_del_campus[i]<- floor(numero_de_grupos/numero_de_campus)
  }
  else{
    cardinalidad_del_campus[i]<- ceiling(numero_de_grupos/numero_de_campus)
  }
  nom_campus<-paste0("C", i, sep = "")
  campus<-dfas[sample(nrow(dfas),cardinalidad_del_campus[i]),]
  assign(nom_campus, campus)
  if(i ==1 ){
    nombre_del_campus<-campus
    dimensiones_de_los_campus<-sum(cardinalidad_del_grupo[1:cardinalidad_del_campus[i]])
  }else{
    nombre_del_campus<-c(nombre_del_campus,campus)
    dimensiones_de_los_campus<-c(dimensiones_de_los_campus, sum(cardinalidad_del_grupo[(i-1):cardinalidad_del_campus[i]]))
  }
}
cardinalidad_del_campus<-c(0,cardinalidad_del_campus)
dimensiones_de_los_campus<-c(0,dimensiones_de_los_campus)

Grupos<-1:dim(dfas)[1]

for(j in 1:numero_de_campus){
  for(i in 1:numero_de_grupos){
    if( i %in% (sum(cardinalidad_del_campus[1:j])+1:sum(cardinalidad_del_campus[j+1])) ){
      if(i<10){
        u<-paste0("-00",i , sep = "")
      }else{
        if(i<100){
          u<-paste0("-0",i, sep = "")
        }else{
          u<-paste0("-",  i, sep = "")
        }
      }
      nom_grupo<-paste0(mod_tr,nombre_del_campus[j],gen_tr,ap,u,sep="")
      grupo_en_formacion<-dfas[sample( sum(dimensiones_de_los_campus[1:j])+1:sum(dimensiones_de_los_campus[1:j+1]),cardinalidad_del_grupo[i]),]
      assign(nom_grupo,grupo_en_formacion )
      Grupos[cardinalidad_del_grupo[i]+1:cardinalidad_del_grupo[i+1]]<-nom_grupo
      if(i == 1 ){
        dfas1<-grupo_en_formacion
      }else{
        dfas1<-rbind(dfas1,grupo_en_formacion)
        }
    }
  }
}

dfas1<-cbind(dfas1,Grupo)




for (k in 1:numero_de_campus){
  for(j in 1:numero_de_grupos){
    for(i in 1:dim(dfas)[1]){
      if( i %in% (seq(from = sum(cardinalidad_del_grupo[1:j])+1, to = sum(cardinalidad_del_grupo[1:(j+1)]))) ){
        if(i<10){
          u<-paste0("-00",i , sep = "")
        }else{
          if(i<100){
            u<-paste0("-0",i, sep = "")
          }else{
            u<-paste0("-",  i, sep = "")
          }
        }
        if(sum(cardinalidad_del_grupo[]))
          nom_grupo<-paste0(mod_tr,nombre_del_campus[1+(floor())],gen_tr,ap,u,sep="")
        grupo_en_formacion<-dfas[sample( seq(from = dimensiones_de_los_campus[k]+1,to = dimensiones_de_los_campus[k+1]),cardinalidad_del_grupo[j]),]
        assign(nom_grupo,grupo_en_formacion )
        Grupos[cardinalidad_del_grupo[j]+1:cardinalidad_del_grupo[j+1]]<-nom_grupo
        if(i == 1 ){
          dfas1<-grupo_en_formacion
        }else{
          dfas1<-rbind(dfas1,grupo_en_formacion)
        }
      }
    }
  }
}  

for(j in 1:numero_de_grupos){
  for (k in 1:numero_de_campus){
    for(i in 1:dim(dfas)[1]){
      if( i %in% (seq(from = sum(cardinalidad_del_grupo[1:j])+1, to = sum(cardinalidad_del_grupo[1:(j+1)]))) ){
        if(j<10){
          u<-paste0("-00",j , sep = "")
        }else{
          if(i<100){
            u<-paste0("-0",j, sep = "")
          }else{
            u<-paste0("-",  j, sep = "")
          }
        }
        if(sum(cardinalidad_del_grupo[1:j+1] %in% seq(from = dimensiones_de_los_campus[k], to = dimensiones_de_los_campus[k+1]))){
          nom_grupo<-paste0(mod_tr,nombre_del_campus[k],gen_tr,ap,u,sep="")
        }  
        grupo_en_formacion<-dfas[sample( seq(from = dimensiones_de_los_campus[k]+1,to = dimensiones_de_los_campus[k+1]),cardinalidad_del_grupo[j]),]
        assign(nom_grupo,grupo_en_formacion )
        Grupos[cardinalidad_del_grupo[j]+1:cardinalidad_del_grupo[j+1]]<-nom_grupo
        if(j == 1 ){
          dfas1<-grupo_en_formacion
        }else{
          dfas1<-rbind(dfas1,grupo_en_formacion)
        }
      }
    }
  }
}  



