library(readxl)
library(tibble)
library(readr)






#Estrategia: Generar una funcion que tome 2 argumentos string, el primero es el nombre del archivo de trayectoria, el segundo es el nombre del archivo de proceso de reinscripcion,
# e imprima las hojas correspondientes con las columnas filtradas y agregadas, en particular la de observaciones



####Condiciones: 
#Las columnas de observaciones deben de ser la misma entre los documentos generados. Las observaciones solamente se pueden hacer con la hoja de trayectoria, y el unico
#vinculo entre la hoja de trayectoria y la de proceso es la parte del nombre del archivo que contiene la generacion y el modulo. 



#Leemos los nombres de los archivos para su procesamiento-------------------------------------------------------------------------------------------------
#
#Impresion<-function(nombre_trayectoria,nombre_proceso, numero_de_grupos,numero_de_campus)
{#
nombre_trayectoria<-"Trayectoria_G50M3"
############# PROSPECT FUNCTION
nombre_proceso<-"Proceso_Reinscripcion_G50M3"

numero_de_grupos<-121

numero_de_campus<-2


if(!substring(nombre_proceso, first = 9, last = 9) == "R"){
  print("Cuidado: Pusiste el nombre de una hoja de procesos de candidatos, no de reinscripciones. ")
}





#Agregamos parte que falta para que se lea la direccion completa del archivo y se carguen las respectivas bases de datos-------------------------------------------------------
#
#direccion<-function(nombre_trayectoria,nombre_proceso){} PROSPECT FUNCTION
nombre_trayectoria_direccion_lectura<-paste0("Proyectos/", nombre_trayectoria,".xlsx", sep = "")
nombre_proceso_direccion_lectura<-paste0("Proyectos/", nombre_proceso,".xlsx", sep = "")



#Leemos los documentos

#Leemos la hoja de trayectoria


#Lectura_trayectoria<-function(nombre_trayectoria_direccion_lectura, nombre_proceso_direccion_lectura) (###PROSPECT FUNCTION)
{
  dftr<-read_excel(nombre_trayectoria_direccion_lectura)
  
  dfpr<-read_excel(nombre_proceso_direccion_lectura)
  
  if(dim(dftr)[1]!=dim(dfpr)[1]){
    print("Cuidado: Las tablas tienen distinta cantidad de alumnos")
    c<-as.numeric(dftr$Folio %in% dfpr$Folio)
    Bajas<-dftr[which(c==0),]
    dftr<-dftr[which(c==1),]
    View(Bajas)
  }
  
  
  #En este punto, ya se tienen las bases de datos cargadas.
}



#=============================================================================================
#
##Rescatamos la generacion y el modulo de trayectoria, y decidimos si son Regularizados y Ordinarios-----------------------------------------------------------
#Generacion

if(substring(nombre_trayectoria,first = 18)==""){
  gen_tr<-substring(nombre_trayectoria, first = 13, last = 15)
  ### APLICAR EL METODO ORDINARIO DE TRAYECTORIA
}else{
  if(substring(nombre_trayectoria,first = 18, last = 19)== "-R"){
    gen_tr<-substring(nombre_trayectoria, first = 13, last = 15)
    ### APLICAR EL METODO REGULARIZADO DE TRAYECTORIA

  }else{
    gen_tr<-substring(nombre_trayectoria, first = 13, last = 15)
    if(substring(nombre_trayectoria,first = 19, last = 20)== "-R"){
      ### gen_tr<-substring(nombre_trayectoria, first = 13, last = 15)
      ### APLICAMOS METODO REGULARIZADO DE TRAYECTORIA
      
    }else{
      ### gen_tr<-substring(nombre_trayectoria, first = 13, last = 15)
      ### APLICAMOS METODO ORDINARIO DE TRAYECTORIA
     
    }
  }
}


#========================================================================================$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

if(substring(nombre_trayectoria,first = 18)==""){
  mod_tr<-substring(nombre_trayectoria, first = 16, last = 17)
  ap<-substring(nombre_trayectoria,first = 18)
  ### APLICAR EL METODO ORDINARIO DE TRAYECTORIA
}else{
  if(substring(nombre_trayectoria,first = 18, last = 19)== "-R"){
    mod_tr<-substring(nombre_trayectoria, first = 16, last = 17)
    ap<-substring(nombre_trayectoria,first = 18)
    ### APLICAR EL METODO REGULARIZADO DE TRAYECTORIA
  }else{
    mod_tr<-substring(nombre_trayectoria, first = 16, last = 18)
    ap<-substring(nombre_trayectoria,first = 19)
    
  }
}


#========================================================================================$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

gen_mod_tr<-paste0(gen_tr, mod_tr, sep = "")

#========================================================================================$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$



#Rescatamos la generacion y el modulo de proceso, y decidimos si son Regularizados y Ordinarios--------------------------------------------------------------


#Generacion

if(substring(nombre_proceso,first = 28)==""){
  gen_pr<-substring(nombre_proceso, first = 23, last = 25)
  ### APLICAR EL METODO ORDINARIO DE PROCESO DE REINSCRIPCION
}else{#####
  if(substring(nombre_proceso,first = 28, last = 29)=="-R"){
    gen_pr<-substring(nombre_proceso, first = 23, last = 25)
    ### APLICAR METODO REGULARIZADO DE PROCESO DE REINSCRIPCION
  }else{
    if(substring(nombre_proceso, first = 29, last = 30)=="-R"){
      gen_pr<-substring(nombre_proceso, first = 23, last = 25)
      ### APLICAR METODO REGULARIZADO DE PROCESO DE REINSCRIPCION
    }else{
      gen_pr<-substring(nombre_proceso, first = 23, last = 25)
      #### APLICAR METODO ORDINARIO DE PROCESO DE REINSCRIPCION
    } 
  }
}

#========================================================================================$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$    

#Modulo

if(substring(nombre_proceso,first = 28)==""){
  mod_pr<-substring(nombre_proceso, first = 26, last = 27)
  ### APLICAR EL METODO ORDINARIO DE PROCESO DE REINSCRIPCION
}else{#####
  if(substring(nombre_proceso,first = 28, last = 29)=="-R"){
    mod_pr<-substring(nombre_proceso, first = 26, last = 27)
    ### APLICAR METODO REGULARIZADO DE PROCESO DE REINSCRIPCION
  }else{
    if(substring(nombre_proceso, first = 29, last = 30)=="-R"){
      mod_pr<-substring(nombre_proceso, first = 26, last = 28)
      ### APLICAR METODO REGULARIZADO DE PROCESO DE REINSCRIPCION
    }else{
      mod_pr<-substring(nombre_proceso, first = 26, last = 28)
      #### APLICAR METODO ORDINARIO DE PROCESO DE REINSCRIPCION
    } 
  }
}

#========================================================================================$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$    


gen_mod_pr<-paste0(gen_pr, mod_pr, sep = "")



#========================================================================================$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$    

#
#
#
#======================================================================================


if(!gen_mod_tr== gen_mod_pr){
  print("Cuidado: Tomaste hojas que no coinciden en generación o en módulo.")
}



#Armamos los preliminares para mas adelante y que no sean condicionales----------------------------------------------------------------------------------------

#Columnas de los modulos
modulos<-0

for (i in 1:23){
  t <- paste("M", i, sep = "")
  modulos<- append(modulos, t)
}

modulos <- modulos[-1]#Columnas de los modulos


  fechas_finales<-0
  for(i in 1:length(modulos)){
    a<-paste("FF",modulos[i],sep="")
    fechas_finales<-c(fechas_finales,a)
  }
  fechas_finales<-fechas_finales[-1]




#Contamos las calificaciones reprobatorias
#
#Calificaciones_reprobatorias<-function(dftr){}
### PROSPECT FUNTION
Calificaciones_reprobatorias<- dftr$Folio

for (i in 1:length(Calificaciones_reprobatorias) ){
  #calificacion_individual<-df[i,modulos][!is.na(df[i,modulos])]
  #s<- sum(calificacion_individual<60)
  a<-sum((dftr[i,modulos]<60), na.rm = TRUE)
  Calificaciones_reprobatorias[i]<-a
}
#Columna de calificaciones reprobatorias

#dftr<-add_column(dftr, Calificaciones_reprobatorias, .after = "Folio")
#Esta linea la aplicamos despues!!!!!!!!!!!!!!
#
#


#Fechas finales (NP)
#
fecha<-0
for (i in 1:dim(dftr)[1]){
  for (j in fechas_finales){
    fecha<-c(fecha,dftr[i,j]) 
  }
}
fecha<-fecha[-1]
fecha<-unlist(fecha, use.names = FALSE)


#Columna de calificacion del ultimo modul (NP)

Calificación<-0
for(i in 1:dim(dftr)[1]){
  for(j in modulos){
    Calificación<-c(Calificación, dftr[i,j])
  }
}

Calificación<-Calificación[-1]

Calificación<-unlist(Calificación, use.names = FALSE)


#Column de los NP

ca<-as.numeric(Calificación)
NP<-ceiling(1-(ca/60))


#Columna de los comentarios de modulo reprobado
#
Mod_Rep <- (NP==1)
Mod_Rep[which(is.na(Mod_Rep))]<-FALSE
Mod_Rep[which(Mod_Rep==FALSE)]<- "FALSO"
Mod_Rep[which(Mod_Rep==TRUE)]<-"VERDADERO"


#

#Columna de fechas de nacimiento
fechas_nacimiento<-substring(dftr$CURP, first = 5, last = 10)


#Limpiamos el formato para que quede como "Y-m-d"

  for (i in 1:length(fechas_nacimiento)) {
    
    if(is.na(substring(fechas_nacimiento[i],first = 1, last = 2))){
      print("Alomejor uno de los documentos no tiene datos")
    }else{
    
    if(as.numeric(substring(fechas_nacimiento[i],first = 1, last = 2)) > 30){
      fechas_nacimiento[i]<-paste("19",fechas_nacimiento[i], sep = "")
    }
    else {
      fechas_nacimiento[i]<-paste("20",fechas_nacimiento[i], sep = "")
    }
    fechas_nacimiento[i]<-sub("(.{4})(.{2})(.*)", "\\1-\\2-\\3", fechas_nacimiento[i])
  }
  }
  fechas_nacimiento<-as.Date(fechas_nacimiento, format = "%Y-%m-%d")#Columna de fechas de nacimiento
  
  
  
  #Columna de las edades
  
  Edades<-0
  for(i in 1:length(fechas_nacimiento)){
    Edades<-c(Edades, edades_decimal(fechas_nacimiento[i],Sys.Date()))
  }
  Edades<- Edades[-1]#Columna de las edades
  #Esta guardado como vector
  
  Folios<-data.frame(Folios = as.character(dftr$Folio))
  ###################
  
  
  
  #Generamos la Columna de las observaciones
  #
  #Primero las observaciones acerca de cuantas calificaciones reprobatorias tiene
  #
  #Obs_Ord_Cont_Rep<-function(Calificaciones_reprobatorias){} ### PROSPECT FUNCTION
  
  Obs_Ord_Cont_Rep<-Calificaciones_reprobatorias
  for(i in 1:dim(dftr)[1]){
    if(Obs_Ord_Cont_Rep[i]>=3){
      a<-paste("EL ALUMNO TIENE 3 MATERIAS REPROBADAS", sep="")
      Obs_Ord_Cont_Rep[i]<-a
    }else{
      a<-""
      Obs_Ord_Cont_Rep[i]<-a
    }
  }
  Obs_Ord_Cont_Rep<-data.frame(Obs_Ord_Cont_Rep)
  #las observaciones acerca de cuantas calificaciones reprobatorias tiene
  #Esta guardad como data.frame
  
  
  #Luego hacemos las observaciones del estatus
  #
  
  Obs_est_ord<-dftr[, "Estatus"]
    for(i in 1:dim(dftr)[1]){
     # Obs_est_ord<-c(Obs_est_ord,substring(dftr[i, "Estatus"], first = 1, last = 1))
                     if(dftr[i,"Estatus"]=="B"){
                       a<-paste("El alumno está en ", dftr$Estatus, sep = "") #Posible cambiar a mayusculas
                       Obs_est_ord[i]<-a
                     }
                     else{
                       a<-""
                       Obs_est_ord[i]<-a
                     }
    }
  #Obs_est_ord<-Obs_est_ord[-1]
  #Obs_est_ord<-data.frame(Obs_est_ord)
    #Columna de las observaciones de estatus
    #Esta guardado como data.frame
  
  
    
    #Observaciones de calificaciones del modulo: ESTO SI ES CONDICIONAL, DEPENDE DE SI SON REGULARIZADOS O NO
    #
    ###obs_mod<-function(dftr){
    ###
    Obs_mod1<-"__"
      if(substring(nombre_trayectoria, first = 19, last = 19) == "R" || substring(nombre_trayectoria, first = 20, last = 20) == "R"){
        ###obs_modr(dftr)
        
        Obs_mod<-dftr[,mod_tr]
        
        for(i in 1:dim(Obs_mod)[1]){
          if(Obs_mod[i,1] %in% 41:59 || is.na(Obs_mod[i,1])){
            a<-""
            Obs_mod1<-c(Obs_mod1,a)
          }
          else{
            a<-paste("El alumno tiene calificación menor a 41 o mayor a 59 en el módulo ", substring(mod_tr,first=2), sep="")
            Obs_mod1<-c(Obs_mod1,a)
          }
        }
        
      }else{
        ###obs_modo(dftr)
        
        Obs_mod<-dftr[,mod_tr]
        for(i in 1:dim(Obs_mod)[1]){
          if(!is.na(Obs_mod[i,1])){
            a<-paste("El alumno ya tiene calificación en el módulo ", substring(mod_tr,first=2), sep="")
            Obs_mod1<-c(Obs_mod1,a)
          }
          else{
            a<-""
            Obs_mod1<-c(Obs_mod1,a)
          }
        }
        
      }
    Obs_mod1<-Obs_mod1[-1]
    Obs_mod<-data.frame(Obs_mod1)
###}
    #
    #
    #
    #Hacemos las observaciones ya todas juntas
    
    Obs<-1:dim(dftr)[1]
    for( i in 1:dim(dftr)[1] ){
      Obs[i]<-sum( (c(Obs_est_ord[i,1], Obs_Ord_Cont_Rep[i,1], Obs_mod[i,1])!= "") >1, na.rm = TRUE )
      if(Obs[i]>1){
        Obs[i]<-paste0(Obs_est_ord[i,1], Obs_Ord_Cont_Rep[i,1], Obs_mod[i,1], collapse = " y ")
      }else{
        Obs[i]<-paste0(Obs_est_ord[i,1], Obs_Ord_Cont_Rep[i,1], Obs_mod[i,1], collapse = " ")
      }
    }
    Obs<-data.frame(Obs)
    
    
    #Convertimos columnas d Edades y Calificaciones en df. Lo hacemos hasta aqui abajo para que no se afecte el metodo arriba por cambiar el tipo
    Calificaciones_reprobatorias<-data.frame(Calificaciones_reprobatorias)
    
    
    
    Edades<-data.frame(Edades)
    
    
    #Generamos el data.frame de la hoja de procesos, cuyas columnas son las que ya tenia el documento de lectura
    #mas la columna de observaciones, que son las observaciones en comun de esta tabla y la tabla de trayectoria
    dfpr<-cbind(dfpr, Obs) 
    
    
    #Generamos la tabla de los NP, cuyas columnas son Folios, Modulos, fecha, Calificacion, NP y Modulos reprobados (Mod Rep)
    dfnp<-data.frame(Folios, Módulos = modulos, fecha, Calificación, NP, Mod_Rep)
    
    columnas<-c("Folio",
                #"Nombre",
                #"Apellidos",
                "Generación",
                "CURP",
                "Estatus",
                "Grupo Actual",
                "Fecha Fin de última asignacion",
                modulos)
    dftr<-dftr[, columnas]
    dftr<-add_column(dftr, Obs, .after = "Folio")
    
   #Generamos el data.frame que corresponde a la tabla de trayectoria. Sus columnas son Folio, Generacion, CURP, Estatus, Grupo Actual, Fecha
   #final de ultima asignacion, los Modulos, y quizas otras... (13/06/23) 
    dftr<-add_column(dftr, Calificaciones_reprobatorias, .after = "Obs")
    
    nombre_grupo<-paste0(mod_pr, "C1", gen_pr, ap)
    

    dfas<-data.frame(Folio = Folios, Nombre_Completo =  paste(dfpr$Nombre, dfpr$Apellidos, sep = " "), Correo = dfpr$Correo)
    dfas<- dfas[which(Obs == ""),]
    
    
    ##########Generacion de los grupos. Para que no haya alumnos que se queden sin asignacion, se va a correr la generacion una y otra vez hasta que la suma de los
    ##########cupos de los grupos de el total
    
    
  #  Generacion_de_grupos<-function(dfas){
    dfas1<-data.frame(NULL, NULL, NULL)
    cardinalidad_del_grupo<-1:numero_de_grupos
    while(sum(cardinalidad_del_grupo)!=dim(dfas)[1]){
      for(i in 1:numero_de_grupos){
        cardinalidad_del_grupo[i]<-sample(floor(dim(dfas)[1]/numero_de_grupos):ceiling(dim(dfas)[1]/numero_de_grupos),1)
      }
    }
    cardinalidad_del_grupo<-c(0,cardinalidad_del_grupo)
    
    
    
    
    #Generacion de los campuses
    nombre_del_campus<-1:numero_de_campus
    dimensiones_de_los_campus<-1:numero_de_campus
    cardinalidad_del_campus=1:numero_de_campus
    cardinalidad_del_campus<-c(0,cardinalidad_del_campus)
    for(i in 1:numero_de_campus){
      if(i*ceiling(numero_de_grupos/numero_de_campus)>numero_de_grupos){
        cardinalidad_del_campus[i+1]<- floor(numero_de_grupos/numero_de_campus)
      }
      else{
        cardinalidad_del_campus[i+1]<- ceiling(numero_de_grupos/numero_de_campus)
      }
      nom_campus<-paste0("C", i, sep = "")
      campus<-dfas[sample(nrow(dfas),cardinalidad_del_campus[i+1]),]
      assign(nom_campus, campus)
      #if(i ==1 ){
        #nombre_del_campus<-nom_campus
       # dimensiones_de_los_campus<-sum(cardinalidad_del_grupo[1:cardinalidad_del_campus[i]])
      #}else{
        nombre_del_campus[i]<-nom_campus
        dimensiones_de_los_campus[i] <- sum(cardinalidad_del_grupo[seq(from = sum(cardinalidad_del_campus[1:i])+2, to = min(length(cardinalidad_del_grupo), sum(cardinalidad_del_campus[1:i+1]+1)))])
      #}
    }
    
    dimensiones_de_los_campus<-c(0,dimensiones_de_los_campus)
    
    
    numero_promedio_de_grupos_por_campus<-numero_de_grupos/numero_de_campus
    numero_promedio_de_alumnos_por_campus<-dim(dfas)[1]/numero_de_campus
    Grupos<-1:dim(dfas)[1]
    grupo_en_formacion<-data.frame(Folios = c(""), Nombre_completo = c(""), Correo = c(""))
    #dfas1<-grupo_en_formacion
    
    for(j in 1:numero_de_grupos){
      for (k in 1:numero_de_campus){
        #for(i in 1:dim(dfas)[1]){
          #if( i %in% (seq(from = sum(cardinalidad_del_grupo[1:j])+1, to = sum(cardinalidad_del_grupo[1:(j+1)]))) ){
            if(j<10){
              u<-paste0("-00",j , sep = "")
            }else{
              if(j<100){
                u<-paste0("-0",j, sep = "")
              }else{
                u<-paste0("-",  j, sep = "")
              }
            }
           # if(sum(cardinalidad_del_grupo[1:j+1] %in% seq(from = dimensiones_de_los_campus[k]+1, to = dimensiones_de_los_campus[k+1]))){
              nom_grupo<-paste0(mod_tr, "C", min(1 +  floor(sum(cardinalidad_del_grupo[1:j+1])/(numero_promedio_de_alumnos_por_campus)),numero_de_campus)  ,gen_tr,ap,u,sep="")
           # }else{
             # k == k+1
            #}
            c<-as.numeric(dfas$Folios %in% grupo_en_formacion$Folios)
            grupo_en_formacion<-dfas[which(c == 0),]
            grupo_en_formacion<-grupo_en_formacion[sample( seq(from = sum(dimensiones_de_los_campus[1:k])+1,to = sum(dimensiones_de_los_campus[1:k+1])),cardinalidad_del_grupo[j+1]),]
            assign(nom_grupo,grupo_en_formacion )
            Grupos[seq(from = sum(cardinalidad_del_grupo[1:j])+1, to = sum(cardinalidad_del_grupo[1:j+1]))]<-nom_grupo
           # if(j == 1 ){
           #   dfas1<-grupo_en_formacion
            #}else{
              dfas1<-rbind(dfas,grupo_en_formacion)
             # }
           # }
         # }
        }
      }
    }  
    dfas1<-dfas1[-1:dim(dfas)[1],]
    dfas1<-cbind(dfas1,Grupo = Grupos)
    
    
    
    #=============================================================================================================================
    
    
    
    #Mandamos a imprimir
    #
    direccion_pr<-paste0("Proyectos/", gen_mod_tr, "/", substring(nombre_proceso, first = 1, last = 21 ), "2", substring(nombre_proceso, first = 22), ".csv", collapse = "" )
    direccion_tr<-paste0("Proyectos/", gen_mod_tr, "/Calificaciones.csv" , collapse = "" )
    direccion_np<-paste0("Proyectos/", gen_mod_tr, "/NP.csv" , collapse = "" )
    direccion_as<-paste0("Proyectos/", gen_mod_tr, "/Asignaciones_",  gen_mod_pr , ap, ".csv" , collapse = "" )
    dir.create(substring(direccion_pr, first = 1, last= 16))
    write_excel_csv(dfpr, direccion_pr, na = "")
    write_excel_csv(dftr, direccion_tr, na = "")
    write_excel_csv(dfnp, direccion_np, na = "")
    write_excel_csv(dfas1, direccion_as, na = "" )
    
}

    
    #=============================================================================================================================#=============================================================================================================================#=============================================================================================================================#=============================================================================================================================#=============================================================================================================================#=============================================================================================================================#=============================================================================================================================#=============================================================================================================================#=============================================================================================================================#=============================================================================================================================#=============================================================================================================================#=============================================================================================================================#=============================================================================================================================#=============================================================================================================================
    

    
    #########################################################################################################################
    ############################################################################################################################


### METODO ORDINARIO DE TRAYECTORIA-------------------------------------------------------------------------------------------------------------------------
#

  
    
    #Obs_mod<-funtion(dftr){}### PROSPECT FUNCTION
    obs_modo<-function(dftr){
    Obs_mod<-dftr[,mod_tr]
    for(i in 1:dim(Obs_mod)[1]){
      if(!is.na(Obs_mod[i,1])){
        a<-paste("El alumno ya tiene calificación en el módulo ", substring(mod_tr,first=2), sep="")
        Obs_mod[i,1]<-a
      }
      else{
        a<-""
        Obs_mod[i,1]<-a
      }
    }#Esta guardado como data.frame
    }
  
  
  
  ### METODO REGULARIZADO DE TRAYECTORIA-------------------------------------------------------------------------------------------------------------------------
  # 
  obs_modr<-function(dftr){
  Obs_mod<-dftr[,mod_tr]
  for(i in 1:dim(Obs_mod)[1]){
    if(Obs_mod[i,1] %in% 41:59 || is.na(Obs_mod[i,1])){
      a<-""
      Obs_mod[i,1]<-a
    }
    else{
      a<-paste("El alumno tiene calificación menor a 41 o mayor a 59 en el módulo ", substring(mod_tr,first=2), sep="")
      Obs_mod[i,1]<-a
    }
  }
  }#Esta guardado como data.frame
  

  
  
  ##############################################################
  ##############################################################
  ##############################################################
  ##############################################################
  
  
  Generacion_de_grupos<-function(dfas){
    
    for(i in 1:numero_de_campus){
      campus<-dfas[sample(nrow(dfas),round(dim(dfas)[1]/numero_de_campus)),]
      name_campus<-paste0("C", i)
      for(j in 1:numero_de_grupos){
        #if(i == 1 && j == 1){
        name_grupo<-paste0(mod_tr, name_campus, gen_tr,ap,"-00",j, sep = "")
        p<-campus[sample(nrow(campus),sample(floor(dim(campus)[1]/numero_de_grupos):ceiling(dim(campus)[1]/numero_de_grupos), 1)),]
        assign(name_grupo, p)
        Grupos[seq(from = ((i-1)*round(dim(campus)[1]/numero_de_grupos)*numero_de_grupos) + ((j-1)*round(dim(campus)[1]/numero_de_grupos)+1) , to = ((i-1)*round(dim(campus)[1]/numero_de_grupos)*numero_de_grupos) + (j*round(dim(campus)[1]/numero_de_grupos)))]<-name_grupo
        if(i*j==1){
          dfas1<-p    
        }else{
          dfas1<-rbind(dfas1,p)
        }
        #  dfas<-p
        #}else{
        # name_grupo<-paste0(mod_tr, name_campus, gen_tr,ap,"-00",j, sep = "")
        # p<-campus[sample(nrow(campus),round(dim(campus)[1]/numero_de_grupos)),]
        #  assign(name_grupo, p)
        # Grupos[seq(from = ((i-1)*round(dim(campus)[1]/numero_de_grupos)*numero_de_grupos) + ((j-1)*round(dim(campus)[1]/numero_de_grupos)+1) , 
        #           to = ((i-1)*round(dim(campus)[1]/numero_de_grupos)*numero_de_grupos) + (j*round(dim(campus)[1]/numero_de_grupos)))]<-name_grupo
        #dfas<-rbind(dfas,p)  
        #}
      }
    }
    #Aparecen ordenados por campus
    return(dfas1)
    return(Grupos)
  }
  
  ### TAREA PARA EL LUNES: EN LA PARTE EN DONDE SE DEFINE SI LAS HOJAS SON DE REGULARIZADOS O NO, PONER obs_modr DONDE SEA DE REGULARIZADOS, Y DONDE SEA
  ### DE ORDINARIOS PONER obs_modo
  ### 
  ### 
  ### FALTA: 
  ### 1. LA TABLA DE LOS NP
  ### 2. ESCRIBIR LAS DIRECCIONES
  ### 3. ESCRIBIR LOS ARCHIVOS CSV