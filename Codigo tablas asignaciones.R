#Codigo completo

{#Primer bracket del codigo completo
  

#Parte modular del codigo:Leer los archivos correspondientes

 #Leer csv  
#Tray <- read.csv("Proyectos/Trayectoria_REM57.csv", encoding="latin1")

#Forma 1 de leer excel  
library(readxl)
Tray <- read_excel("Proyectos/Trayectoria_Candidatos__G34M16_05-06-2023.xlsx")#Usamos el paquete que lee xlsx porque con csv marca problemas sobre la "linea final"la codificacion.
#View(Tray)


#Forma 2 de leer um xlsx
#library(xlsx)
#Tray <- read.table("Proyectos/Trayectoria_Candidatos__G34M16_05-06-2023.xlsx",
 #                  encoding="UTF-8")
  
#Almacenamos el nombre del archivo para sustraer el modulo y la generacion
nombre<-"Trayectoria_Candidatos__G34M16_05-06-2023"
  
  
#1.Contar calificaciones reprobatorias-----

#Importamos el data.frame de los estudiantes
  

#Primera parte: Nos restringimos a las columnas de interes
{#Primer braket de 1

  
#Generamos un vector con las columnas que nos interesa analizar
{
modulos<-0

for (i in 1:23){
  t <- paste("M", i, sep = "")
  modulos<- append(modulos, t)
}

modulos <- modulos[-1]

columnas <- c("Folio", "Nombre", "Apellidos", "Estatus", "CURP",  modulos, fechas_finales )

#Restringimos la tabla a las columnas que nos interesan
Tray<- Tray[,columnas]




#Contamos las calificaciones reprobatorias
Calificaciones_reprobatorias<- 0

for (i in 1:dim(Tray)[1] ){
  #calificacion_individual<-Tray[i,modulos][!is.na(Tray[i,modulos])]
  #s<- sum(calificacion_individual<60)
  s<-sum((Tray[i,modulos]<60), na.rm = TRUE)
  Calificaciones_reprobatorias<-c(Calificaciones_reprobatorias, s)
}
Calificaciones_reprobatorias<-Calificaciones_reprobatorias[-1]
}

#Segunda parte: Agregamosordenamos las columnas que nos interesan y generamos la tabla variable Tray_mr
{
Tray$Calificaciones_reprobatorias<-Calificaciones_reprobatorias

Tray_mr<-Tray[,c("Folio", "Nombre", "Estatus", "Apellidos", "CURP", "Calificaciones_reprobatorias",  modulos, fechas_finales)]
}
#View(Tray_mr)
  

#Pasamos las columnas de modulos como vectores para cambiar los NA por ""
  
#Hacemos este proceso porque R señala una "incompatibilidad" de los datos cuando tratamos de hacer la sustitución directamente
  #en el data.frame
  
  
#{
#Generamos la matriz de las columnas de los modulos
 # modulos_vector<-Tray_mr$M1
  #for(i in modulos){
    #l<-as.vector()
   # modulos_vector<-cbind(modulos_vector,Tray_mr[,i]) #Observacion: Convertir las columnas a vector funcionó cuando el archivo leido es csv. 
                                                      #No funcionó cuando el archivo es xlxs, 
                                                      #se arma la matriz con las columnas directamente, sin cambiar el tipo de dato
 # }
 # modulos_vector<-modulos_vector[,-1]
  
#Cambiamos los NA por ""  
#for (i in 1:ncol(modulos_vector)){
 # modulos_vector[which(is.na(modulos_vector[,i])),i]<-""
#}
  
#Cambiamos las columnas originales del data.frame por las nuevas que ya no tienen NA
#Tray_mr[,modulos]<-modulos_vector
  
#}
#NO HACE FALTA HACER TODO ESTO, SE PUEDE ESCRIBIR EL CSV HACIENDO na=""

}#braket final de 1

#View(Tray)



#write.csv(Tray_materias_reprobadas, "C:\\Users\\Luis Lopez\\Documents\\Proyectos\\Trayectoria_REM57_Materias_reprobadas.csv", row.names = FALSE)



#2.Edad de los estudiantes----

#Obtenemos la informacion de la fecha de nacimiento contenida en la CURP
{#braket inicial de 2
  
{
    fechas_nacimiento<-substring(Tray_mr$CURP, first = 5, last = 10)
}
  
#Limpiamos el formato para que quede como "Y-m-d"
{
  for (i in 1:length(fechas_nacimiento)) {
    if(as.numeric(substring(fechas_nacimiento[i],first = 1, last = 2)) > 30){
      fechas_nacimiento[i]<-paste("19",fechas_nacimiento[i], sep = "")
}
    else {
      fechas_nacimiento[i]<-paste("20",fechas_nacimiento[i], sep = "")
    }
    fechas_nacimiento[i]<-sub("(.{4})(.{2})(.*)", "\\1-\\2-\\3", fechas_nacimiento[i])
  }
    
    fechas_nacimiento<-as.Date(fechas_nacimiento, format = "%Y-%m-%d")}
  
  
#Generamos un vector de las edades
{
  Edades<-0
    for(i in 1:length(fechas_nacimiento)){
      Edades<-c(Edades, edades_decimal(fechas_nacimiento[i],Sys.Date()))
    }
    Edades<- Edades[-1]
}
  
  #Agregamos las columnas correspondientes
{
    Tray_mr$Edades<-Edades
    Tray_edades_y_materias<-Tray_mr[,c("Folio", "Edades","Estatus", "Nombre", "Apellidos", "CURP", "Calificaciones_reprobatorias",  modulos, fechas_finales)]
}
  
  
  

#View(Tray_edades_y_materias)

#write.csv(Tray_edades_y_materias, "C:\\Users\\Luis Lopez\\Documents\\Proyectos\\Tray_edades_y_materias.csv", row.names = FALSE)




}#Braket final de 2

#View(Tray_1)

#write.csv(Tray_1, "C:\\Users\\Luis Lopez\\Documents\\Proyectos\\Trayectoria_REM57_1.csv", row.names = FALSE)



#3.Columnas de observaciones-----
{#Braket inicial de 3
  #Primera tarea: Generamos todas las columnas de observaciones 
  {
    #Generamos la columna de observaciones1 (Cantidad de materias reprobadas)
    {
      Observaciones1<- 0 
      for(i in 1:dim(Tray_edades_y_materias)[1]){
        if(Tray_edades_y_materias[i, "Calificaciones_reprobatorias"]>=3){
          s<-paste("EL ALUMNO TIENE 3 MATERIAS REPROBADAS", sep="")
          Observaciones1<-c(Observaciones1,s)
        }else{
          s<-""
          Observaciones1<-c(Observaciones1,s)
        }
        
        
      }
      Observaciones1<- Observaciones1[-1]
    }
    
    
    
    #Generamos la columna de observaciones2 (Baja parcial)
    {
      Observaciones2<- 0 
      for(i in 1:dim(Tray_edades_y_materias)[1]){
        if(Tray_edades_y_materias[i, "Estatus"] == "Baja parcial" || Tray_edades_y_materias[i, "Estatus"] == "Baja Parcial" ){
          s<-paste("EL ALUMNO TIENE BAJA PARCIAL", sep="")
          Observaciones2<-c(Observaciones2,s)
        }else{
          s<-""
          Observaciones2<-c(Observaciones2,s)
        }
        
        
      }
      Observaciones2<- Observaciones2[-1]
    }
    
    #observaciones3 Baja temporal
    {
      Observaciones3<- 0 
      for(i in 1:dim(Tray_edades_y_materias)[1]){
        if(Tray_edades_y_materias[i, "Estatus"] == "Baja temporal" || Tray_edades_y_materias[i, "Estatus"] == "Baja Temporal" ){
          s<-paste("EL ALUMNO TIENE BAJA TEMPORAL", sep="")
          Observaciones3<-c(Observaciones3,s)
        }else{
          s<-""
          Observaciones3<-c(Observaciones3,s)
        }
        
        
      }
      Observaciones3<- Observaciones3[-1]
    }
    
    #observaciones4 Baja definitiva por sancion
    {
      Observaciones4<- 0 
      for(i in 1:dim(Tray_edades_y_materias)[1]){
        if(Tray_edades_y_materias[i, "Estatus"] == "Baja Definitiva por Sanción" || Tray_edades_y_materias[i, "Estatus"] == "Baja definitiva por sanción" ){
          s<-paste("EL ALUMNO TIENE BAJA DEFINITIVA POR SANCION", sep="")
          Observaciones4<-c(Observaciones4,s)
        }else{
          s<-""
          Observaciones4<-c(Observaciones4,s)
        }
        
        
      }
      Observaciones4<- Observaciones4[-1]
    }
    
    #observaciones5 Baja Definitiva por Oportunidades
    {
      Observaciones5<- 0 
      for(i in 1:dim(Tray_edades_y_materias)[1]){
        if(Tray_edades_y_materias[i, "Estatus"] == "Baja Definitiva por Oportunidades" || Tray_edades_y_materias[i, "Estatus"] == "Baja definitiva por oportunidades" ){
          s<-paste("EL ALUMNO TIENE BAJA DEFINITIVA POR OPORTUNIDADES", sep="")
          Observaciones5<-c(Observaciones5,s)
        }else{
          s<-""
          Observaciones5<-c(Observaciones5,s)
        }
        
        
      }
      Observaciones5<- Observaciones5[-1]
    }
    
    #OBSERVACIONES6 Baja Definitiva Solicitada
    {
      Observaciones6<- 0 
      for(i in 1:dim(Tray_edades_y_materias)[1]){
        if(Tray_edades_y_materias[i, "Estatus"] == "Baja Definitiva Solicitada" || Tray_edades_y_materias[i, "Estatus"] == "Baja definitiva solicitada" ){
          s<-paste("EL ALUMNO TIENE BAJA DEFINITIVA SOLICITADA", sep="")
          Observaciones6<-c(Observaciones6,s)
        }else{
          s<-""
          Observaciones6<-c(Observaciones6,s)
        }
        
        
      }
      Observaciones6<- Observaciones6[-1]
    }
    
    #OBSERVACIONES7 Baja Definitiva por Inactividad
    {
      Observaciones7<- 0 
      for(i in 1:dim(Tray_edades_y_materias)[1]){
        if(Tray_edades_y_materias[i, "Estatus"] == "Baja Definitiva por Inactividad" || Tray_edades_y_materias[i, "Estatus"] == "Baja definitiva por inactividad" ){
          s<-paste("EL ALUMNO TIENE BAJA DEFINITIVA POR INACTIVIDAD", sep="")
          Observaciones7<-c(Observaciones7,s)
        }else{
          s<-""
          Observaciones7<-c(Observaciones7,s)
        }
        
        
      }
      Observaciones7<- Observaciones7[-1]
    }
    
    #OBERSVACIONES8 Baja Parcial Solicitada
    {
      Observaciones8<- 0 
      for(i in 1:dim(Tray_edades_y_materias)[1]){
        if(Tray_edades_y_materias[i, "Estatus"] == "Baja Parcial Solicitada" || Tray_edades_y_materias[i, "Estatus"] == "Baja parcial solicitada" ){
          s<-paste("EL ALUMNO TIENE BAJA PARCIAL SOLICITADA", sep="")
          Observaciones8<-c(Observaciones8,s)
        }else{
          s<-""
          Observaciones8<-c(Observaciones8,s)
        }
        
        
      }
      Observaciones8<- Observaciones8[-1]
    }
    
    #OBSERVACIONES9 Suspensión
    {
      Observaciones9<- 0 
      for(i in 1:dim(Tray_edades_y_materias)[1]){
        if(Tray_edades_y_materias[i, "Estatus"] == "Suspensión" || Tray_edades_y_materias[i, "Estatus"] == "suspensión" ){
          s<-paste("EL ALUMNO TIENE SUSPENSION", sep="")
          Observaciones9<-c(Observaciones9,s)
        }else{
          s<-""
          Observaciones9<-c(Observaciones9,s)
        }
        
        
      }
      Observaciones9<- Observaciones9[-1]
    }
    
    #OBSERVACIONES10 Baja Temporal Disciplinaria por plagio
    {
      Observaciones10<- 0 
      for(i in 1:dim(Tray_edades_y_materias)[1]){
        if(Tray_edades_y_materias[i, "Estatus"] == "Baja Temporal Disciplinaria por plagio" || Tray_edades_y_materias[i, "Estatus"] == "Baja Temporal Disciplinaria por Plagio" ){
          s<-paste("EL ALUMNO TIENE BAJA TEMPORAL DISCIPLINARIA POR PLAGIO", sep="")
          Observaciones10<-c(Observaciones10,s)
        }else{
          s<-""
          Observaciones10<-c(Observaciones10,s)
        }
        
        
      }
      Observaciones10<- Observaciones10[-1]
    }
    
    #observaciones11 Activo
    {
      Observaciones11<- 0 
      for(i in 1:dim(Tray_edades_y_materias)[1]){
        if(Tray_edades_y_materias[i, "Estatus"] == "Activo" || Tray_edades_y_materias[i, "Estatus"] == "Activo" ){
          s<-paste("EL ALUMNO ESTA ACTIVO", sep="")
          Observaciones11<-c(Observaciones11,s)
        }else{
          s<-""
          Observaciones11<-c(Observaciones11,s)
        }
        
        
      }
      Observaciones11<- Observaciones11[-1]
    }
    
    #OBSERVACIONES12 Egresado
    {
      Observaciones12<- 0 
      for(i in 1:dim(Tray_edades_y_materias)[1]){
        if(Tray_edades_y_materias[i, "Estatus"] == "Egresado" || Tray_edades_y_materias[i, "Estatus"] == "egresado" ){
          s<-paste("EL ALUMNO ES EGRESADO", sep="")
          Observaciones12<-c(Observaciones12,s)
        }else{
          s<-""
          Observaciones12<-c(Observaciones12,s)
        }
        
        
      }
      Observaciones12<- Observaciones12[-1]
    }
    
    #OBSERVACIONES13 Promovido
    {
      Observaciones13<- 0 
      for(i in 1:dim(Tray_edades_y_materias)[1]){
        if(Tray_edades_y_materias[i, "Estatus"] == "Promovido" || Tray_edades_y_materias[i, "Estatus"] == "promovido" ){
          s<-paste("EL ALUMNO ES PROMOVIDO", sep="")
          Observaciones13<-c(Observaciones13,s)
        }else{
          s<-""
          Observaciones13<-c(Observaciones13,s)
        }
        
        
      }
      Observaciones13<- Observaciones13[-1]
    }
    
    #OBSERVACIONES14 No Promovido
    {
      Observaciones14<- 0 
      for(i in 1:dim(Tray_edades_y_materias)[1]){
        if(Tray_edades_y_materias[i, "Estatus"] == "No Promovido" || Tray_edades_y_materias[i, "Estatus"] == "No promovido" ){
          s<-paste("EL ALUMNO ES NO PROMOVIDO", sep="")
          Observaciones14<-c(Observaciones14,s)
        }else{
          s<-""
          Observaciones14<-c(Observaciones14,s)
        }
        
        
      }
      Observaciones14<- Observaciones14[-1]
    }
    
    #OBSERVACIONES15 Activo con Equivalencia
    {
      Observaciones15<- 0 
      for(i in 1:dim(Tray_edades_y_materias)[1]){
        if(Tray_edades_y_materias[i, "Estatus"] == "Activo con Equivalencia" || Tray_edades_y_materias[i, "Estatus"] == "Activo con equivalencia" ){
          s<-paste("EL ALUMNO ESTA ACTIVO CON EQUIVALENCIA", sep="")
          Observaciones15<-c(Observaciones15,s)
        }else{
          s<-""
          Observaciones15<-c(Observaciones15,s)
        }
        
        
      }
      Observaciones15<- Observaciones15[-1]
    }
    
    #OBSERVACIONES16 Activo Regularizado
    {
      Observaciones16<- 0 
      for(i in 1:dim(Tray_edades_y_materias)[1]){
        if(Tray_edades_y_materias[i, "Estatus"] == "Activo Regularizado" || Tray_edades_y_materias[i, "Estatus"] == "Activo regularizado" ){
          s<-paste("EL ALUMNO ESTA ACTIVO REGULARIZADO", sep="")
          Observaciones16<-c(Observaciones16,s)
        }else{
          s<-""
          Observaciones16<-c(Observaciones16,s)
        }
        
        
      }
      Observaciones16<- Observaciones16[-1]
    }
  }
  
  #Juntamos todas las columnas 
  { 
    matriz_observaciones<-cbind(Observaciones1,Observaciones2,Observaciones3,Observaciones4,Observaciones5,Observaciones6,Observaciones7,Observaciones8,Observaciones9,Observaciones10,Observaciones11, Observaciones12, Observaciones13, Observaciones14, Observaciones15, Observaciones16)
    Observaciones<- 0 
    for ( i in 1:length(Observaciones1)){
      r<-!(matriz_observaciones[i,]=="")
     if(sum(r)>1){
       a<-paste0(matriz_observaciones[i,],collapse=" Y ")
       Observaciones<-c(Observaciones, a)
     }
      else{
        a<-paste0(matriz_observaciones[i,],collapse="")
        Observaciones<-c(Observaciones, a)
      } 
    }
    Observaciones<-Observaciones[-1]

  }
  
  Tray_edades_y_materias$Observaciones<-Observaciones
  
  Tray_1<-Tray_edades_y_materias[,c("Folio", "Edades","Estatus", "Nombre", "Apellidos", "CURP", "Calificaciones_reprobatorias",  "Observaciones", modulos)]  



  
  
}#Braket final de 3


#4.Revisar si el modulo correspondiente tiene calificacion (aqui si hay que cambiar el nombre dependiendo de la tabla) ----
{#Braket inicial de 4
#Generamos una columna con observaciones sobre si el alumno tiene calificacion en el modulo del nombre del archivo  
{
  nombre_modulo<-substring(nombre, first = 28, last = 30)
  
  Observaciones17<- 0 
  for(i in 1:dim(Tray_1)[1]){
    if(is.na(Tray_1[i, nombre_modulo])){#El modulo que aparece en el nombre
      s<-""
      Observaciones17<-c(Observaciones17,s)
    }else{
      a<-substring(nombre_modulo, first = 2, last = 3)
      s<-paste("OBSERVACION: EL ALUMNO YA CURSO EL MODULO ",a , sep="")
      Observaciones17<-c(Observaciones17,s)
    }
}
  Observaciones17<-Observaciones17[-1]
}
  
#Combinamos esta columna con las observaciones que existen desde el punto 3. 
{
  for(i in 1:length(Tray_1$Observaciones)){
    if(sum(c(Observaciones[i], Observaciones17[i])=="")<2){
    a<-paste0(Observaciones17[i], Observaciones[i], collapse="" )
    Tray_1$Observaciones[i]<-a
    }
    else{
      a<-paste0(Observaciones17[i], Observaciones[i], collapse=" Y " )
      Tray_1$Observaciones[i]<-a
    }
}  

  
}

#View(Tray_1)




}#Braket final de 4


#5. Creamos la tabla de los NP----

{#Bracket inicial de 5

  #Generamos la columna de los folios
{
Folios_repetidos<-rep(Tray$Folio, each = 23)
Modulos<-modulos
}

 #Fechas de fin de curso
{
  fechas_finales<-0
  for(i in 1:length(modulos)){
       a<-paste("FF",modulos[i],sep="")
       fechas_finales<-c(fechas_finales,a)
  }
  fechas_finales<-fechas_finales[-1]
}

#Columna de "fecha" en np
{
  fecha<-0
  for (i in 1:length(Tray$Folio)){
    for (j in fechas_finales){
     fecha<-c(fecha,Tray[i,j]) 
    }
  }
  fecha<-fecha[-1]
  fecha<-unlist(fecha, use.names = FALSE)
  

}


{
  Calificación<-0
  for(i in 1:length(Tray$Folio)){
    for(j in modulos){
      Calificación<-c(Calificación, Tray[i,j])
    }
  }
  Calificación<-Calificación[-1]
  
  Calificación<-unlist(Calificación, use.names = FALSE)
  

}

#Generamos la columna NP
{
ca<-as.numeric(Calificación)
NP<-ceiling(1-(ca/60))
}
  
  
  #Generamos el data.frame y el csv
  {
    np<-data.frame(Folios, Módulos=Modulos, Fecha=fecha, Calificación, NP = NP)
    
    to_remove <- c("Calificaciones_reprobatorias")
    Tray_1<-Tray_1[ , !(names(Tray_1) %in% to_remove)]
  }
  
  
#Generamos columna Mod Rep
{
    Mod_Rep <- (NP==1)
    Mod_Rep[which(is.na(Mod_Rep))]<-FALSE
    Mod_Rep[which(Mod_Rep==FALSE)]<- "FALSO"
    Mod_Rep[which(Mod_Rep==TRUE)]<-"VERDADERO"
    np<-cbind(np, Mod_Rep)
}

#Limpiamos los NA de Calificacion y NP 
#{
 # Calificación[which(is.na(Calificación))]<-""
  #NP[which(is.na(NP))]<-""
#}


  
    

}#Bracket final de 5








#Mandamos a imprimir----
{
    

  
  library(readr)
  #https://stackoverflow.com/questions/39202447/write-csv-with-encoding-utf8
  
  direccion_1<-"C:\\Users\\Luis Lopez\\Documents\\Proyectos\\Trayectoria_Candidatos__"
  direccion_2<-substring(nombre, first = 25)
  direccion_3<-"_revisado.csv"
  direccion<-paste0(direccion_1, direccion_2, direccion_3, sep="")
  
  direccion_4<-"C:\\Users\\Luis Lopez\\Documents\\Proyectos\\NP"
  direccion_5<-".csv"
  direccion2<- paste0(direccion_4, direccion_2, direccion_5, sep = "")
  
  write_excel_csv(Tray_1, direccion, na="")
  
  write.csv.utf8.BOM(np,direccion2)
  # write.xlsx.utf8.BOM(np,"C:\\Users\\Luis Lopez\\Documents\\Proyectos\\NP.xlsx" )
 



#Visualizar las tablas que resultan

  #View(Tray)
  #View(Tray_mr)
  #View(Tray_edades_y_materias)
  View(Tray_1)
  View(np)
}




#6.Generamos documento de proceso de reinscripcion (Regularizados)
{
  Proc_R_R <- read_excel("Proyectos/Proceso_Reinscripcion_G28M21-R.xlsx")
  nombre_proc_r_r_1<-"Proyectos/Proceso_Reinscripcion"
  nombre_proc_r_r_2<-"_G28M21-R.csv"
  nombre_proc_r_r_path<-paste0(nombre_proc_r_r_1, "2", nombre_proc_r_r_2, collapse = "")
  nombre_modulo_r<-substring(nombre_proc_r_r_2, first=5, last=7)
  Proc_R_R$Observaciones<- Observaciones_funcion(Proc_R_R)
  
  for(i in 1:length(Proc_R_R$Observaciones)){
    if(Proc_R_R[i,"Calificación"] < 60){
      if(Proc_R_R[i,"Calificación"] < 41){
        t<-paste0(Proc_R_R$Observaciones[i], "EL ALUMNO TIENE CALIFICACION MENOR A 41 EN EL MODULO ", substring(nombre_modulo_r, first=2), collapse = "")
        s<-paste0(Proc_R_R$Observaciones[i], t, sep = " Y ")
        Proc_R_R$Observaciones[i]<-s
      }
      else{
        t<-paste0("EL ALUMNO TIENE CALIFICACION ENTRE 41 Y 59 EN EL MODULO ", substring(nombre_modulo_r, first=2), sep = " ")
        s<-paste(Proc_R_R$Observaciones[i], t, sep = " Y ")
        Proc_R_R$Observaciones[i]<-s
      }
    }
  }
  
  for(i in 1:dim(Proc_R_R)[1]){
    Nombres<-paste(Proc_R_R$Nombre,Proc_R_R$Apellidos, sep = " " )
  }
  
  
  Proc_R_R$Nombres<-Nombres
  
  #Proc_R_R<-Proc_R_R[,which(names(Proc_R_R) %in% c( "Folio", "Nombres","Correo", "Grupo Anterior", "Calificación", "Estatus", "Observaciones"))]
  
  
  write_excel_csv(Proc_R_R, nombre_proc_r_r_path)
  }



}#Braket final del codigo completo


#Funciones particulares----



#Funcion para modularizar las observaciones a los archivos de proceso

Observaciones_funcion<-function(Tray_edades_y_materias){
  {
    Observaciones2<- 0 
    for(i in 1:dim(Tray_edades_y_materias)[1]){
      if(Tray_edades_y_materias[i, "Estatus"] == "Baja parcial" || Tray_edades_y_materias[i, "Estatus"] == "Baja Parcial" ){
        s<-paste("EL ALUMNO TIENE BAJA PARCIAL", sep="")
        Observaciones2<-c(Observaciones2,s)
      }else{
        s<-""
        Observaciones2<-c(Observaciones2,s)
      }
      
      
    }
    Observaciones2<- Observaciones2[-1]
  }
  
  #observaciones3 Baja temporal
  {
    Observaciones3<- 0 
    for(i in 1:dim(Tray_edades_y_materias)[1]){
      if(Tray_edades_y_materias[i, "Estatus"] == "Baja temporal" || Tray_edades_y_materias[i, "Estatus"] == "Baja Temporal" ){
        s<-paste("EL ALUMNO TIENE BAJA TEMPORAL", sep="")
        Observaciones3<-c(Observaciones3,s)
      }else{
        s<-""
        Observaciones3<-c(Observaciones3,s)
      }
      
      
    }
    Observaciones3<- Observaciones3[-1]
  }
  
  #observaciones4 Baja definitiva por sancion
  {
    Observaciones4<- 0 
    for(i in 1:dim(Tray_edades_y_materias)[1]){
      if(Tray_edades_y_materias[i, "Estatus"] == "Baja Definitiva por Sanción" || Tray_edades_y_materias[i, "Estatus"] == "Baja definitiva por sanción" ){
        s<-paste("EL ALUMNO TIENE BAJA DEFINITIVA POR SANCION", sep="")
        Observaciones4<-c(Observaciones4,s)
      }else{
        s<-""
        Observaciones4<-c(Observaciones4,s)
      }
      
      
    }
    Observaciones4<- Observaciones4[-1]
  }
  
  #observaciones5 Baja Definitiva por Oportunidades
  {
    Observaciones5<- 0 
    for(i in 1:dim(Tray_edades_y_materias)[1]){
      if(Tray_edades_y_materias[i, "Estatus"] == "Baja Definitiva por Oportunidades" || Tray_edades_y_materias[i, "Estatus"] == "Baja definitiva por oportunidades" ){
        s<-paste("EL ALUMNO TIENE BAJA DEFINITIVA POR OPORTUNIDADES", sep="")
        Observaciones5<-c(Observaciones5,s)
      }else{
        s<-""
        Observaciones5<-c(Observaciones5,s)
      }
      
      
    }
    Observaciones5<- Observaciones5[-1]
  }
  
  #OBSERVACIONES6 Baja Definitiva Solicitada
  {
    Observaciones6<- 0 
    for(i in 1:dim(Tray_edades_y_materias)[1]){
      if(Tray_edades_y_materias[i, "Estatus"] == "Baja Definitiva Solicitada" || Tray_edades_y_materias[i, "Estatus"] == "Baja definitiva solicitada" ){
        s<-paste("EL ALUMNO TIENE BAJA DEFINITIVA SOLICITADA", sep="")
        Observaciones6<-c(Observaciones6,s)
      }else{
        s<-""
        Observaciones6<-c(Observaciones6,s)
      }
      
      
    }
    Observaciones6<- Observaciones6[-1]
  }
  
  #OBSERVACIONES7 Baja Definitiva por Inactividad
  {
    Observaciones7<- 0 
    for(i in 1:dim(Tray_edades_y_materias)[1]){
      if(Tray_edades_y_materias[i, "Estatus"] == "Baja Definitiva por Inactividad" || Tray_edades_y_materias[i, "Estatus"] == "Baja definitiva por inactividad" ){
        s<-paste("EL ALUMNO TIENE BAJA DEFINITIVA POR INACTIVIDAD", sep="")
        Observaciones7<-c(Observaciones7,s)
      }else{
        s<-""
        Observaciones7<-c(Observaciones7,s)
      }
      
      
    }
    Observaciones7<- Observaciones7[-1]
  }
  
  #OBERSVACIONES8 Baja Parcial Solicitada
  {
    Observaciones8<- 0 
    for(i in 1:dim(Tray_edades_y_materias)[1]){
      if(Tray_edades_y_materias[i, "Estatus"] == "Baja Parcial Solicitada" || Tray_edades_y_materias[i, "Estatus"] == "Baja parcial solicitada" ){
        s<-paste("EL ALUMNO TIENE BAJA PARCIAL SOLICITADA", sep="")
        Observaciones8<-c(Observaciones8,s)
      }else{
        s<-""
        Observaciones8<-c(Observaciones8,s)
      }
      
      
    }
    Observaciones8<- Observaciones8[-1]
  }
  
  #OBSERVACIONES9 Suspensión
  {
    Observaciones9<- 0 
    for(i in 1:dim(Tray_edades_y_materias)[1]){
      if(Tray_edades_y_materias[i, "Estatus"] == "Suspensión" || Tray_edades_y_materias[i, "Estatus"] == "suspensión" ){
        s<-paste("EL ALUMNO TIENE SUSPENSION", sep="")
        Observaciones9<-c(Observaciones9,s)
      }else{
        s<-""
        Observaciones9<-c(Observaciones9,s)
      }
      
      
    }
    Observaciones9<- Observaciones9[-1]
  }
  
  #OBSERVACIONES10 Baja Temporal Disciplinaria por plagio
  {
    Observaciones10<- 0 
    for(i in 1:dim(Tray_edades_y_materias)[1]){
      if(Tray_edades_y_materias[i, "Estatus"] == "Baja Temporal Disciplinaria por plagio" || Tray_edades_y_materias[i, "Estatus"] == "Baja Temporal Disciplinaria por Plagio" ){
        s<-paste("EL ALUMNO TIENE BAJA TEMPORAL DISCIPLINARIA POR PLAGIO", sep="")
        Observaciones10<-c(Observaciones10,s)
      }else{
        s<-""
        Observaciones10<-c(Observaciones10,s)
      }
      
      
    }
    Observaciones10<- Observaciones10[-1]
  }
  
  #observaciones11 Activo
  {
    Observaciones11<- 0 
    for(i in 1:dim(Tray_edades_y_materias)[1]){
      if(Tray_edades_y_materias[i, "Estatus"] == "Activo" || Tray_edades_y_materias[i, "Estatus"] == "Activo" ){
        s<-paste("EL ALUMNO ESTA ACTIVO", sep="")
        Observaciones11<-c(Observaciones11,s)
      }else{
        s<-""
        Observaciones11<-c(Observaciones11,s)
      }
      
      
    }
    Observaciones11<- Observaciones11[-1]
  }
  
  #OBSERVACIONES12 Egresado
  {
    Observaciones12<- 0 
    for(i in 1:dim(Tray_edades_y_materias)[1]){
      if(Tray_edades_y_materias[i, "Estatus"] == "Egresado" || Tray_edades_y_materias[i, "Estatus"] == "egresado" ){
        s<-paste("EL ALUMNO ES EGRESADO", sep="")
        Observaciones12<-c(Observaciones12,s)
      }else{
        s<-""
        Observaciones12<-c(Observaciones12,s)
      }
      
      
    }
    Observaciones12<- Observaciones12[-1]
  }
  
  #OBSERVACIONES13 Promovido
  {
    Observaciones13<- 0 
    for(i in 1:dim(Tray_edades_y_materias)[1]){
      if(Tray_edades_y_materias[i, "Estatus"] == "Promovido" || Tray_edades_y_materias[i, "Estatus"] == "promovido" ){
        s<-paste("EL ALUMNO ES PROMOVIDO", sep="")
        Observaciones13<-c(Observaciones13,s)
      }else{
        s<-""
        Observaciones13<-c(Observaciones13,s)
      }
      
      
    }
    Observaciones13<- Observaciones13[-1]
  }
  
  #OBSERVACIONES14 No Promovido
  {
    Observaciones14<- 0 
    for(i in 1:dim(Tray_edades_y_materias)[1]){
      if(Tray_edades_y_materias[i, "Estatus"] == "No Promovido" || Tray_edades_y_materias[i, "Estatus"] == "No promovido" ){
        s<-paste("EL ALUMNO ES NO PROMOVIDO", sep="")
        Observaciones14<-c(Observaciones14,s)
      }else{
        s<-""
        Observaciones14<-c(Observaciones14,s)
      }
      
      
    }
    Observaciones14<- Observaciones14[-1]
  }
  
  #OBSERVACIONES15 Activo con Equivalencia
  {
    Observaciones15<- 0 
    for(i in 1:dim(Tray_edades_y_materias)[1]){
      if(Tray_edades_y_materias[i, "Estatus"] == "Activo con Equivalencia" || Tray_edades_y_materias[i, "Estatus"] == "Activo con equivalencia" ){
        s<-paste("EL ALUMNO ESTA ACTIVO CON EQUIVALENCIA", sep="")
        Observaciones15<-c(Observaciones15,s)
      }else{
        s<-""
        Observaciones15<-c(Observaciones15,s)
      }
      
      
    }
    Observaciones15<- Observaciones15[-1]
  }
  
  #OBSERVACIONES16 Activo Regularizado
  {
    Observaciones16<- 0 
    for(i in 1:dim(Tray_edades_y_materias)[1]){
      if(Tray_edades_y_materias[i, "Estatus"] == "Activo Regularizado" || Tray_edades_y_materias[i, "Estatus"] == "Activo regularizado" ){
        s<-paste("EL ALUMNO ESTA ACTIVO REGULARIZADO", sep="")
        Observaciones16<-c(Observaciones16,s)
      }else{
        s<-""
        Observaciones16<-c(Observaciones16,s)
      }
      
      
    }
    Observaciones16<- Observaciones16[-1]
  }
}




#Definimos una funcion que, dada la fecha de nacimiento, calcule la edad numerica exacta al 1 de juno de 2023
{
  edades_decimal<-function(x,y, A=2023){
    Y<-as.numeric(format(x, "%Y" ) )
    A<-as.numeric(format(y, "%Y" ) )
    m<-as.numeric(format(x, "%m" ) )
    M<-as.numeric(format(y, "%m" ) )
    d<-as.numeric(format(x, "%d" ) )
    D<-as.numeric(format(y, "%d" ) )
    (A-Y)+((M-m)/12)+((D-d)/365.25)
  }
}


#Funcion para escribir csv con acentos. 
#Workaround tomado de https://stackoverflow.com/questions/7402307/export-utf-8-bom-to-csv-in-r#   
{
  write.csv.utf8.BOM <- function(df, filename)
  {
    con <- file(filename, "w")
    tryCatch({
      for (i in 1:ncol(df))
        df[,i] = iconv(df[,i], to = "UTF-8") 
      writeChar(iconv("\ufeff", to = "UTF-8"), con, eos = NULL)
      write.csv(df, file = con, row.names = FALSE, na="")
    },finally = {close(con)})
  }
} 

#Intento (fallido) de la version de la funcion anterior con xlsx
{
  write.xlsx.utf8.BOM <- function(df, filename)
  {
    con <- file(filename, "w")
    tryCatch({
      for (i in 1:ncol(df))
        df[,i] = iconv(df[,i], to = "UTF-8") 
      writeChar(iconv("\ufeff", to = "UTF-8"), con, eos = NULL)
      write.xlsx(df, file = con, rowNames = FALSE)
    },finally = {close(con)})
  }
} 


{
  Observaciones____1<-""
  for(i in 1:dim(x)[1]){#Primero revisamos si la columna de estatus comienza con activo o baja
    if(substring(x[i, "Estatus"],last = 1)=="A"  ){
      s<-paste0("El alumno esta", x[i, "Estatus"], collapse = " ")
      Observaciones____1<-c(Observaciones____1,s)
    }
    else{
      s<-paste0("El alumno esta en", x[i, "Estatus"], collapse = " ")
      Observaciones<-c(Observaciones,s)
    }
    Observaciones____1<-Observaciones____1[-1]
    Observaciones____2<-""
    if(x[i,"Calificaciones_reprobatorias"]>2){
      s<-paste0("El alumno tiene", x[i,"Calificaciones_reprobatorias"], "calificaciones reprobatorias", collapse = " ")
      Observaciones____2<-c(Observaciones____2, s)
    }
    else{
      s<-""
      Observaciones____2<-c(Observaciones__2, s)
    }
    Observaciones____2<-Observaciones____2[-1]
    
    Observaciones____3<-""
    if(x[i,nombre_modulo]>=0){
      s<-paste0("Observación: El alumno tiene calificación en el módulo", substring(nombre_modulo,first=2),collapse = " ")
      Observaciones____3<-c(Observaciones____3,s)
    }
    else{
      s<-""
      Observaciones____3<-c(Observaciones____3, s)
    }
    Observaciones____3<-Observaciones____3[-1]
    Observaciones____4<-""
    if(x[i, "Calificación"]<60){
      if(x[i, "Calificación"]>40){
      s<-paste0("El alumno tiene calificación entre 41 y 59 en el módulo", substring(nombre_modulo,first=2),collapse = " ")
      Observaciones____4<-c(Observaciones____4, s)
      }
    else{
        s<-paste0("El alumno tiene calificación menor a 41 en el módulo", substring(nombre_modulo,first=2),collapse = " ")
        Observaciones____4<-c(Observaciones____4,s)
    }
    } 
    else{
      s<-""
      Observaciones____4<-c(Observaciones____4, c=s)
    }
    
    Observaciones<-""
    Observaciones____4<-Observaciones____4[-1]
    if(sum(c(Observaciones____1[i],Observaciones____2[i],Observaciones____3[i],Observaciones____4[i])=="")>1){
      s<-paste0(Observaciones____1[i],Observaciones____2[i],Observaciones____3[i],Observaciones____4[i],collapse = " y ")
      Observaciones<-c(Observaciones,s)
    }
    else
      {
        s<-paste0(Observaciones____1[i],Observaciones____2[i],Observaciones____3[i],Observaciones____4[i],collapse = " ")
        Observaciones<-c(Observaciones,s)
    }
  }
}



observaciones_funci<-function(x){
  {
    Observaciones2<- 0 
    for(i in 1:dim(x)[1]){
      if(x[i, "Estatus"] == "Baja parcial" || x[i, "Estatus"] == "Baja Parcial" ){
        s<-paste("EL ALUMNO TIENE BAJA PARCIAL", sep="")
        Observaciones2<-c(Observaciones2,s)
      }else{
        s<-""
        Observaciones2<-c(Observaciones2,s)
      }
      
      
    }
    Observaciones2<- Observaciones2[-1]
  }
  
  #observaciones3 Baja temporal
  {
    Observaciones3<- 0 
    for(i in 1:dim(Tray_edades_y_materias)[1]){
      if(Tray_edades_y_materias[i, "Estatus"] == "Baja temporal" || Tray_edades_y_materias[i, "Estatus"] == "Baja Temporal" ){
        s<-paste("EL ALUMNO TIENE BAJA TEMPORAL", sep="")
        Observaciones3<-c(Observaciones3,s)
      }else{
        s<-""
        Observaciones3<-c(Observaciones3,s)
      }
      
      
    }
    Observaciones3<- Observaciones3[-1]
  }
  
  #observaciones4 Baja definitiva por sancion
  {
    Observaciones4<- 0 
    for(i in 1:dim(Tray_edades_y_materias)[1]){
      if(Tray_edades_y_materias[i, "Estatus"] == "Baja Definitiva por Sanción" || Tray_edades_y_materias[i, "Estatus"] == "Baja definitiva por sanción" ){
        s<-paste("EL ALUMNO TIENE BAJA DEFINITIVA POR SANCION", sep="")
        Observaciones4<-c(Observaciones4,s)
      }else{
        s<-""
        Observaciones4<-c(Observaciones4,s)
      }
      
      
    }
    Observaciones4<- Observaciones4[-1]
  }
  
  #observaciones5 Baja Definitiva por Oportunidades
  {
    Observaciones5<- 0 
    for(i in 1:dim(Tray_edades_y_materias)[1]){
      if(Tray_edades_y_materias[i, "Estatus"] == "Baja Definitiva por Oportunidades" || Tray_edades_y_materias[i, "Estatus"] == "Baja definitiva por oportunidades" ){
        s<-paste("EL ALUMNO TIENE BAJA DEFINITIVA POR OPORTUNIDADES", sep="")
        Observaciones5<-c(Observaciones5,s)
      }else{
        s<-""
        Observaciones5<-c(Observaciones5,s)
      }
      
      
    }
    Observaciones5<- Observaciones5[-1]
  }
  
  #OBSERVACIONES6 Baja Definitiva Solicitada
  {
    Observaciones6<- 0 
    for(i in 1:dim(Tray_edades_y_materias)[1]){
      if(Tray_edades_y_materias[i, "Estatus"] == "Baja Definitiva Solicitada" || Tray_edades_y_materias[i, "Estatus"] == "Baja definitiva solicitada" ){
        s<-paste("EL ALUMNO TIENE BAJA DEFINITIVA SOLICITADA", sep="")
        Observaciones6<-c(Observaciones6,s)
      }else{
        s<-""
        Observaciones6<-c(Observaciones6,s)
      }
      
      
    }
    Observaciones6<- Observaciones6[-1]
  }
  
  #OBSERVACIONES7 Baja Definitiva por Inactividad
  {
    Observaciones7<- 0 
    for(i in 1:dim(Tray_edades_y_materias)[1]){
      if(Tray_edades_y_materias[i, "Estatus"] == "Baja Definitiva por Inactividad" || Tray_edades_y_materias[i, "Estatus"] == "Baja definitiva por inactividad" ){
        s<-paste("EL ALUMNO TIENE BAJA DEFINITIVA POR INACTIVIDAD", sep="")
        Observaciones7<-c(Observaciones7,s)
      }else{
        s<-""
        Observaciones7<-c(Observaciones7,s)
      }
      
      
    }
    Observaciones7<- Observaciones7[-1]
  }
  
  #OBERSVACIONES8 Baja Parcial Solicitada
  {
    Observaciones8<- 0 
    for(i in 1:dim(Tray_edades_y_materias)[1]){
      if(Tray_edades_y_materias[i, "Estatus"] == "Baja Parcial Solicitada" || Tray_edades_y_materias[i, "Estatus"] == "Baja parcial solicitada" ){
        s<-paste("EL ALUMNO TIENE BAJA PARCIAL SOLICITADA", sep="")
        Observaciones8<-c(Observaciones8,s)
      }else{
        s<-""
        Observaciones8<-c(Observaciones8,s)
      }
      
      
    }
    Observaciones8<- Observaciones8[-1]
  }
  
  #OBSERVACIONES9 Suspensión
  {
    Observaciones9<- 0 
    for(i in 1:dim(Tray_edades_y_materias)[1]){
      if(Tray_edades_y_materias[i, "Estatus"] == "Suspensión" || Tray_edades_y_materias[i, "Estatus"] == "suspensión" ){
        s<-paste("EL ALUMNO TIENE SUSPENSION", sep="")
        Observaciones9<-c(Observaciones9,s)
      }else{
        s<-""
        Observaciones9<-c(Observaciones9,s)
      }
      
      
    }
    Observaciones9<- Observaciones9[-1]
  }
  
  #OBSERVACIONES10 Baja Temporal Disciplinaria por plagio
  {
    Observaciones10<- 0 
    for(i in 1:dim(Tray_edades_y_materias)[1]){
      if(Tray_edades_y_materias[i, "Estatus"] == "Baja Temporal Disciplinaria por plagio" || Tray_edades_y_materias[i, "Estatus"] == "Baja Temporal Disciplinaria por Plagio" ){
        s<-paste("EL ALUMNO TIENE BAJA TEMPORAL DISCIPLINARIA POR PLAGIO", sep="")
        Observaciones10<-c(Observaciones10,s)
      }else{
        s<-""
        Observaciones10<-c(Observaciones10,s)
      }
      
      
    }
    Observaciones10<- Observaciones10[-1]
  }
  
  #observaciones11 Activo
  {
    Observaciones11<- 0 
    for(i in 1:dim(Tray_edades_y_materias)[1]){
      if(Tray_edades_y_materias[i, "Estatus"] == "Activo" || Tray_edades_y_materias[i, "Estatus"] == "Activo" ){
        s<-paste("EL ALUMNO ESTA ACTIVO", sep="")
        Observaciones11<-c(Observaciones11,s)
      }else{
        s<-""
        Observaciones11<-c(Observaciones11,s)
      }
      
      
    }
    Observaciones11<- Observaciones11[-1]
  }
  
  #OBSERVACIONES12 Egresado
  {
    Observaciones12<- 0 
    for(i in 1:dim(Tray_edades_y_materias)[1]){
      if(Tray_edades_y_materias[i, "Estatus"] == "Egresado" || Tray_edades_y_materias[i, "Estatus"] == "egresado" ){
        s<-paste("EL ALUMNO ES EGRESADO", sep="")
        Observaciones12<-c(Observaciones12,s)
      }else{
        s<-""
        Observaciones12<-c(Observaciones12,s)
      }
      
      
    }
    Observaciones12<- Observaciones12[-1]
  }
  
  #OBSERVACIONES13 Promovido
  {
    Observaciones13<- 0 
    for(i in 1:dim(Tray_edades_y_materias)[1]){
      if(Tray_edades_y_materias[i, "Estatus"] == "Promovido" || Tray_edades_y_materias[i, "Estatus"] == "promovido" ){
        s<-paste("EL ALUMNO ES PROMOVIDO", sep="")
        Observaciones13<-c(Observaciones13,s)
      }else{
        s<-""
        Observaciones13<-c(Observaciones13,s)
      }
      
      
    }
    Observaciones13<- Observaciones13[-1]
  }
  
  #OBSERVACIONES14 No Promovido
  {
    Observaciones14<- 0 
    for(i in 1:dim(Tray_edades_y_materias)[1]){
      if(Tray_edades_y_materias[i, "Estatus"] == "No Promovido" || Tray_edades_y_materias[i, "Estatus"] == "No promovido" ){
        s<-paste("EL ALUMNO ES NO PROMOVIDO", sep="")
        Observaciones14<-c(Observaciones14,s)
      }else{
        s<-""
        Observaciones14<-c(Observaciones14,s)
      }
      
      
    }
    Observaciones14<- Observaciones14[-1]
  }
  
  #OBSERVACIONES15 Activo con Equivalencia
  {
    Observaciones15<- 0 
    for(i in 1:dim(Tray_edades_y_materias)[1]){
      if(Tray_edades_y_materias[i, "Estatus"] == "Activo con Equivalencia" || Tray_edades_y_materias[i, "Estatus"] == "Activo con equivalencia" ){
        s<-paste("EL ALUMNO ESTA ACTIVO CON EQUIVALENCIA", sep="")
        Observaciones15<-c(Observaciones15,s)
      }else{
        s<-""
        Observaciones15<-c(Observaciones15,s)
      }
      
      
    }
    Observaciones15<- Observaciones15[-1]
  }
  
  #OBSERVACIONES16 Activo Regularizado
  {
    Observaciones16<- 0 
    for(i in 1:dim(Tray_edades_y_materias)[1]){
      if(Tray_edades_y_materias[i, "Estatus"] == "Activo Regularizado" || Tray_edades_y_materias[i, "Estatus"] == "Activo regularizado" ){
        s<-paste("EL ALUMNO ESTA ACTIVO REGULARIZADO", sep="")
        Observaciones16<-c(Observaciones16,s)
      }else{
        s<-""
        Observaciones16<-c(Observaciones16,s)
      }
      
      
    }
    Observaciones16<- Observaciones16[-1]
  }
}
