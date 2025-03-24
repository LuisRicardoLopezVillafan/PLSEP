library(readxl)
library(tibble)
library(readr)


#Trayectora->Calificaciones



Trayectoria_Proceso<-function(x,y){
{
Lectura_nombre_trayectoria_1<-function(x,y){
  x<-"Trayectoria_G28M22"
  y<-"Proceso_Reinscripcion_G28M22"
  
  name<-paste0("Proyectos/", x, ".xlsx", collapse = "")
  Lectura_nombre_trayectoria(name)
}

#Funcion Lectura_nombre_trayectoria(s)
Lectura_nombre_trayectoria<-function(name){#Braket inicial lectura de nombre
  s<-name
  #Tray__nombre<-substring(s, first = 11, last = 53) #Esta linea si hay que hacerla manualmente
  Tray__nombre_generacion<-substring(Tray__nombre, first = 13, last = 15) #Para generar la direccion en la carpeta de proyectos
  
  if(substring(Tray__nombre, first = 30, last = 30)=="."){
  Tray__nombre_modulo<-substring(Tray__nombre, first = 28, last = 29)  #Sustraemos el modulo del nombre
  }else{
    Tray__nombre_modulo<-substring(Tray__nombre, first = 28, last = 30)  #Sustraemos el modulo del nombre
  }
  Tray_nombre_gen_mod<-paste0(Tray__nombre_generacion, Tray__nombre_modulo, sep = "")
  
  Tray__nombre_decide<-substring(Tray__nombre, first = 43) #Si Tray__nombre_decide es R, entonces es regularizado, si es x, entonces es ordinario
  foldername<-paste0(Tray__nombre_generacion, Tray__nombre_modulo, collapse = "")
  path_out<-paste0("Proyectos/", foldername, collapse =  "")
  dir.create(path_out)
  
  #s<-"Proceso_Reinscripcion_G28M22"
  procr_nombre_gen<-substring(y,23,25 )
  procr_nombre_mod<-substring(y,first = 26)
  procr_nombre_gen_mod<-paste0(procr_nombre_gen,procr_nombre_mod,sep = "")
  name_<-paste0("Proyectos/", y, ".xlsx", collapse = "")
  
  if(!substring(s, first = 9, last = 9)=="R"){
    print("CUIDADO: Elegiste una tabla de Proceso para candidatos, no para regularizados. La de candidatos no tiene columna de estatus")
  }
  
  
  if(Tray__nombre_decide == "R"){
    Hoja_reg(s)
    filename<-paste0(path_out, "R", "/Calificaciones.csv", collapse = "")
    filenameprocr<-paste0(path_out, "/Proceso_Reinscripcion2_", procr_nombre_gen_mod,"-R.csv",sep = "")
    filenamenp<-paste0(path_out,"/NP.csv", sep = "")
    write_excel_csv(df, filename, na="")#Imprimir el documento
    write_excel_csv(dfpr,filenameprocr , na="")
    write_excel_csv(np,filenamenp , na="")
  }
  else{
    Hoja_ord(s)
    filename<-paste0(path_out, "/Calificaciones.csv", collapse = "")
    filenameprocr<-paste0(path_out, "/Proceso_Reinscripcion2_", procr_nombre_gen_mod,".csv",sep = "")
    filenamenp<-paste0(path_out,"/NP.csv", sep = "")
    write_excel_csv(df, filename, na="")
    write_excel_csv(np,filenamenp , na="")#Imprimir el documento
  }
  
  
  #Imprimimos el documento
  #{
   # filename<-paste0(foldername, "/Calificaciones.csv", collapse = "")
    #write_excel_csv(df, filename, na="")
  #}
  
  #Imprimir el documento----
  
  
  
}


#Funcion Hoja_Calificacion_ord
Hoja_ord<-function(s){
  #s<-"Proyectos/Trayectoria_Candidatos__G29M20_05-06-2023.xlsx"  
  df <- read_excel(s)#Usamos el paquete que lee xlsx porque con csv marca problemas sobre la "linea final"la codificacion.
  
  
  #Generamos un vector con las columnas que nos interesa analizar
  
    
    #Columnas de los modulos-----
    modulos<-0
    
    for (i in 1:23){
      t <- paste("M", i, sep = "")
      modulos<- append(modulos, t)
    }
    
    modulos <- modulos[-1]
    
    
    #Columna de calificaciones reprobatorias-----
    #Contamos las calificaciones reprobatorias
      Calificaciones_reprobatorias<- 0
      
      for (i in 1:dim(df)[1] ){
        #calificacion_individual<-df[i,modulos][!is.na(df[i,modulos])]
        #s<- sum(calificacion_individual<60)
        s<-sum((df[i,modulos]<60), na.rm = TRUE)
        Calificaciones_reprobatorias<-c(Calificaciones_reprobatorias, s)
      }
      Calificaciones_reprobatorias<-Calificaciones_reprobatorias[-1]
      
      df<-add_column(df, Calificaciones_reprobatorias, .after = "Folio")
    
    
    #Columna de fechas de nacimiento-----
    
      {
        fechas_nacimiento<-substring(df$CURP, first = 5, last = 10)
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
    
      Edades<-0
      for(i in 1:length(fechas_nacimiento)){
        Edades<-c(Edades, edades_decimal(fechas_nacimiento[i],Sys.Date()))
      }
      Edades<- Edades[-1]
    
    
    
    
    #Columna de observaciones----
    
      Obs<-observaciones_ord(df)
    
    #Columnas documento----
    
      columnas <- c("Folio",
                    ####"Nombre", "Apellidos", 
                    "Obs",
                    #"Contar Rep",
                    "Calificaciones_reprobatorias",
                    #"Generación",
                    "CURP",
                    "Estatus",
                    #"Grupo Actual",
                    #"Fecha fin de última asignación",
                    #fechas_finales,
                    modulos  
                    #"Probabilidad de no pasar el módulo",
                    # "Prob Aprob",
                    #"Obs4"
      )
      
      #Restringimos la tabla a las columnas que nos interesan (Basados en la hoja "Calificaciones" del libro Proceso2)
      df<- df[,columnas]
    
    
      #Documento de proceso
      {
        
        dfpr<-read_excel(name_)
        
        if(Tray_nombre_gen_mod==procr_nombre_gen_mod){
          Obs<-observaciones_ord(df)
        }
        dfpr<-cbind(dfpr,Obs)
       
      }
    
    
    
      #NP----
      
      {#Bracket inicial de 5
        
        #Generamos la columna de los folios
        {
          Folios_repetidos<-rep(x$Folio, each = 23)
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
          for (i in 1:length(x$Folio)){
            for (j in fechas_finales){
              fecha<-c(fecha,x[i,j]) 
            }
          }
          fecha<-fecha[-1]
          fecha<-unlist(fecha, use.names = FALSE)
          
          
        }
        
        
        {
          Calificación<-0
          for(i in 1:length(x$Folio)){
            for(j in modulos){
              Calificación<-c(Calificación, x[i,j])
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
          
          #to_remove <- c("Calificaciones_reprobatorias")
          #x<-x[ , !(names(x) %in% to_remove)]
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
      
      
      
      
      
      
      
      
      
  
}


#Funcion Hoja_Calificacion_reg
Hoja_reg<-function(s){
  df<-read_excel(s)
  #Columnas de los modulos-----
  {modulos<-0
  
  for (i in 1:23){
    t <- paste("M", i, sep = "")
    modulos<- append(modulos, t)
  }
  
  modulos <- modulos[-1]}
  
  
  #Columna de calificaciones reprobatorias-----
  {#Contamos las calificaciones reprobatorias
    Calificaciones_reprobatorias<- 0
    
    for (i in 1:dim(df)[1] ){
      #calificacion_individual<-df[i,modulos][!is.na(df[i,modulos])]
      #s<- sum(calificacion_individual<60)
      s<-sum((df[i,modulos]<60), na.rm = TRUE)
      Calificaciones_reprobatorias<-c(Calificaciones_reprobatorias, s)
    }
    Calificaciones_reprobatorias<-Calificaciones_reprobatorias[-1]
    
    df<-add_column(df, Calificaciones_reprobatorias, .after = "Folio")
  }
  
  #Columna de fechas de nacimiento-----
  {
    {
      fechas_nacimiento<-substring(df$CURP, first = 5, last = 10)
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
    
  }
  
  
  
  
  #Columna de observaciones----
  {
    Obs<-observaciones_reg(df)
  }
  #Columnas documento----
  {
    columnas <- c("Folio",
                  ####"Nombre", "Apellidos", 
                  #"Obs",
                  #"Contar Rep",
                  "Calificaciones_reprobatorias",
                  #"Generación",
                  "CURP",
                  "Estatus",
                  #"Grupo Actual",
                  #"Fecha fin de última asignación",
                  #fechas_finales,
                  modulos  
                  #"Probabilidad de no pasar el módulo",
                  # "Prob Aprob",
                  #"Obs4"
    )
    
    #Restringimos la tabla a las columnas que nos interesan (Basados en la hoja "Calificaciones" del libro Proceso2)
    df<- df[,columnas]
  }

  {
   
    dfpr<-read_excel(name_)
    
    if(Tray_nombre_gen_mod==procr_nombre_gen_mod){
      Obs<-observaciones_ord(df)
    }
    dfpr<-cbind(dfpr,Obs)
    
  }
  
 
  #NP----
  
  {#Bracket inicial de 5
    
    #Generamos la columna de los folios
    {
      Folios_repetidos<-rep(x$Folio, each = 23)
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
      for (i in 1:length(x$Folio)){
        for (j in fechas_finales){
          fecha<-c(fecha,x[i,j]) 
        }
      }
      fecha<-fecha[-1]
      fecha<-unlist(fecha, use.names = FALSE)
      
      
    }
    
    
    {
      Calificación<-0
      for(i in 1:length(x$Folio)){
        for(j in modulos){
          Calificación<-c(Calificación, x[i,j])
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
      
      #to_remove <- c("Calificaciones_reprobatorias")
      #x<-x[ , !(names(x) %in% to_remove)]
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
  }





}




  



{Obs_est_ord<-""
  for(i in 1:dim(dfpr)[1]){
    Obs_est_ord<-c(substring(dfpr$Estatus[i], first = 1, last = 1)
                   if(substr(t, start=1, stop = 1)=="B"){
                     a<-paste("El alumno está en ", t, sep = "") #Posible cambiar a mayusculas
                     Obs_est_ord<-c(Obs_est_ord, a)
                   }
                   else{
                     a<-""
                     Obs_est_ord<-c(Obs_est_ord,a)
                   }
  }
  Obs_est_ord<-Obs_est_ord[-1]}
}

observaciones_ord<-function(x){
  Obs<-""
  for(i in 1:dim(x)[1]){
    t<-c(Obs_est_ord(x)[i], observaciones_mod_ord(x)[i])
    s<-!(is.na(t))
    if(sum(s)==2){
      a<-paste0(t[1], t[2], collapse = " y ")
      Obs<-c(Obs,a)
    }
    else{
      t[which(is.na(t))]<-""
      a<-paste(t[1], t[2], sep = "")
      Obs<-c(Obs,a)
    }
  }
  Obs<-Obs[-1]
}

Obs_est_ord<-function(x){
  Obs1<-""
  for(i in 1:dim(x)[1]){
    Obs1<- c(Obs1,substring(x$Estatus[i], 1 , 1 ))
  }
  Obs1<-Obs1[-1]
  c<-(Obs1 == "B")
  for(i in 1:length(c)){
    if(isTRUE(c[i])){
      c[i]<-paste("El alumno tiene ", x$Estatus[i], sep = "" )
    }else{
      c[i]<-""
    }
  }
  c
}


Obs_mod_ord<-function(x){
  a<-paste0(procr_nombre_gen,procr_nombre_mod,collapse = "")
  b<-paste0("Trayectoria_Candidatos__", a,)
}
