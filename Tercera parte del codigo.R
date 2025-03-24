#Tarea para el viernes: Analizar la tabla, columna del estatus, si estan activos no hacer nada, si estan en baja parcial, ponerles observaciones, si tienen tres modulos reprobados, tambien poner las observaciones, y las observaciones se ponen en una sola columna 

#Primera tarea: Generamos todas las columnas de observaciones 
{
{
#Generamos la columna de observaciones1 (Cantidad de materias reprobadas)
{
  Observaciones1<- 0 
  for(i in 1:dim(Trayectoria_REM57_edades_materias)[1]){
    if(Trayectoria_REM57_edades_materias[i, "Calificaciones_reprobadas"]>=3){
    s<-paste("EL ALUMNO TIENE 3 MATERIAS REPROBADAS", sep="")
    Observaciones1<-c(Observaciones1,s)
    }else{
      s<-" "
      Observaciones1<-c(Observaciones1,s)
  }
 

  }
  Observaciones1<- Observaciones1[-1]
}

#Generamos la columna de observaciones2 (Baja parcial)
{
  Observaciones2<- 0 
  for(i in 1:dim(Trayectoria_REM57_edades_materias)[1]){
    if(Trayectoria_REM57_edades_materias[i, "Estatus"] == "Baja parcial" || Trayectoria_REM57_edades_materias[i, "Estatus"] == "Baja Parcial" ){
      s<-paste("EL ALUMNO TIENE BAJA PARCIAL", sep="")
      Observaciones2<-c(Observaciones2,s)
    }else{
      s<-" "
      Observaciones2<-c(Observaciones2,s)
    }
  
  
  }
  Observaciones2<- Observaciones2[-1]
}

#observaciones3 Baja temporal
{
  Observaciones3<- 0 
  for(i in 1:dim(Trayectoria_REM57_edades_materias)[1]){
    if(Trayectoria_REM57_edades_materias[i, "Estatus"] == "Baja temporal" || Trayectoria_REM57_edades_materias[i, "Estatus"] == "Baja Temporal" ){
      s<-paste("EL ALUMNO TIENE BAJA TEMPORAL", sep="")
      Observaciones3<-c(Observaciones3,s)
    }else{
      s<-" "
      Observaciones3<-c(Observaciones3,s)
    }
    
    
  }
  Observaciones3<- Observaciones3[-1]
}

#observaciones4 Baja definitiva por sancion
{
  Observaciones4<- 0 
  for(i in 1:dim(Trayectoria_REM57_edades_materias)[1]){
    if(Trayectoria_REM57_edades_materias[i, "Estatus"] == "Baja Definitiva por Sanción" || Trayectoria_REM57_edades_materias[i, "Estatus"] == "Baja definitiva por sanción" ){
      s<-paste("EL ALUMNO TIENE BAJA DEFINITIVA POR SANCION", sep="")
      Observaciones4<-c(Observaciones4,s)
    }else{
      s<-" "
      Observaciones4<-c(Observaciones4,s)
    }
    
    
  }
  Observaciones4<- Observaciones4[-1]
}

#observaciones5 Baja Definitiva por Oportunidades
{
  Observaciones5<- 0 
  for(i in 1:dim(Trayectoria_REM57_edades_materias)[1]){
    if(Trayectoria_REM57_edades_materias[i, "Estatus"] == "Baja Definitiva por Oportunidades" || Trayectoria_REM57_edades_materias[i, "Estatus"] == "Baja definitiva por oportunidades" ){
      s<-paste("EL ALUMNO TIENE BAJA DEFINITIVA POR OPORTUNIDADES", sep="")
      Observaciones5<-c(Observaciones5,s)
    }else{
      s<-" "
      Observaciones5<-c(Observaciones5,s)
    }
    
    
  }
  Observaciones5<- Observaciones5[-1]
}

#OBSERVACIONES6 Baja Definitiva Solicitada
{
  Observaciones6<- 0 
  for(i in 1:dim(Trayectoria_REM57_edades_materias)[1]){
    if(Trayectoria_REM57_edades_materias[i, "Estatus"] == "Baja Definitiva Solicitada" || Trayectoria_REM57_edades_materias[i, "Estatus"] == "Baja definitiva solicitada" ){
      s<-paste("EL ALUMNO TIENE BAJA DEFINITIVA SOLICITADA", sep="")
      Observaciones6<-c(Observaciones6,s)
    }else{
      s<-" "
      Observaciones6<-c(Observaciones6,s)
    }
    
    
  }
  Observaciones6<- Observaciones6[-1]
}

#OBSERVACIONES7 Baja Definitiva por Inactividad
{
  Observaciones7<- 0 
  for(i in 1:dim(Trayectoria_REM57_edades_materias)[1]){
    if(Trayectoria_REM57_edades_materias[i, "Estatus"] == "Baja Definitiva por Inactividad" || Trayectoria_REM57_edades_materias[i, "Estatus"] == "Baja definitiva por inactividad" ){
      s<-paste("EL ALUMNO TIENE BAJA DEFINITIVA POR INACTIVIDAD", sep="")
      Observaciones7<-c(Observaciones7,s)
    }else{
      s<-" "
      Observaciones7<-c(Observaciones7,s)
    }
    
    
  }
  Observaciones7<- Observaciones7[-1]
}

#OBERSVACIONES8 Baja Parcial Solicitada
{
  Observaciones8<- 0 
  for(i in 1:dim(Trayectoria_REM57_edades_materias)[1]){
    if(Trayectoria_REM57_edades_materias[i, "Estatus"] == "Baja Parcial Solicitada" || Trayectoria_REM57_edades_materias[i, "Estatus"] == "Baja parcial solicitada" ){
      s<-paste("EL ALUMNO TIENE BAJA PARCIAL SOLICITADA", sep="")
      Observaciones8<-c(Observaciones8,s)
    }else{
      s<-" "
      Observaciones8<-c(Observaciones8,s)
    }
    
    
  }
  Observaciones8<- Observaciones8[-1]
}

#OBSERVACIONES9 Suspensión
{
  Observaciones9<- 0 
  for(i in 1:dim(Trayectoria_REM57_edades_materias)[1]){
    if(Trayectoria_REM57_edades_materias[i, "Estatus"] == "Suspensión" || Trayectoria_REM57_edades_materias[i, "Estatus"] == "suspensión" ){
      s<-paste("EL ALUMNO TIENE SUSPENSION", sep="")
      Observaciones9<-c(Observaciones9,s)
    }else{
      s<-" "
      Observaciones9<-c(Observaciones9,s)
    }
    
    
  }
  Observaciones9<- Observaciones9[-1]
}

#OBSERVACIONES10 Baja Temporal Disciplinaria por plagio
{
  Observaciones10<- 0 
  for(i in 1:dim(Trayectoria_REM57_edades_materias)[1]){
    if(Trayectoria_REM57_edades_materias[i, "Estatus"] == "Baja Temporal Disciplinaria por plagio" || Trayectoria_REM57_edades_materias[i, "Estatus"] == "Baja Temporal Disciplinaria por Plagio" ){
      s<-paste("EL ALUMNO TIENE BAJA TEMPORAL DISCIPLINARIA POR PLAGIO", sep="")
      Observaciones10<-c(Observaciones10,s)
    }else{
      s<-" "
      Observaciones10<-c(Observaciones10,s)
    }
    
    
  }
  Observaciones10<- Observaciones10[-1]
}

#observaciones11 Activo
{
  Observaciones11<- 0 
  for(i in 1:dim(Trayectoria_REM57_edades_materias)[1]){
    if(Trayectoria_REM57_edades_materias[i, "Estatus"] == "Activo" || Trayectoria_REM57_edades_materias[i, "Estatus"] == "Activo" ){
      s<-paste("EL ALUMNO ESTA ACTIVO", sep="")
      Observaciones11<-c(Observaciones11,s)
    }else{
      s<-" "
      Observaciones11<-c(Observaciones11,s)
    }
    
    
  }
  Observaciones11<- Observaciones11[-1]
}

#OBSERVACIONES12 Egresado
{
  Observaciones12<- 0 
  for(i in 1:dim(Trayectoria_REM57_edades_materias)[1]){
    if(Trayectoria_REM57_edades_materias[i, "Estatus"] == "Egresado" || Trayectoria_REM57_edades_materias[i, "Estatus"] == "egresado" ){
      s<-paste("EL ALUMNO ES EGRESADO", sep="")
      Observaciones12<-c(Observaciones12,s)
    }else{
      s<-" "
      Observaciones12<-c(Observaciones12,s)
    }
    
    
  }
  Observaciones12<- Observaciones12[-1]
}

#OBSERVACIONES13 Promovido
{
  Observaciones13<- 0 
  for(i in 1:dim(Trayectoria_REM57_edades_materias)[1]){
    if(Trayectoria_REM57_edades_materias[i, "Estatus"] == "Promovido" || Trayectoria_REM57_edades_materias[i, "Estatus"] == "promovido" ){
      s<-paste("EL ALUMNO ES PROMOVIDO", sep="")
      Observaciones13<-c(Observaciones13,s)
    }else{
      s<-" "
      Observaciones13<-c(Observaciones13,s)
    }
    
    
  }
  Observaciones13<- Observaciones13[-1]
}

#OBSERVACIONES14 No Promovido
{
  Observaciones14<- 0 
  for(i in 1:dim(Trayectoria_REM57_edades_materias)[1]){
    if(Trayectoria_REM57_edades_materias[i, "Estatus"] == "No Promovido" || Trayectoria_REM57_edades_materias[i, "Estatus"] == "No promovido" ){
      s<-paste("EL ALUMNO ES NO PROMOVIDO", sep="")
      Observaciones14<-c(Observaciones14,s)
    }else{
      s<-" "
      Observaciones14<-c(Observaciones14,s)
    }
    
    
  }
  Observaciones14<- Observaciones14[-1]
}

#OBSERVACIONES15 Activo con Equivalencia
{
  Observaciones15<- 0 
  for(i in 1:dim(Trayectoria_REM57_edades_materias)[1]){
    if(Trayectoria_REM57_edades_materias[i, "Estatus"] == "Activo con Equivalencia" || Trayectoria_REM57_edades_materias[i, "Estatus"] == "Activo con equivalencia" ){
      s<-paste("EL ALUMNO ESTA ACTIVO CON EQUIVALENCIA", sep="")
      Observaciones15<-c(Observaciones15,s)
    }else{
      s<-" "
      Observaciones15<-c(Observaciones15,s)
    }
    
    
  }
  Observaciones15<- Observaciones15[-1]
}

#OBSERVACIONES16 Activo Regularizado
{
  Observaciones16<- 0 
  for(i in 1:dim(Trayectoria_REM57_edades_materias)[1]){
    if(Trayectoria_REM57_edades_materias[i, "Estatus"] == "Activo Regularizado" || Trayectoria_REM57_edades_materias[i, "Estatus"] == "Activo regularizado" ){
      s<-paste("EL ALUMNO ESTA ACTIVO REGULARIZADO", sep="")
      Observaciones16<-c(Observaciones16,s)
    }else{
      s<-" "
      Observaciones16<-c(Observaciones16,s)
    }
    
    
  }
  Observaciones16<- Observaciones16[-1]
}
}

#Juntamos todas las columnas 
{
  Observaciones<- 0 
  for ( i in 1:length(Observaciones2)){
    a<- paste(Observaciones1[i], Observaciones2[i], Observaciones3[i], Observaciones4[i], Observaciones5[i], Observaciones6[i], Observaciones7[i], Observaciones8[i], Observaciones9[i], Observaciones10[i], Observaciones11[i], Observaciones12[i], Observaciones13[i], Observaciones14[i], Observaciones15[i], Observaciones16[i] , sep = " " )
    Observaciones<-c(Observaciones, a)
  }
  Observaciones<-Observaciones[-1]
  
}
}

View(Tray_1)
Tray_edades_y_materias$Observaciones<-Observaciones

Tray_1<-Tray_edades_y_materias[,c("Folio", "Edades","Estatus", "Nombre", "Apellidos", "CURP", "Calificaciones_reprobatorias", "Observaciones", modulos)]

write.csv(Tray_1, "C:\\Users\\Luis Lopez\\Documents\\Proyectos\\Trayectoria_REM57_1.csv", row.names = FALSE)





#Borradores-----
{
  #Funcion que da la edad en formato literal (esta mal)
  
  {
    eliminacion_de_na<-function(x){
      x<-as.character(x)
      for(i in length(x)){
        if(x[i]=="NA"){
          x[i]<-""
        }
        else{
          x[i]<-as.numeric(x[i])
        }
      }
    }
  }
  
  
  #Intento n-simo de quitar los NA
  {
    {
      Tray[sapply(Tray, is.na)] <-lapply(Tray, function(x) if(is.na(x)) as.character(x) else x)
    }
    
    
    {
      nas<-which(is.na(Tray),arr.ind = TRUE)
      
      
      for(i in 1:dim(nas)[1]){
        c<-c(nas[i,][1],nas[i][2])
        c<-as.numeric(c)
        Tray[c[1],c[2]]<--1
      }
    }
    
    
    {
      library(dplyr)
      library(magrittr)
      Tray%<>% 
        mutate_each(funs(if(is.na(.)) as.character(.) else .))
    }
  }
  
  
  #Segundo intento para borrar los NA (EL BUENO)
  {
    #Reemplazamos "NA" por -1
    {
      Tray_mr[is.na(Tray_mr)]<- -1
    }
    
    {
      for(i in 1:23){
        a<-paste("Trayectoria_REM57_materias_reprobadas$M",i,"<-as.character(Trayectoria_REM57_materias_reprobadas$M",i,")", sep = "")
        print(a)
      }
    }#No hace falta imprimir este codigo
    
    {
      Trayectoria_REM57_materias_reprobadas$M1<-as.character(Trayectoria_REM57_materias_reprobadas$M1)
      Trayectoria_REM57_materias_reprobadas$M2<-as.character(Trayectoria_REM57_materias_reprobadas$M2)
      Trayectoria_REM57_materias_reprobadas$M3<-as.character(Trayectoria_REM57_materias_reprobadas$M3)
      Trayectoria_REM57_materias_reprobadas$M4<-as.character(Trayectoria_REM57_materias_reprobadas$M4)
      Trayectoria_REM57_materias_reprobadas$M5<-as.character(Trayectoria_REM57_materias_reprobadas$M5)
      Trayectoria_REM57_materias_reprobadas$M6<-as.character(Trayectoria_REM57_materias_reprobadas$M6)
      Trayectoria_REM57_materias_reprobadas$M7<-as.character(Trayectoria_REM57_materias_reprobadas$M7)
      Trayectoria_REM57_materias_reprobadas$M8<-as.character(Trayectoria_REM57_materias_reprobadas$M8)
      Trayectoria_REM57_materias_reprobadas$M9<-as.character(Trayectoria_REM57_materias_reprobadas$M9)
      Trayectoria_REM57_materias_reprobadas$M10<-as.character(Trayectoria_REM57_materias_reprobadas$M10)
      Trayectoria_REM57_materias_reprobadas$M11<-as.character(Trayectoria_REM57_materias_reprobadas$M11)
      Trayectoria_REM57_materias_reprobadas$M12<-as.character(Trayectoria_REM57_materias_reprobadas$M12)
      Trayectoria_REM57_materias_reprobadas$M13<-as.character(Trayectoria_REM57_materias_reprobadas$M13)
      Trayectoria_REM57_materias_reprobadas$M14<-as.character(Trayectoria_REM57_materias_reprobadas$M14)
      Trayectoria_REM57_materias_reprobadas$M15<-as.character(Trayectoria_REM57_materias_reprobadas$M15)
      Trayectoria_REM57_materias_reprobadas$M16<-as.character(Trayectoria_REM57_materias_reprobadas$M16)
      Trayectoria_REM57_materias_reprobadas$M17<-as.character(Trayectoria_REM57_materias_reprobadas$M17)
      Trayectoria_REM57_materias_reprobadas$M18<-as.character(Trayectoria_REM57_materias_reprobadas$M18)
      Trayectoria_REM57_materias_reprobadas$M19<-as.character(Trayectoria_REM57_materias_reprobadas$M19)
      Trayectoria_REM57_materias_reprobadas$M20<-as.character(Trayectoria_REM57_materias_reprobadas$M20)
      Trayectoria_REM57_materias_reprobadas$M21<-as.character(Trayectoria_REM57_materias_reprobadas$M21)
      Trayectoria_REM57_materias_reprobadas$M22<-as.character(Trayectoria_REM57_materias_reprobadas$M22)
      Trayectoria_REM57_materias_reprobadas$M23<-as.character(Trayectoria_REM57_materias_reprobadas$M23)
    }
    
    #A mano
    {
      
      for(i in 1:length(Trayectoria_REM57_materias_reprobadas$M1)){
        if(Trayectoria_REM57_materias_reprobadas$M1[i]=="-1"){
          Trayectoria_REM57_materias_reprobadas$M1[i]<-""
        }
      }
      
      
      for(i in 1:length(Trayectoria_REM57_materias_reprobadas$M2)){
        if(Trayectoria_REM57_materias_reprobadas$M2[i]=="-1"){
          Trayectoria_REM57_materias_reprobadas$M2[i]<-""
        }
      }
      
      
      for(i in 1:length(Trayectoria_REM57_materias_reprobadas$M23)){
        if(Trayectoria_REM57_materias_reprobadas$M3[i]=="-1"){
          Trayectoria_REM57_materias_reprobadas$M3[i]<-""
        }
      }
      
      
      for(i in 1:length(Trayectoria_REM57_materias_reprobadas$M4)){
        if(Trayectoria_REM57_materias_reprobadas$M4[i]=="-1"){
          Trayectoria_REM57_materias_reprobadas$M4[i]<-""
        }
      }
      
      
      for(i in 1:length(Trayectoria_REM57_materias_reprobadas$M5)){
        if(Trayectoria_REM57_materias_reprobadas$M5[i]=="-1"){
          Trayectoria_REM57_materias_reprobadas$M5[i]<-""
        }
      }
      
      
      for(i in 1:length(Trayectoria_REM57_materias_reprobadas$M6)){
        if(Trayectoria_REM57_materias_reprobadas$M6[i]=="-1"){
          Trayectoria_REM57_materias_reprobadas$M6[i]<-""
        }
      }
      
      
      for(i in 1:length(Trayectoria_REM57_materias_reprobadas$M7)){
        if(Trayectoria_REM57_materias_reprobadas$M7[i]=="-1"){
          Trayectoria_REM57_materias_reprobadas$M7[i]<-""
        }
      }
      
      
      for(i in 1:length(Trayectoria_REM57_materias_reprobadas$M8)){
        if(Trayectoria_REM57_materias_reprobadas$M8[i]=="-1"){
          Trayectoria_REM57_materias_reprobadas$M8[i]<-""
        }
      }
      
      for(i in 1:length(Trayectoria_REM57_materias_reprobadas$M9)){
        if(Trayectoria_REM57_materias_reprobadas$M9[i]=="-1"){
          Trayectoria_REM57_materias_reprobadas$M9[i]<-""
        }
      }
      
      
      for(i in 1:length(Trayectoria_REM57_materias_reprobadas$M10)){
        if(Trayectoria_REM57_materias_reprobadas$M10[i]=="-1"){
          Trayectoria_REM57_materias_reprobadas$M10[i]<-""
        }
      }
      
      
      for(i in 1:length(Trayectoria_REM57_materias_reprobadas$M11)){
        if(Trayectoria_REM57_materias_reprobadas$M11[i]=="-1"){
          Trayectoria_REM57_materias_reprobadas$M11[i]<-""
        }
      }
      
      
      for(i in 1:length(Trayectoria_REM57_materias_reprobadas$M12)){
        if(Trayectoria_REM57_materias_reprobadas$M12[i]=="-1"){
          Trayectoria_REM57_materias_reprobadas$M12[i]<-""
        }
      }
      
      
      for(i in 1:length(Trayectoria_REM57_materias_reprobadas$M13)){
        if(Trayectoria_REM57_materias_reprobadas$M13[i]=="-1"){
          Trayectoria_REM57_materias_reprobadas$M13[i]<-""
        }
      }
      
      
      for(i in 1:length(Trayectoria_REM57_materias_reprobadas$M14)){
        if(Trayectoria_REM57_materias_reprobadas$M14[i]=="-1"){
          Trayectoria_REM57_materias_reprobadas$M14[i]<-""
        }
      }
      
      
      for(i in 1:length(Trayectoria_REM57_materias_reprobadas$M15)){
        if(Trayectoria_REM57_materias_reprobadas$M15[i]=="-1"){
          Trayectoria_REM57_materias_reprobadas$M15[i]<-""
        }
      }
      
      
      for(i in 1:length(Trayectoria_REM57_materias_reprobadas$M16)){
        if(Trayectoria_REM57_materias_reprobadas$M16[i]=="-1"){
          Trayectoria_REM57_materias_reprobadas$M16[i]<-""
        }
      }
      
      for(i in 1:length(Trayectoria_REM57_materias_reprobadas$M17)){
        if(Trayectoria_REM57_materias_reprobadas$M17[i]=="-1"){
          Trayectoria_REM57_materias_reprobadas$M17[i]<-""
        }
      }
      
      
      for(i in 1:length(Trayectoria_REM57_materias_reprobadas$M18)){
        if(Trayectoria_REM57_materias_reprobadas$M18[i]=="-1"){
          Trayectoria_REM57_materias_reprobadas$M18[i]<-""
        }
      }
      
      
      for(i in 1:length(Trayectoria_REM57_materias_reprobadas$M19)){
        if(Trayectoria_REM57_materias_reprobadas$M19[i]=="-1"){
          Trayectoria_REM57_materias_reprobadas$M19[i]<-""
        }
      }
      
      
      for(i in 1:length(Trayectoria_REM57_materias_reprobadas$M20)){
        if(Trayectoria_REM57_materias_reprobadas$M20[i]=="-1"){
          Trayectoria_REM57_materias_reprobadas$M20[i]<-""
        }
      }
      
      
      for(i in 1:length(Trayectoria_REM57_materias_reprobadas$M21)){
        if(Trayectoria_REM57_materias_reprobadas$M21[i]=="-1"){
          Trayectoria_REM57_materias_reprobadas$M21[i]<-""
        }
      }
      
      
      for(i in 1:length(Trayectoria_REM57_materias_reprobadas$M22)){
        if(Trayectoria_REM57_materias_reprobadas$M22[i]=="-1"){
          Trayectoria_REM57_materias_reprobadas$M22[i]<-""
        }
      }
      
      
      for(i in 1:length(Trayectoria_REM57_materias_reprobadas$M23)){
        if(Trayectoria_REM57_materias_reprobadas$M23[i]=="-1"){
          Trayectoria_REM57_materias_reprobadas$M23[i]<-""
        }
      }
      
      
    }
    
    
    
    
    
    
    #For por los renglones (misma logica que por las columnas, pero no entiendo por que no funciona)
    {
      for(i in 1:nrow(Trayectoria_REM57_materias_reprobadas)){
        for(j in 1:length(Trayectoria_REM57_materias_reprobadas[i])){
          if(Trayectoria_REM57_materias_reprobadas[j,i]=="-1"){
            Trayectoria_REM57_materias_reprobadas[j,i]<-""
          }
        }
      }
      
    }
    
    {
      for(i in 1:ncol(Trayectoria_REM57_materias_reprobadas)){
        for(j in 1:length(Trayectoria_REM57_materias_reprobadas[i])){
          if(Trayectoria_REM57_materias_reprobadas[i,j]=="-1"){
            Trayectoria_REM57_materias_reprobadas[i,j]<-""
          }
        }
      }
      
    }
    
  }
  
  
  #Primer intento para borrar los NA
  {Trayectoria_REM57<-as.character(Trayectoria_REM57)
    
    Trayectoria_REM57[is.na(Trayectoria_REM57)]<-""
    
    Trayectoria_REM57<-as.data.frame(Trayectoria_REM57)
    
    write.csv(Trayectoria_REM57_materias_reprobadas, "C:\\Users\\Luis Lopez\\Documents\\Proyectos\\Trayectoria_REM57_Materias_reprobadas_blancos.csv", row.names = FALSE)
  }
}
