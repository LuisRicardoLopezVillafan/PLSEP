#Script que funciona----


#Obtenemos la informacion de la fecha de nacimiento contenida en la CURP
{

{
  fechas_nacimiento<-substring(Trayectoria_REM57$CURP, first = 5, last = 10)
}

#Limpiamos el formato para que quede como "Y-m-d"
{for (i in 1:length(fechas_nacimiento)) {
  if(as.numeric(substring(fechas_nacimiento[i],first = 1, last = 2)) > 30){
    fechas_nacimiento[i]<-paste("19",fechas_nacimiento[i], sep = "")
  }
  else {
    fechas_nacimiento[i]<-paste("20",fechas_nacimiento[i], sep = "")
  }
  fechas_nacimiento[i]<-sub("(.{4})(.{2})(.*)", "\\1-\\2-\\3", fechas_nacimiento[i])
}

fechas_nacimiento<-as.Date(fechas_nacimiento, format = "%Y-%m-%d")}


#Definimos una funcion que, dada la fecha de nacimiento, calcule la edad numerica exacta al 1 de juno de 2023
{
edades_decimal<-function(x,D,M,A){
  Y<-as.numeric(format(x, "%Y" ) )
  m<-as.numeric(format(x, "%m" ) )
  d<-as.numeric(format(x, "%d" ) )
  (A-Y)+((M-m)/12)+((D-d)/365.25)
}
}


#Generamos un vector de las edades
{Edades<-0
for(i in 1:length(fechas_nacimiento)){
  Edades<-c(Edades, edades_decimal(fechas_nacimiento[i],1,6,2023))
}
Edades<- Edades[-1]}

#Agregamos las columnas correspondientes
{
Tray_materias_reprobadas$Edades<-Edades
Tray_edades_y_materias<-Tray_materias_reprobadas[,c("Folio", "Edades","Estatus", "Nombre", "Apellidos", "CURP", "Calificaciones_reprobatorias", modulos)]
}

}

View(Tray_edades_y_materias)

write.csv(Tray_edades_y_materias, "C:\\Users\\Luis Lopez\\Documents\\Proyectos\\Trayectoria_REM57_edades_materias.csv", row.names = FALSE)

#Borrador-----

#Funcion que calcula la edad en dias, meses y años para el 1 de junio de 2023
{edad<-function(x){
  Y<-as.numeric(format(x, "%Y" ) )
  m<-as.numeric(format(x, "%m" ) )
  d<-as.numeric(format(x, "%d" ) )
  if(m<6){
    a<-2023-Y
    c<-6+m
    e<-d-1
    if(c==0){
      if(e==0){
        b<-paste(a,"años",sep = " ")
        print(b)
      }else{
        if(e==1){
          b<-paste(a,"años y ", e, "día",sep = " ")
          print(b)
        }else{
          b<-paste(a,"años y ", e, "días",sep = " ")
          print(b)
        }
      }
    }else{
      if(c==1){
        if(e==0){
          b<-paste(a,"años y ", c, "mes",sep = " ")
          print(b)
        }else{
          if(e==1){
            b<-paste(a,"años, ", c, "mes y", e, "día",sep = " ")
            print(b)
          }else{
            b<-paste(a,"años, ", c, "mes y", e, "días",sep = " ")
            print(b)
          }
        }
      }else{
        if(e==0){
          b<-paste(a,"años y", c, "meses",sep = " ")
          print(b)
        }else{
          if(e==1){
            b<-paste(a,"años,", c, "meses y", e, "día",sep = " ")
            print(b)
          }else{
            b<-paste(a,"años,", c, "meses y", e, "días",sep = " ")
            print(b)
          }
        }
      }
    }
  }else{
    a<-2023-Y
    c<-6+m
    e<-d-1
    if(c==0){
      if(e==0){
        b<-paste(a,"años",sep = " ")
        print(b)
      }else{
        if(e==1){
          b<-paste(a,"años y ", e, "día",sep = " ")
          print(b)
        }else{
          b<-paste(a,"años y ", e, "días",sep = " ")
          print(b)
        }
      }
    }else{
      if(c==1){
        if(e==0){
          b<-paste(a,"años y ", c, "mes",sep = " ")
          print(b)
        }else{
          if(e==1){
            b<-paste(a,"años, ", c, "mes y", e, "día",sep = " ")
            print(b)
          }else{
            b<-paste(a,"años, ", c, "mes y", e, "días",sep = " ")
            print(b)
          }
        }
      }else{
        if(e==0){
          b<-paste(a,"años y", c, "meses",sep = " ")
          print(b)
        }else{
          if(e==1){
            b<-paste(a,"años,", c, "meses y", e, "día",sep = " ")
            print(b)
          }else{
            b<-paste(a,"años,", c, "meses y", e, "días",sep = " ")
            print(b)
          }
        }
      }
    }
  }
}
}

