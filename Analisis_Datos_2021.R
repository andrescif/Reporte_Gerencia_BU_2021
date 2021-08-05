library(tidyverse)
#Debe estar cargada la base de datos limpia y con 26 variables incluidas

#-------------------¿De que programas y de que sexo son los inscritos?------------------
x <- BASE_DATOS_PSOCIALES_JULIO2021 %>%
        group_by(PROGRAMA,SEXO) %>%
        summarise(n(),b=length(unique(CUI)),(c=b/n())*100)
        #Verificando
        BASE_DATOS_PSOCIALES_JULIO2021 %>%
                filter(BASE_DATOS_PSOCIALES_JULIO2021$PROGRAMA=="ADULTO MAYOR") %>%
                distinct(CUI,.keep_all=TRUE) %>%
                group_by(SEXO) %>%
                summarise(n())
colnames(x) <- c("PROGRAMA","SEXO","CANTIDAD DE REGISTRADOS","REGISTROS ÚNICOS","% DE REGISTROS ÚNICOS")
x$`% DE REGISTROS ÚNICOS`<- format(round(x$`% DE REGISTROS ÚNICOS`,2),nsmall = 2)
write.csv(x,
          file = "Registros por programa, genero y cantidad de únicos")
        #Gráficos
        x %>%
                filter(!is.na(SEXO)) %>%
                ggplot(aes(x=reorder(PROGRAMA,-`CANTIDAD DE REGISTRADOS`),y=`CANTIDAD DE REGISTRADOS`,
                       fill=SEXO))+
                theme_bw()+
                geom_col(position = "dodge")+
                theme(axis.text.x = element_text(angle = 45,hjust =1))+
                xlab("Programa")+
                ylab("Cantidad de registros")+
                ggtitle("Registros Totales por Programa y Sexo")
        
        x %>%
                filter(!is.na(SEXO)) %>%
                ggplot(aes(x=reorder(PROGRAMA,-`REGISTROS ÚNICOS`),y=`REGISTROS ÚNICOS`,
                           fill=SEXO))+
                theme_bw()+
                geom_col(position = "dodge")+
                theme(axis.text.x = element_text(angle = 45,hjust =1))+
                xlab("Programa")+
                ylab("Cantidad de registros")+
                ggtitle("Registros Únicos por Programa y Sexo")
rm(x)
#----------------------¿De qué sexo son los inscritos y los inscritos únicos?-----------
x <- BASE_DATOS_PSOCIALES_JULIO2021 %>%
        group_by(SEXO,GRUPO_EDAD) %>%
        summarise(a=n(),b=length(unique(CUI)),c=(b/a)*100)
colnames(x) <- c("SEXO","GRUPO DE EDAD","REGISTROS TOTALES","REGISTROS ÚNICOS",
                 "% DE REGISTROS ÚNICOS")                
x$`% DE REGISTROS ÚNICOS`<- format(round(x$`% DE REGISTROS ÚNICOS`,2),nsmall = 2)
write.csv(x,
          file = "Registros por sexo, grupo de edad y cantidad de únicos")
        #Gráficos
        x %>%
                filter(!is.na(SEXO) & !is.na(`GRUPO DE EDAD`)) %>%
                ggplot(aes(x=reorder(`GRUPO DE EDAD`,-`REGISTROS TOTALES`),
                           y=`REGISTROS TOTALES`,fill=SEXO))+
                geom_col()+
                xlab("Grupo de edad")+
                ylab("Registros totales")+
                ggtitle("Registros Totales por Grupo de Edad y Sexo")
        
        x %>%
                filter(!is.na(SEXO) & !is.na(`GRUPO DE EDAD`)) %>%
                ggplot(aes(x=reorder(`GRUPO DE EDAD`,-`REGISTROS ÚNICOS`),
                           y=`REGISTROS ÚNICOS`,fill=SEXO))+
                geom_col()+
                xlab("Grupo de edad")+
                ylab("Registros únicos")+
                ggtitle("Registros Únicos por Grupo de Edad y Sexo")
rm(x)
#------------------Registros por programa y grupo de edad-----------------------
x <- BASE_DATOS_PSOCIALES_JULIO2021 %>%
        group_by(GRUPO_EDAD,PROGRAMA) %>%
        summarise(a=n(),b=length(unique(CUI)),c=(b/a)*100)
colnames(x) <- c("GRUPO DE EDAD","PROGRAMA","REGISTROS TOTALES","REGISTROS ÚNICOS",
                 "% DE REGISTROS ÚNICOS")
write.csv(x,
          file = "Registros por grupo de edad y programa")
rm(x)
        #Gráfico
        BASE_DATOS_PSOCIALES_JULIO2021 %>%
                filter(!is.na(GRUPO_EDAD)) %>%
                ggplot(aes(x=GRUPO_EDAD,fill=PROGRAMA))+
                geom_bar()+
                ylab("Grupo de edad")+
                xlab("Cantidad total de registros")+
                ggtitle("Registros Totales por Grupo de Edad y Programa")

x <- BASE_DATOS_PSOCIALES_JULIO2021 %>%
        group_by(PROGRAMA,GRUPO_EDAD) %>%
        summarise(a=n(),b=length(unique(CUI)),c=(b/a)*100)
colnames(x) <- c("PROGRAMA","GRUPO DE EDAD","REGISTROS TOTALES","REGISTROS ÚNICOS",
                 "% DE REGISTROS ÚNICOS")
write.csv(x,
          file = "Resitsros por programa y grupo de edad totales y únicos")
rm(x)
        #Gráfico
        BASE_DATOS_PSOCIALES_JULIO2021 %>%
                filter(!is.na(GRUPO_EDAD) & PROGRAMA!="CONVIVENCIA") %>%
                mutate(PROGRAMA = fct_reorder(PROGRAMA,PROGRAMA, .fun='length' )) %>%
                ggplot(aes(x=PROGRAMA,fill=GRUPO_EDAD))+
                geom_bar()+
                theme(axis.text.x = element_text(angle = 45,hjust =1))+
                ylab("Cantidad de registros")+
                xlab("Programa")+
                guides(fill=guide_legend(title="Grupo de edad"))+
                ggtitle("Registros Totales por Programa y Grupo de Edad")
        #Porcentaje
        x<- BASE_DATOS_PSOCIALES_JULIO2021 %>%
                group_by(PROGRAMA,GRUPO_EDAD) %>%
                summarise(a=length(unique(CUI)))
        y <- BASE_DATOS_PSOCIALES_JULIO2021 %>%
                group_by(PROGRAMA) %>%
                summarise(a=length(unique(CUI)))
        x$b <- y$a[match(x$PROGRAMA,y$PROGRAMA)]
        x$c <- (x$a/x$b)*100
        x$c<- format(round(x$c,2),nsmall = 2)
        #Grafico
        x %>%
                filter(PROGRAMA!="CONVIVENCIA") %>%
                mutate(PROGRAMA = fct_reorder(PROGRAMA,PROGRAMA, .fun='length' )) %>%
                ggplot(aes(x=PROGRAMA,y=a,fill=GRUPO_EDAD)) +
                geom_bar(position="fill", stat="identity")+
                xlab("Programa")+
                ylab("% de registros únicos")+
                guides(fill=guide_legend(title="Grupo de edad"))+
                ggtitle("Porcentaje de Contactos Únicos por Grupo de edad")
        
        x %>%
                filter(PROGRAMA!="CONVIVENCIA"&
                               !is.na(GRUPO_EDAD)) %>%
                mutate(PROGRAMA = fct_reorder(PROGRAMA,PROGRAMA, .fun='length' )) %>%
                ggplot(aes(x=PROGRAMA,y=a,fill=GRUPO_EDAD)) +
                geom_bar(position="fill", stat="identity")+
                xlab("Programa")+
                ylab("% de registros únicos")+
                guides(fill=guide_legend(title="Grupo de edad"))+
                ggtitle("Porcentaje de Contactos Únicos por Grupo de Edad")
                rm(y)
                rm(x)        

#---------------Medios de comunicación con los beneficiarios por programa-------------
x<- BASE_DATOS_PSOCIALES_JULIO2021 %>%
        group_by(PROGRAMA,COMUNICACION) %>%
        summarise(a=n(),b=length(unique(CUI)),c=(b/a)*100)
colnames(x) <- c("PROGRAMA","GRUPO DE EDAD","REGISTROS TOTALES","REGISTROS ÚNICOS",
                 "% DE REGISTROS ÚNICOS")
rm(x)
write.csv(x,
          file = "Registros por programa e info de contacto totales y únicos")
        #Grafico
        BASE_DATOS_PSOCIALES_JULIO2021 %>%
                filter(PROGRAMA!="CONVIVENCIA") %>%
                mutate(PROGRAMA = fct_reorder(PROGRAMA,PROGRAMA, .fun='length' )) %>%
                ggplot(aes(x=PROGRAMA,fill=COMUNICACION))+
                geom_bar()+
                theme(axis.text.x = element_text(angle = 45,hjust =1))+
                ylab("Cantidad de registros")+
                xlab("Programa")+
                guides(fill=guide_legend(title="Info de contacto"))+
                ggtitle("Registros Totales por Programa e Información de Contacto")
        #¿Qué programa es mejor en generar contactos con informacion?
        x<- BASE_DATOS_PSOCIALES_JULIO2021 %>%
                group_by(PROGRAMA,COMUNICACION) %>%
                summarise(a=length(unique(CUI)))
        y <- BASE_DATOS_PSOCIALES_JULIO2021 %>%
                group_by(PROGRAMA) %>%
                summarise(a=length(unique(CUI)))
        x$b <- y$a[match(x$PROGRAMA,y$PROGRAMA)]
        x$c <- (x$a/x$b)*100
        x$c<- format(round(x$c,2),nsmall = 2)
        write.csv(x,
                  file = "Registros únicos por programa y medio de comunicacion con %")        
                #Grafico
                x %>%
                        filter(PROGRAMA!="CONVIVENCIA") %>%
                        mutate(PROGRAMA = fct_reorder(PROGRAMA,PROGRAMA, .fun='length' )) %>%
                        ggplot(aes(x=PROGRAMA,y=c,fill=COMUNICACION)) +
                        geom_bar(position="dodge", stat="identity")+
                        xlab("Programa")
                        
                x %>%
                        filter(PROGRAMA!="CONVIVENCIA") %>%
                        mutate(PROGRAMA = fct_reorder(PROGRAMA,PROGRAMA, .fun='length' )) %>%
                        ggplot(aes(x=PROGRAMA,y=a,fill=COMUNICACION)) +
                        geom_bar(position="fill", stat="identity")+
                        xlab("Programa")+
                        ylab("% de registros únicos")+
                        guides(fill=guide_legend(title="Info de contacto"))+
                        ggtitle("Porcentaje de Contactos Únicos por Info Comunicación")
                rm(y)
                rm(x)
#-----------Por zona y región------------------
x<- BASE_DATOS_PSOCIALES_JULIO2021 %>%
        group_by(PROGRAMA,REGION) %>%
        summarise(a=length(unique(CUI)))
y <- BASE_DATOS_PSOCIALES_JULIO2021 %>%
        group_by(PROGRAMA) %>%
        summarise(a=length(unique(CUI)))
x$b <- y$a[match(x$PROGRAMA,y$PROGRAMA)]
x$c <- (x$a/x$b)*100
x$c<- format(round(x$c,2),nsmall = 2)
write.csv(x,
          file = "Registros únicos por programa y por región con %")
        #Grafico
        x %>%
                filter(PROGRAMA!="CONVIVENCIA" &
                               !is.na(REGION)) %>%
                mutate(PROGRAMA = fct_reorder(PROGRAMA,PROGRAMA, .fun='length' )) %>%
                ggplot(aes(x=PROGRAMA,y=a,fill=REGION)) +
                geom_bar(stat="identity")+
                xlab("Programa")+
                ylab("Total de registros únicos")+
                guides(fill=guide_legend(title="Región"))+
                ggtitle("Registros Únicos por Programa y Región")

        x %>%
                filter(PROGRAMA!="CONVIVENCIA" &
                               !is.na(REGION)) %>%
                mutate(PROGRAMA = fct_reorder(PROGRAMA,PROGRAMA, .fun='length' )) %>%
                ggplot(aes(x=PROGRAMA,y=a,fill=REGION)) +
                geom_bar(position="fill",stat="identity")+
                xlab("Programa")+
                ylab("Total de registros únicos")+
                guides(fill=guide_legend(title="Región"))+
                ggtitle("Porcentaje de Registros Únicos por Programa y Región")
rm(y)
rm(x)
#-------------Analisis de los CUI-------------------
#Obtener los CUI únicos
CUI_UNICOS <- BASE_DATOS_PSOCIALES_JULIO2021 %>%
                distinct(BASE_DATOS_PSOCIALES_JULIO2021$CUI,.keep_all=TRUE)
#Agregar variable municipalidad de naciemiento
CUI_UNICOS$COD_MUNI_NAC <- NA
CUI_UNICOS$COD_MUNI_NAC <-substr(CUI_UNICOS$CUI,10,13)
CUI_UNICOS$COD_MUNI_NAC <- as.integer(CUI_UNICOS$COD_MUNI_NAC)
#Agregar variable departamento de nacimiento
CUI_UNICOS$COD_DEPO_NAC <- NA
CUI_UNICOS$COD_DEPO_NAC <- substr(CUI_UNICOS$CUI,10,11)
CUI_UNICOS$COD_DEPO_NAC <- as.integer(CUI_UNICOS$COD_DEPO_NAC)
#Cargar base de datos de códigos y crear tablas
DEPOS_MUNIS <- 
        read.csv("~/R Projects/Código útil Muni/Data_util/Departamentos y municipios-Table 1.csv")
MUNIS <- DEPOS_MUNIS %>%
        filter(Nivel=="Municipio")
DEPOS <- DEPOS_MUNIS %>%
        filter(Nivel=="Departamento")
rm(DEPOS_MUNIS)                
#Agregar nombre del departamento y municipio de nacimiento
CUI_UNICOS$MUNI_NAC <- MUNIS$Nombre[match(CUI_UNICOS$COD_MUNI_NAC,MUNIS$Código)]
CUI_UNICOS$DEPO_NAC <- DEPOS$Nombre[match(CUI_UNICOS$COD_DEPO_NAC,DEPOS$Código)]
#Cantidad de CUI únicos que no nacieron en Muniguate y no viven en Muniguate
x<- CUI_UNICOS %>%
        filter(!is.na(COD_MUNI_NAC)) %>%
        group_by(COD_MUNI_NAC==101,is.na(ZONA)) %>%
        summarise(a=n(),(b=a/12759)*100)
write.csv(x,
          file = "Tabla de beneficiarios BU no vecinos y no nacidos en Muniguate")
rm(x)
#Filtrar sólo los que no viven en el Municipio
VIVEN_NO_GT <- CUI_UNICOS %>%
        filter(is.na(ZONA))
#Analisis de los CUI unicos y los programas a los que pertenecen
x <- BASE_DATOS_PSOCIALES_JULIO2021 %>%
        group_by(CUI) %>%
        tally()
x$n <- as.factor(x$n)
colnames(x) <- c("CUI","CANTIDAD_DE_APARICIONES")
table(x$n,useNA = "ifany")
y <- BASE_DATOS_PSOCIALES_JULIO2021 %>%
        group_by(CUI,PROGRAMA) %>%
        tally()
y$n <- as.factor(y$n)
table(y$n,useNA = "ifany")
#Crear una nueva base de datos para graficar
z <- y
z <- y %>%
        filter(PROGRAMA!="CONVIVENCIA")
z$BENE_UNI <- NA
z$BENE_UNI <- as.character(z$BENE_UNI)
z$BENE_UNI[z$n==1] <- "1"
z$BENE_UNI[z$n==2] <- "2"
z$BENE_UNI[z$n==3] <- "3"
z$BENE_UNI[z$n==4] <- "4"
z$BENE_UNI[z$n==5] <- "5"
z$BENE_UNI[z$n==6] <- ">5"
z$BENE_UNI[z$n==7] <- ">5"
z$BENE_UNI[z$n==8] <- ">5"
z$BENE_UNI[z$n==9] <- ">5"
z$BENE_UNI[z$n==10] <- ">5"
z$BENE_UNI[z$n==11] <- ">5"
z$BENE_UNI[z$n==12] <- ">5"
z$BENE_UNI[z$n==13] <- ">5"
z$BENE_UNI[z$n==14] <- ">5"
z$BENE_UNI[z$n==15] <- ">5"
z$BENE_UNI[z$n==16] <- ">5"
z$BENE_UNI[z$n==17] <- ">5"
z$BENE_UNI[z$n==18] <- ">5"
z$BENE_UNI[z$n==19] <- ">5"
z$BENE_UNI[z$n==20] <- ">5"
z$BENE_UNI[z$n==21] <- ">5"
z$BENE_UNI[z$n==22] <- ">5"
z$BENE_UNI[z$n==23] <- ">5"
z$BENE_UNI[z$n==24] <- ">5"
z$BENE_UNI[z$n==25] <- ">5"
z$BENE_UNI <- as.factor(z$BENE_UNI)
z$BENE_UNI <- factor(z$BENE_UNI,levels = c("1","2","3","4","5",">5"))
z$PROGRAMA <- factor(z$PROGRAMA,levels = c("MUNIEDUCA MÓVIL","TÉCNICO PRODUCTIVO",
                                           "BIBLIOTECAS MUNICIPALES","ADULTO MAYOR",
                                           "GESTIÓN DE RIESGO","COMUNIDAD JUVENIL"))
z %>%
        ggplot(aes(x=BENE_UNI,fill=PROGRAMA))+
        geom_bar(position = "dodge")+
        xlab("Número de veces que aparece cada usuario")+
        ylab("Número de usuarios")+
        ggtitle("Veces que Aparece Cada Beneficiario por Programa")
rm(x)
rm(z)
        #¿En qué pogramas participan los únicos?
        y$NUM_UNICO <- y %>%
                group_indices()
        x <- BASE_DATOS_PSOCIALES_JULIO2021 %>%
                group_by(CUI) %>%
                tally()
        colnames(x) <- c("CUI","CANTIDAD_DE_APARICIONES")
        z <- y %>%
                group_by(NUM_UNICO) %>%
                summarise(n())
        x$PROGRAMA <- z$`n()`
        write.csv(x,
                  file = "Tabla CUI unicos con cantidad de apariciones y cantidad de programas")
        rm(y)
        rm(x)
        rm(z)
#¿Qué cursos son los más pobaldos por hombres?
SOLO_HOMBRES <- CUI_UNICOS %>%
        filter(SEXO=="Masculino")


        





