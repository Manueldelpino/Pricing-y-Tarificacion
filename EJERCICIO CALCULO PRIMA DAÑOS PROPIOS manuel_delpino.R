### EJERCICIO CALCULO PRIMA DAÑOS PROPIOS - Manuel del Pino Guerrero


##### 1. Bloque de inicializacion de librerías #####

if(!require("zoo")){
  install.packages("zoo")
  library("zoo")
}

if(!require("caTools")){
  install.packages("caTools")
  library("caTools")
}

if(!require("ROCR")){
  install.packages("ROCR")
  library("ROCR")
}

if(!require("dplyr")){
  install.packages("dplyr")
  library("dplyr")
}

## -------------------------------------------------------------------------

##### 2. Bloque de parámetros y datasets iniciales necesarios para la práctica #####

setwd("C:/Users/Knowhow/Desktop/CUNEF/PRICING Y TARIFICACION")

# fichero de polizas
Polizas=read.csv2('Policy_v1.csv')


# fichero de polizas
Polizas2=read.csv2('Policy_v2.csv')

# fichero de siniestros
Siniestros = read.csv2('Claims_v1.csv')

str(Siniestros)
summary(Siniestros)

hist(Siniestros$Costes)
svd(Siniestros$Costes)


table(Siniestros$TipoSin)

##### 4. Análisis descriptivo de Pólizas #####

str(Polizas2)
summary(Polizas2)

hist(Polizas$Valor)
hist(Polizas$Potencia)
hist(Polizas$Peso)


table(Polizas$Forma_Pago)
table(Polizas$Antiguedad)
table(Polizas$Sexo)
table(Polizas$Edad,Polizas$Comb) #donde Comb es Combustible
#Polizas$start_date2<-as.Date(Polizas$start_date)
#Polizas$Expuestos=diff.Date(Polizas$end_date,Polizas$start_date)/365

# 5. Procedo ahora a realizar el proceso de cálculo de la prima para Own Damage ("Daños Propios")

SINI_RCC=Siniestros[Siniestros$TipoSin %in% c("Daños Propios"),] 
Costes=aggregate(SINI_RCC$Costes, by = list(SINI_RCC$ID_POL), mean) #Aquí le agrego los costes
Numero=aggregate(sign(SINI_RCC$Costes), by = list(SINI_RCC$ID_POL), sum)
Costes <- data.frame(ID_POL=Costes$Group.1, Costes=Costes$x)
Numer <- data.frame(ID_POL=Numero$Group.1,Nsini=Numero$x)
SINI_RCC=merge(SINI_RCC,Costes)
SINI_RCC=merge(SINI_RCC,Numer)
summary(SINI_RCC)  # Ya podemos ver agregados los datos de Costes más frecuencia o Número de Siniestros.

hist(SINI_RCC$Costes)

# 6. Modelo Corp Damage (Modelo de COSTE DE SINIESTROS)
RCC2 <- merge(SINI_RCC, Polizas2, by = "ID_POL")
RCC2 <- RCC2 %>% filter(Costes>0)
modelo2_C1=glm(Costes~Edad_FMT,data=RCC2,family=Gamma)
summary(modelo2_C1)
modelo2_C4=glm(Costes~Carnet_FMT+Forma_Pago,data=RCC2,
               family=Gamma)
summary(modelo2_C4)

Polizas2 <- Polizas2 %>% mutate(PredCorpDamage=predict(modelo2_C1,newdata = Polizas2,type = "response"))



# 7. Modelo NÚMERO DE SINIESTROS

RCCF <- merge(Polizas2,SINI_RCC, by = "ID_POL",all.x=TRUE)
RCCF[is.na(RCCF$Nsini),"Nsini"]<-0
RCCF <- RCCF %>% filter(Nsini>=0)
summary(RCCF)
summary(SINI_RCC)

ModeloN_C1=glm(Nsini~Edad_FMT+Valor_FMT+Sexo+Comb+
                 Potencia_FMT+Peso_FMT+Bonus_RC,data=RCCF,
               family=poisson(link = "log"))
summary(ModeloN_C1)

Polizas2 <- Polizas2 %>% mutate(PredCorpDamageFRQ=predict(ModeloN_C1,newdata = Polizas2,type = "response"))
Polizas2 <- Polizas2 %>% mutate(Prima=(PredCorpDamage * PredCorpDamageFRQ))
summary(Polizas2$Prima)

## 8. MODELO CÁLCULO PRIMA DAÑOS PROPIOS (La variable target o Intercept va a ser Prima en este caso para el Modelo GLM)


ModeloN_prm=glm(Prima~Edad_FMT+Valor_FMT+Sexo+Comb+
                  Potencia_FMT+Peso_FMT+Bonus_RC,data=Polizas2,
                family=Gamma(link = "log"))
Polizas2 <- Polizas2 %>% mutate(PredPrimaDaños=predict(ModeloN_prm,newdata = Polizas2,type = "response"))
summary(ModeloN_prm)
summary(Polizas2)  

# Una vez creados los modelos para costes, siniestros y daños propios, vemos que se han creado las columnas
## Prima y PredPrimaDaños en la tabla polizas2, las cuales he denominado así para llevar a cabo los cálculos que se requieren en la presente práctica.
## Posteriormente, hacemos un filtrado en dicho DataFrame polizas2 con los valores determinados para así calcular los valores de Prima de Daños.