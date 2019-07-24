### PRACTIA 1 TARIFICACIÓN Y PRICING PRIMAS DE SEGUROS ###

# Vamos a usar los csv sobre datos con Pólizas y Siniestros (Claims)

# Usaremos Modelos GLM para explicar el COSTE y la Frecuencia de los SINIESTROS

#fichero de pólizas
poliza1 <- read.csv(file = "Policy_v1.csv", row.names = NULL, sep = ";")
summary(poliza1)

poliza2 <- read.csv(file = "Policy_v2.csv", row.names = NULL, sep = ";")
summary(poliza2)
str(poliza2)

polizas <- merge(poliza1,poliza2, by = c("ID_POL"))


siniestros <- read.csv(file = "Claims_v1.csv", row.names = NULL, sep = ";")
summary(siniestros)

hist(as.numeric(siniestros$Costes))

table(siniestros$TipoSiniestro)

#Modelo Corp Damage


summary(polizas)

SINI_RCC <- siniestros[siniestros$TipoSiniestro %in% c("RC_Corporal"),]




