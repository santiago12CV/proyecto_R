########################

#######################


# DEFINIR PARAMETROS ------------------------------------------------------
N <- 5010
CO <- 0.3
pmc <- 0.6
sigma <- 3

# CREAR INGRESOS ----------------------------------------------------------
set.seed(777)
ingresos <- runif(n = N,min = 1,max = 10)
head(ingresos)
tail(ingresos)

# CREAR PERTURBACIONES ----------------------------------------------------
set.seed(69)
e <- rnorm(n = N,mean = 0,sd = sigma)

# CREAR CONSUMOS ----------------------------------------------------------

consumos <-CO+pmc*ingresos+e 

# GRAFICOS ----------------------------------------------------------------

plot(ingresos,consumos)

# AISLAR DATOS OBSERVABLES ------------------------------------------------

datos <- cbind(consumos,ingresos)
head(datos)

#VAMOS A BORRAR LO QUE NO NECESITAMOS para dejar solo "datos"
rm(CO,consumos,e,ingresos,N,pmc,sigma)




# ESTIMACIÓN --------------------------------------------------------------

Y <- datos[,2] #ingresos
C <- datos[,1] #consumos
nobs <- length(C) #número de observaciones


# medias ------------------------------------------------------------------

#ingresos
media_y <- sum(Y)/nobs
#CONSUMO
media_c <- sum(C)/nobs

#ESTIMAR pmc (BETA2GORRO)
numerador <- sum((C-media_c)*(Y-media_y))*sqrt(sum((C-media_c)^2))
denominador <- sqrt(sum((C-media_c)^2)*sum((Y-media_y)^2))*sqrt(sum((Y-media_y)^2))
pmcgorro <- numerador/denominador#beta2gorro
cogorro <- media_c-pmcgorro*media_y#beta1gorro
Ygorro <- cogorro+pmcgorro*Y
egorro <- C-Ygorro
mean_egorro <- sum(egorro)/length(egorro)
egorro
sum(egorro)
#R^2 normal a mano

RCUADRADO <-1- (sum((egorro-mean_egorro)^2)/sum((C-media_c)^2))
RCUADRADO


R2 <- function(y,e_gorro){
  y <- as.matrix(y)
  e_gorro <- as.matrix(e_gorro)
  mean_y <- sum(y)/length(y)
  R_2 <- 1- (sum((e_gorro)^2)/(sum((y-mean_y)^2)))
  return(R_2)
}
R2(y=C,e_gorro= egorro)
  
R2(y=Y,egorro=egorro)
R2

R2ADJ <- function(y,e_gorro,n,k,R22){
  y <- as.matrix(y)
  e_gorro <- as.matrix(e_gorro)
  mean_y <- sum(y)/length(y)
  R_2 <- 1- (sum((e_gorro)^2)/(sum((y-mean_y)^2)))
  R2ADJ <- 1-(((n-1)/(n-k-1))*(1-R_2)) 
  return(R2ADJ)
}


R2ADJ(n=N,2,R2=R2)





# GRAFICA -----------------------------------------------------------------

plot(Y,C,main="Ejericio de similacion consumo keynesiano",
     col="darkgreen",xlab = "ingresos",ylab="consumos")
abline(a=cogorro,b=pmcgorro,lwd=4,col="orange")
