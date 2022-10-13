

#tiene en cuenta la posicion 0
ACFfun0 <- function(Z,K){
  media <- mean(Z)
  N <- length(Z)
  ACF <- matrix(data = NA,nrow = (K+1),ncol = 1)#creación vector vacio
  for(k in 0:K){
    Zk <- Z[(k+1):N] # valor eliminados al inicio
    Zklag <- Z[1:(N-k)] #valores eliminados al final
    numerador <- sum((Zk-media)*(Zklag-media))
    denominador <- sum((Z-media)^2)
    ACF[k+1] <- numerador/denominador
  }
  #plot(ACF,type="h")
  return(ACF)
}



#media cero
ACFfun00 <- function(Z,K){
  media <- 0
  N <- length(Z)
  ACF <- matrix(data = NA,nrow = (K+1),ncol = 1)#creación vector vacio
  for(k in 0:K){
    Zk <- Z[(k+1):N] # valor eliminados al inicio
    Zklag <- Z[1:(N-k)] #valores eliminados al final
    numerador <- sum((Zk-media)*(Zklag-media))
    denominador <- sum((Z-media)^2)
    ACF[k+1] <- numerador/denominador
  }
  plot(ACF,type="h")
  return(ACF)
}

# omite el valor en la posicion 0
ACFfun <- function(Z,K){
  media <- mean(Z)
  N <- length(Z)
  ACF <- matrix(data = NA,nrow = (K),ncol = 1)#creación vector vacio
  for(k in 1:K){
    Zk <- Z[(k+1):N] # valor eliminados al inicio
    Zklag <- Z[1:(N-k)] #valores eliminados al final
    numerador <- sum((Zk-media)*(Zklag-media))
    denominador <- sum((Z-media)^2)
    ACF[k] <- numerador/denominador
  }
  #plot(ACF,type="h")
  return(ACF)
}