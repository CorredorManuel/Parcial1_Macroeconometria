PACFfun <- function(Z,K){
  N <- length(Z)
  ACF <- ACF[-c(1)] #con esto se soluciona el problema  de la grafica que inica en 1
  PACF <- matrix(data = NA,nrow = (K),ncol = 1)
  for (k in 1:K) {
    denominador <- matrix(data = NA,nrow = k,ncol = k)
    for(i in 1:k){
      for(j in 1:k){
        if(i==j){
          denominador[i,j] <- 1
        }else{
          denominador[i,j] <- ACF[abs(i-j)]  
        }
      }
    }
    numerador <- denominador
    numerador[,k] <- ACF[1:k]
    
    PACF[k] <- det(numerador)/det(denominador)
    
  }
  #plot(PACF,type="h")
  return(PACF)
  #PACF
}
