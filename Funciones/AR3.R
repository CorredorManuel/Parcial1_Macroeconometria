


AR3 <- function(phi1,phi2,phi3,k){
  
  
  raices <- polyroot(c(1,-phi1,-phi2,-phi3))
  factores <- c(1/raices[1],1/raices[2],1/raices[3])
  raices1 <- as.numeric(raices) #artificio para imprimir en decimal
  #raices1 <-raices
  print(paste("G1^-1 = ", as.numeric(raices[3]), "y G1 = ", as.numeric((1/raices[3]))))
  print(paste("G2^-1 = ", as.numeric(raices[2]), "y G2 = ", as.numeric((1/raices[2]))))
  print(paste("G3^-1 = ", as.numeric(raices[1]), "y G3 = ", as.numeric((1/raices[1]))))
  
  #1  verifiacion estacionariedad 
  if (abs(raices[1])>1 & abs(raices[2])>1 &abs(raices[3])>1) {
    print("El proceso es estacionario")
  }else{
    print("El proceso no es estacionario")
  }
  
  #2 ACF
  rho0 <- 1 
  
  
  rho1 <- (phi1+phi3*phi2)/(1-phi2-phi1*phi3-phi3**2)
  rho2 <- rho1*(phi1+phi3)+phi2
  valores1  <- c(1,rho1,rho2)
  psi1 <- phi1
  psi2 <- phi2+phi1^2
  valores2 <-c(1,psi1,psi2)   
  RhosFun <- cbind()
  cond_in <- cbind() 
  macond_ini <- cbind()
  psis <- cbind()
  
  if((raices1[3])==(raices1[2]) & (raices1[2])==(raices1[1])){
    print("todas las raices iguales")
    cond_in <- solve(matrix(c(1,factores[1],factores[1]**2,1,factores[1],factores[1]**2,0,factores[1],2*factores[1]**2,3,3)))%*%valores1
    for (i in 0:k) {
      RhosFun[i+1] <- (cond_in[1]+cond_in[2]+(cond_in[3]*i))*(factores[1]**i)
    }
    macond_ini <- solve(matrix(c(1,factores[1],factores[1]**2,1,factores[1],factores[1]**2,0,factores[1],2*factores[1]**2,3,3)))%*%valores2 
    for (i in 0:k) {
      psis[i+1] <- (macond_ini[1]+macond_ini[2]+(macond_ini[3]*i))*(factores[1]**i)
    } 
  }else if(raices1[3]!=raices1[2] & raices1[2]!=raices1[1] & raices1[1] != raices1[3] ){
    print("raices diferentes") 
    cond_in <- solve(matrix(c(1,factores[3],factores[3]**2,1,factores[2],factores[2]**2,1,factores[1],factores[1]**2),3,3))%*%valores1     
    for (i in 0:k) {
      RhosFun[i+1] <- as.numeric(cond_in[1]*factores[3]**i+cond_in[2]*factores[2]**i+cond_in[3]*factores[1]**i)
    }
    macond_ini <- solve(matrix(c(1,factores[3],factores[3]**2,1,factores[2],factores[2]**2,1,factores[1],factores[1]**2),3,3))%*%valores2 
    
    for (i in 0:k) {
      psis[i+1] <- as.numeric(macond_ini[1]*factores[3]**i+macond_ini[2]*factores[2]**i+macond_ini[3]*factores[1]**i)
    }           
  }else if(raices1[3]==(raices1[2]) ){
      print("raices iguales 1 y 2 ")
      cond_in <- solve(matrix(c(1,factores[3],factores[3]**2,0,factores[3],2*factores[3]**2,1,factores[1],factores[1]**2),3,3))%*%valores1
       
      for (i in 0:k) {
        RhosFun[i+1] <- as.numeric((cond_in[1]+cond_in[2]*i)*(factores[3]**i)+(cond_in[3])*(factores[1]**i)) #dos raices
      }
      macond_ini <- solve(matrix(c(1,factores[3],factores[3]**2,0,factores[3],2*factores[3]**2,1,factores[1],factores[1]**2),3,3))%*%valores2
      
      for (i in 0:k) {
        psis[i+1] <- as.numeric((macond_ini[1]+macond_ini[2]*i)*(factores[3]**i)+(macond_ini[3])*(factores[1]**i))
      }  
  }else if(raices1[2]==(raices1[1]) ){
    print("raices iguales 2 y 3 ")
    cond_in <- solve(matrix(c(1,factores[3],factores[3]**2,1,factores[2],factores[2]**2,0,factores[2],2*factores[2]**2),3,3))%*%valores1
    for (i in 0:k) {
      RhosFun[i+1] <- as.numeric((cond_in[2]+cond_in[3]*i)*(factores[2]**i)+(cond_in[1])*(factores[3]**i)) #dos raices
    }
    macond_ini <- solve(matrix(c(1,factores[3],factores[3]**2,1,factores[2],factores[2]**2,0,factores[2],2*factores[2]**2),3,3))%*%valores2
    
    for (i in 0:k) {
      psis[i+1] <- as.numeric((macond_ini[2]+macond_ini[3]*i)*(factores[2]**i)+(macond_ini[1])*(factores[3]**i))
    }  
  }
  

  
  
  
  
  print(paste("A1 = ",as.numeric(cond_in[1]), ", A2 = ", as.numeric(cond_in[2]), ", A3 = ", as.numeric(cond_in[3])))
  print("ACF Teorico") 
  print(cbind(round(RhosFun,8))) 
  
  
  
  #Calculo de Phi 22 por crammer
  num <- det(matrix(c(1,RhosFun[2],RhosFun[2],RhosFun[3]),2,2))
  den <- det(matrix(c(1,RhosFun[2],RhosFun[2],1),2,2))
  phi_22 <- num/den
  #Calculo de Phi 33 por crammer
  num1 <- det(matrix(c(1,RhosFun[2],RhosFun[2],RhosFun[2],1,RhosFun[3],RhosFun[3],RhosFun[2],RhosFun[4]),3,3))
  den1 <- det(matrix(c(1,RhosFun[2],RhosFun[3],RhosFun[2],1,RhosFun[2],RhosFun[3],RhosFun[2],1),3,3))
  phi_33 <- num1/den1
  
  print("PACF Teorico")
  print(paste("phi11 es ",round(RhosFun[2],4)))
  print(paste("phi22 es ",phi_22))
  print(paste("phi33 es ",phi_33))
  

  #valores2
  print(paste("A1 MA = ",as.numeric(macond_ini[1]), ", A2 MA = ",as.numeric(macond_ini[2]),", A3 MA = ",as.numeric(macond_ini[3])))
  print("Psis") 
  print(cbind(round(psis,8)))
  
  #plot(RhosFun,type="h")
  #print(cbind(round(RhosFun,8))) ##para meter esto despues en la funcion PACF
  }
  #AR3(0.5,0.0525,0.0582,6)
  #AR3(-0.5,0.0325,0.0382,6)
#AR3(0.78,-0.1944,0.0156,8)
#AR3(0.7,0.14,-0.12,7)

