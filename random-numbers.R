#Exercícios Ripley - Stochastic Simulation
#Ex.1 - Gerador Middle-Square de von Neumann


vonNeumann <- function(n, seed){
res <- matrix(nrow = n+1, ncol = 1)
  res[1,1] <- seed
  for(i in c(1:n)){
    a <- nchar(res[i,1]^2)
    b <- ((nchar(res[i,1]^2)/4)-1)*2
      res[1+i,1] <- round((res[i,1]^2 %% 10^(a-floor(b)))%/%10^ceiling(b))
  }
return(res)
}

vonNeumann(n = 100, seed = 8650)
vonNeumann(n = 100, seed = 8651)
vonNeumann(n = 100, seed = 8652)
vonNeumann(n = 100, seed = 8653)
vonNeumann(n = 100, seed = 8654)
vonNeumann(n = 100, seed = 8655)
vonNeumann(n = 100, seed = 8656)
vonNeumann(n = 100, seed = 8657)
vonNeumann(n = 100, seed = 8658)
lattice_1 <- vonNeumann(n = 100, seed = 8659)

#Ex. 4, 5 e 6 Geradores congruenciais

congGen <- function(seed, M, multiplier, shift, n){
unif <- matrix(nrow=n+1, ncol = 1)
unif[1,1] <- seed
for(i in c(1:n)){
unif[i+1,1] <- (multiplier*unif[i,1] + shift) %% M
  }
return(unif)
}

congGen(seed = 1.97, M = 1, multiplier = 1013, shift = 0, n =50)

congGen(seed = 2, M = 64, multiplier = 29, shift = 17, n =50)
congGen(seed = 2, M = 64, multiplier = 9, shift = 1, n =50)
congGen(seed = 2, M = 64, multiplier = 13, shift = 0, n =50)
congGen(seed = 2, M = 64, multiplier = 11, shift = 0, n =50)

#Período = 33
congGen(seed = 2, M = 67, multiplier = 10, shift = 0, n =50)
#Período = 66
congGen(seed = 2, M = 67, multiplier = 12, shift = 0, n =150)
#Período = 99
congGen(seed = 2, M = 67, multiplier = 16, shift = 0, n =150)
#Período = 132
lattice <- congGen(seed = 2, M = 67, multiplier = 18, shift = 0, n =150)

#Plotando retículados dos números gerados:
plot(x = lattice[1:150], y = lattice[2:151])
plot(lattice_1[1:100,], lattice_1[2:101,1])

#2.16 implemente os testes de independencia (Gaps, runs e permutações) 
#e uniformidade:
