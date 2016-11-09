#Fonction retournant la proportion de NA d'un vecteur
na.ratio <- function(v)
{
  return(round(length(v[is.na(v)]) / length(v), 2))
}


# fonction permettant de simuler des donn?es
sim.data <- function(n)
{
  x1 <- runif(n)
  x2 <- runif(n)
  x3 <- runif(n)
  x4 <- runif(n)
  x5 <- runif(n)
  e <- rnorm(n, 0, 1)
  y <- 10*sin(pi*x1*x2) + 20*(x3 - 0.5)^2+10*x4+5*x5+e
  return(data.frame(x1, x2, x3, x4, x5, y))
}


# Calcule le test de Luhn (pour la validite des numeros SIREN et SIRET par exemple)
luhnTest <- function(cc) {
  if (is.na(cc) || grepl(pattern = "[a-zA-Z]", cc))
    return(FALSE)
  else
  {
    # Reverse the digits, convert to numeric vector
    cc2 <- as.numeric(unlist(strsplit(as.character(cc), "")))[nchar(cc):1]
    
    s1 <- 0
    s2 <- 0
    
    for (index in 1:length(cc2)) {
      if (index %% 2 == 1) {
        s1 <- sum(s1, cc2[index])
      } else if (cc2[index] >= 5) {
        s2 <- sum(s2, (cc2[index] * 2 - 9))
      } else {
        s2 <- sum(s2, (cc2[index] * 2))
      }
    }
    
    return ((s1 + s2) %% 10 == 0)
  }
}

# Fonction permettant de scaler un vecteur
scale.boundaries <- function(val, v.min, v.max, newmin, newmax){
  coe <- (newmax - newmin)/(v.max-v.min)
  return((val-v.min)*coe + newmin)
}
  
# Génère des données 
n_rows <- 5000
n_cols <- 100

df <- data.frame(
  matrix(runif(n_rows*n_cols), nrow = n_rows, ncol = n_cols))

df$target <- sample(c(0,1), size = n_rows, replace = T, prob = c(0.7, 0.3))

write.csv(file = 'random_data.csv', x = df, row.names = F)
