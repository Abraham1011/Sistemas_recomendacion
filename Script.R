library(readxl)
library(tidyverse)
library(recommenderlab)

get_ratings <- function(R){
  r_hat <- apply(R, 1, function(x){
    x[!is.na(x)]
  })
  r_hat <- round(r_hat,0)
  r_hat <- ifelse(r_hat < 0, 0,
                  ifelse(r_hat > 7, 7, r_hat))
  return(r_hat)
}

df <- read_excel('omm_resultados_completo.xlsx')



modelos <- c("UBCF","IBCF","ALS",
             "POPULAR","SVD","RANDOM")

replicas <- 30
resultados <- matrix(ncol = 2, nrow = replicas * length(modelos))
resultados <- as.data.frame(resultados)
colnames(resultados) <- c("Modelo","ECM")

counter <- 1
for (metodo in modelos) {
  for(i in 1:replicas){
    df_in <- df
    r_real <- c()
    for(j in 1:nrow(df)){
      remover <- sample(x = 1:6, size = 1)
      df_in[j,remover + 2] <- NA
      r_real[j] <- as.numeric(df[j,remover + 2])
      
    }
    
    R <- as.matrix(df_in[,3:8])
    r <- as(R,"realRatingMatrix")
    
    if(metodo != "SVD"){
      model <- Recommender(r, method = metodo)
    }else{
      model <- Recommender(r, method = metodo, 
                           parameter = list(k = 3))
    }
    recom <- predict(object = model, newdata = 1:nrow(R),
                     n = 1,data = r)
    R_hat <- as(recom, "matrix")
    r_hat <- get_ratings(R_hat)
    
    ecm <- mean((r_real - r_hat)^2)
    resultados[counter,1] <- metodo
    resultados[counter,2] <- ecm
    counter <- counter + 1
    
  }
}

saveRDS(resultados,"resultados.RDS")

#toBibtex(citation("recommenderlab"))



