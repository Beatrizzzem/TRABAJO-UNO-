install.packages("tidyverse")
library(tidyverse)

setwd("~/GitHub/TRABAJO-UNO-")

set.seed(10)
sample(c("SI","NO"), 10, replace = T)
########## EJERCICIO 1 ############

# FUNCION SUFRAGIO
sufragio <- function(total,votosSI,votosNO){
  if (votosSI >= ((total*0.5)+1)) {
    print("TRIUNFA EL SI")
  } else{
    print("votosSI no alzcanza Quorum")
    if (votosSI >= total*0.3) {
      print("votosSI obtiene el 30% de Quorum")
      if (votosSI >= total*0.3 & votosNO < total*0.3) {
        print("TRIUNFA EL SI")
      } else{
        if (votosNO >= ((total*0.5)+1)) {
          print("TRIUNFA EL NO")
        } else{
          print("votosNO obtiene el 30% de Quorum")
          if (votosSI == votosNO) {
            print("Los Quorums son identicos, ha ganado el SI por empate")
          } else{
            if (votosSI > votosNO) {
              print("TRIUNFA EL SI")
            } else{
              print("TRIUNFA EL NO")
            }
          }
        }
      }
    } else{
      print("TRIUNFA EL NO")
    }
  }
}

# SE OCUPA LA FUNCION
sufragio (10,6,4)


# FUNICION SUFRAGIO
sufragio_solo_total <- function(total){
  set.seed(total)
  padron <- sample(c("SI","NO"),total,replace = TRUE)
  padron <- as.data.frame(padron)
  names(padron) <- c("votos")
  votosSI <- sum(with(padron,votos == "SI"))
  votosNO <- sum(with(padron,votos == "NO"))
  
  if (votosSI >= ((total*0.5)+1)) {
    print("TRIUNFA EL SI")
  } else{
    print("votosSI no alcanza Quorum")
    if (votosSI >= total*0.3) {
      print("votosSI obtiene el 30% de Quorum")
      if (votosSI >= total*0.3 & votosNO < total*0.3) {
        print("TRIUNFA EL SI")
      } else{
        if (votosNO >= ((total/2)+1)) {
          print("TRIUNFA EL NO")
        } else{
          print("votosNO obtiene el 30% de Quorum")
          if (votosSI == votosNO) {
            print("Los Quorums son identicos, ha ganado el NO por empate")
          } else{
            if (votosSI > votosNO) {
              print("TRIUNFA EL SI")
            } else{
              print("TRIUNFA EL NO")
            }
          }
        }
      }
    } else{
      print("TRIUNFA EL NO")
    }
  }
}

# SE OCUPA LA FUNCION, HAY QUE INGRESAR LOS 10 VOTOS.
sufragio_solo_total( 10 )

# RESPUESTAS:
# FUNCION SET.SEED() ES ADEACUADA UTILIZARLA PARA OBTENER NUMEROS DE FORMA ALEATORIA.
# FUNCION SAMPLE() FACULTA MOSTRAR LOS DATOS (NUMEROS DE FORMA ALEATORIA) DE UNA MANERA FIJA.

########ejercicio 2##########
listaDocumentos <- list(c("mp","Juan","Christofer"),c("of","av01","ampr"),c("of","av01","ante"),
                        c("of","av08","arme"),c("of","av02","ante"),c("of","av07","ampr"),
                        c("of","av03","dape"),c("of","av01","meca"),c("of","av02","dape"),
                        c("mp","Antonia"),c("mp","Christian","Mario"),
                        c("mp","Jose","Pedro","Antonela"),c("of","av05","meca"),
                        c("of","av04","dape"),c("of","av02","arme"))
conteodeniños <- list()
for(documento in listaDocumentos){
  if(documento[1] == "mp"){
    # variable que detecta si encontro o no conteo de niños
    encontro <- F
    # revisando si el conteo de niños tiene datos
    if(length(conteodeniños) != 0){
      # revisando conteo de niños 
      for (posicionconteodeniños in 1:length(conteodeniños)) {
        # se sacan los elementos de la lista
        conteodeniñosunlist <- unlist(conteodeniños[posicionconteodeniños])
        # se revisa conteo de niños
        if(conteodeniñosunlist[1] == (length(documento)-1)){
          conteodeniñosunlist[2] <- conteodeniñosunlist[2]+1
          conteodeniños[posicionconteodeniños] <- list(c(conteodeniñosunlist))
          encontro <- T
        }
      }
    }
    # se hace nuevo conteo de niños
    if(!encontro){
      nuevaconteodeniños <- c()
      nuevaconteodeniños[1] <- (length(documento)-1)
      nuevaconteodeniños[2] <- 1
      conteodeniños <- c(conteodeniños,list(c(nuevaconteodeniños)))
    }
  }
}
# conteo de niños
for (conteodeniños in conteodeniños) {
  print(paste("Se cuentan con",conteodeniños[2],"mp de",conteodeniños[1],"niños"))
}
  
