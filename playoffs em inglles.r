#Autor JoAO LUCIO MATOS RESENDE
#Project: Bracket of playoffs

########
#All functions
#

# confront: get the result from the confront of two teams
#equipe is like team in portuguease
confront <- function(equipe_1, equipe_2){
  result <- sample(c(equipe_1[1], equipe_2[1]), size = 1,prob = c(equipe_1[2], equipe_2[2]))
  return(get(result[[1]]))}

# sorteia_times: put the teams in a randon order. Is like a raffle od the teams

sorteia_times <- function(times){
  sorteio_times <- sample(times)
  
  #criation of the two sides of the bracket. 'chave' is the name given for each side of the bracket. Contais 4 teams.
  chave1 <- sorteio_times[c(1,2,3,4)]
  chave2 <- sorteio_times[c(5,6,7,8)]
  list(chave_1 = chave1, chave_2 = chave2)
} # End on the function of raffle.

# playoffs: put the 1st, 2nd and 3nd and 4nd in confront. Of this function we get the champion (campeao in portuguease)

playoffs <- function(chave1, chave2){
  
  # Fase 1 of the playofss
  
  semi_finalista_1 <- confronto(equipe_1 = chave1[[1]], equipe_2 = chave1[[2]])
  semi_finalista_2 <- confronto(equipe_1 = chave1[[3]], equipe_2 = chave1[[4]])
  semi_finalista_3 <- confronto(equipe_1 = chave2[[1]], equipe_2 = chave2[[2]])
  semi_finalista_4 <- confronto(equipe_1 = chave2[[3]], equipe_2 = chave2[[4]])
  
  # semifinals of playoffs
  
  finalista_1 <- confronto(equipe_1 = semi_finalista_1 , equipe_2 = semi_finalista_2)
  finalista_2 <- confronto(equipe_1 = semi_finalista_3 , equipe_2 = semi_finalista_4)
  
  # the final
  
  final <- confronto(equipe_1 = finalista_1 , equipe_2 = finalista_2)
  
  # print only the name of the champio, without the force of the team.
  
  campeao <- final[1]
  
} # end of the playoff function

########################

#Criation of the teams. 'times' is alist of all teamn and the respective force.

times <- list(
  time_A <- list("time_A",9),
  time_B <- list("time_B",9),
  time_C <- list("time_C",9),
  time_D <- list("time_D",9),
  time_E <- list("time_E",1),
  time_F <- list("time_F",1),
  time_G <- list("time_G",1),
  time_H <- list("time_H",1)
)

ordem <- list()
campeao <- NULL

#This function give the raffle.The number of repetitions is given by the number in the fisrt line

for(i in 1:8064000){
  i <- i + 1
  sorteio <- sorteia_times(times)
  aux <- c(sorteio$chave_1[[1]][1], sorteio$chave_1[[2]][1], sorteio$chave_1[[3]][1], sorteio$chave_1[[4]][1],
           sorteio$chave_2[[1]][1], sorteio$chave_2[[2]][1], sorteio$chave_2[[3]][1], sorteio$chave_2[[4]][1])
  
  # This part extract only the string. Is important, because is this the way that i can filter and group my results:
  
  aux <- unlist(aux)
  aux <- sub(pattern = "time_", replacement = "", x = aux)
  string <- paste(aux, collapse ="")
  ordem[[i]] <- string
  
  campeao <- c(campeao, playoffs(chave1 = sorteio$chave_1, chave2 = sorteio$chave_2)[[1]])
}

# Whith this function i can pickup only the results of a given raffle (sorteio):
# In this exemple i gonna pick up the fallow sequence 'ABCDEFGH':

arranjo <- "ABCDEFGH"
cond <- which(ordem == arranjo)

# Pick up the champions of the string 

resultados <- campeao[cond]

# set numerical the number of time that a team was champion

table(resultados)
tab_result <- table(resultados)
total <- sum(tab_result)

# transform the results in a bar plot.

barplot(table(resultados)/total, main = arranjo, ylim = c(0,0.5))
text(3, 0.45, total)
