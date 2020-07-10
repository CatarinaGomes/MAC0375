matriz_considerada = matrix(nrow = 4, ncol = 4)
matriz_considerada[,1] = c(-1,1,0,0) #Arcos que saem de x1
matriz_considerada[,2] = c(0,0,-1,-1) #Arcos que saem de x2
matriz_considerada[,3] = c(-1,0,0,1) #Arcos que saem de x3
matriz_considerada[,4] = c(0,-1,1,0) #Arcos que saem de x4

matriz_considerada

#1. - Obtendo todos os possíveis estados iniciais

n = 4 #Numero de genes na rede
p = rep(list(0:1), n) #Possibilidades de estado de ativação

possiveis_estados_iniciais = as.matrix(expand.grid(p))
possiveis_estados_iniciais

#2. - Calculando os inputs a partir dos estados iniciais possíveis

#Lista para armazenar os inputs para o proximo estado de cada estado inicial
lista_de_input = vector(mode = "list", length = 16)


for(e in 1:nrow(possiveis_estados_iniciais)){
  t_poss_state = matrix(NA, 4, 1) #objeto temporario para transpor as linhas da matriz
  t_poss_state[,1] = possiveis_estados_iniciais[e,] # transpondo
  prox_input = matriz_considerada %*% t_poss_state # multiplicando as matrizes
  lista_de_input[[e]] = c(prox_input)
}

lista_de_input

#3. - Aplicando as restrições de acordo com os inputs e calculando proximo estado

#xi(t+1):
  #1, if input(t) > 0
  #0, if input(t) < 0
  #xi(t), if input(t) = 0

#Lista para armazenar o proximo estado de cada estado inicial
lista_prox_estado = vector(mode = "list", length = 16)

for(e in 1:nrow(possiveis_estados_iniciais)){
  prox_estado = c()
  for(r in 1:4){
    if(lista_de_input[[e]][r] > 0){prox_estado[r] = 1}
    else if(lista_de_input[[e]][r] < 0){prox_estado[r] = 0}
    else{ prox_estado[r] = possiveis_estados_iniciais[e,r] }
  }
  lista_prox_estado[[e]] = c(prox_estado)
}

lista_prox_estado


#4. - Verificando o próximo estado para cada estado inicial

library(tidyr)

lista_prox_estado = as.data.frame(do.call(rbind, lista_prox_estado))
b = unite(lista_prox_estado, "y", V1:V4, remove = FALSE, sep = "-")

possiveis_estados_iniciais = as.data.frame(possiveis_estados_iniciais)
a = unite(possiveis_estados_iniciais, "x", Var1:Var4, remove = FALSE, sep = "-")

transicao = data.frame(estado_inicial=a$x, prox_estado=b$y) 
transicao

# Plotando
library(igraph)

g2 = graph.data.frame(transicao, directed=TRUE) 

# Plot graph
plot(g2, vertex.label.cex=c(0.5), vertex.color = "gold",
     vertex.label.color = "black",edge.color="black", 
     edge.arrow.size=.2)
