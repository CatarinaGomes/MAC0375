matriz_considerada = matrix(nrow = 2, ncol = 2)
matriz_considerada[,1] = c(0,1) #Arcos que saem de x1
matriz_considerada[,2] = c(1,-1) #Arcos que saem de x3
matriz_considerada

#1. Possiveis estados iniciais
p = rep(list(0:1), 2)
possiveis_estados_iniciais = as.matrix(expand.grid(p))
possiveis_estados_iniciais

#2. Funções de input
lista_de_input = vector(mode = "list", length = 4)

 for(e in 1:nrow(possiveis_estados_iniciais)){
   t_poss_state = matrix(NA, 2, 1) #objeto temporario para transpor as linhas da matriz
   t_poss_state[,1] = possiveis_estados_iniciais[e,] # transpondo
   prox_input = matriz_considerada %*% t_poss_state # multiplicando as matrizes
   lista_de_input[[e]] = c(prox_input)
 }
 
lista_de_input
 
#3. Calculando próximo estado
list_prox_estado = vector(mode = "list", length = 4)
 
 for(e in 1:nrow(possiveis_estados_iniciais)){
   prox_estado = c()
   for(r in 1:2){
     if(lista_de_input[[e]][r] > 0){prox_estado[r] = 1}
     else if(lista_de_input[[e]][r] < 0){prox_estado[r] = 0}
     else{ prox_estado[r] = possiveis_estados_iniciais[e,r] }
   }
   list_prox_estado[[e]] = c(prox_estado)
 }

list_prox_estado

tab_verdade = as.data.frame(possiveis_estados_iniciais)

lista_prox_estado = as.data.frame(do.call(rbind, list_prox_estado))
tab_verdade$x3_tmais1 = lista_prox_estado$V2

colnames(tab_verdade) = c("x1(t)", "x3(t)", "x3(t+1)")
tab_verdade
