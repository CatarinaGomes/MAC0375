#######  Item a #######

dados = read.csv("tabela_4.csv", row.names = 1) #Tabela do enunciado

library(plyr)
dados_count = count(dados, vars = c("x1", "x2", "x3"))  #Verifica a frequencia de cada estado 
dados_count

#Veririfica a probabilidade de cada estado ocorrer
dados_count$prob = dados_count$freq/nrow(dados) 
dados_count
p = rep(list(0:1), 2) #2 genes com dois estados iniciais possiveis no tempo t
possiveis_estados_iniciais = as.data.frame(expand.grid(p))
colnames(possiveis_estados_iniciais)=c("x1(t)", "x2(t)")

#Possiveis estados iniciais dos genes x1 e x2
possiveis_estados_iniciais 

#Função que calcula as probabilidades conjuntas de x1 e x2 em t
probs = function(p){
  
  #p é cada uma das linhas da tabela possiveis_estados_iniciais
  #dados_count armazena a frequencia dos estados nos dados e as suas probabilidades
  
  c = (dados_count$x1 == p[1] & dados_count$x2 == p[2]) 
  c = dados_count[c,] #Seleciona apenas os true
  
  prob = sum(c$prob) #Soma as probabilidades
}

tab = possiveis_estados_iniciais

#aplica a função em cada linha da tabela
tab$prob = apply(possiveis_estados_iniciais, 1, probs) 
tab

#Função que calcula as probabilidades conjuntas de x1, x2 e x3 para os casos em que x3 é = 0. (P[a,b])

pabzero = function(p){
  
  #p é cada uma das linhas da tabela possiveis_estados_iniciais
  #dados_count armazena a frequencia dos estados nos dados e as suas probabilidades
  
  c2 = (dados_count$x1 == p[1] & dados_count$x2 == p[2] & dados_count$x3 == 0)
  c2 = dados_count[c2,] #Seleciona apenas os true
  
  c2 = sum(c2$prob) #Soma as probabilidades
}

tab$pab_zero = apply(possiveis_estados_iniciais, 1, pabzero) #aplica em todas as linhas


#Função que calcula as probabilidades conjuntas de x1, x2 e x3 para os casos em que x3 é = 1. (P[a,b])

pabone = function(p){
  c3 = (dados_count$x1 == p[1] & dados_count$x2 == p[2] & dados_count$x3 == 1)
  c3 = dados_count[c3,] #Seleciona apenas os true
  
  c3 = sum(c3$prob) #Soma as probabilidades 
}

tab$pab_one = apply(possiveis_estados_iniciais, 1, pabone) #aplica em todas as linhas

tab

#Calcula de fato a probabilidade condicional para x3 = 0 em t+1
tab$prob_cond_zero = tab$pab_zero/tab$prob 

#Calcula de fato a probabilidade condicional para x3 = 1 em t+1
tab$prob_cond_one = tab$pab_one/tab$prob

#Excluindo as colunas intermediarias que armazenaram P[a,b]
tab$pab_zero = tab$pab_one = NULL 

tab

# Calculando a entropia condicional para o gene x3 em t+1, dado cada estado inicial de x1 e x2

tab$incerteza = (tab$prob_cond_one*-log2(tab$prob_cond_one)) + 
  (tab$prob_cond_zero*-log2(tab$prob_cond_zero))

tab

#Substituindo NaN por 0
tab$incerteza = as.numeric(gsub(NaN, 0, tab$incerteza))

#Calculando a entropia normalizada de cada linha
tab$entropia_norm =  tab$incerteza * tab$prob

colnames(tab)= c("x1(t)", "x2(t)", 
                 "Prob", "P[x3(t+1)=0|x1(t),x2(t)]", 
                 "P[x3(t+1)=1|x1(t),x2(t)]", "E[x3(t+1)|x1(t),x2(t)]", 
                 "Prob x E[x3(t+1)|x1(t),x2(t)]")

tab


####### Item b #######
H = sum(tab$`Prob x E[x3(t+1)|x1(t),x2(t)]`)
H
