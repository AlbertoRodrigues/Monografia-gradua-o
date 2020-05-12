require(MASS)
#Importação dos dados

#Deu certo
#Dados Iris
iris=datasets::iris
y=iris$Sepal.Length
#colnames(iris)
x=iris[,c("Sepal.Width","Petal.Length","Petal.Width","Species")]

#Deu certo
#Dados mpg
data=read.csv(file.choose())
x=data[,-c(1,7)]
y=data$X5 

#Deu certo
#Dados Slump test
data=read.csv(file.choose())
y=data$Compressive.Strength..28.day..Mpa.
y=as.numeric(as.character(y))
x=data[,-c(1,11)]

#Deu certo para poucas iterações, banco de dados grande.
#Dados parkinson
data=read.csv(file.choose())
#summary(data)
x=data[,-5]
y=data[,5] 

#Em algumas iterações o algoritmo não converge, solução: 
#fazer várias vezes até a quantidade necessária
#Dados preços de casas
data=read.csv(file.choose())
#summary(data)
x=data[,-c(1,2,8)]
y=data[,8]

#Dados discretos
#dados vinho
data=read.csv(file.choose())
x=data[,-12]
y=data[,12] 



#Pré-processamentos
sapply(x,class)
for(i in 1:ncol(x))
{
  if(sapply(x,class)[i]=="integer")
  {
    x[,i]=as.numeric(x[,i])
  }
}
sapply(x,class)


porc_validacao=0.4
tipo_resposta="continuo"
#Hiperparâmetros iniciais
num_minimo_terminal=12
num_minimo_interno=15
prof_max=3
porc_min=0.4
tipo_poda=2
porc_modelos=0.3
num_minimo_modelos=3
num_min_obs_test_var=10
  

  



#Realizando a partição 
#função dos indices da partição de treino  
x_treino=x
y_treino=y

#Fazendo validação se necessário
if(tipo_poda==1)
{
  amostra2=sample(1:nrow(x_treino),ceiling(nrow(x_treino)*porc_validacao),replace=F)
  x_validacao =x_treino[amostra2,]
  y_validacao =y_treino[amostra2]
  x_treino =x_treino[-amostra2,]
  y_treino =y_treino[-amostra2]
}

#vetor_ind_obs=list()
#Função que faz todas as medias consecutivas da variavel explicativa numerica
retorna_medias_var_numerica=function(x)
{
  #Seleciona os únicos e ordena
  x=sort(unique(x))
  #Vetor de médias
  medias=c()
  for(i in 1:length(x))
  {
    if(i!=length(x))
    {
      medias=append(medias,(x[i]+x[i+1])/2)
    }
  }
  return(medias)
}

#Funcao que faz o melhor separador para variavel explicativa numerica,
#ela será usada em cada nó da árvore, em quanto outra função chamará ela em todos os nós.
#Observação: nossa algoritmo tem que existir pelo menos uma variável numérica,
#ou temos que pensar melhor nessa função.

separador_subconjuntos=function(x,y){
  separador=sd(y)
  #Valor separador
  separador2=0
  #Indice da coluna do melhor separador
  coluna_var_separadora=3
  #Contador
  contador=0
  #Vetor de tipos das variáveis
  a=sapply(x,class)
  #Posição de alguma coluna numérica se o subconjunto não tiver separador "bom",
  #ou seja, o caso em que nenhum separador melhor o ganho em desvio-padrão. 
  a=which(a=="numeric")[1]
  
  for(i in 1:ncol(x))
  {
    if(class(x[,i])=="numeric")
    {
      #Chamando a função que dá as médias ordenadas
      medias_separadoras=retorna_medias_var_numerica(x[,i])
      #print(medias_separadoras)
      for(j in 1:length(medias_separadoras))
      {
        #Posicoes que atendem a condicao
        posicoes_obs_atendidas=which(x[,i]<=medias_separadoras[j])
        #Posicoes que não atendem a condicao
        posicoes_obs_nao_atendidas=setdiff(1:length(x[,i]),posicoes_obs_atendidas)
        #Calculando todos os desvios-padroes ponderados e verificando se é melhor 
        #trocar para esse separador
        if(length(posicoes_obs_atendidas)>num_min_obs_test_var & 
           length(posicoes_obs_nao_atendidas)>num_min_obs_test_var)
        {
          desvio_padrao_ponderado=((length(posicoes_obs_atendidas)/length(x[,i]))*
                                     sd(y[posicoes_obs_atendidas])+(length(posicoes_obs_nao_atendidas)/length(x[,i]))*
                                     sd(y[posicoes_obs_nao_atendidas]))
          
          if(desvio_padrao_ponderado < separador)
          {
            contador=contador+1
            separador=desvio_padrao_ponderado
            separador2=medias_separadoras[j]
            coluna_var_separadora=i
          }
        }
      }
      #Verificação se não existe redução do desvio-padrão da variável resposta
      if(contador==0)
      {
        #Pondo um valor do separador extremamente específico para que identifiquemos
        #partições inutéis.
        separador2=123456789
        #Posição da primeira variável numérica encontrada, usada também para
        #identificar
        coluna_var_separadora=a
      }
    }
  }
  #Retorna o valor separador e o nome da variável que é do melhor separador.
  return(list(separador2,colnames(x)[coluna_var_separadora]))
}


#Funcao de construcao da arvore, retorna uma lista com todas as observacoes em cada
#nó da árvore
#Chama as funções separador_subconjuntos, que chama a função retorna_medias_var_numerica
#para nó, e assim construir a primeira versão da árvore completa, com cada observação
#em seu respectivo nó.

constroi_arvore=function(x,y)
{
  #Listas para receber o separador e variável separadora
  lista_temporaria2=c()
  lista_temporaria3=c()
  #Indice de todas as observações
  ind_obs=list(1:length(y))
  #Realizando as partições com a profundidade máxima
  for(i in 1:(2^(prof_max)-1))
  {
    #Contando com o fato de que existe pelo menos um separador
    if(i>=2)
    {
      if(length(ind_obs[[floor(i/2)]])>1)
      {
        lista_temporaria=separador_subconjuntos(x[ind_obs[[i]],],y[ind_obs[[i]]])
        #print(lista_temporaria)
        lista_temporaria2[i]=lista_temporaria[[1]]
        lista_temporaria3[i]=lista_temporaria[[2]]
        #Observações do filho esquerdo
        pos_lado_e=(ind_obs[[i]][which(x[ind_obs[[i]],lista_temporaria[[2]]]
                                       <=lista_temporaria[[1]])])
        #Observações do filho direito
        pos_lado_d=(ind_obs[[i]][which(x[ind_obs[[i]],lista_temporaria[[2]]]
                                       >lista_temporaria[[1]])])
        #Condições dos hiperparâmetros para filhos não-folhas
        if((2*i<=(2^(prof_max)-1)) & length(pos_lado_d)>=num_minimo_interno &
           length(pos_lado_e)>=num_minimo_interno & sd(y[ind_obs[[i]]])>=sd(y)*porc_min
           & length(pos_lado_d)>0 & length(pos_lado_e)>0)
        { 
          ind_obs[[2*i]]=pos_lado_e
          ind_obs[[2*i+1]]=pos_lado_d
        }
        else
        {
          #Condições dos hiperparâmetros para filhos folhas
          if((2*i>(2^(prof_max)-1)) & length(pos_lado_d)>=num_minimo_terminal &
             length(pos_lado_e)>=num_minimo_terminal & sd(y[ind_obs[[i]]])>=
             sd(y)*porc_min & length(pos_lado_d)>0 & length(pos_lado_e)>0)
          {
            ind_obs[[2*i]]=pos_lado_e
            ind_obs[[2*i+1]]=pos_lado_d
          }
          else
          {
            ind_obs[[2*i]]=0
            ind_obs[[2*i+1]]=0   
          }        
        }
      }
      else
      {
        ind_obs[[2*i]]=0
        ind_obs[[2*i+1]]=0 
      }
    }
    else{
      lista_temporaria=separador_subconjuntos(x[ind_obs[[i]],],y[ind_obs[[i]]])
      #print(lista_temporaria)
      lista_temporaria2[i]=lista_temporaria[[1]]
      lista_temporaria3[i]=lista_temporaria[[2]]
      pos_lado_e=(ind_obs[[i]][which(x[ind_obs[[i]],lista_temporaria[[2]]]
                                     <=lista_temporaria[[1]])])
      pos_lado_d=(ind_obs[[i]][which(x[ind_obs[[i]],lista_temporaria[[2]]]
                                     >lista_temporaria[[1]])])
      ind_obs[[2*i]]=pos_lado_e
      ind_obs[[2*i+1]]=pos_lado_d
    }
  }
  # print(ind_obs)
  return(list(ind_obs,lista_temporaria2,lista_temporaria3))  
}

arvore1=constroi_arvore(x_treino,y_treino)
arvore=arvore1[[1]];arvore
data_frame_sep=cbind.data.frame(arvore1[[2]],arvore1[[3]])
colnames(data_frame_sep)=c("Valores Separadores","Variáveis Separadoras")
#data_frame_sep

#x$Post.Month
dados=cbind.data.frame(x_treino,y_treino)
#head(dados)
coef_modelo=list()
modelos_links=c()
modelos_dist=c()
indices_modelos_retirados=c()
erro_menor=rep(-1,length(arvore))
melhores_modelos=list()
if(tipo_resposta=="continuo")
{
  indices_modelos=1:9
  for(k in 1:length(arvore))
  {
    if(length(arvore[[k]])>1)
    {
      colunas_inuteis=c()
      dados2=dados[arvore[[k]],]
      for(j in 1:ncol(dados2))
      {
        if(class(dados2[1,j])=="factor" & length(unique(dados2[,j]))==1)
        {
          colunas_inuteis=append(colunas_inuteis,j)
        }
      }
      
      if(length(colunas_inuteis)>0)
      {
        dados2=dados2[,-colunas_inuteis]
      }
      
      modelos=list(7)
      
      erros_temporarios=c()
      indices_modelos=setdiff(indices_modelos,indices_modelos_retirados)
      #print(indices_modelos)
      for(p in indices_modelos)
      {
        if(p==1)
        {
          modelo1=glm(y_treino~.,data=dados2,family=gaussian(link="identity"))
          modelos[[1]]=modelo1
        }else if(p==2)
        {
          modelo3=glm(y_treino~.,data=dados2,family=gaussian(link="inverse"))
          modelos[[2]]=modelo3
        }else if(p==3)
        {
          modelo4=glm(y_treino~.,data=dados2,family=gaussian(link="log"))
          modelos[[3]]=modelo4
        }else if(p==4)
        {
          modelo2=glm(y_treino~.,data=dados2,family=Gamma(link="identity"))
          modelos[[4]]=modelo2
        }else if(p==5)
        {
          modelo2=glm(y_treino~.,data=dados2,family=Gamma(link="inverse"))
          modelos[[5]]=modelo2
        }else if(p==6)
        {
          modelo2=glm(y_treino~.,data=dados2,family=Gamma(link="log"))
          modelos[[6]]=modelo2
        }else if(p==7)
        {
          modelo2=glm(y_treino~.,data=dados2,family=inverse.gaussian(link="identity"))
          modelos[[7]]=modelo2
        }else if(p==8)
        {
          modelo2=glm(y_treino~.,data=dados2,family=inverse.gaussian(link="inverse"))
          modelos[[8]]=modelo2
        }else if(p==9){
          modelo2=glm(y_treino~.,data=dados2,family=inverse.gaussian(link="log"))
          modelos[[9]]=modelo2
        }
        #else if(p==8)
        #{
        # modelo2=glm(y_treino~.,data=dados2,family=Gamma(link="1/mu^2"))
        #  modelos[[8]]=modelo2
        #}
        #else if(p==4)
        #{
        # modelo2=glm(y_treino~.,data=dados2,family=gaussian(link="1/mu^2"))
        #  modelos[[4]]=modelo2
        #}
        #else if(p==9)
        #{ 
        # modelo2=glm(y_treino~.,data=dados2,family=gaussian(link="sqrt"))
        #modelos[[9]]=modelo2
        #}else if(p==10)
        #{
        # modelo2=glm(y_treino~.,data=dados2,family=Gamma(link="sqrt"))
        #modelos[[10]]=modelo2
        #}
        #else if(p==13)
        #{
        # modelo2=glm(y_treino~.,data=dados2,family=inverse.gaussian(link="sqrt"))
        #modelos[[13]]=modelo2
        #}else if(p==14)
        #else if(p==15)
        #{
        # modelo2=glm(y_treino~.,data=dados2,family=inverse.gaussian(link="1/mu^2"))
        #modelos[[15]]=modelo2
        #}
      }
      #print(length(indices_modelos))
      erro_menor[k]=1000000
      #15 porque é o tamanho total de modelos
      for(i in 1:9)
      {
        if(is.element(i,indices_modelos))
        {
          #print(i)
          erro=mean(abs(y_treino[arvore[[k]]]-modelos[[i]]$fitted.values))
          erros_temporarios[i]=erro
          if(erro<erro_menor[k])
          {
            melhores_modelos[[k]]=modelos[[i]]
            coef_modelo[[k]]=modelos[[i]]$coefficients
            modelos_links[k]=modelos[[i]]$family$link
            modelos_dist[k]=modelos[[i]]$family$family
            erro_menor[k]=erro
            #print(erro_max)
            #print(k)
            #print(i)
            #print(modelos[[i]]$coefficients)
            #print(coef_modelo)
          }
        }
        else
        {
          erros_temporarios[i]=NA 
        }
      }
      ind=length(which(is.na(erros_temporarios)==TRUE))
      erros_ordenados=order(erros_temporarios)
      #print(erros_temporarios)
      #print(erros_ordenados)
      #(print(ceiling((length(
      # erros_ordenados)-ind)*porc_modelos):(length(erros_ordenados)-ind)))
      (indices_modelos_retirados=erros_ordenados[(ceiling((length(
        erros_ordenados)-ind)*porc_modelos)):(length(erros_ordenados)-ind)])
      
      #(indices_modelos_retirados=erros_ordenados[(ceiling(length(
      # erros_ordenados)*porc_modelos)-ind):(length(erros_ordenados)-ind)])
      
      #print(erros_temporarios)
      #print(indices_modelos_retirados)
    }
    else
    {
      melhores_modelos[[k]]=0
      coef_modelo[[k]]=0
      modelos_links[k]="nao_existe"
      modelos_dist[k]="nao_existe"
    }
    if(length(setdiff(indices_modelos,indices_modelos_retirados))<num_minimo_modelos)
    {
      indices_modelos_retirados=c()
    }
  }
}else{
  indices_modelos=1:4
  for(k in 1:length(arvore))
  {
    if(length(arvore[[k]])>1)
    {
      colunas_inuteis=c()
      dados2=dados[arvore[[k]],]
      for(j in 1:ncol(dados2))
      {
        if(class(dados2[1,j])=="factor" & length(unique(dados2[,j]))==1)
        {
          colunas_inuteis=append(colunas_inuteis,j)
        }
      }
      
      if(length(colunas_inuteis)>0)
      {
        dados2=dados2[,-colunas_inuteis]
      }
      
      modelos=list(7)
      #Tentar depois
      #a=gaussian(link=identity)
      
      erros_temporarios=c()
      indices_modelos=setdiff(indices_modelos,indices_modelos_retirados)
      #print(indices_modelos)
      for(p in indices_modelos)
      {
        if(p==1)
        {
          modelo1=glm(y_treino~.,data=dados2,family=poisson(link="identity"))
          modelos[[1]]=modelo1
        }else if(p==2)
        { 
          modelo2=glm(y_treino~.,data=dados2,family=poisson(link="inverse"))
          modelos[[2]]=modelo2
        }else if(p==3)
        {
          modelo2=glm.nb(y_treino~.,data=dados2,link="identity")
          modelos[[3]]=modelo2
        }else if(p==4)
        {
          modelo2=glm.nb(y_treino~.,data=dados2,link="inverse")
          modelos[[4]]=modelo2
        }
        #else if(p==6)
        #{
        # modelo2=glm.nb(y_treino~.,data=dados2,link="log")
        #modelos[[6]]=modelo2
        #}
        #else if(p==3)
        #{
        # modelo3=glm(y_treino~.,data=dados2,family=poisson(link="log"))
        #modelos[[3]]=modelo3
        #}
      }
      #print(length(indices_modelos))
      erro_menor[k]=1000000
      #6 porque é o tamanho total de modelos
      for(i in 1:4)
      {
        if(is.element(i,indices_modelos))
        {
          #print(i)
          erro=mean(abs(y_treino[arvore[[k]]]-modelos[[i]]$fitted.values))
          erros_temporarios[i]=erro
          if(erro<erro_menor[k])
          {
            melhores_modelos[[k]]=modelos[[i]]
            coef_modelo[[k]]=modelos[[i]]$coefficients
            modelos_links[k]=modelos[[i]]$family$link
            modelos_dist[k]=modelos[[i]]$family$family
            erro_menor[k]=erro
            #print(erro_max)
            #print(k)
            #print(i)
            #print(modelos[[i]]$coefficients)
            #print(coef_modelo)
          }
        }
        else
        {
          erros_temporarios[i]=NA 
        }
      }
      ind=length(which(is.na(erros_temporarios)==TRUE))
      erros_ordenados=order(erros_temporarios)
      (indices_modelos_retirados=erros_ordenados[(ceiling((length(
        erros_ordenados)-ind)*porc_modelos)):(length(erros_ordenados)-ind)])
      #print(erros_ordenados)
      #print(erros_temporarios)
      #print(indices_modelos_retirados)
    }
    else
    {
      melhores_modelos[[k]]=0
      coef_modelo[[k]]=0
      modelos_links[k]="nao_existe"
      modelos_dist[k]="nao_existe"
    }
    if(length(setdiff(indices_modelos,indices_modelos_retirados))<num_minimo_modelos)
    {
      indices_modelos_retirados=c()
    }
    #print(modelos_links)
    #print(modelos_dist)
  }
}
#arvore
#melhores_modelos
#erro_menor
#arvore
#coef_modelo
##modelos_links
#modelos_dist

data_frame_sep
#REVER!!-Importante
#Função que diz onde qual o índice do subconjunto da observação na árvore.
predict=function(x,i)
{
  if(data_frame_sep[2*i,1]!=123456789 && data_frame_sep[2*i+1,1]!=123456789 
     && 2*i<2^prof_max && 2*i+1<2^prof_max)
  {
    if(x[which(colnames(x)==data_frame_sep[i,2])]<=data_frame_sep[i,1])
    {
      return(predict(x,2*i))
    }
    else
    {
      return(predict(x,2*i+1))
    }
  }else if(data_frame_sep[i,1]!=123456789 && 2*i>=2^prof_max && arvore[[2*i]]!=0)
  {
    if(x[which(colnames(x)==data_frame_sep[i,2])]<=data_frame_sep[i,1])
    {
      return(2*i)
    }
    else
    {
      return(2*i+1)
    }
  }
  else
  {
    return(i)
  }
}

#Teste
#a=which(x_teste$Petal.Length<=4.25 & x_teste$Petal.Length>3.4);a
#for(i in a)
#{
#  print(predict(x_teste[i,],1))  
#}
data_frame_sep
#Função que prediz o valor da observação
predict2=function(x)
{
  return(predict.glm(melhores_modelos[[predict(x,1)]],x,type="response"))
}

#Previsoes
predict3=function(x)
{
  valores_preditos=c()
  for(i in 1:nrow(x))
  {
    valores_preditos=append(valores_preditos,predict2(x[i,]))
  }
  return(valores_preditos)
}
#Teste
#predict3(x_teste)
erro_medio_absoluto=function(y_real,y_pred)
{
  return(mean(abs(y_real-y_pred)))
}
#erro_medio_absoluto(y_teste,predict3(x_teste))
#Resultados
#cbind(predict3(x_teste),y_teste)




if(tipo_poda==1)
{
  #Função que fiz o subconjunto(folha) final do conjunto de validação
  subconjunto_folha=c()
  for(i in 1:nrow(x_validacao))
  {
    subconjunto_folha=append(subconjunto_folha,predict(x_validacao[i,],1))
  }
}
#subconjunto_folha


#Função que fiz em qual profundidade da árvore a observação está
#Refazer
if(tipo_poda==1)
{
  niveis_prof=c(1,3,7,15)
  prof_validacao=c()
  for(i in 1:nrow(x_validacao))
  {
    for(j in 1:(length(niveis_prof)-1))
    {
      if(subconjunto_folha[i]>niveis_prof[j] && subconjunto_folha[i]<=niveis_prof[j+1])
      {
        prof_validacao[i]=j
      }
    }
  }
}
#Teste
#cbind(prof_validacao,subconjunto_folha)

if(tipo_poda==1)
{
  lista_poda1=list()
  for(i in 1:nrow(x_validacao))
  {
    vetor_temporario=c()
    for(j in 0:prof_validacao[i])
    {
      vetor_temporario=append(vetor_temporario,floor(subconjunto_folha[i]/(2^j)))
    }
    lista_poda1[[i]]=vetor_temporario
  }
}

#lista_poda1
#subconjunto_folha
#arvore
#melhores_modelos
if(tipo_poda==1)
{
  for(i in seq((2^prof_max)-1,1,-1))
  {
    if(arvore[[i]]!=0)
    {
      pos_filho_direito=c()
      pos_filho_esquerdo=c()
      for(k in 1:length(lista_poda1))
      {
        if(is.element(2*i,lista_poda1[[k]]))
        {
          pos_filho_esquerdo=append(pos_filho_direito,k)
        }else if(is.element(2*i+1,lista_poda1[[k]]))
        {
          pos_filho_direito=append(pos_filho_direito,k)
        }
      }
      #print(pos_filho_esquerdo)
      #print(length(pos_filho_esquerdo))
      #print(pos_filho_direito)
      #print(length(pos_filho_direito))
      #erro_filho_direito
      if(length(pos_filho_esquerdo)>0 && length(pos_filho_direito)>0)
      {
        num_obs_pai=length(pos_filho_esquerdo)+length(pos_filho_direito)
        (erro_filho_direito=(length(pos_filho_direito)/num_obs_pai)*
            erro_medio_absoluto(y_validacao[pos_filho_direito],predict3(x_validacao[pos_filho_direito,])))
        
        (erro_filho_esquerdo=(length(pos_filho_esquerdo)/num_obs_pai)*
            erro_medio_absoluto(y_validacao[pos_filho_esquerdo],predict3(x_validacao[pos_filho_esquerdo,])))
        
        (erro_pai=erro_medio_absoluto(y_validacao[union(pos_filho_direito,pos_filho_esquerdo)],
                                      predict3(x_validacao[union(pos_filho_direito,pos_filho_esquerdo),])))
        
        if(erro_pai<=(erro_filho_esquerdo+erro_filho_direito))
        {
          #print(erro_pai)
          #print(erro_filho_esquerdo+erro_filho_direito)
          coef_modelo[[2*i+1]]=0
          coef_modelo[[2*i]]=0
          modelos_links[2*i+1]="nao_existe2"
          modelos_links[2*i]="nao_existe2"
          modelos_dist[2*i+1]="nao_existe2"
          modelos_dist[2*i]="nao_existe2"
          arvore[[2*i]]=0
          arvore[[2*i+1]]=0
          erro_menor[2*i]=-1
          erro_menor[2*i+1]=-1
        }
      }
    }
  }
}

#modelos_dist
if(tipo_poda==1)
{
  for(i in seq(1,(2^prof_max)-1,1))
  {
    if(modelos_links[i]=="nao_existe2")
    {
      coef_modelo[[2*i+1]]=0
      coef_modelo[[2*i]]=0
      modelos_links[2*i+1]="nao_existe2"
      modelos_links[2*i]="nao_existe2"
      modelos_dist[2*i+1]="nao_existe2"
      modelos_dist[2*i]="nao_existe2"
      arvore[[2*i]]=0
      arvore[[2*i+1]]=0
      erro_menor[2*i]=-1
      erro_menor[2*i+1]=-1 
    }
  }
}  
#modelos_links
#modelos_dist
#arvore

#Poda com os dados de treinamento
if(tipo_poda==2)
{
  for(i in seq((2^prof_max)-1,1,-1))
  {
    if(erro_menor[i]!=-1 && i!= 1 && erro_menor[2*i]!=-1 && erro_menor[2*i+1]!=-1)
    {
      (erro_filho_direito=(length(arvore[[2*i+1]])/length(arvore[[i]]))*erro_menor[2*i+1]*((
        length(arvore[[2*i+1]])+length(coef_modelo[[2*i+1]]))/(length(arvore[[2*i+1]])-
                                                                 length(coef_modelo[[2*i+1]]))))
      (erro_filho_esquerdo=(length(arvore[[2*i]])/length(arvore[[i]]))*erro_menor[2*i]*((
        length(arvore[[2*i+1]])+length(coef_modelo[[2*i+1]]))/(length(arvore[[2*i+1]])-
                                                                 length(coef_modelo[[2*i+1]]))))
      #print(i)
      #print(erro_menor[i])
      #print(erro_filho_direito+erro_filho_esquerdo)
      if(erro_menor[i]<=(erro_filho_direito+erro_filho_esquerdo))
      {
        coef_modelo[[2*i+1]]=0
        coef_modelo[[2*i]]=0
        modelos_links[2*i+1]="nao_existe2"
        modelos_links[2*i]="nao_existe2"
        modelos_dist[2*i+1]="nao_existe2"
        modelos_dist[2*i]="nao_existe2"
        arvore[[2*i]]=0
        arvore[[2*i+1]]=0
        erro_menor[2*i]=-1
        erro_menor[2*i+1]=-1
      }
    }
  }
}

modelos_links
modelos_dist
melhores_modelos
coef_modelo










require(xtable)
tabela=read.csv(file.choose());tabela
xtable(tabela,digits = 4)
#Validação cruzada

