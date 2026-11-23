#Vamos começar com a leitura das bases salvas

base_UF = read.csv("base_UF.csv")
base_MUN = read.csv("base_MUN.csv")
base_MT = read.csv("base_MT.csv")
base_CO = read.csv("base_CO.csv")

#quado lemos uma base csv as variáveis podem não estar no seu tipo corrteto
str(base_UF)
base_UF$REGIAO = as.factor(base_UF$REGIAO)

str(base_MUN)
base_MUN$UF = as.factor(base_MUN$UF)
base_MUN$REGIAO = as.factor(base_MUN$REGIAO)

str(base_MT)
base_MT$UF = as.factor(base_MT$UF)

str(base_CO)
base_CO$UF = as.factor(base_CO$UF)
base_CO$REGIAO = as.factor(base_CO$REGIAO)


############################
### Gráficos
############################

#Gráficos para variáveis categóricas:
#UF        
#REGIAO    

#gráfico de barras
table(base_MUN$UF)
tab = table(base_MUN$UF)
tab
barplot(tab)
#vamos enfeitar
barplot(tab,
        main = "Quantidade de municípios por UF",
        col =  "orange",
        las = 2
)
#precisamos aumentar a margem
#valores padrao
#mar = c(5.1, 4.1, 4.1, 2.1))
#mgp = c(3, 1, 0)
par(mar = c(10, 4, 4, 2))
barplot(tab,
        main = "Quantidade de municípios por UF",
        col =  "orange",
        las = 2
)
#podemos aumentar a fonte
barplot(tab,
        main = "Quantidade de municípios por UF",
        col =  "orange",
        las = 2, 
        cex.main = 2, 
        cex.lab = 1.5,
        cex.axis = 1.5
)
barplot(tab,
        "Quantidade de municípios por UF",
        col =  "orange",
        las = 2, 
        cex.main = 2, 
        cex.lab = 1.5,
        cex.axis = 1.5, 
        cex.names = 1.5
)
par(mar = c(13, 4, 4, 2))
barplot(tab,
        main = "Quantidade de municípios por UF",
        col =  "orange",
        las = 2, 
        cex.main = 2, 
        cex.lab = 1.5,
        cex.axis = 1.5, 
        cex.names = 1.5
)
barplot(tab,
        main = "Quantidade de municípios por UF",
        col =  "orange",
        las = 2, 
        cex.main = 2, 
        cex.lab = 1.5,
        cex.axis = 1.5, 
        cex.names = 1.5, 
        ylim = c(0,1000)
)


#Replicar para a base base_CO: quantidade de municípios por UF
#Replicar para a base base_UF: quantidade de UF por região 
#Replicar para a base base_MUN: quantidade de municípios por região

#voltar as margens para o padrão
par(mar = c(5.1, 4.1, 4.1, 2.1),
    mgp = c(3, 1, 0))


#Gráfico de barras empílhadas
base_UF$PROP_PV
base_UF$PROP_PC

tab = base_UF[,c("PROP_PV","PROP_PC")]
tab
barplot(t(tab))
barplot(t(tab),
        main = "",
        ylab = "",
        col = c("darkblue","darkgreen"),
        names.arg = base_UF$UF,
        las=3)

par(mar = c(10, 4.1, 4.1, 2.1),
    mgp = c(3, 1, 0))

barplot(t(tab),
        main = "Proporção de cada tipo de parto por UF",
        ylab = "quantidade de municípios",
        col = c("darkblue","darkgreen"),
        names.arg = base_UF$UF,
        legend.text = c("vaginais","cesareos"),
        las=3)
abline(h=0.5,lty=2,col="gray")

#Replicar para a base base_CO
#Replicar para a base base_UF
#Replicar para a base base_MUN




#voltar as margens para o padrão
par(mar = c(5.1, 4.1, 4.1, 2.1),
    mgp = c(3, 1, 0))

#Histograma

hist(base_MUN$IDHM_CA)
hist(base_MUN$IDHM_CA,
     main = "Histograma do IDHM da população residente em Censo Demográfico",
     xlab = "IDH",
     ylab = "Frequência - quantidade de municípios")

#replicar para as outras bases

base_MUN$TRP_BP
hist(base_MUN$TRP_BP)

#para o gráfico ficar melhor , vamos considerar os municipios com menos 
#de 5.000 habitantes
base_MUN_peq = base_MUN[base_MUN$POPRC_T<5235,]

base_MUN_peq$TRP_BP
hist(base_MUN_peq$TRP_BP)
hist(base_MUN_peq$TRP_BP,
     main = "Histograma do total de nascimentos baixo peso",
     xlab = "Nº de nascimentos com baixo peso",
     ylab = "Frequência - quantidade de municípios")

#como seria para os monicípios de tamanho médio, com pop entre 7000 e 18000
base_MUN_med = base_MUN[base_MUN$POPRC_T<10934 & base_MUN$POPRC_T>=5235,]
hist(base_MUN_med$TRP_BP,
     main = "Histograma do total de nascimentos baixo peso",
     xlab = "Nº de nascimentos com baixo peso",
     ylab = "Frequência - quantidade de municípios")

#replicar para os monicipio grandes, com pop entre 10934 e 23424 



# Boxplot
boxplot(base_MUN$IDHM_CA)
boxplot(base_MUN$IDHM_CA,
        main="IDHM da população residente em Censo Demográfico")

boxplot(base_MUN$IDHM_CA ~ base_MUN$REGIAO,
        main="IDHM da população residente em Censo Demográfico por região",
        xlab = "Regiões",
        ylab = "IDH")


boxplot(base_MUN_peq$TPV)
boxplot(base_MUN_peq$TPV,
        main = "Total de partos vaginais nos municípios pequenos",
        col="darkgreen")

boxplot(base_MUN_peq$TPC,
        main = "Total de partos cesáreos nos municípios pequenos",
        col="darkblue")

#vamos colocar os dois juntos para comparacao
boxplot(base_MUN_peq$TPV,base_MUN_peq$TPC,
        main = "Total de partos vaginais e cesários nos municípios pequenos",
        col=c("darkgreen","darkblue"),
        names = c("vaginais","cesáreos"))

boxplot(base_MUN_med$TPV,base_MUN_med$TPC,
        main = "Total de partos vaginais e cesários nos municípios médios",
        col=c("darkgreen","darkblue"),
        names = c("vaginais","cesáreos"))

#replicar para os monicipio grandes, com pop entre 10934 e 23424 



#Grafico de dispersão

#E se quisermnos analisar a relacao entre duas variáveis quantitativas?
#Poe exemplo, IDH e nascidos com baixo peso, será que existe relação?
plot(x = base_MUN$IDHM_CA,
     y = base_MUN$PROP_BP)
plot(x = base_MUN$IDHM_CA,
     y = base_MUN$PROP_BP,
     main = "Gráfico de dispersão entre o IDH e a proporção de nascimentos baixo peso",
     xlab = "IDH",
     ylab = "proporção dos nascimentos com baixo peso")
#vamos pintar os municípios pelas regiõies?
plot(x = base_MUN$IDHM_CA,
     y = base_MUN$PROP_BP,
     main = "Gráfico de dispersão entre a proporção de nascidos com baixo peso e a proporção de óbitos neo natal",
     xlab = "Proporção de nascidos com baixo peso",
     ylab = "Proporção de óbitos neo natal",
     col="white"
)
points(x = base_MUN$IDHM_CA[base_MUN$REGIAO=="Norte"],
       y = base_MUN$PROP_BP[base_MUN$REGIAO=="Norte"],
       col="tomato",
       pch=3
)
points(x = base_MUN$IDHM_CA[base_MUN$REGIAO=="Sudeste"],
       y = base_MUN$PROP_BP[base_MUN$REGIAO=="Sudeste"],
       col="skyblue",
       pch=3
)
points(x = base_MUN$IDHM_CA[base_MUN$REGIAO=="Nordeste"],
       y = base_MUN$PROP_BP[base_MUN$REGIAO=="Nordeste"],
       col="pink",
       pch=3
)
points(x = base_MUN$IDHM_CA[base_MUN$REGIAO=="Centro-Oeste"],
       y = base_MUN$PROP_BP[base_MUN$REGIAO=="Centro-Oeste"],
       col="gold",
       pch=3
)
points(x = base_MUN$IDHM_CA[base_MUN$REGIAO=="Sul"],
       y = base_MUN$PROP_BP[base_MUN$REGIAO=="Sul"],
       col="springgreen",
       pch=3
)
#qual é o estado representado pelo + mais à esquerda
which.min(base_MUN$IDHM_CA)
base_MUN[1298,]

#qual é o estado representado pelo + mais à direita
which.max(base_MUN$IDHM_CA)
base_MUN[178,]

#outro grafico de dispersao
#entre total de baixo peso e total de obitos
plot(x = base_MUN$PROP_BP,
     y = base_MUN$PROP_ONT,
     main = "Gráfico de dispersão entre a proporção de nascidos com baixo peso e a proporção de óbitos neo natal",
     xlab = "Proporção de nascidos com baixo peso",
     ylab = "Proporção de óbitos neo natal",
     pch=19)
#vamos pintar os municípios pelas regiõies?
plot(x = base_MUN$PROP_BP,
     y = base_MUN$PROP_ONT,
     main = "Gráfico de dispersão entre a proporção de nascidos com baixo peso e a proporção de óbitos neo natal",
     xlab = "Proporção de nascidos com baixo peso",
     ylab = "Proporção de óbitos neo natal",
     col="white"
     )
points(x = base_MUN$PROP_BP[base_MUN$REGIAO=="Norte"],
       y = base_MUN$PROP_ONT[base_MUN$REGIAO=="Norte"],
       col="tomato",
       pch=3
       )
points(x = base_MUN$PROP_BP[base_MUN$REGIAO=="Sudeste"],
       y = base_MUN$PROP_ONT[base_MUN$REGIAO=="Sudeste"],
       col="skyblue",
       pch=3
)
points(x = base_MUN$PROP_BP[base_MUN$REGIAO=="Nordeste"],
       y = base_MUN$PROP_ONT[base_MUN$REGIAO=="Nordeste"],
       col="pink",
       pch=3
)
points(x = base_MUN$PROP_BP[base_MUN$REGIAO=="Centro-Oeste"],
       y = base_MUN$PROP_ONT[base_MUN$REGIAO=="Centro-Oeste"],
       col="gold",
       pch=3
)
points(x = base_MUN$PROP_BP[base_MUN$REGIAO=="Sul"],
       y = base_MUN$PROP_ONT[base_MUN$REGIAO=="Sul"],
       col="springgreen",
       pch=3
)
#qual é o estado representado pelo pontinho mais alto?
which.max(base_MUN$PROP_ONT)
base_MUN[5538,]
