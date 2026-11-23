
# Importação de arquivos CSV

bdem <- read.csv("BDEM_2015.csv")
bdem

View(bdem)
dim(bdem)
nrow(bdem)
ncol(bdem)
head(bdem)
tail(bdem)
bdem[1,1]
bdem[1,2]
bdem[1:5,]
bdem[1:5,1:8]
rownames(bdem)
colnames(bdem)
names(bdem)

head(bdem[,c("ANO","NIVEL","CODMUNRES","POPRC_T","POPRC_F","TPV","TPC","IDHM_CA","TRP_BP","TO_NT")])
tail(bdem[,c("ANO","NIVEL","CODMUNRES","POPRC_T","POPRC_F","TPV","TPC","IDHM_CA","TRP_BP","TO_NT")])

base = bdem[,c("ANO","NIVEL","CODMUNRES","POPRC_T","POPRC_F","TPV","TPC","IDHM_CA","TRP_BP","TO_NT")]

dim(base)
nrow(base)
ncol(base)
head(base)
tail(base)
names(base)
str(base)

#acessando cada variável da base e resolvendo os problemas de tipo de variáveis

#ANO = ano de referência do BDEM
base$ANO
summary(base$ANO)
base$ANO <- NULL
dim(base)
nrow(base)
ncol(base)
head(base)
tail(base)
names(base)
str(base)

#NIVEL = nível da informação: UF ou MUNICIPIO
base$NIVEL
table(base$NIVEL)
class(base$NIVEL)
base$NIVEL <- as.factor(base$NIVEL)
base$NIVEL
table(base$NIVEL)
class(base$NIVEL)
str(base)

#CODMUNRES = Código da UF ou do município de residência segundo IBGE
base$CODMUNRES
class(base$CODMUNRES)
base$CODMUNRES <- as.character(base$CODMUNRES)
class(base$CODMUNRES)
base$CODMUNRES
str(base)

#POPRC_T = população total residente em Censo Demográfico mais recente anterior a ANO
base$POPRC_T
class(base$POPRC_T)
summary(base$POPRC_T)
mean(base$POPRC_T)
mean(base$POPRC_T,na.rm = T)
min(base$POPRC_T,na.rm = T)
max(base$POPRC_T,na.rm = T)
median(base$POPRC_T,na.rm = T)
sd(base$POPRC_T,na.rm = T)
var(base$POPRC_T,na.rm = T)
sqrt(var(base$POPRC_T,na.rm = T))
sd(base$POPRC_T,na.rm = T)^2
str(base)

#POPRC_F = população feminina residente em Censo Demográfico mais recente anterior a ANO
base$POPRC_F
class(base$POPRC_F)
summary(base$POPRC_F)
str(base)

#TPV = total de partos vaginais
base$TPV
class(base$TPV)
summary(base$TPV)
str(base)

#TPC = total de partos cesáreos
base$TPC
class(base$TPV)
summary(base$TPV)
str(base)

#IDHM_CA = IDHM da população residente em Censo Demográfico mais recente anterior a ANO
base$IDHM_CA
class(base$IDHM_CA)
summary(base$IDHM_CA)
str(base)

#TRP_BP = total de recém-nascidos com baixo peso, peso < 2.500 g
base$TRP_BP
class(base$TRP_BP)
summary(base$TRP_BP)
str(base)

#TO_NT = total de óbitos neonatais (0 ≤ idade ≤ 27 dias)
base$TO_NT
class(base$TO_NT)
summary(base$TO_NT)
str(base)
summary(base)

#Criar novas variáveis a partir das existentes na base

#Vamos criar uma variável definida pela proporção de partos vaginais entre todos os partos: vaginais e cesáreos
#TPC = total de partos cesáreos
#TPV = total de partos vaginais
TOT_P = base$TPC + base$TPV
TOT_P = ifelse(is.na(base$TPV) | is.na(base$TPC), NA, base$TPC + base$TPV)


PROP_PV = base$TPV/TOT_P
PROP_PC = base$TPC/TOT_P

#vamos agora incluir essas novas variáveis na base
base$TOT_P = TOT_P
str(base)
summary(base)

base$PROP_PV = PROP_PV
summary(base)

base$PROP_PC = PROP_PC
summary(base)

#Vamos criar uma variável definida pela proporção do num de 
#RN bvaixo peso pelo num de partos
PROP_BP = base$TRP_BP/base$TOT_P
base$PROP_BP = PROP_BP
summary(base)


#Vamos criar uma variável definida pela proporção do obitos NN
#pelo num de partos
PROP_ONT = base$TO_NT/base$TOT_P
base$PROP_ONT = PROP_ONT
summary(base)


#como pegar somente algumas linhas da base? 

#Se a gente quiser ver uma linha específica podemos 
base[1,]
base[1124,]
base[base$NIVEL == "UF",]

#Como criar uma nova base somente com os estados
base_UF = base[base$NIVEL == "UF", ]
base_UF
#OUTRA FORMA
base_UF = subset(base, NIVEL == "UF")
base_UF

#Como criar uma nova base somente com os municipios
base_MUN = base[base$NIVEL == "MUNICIPIO", ]
base_MUN
#OUTRA FORMA
base_MUN = subset(base, NIVEL == "MUNICIPIO")
base_MUN

#seria interessante na base dos UF aparecer o nome do UF 
#vamos pegar essa informação em outra base
cod_UF <- read.csv("codigos_UF.csv")
class(cod_UF)
head(cod_UF)
tail(cod_UF)


#para juntar duas bases que podem estar com as linhas em prdens diferentes, 
#vamos precisar da função merge
?merge
#o ideal é que as duas bases estejam com as colunas nomeadas de forma padronizada
names(base_UF)
names(cod_UF)
names(cod_UF) = c("CODMUNRES","UF","REGIAO")
names(cod_UF)

merge(base_UF, cod_UF, by = "CODMUNRES")
base_UF <- merge(base_UF, cod_UF, by = "CODMUNRES")
dim(base_UF)
str(base_UF)
summary(base_UF)
head(base_UF)

#vamos fazer a variavel regiao virar factor, porque ela é uma categórica
base_UF$REGIAO = as.factor(base_UF$REGIAO)

summary(base_UF)
#vamos tirar desta base as variáveis constantes
base_UF$NIVEL = NULL





#seria interessante na base dos MUNICIPIOS aparecer o nome do UF e o nome do Municipio
#vamos pegar essa informação em outra base
cod_municipios <- read.csv("municipios_ibge.csv")
class(cod_municipios)
head(cod_municipios)
tail(cod_municipios)

#vamos precisar da função merge de novo
#o ideal é que as duas bases estejam com as colunas nomeadas de forma padronizada
names(base_MUN)
names(cod_municipios)
names(cod_municipios) = c("CODMUNRES","UF","MUNICÍPIO","REGIAO")
names(cod_municipios)

merge(base_MUN, cod_municipios, by = "CODMUNRES")
base_MUN <- merge(base_MUN, cod_municipios, by = "CODMUNRES")
dim(base_MUN)
str(base_MUN)
summary(base_MUN)
head(base_MUN)

#vamos fazer a variavel regiao virar factor, porque ela é uma categórica
base_MUN$REGIAO = as.factor(base_MUN$REGIAO)
#vamos fazer a variavel UF virar factor, porque ela nessa base é categórica
base_MUN$UF = as.factor(base_MUN$UF)

summary(base_MUN)
#vamos tirar desta base as variáveis constantes
base_MUN$NIVEL = NULL


#agora vamos fazer uma terceira base somente com os municípios de MT
base_MUN[base_MUN$UF=="Mato Grosso",]
base_MT = base_MUN[base_MUN$UF=="Mato Grosso",]
dim(base_MT)
#OUTRA FORMA
base_MT = subset(base_MUN,UF=="Mato Grosso")
dim(base_MT)

#agora vamos fazer uma terceira base somente com os municípios da regiao centro-oeste
#Duas maneiras diferentes
#Primeira
base_MUN[base_MUN$REGIAO=="Centro-Oeste",]
base_CO = base_MUN[base_MUN$REGIAO=="Centro-Oeste",]
#ou
base_CO = subset(base_MUN,REGIAO=="Centro-Oeste")
dim(base_CO)
head(base_CO)
tail(base_CO)

#como reorganizar as linhas da base a partir de uma coluna específica?
#por ordem de UF
base_MUN = base_MUN[order(base_MUN$UF), ]
head(base_MUN)
tail(base_MUN)
#por ordem de regiao
base_MUN = base_MUN[order(base_MUN$REGIAO), ]
head(base_MUN)
tail(base_MUN)
#por ordem de população
base_MUN = base_MUN[order(base_MUN$POPRC_T), ]
head(base_MUN)
tail(base_MUN)
#por ordem de população - da maior para a menor
base_MUN = base_MUN[order(base_MUN$POPRC_T,decreasing = T), ]
head(base_MUN)
tail(base_MUN)




#temos 4 bases: base_UF, base_MUN , base_MT, base_CO
head(base_UF)
head(base_MUN)
head(base_MT)
head(base_CO)


# Vamos salvar essas bases para que o trabalho todo realizado não precise ser 
# reprodzido em outro momento. 

write.csv(base_UF,"base_UF.csv")
write.csv(base_MUN,"base_MUN.csv")
write.csv(base_MT,"base_MT.csv")
write.csv(base_CO,"base_CO.csv")

