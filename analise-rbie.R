#---------------------------------
# ANALISE DE DESEMPENHO EM IPC
# 2016 (sem gamificacao) x 2017 (com gamificacao)
#---------------------------------


# Limpa variaveis de ambiente
rm(list=ls())

# Ajusta diretorio de trabalho
setwd("C://Users//llean//Dropbox//pesquisa//__Meus Artigos//Artigos 2019//RBIE//dadosDavid")

#---------------------------------
# CONFIGURACAO DE GRAFICOS
#---------------------------------

# mfrow: sets the plotting area into a arg1 x arg2 array
# mar  : sets the bottom, left, top and right margins respectively
par(mfrow=c(1, 1), mar=c(5, 5, 3, 0.5), las=1)

# fonte size
escala = 1.8

# Box width in boxplots
boxwidth = 0.2

#---------------------------------
# ABRIR ARQUIVOS
#---------------------------------

# read csv file
df_2016 = read.csv2("ipc-consolid-2016.csv", header = TRUE, sep = ";", dec = ",")
attach(df_2016)

df_2017 = read.csv2("ipc-consolid-2017.csv", header = TRUE, sep = ";", dec = ",")
attach(df_2017)

# read csv file
df_2016c = read.csv2("combined2016-1.csv", header = TRUE, sep = ";", dec = ",")
attach(df_2016c)

df_2017c = read.csv2("combined2017-1.csv", header = TRUE, sep = ";", dec = ",")
attach(df_2017c)


#----------------------------------------------------------
# LIMPEZA DOS DADOS
#----------------------------------------------------------

# Convertendo separador de decimal
df_2016$IEM011 <- sub(",", ".", df_2016$IEM011)
df_2016$IEM011 <- as.numeric(df_2016$IEM011)

df_2016$IEM012 <- sub(",", ".", df_2016$IEM012)
df_2016$IEM012 <- as.numeric(df_2016$IEM012)

df_2016$NU_NOTA_L <- sub(",", ".", df_2016$NU_NOTA_L)
df_2016$NU_NOTA_L <- as.numeric(df_2016$NU_NOTA_L)

df_2016$NU_NOTA_CH <- sub(",", ".", df_2016$NU_NOTA_CH)
df_2016$NU_NOTA_CH <- as.numeric(df_2016$NU_NOTA_CH)

df_2016$NU_NOTA_CN <- sub(",", ".", df_2016$NU_NOTA_CN)
df_2016$NU_NOTA_CN <- as.numeric(df_2016$NU_NOTA_CN)

df_2016$NU_NOTA_M <- sub(",", ".", df_2016$NU_NOTA_M)
df_2016$NU_NOTA_M <- as.numeric(df_2016$NU_NOTA_M)

df_2016$NU_NOTA_R <- sub(",", ".", df_2016$NU_NOTA_R)
df_2016$NU_NOTA_R <- as.numeric(df_2016$NU_NOTA_R)

df_2016$MedEnem <- sub(",", ".", df_2016$MedEnem)
df_2016$MedEnem <- as.numeric(df_2016$MedEnem)

df_2016$CR <- sub(",", ".", df_2016$CR)
df_2016$CR <- as.numeric(df_2016$CR)


df_2017$IEM011 <- sub(",", ".", df_2017$IEM011)
df_2017$IEM011 <- as.numeric(df_2017$IEM011)

df_2017$IEM012 <- sub(",", ".", df_2017$IEM012)
df_2017$IEM012 <- as.numeric(df_2017$IEM012)

df_2017$NU_NOTA_L <- sub(",", ".", df_2017$NU_NOTA_L)
df_2017$NU_NOTA_L <- as.numeric(df_2017$NU_NOTA_L)

df_2017$NU_NOTA_CH <- sub(",", ".", df_2017$NU_NOTA_CH)
df_2017$NU_NOTA_CH <- as.numeric(df_2017$NU_NOTA_CH)

df_2017$NU_NOTA_CN <- sub(",", ".", df_2017$NU_NOTA_CN)
df_2017$NU_NOTA_CN <- as.numeric(df_2017$NU_NOTA_CN)

df_2017$NU_NOTA_M <- sub(",", ".", df_2017$NU_NOTA_M)
df_2017$NU_NOTA_M <- as.numeric(df_2017$NU_NOTA_M)

df_2017$NU_NOTA_R <- sub(",", ".", df_2017$NU_NOTA_R)
df_2017$NU_NOTA_R <- as.numeric(df_2017$NU_NOTA_R)

df_2017$MedEnem <- sub(",", ".", df_2017$MedEnem)
df_2017$MedEnem <- as.numeric(df_2017$MedEnem)

df_2017$CR <- sub(",", ".", df_2017$CR)
df_2017$CR <- as.numeric(df_2017$CR)



#----------------------------------------------------------
# Caracterizacao das amostras
#----------------------------------------------------------


# Calcular estatisticas descritivas da turma de 2016/1
NAlunos <- length(df_2016$MATR_ALUNO)
NMasc   <- length(df_2016$MATR_ALUNO[df_2016$SEXO == "M"])
NFem    <- length(df_2016$MATR_ALUNO[df_2016$SEXO == "F"])
NRepet  <- length(df_2016$MATR_ALUNO[df_2016$REPET == 1])
NNRepet <- length(df_2016$MATR_ALUNO[df_2016$REPET == 0])
NExp    <- length(df_2016$MATR_ALUNO[df_2016$EXP_PROG == "yes"])
NNExp   <- length(df_2016$MATR_ALUNO[df_2016$EXP_PROG == "no"])
NExpND  <- length(df_2016$MEDIA_FINAL[df_2016$EXP_PROG == "#N/D"])
NCalour <- length(df_2016$MEDIA_FINAL[df_2016$ANO_INGRESSO == 2016])
NPeriod <- length(df_2016$MEDIA_FINAL[df_2016$PERIODIZADO == "yes"])
NPSC    <- length(df_2016$MEDIA_FINAL[df_2016$PSELETIVO == "PSC"])
NSISU   <- length(df_2016$MEDIA_FINAL[df_2016$PSELETIVO == "SISU"])
NOutro  <- length(df_2016$MEDIA_FINAL[df_2016$PSELETIVO == "Outro"])
NAC     <- length(df_2016$MEDIA_FINAL[df_2016$COTA == "AC"])
NRenda  <- length(df_2016$MEDIA_FINAL[df_2016$COTA == "Renda"])
NNRenda <- length(df_2016$MEDIA_FINAL[df_2016$COTA == "NRenda"])
idadeVec<- df_2016$IDADE

# Imprimir resultados
cat("--------------\n")
cat("IPC 2016: \n")
cat("--------------\n")
cat("Total : ", NAlunos, "\n")
cat("No. de homens : ", NMasc, "\n")
cat("  % de homens : ", round(NMasc/NAlunos * 100, 1), "% \n")
cat("No. de mulheres : ", NFem, "\n")
cat("  % de mulheres : ", round(NFem/NAlunos * 100, 1), "% \n")
cat("No. de repetentes : ", NRepet, "\n")
cat("  % de repetentes : ", round(NRepet/NAlunos * 100, 1), "% \n")
cat("No. de nao repetentes : ", NNRepet, "\n")
cat("  % de nao repetentes : ", round(NNRepet/NAlunos * 100, 1), "% \n")
cat("No. de experientes : ", NExp, "\n")
cat("  % de experientes : ", round(NExp/NAlunos * 100, 1), "% \n")
cat("No. de nao-experientes : ", NNExp, "\n")
cat("  % de nao-experientes : ", round(NNExp/NAlunos * 100, 1), "% \n")
cat("No. de experientes ND : ", NExpND, "\n")
cat("  % de experientes ND : ", round(NExpND/NAlunos * 100, 1), "% \n")
cat("No. de calouros : ", NCalour, "\n")
cat("  % de calouros : ", round(NCalour/NAlunos * 100, 1), "% \n")
cat("No. de periodizados : ", NPeriod, "\n")
cat("  % de periodizados : ", round(NPeriod/NAlunos * 100, 1), "% \n")

cat("Idade (media): ", round(mean(idadeVec), 1), "\n")
cat("Idade (sdev) : ", round(sd(idadeVec), 1), "\n")

cat("PSELETIVO ---------------------\n")
cat("No. de PSC : ", NPSC, "\n")
cat("  % de PSC : ", round(NPSC/NAlunos * 100, 1), "% \n")
cat("No. de SISU : ", NSISU, "\n")
cat("  % de SISU : ", round(NSISU/NAlunos * 100, 1), "% \n")
cat("No. de Outros : ", NOutro, "\n")
cat("  % de Outros : ", round(NOutro/NAlunos * 100, 1), "% \n")

cat("COTA ---------------------\n")
cat("No. de AC : ", NAC, "\n")
cat("  % de AC : ", round(NAC/NAlunos * 100, 1), "% \n")
cat("No. de Renda : ", NRenda, "\n")
cat("  % de Renda : ", round(NRenda/NAlunos * 100, 1), "% \n")
cat("No. de NRenda : ", NNRenda, "\n")
cat("  % de NRenda : ", round(NNRenda/NAlunos * 100, 1), "% \n")

cat("\n\n")



# Calcular estatisticas descritivas da turma de 2017/1
NAlunos <- length(df_2017$MATR_ALUNO)
NMasc   <- length(df_2017$MATR_ALUNO[df_2017$SEXO == "M"])
NFem    <- length(df_2017$MATR_ALUNO[df_2017$SEXO == "F"])
NRepet  <- length(df_2017$MATR_ALUNO[df_2017$REPET == 1])
NNRepet <- length(df_2017$MATR_ALUNO[df_2017$REPET == 0])
NExp    <- length(df_2017$MATR_ALUNO[df_2017$EXP_PROG == "yes"])
NNExp   <- length(df_2017$MATR_ALUNO[df_2017$EXP_PROG == "no"])
NExpND  <- length(df_2017$MATR_ALUNO[df_2017$EXP_PROG == "#N/D"])
NCalour <- length(df_2017$MEDIA_FINAL[df_2017$ANO_INGRESSO == 2017])
NPeriod <- length(df_2017$MEDIA_FINAL[df_2017$PERIODIZADO == "yes"])
NPSC    <- length(df_2017$MEDIA_FINAL[df_2017$PSELETIVO == "PSC"])
NSISU   <- length(df_2017$MEDIA_FINAL[df_2017$PSELETIVO == "SISU"])
NOutro  <- length(df_2017$MEDIA_FINAL[df_2017$PSELETIVO == "Outro"])
NAC     <- length(df_2017$MEDIA_FINAL[df_2017$COTA == "AC"])
NRenda  <- length(df_2017$MEDIA_FINAL[df_2017$COTA == "Renda"])
NNRenda <- length(df_2017$MEDIA_FINAL[df_2017$COTA == "NRenda"])

idadeVec<- df_2017$IDADE

# Imprimir resultados
cat("--------------\n")
cat("IPC 2017: \n")
cat("--------------\n")
cat("Total : ", NAlunos, "\n")
cat("No. de homens : ", NMasc, "\n")
cat("  % de homens : ", round(NMasc/NAlunos * 100, 1), "% \n")
cat("No. de mulheres : ", NFem, "\n")
cat("  % de mulheres : ", round(NFem/NAlunos * 100, 1), "% \n")
cat("No. de repetentes : ", NRepet, "\n")
cat("  % de repetentes : ", round(NRepet/NAlunos * 100, 1), "% \n")
cat("No. de nao repetentes : ", NNRepet, "\n")
cat("  % de nao repetentes : ", round(NNRepet/NAlunos * 100, 1), "% \n")
cat("No. de experientes : ", NExp, "\n")
cat("  % de experientes : ", round(NExp/NAlunos * 100, 1), "% \n")
cat("No. de nao-experientes : ", NNExp, "\n")
cat("  % de nao-experientes : ", round(NNExp/NAlunos * 100, 1), "% \n")
cat("No. de experientes ND : ", NExpND, "\n")
cat("  % de experientes ND : ", round(NExpND/NAlunos * 100, 1), "% \n")
cat("No. de calouros : ", NCalour, "\n")
cat("  % de calouros : ", round(NCalour/NAlunos * 100, 1), "% \n")
cat("No. de periodizados : ", NPeriod, "\n")
cat("  % de periodizados : ", round(NPeriod/NAlunos * 100, 1), "% \n")

cat("Idade (media): ", round(mean(idadeVec), 1), "\n")
cat("Idade (sdev) : ", round(sd(idadeVec), 1), "\n")

cat("PSELETIVO ---------------------\n")
cat("No. de PSC : ", NPSC, "\n")
cat("  % de PSC : ", round(NPSC/NAlunos * 100, 1), "% \n")
cat("No. de SISU : ", NSISU, "\n")
cat("  % de SISU : ", round(NSISU/NAlunos * 100, 1), "% \n")
cat("No. de Outros : ", NOutro, "\n")
cat("  % de Outros : ", round(NOutro/NAlunos * 100, 1), "% \n")

cat("COTA ---------------------\n")
cat("No. de AC : ", NAC, "\n")
cat("  % de AC : ", round(NAC/NAlunos * 100, 1), "% \n")
cat("No. de Renda : ", NRenda, "\n")
cat("  % de Renda : ", round(NRenda/NAlunos * 100, 1), "% \n")
cat("No. de NRenda : ", NNRenda, "\n")
cat("  % de NRenda : ", round(NNRenda/NAlunos * 100, 1), "% \n")

cat("\n")


# Intercessao entre as turmas de 2016 e 2017
union <- c(df_2016$MATR_ALUNO, df_2017$MATR_ALUNO)
cat("Alunos repetentes de 2016 nao-desistentes em 2017 : ", length(union) - length(unique(union)), "\n")

union <- c(df_2016c$MATR_ALUNO, df_2017c$MATR_ALUNO)
cat("Alunos repetentes de 2016 matriculados em 2017 : ", length(union) - length(unique(union)), "\n")
cat("\n")


#---------------------------------
# CONFIGURACAO DE GRAFICOS
#---------------------------------

# mfrow: sets the plotting area into a arg1 x arg2 array
# mar  : sets the bottom, left, top and right margins respectively
par(mfrow=c(1, 1), mar=c(5, 5, 3, 0.5), las=1)

#------------------------------------
# MEDIAS FINAIS DE IPC

# Teste de normalidade Shapiro-Wilk
teste <- shapiro.test(df_2016$MEDIA_FINAL)
cat("Teste Shapiro-Wilk | IPC 2016: ", 
    teste$p.value,
    "\n")

teste <- shapiro.test(df_2017$MEDIA_FINAL)
cat("Teste Shapiro-Wilk | IPC 2017: ", 
    teste$p.value,
    "\n")

# Teste Mann Whitney
teste <- wilcox.test(df_2016$MEDIA_FINAL, df_2017$MEDIA_FINAL, paired = F)
cat("Teste Mann-Whitney | IPC 2016 x 2017: ", 
    teste$p.value,
    "\n")

cat("\n")

# Boxplot das notas finais de IPC
boxplot(df_2016$MEDIA_FINAL, df_2017$MEDIA_FINAL, 
        names = c("2016/1", "2017/1"), 
        main = "Nota final em IPC (não desistentes)", 
        xlab = "Período letivo", 
        ylab = "Nota",
        las = 1,
        boxwex = boxwidth,
        cex.main = escala,
        cex.axis = escala,
        cex.lab = escala, col="gold")

# imprimir valor p
text(1.5, 5, 
     substitute(paste("p", " " %~~% " ", valorP), 
                list(valorP = round(teste$p.value, 2))),
     cex = escala)


#------------------------------------
# MEDIAS DO ENEM

# Configurar graficos
# mfrow: sets the plotting area into a arg1 x arg2 array
# mar  : sets the bottom, left, top and right margins respectively
par(mfrow=c(2, 2), mar=c(3, 4, 2, 0.5), las=1)

# Teste de normalidade Shapiro-Wilk
teste <- shapiro.test(df_2016$MedEnem)
cat("Teste Shapiro-Wilk | MedEnem 2016: ", 
    teste$p.value,
    "\n")

teste <- shapiro.test(df_2017$MedEnem)
cat("Teste Shapiro-Wilk | MedEnem 2017: ", 
    teste$p.value,
    "\n")

# Teste Mann Whitney
teste <- wilcox.test(df_2016$MedEnem, df_2017$MedEnem, paired = F)
cat("Teste Mann-Whitney | MedEnem 2016 x 2017: ", 
    teste$p.value,
    "\n")

cat("\n")

# Boxplot das notas finais de IPC
boxplot(df_2016$MedEnem, df_2017$MedEnem, 
        names = c("2016/1", "2017/1"), 
        main = "Média das notas no Enem", 
        xlab = "", 
        ylab = "",
        las = 1,
        boxwex = boxwidth,
        cex.main = escala,
        cex.axis = escala,
        cex.lab = escala, col="deepskyblue")

# imprimir valor p
text(1.5, 550, 
     substitute(paste("p", " " %~~% " ", valorP), 
                list(valorP = round(teste$p.value, 2))),
     cex = escala)


#------------------------------------
# MEDIAS FINAIS DE CALCULO 1 (IEM011)

# Teste de normalidade Shapiro-Wilk
teste <- shapiro.test(df_2016$IEM011)
cat("Teste Shapiro-Wilk | CALC 2016: ", 
    teste$p.value,
    "\n")

teste <- shapiro.test(df_2017$IEM011)
cat("Teste Shapiro-Wilk | CALC 2017: ", 
    teste$p.value,
    "\n")

# Teste Mann Whitney
teste <- wilcox.test(df_2016$IEM011, df_2017$IEM011, paired = F)
cat("Teste Mann-Whitney | CALC 2016 x 2017: ", 
    teste$p.value,
    "\n")

cat("\n")


# Boxplot das notas finais de Calculo
boxplot(df_2016$IEM011, df_2017$IEM011, 
        names = c("2016/1", "2017/1"), 
        main = "Nota final em Cálculo 1", 
        xlab = "", 
        ylab = "",
        las = 1,
        boxwex = boxwidth,
        cex.main = escala,
        cex.axis = escala,
        cex.lab = escala, col="deepskyblue")

# imprimir valor p
text(1.5, 5, 
     substitute(paste("p", " " %~~% " ", valorP), 
                list(valorP = round(teste$p.value, 2))),
     cex = escala)


#------------------------------------
# MEDIAS FINAIS DE ALGEBRA LINEAR 1 (IEM012)


# Teste de normalidade Shapiro-Wilk
teste <- shapiro.test(df_2016$IEM012)
cat("Teste Shapiro-Wilk | AL 2016: ", 
    teste$p.value,
    "\n")

teste <- shapiro.test(df_2017$IEM012)
cat("Teste Shapiro-Wilk | AL 2017: ", 
    teste$p.value,
    "\n")

# Teste Mann Whitney
teste <- wilcox.test(df_2016$IEM012, df_2017$IEM012, paired = F)
cat("Teste Mann-Whitney | AL 2016 x 2017: ", 
    teste$p.value,
    "\n")

cat("\n")


# Boxplot das notas finais de AL
boxplot(df_2016$IEM012, df_2017$IEM012, 
        names = c("2016/1", "2017/1"), 
        main = "Nota final em Álgebra Linear 1", 
        xlab = "", 
        ylab = "",
        las = 1,
        boxwex = boxwidth,
        cex.main = escala,
        cex.axis = escala,
        cex.lab = escala, col="deepskyblue")

# imprimir valor p
text(1.5, 5, 
     substitute(paste("p", " " %~~% " ", valorP), 
                list(valorP = round(teste$p.value, 2))),
     cex = escala)

#------------------------------------
# COEFICIENTE DE RENDIMENTO (CR)
# CR é a média de todas as disciplinas cursadas pelo aluno, exceto IPC

# Teste de normalidade Shapiro-Wilk
teste <- shapiro.test(df_2016$CR)
cat("Teste Shapiro-Wilk | CR 2016: ", 
    teste$p.value,
    "\n")

teste <- shapiro.test(df_2017$CR)
cat("Teste Shapiro-Wilk | CR 2017: ", 
    teste$p.value,
    "\n")

# Teste Mann Whitney
teste <- wilcox.test(df_2016$CR, df_2017$CR, paired = F)
cat("Teste Mann-Whitney | CR 2016 x 2017: ", 
    teste$p.value,
    "\n")

cat("\n")


# Boxplot dos CR
boxplot(df_2016$CR, df_2017$CR, 
        names = c("2016/1", "2017/1"), 
        main = "Coeficiente de Rendimento", 
        xlab = "", 
        ylab = "",
        las = 1,
        boxwex = boxwidth,
        cex.main = escala,
        cex.axis = escala,
        cex.lab = escala, col="deepskyblue")

# imprimir valor p
text(1.5, 5, 
     substitute(paste("p", " " %~~% " ", valorP), 
                list(valorP = round(teste$p.value, 2))),
     cex = escala)


#----------------------------------------------------------
# Comparacao das notas do Enem entre as turmas 2016 x 2017

provas <- c('NU_NOTA_L', 'NU_NOTA_M', 'NU_NOTA_CN', 'NU_NOTA_CH', 'NU_NOTA_R', 'MedEnem')
fim <- length(provas)

for (i in c(1:fim)) {
  # Convertendo listas para vetor de numeros
  enem_2016 = as.numeric(unlist(df_2016[provas[i]]))
  enem_2017 = as.numeric(unlist(df_2017[provas[i]]))
  
  # Teste de normalidade Shapiro-Wilk
  cat("Teste Shapiro-Wilk | ", provas[i], " 2016: ")
  testeA <- shapiro.test(enem_2016)
  print(testeA$p.value)
  
  cat("Teste Shapiro-Wilk | ", provas[i], " 2017: ")
  testeB <- shapiro.test(enem_2017)
  print(testeB$p.value)
  
  # Teste de comparacao de medias
  # Se AMBAS as distribuicoes forem normais, aplica-se o teste T
  if (testeA$p.value > 0.05 & testeB$p.value > 0.05) {
    # Teste T
    cat("Teste T | ", provas[i], " 2016 x 2017: ")
    teste <- t.test(enem_2016, enem_2017, paired = F)
    print(teste$p.value)
  }
  # Se pelo menos uma delas nao for normal, aplica-se o teste nao parametrico
  else {
    # Teste Mann Whitney
    cat("Teste Mann-Whitney | ", provas[i], " 2016 x 2017: ")
    teste <- wilcox.test(enem_2016, enem_2017, paired = F)
    print(teste$p.value)
  }
  
  cat("\n")
}



#----------------------------------------------------------
# USO DO CODEBENCH entre as turmas 2016 x 2017
#----------------------------------------------------------

# Quantidade de exercicios valendo nota
NExerc16 <- 75
NExerc17 <- 87

# Nomes das colunas do dataframe que contem dados de uso do CoodeBench
atrib_CB <- c('N_LOGINS', 
              'N_TESTES', 
              'N_SUBMISSOES', 
              'TOTAL_TEMPO_IDE_SEG', 
              'N_EX_CORRETOS', 
              'N_EX_NCORRETOS')
fim <- length(atrib_CB)

# Listagem de titulos de graficos
plotTitle <- c('N° de logins / exercício', 
               'N° de testes / exercício', 
               'N° de submissões / exercício', 
               'Tempo de IDE (segundos) / exercício',
               '% de exercícios corretos', 
               '% de exercícios incorretos')

# Caracterizacao dos atributos de uso do CodeBench
for (i in c(1:fim)) {
  # Normaliza os atributos pela quantidade de exercicios disponibilizada no CodeBench
  atrib_2016 = as.numeric(as.character(unlist(df_2016[atrib_CB[i]]))) / NExerc16
  atrib_2017 = as.numeric(as.character(unlist(df_2017[atrib_CB[i]]))) / NExerc17
  
  cat(plotTitle[i], " (mediana - 2016) : ", mean(na.omit(atrib_2016)), "\n")
  cat(plotTitle[i], " (mediana - 2017) : ", mean(na.omit(atrib_2017)), "\n")
  cat("\n")
}

# Configuracao do grafico
# mfrow: sets the plotting area into a arg1 x arg2 array
# mar  : sets the bottom, left, top and right margins respectively
par(mfrow=c(3, 2), mar=c(3, 4, 2, 0.5), las=1)


# Testes estatisticos de comparacao de media
# Distribuicao dos atributos do CodeBench em 2016 x 2017
for (i in c(1:fim)) {
  # Normaliza os atributos pela quantidade de exercicios disponibilizada no CodeBench
  atrib_2016 = as.numeric(as.character(unlist(df_2016[atrib_CB[i]]))) / NExerc16
  atrib_2017 = as.numeric(as.character(unlist(df_2017[atrib_CB[i]]))) / NExerc17
  
  # Teste de normalidade Shapiro-Wilk
  testeA <- shapiro.test(atrib_2016)
  cat("Teste Shapiro-Wilk | ", atrib_CB[i], " 2016: ",
      testeA$p.value,
      "\n")
  
  testeB <- shapiro.test(atrib_2017)
  cat("Teste Shapiro-Wilk | ", atrib_CB[i], " 2017: ",
      testeB$p.value,
      "\n")
  
  # Teste de comparacao de medias
  # Se AMBAS as distribuicoes forem normais, aplica-se o teste T
  if (testeA$p.value > 0.05 & testeB$p.value > 0.05) {
    # Teste T
    teste <- t.test(atrib_2016, atrib_2017, paired = F)
    cat("Teste T | ", atrib_CB[i], " 2016 x 2017: ",
        teste$p.value,
        "\n")
  }
  # Se pelo menos uma delas nao for normal, aplica-se o teste nao parametrico
  else {
    # Teste Mann Whitney
    teste <- wilcox.test(atrib_2016, atrib_2017, paired = F)
    cat("Teste Mann-Whitney | ", atrib_CB[i], " 2016 x 2017: ",
        teste$p.value,
        "\n")
  }

  cat("\n")

  boxplot(atrib_2016, atrib_2017, 
          names = c("2016/1", "2017/1"),
          main = plotTitle[i],
          xlab = "",
          ylab = "",
          las = 1,
          boxwex = boxwidth,
          outline=FALSE,
          yaxt="n",
          cex.main = escala * 0.9,
          cex.axis = escala * 0.9,
          cex.lab = escala * 0.9, 
          col="paleturquoise1")
  
  # posicao vertical da anotacao do valor p
  vertpos <- max(na.omit(c(atrib_2016, atrib_2017))) / 10

  # imprimir valor p
  text(1.5, vertpos,
       substitute(paste("p", " " %~~% " ", valorP), 
                  list(valorP = round(teste$p.value, 4))),
       cex = escala)
  
  # Caso o atributo seja um %, imprimir numeros do eixo Y em %
  if (atrib_CB[i] == 'N_EX_CORRETOS') {
    axis(2, at=pretty(atrib_2016), lab=pretty(atrib_2016) * 100, cex.axis = escala * 0.9, las=TRUE)
  } 
  else if (atrib_CB[i] == 'N_EX_NCORRETOS') {
    axis(2, at=pretty(atrib_2016/2), lab=pretty(atrib_2016/2) * 100, cex.axis = escala * 0.9, las=TRUE)
  }
  else {
    axis(2, cex.axis = escala * 0.9, las=TRUE)
  }
}



#----------------------------------------------------------
# Testes de correlacao entre:
# notas do Enem e desempenho em disciplinas (IPC, Calc e AL), para turmas 2016 e 2017

provas <- c('NU_NOTA_L', 'NU_NOTA_M', 'NU_NOTA_CN', 'NU_NOTA_CH', 'NU_NOTA_R', 'MedEnem')
fim <- length(provas)

for (i in c(1:fim)) {
  # Convertendo listas para vetor de numeros
  enem_2016 = as.numeric(unlist(df_2016[provas[i]]))
  enem_2017 = as.numeric(unlist(df_2017[provas[i]]))

  cat("Correlacao de Kendall | IPC x ", provas[i], " (2016): ")
  corrTeste <- cor.test(df_2016$MEDIA_FINAL, enem_2016, method = "spearman", na.rm=TRUE)
  if (corrTeste$p.value <= 0.05) {
    cat("tau: ", round(corrTeste$estimate, 2), "  |  ")
    cat("valor p: ", round(corrTeste$p.value, 2), "\n")
  }
  else {
    cat("valor p: ", corrTeste$p.value, "\n")
  }
  
  cat("Correlacao de Kendall | IPC x ", provas[i], " (2017): ")
  corrTeste <- cor.test(df_2017$MEDIA_FINAL, enem_2017, method = "spearman", na.rm=TRUE)
  if (corrTeste$p.value <= 0.05) {
    cat("tau: ", round(corrTeste$estimate, 2), "  |  ")
    cat("valor p: ", round(corrTeste$p.value, 2), "\n")
  }
  else {
    cat("valor p: ", corrTeste$p.value, "\n")
  }
  
  cat("\n")
}



#------------------------------------
# MEDIAS FINAIS DE IPC
# ESTRATIFICACAO: EXPERIENCIA PREVIA EM PROGRAMACAO

# Notas finais de cada categoria (com/sem experiencia) para cada ano (2016, 2017)
mf16_exp  <- df_2016$MEDIA_FINAL[df_2016$EXP_PROG == "yes"]
mf16_nexp <- df_2016$MEDIA_FINAL[df_2016$EXP_PROG == "no"]
mf16_expnd<- df_2016$MEDIA_FINAL[df_2016$EXP_PROG == "#N/D"]
mf17_exp  <- df_2017$MEDIA_FINAL[df_2017$EXP_PROG == "yes"]
mf17_nexp <- df_2017$MEDIA_FINAL[df_2017$EXP_PROG == "no"]

# Teste Mann Whitney (2016 x 2017)
teste <- wilcox.test(mf16_exp, mf17_exp, paired = F)
cat("Teste Mann-Whitney | IPC 2016 x 2017 (experientes): ", 
    teste$p.value, "\n")
valorp_exp_16x17 <- teste$p.value

teste <- wilcox.test(mf16_nexp, mf17_nexp, paired = F)
cat("Teste Mann-Whitney | IPC 2016 x 2017 (nao experientes): ", 
    teste$p.value, "\n")
cat("\n")
valorp_nexp_16x17 <- teste$p.value

# Teste Kruskal-Wallis por postos (2016 x 2017), para > 2 categorias
teste <- kruskal.test(MEDIA_FINAL ~ EXP_PROG, data = df_2016)
cat("Teste Kruskal | IPC (2016) experientes x nao-experientes x ND: ", 
    teste$p.value, "\n")



#---------------------------------
# CONFIGURACAO DE GRAFICOS
#---------------------------------

# mfrow: sets the plotting area into a arg1 x arg2 array
# mar  : sets the bottom, left, top and right margins respectively
par(mfrow=c(1, 1), mar=c(5, 5, 3, 0.5), las=1)

#---------------------------------

# Boxplot das notas finais de IPC
# 2016 x 2017 (experientes)
boxplot(mf16_exp, mf17_exp,
        names = c("2016/1", "2017/1"),
        main = "Nota final em IPC (não desistentes e experientes)",
        xlab = "Período letivo",
        ylab = "Nota",
        las = 1,
        boxwex = boxwidth,
        cex.main = escala,
        cex.axis = escala,
        cex.lab = escala, col="paleturquoise1")

# Boxplot das notas finais de IPC
# 2016 x 2017 (nao experientes)
boxplot(mf16_nexp, mf17_nexp,
        names = c("2016/1", "2017/1"),
        main = "Nota final em IPC (não desistentes e não experientes)",
        xlab = "Período letivo",
        ylab = "Nota",
        las = 1,
        boxwex = boxwidth,
        cex.main = escala,
        cex.axis = escala,
        cex.lab = escala, col="paleturquoise1")


# Juntar os dois boxplots anteriores em um soh grafico
# Adaptado de https://stackoverflow.com/questions/47479522/how-to-create-a-grouped-boxplot-in-r

DF1 <- data.frame(
  x = c(c(mf16_exp, mf17_exp), c(mf16_nexp, mf17_nexp)),
  y = c(rep("experientes", each = length(mf16_exp) + length(mf17_exp)), 
        rep("não experientes", each = length(mf16_nexp) + length(mf17_nexp))),
  z = c(rep("2016", each = length(mf16_exp)),
        rep("2017", each = length(mf17_exp)),
        rep("2016", each = length(mf16_nexp)),
        rep("2017", each = length(mf17_nexp))),
  stringsAsFactors = FALSE
)
str(DF1)

#---------------------------------
# CONFIGURACAO DE GRAFICOS
#---------------------------------

# mfrow: sets the plotting area into a arg1 x arg2 array
# mar  : sets the bottom, left, top and right margins respectively
par(mfrow=c(1, 1), mar=c(5, 5, 3, 0.5), las=1)

boxcores <- c("darkolivegreen1", "tan1")
boxwidth < 0.8
#---------------------------------

# Boxplot das notas finais de IPC
# 2016 x 2017 (experientes)
boxplot(x ~ z + y, data = DF1,
        at = c(1,2,5,6),
        names = c("sim", "", "não", ""),
        xaxs = FALSE,
        main = "Nota final em IPC (não desistentes)",
        xlab = "Experiência prévia em programação",
        ylab = "Nota",
        las = 1,
        xaxt='n',
        boxwex = boxwidth,
        cex.main = escala,
        cex.axis = escala,
        cex.lab = escala, 
        col = boxcores)
axis(1, at = c(1.5, 5.5), labels = c("sim", "não"), cex.axis = escala)
legend("bottom", fill = boxcores, legend = c("2016", "2017"), horiz = T, cex = escala)

# imprimir valor p
text(1.5, 10, 
     substitute(paste("p", " " %~~% " ", valorP), 
                list(valorP = round(valorp_exp_16x17, 2))),
     cex = escala)

text(5.5, 10, 
     substitute(paste("p", " " %~~% " ", valorP), 
                list(valorP = round(valorp_nexp_16x17, 2))),
     cex = escala)



#------------------------------------
# MEDIAS FINAIS DE CALCULO 1 (IEM011)
# ESTRATIFICACAO: EXPERIENCIA PREVIA EM PROGRAMACAO

# Notas finais de cada categoria (com/sem experiencia) para cada ano (2016, 2017)
mf16_exp  <- df_2016$IEM011[df_2016$EXP_PROG == "yes"]
mf16_nexp <- df_2016$IEM011[df_2016$EXP_PROG == "no"]
mf17_exp  <- df_2017$IEM011[df_2017$EXP_PROG == "yes"]
mf17_nexp <- df_2017$IEM011[df_2017$EXP_PROG == "no"]


# Teste Mann Whitney (2016 x 2017)
teste <- wilcox.test(mf16_exp, mf17_exp, paired = F)
cat("Teste Mann-Whitney | Calculo 1 (IEM011) 2016 x 2017 (experientes): ", 
    teste$p.value, "\n")

teste <- wilcox.test(mf16_nexp, mf17_nexp, paired = F)
cat("Teste Mann-Whitney | Calculo 1 (IEM011) 2016 x 2017 (nao experientes): ", 
    teste$p.value, "\n")
cat("\n")



#------------------------------------
# MEDIAS FINAIS DE ALGEBRA LINEAR 1 (IEM012)
# ESTRATIFICACAO: EXPERIENCIA PREVIA EM PROGRAMACAO

# Notas finais de cada categoria (com/sem experiencia) para cada ano (2016, 2017)
mf16_exp  <- df_2016$IEM012[df_2016$EXP_PROG == "yes"]
mf16_nexp <- df_2016$IEM012[df_2016$EXP_PROG == "no"]
mf17_exp  <- df_2017$IEM012[df_2017$EXP_PROG == "yes"]
mf17_nexp <- df_2017$IEM012[df_2017$EXP_PROG == "no"]

# Teste Mann Whitney (2016 x 2017)
teste <- wilcox.test(mf16_exp, mf17_exp, paired = F)
cat("Teste Mann-Whitney | Algebra Linear 1 (IEM012) 2016 x 2017 (experientes): ", 
    teste$p.value, "\n")

teste <- wilcox.test(mf16_nexp, mf17_nexp, paired = F)
cat("Teste Mann-Whitney | Algebra Linear 1 (IEM012) 2016 x 2017 (nao experientes): ", 
    teste$p.value, "\n")
cat("\n")



#------------------------------------
# MEDIAS FINAIS DE IPC
# ESTRATIFICACAO: REPETENTES X NAO-REPETENTES

# Notas finais de cada categoria (repetentes sim/nao) para cada ano (2016, 2017)
mf16_repet  <- df_2016$MEDIA_FINAL[df_2016$REPET == 1]
mf16_nrepet <- df_2016$MEDIA_FINAL[df_2016$REPET == 0]
mf17_repet  <- df_2017$MEDIA_FINAL[df_2017$REPET == 1]
mf17_nrepet <- df_2017$MEDIA_FINAL[df_2017$REPET == 0]

# Teste Mann Whitney (2016 x 2017)
teste <- wilcox.test(mf16_repet, mf17_repet, paired = F)
cat("Teste Mann-Whitney | IPC 2016 x 2017 (repetentes): ", 
    teste$p.value, "\n")
valorp_rep_16x17 <- teste$p.value

teste <- wilcox.test(mf16_nrepet, mf17_nrepet, paired = F)
cat("Teste Mann-Whitney | IPC 2016 x 2017 (nao repetentes): ", 
    teste$p.value, "\n")
cat("\n")
valorp_nrep_16x17 <- teste$p.value


#---------------------------------
# CONFIGURACAO DE GRAFICOS
#---------------------------------

# mfrow: sets the plotting area into a arg1 x arg2 array
# mar  : sets the bottom, left, top and right margins respectively
par(mfrow=c(1, 1), mar=c(5, 5, 3, 0.5), las=1)

#---------------------------------

# Boxplot das notas finais de IPC
# 2016 x 2017 (experientes)
boxplot(mf16_repet, mf17_repet,
        names = c("2016/1", "2017/1"),
        main = "Nota final em IPC (não desistentes e repetentes)",
        xlab = "Período letivo",
        ylab = "Nota",
        las = 1,
        boxwex = boxwidth,
        cex.main = escala,
        cex.axis = escala,
        cex.lab = escala, col="paleturquoise1")

# Boxplot das notas finais de IPC
# 2016 x 2017 (nao repetentes)
boxplot(mf16_nrepet, mf17_nrepet,
        names = c("2016/1", "2017/1"),
        main = "Nota final em IPC (não desistentes e não repetentes)",
        xlab = "Período letivo",
        ylab = "Nota",
        las = 1,
        boxwex = boxwidth,
        cex.main = escala,
        cex.axis = escala,
        cex.lab = escala, col="paleturquoise1")



# Adaptado de https://stackoverflow.com/questions/47479522/how-to-create-a-grouped-boxplot-in-r

DF1 <- data.frame(
  x = c(c(mf16_repet, mf17_repet), c(mf16_nrepet, mf17_nrepet)),
  y = c(rep("repetentes", each = length(mf16_repet) + length(mf17_repet)), 
        rep("não repetentes", each = length(mf16_nrepet) + length(mf17_nrepet))),
  z = c(rep("2016", each = length(mf16_repet)),
        rep("2017", each = length(mf17_repet)),
        rep("2016", each = length(mf16_nrepet)),
        rep("2017", each = length(mf17_nrepet))),
  stringsAsFactors = FALSE
)
str(DF1)

#---------------------------------
# CONFIGURACAO DE GRAFICOS
#---------------------------------

# mfrow: sets the plotting area into a arg1 x arg2 array
# mar  : sets the bottom, left, top and right margins respectively
par(mfrow=c(1, 1), mar=c(5, 5, 3, 0.5), las=1)

boxcores <- c("darkolivegreen1", "tan1")
boxwidth < 0.8
#---------------------------------

# Boxplot das notas finais de IPC
# 2016 x 2017 (repetentes)
boxplot(x ~ z + y, data = DF1,
        at = c(1,2,5,6),
        names = c("não", "", "sim", ""),
        xaxs = FALSE,
        main = "Nota final em IPC (não desistentes)",
        xlab = "Repetente em IPC",
        ylab = "Nota",
        las = 1,
        xaxt='n',
        boxwex = boxwidth,
        cex.main = escala,
        cex.axis = escala,
        cex.lab = escala, 
        col = boxcores)
axis(1, at = c(1.5, 5.5), labels = c("não", "sim"), cex.axis = escala)

# imprimir valor p
text(1.5, 10, 
     substitute(paste("p", " " %~~% " ", valorP), 
                list(valorP = round(valorp_nrep_16x17, 2))),
     cex = escala)

text(5.5, 10, 
     substitute(paste("p", " " %~~% " ", valorP), 
                list(valorP = round(valorp_rep_16x17, 2))),
     cex = escala)

legend("bottom", fill = boxcores, legend = c("2016", "2017"), horiz = T, cex = escala)



#----------------------------------------------------------
# TESTES QUI-QUADRADO
# Vetores de frequencia das amostras de 2016 e 2017
#----------------------------------------------------------


cat("--------------\n")
cat("TESTES QUI-QUADRADO: \n")
cat("--------------\n")

# Sexo
#----------------------------------------------------------
freqVecSex16 <- table(df_2016$SEXO) / length(df_2016$SEXO)
freqVecSex17 <- table(df_2017$SEXO) / length(df_2017$SEXO)

# Teste Qui-Quadrado
chitest <- chisq.test(freqVecSex16, freqVecSex17, correct = F)
cat("Sexo 2016 x 2017 : ", round(chitest$p.value, 5), "\n")
chitestSex <- chitest


# Idade
#----------------------------------------------------------

# Cria vetor de frequencia das idades
# Lista de categorias de idade: <=17, 18, 19, 20, 21, >=22
idadeFim <- 21
idadeList <- c(18:idadeFim)

# Cria vetores vazios
freqVecAge16 <- vector()
freqVecAge17 <- vector()

# Calcula frequencia dos menores de 17 anos
freqVecAge16[1] <- length(df_2016$IDADE[df_2016$IDADE <= 17]) / length(df_2016$IDADE)
freqVecAge17[1] <- length(df_2017$IDADE[df_2017$IDADE <= 17]) / length(df_2017$IDADE)

# Calcula frequencia dos maiores de 22 anos
freqVecAge16[length(idadeList) + 2] <- length(df_2016$IDADE[df_2016$IDADE > idadeFim]) / length(df_2016$IDADE)
freqVecAge17[length(idadeList) + 2] <- length(df_2017$IDADE[df_2017$IDADE > idadeFim]) / length(df_2017$IDADE)

# Calcula frequencia das demais idades (18 a 21 anos)
for (i in c(1:length(idadeList))) {
  freqVecAge16[i+1] <- length(df_2016$IDADE[df_2016$IDADE == idadeList[i]]) / length(df_2016$IDADE)
  freqVecAge17[i+1] <- length(df_2017$IDADE[df_2017$IDADE == idadeList[i]]) / length(df_2017$IDADE)
}


# Teste Qui-Quadrado
chitest <- chisq.test(freqVecAge16, freqVecAge17, correct = F)
cat("Idade 2016 x 2017 : ", round(chitest$p.value, 4), "\n")
chitestAge <- chitest

# Calouros
#----------------------------------------------------------
freqVecCal16 <- vector()
freqVecCal17 <- vector()

# Construcao do vetor de frequencia: [2 anos ou mais, 1 ano, calouro]
freqVecCal16[3] <- length(df_2016$ANO_INGRESSO[df_2016$ANO_INGRESSO <= 2014]) / length(df_2016$ANO_INGRESSO)
freqVecCal17[3] <- length(df_2017$ANO_INGRESSO[df_2017$ANO_INGRESSO <= 2015]) / length(df_2017$ANO_INGRESSO)

freqVecCal16[2] <- length(df_2016$ANO_INGRESSO[df_2016$ANO_INGRESSO == 2015]) / length(df_2016$ANO_INGRESSO)
freqVecCal17[2] <- length(df_2017$ANO_INGRESSO[df_2017$ANO_INGRESSO == 2016]) / length(df_2017$ANO_INGRESSO)

freqVecCal16[1] <- length(df_2016$ANO_INGRESSO[df_2016$ANO_INGRESSO == 2016]) / length(df_2016$ANO_INGRESSO)
freqVecCal17[1] <- length(df_2017$ANO_INGRESSO[df_2017$ANO_INGRESSO == 2017]) / length(df_2017$ANO_INGRESSO)


# Teste Qui-Quadrado
chitest <- chisq.test(freqVecCal16, freqVecCal17, correct = F)
cat("Calouros 2016 x 2017 : ", round(chitest$p.value, 4), "\n")
chitestCal <- chitest

# Repetentes
#----------------------------------------------------------
freqVecRepet16 <- table(df_2016$PERIODIZADO) / length(df_2016$PERIODIZADO)
freqVecRepet17 <- table(df_2017$PERIODIZADO) / length(df_2017$PERIODIZADO)

# Teste Qui-Quadrado
chitest <- chisq.test(freqVecRepet16, freqVecRepet17, correct = F)
cat("Repetentes 2016 x 2017 : ", round(chitest$p.value, 4), "\n")
chitestRepet <- chitest


# Experiencia Previa
#----------------------------------------------------------
freqVecExp16 <- table(df_2016$EXP_PROG) / length(df_2016$EXP_PROG)
freqVecExp17 <- table(df_2017$EXP_PROG) / length(df_2017$EXP_PROG)

# Teste Qui-Quadrado
chitest <- chisq.test(freqVecExp16, freqVecExp17, correct = F)
cat("Experiencia Previa 2016 x 2017 : ", round(chitest$p.value, 4), "\n")
chitestExp <- chitest


# Processo Seletivo
#----------------------------------------------------------
freqVecPSel16 <- table(df_2016$PSELETIVO) / length(df_2016$PSELETIVO)
freqVecPSel17 <- table(df_2017$PSELETIVO) / length(df_2017$PSELETIVO)

# Teste Qui-Quadrado
chitest <- chisq.test(freqVecPSel16, freqVecPSel17, correct = F)
cat("Processo Seletivo 2016 x 2017 : ", round(chitest$p.value, 4), "\n")
chitestPSel <- chitest


# Cota
#----------------------------------------------------------
freqVecCota16 <- table(df_2016$COTA) / length(df_2016$COTA)
freqVecCota17 <- table(df_2017$COTA) / length(df_2017$COTA)

# Teste Qui-Quadrado
chitest <- chisq.test(freqVecCota16, freqVecCota17, correct = F)
cat("Cota 2016 x 2017 : ", round(chitest$p.value, 4), "\n")
chitestCota <- chitest


#--------------
# Verificar correcao das formulas:
# ( a soma dos vetores de frequencia deve ser 1)

sum(freqVecSex16)
sum(freqVecSex17)
sum(freqVecAge16)
sum(freqVecAge17)
sum(freqVecCal16)
sum(freqVecCal17)
sum(freqVecRepet16)
sum(freqVecRepet17)
sum(freqVecExp16)
sum(freqVecExp17)
sum(freqVecPSel16)
sum(freqVecPSel17)
sum(freqVecCota16)
sum(freqVecCota17)

#-----------------------------------------------
# Graficos de distribuicao
#-----------------------------------------------

plotTitle <- c('Sexo', 
               'Idade', 
               'Tempo de curso',
               'Proporção de Repetentes',
               'Experiência prévia', 
               'Processo Seletivo', 
               'Cota')

# mfrow: sets the plotting area into a arg1 x arg2 array
# mar  : sets the bottom, left, top and right margins respectively
par(mfrow=c(length(plotTitle), 3), mar=c(3, 4, 2, 0.5), las=1)

precisao <- 4

# Sexo -----------------------------------------------
yrange <- c(0, max(c(freqVecSex16, freqVecSex17)))
yticks <- pretty(yrange, n = 2)
xlabels <- c("feminino", "masculino")
freqVector <- freqVecSex16

xx <- barplot(freqVector,
        main = "Sexo 2016/1", 
        names = xlabels, 
        ylim = c(min(yticks), max(yticks)),
        cex.names = escala * .7,
        yaxt = "n",
        col = "violet")
# Altera exibicao de valores do eixo y
axis(2, at = yticks, labels = yticks, cex.axis = escala * 0.7)
# Add text at top of bars
text(x = xx, y = freqVector, label = round(freqVector, 2), pos = 3, cex = escala * .7)

freqVector <- freqVecSex17
xx <- barplot(freqVector,
        main = "Sexo 2017/1", 
        names = xlabels, 
        ylim = c(min(yticks), max(yticks)),
        cex.names = escala * .7,
        yaxt = "n",
        col = "violet")
# Altera exibicao de valores do eixo y
axis(2, at = yticks, labels = yticks, cex.axis = escala * 0.7)
# Add text at top of bars
text(x = xx, y = freqVector, label = round(freqVector, 2), pos = 3, cex = escala * .7)

# Imprime valores p na area de um grafico
plot.new()
plot.window(0:1, 0:1)
mtext(substitute(paste("p", " " %~~% " ", valorP), 
                 list(valorP = round(chitestSex$p.value, precisao))),
      cex = escala * .7,
      side = 2, adj = 0)


# Idade -----------------------------------------------
yrange <- c(0, 0.5)
yticks <- pretty(yrange, n = 2)
xlabels <- c("<18", "18", "19", "20", "21", ">21")
freqVector <- freqVecAge16

xx <- barplot(freqVector,
        main = "Idade 2016/1", 
        names = xlabels,
        ylim = c(min(yticks), max(yticks)),
        cex.names = escala * .7,
        yaxt = "n",
        col = "violet")
axis(2, at = yticks, labels = yticks, cex.axis = escala * 0.7)
# Add text at top of bars
text(x = xx, y = freqVector, label = round(freqVector, 2), pos = 3, cex = escala * .7)

freqVector <- freqVecAge17
xx <- barplot(freqVector,
        main = "Idade 2017/1", 
        names = xlabels,
        ylim = c(min(yticks), max(yticks)),
        cex.names = escala * .7,
        yaxt = "n",
        col = "violet")
axis(2, at = yticks, labels = yticks, cex.axis = escala * 0.7)
# Add text at top of bars
text(x = xx, y = freqVector, label = round(freqVector, 2), pos = 3, cex = escala * .7)

# Imprime valores p na area de um grafico
plot.new()
plot.window(0:1, 0:1)
mtext(substitute(paste("p", " " %~~% " ", valorP), 
                 list(valorP = round(chitestAge$p.value, precisao))),
      cex = escala * .7,
      side = 2, adj = 0)


# Tempo de curso -----------------------------------------------
yrange <- c(0, max(c(freqVecCal16, freqVecCal17)))
yticks <- pretty(yrange, n = 2)
xlabels <- c("Calouro", "1 ano", "> 1 ano")
freqVector <- freqVecCal16

xx <- barplot(freqVector,
        main = "Tempo de curso 2016/1", 
        names = xlabels,
        ylim = c(min(yticks), max(yticks)),
        cex.names = escala * .7,
        yaxt = "n",
        col = "violet")
axis(2, at = yticks, labels = yticks, cex.axis = escala * 0.7)
# Add text at top of bars
text(x = xx, y = freqVector, label = round(freqVector, 2), pos = 3, cex = escala * .7)

freqVector <- freqVecCal17
xx <- barplot(freqVector,
        main = "Tempo de curso 2017/1", 
        names = xlabels,
        ylim = c(min(yticks), max(yticks)),
        cex.names = escala * .7,
        yaxt = "n",
        col = "violet")
axis(2, at = yticks, labels = yticks, cex.axis = escala * 0.7)
# Add text at top of bars
text(x = xx, y = freqVector, label = round(freqVector, 2), pos = 3, cex = escala * .7)

# Imprime valores p na area de um grafico
plot.new()
plot.window(0:1, 0:1)
mtext(substitute(paste("p", " " %~~% " ", valorP), 
                 list(valorP = round(chitestCal$p.value, precisao))),
      cex = escala * .7,
      side = 2, adj = 0)


# Primeira vez -----------------------------------------------
yrange <- c(0, 1.2)
yticks <- pretty(yrange, n = 2)
freqVector <- freqVecRepet16
  
xx <- barplot(freqVector,
        main = "Primeira vez 2016/1", 
        names = c("não", "sim"), 
        ylim = c(min(yticks), max(yticks)),
        cex.names = escala * .7,
        yaxt = "n",
        col = "violet")
axis(2, at = yticks, labels = yticks, cex.axis = escala * 0.7)
# Add text at top of bars
text(x = xx, y = freqVector, label = round(freqVector, 2), pos = 3, cex = escala * .7)

freqVector <- freqVecRepet17
xx <- barplot(freqVector,
        main = "Primeira vez 2017/1", 
        names = c("não", "sim"), 
        ylim = c(min(yticks), max(yticks)),
        cex.names = escala * .7,
        yaxt = "n",
        col = "violet")
axis(2, at = yticks, labels = yticks, cex.axis = escala * 0.7)
# Add text at top of bars
text(x = xx, y = freqVector, label = round(freqVector, 2), pos = 3, cex = escala * .7)

# Imprime valores p na area de um grafico
plot.new()
plot.window(0:1, 0:1)
mtext(substitute(paste("p", " " %~~% " ", valorP), 
                 list(valorP = round(chitestRepet$p.value, precisao))),
      cex = escala * .7,
      side = 2, adj = 0)


# Experiência prévia -----------------------------------------------
yrange <- c(0, max(c(freqVecExp16, freqVecExp17)))
yticks <- pretty(yrange, n = 2)
xlabels <- c("N/D", "não", "sim")
freqVector <- freqVecExp16
  
xx <- barplot(freqVector,
        main = "Experiência prévia 2016/1", 
        names = xlabels, 
        ylim = c(min(yticks), max(yticks)),
        cex.names = escala * .7,
        yaxt = "n",
        col = "violet")
axis(2, at = yticks, labels = yticks, cex.axis = escala * 0.7)
# Add text at top of bars
text(x = xx, y = freqVector, label = round(freqVector, 2), pos = 3, cex = escala * .7)

freqVector <- freqVecExp17
xx <- barplot(freqVector,
        main = "Experiência prévia 2017/1", 
        names = xlabels, 
        ylim = c(min(yticks), max(yticks)),
        cex.names = escala * .7,
        yaxt = "n",
        col = "violet")
axis(2, at = yticks, labels = yticks, cex.axis = escala * 0.7)
# Add text at top of bars
text(x = xx, y = freqVector, label = round(freqVector, 2), pos = 3, cex = escala * .7)

# Imprime valores p na area de um grafico
plot.new()
plot.window(0:1, 0:1)
mtext(substitute(paste("p", " " %~~% " ", valorP), 
                 list(valorP = round(chitestExp$p.value, precisao))),
      cex = escala * .7,
      side = 2, adj = 0)


# Processo Seletivo -----------------------------------------------
yrange <- c(0, 1)
yticks <- pretty(yrange, n = 2)
freqVector <- freqVecPSel16

xx <- barplot(freqVector,
        main = "Processo Seletivo 2016/1", 
        ylim = c(min(yticks), max(yticks)),
        cex.names = escala * .7,
        yaxt = "n",
        col = "violet")
axis(2, at = yticks, labels = yticks, cex.axis = escala * 0.7)
# Add text at top of bars
text(x = xx, y = freqVector, label = round(freqVector, 2), pos = 3, cex = escala * .7)

freqVector <- freqVecPSel17
xx <- barplot(freqVector,
        main = "Processo Seletivo 2017/1", 
        ylim = c(min(yticks), max(yticks)),
        cex.names = escala * .7,
        yaxt = "n",
        col = "violet")
axis(2, at = yticks, labels = yticks, cex.axis = escala * 0.7)
# Add text at top of bars
text(x = xx, y = freqVector, label = round(freqVector, 2), pos = 3, cex = escala * .7)

# Imprime valores p na area de um grafico
plot.new()
plot.window(0:1, 0:1)
mtext(substitute(paste("p", " " %~~% " ", valorP), 
                 list(valorP = round(chitestPSel$p.value, precisao))),
      cex = escala * .7,
      side = 2, adj = 0)


# Cota -----------------------------------------------
yrange <- c(0, max(c(freqVecCota16, freqVecCota17)))
yticks <- pretty(yrange, n = 2)
xlabels <- c("AC", "Ind. Renda", "Renda")
freqVector <- freqVecCota16
  
xx <- barplot(freqVector,
        main = "Cota 2016/1", 
        names = xlabels, 
        ylim = c(min(yticks), max(yticks)),
        cex.names = escala * .7,
        yaxt = "n",
        col = "violet")
axis(2, at = yticks, labels = yticks, cex.axis = escala * 0.7)
# Add text at top of bars
text(x = xx, y = freqVector, label = round(freqVector, 2), pos = 3, cex = escala * .7)

freqVector <- freqVecCota17
xx <- barplot(freqVector,
        main = "Cota 2017/1", 
        names = xlabels, 
        ylim = c(min(yticks), max(yticks)),
        cex.names = escala * .7,
        yaxt = "n",
        col = "violet")
axis(2, at = yticks, labels = yticks, cex.axis = escala * 0.7)
# Add text at top of bars
text(x = xx, y = freqVector, label = round(freqVector, 2), pos = 3, cex = escala * .7)

# Imprime valores p na area de um grafico
plot.new()
plot.window(0:1, 0:1)
mtext(substitute(paste("p", " " %~~% " ", valorP), 
                 list(valorP = round(chitestCota$p.value, precisao))),
      cex = escala * .7,
      side = 2, adj = 0)



#------------------------------------
# REGRESSAO LINEAR MULTIPLA
# Nota na Ufam ~ NOTAS DO ENEM
#------------------------------------

# IPC
lm16 <- lm(MEDIA_FINAL ~ NU_NOTA_CH + NU_NOTA_CN + NU_NOTA_L + NU_NOTA_M, data = df_2016)
summary(lm16)
lm17 <- lm(MEDIA_FINAL ~ NU_NOTA_CH + NU_NOTA_CN + NU_NOTA_L + NU_NOTA_M, data = df_2017)
summary(lm17)

# Calculo 1
lmCalc16 <- lm(IEM011 ~ NU_NOTA_CH + NU_NOTA_CN + NU_NOTA_L + NU_NOTA_M, data = df_2016)
summary(lmCalc16)
lmCalc17 <- lm(IEM011 ~ NU_NOTA_CH + NU_NOTA_CN + NU_NOTA_L + NU_NOTA_M, data = df_2017)
summary(lmCalc17)

# Algebra Linear 1
lmAL16 <- lm(IEM012 ~ NU_NOTA_CH + NU_NOTA_CN + NU_NOTA_L + NU_NOTA_M, data = df_2016)
summary(lmAL16)
lmAL17 <- lm(IEM012 ~ NU_NOTA_CH + NU_NOTA_CN + NU_NOTA_L + NU_NOTA_M, data = df_2017)
summary(lmAL17)

# CR
lmCR16 <- lm(CR ~ NU_NOTA_CH + NU_NOTA_CN + NU_NOTA_L + NU_NOTA_M, data = df_2016)
summary(lmCR16)
lmCR17 <- lm(CR ~ NU_NOTA_CH + NU_NOTA_CN + NU_NOTA_L + NU_NOTA_M, data = df_2017)
summary(lmCR17)
