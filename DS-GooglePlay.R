#Inicializando Valores
#install.packages("rjson")
numberOfLines <- 50000
library("rjson")

startTime <- Sys.time()
file = readLines(  "/Users/phalves/Documents/DS-BancoGooglePlay/playtore2015.json", numberOfLines)
endReadTime <- Sys.time()
readTime <- endReadTime - startTime
readTime

sizeFile <- length(file)

#Criando o vetor de contagem para o Histograma e identificando o app com mais reviews
countVet <- c()
maxScore <- 0
category <- fromJSON(file[1])$Category
#category <- substr(fromJSON(file[1])$Category,22,100)

#Criando o vetor de contagem para o Histograma e identificando o app com mais Downloads
countVetDownloads <- c()
maxDownloads <- 0
maxDownloadsApps <- c()

#Para criar o dataframe
jsonFields<-c()
colunasAppSize<-c()
colunasCategory<-c()
colunasContentRating<-c()
colunasHaveInAppPurchases<-c()
colunasIsFree<-c()
colunasMinimumOSVersion<-c()


for(i in 1:sizeFile)
{
  jsonSet <- fromJSON( file[i] )
  sizeCategory <- length(category)
  flag <- TRUE
  
  for (j in 1:sizeCategory)
    if(category[j]==jsonSet$Category)
      flag <- FALSE
  
  if (flag==TRUE){
    category <- c(category,jsonSet$Category)
  }
}

#Criando matriz dos Apps por categorias
mat=matrix(data=0L,ncol=4,nrow=sizeCategory)
for(j in 1:sizeCategory)
  mat[j,1]<-category[j]

#Alterando nome das colunas
colnames(mat) <- c("Category", "#Apps", "#Reviews", "ValorR$")

#Contando Apps por categoria - Numero de reviews - Preco aproximado
for(i in 1:sizeFile)
{
  jsonSet <- fromJSON( file[i] )
  for (j in 1:sizeCategory){
    if(jsonSet$Category == category[j]){
      mat[j,2]<-as.numeric(mat[j,2])+1;
      mat[j,3]<-as.numeric(mat[j,3])+jsonSet$Reviewers;
      mat[j,4]<-as.numeric(mat[j,4])+(jsonSet$Reviewers*jsonSet$Price);
    }
  }
  
  countVet <- c(countVet, jsonSet$Score$Count)
  if(jsonSet$Score$Count>maxScore){
    maxScore <- jsonSet$Score$Count
    appMaxScore <- jsonSet
  }
  
  #Removendo as virgulas para realizar a ordenacao
  instalationClean <- gsub(",", "",jsonSet$Instalations)
  
  countVetDownloads <- c(countVetDownloads, instalationClean)
  if(instalationClean>maxDownloads){
    maxDownloads <- instalationClean
    appMaxDownload <- jsonSet
    
    #NAO ESTA FUNFANDO!
    if("500000000 - 1000000000"==maxDownloads)
      maxDownloadsApps <- c(maxDownloadsApps,jsonSet)
  }
  
  if(length(jsonFields)==0)
    jsonFields <- attributes(jsonSet)
  
  
  colunasAppSize<-c(colunasAppSize,jsonSet$AppSize)
  colunasCategory<-c(colunasCategory,substr(jsonSet$Category,22,100))
  colunasContentRating<-c(colunasContentRating,jsonSet$ContentRating)
  colunasHaveInAppPurchases<-c(colunasHaveInAppPurchases,jsonSet$HaveInAppPurchases)
  colunasIsFree<-c(colunasIsFree,jsonSet$IsFree)
  colunasMinimumOSVersion<-c(colunasMinimumOSVersion,jsonSet$MinimumOSVersion)
  
  
}

#Criando o Dataframe
dt<-data.frame(colunasAppSize,colunasCategory,colunasContentRating,colunasHaveInAppPurchases,colunasIsFree,colunasMinimumOSVersion)

#Montando histogramas
#hist(dt[,1],xlim=c(-1,200))

#PARAMETROS PARA O PLOT
#plot(table(factor(dt[,3])))
originalParameters <- par()
par(mar=c(15,5,1,1)+0.1)

#AppSize
plot(table(factor(dt[,1])),main="AppSize")

#Category
plot(table(factor(dt[,2])),main="Category", las=2, ylab="# Apps", ylim=c(0,max(table(factor(dt[,2])))*1.2))
#axis(2, at=seq(1,50, by=5))

#ContentRating
barplot(table(factor(dt[,3])),main="ContentRating", ylim=c(0,max(table(factor(dt[,3])))*1.2))

#HaveInAppPurchases
#plot(table(factor(dt[,4])),main="HaveInAppPurchases")
barplot(table(dt[,4]), horiz=TRUE, las=0, main="HaveInAppPurchases", xlim=c(0,max(table(factor(dt[,4])))*1.2))

#IsFree
barplot(table(factor(dt[,5])),main="IsFree", ylim=c(0,max(table(factor(dt[,5])))*1.2))

#MinimumOSVersion
barplot(table(factor(dt[,6])),main="MinimumOSVersion", las=2, ylim=c(0,max(table(factor(dt[,6])))*1.2))



appMaxScore$Name
appMaxScore$Instalations

appMaxDownload$Name
appMaxDownload$Instalations

#Separando as faixas de download
levels <- countVetDownloads[!duplicated(countVetDownloads)]
#FIM: Separando as faixas de download

#Numero de Apps nas faixas de valores de instalacao
countFactorVetDownloads = factor(countVetDownloads)
barplot(table(countFactorVetDownloads),las=2)

#Tempo total de execucao
endTime <- Sys.time()
totalTime <- endTime-startTime
totalTime


# Qual categoria de monetização é mais popular/ bem avaliada em cada categoria de aplicativo?
# Criar array com apps Free 
# Criar array com apps Pagos
# Fazer a média de estrelas (Score$Total) dos apps Free
# Fazer a média de estrelas (Score$Total) dos apps Pagos

#Especializacao...
# Criar array com apps Free com itens pagos (HaveInAppPurchases)
# Criar array com apps Pagos com itens pagos (HaveInAppPurchases)
# Fazer a média de estrelas (Score$Total) dos apps Free com itens pagos (HaveInAppPurchases)
# Fazer a média de estrelas (Score$Total) dos apps Pagos com itens pagos (HaveInAppPurchases)

# Analisar nível de interação do usuário de acordo com o tipo de monetização
# Usar array com apps Free
# Usar array com apps Pagos
# Fazer a média de "counts" (reviews feitos Score$Count) dos apps Free
# Fazer a média de "counts" (reviews feitos Score$Count) dos apps Pagos



# #TESTE PLOT
# par(mar=c(4,4,4,7))
# hist(table(countVetDownloads),breaks=6)
# 
# par(mar=c(4,4,4,7))
# hist(countVet,breaks=10)