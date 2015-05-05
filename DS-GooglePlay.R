#Inicializando Valores
#install.packages("rjson")
library("rjson")
file = readLines(  "/Users/phalves/Documents/DS-BancoGooglePlay/playtore2015.json" )
countFree <- 0
countPaid <- 0
sizeFile <- length(file)
#FIM: Inicializando Valores

#Listando categorias
for(i in 1:sizeFile)
{
  jsonSet <- fromJSON( file[i] )
  sizeCategory <- length(category)
  flag <- TRUE
  
  for (j in 1:sizeCategory)
    if(category[j]==jsonSet$Category)
      flag <- FALSE
  
  if (flag==TRUE)
    category <- c(category,jsonSet$Category)
  
  if(jsonSet$Price == 0)
    countFree <- countFree+1
  else
    countPaid <- countPaid+1
}
#FIM: Listando categorias


#Contagem por categoria
mat=matrix(data=c(category)),ncol=1)
mat<-cbind(mat,mat[,1]<-0L)

for(i in 1:sizeFile)
{
  jsonSet <- fromJSON( file[i] )
  for (j in 1:sizeCategory)
    if(jsonSet$Category == category[j]){
      mat[j,2]<-as.numeric(mat[j,2])+1;
    }
}

#Limpeza de categoria
mat<-mat[-43,1:2]
#FIM: Limpeza de categoria
#FIM: Contagem por categoria

#Maior numero de downloads
maiorDownload <- which(mat == max(as.numeric(mat[,2])), arr.ind = TRUE)
mat[maiorDownload[1,1],]

#Menor numero de downloads
menorDownload <- which(mat == min(as.numeric(mat[,2])), arr.ind = TRUE)
mat[menorDownload[1,1],]

countFree
countPaid

#REVIEWS
#Adicionando coluna de Reviews
mat<-cbind(mat,mat[,1]<-0L)

#Somando reviews
for(i in 1:sizeFile)
{
  jsonSet <- fromJSON( file[i] )
  for (j in 1:sizeCategory)
    if(jsonSet$Category == category[j]){
      mat[j,3]<-as.numeric(mat[j,3])+jsonSet$Reviewers;
    }
}
#Mais Reviews
maiorReview <- which(mat == max(as.numeric(mat[,3])), arr.ind = TRUE)
mat[maiorReview[1,1],]

#Menos Reviews
menorReview <- which(mat == min(as.numeric(mat[,3])), arr.ind = TRUE)
mat[menorReview[1,1],]
#FIM: REVIEWS


#Adicionando coluna de valores
mat<-cbind(mat,mat[,1]<-0L)

#Somando valores
for(i in 1:sizeFile)
{
  jsonSet <- fromJSON( file[i] )
  for (j in 1:sizeCategory)
    if(jsonSet$Category == category[j]){
      mat[j,4]<-as.numeric(mat[j,4])+(jsonSet$Reviewers*jsonSet$Price);
    }
}

#Maior Valor (#reviews*preco)
maiorValor <- which(mat == max(as.numeric(mat[,4])), arr.ind = TRUE)
mat[maiorValor[1,1],]

#Menor Valor
menorValor <- which(mat == min(as.numeric(mat[,4])), arr.ind = TRUE)
mat[menorValor[1,1],]

#Alterando nome das colunas
colnames(mat) <- c("Category", "#Apps", "#Reviews", "ValorR$")
