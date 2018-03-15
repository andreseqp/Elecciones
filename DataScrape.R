# Attempt to extracty data from COlombian Parliamentary elctions ---------------------


# Libraries ---------------------------------------------------------------------------

library('data.table') # library('XML');
library('rvest')

# URL -------------------------------------------------------------------------------

url.reg.nal<-"http://resultados2018.registraduria.gov.co/resultados/99SE/BXXXX/DSE99999.htm"

# Xpath in the website --------------------------------------------------------------

xpathCand<-'//*[@id="sol1"]/div[5]/div[3]/div[2]/table/tbody/tr[1]/th/div/div/text()'


xpathPart<-'//*[@id="sol1"]/div[5]/div[2]/div[1]/div/span[2]'


xpathVotos<-'//*[@id="sol1"]/div[5]/div[3]/div[2]/table/tbody/tr[1]/td[1]'


# trials -------------------------------------------------------------------------

regisNAl<-read_html(url.reg.nal)

rawData<-regisNAl%>%
  html_nodes(".campo2de3TablaCandidatos , .nombreCandidato , img+ span")%>%
  html_text()

str(rawData)

candidatos.dirt<-gsub("\n","",grep("\n ",rawData,value = TRUE))

candidatos<-do.call(rbind,lapply(candidatos.dirt,function(x){
  full<-strsplit(x," ")
  nomb<-paste(grep("[[:alpha:]]",full[[1]],value = TRUE),collapse = " ")
  return(nomb)
}))

numero<-do.call(rbind,lapply(candidatos.dirt,function(x){
  full<-strsplit(x," ")
  numer<-grep("[[:digit:]]",full[[1]],value = TRUE)
  if(length(numer)>0){
      return(as.numeric(substr(numer,2,4)))
  }
  else{
    return(NA)
  }
}))

grep("[[:digit:]]",candidatos.dirt,invert = TRUE)
candidatos.dirt[numero[,1]!=numero[,2]]
numero[numero[,1]!=numero[,2],]
candidatos.dirt[numero[,1]!=numero[,2]]

dim(numero)[1]==length(candidatos.dirt)


head(candidatos)
votos<-grep("%",rawData,value = TRUE)
head(votos)

length(candidatos)==length(votos)

votos.int<-unlist(lapply(votos,function(list1)
  {strsplit(list1," ")[[1]][1]}))

gsub(".","",votos.int[962])

length(candidatos)==length(votos.int)

head(votos.int)

votos.perc<-unlist(lapply(votos,function(list1)
{
  tmp<-gsub(",",".",strsplit(list1," ")[[1]][2])
  return(as.numeric(substr(tmp,2,nchar(tmp)-2)))
}))


elecciones<-data.table(cbind(numero[,1],candidatos,votos.int,votos.perc))
names(elecciones)[1:2]<-c("numero","Nombre")



