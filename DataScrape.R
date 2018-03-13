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

candidatos<-grep("\n ",rawData,value = TRUE)
votos<-grep("%",rawData,value = TRUE)

length(candidatos)==length(votos.int)

votos.int<-unlist(lapply(votos,function(list1)
  {as.numeric(strsplit(list1," ")[[1]][1])}))

votos.perc<-unlist(lapply(votos,function(list1)
{substr(strsplit(list1," ")[[1]][2])}))







