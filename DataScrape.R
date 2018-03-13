# Attempt to extracty data from COlombian Parliamentary elctions ---------------------


# Libraries ---------------------------------------------------------------------------

library('XML');library('data.table')
library('rvest')

# URL -------------------------------------------------------------------------------

url.reg.nal<-"http://resultados2018.registraduria.gov.co/resultados/99SE/BXXXX/DSE99999.htm"

# Xpath in the website --------------------------------------------------------------

xpathCand<-'//*[@id="sol1"]/div[5]/div[3]/div[2]/table/tbody/tr[1]/th/div/div/text()'



xpathPart<-'//*[@id="sol1"]/div[5]/div[2]/div[1]/div/span[2]'


xpathVotos<-'//*[@id="sol1"]/div[5]/div[3]/div[2]/table/tbody/tr[1]/td[1]'


# trials -------------------------------------------------------------------------

regisNAl<-html(url.reg.nal)

str(regisNAl%>%
  html_nodes(".campo2de3TablaCandidatos , .nombreCandidato , img+ span")%>%
  html_text())


cands<-getNodeSet(doc1,xpathCand)

party<-getNodeSet(doc1,xpathPart)


xmlValue(party)
