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
candidatos.ids<-grep("\n ",rawData)

length(candidatos.dirt)==length(candidatos.ids)

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


votos.int<-as.numeric(gsub(".","",votos.int,fixed = TRUE))

length(candidatos)==length(votos.int)

head(votos.int)

votos.perc<-unlist(lapply(votos,function(list1)
{
  tmp<-gsub(",",".",strsplit(list1," ")[[1]][2])
  return(as.numeric(substr(tmp,2,nchar(tmp)-2)))
}))


elecciones<-data.table(numero[,1],candidatos,votos.int,votos.perc,candidatos.ids)
names(elecciones)[1:2]<-c("numero","Nombre")

rawData[1:50]
partidos<-rawData
for(filt in c("\n","%")){
  partidos<-grep(filt,partidos,value = TRUE,invert = TRUE)
}
ids.partidos<-do.call(rbind,lapply(partidos,grep,x = rawData,fixed=TRUE))
partidos.df<-data.frame(partidos,ids.partidos)
str(partidos.df)

elecciones$partido<-rep("NA",dim(elecciones)[1])
for (id in 2:dim(partidos.df)[1]) {
  elecciones[candidatos.ids<partidos.df[id,"ids.partidos"] & 
               candidatos.ids>partidos.df[id-1,"ids.partidos"],partido:=partidos[id-1]]
}
elecciones[partido=="NA",partido:=partidos[23]]

tot.votos.part.reg<-14474450
votos.blanc.reg<-835445
votos.blanc.reg.ind<-340798


elecciones[,votos.perc.calc:=100*prop.table(votos.int)]
elecciones[grep("solo por la lista",Nombre,ignore.case = TRUE)]

elecciones.senado<-elecciones[partido%in%unique(partido)[1:16],]
elecciones.senado.indig<-elecciones[partido%in%unique(partido)[17:23],]


elecciones.senado[,':='(votos.perc.calc=100*prop.table(votos.int),
                         votos.perc.blanc=100*votos.int/(sum(votos.int)+votos.blanc.reg))]
elecciones.senado.indig[,':='(votos.perc.calc=100*prop.table(votos.int),
                        votos.perc.blanc=100*votos.int/(sum(votos.int)+votos.blanc.reg.ind))]

elecciones.senado[,sum(votos.int)]
elecciones.senado.indig[,sum(votos.int)]


partidos.senado<-elecciones.senado[,.(votos.int=sum(votos.int),
                               votos.perc=sum(votos.perc),
                               votos.perc.calc=sum(votos.perc.calc),
                               votos.perc.blanc=sum(votos.perc.blanc)),
                               by=partido]

elecciones.senado[,.(sum(votos.perc),sum(votos.perc.calc),sum(votos.perc.blanc))]

partidos.senado[,.(sum(votos.perc),sum(votos.perc.calc),sum(votos.perc.blanc))]

100*votos.blanc.reg/(votos.blanc.reg+elecciones.senado[,sum(votos.int)])

with(elecciones.senado[grep("centro democrático",partido,ignore.case = TRUE)],{
  par(las=1,xpd=TRUE)
  cd.bar<<-barplot(votos.perc.blanc[order(votos.perc.blanc,decreasing = TRUE)]
          ,col = c(rep('blue',20),rep('black',length(votos.int)-20)))
  arrows(x1=cd.bar[numero[order(votos.perc.blanc,decreasing = TRUE)]==1]
         ,y1=votos.perc.blanc[numero==1],
         y0=votos.perc.blanc[numero==1]+0.2,
         x0=cd.bar[numero[order(votos.perc.blanc,decreasing = TRUE)]==1]+7,
         lwd=3)
  text(x = cd.bar[numero[order(votos.perc.blanc,decreasing = TRUE)]==1]+7,
       y=votos.perc.blanc[numero[order(votos.perc.blanc,decreasing = TRUE)]==1],
       labels = Nombre[numero[order(votos.perc.blanc,decreasing = TRUE)]==1],
       adj = c(-0.2,-1))
  arrows(x1=cd.bar[grep("solo por la lista",
                        Nombre[order(votos.perc.blanc,decreasing = TRUE)],
                        ignore.case = TRUE)],
         y1=votos.perc.blanc[grep("solo por la lista",
                                  Nombre,
                                  ignore.case = TRUE)],
         y0=votos.perc.blanc[grep("solo por la lista",
                                  Nombre,
                                  ignore.case = TRUE)]+0.2,
         x0=cd.bar[grep("solo por la lista",
                        Nombre[order(votos.perc.blanc,decreasing = TRUE)],
                                  ignore.case = TRUE)]+7,lwd=3)
  text(x = cd.bar[grep("solo por la lista",
                       Nombre[order(votos.perc.blanc,decreasing = TRUE)],
                       ignore.case = TRUE)]+7,
       y=votos.perc.blanc[grep("solo por la lista",
                               Nombre,
                               ignore.case = TRUE)],
       labels = Nombre[grep("solo por la lista",
                            Nombre,
                            ignore.case = TRUE)],
       adj = c(-0.2,-1))
})

with(elecciones.senado[grep("cambio radical",partido,ignore.case = TRUE)],{
  par(las=1,xpd=TRUE)
  cd.bar<<-barplot(votos.perc.blanc[order(votos.perc.blanc,decreasing = TRUE)]
                   ,col = c(rep('blue',17),rep('black',length(votos.int)-20)))
  arrows(x1=cd.bar[numero[order(votos.perc.blanc,decreasing = TRUE)]==1]
         ,y1=votos.perc.blanc[numero==1],
         y0=votos.perc.blanc[numero==1]+0.1,
         x0=cd.bar[numero[order(votos.perc.blanc,decreasing = TRUE)]==1]+1,
         lwd=3)
  text(x = cd.bar[numero[order(votos.perc.blanc,decreasing = TRUE)]==1]+7,
       y=votos.perc.blanc[numero==1],
       labels = Nombre[numero==1])
  arrows(x1=cd.bar[grep("solo por la lista",
                        Nombre[order(votos.perc.blanc,decreasing = TRUE)],
                        ignore.case = TRUE)],
         y1=votos.perc.blanc[grep("solo por la lista",
                                  Nombre,
                                  ignore.case = TRUE)],
         y0=votos.perc.blanc[grep("solo por la lista",
                                  Nombre,
                                  ignore.case = TRUE)]+0.2,
         x0=cd.bar[grep("solo por la lista",
                        Nombre[order(votos.perc.blanc,decreasing = TRUE)],
                        ignore.case = TRUE)]+7,lwd=3)
  text(x = cd.bar[grep("solo por la lista",
                       Nombre[order(votos.perc.blanc,decreasing = TRUE)],
                       ignore.case = TRUE)]+7,
       y=votos.perc.blanc[grep("solo por la lista",
                               Nombre,
                               ignore.case = TRUE)],
       labels = Nombre[grep("solo por la lista",
                            Nombre,
                            ignore.case = TRUE)],
       adj = c(-0.2,-1))
})

elecciones.senado[grep("cambio radical",partido,ignore.case = TRUE),]
