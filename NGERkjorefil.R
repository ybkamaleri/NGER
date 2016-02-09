################
### StartFile
################
rm(list=ls())
# NGERData <- read.table('~/Dropbox/OUS/NGER/NewData/AlleVarNum2015-06-23 15-05-17.txt', sep=';', header=T)
NGERData <- read.table('~/Copy/OUS/NGER/20151208//AlleVarNum2015-12-08 12-57-33.txt', sep=';', header=T)

## Inndata til funksjon:
RegData <- NGERData
reshID <- 110734
minald <- 0	 #alder, fra og med
maxald <- 100 	#alder, til og med

MCEType <-   #1-Laparoskopi, 2-Hyseroskopi, 3-Begge

### Operasjon Dato
datoFra <- '2013-01-01'
datoTil <- '2014-01-02'

enhetsUtvalg <- 1
##0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet

valgtVar <- 'MaritalStatus'
## Education, MaritalStatus, PatientUndNorwegian, Age, BMI, OpEarlierVaginal,
## OpEarlierLaparoscopy, OpEarlierLaparatomy, OpType, Opcat, OpDaySurgery,
## OpOpcatOutsideDaytime, MCEType


outfile <- "" #paste(valgtVar, '.pdf', sep='')	#Navn angis av Jasper
libkat <- '~/Copy/OUS/NGER/Backup/'		#del av sti til bibliotekkatalog, før /lib/r/<funksjon.R>
setwd('~/Copy/OUS/NGER/Backup/')

source("NGERFigAndeler.R", encoding="UTF-8")

FigAndeler(RegData=RegData, datoFra=datoFra, valgtVar=valgtVar, datoTil=datoTil,
           reshID=reshID, enhetsUtvalg=enhetsUtvalg, libkat=libkat, outfile=outfile, 
           minald=minald, maxald=maxald)
