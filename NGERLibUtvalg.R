LibUtvalg <- function(RegData, datoFra, datoTil, fargepalett='BlaaOff', minald, maxald, MCEType)
{

    ## Hvis "Variabel" ikke definert
    "%i%" <- intersect
    if (length(which(names(RegData) == 'Variabel')) == 0 ) {RegData$Variabel <- 0}
    Ninn <- dim(RegData)[1]

    indVarMed <- which(RegData$Variabel !='NA') %i% which(RegData$Variabel != 'NaN') %i% which(RegData$Variabel != '') %i% which(!is.na(RegData$Variabel))

    indAld <- which(RegData$Alder >= minald & RegData$Alder <= maxald)
    indDato <- which(RegData$InnDato >= as.POSIXlt(datoFra) & RegData$InnDato <= as.POSIXlt(datoTil))

    indMCE <- if (MCEType %in% c(1:3)){which(RegData$MCEType == MCEType)
              } else {indMCE <- 1:Ninn}

    ##index to be included

    indMed <- indAld %i% indDato %i% indMCE %i% indVarMed 

    RegData <- RegData[indMed,]

    N <- dim(RegData)[1]


    utvalgTxt <- c(paste('Operasjonsdato: ', if (N>0) {min(RegData$InnDato, na.rm=T)} else {datoFra},
                         ' til ', if (N>0) {max(RegData$InnDato, na.rm=T)} else {datoTil}, sep='' ),
                   if ((minald>0) | (maxald<130))
                   {paste('Pasienter fra ', if (N>0) {min(RegData$Alder, na.rm=T)} else {minald},
                          ' til ', if (N>0) {max(RegData$Alder, na.rm=T)} else {maxald}, ' år', sep='')},
                   if (MCEType %in% c(1:3)){paste('Operasjonsmetode: ', c('Laparoskopi', 'Hysteroskopi', 'Begge')[MCEType], sep='')})
    

    UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt, fargepalett=fargepalett) #GronnHNpms624,
    return(invisible(UtData))
}
