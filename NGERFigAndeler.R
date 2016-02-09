FigAndeler  <- function(RegData, valgtVar, datoFra='2000-04-01', datoTil='2050-12-31',
                        minald=0, maxald=130, libkat, outfile='', reshID, enhetsUtvalg=1, MCEType='')
{

    ## Søylediagram som viser andeler av ulike variable:

    ## Inngangsdata:
    ## 	  RegData - ei dataramme med alle nødvendige variable fra registeret
    ## 	  libkat - sti til bibliotekkatalog
    ##   outfile - navn på fil figuren skrives ned til
    ## 	  reshID - avdelingsid for egen avdeling, standard: 0-hele landet
    ## 	Brukerstyrt i Jasper:
    ## 		valgtVar - Må velges: ...
    ## 		minald - alder, fra og med
    ## 		maxald - alder, til og med
    ## 		datoFra <- '2010-01-01'    # min og max dato i utvalget vises alltid i figuren.
    ## 		datoTil <- '2013-05-25'
    ## 		valgtMaal - 'Med' = median. Alt annet gir gjennomsnitt
    ## 		enhetsUtvalg - 0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet,



### Trenger funksjonene LibFigFilType.R og LibUtvalg.R
    source(paste(libkat, 'LibFigFilType.R', sep=''), encoding="UTF-8")
    source(paste(libkat, 'NGERLibUtvalg.R', sep=''), encoding="UTF-8")
#     source('C:/SVN/jasper/nger_old/trunk/NGERLibUtvalg.R', encoding="UTF-8")

###------------Gjøre utvalg-------------------------
    ##Definerer registerspesifikke variable................
    RegData$Alder <- floor(difftime(Sys.Date(), as.Date(RegData$BirthDate, format = "%Y-%m-%d"), units = "days")/365)
    RegData$Alder <- as.numeric(RegData$Alder)
    RegData$OpEarlierLaparatomy <- RegData$OpEarlierPaparotomy
    
    RegData$InnDato <- as.POSIXct(RegData$OpDate, format="%Y-%m-%d") # %H:%M:%S" )	#"%d.%m.%Y"	"%Y-%m-%d"
    RegData$Variabel <- 0	#Fordi LibUtvalg trenger denne variabelen uansett
    names(RegData)[which(names(RegData)=='AVD_RESH')] <- 'ReshId' #Change var name

    shtxt <- switch(as.character(enhetsUtvalg),
                    '0' = 'Hele landet',
                    '1' = as.character(RegData$Avdeling[match(reshID, RegData$ReshId)]),
                    '2' = as.character(RegData$Avdeling[match(reshID, RegData$ReshId)]))

###############
### Variable
###############

    if (valgtVar %in% c("Education", "MaritalStatus", "Alder", "OpEarlierVaginal", "OpEarlierLaparoscopy",
                        "OpEarlierLaparatomy")) {
      RegData$Variabel <- RegData[ , valgtVar]
    }

    if (valgtVar == "PatientUndNorwegian") {
        RegData <- RegData[match(unique(RegData$PatientID), RegData$PatientID), ] #exclude duplikate PatientIDq
        RegData$Variabel <- RegData$PatientNorwegian
    }

    if (valgtVar == "BMI") {
        RegData <- RegData[match(unique(RegData$PatientID), RegData$PatientID), ] #exclude duplikate PatientIDq
        RegData$Variabel <- RegData$OpBMICategory
    }

    ## Per operasjoner

    if (valgtVar %in% c("OpType", "Opcat", 'OpOpcatOutsideDaytime', 'OpDaySurgery',
                        "MCEType")) {
        RegData$Variabel <- RegData[ , valgtVar]
    }

    
###Tar ut de med manglende registrering av valgt variabel og gjør utvalg (LibUtvalg)
    NGERUtvalg <- LibUtvalg(RegData = RegData, minald = minald, maxald = maxald, datoFra = datoFra, datoTil = datoTil, MCEType = MCEType)
    RegData <- NGERUtvalg$RegData
    utvalgTxt <- NGERUtvalg$utvalgTxt

    ## ## Per individer
    ## if (valgtVar %in% c("Education", "MaritalStatus", "Alder", "OpEarlierVaginal", "OpEarlierLaparoscopy",
    ##                     "OpEarlierLaparatomy")) {
    ##   RegData <- RegData[match(unique(RegData$PatientID), RegData$PatientID), ] #exclude duplikate PatientID
    ## }

    ##Hvis man ikke skal sammenligne, får man ut resultat for eget sykehus
    ##if ((sml == 0) & (egenavd == 1)) {RegData <- RegData[which(RegData$ReshId == reshID), ]}	#{indShUt <- which(RegData$ReshId != reshID)}
    if (enhetsUtvalg == 2) {RegData <- RegData[which(RegData$ReshId == reshID), ]}

###----------- Figurparametre ------------------------------
    cexgr <- 1  	#Kan endres for enkeltvariable
    retn <- 'V'		#Vertikal som standard. 'H' angis evt. for enkeltvariable
    grtxt <- ''		#Spesifiseres for hver enkelt variabel
    grtxt2 <- ''	#Spesifiseres evt. for hver enkelt variabel
    subtxt <- ''	#Benevning



    ##Hvis for få observasjoner..
    ##if (dim(RegData)[1] < 10 | (length(which(RegData$ReshId == reshID))<5 & egenavd==1)) {
    if (dim(RegData)[1] < 10 | (length(which(RegData$ReshId == reshID))<5 & enhetsUtvalg == 1)) {

###-----------Figur---------------------------------------
        FigTypUt <- figtype(outfile)
        farger <- FigTypUt$farger
        plot.new()
        title(main=paste('variabel: ', valgtVar, sep=''))	#, line=-6)
        legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
        text(0.5, 0.6, 'Færre enn 5 egne registreringer eller færre 10 totalt', cex=1.2)
        if ( outfile != '') {dev.off()}
    } else {


###--------------- Gjøre beregninger ------------------------------
        medSml <- 0
        utvalg <- c('Sh', 'Rest')	#Sh vil angi enhet, evt. hele landet hvis ikke gjøre sml, 'Rest' utgjør sammenligningsgruppa
        Andeler <- list(Sh = 0, Rest =0)

        ##Hvis det skal gjøres sammenligning:
        if (enhetsUtvalg == 1) {
            indSh <-which(RegData$ReshId == reshID)
            indRest <- which(RegData$ReshId != reshID)
            RegDataLand <- RegData
            ind <- list(Sh=indSh, Rest=indRest)
            medSml <- 1
        }

        for (teller in 1:(medSml+1)) {
            if (medSml == 1) {
                RegData <- RegDataLand[switch(utvalg[teller], Sh = ind$Sh, Rest=ind$Rest), ]
            }

###Variablene kjøres for angitt indeks, dvs. to ganger hvis vi skal ha sammenligning med Resten.

            if (valgtVar == 'Education') {
                tittel <- 'Utdanningsnivå'
                grtxt <- c('Grunnskole', 'VG', 'Fagskole', 'Universitet < 4 år', 'Universitet > 4 år', 'Ukjent')
                RegData$VariabelGr <- factor(RegData$Variabel, levels=1:6, labels = grtxt)
                retn <- 'H'
            }

            if (valgtVar == 'MaritalStatus') {
                tittel <- 'Sivilstatus'
                grtxt <- c('Enslig', 'Særboer', 'Samboer', 'Gift', 'Skilt', 'Enke', 'Ukjent')
               RegData$VariabelGr <- factor(RegData$Variabel, levels=c(1:6,9), labels=grtxt)
                retn <- 'H'
            }

            if (valgtVar == 'PatientUndNorwegian') {
                tittel <- 'Patient forstår og gjøre seg forstått på norsk'
                grtxt <- c('Nei', 'Ja', 'Delvis', 'Ukjent')
                RegData$VariabelGr <- factor(RegData$Variabel, levels = c(0:2,9), labels = grtxt)
                retn <- 'H'
            }

            if (valgtVar == 'BMI') {
                tittel <- 'BMI kategorier'
                grtxt <- c('Alvorlig undervekt','moderat undervekt', 'mild undervekt', 'normal vekt', 'overvekt',
                           'fedme kl.I', 'fedme kl.II', 'fedme kl.III')
                RegData$VariabelGr <- factor(RegData$Variabel, levels = 1:8, labels = grtxt)
                retn <- 'H'
            }

            if (valgtVar == 'Alder') {
                tittel <- 'Aldersfordeling'
                gr <- c(0, seq(15, 80, 5), 120)
                RegData$VariabelGr <- cut(RegData$Variabel, breaks = gr, include.lowest = TRUE, right = FALSE)
                grtxt <- c('<15', levels(RegData$VariabelGr)[2:(length(gr)-2)], '80+')
                subtxt <- 'Aldersgrupper'
                retn <- 'V'
            }

            if (valgtVar %in% c('OpEarlierVaginal', 'OpEarlierLaparoscopy', 'OpEarlierLaparatomy')) {
                tittel <- sprintf('Tidligere %s', switch(as.character(valgtVar),
                                                         'OpEarlierVaginal' = 'vaginale inngrep',
                                                         'OpEarlierLaparoscopy' = 'laparoskopiske inngrep',
                                                         'OpEarlierLaparatomy' = 'laparatomi'))
                grtxt <- c('Nei', 'Ja', 'Vet ikke')
                RegData$VariabelGr <- factor(RegData$Variabel, levels = c(0,1,9), labels = grtxt)
                retn <- 'H'
            }

            if (valgtVar == 'OpType') {
                tittel <- 'Operasjonstype'
                grtxt <- c('Primærinngrep', 'Reoperasjon')
                RegData$VariabelGr <- factor(RegData$Variabel, levels = 1:2, labels = grtxt)
                retn <- 'H'
            }

            
            if (valgtVar == 'Opcat') {
                tittel <- 'Operasjonskategori'
                grtxt <- c('Elektiv', 'Akutt', 'Øyeblikkelig hjelp')
                RegData$VariabelGr <- factor(RegData$Variabel, levels = 1:3, labels = grtxt)
                retn <- 'H'
            }

            if (valgtVar %in% c('OpOpcatOutsideDaytime', 'OpDaySurgery')) {
                tittel <- sprintf('%s', switch(as.character(valgtVar),
                                               'OpOpcatOutsideDaytime' = 'Operasjon i vakttid',
                                               'OpDaySurgery' = 'Dagkirurgiske Inngrep'))
                grtxt <- c('Nei', 'Ja')
                RegData$VariabelGr <- factor(RegData$Variabel, levels = 0:1, labels = grtxt)
                retn <- 'H'
            }

            
            if (valgtVar == 'MCEType') {
                tittel <- 'Operasjonsmetode'
                grtxt <- c('Laparoskopi', 'Hyseroskopi', 'Begge')
                RegData$VariabelGr <- factor(RegData$Variabel, levels = 1:3, labels = grtxt)
                retn <- 'H'
            }

            
        
        if (teller == 1) {Andeler$Sh <- 100*table(RegData$VariabelGr)/length(RegData$VariabelGr)
            Nsh <- dim(RegData)[1]}
        if (teller == 2) {Andeler$Rest <- 100*table(RegData$VariabelGr)/length(RegData$VariabelGr)
            Nrest <- dim(RegData)[1]}


    }	#for-løkke


###-----------Figur---------------------------------------
###Innparametre: subtxt, grtxt, grtxt2, tittel, Andeler, utvalgTxt, retn, cexgr


    ##Plottspesifikke parametre:
    FigTypUt <- figtype(outfile, fargepalett=NGERUtvalg$fargepalett)
    ##Tilpasse marger for å kunne skrive utvalgsteksten
    NutvTxt <- length(utvalgTxt)
    grtxtpst <- paste(rev(grtxt), ' (', rev(sprintf('%.1f',Andeler$Sh)), '%)', sep='')
    if (grtxt2 == '') {grtxt2 <- paste(sprintf('%.1f',Andeler$Sh), '%', sep='')}
    vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxtpst, units='figure', cex=cexgr)*0.7))
    ##vmarg <- max(0, strwidth(grtxtpst, units='figure', cex=cexgr)*0.7)
    par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

    farger <- FigTypUt$farger
    fargeSh <- farger[1]
    fargeRest <- farger[3]
    antGr <- length(grtxt)
    lwdRest <- 3	#tykkelse på linja som repr. landet
    cexleg <- 1	#Størrelse på legendtekst

    ##Horisontale søyler
    if (retn == 'H') {
        xmax <- min(max(c(Andeler$Sh, Andeler$Rest),na.rm=T)*1.25, 100)
        pos <- barplot(rev(as.numeric(Andeler$Sh)), horiz=TRUE, beside=TRUE, las=1, xlab="Andel pasienter (%)", #main=tittel,
                       col=fargeSh, border='white', font.main=1, xlim=c(0, xmax), ylim=c(0.05,1.4)*antGr)	#
        mtext(at=pos+0.05, text=grtxtpst, side=2, las=1, cex=cexgr, adj=1, line=0.25)

        if (medSml == 1) {
            points(as.numeric(rev(Andeler$Rest)), pos, col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
            legend('top', c(paste(shtxt, ' (N=', Nsh,')', sep=''), paste('Landet forøvrig (N=', Nrest,')', sep='')),
                   border=c(fargeSh,NA), col=c(fargeSh,fargeRest), bty='n', pch=c(15,18), pt.cex=2,
                   lwd=lwdRest,	lty=NA, ncol=1, cex=cexleg)
        } else {
            legend('top', paste(shtxt, ' (N=', Nsh,')', sep=''),
                   border=NA, fill=fargeSh, bty='n', ncol=1, cex=cexleg)
        }
    }

    if (retn == 'V' ) {
        ##Vertikale søyler eller linje
        ymax <- min(max(c(Andeler$Sh, Andeler$Rest),na.rm=T)*1.25, 100)
        pos <- barplot(as.numeric(Andeler$Sh), beside=TRUE, las=1, ylab="Andel pasienter (%)",
                       sub=subtxt,	col=fargeSh, border='white', ylim=c(0, ymax))
        mtext(at=pos, grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
        mtext(at=pos, grtxt2, side=1, las=1, cex=cexgr, adj=0.5, line=1.5)
        if (medSml == 1) {
            points(pos, as.numeric(Andeler$Rest), col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
            legend('top', c(paste(shtxt, ' (N=', Nsh,')', sep=''), paste('Landet forøvrig (N=', Nrest,')', sep='')),
                   border=c(fargeSh,NA), col=c(fargeSh,fargeRest), bty='n', pch=c(15,18), pt.cex=2, lty=c(NA,NA),
                   lwd=lwdRest, ncol=2, cex=cexleg)
        } else {
            legend('top', paste(shtxt, ' (N=', Nsh,')', sep=''),
                   border=NA, fill=fargeSh, bty='n', ncol=1, cex=cexleg)
        }
    }


    title(tittel, line=1, font.main=1)

###Tekst som angir hvilket utvalg som er gjort
    avst <- 0.8
    utvpos <- 3	#Startlinje for teksten
    mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

    par('fig'=c(0, 1, 0, 1))
    if ( outfile != '') {dev.off()}

}
}
