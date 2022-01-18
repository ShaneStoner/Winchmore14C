#Welcome!
library(SoilR)
library(doParallel)
library(gridExtra)
#library(pROC)
#library(cvAUC)
library(networkD3)
library(rcompanion)
library(dplyr)
library(reshape2)
library(ggplot2)
library(FME)
library(grid)
library(gridExtra)
library(gplots)
library(DescTools)
library(diagram)
registerDoParallel(6)

setwd('/Users/shane/14Constraint Dropbox/Shane Stoner/IMPRS/NZ')
load('/Users/shane/14Constraint Dropbox/Shane Stoner/IMPRS/NZ/kTurnover14C.Rdata')
load('/Users/shane/14Constraint Dropbox/Shane Stoner/IMPRS/NZ/SteadyFrame.Rdata')
atm14C = read.csv('/Users/shane/14Constraint Dropbox/Shane Stoner/IMPRS/NZ/atm14C_SH.csv', sep=',')
winchIrrC = read.csv('/Users/shane/14Constraint Dropbox/Shane Stoner/IMPRS/NZ/WinchCN_timeCondron855.csv')
soilMass <- 0.075 * 10000 * 1.14 #1.0375 g/cm3, 1.14 used in Schipper et al. 2013, 1.0375 from another publication
alphaVal = 150 #Transparency of colors
AGmod = 0.29 #Amount of above ground production entering soil as litter + feces
rootmod = 0.7 #Amount of root mass found in top 7.5 cm as function of those found in top 20 cm
irrInputs <- c(dry = 2.212, irr10 = 2.659, irr20 = 2.862)
fertInputs <- c(con = 1.91, res = 2.7, high = 2.996)
fertInputs <- c(con = 2.005, res = 2.81, high = 3.1085)
rootProd <- c(Dry = 2, Irr10 = 2, Irr20 = 2.1, Unfert = 1.9, ResFert = 2.1, HighFert = 2.3)

allInputs <- read.csv('/Users/shane/14Constraint Dropbox/Shane Stoner/IMPRS/NZ/InputsTime_TBdata.csv')
allInputs$Trial = factor(allInputs$Trial, levels = unique(allInputs$Trial), ordered = TRUE)
ggplot(allInputs, aes(Year, Input, color = Trial)) + geom_line() + theme_bw() + geom_smooth()

##Construct input data frame with yearly production values
#Fill in all levels with mean
InFrame <- data.frame(
  Unfert = rep(2.0 , length(seq(1958, 2010))),
  ResFert = rep(4 , length(seq(1958, 2010))),
  HighFert = rep(4.9 , length(seq(1958, 2010))),
  Dry = rep(2.8 , length(seq(1958, 2010))),
  Irr10 = rep(4.1 , length(seq(1958, 2010))),
  Irr20 = rep(4.8 , length(seq(1958, 2010)))
)

InFrame <- melt(InFrame)
InFrame$Year <- rep(seq(1958, 2010), length(trialList))
colnames(InFrame) <- c('Trial', 'Input', 'Year')

for(t in levels(allInputs$Trial)){
  for(i in dplyr::filter(allInputs, Trial == t & Year >= 1958)$Year){
    InFrame[(InFrame['Trial'] == t & InFrame['Year']==i),]$Input <- mean(allInputs[(allInputs['Trial'] == t & allInputs['Year'] == i),]$Input)
  }
}

ggplot(InFrame, aes(x = Year, y = Input, color = Trial)) + geom_line(size = 2) +
  ggtitle('Annual Above Ground Production') + ylab('Tonnes C Per Hectare') + theme_bw() + geom_smooth(se = FALSE, lty = 2) +
  scale_color_manual(values = c(unfertcols[2], rescols[2], highcols[2], drycols[2], irr10cols[2], irr20cols[2])) +
  theme(axis.text=element_text(size=14), legend.text = element_text(size = 12), panel.background = element_blank(),
        legend.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 18, face = 'bold'),
        axis.title=element_text(size=14,face="bold"),
        strip.text.y = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

#Annual Inputs, adjusted for AG and BG return/production
InputWin <- data.frame(Trial = character(), Input = double(), Year = integer())
for(t in c("Dry","Irr10","Irr20","Unfert","ResFert","HighFert")){
  InputWin <-
    rbind(InputWin, data.frame(Trial = t,
                               Input = (dplyr::filter(InFrame, Trial == t & Year >= trialStart & Year <= trialEnd)$Input * AGmod) +
                                 (rootProd[t] * rootmod),
                               Year = seq(trialStart, trialEnd)))
}

ggplot(InputWin, aes(x = Year, y = Input, col = Trial)) + geom_line(size = 1.5) +
  ggtitle('Annual Model Inputs') + ylab('Tonnes C Per Hectare') + theme_bw() +
  geom_smooth(se = FALSE, lty =2) +
  scale_color_manual(values = c(drycols[2], irr10cols[2], irr20cols[2],unfertcols[2], rescols[2], highcols[2])) +
  theme(axis.text=element_text(size=14), legend.text = element_text(size = 12), panel.background = element_blank(),
        legend.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 18, face = 'bold'),
        axis.title=element_text(size=14,face="bold"),
        strip.text.y = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


#Initialize log file
txtname <- paste0('WinchmoreModelRuns_', Sys.Date(), '.txt')
writeLines(c(""), txtname)

#Specify file name for output PDF
filename <- paste0('PostSubmission_',Sys.Date(),'_')

#Select trials (will all run in parallel)
trialList = c('Dry', 'Irr. 20', 'Unfert', 'Res. Fert', 'High Fert', 'Irr. 10')
#trialList = c('Unfert')

#Select beginning and end of modeled time window (between 1958 and 2010)
starts = c(1958, 1985)
ends = c(1992, 2010)

starts = c(1985, 1958, 1958)
ends = c(2010, 1992, 2010)

starts = c(1979, 1985)
ends = c(2010, 2010)

xs <- seq(1, length(starts))

#Let's go
foreach(trial = trialList) %dopar% {
  for(trial in trialList){
  #To prevent errors and duplicate file names erasing one another (Irr. 10 and Irr. 20)
  jk <- substr(trial, 1, 3)
  if(trial == 'Irr. 10'){
    jk = 'Ir1'
  }

  #Parameters for Bayesian / MCMC
  niter <- 10000
  burnin <- 5000
  updatecov <- 10
  outLength <- 1000
  pseudo_n <- 8000

  for(x in xs){

    #Begin parallel writing to same log .txt file
    #sink(txtname, append = TRUE)
    #print(Sys.time())
    #print(trial)

    #Make and abbreviated trial name
    jk <- substr(trial, 1, 3)
    if(trial == 'Irr. 10'){
      jk = 'Ir1'
    }

    #trialStart <- starts[x]
    #trialEnd <- ends[x]

    if(trial == 'High Fert'){
      winchFertC <- read.csv('./WinchFertC_full.csv', sep = ',')

      soilC <- data.frame(winchFertC$year, ((winchFertC$high / 100) *soilMass), ((winchFertC$highC_err / 100) *soilMass))
      colnames(soilC) <- c('time', 'soilC','err')

      #Manual data input
      dattime = c(Winch14C$year, trialEnd)
      dathighfert<- c(Winch14C$f_high, NA)
      daterr <- c(Winch14C$high_err, NA)

      dat = data.frame(dattime, dathighfert, daterr)
      colnames(dat) <- c('time', 'soilc14', 'err')

      if(roots == FALSE){
        AGprod <- filter(InFrame, Trial == 'HighFert' & Year >= trialStart & Year <= trialEnd)[,-1]
        inputs <- data.frame(Year = AGprod$Year,inputs = (2.3 * rootmod) + (AGprod$Input * AGmod))
      } else {
        inputs = 2.3
      }

      years = seq(trialStart, trialEnd)

      Ctotal= soilC[soilC$time == trialStart+1,][2]

      #Initial bulk soil 14C: -26.4

      nospace <- 'highFert'

      F14C0 = F14C0_fert

      highcols <- c(rgb(177,0,38,alphaVal,,255), rgb(177,0,38,,,255)) # Highfert
      cols <- c('salmon1', 'darkorange3') # Highfert

      atm14Cclip = atm14C[atm14C[,1] >= trialStart-1, 1:2]
      atm14Cclip = atm14Cclip[atm14Cclip[,1] <= trialEnd, 1:2]
      atm_in = data.frame(atm14Cclip[,1:2])

      # Remove rows with NAs
      dat<- dat[-c(4,6,8,10,14),]
      dathighfert <- dat
      soilC <- soilC[-c(1,2),]
      soilC$err[16] <- 1.17305
      soilChigh <- soilC

      inputHigh <- inputs

      fertInputs[3] <- mean(inputs$inputs)
    }

    if(trial == 'Res. Fert'){
      winchFertC <- read.csv('./WinchFertC_full.csv', sep = ',')

      soilC <- data.frame(winchFertC$year, ((winchFertC$res / 100) *soilMass), (winchFertC$resC_err / 100) *soilMass)
      colnames(soilC) <- c('time', 'soilC', 'err')

      #Manual data input
      dattime = c(Winch14C$year, trialEnd)
      datresfert<- c(Winch14C$f_res, NA)
      daterr <- c(Winch14C$res_err, NA)

      dat = data.frame(dattime, datresfert, daterr)
      colnames(dat) <- c('time', 'soilc14', 'err')

      if(roots == FALSE){
        AGprod <- filter(InFrame, Trial == 'ResFert' & Year >= trialStart & Year <= trialEnd)[,-1]
        inputs <- data.frame(Year = AGprod$Year,inputs = (2.1 * rootmod) + (AGprod$Input * AGmod))
      } else {
        inputs = 1.9
      }

      years = seq(trialStart, trialEnd)

      Ctotal= soilC[soilC$time == trialStart,][2]

      #Initial bulk soil 14C: -26.4

      nospace <- 'resFert'

      F14C0 = F14C0_fert

      rescols <- c(rgb(252,78,42,alphaVal,,255), rgb(252,78,42,,,255))

      atm14Cclip = atm14C[atm14C[,1] >= trialStart-1, 1:2]
      atm14Cclip = atm14Cclip[atm14Cclip[,1] <= trialEnd, 1:2]
      atm_in = data.frame(atm14Cclip[,1:2])

      dat<- dat[-c(4,6,8,10,14),]
      datresfert <- dat
      soilC <- soilC[-c(1,2),]
      soilCres <- soilC

      inputRes <- inputs

      fertInputs[2] <- mean(inputs$inputs)
    }

    if(trial == 'Unfert'){
      winchFertC <- read.csv('./WinchFertC_full.csv', sep = ',')
      soilC <- data.frame(winchFertC$year, ((winchFertC$unfert / 100) *soilMass), ((winchFertC$unC_err / 100)*soilMass))
      colnames(soilC) <- c('time', 'soilC','err')

      #Manual data input
      dattime = c(Winch14C$year, trialEnd)
      datunfert<- c(Winch14C$f_con, NA)
      daterr <- c(Winch14C$con_err, NA)

      dat = data.frame(dattime, datunfert, daterr)
      colnames(dat) <- c('time', 'soilc14', 'err')

      if(roots == FALSE){
        AGprod <- filter(InFrame, Trial == 'Unfert' & Year >= trialStart & Year <= trialEnd)[,-1]
        inputs <- data.frame(Year = AGprod$Year,inputs = (1.9 * rootmod) + (AGprod$Input * AGmod))
      } else {
        inputs = 1.9
      }

      years = seq(trialStart, trialEnd)

      Ctotal= soilC[soilC$time == trialStart,][2]

      #Initial bulk soil 14C: -27.3

      nospace <- 'unfert'

      F14C0 = F14C0_fert

      unfertcols <- c(rgb(254,178,76,alphaVal,,255), rgb(254,178,76,,,255))
      cols <- c('cornsilk3', 'cornsilk4', 'cornsilk') # Unfert

      atm14Cclip = atm14C[atm14C[,1] >= trialStart - 1, 1:2]
      atm14Cclip = atm14Cclip[atm14Cclip[,1] <= trialEnd, 1:2]
      atm_in = data.frame(atm14Cclip[,1:2])

      dat<- dat[-c(4,6,8,10,14),]
      datunfert <- dat
      soilC <- soilC[-c(1,2),]
      soilCun <- soilC

      inputUn <- inputs

      fertInputs[1] <- mean(inputs$inputs)
    }

    if(trial == 'Irr. 20'){

      soilC <- data.frame(winchIrrC$year, ((winchIrrC$perC_irr20 / 100) *soilMass), ((winchIrrC$irr20C_err / 100) *soilMass))
      colnames(soilC) <- c('time', 'soilC', 'err')


      #Manual data input
      time20 = c(1949,1959, 1961, 1967, 1971, 1975, 1980, 1986, 1991, 1997, 2002, 2003)
      dattime = c(1949,1959, 1961, 1967, 1971, 1975, 1980, 1986, 1991, 1997, 2002, 2003)
      dat20 = c(NA,-36.6, -10.1, 156.5, 203.3, 203.3, 185.9, 156.1, 135.2, 109, 93.7, NA)
      dat20_13 = c(NA,-26.99,-27.26,-27.42, -27.63,-27.45,-27.64,-27.54,-27.96,-27.86,-28.25,NA)

      dattime = c(Winch14C$year, trialEnd)
      dat20<- c(Winch14C$i_20, NA)
      daterr <- c(Winch14C$i20_err, NA)

      dat = data.frame(dattime, dat20, daterr)
      colnames(dat) <- c('time', 'soilc14', 'err')

      if(roots == FALSE){
        AGprod <- dplyr::filter(InFrame, Trial == 'Irr20' & Year >= trialStart & Year <= trialEnd)[,-1]
        inputs <- data.frame(Year = AGprod$Year,inputs = (2.1 * rootmod) + (AGprod$Input * AGmod))
      } else {
        inputs = 2.1
      }

      #From Kelliher et al. 2012
      # root <- 1.6 * rootmod
      # inputs = 5.9 + root

      years = seq(trialStart, trialEnd)

      #Initial C content
      Ctotal= 32.2 #mg C g soil-1
      #Ctotal = 26.676
      Ctotal= soilC[soilC$time == trialStart+1,][2]

      #Initial bulk soil 14C: -36.6

      nospace <- 'irr20'

      F14C0 = F14C0_irr

      irr20cols <- c(rgb(12,44,132,alphaVal,,255), rgb(12,44,132,,,255)) #Irr 20
      cols <- c('lightblue1', 'dodgerblue2') #Irr 20

      atm14Cclip = atm14C[atm14C[,1] >= trialStart-1, 1:2]
      atm14Cclip = atm14Cclip[atm14Cclip[,1] <= trialEnd, 1:2]
      atm_in = data.frame(atm14Cclip[,1:2])

      dat<- dat[-c(6,8,10),]
      dat20 <- dat
      soilC <- soilC[-c(1,2,3,4,5,6,7,8,9,10,12,14,15,16,17,18,28,29,30,33,34,36,37,39,40,44,46,47,55,56,57,58,59,60),]
      soilC20 <- soilC

      irrInputs[3] <- mean(inputs$inputs)
      input20 <- inputs
    }

    if(trial == 'Irr. 10'){

      if(trialEnd == 2010){
        trialEnd = 2010
      }
      soilC <- data.frame(winchIrrC$year, ((winchIrrC$perC_irr10 / 100) *soilMass), ((winchIrrC$irr10C_err /100) *soilMass))
      colnames(soilC) <- c('time', 'soilC','err')


      #Manual data input
      time10 = c(1949,1959, 1961, 1967, 1971, 1975, 1980, 1986, 1991, 1997, 2002, 2003)
      dattime = c(1949,1959, 1961, 1967, 1971, 1975, 1980, 1986, 1991, 1997, 2002, 2003)
      dat10 = c(NA, -26.2, 4.8, 138.8, 201.5, 216.3, 193.9, 167.1, 144.3, 123.8, 104.3, NA)

      dattime = c(Winch14C$year, 2003)
      dat10<- c(Winch14C$i_10, NA)
      daterr <- c(Winch14C$i10_err, NA)

      dat = data.frame(dattime, dat10, daterr)
      colnames(dat) <- c('time', 'soilc14', 'err')

      if(roots == FALSE){
        AGprod <- filter(InFrame, Trial == 'Irr10' & Year >= trialStart & Year <= trialEnd)[,-1]
        inputs <- data.frame(Year = AGprod$Year,inputs = (2.0 * rootmod) + (AGprod$Input * AGmod))
      } else {
        inputs = 2.0
      }

      years = seq(trialStart, trialEnd)

      #Initial C content
      Ctotal= soilC[soilC$time == trialStart+1,][2]

      #Initial bulk soil 14C: -26.2

      nospace <- 'irr10'

      F14C0 = F14C0_irr

      irr10cols <- c(rgb(29,145,192,alphaVal,,255), rgb(29,145,192,,,255)) # Irr 10
      cols <- c("skyblue1",'skyblue3') # Irr 10

      atm14Cclip = atm14C[atm14C[,1] >= trialStart-1, 1:2]
      atm14Cclip = atm14Cclip[atm14Cclip[,1] <= trialEnd, 1:2]
      atm_in = data.frame(atm14Cclip[,1:2])
      dat<- dat[-c(6,8,10),]
      dat10 <- dat
      soilC <- soilC[-c(1,2,3,4,5,6,7,8,9,10,12,14,15,16,17,18,28,29,30,33,34,36,37,39,40,44,46,47,55,56,57,58,59,60),]
      soilC10 <- soilC

      irrInputs[2] <- mean(inputs$inputs)
      input10 <- inputs
    }

    if(trial == 'Dry'){

      winchIrrC <- read.csv('./WinchCN_timeCondron855.csv', sep = ',')
      soilC <- data.frame(winchIrrC$year, ((winchIrrC$perC_dry / 100) *soilMass), ((winchIrrC$dryC_err / 100) *soilMass))
      colnames(soilC) <- c('time', 'soilC','err')

      #Data input
      dattime = c(Winch14C$year, trialEnd)
      datdry<- c(Winch14C$i_dry, NA)
      daterr <- c(Winch14C$dry_err, NA)

      dat = data.frame(dattime, datdry, daterr)
      colnames(dat) <- c('time', 'soilc14', 'err')

      if(roots == FALSE){
        AGprod <- filter(InFrame, Trial == 'Dry' & Year >= trialStart & Year <= trialEnd)[,-1]
        inputs <- data.frame(Year = AGprod$Year,inputs = (2.0 * rootmod) + (AGprod$Input * AGmod))
      } else {
        inputs = 2.0
      }


      #From Kelliher et al. 2012
      # root <- 1.9 * rootmod
      # inputs = 3.6 + root

      years = seq(trialStart, trialEnd)

      #Initial C content
      Ctotal= soilC[soilC$time == trialStart+1,][2]


      #Initial bulk soil 14C: -43.9

      nospace <- 'dry'

      F14C0 = F14C0_irr

      drycols <- c(rgb(127,205,187,alphaVal,,255), rgb(127,205,187,,,255)) #Dry
      cols <- c("lightsteelblue1", 'lightsteelblue3') #Dry

      atm14Cclip = atm14C[atm14C[,1] >= trialStart-1, 1:2]
      atm14Cclip = atm14Cclip[atm14Cclip[,1] <= trialEnd, 1:2]
      atm_in = data.frame(atm14Cclip[,1:2])

      dat <- dat[-c(9),]
      datdry <- dat
      soilC <- soilC[-c(1,2,3,4,5,6,7,8,9,10,12,14,15,16,17,18,28,29,30,33,34,36,37,39,40,44,46,47,55,56,57,58,59,60),]
      soilCdry <- soilC

      irrInputs[1] <- mean(inputs$inputs)
      inputDry <- inputs
    }

    #Start PDF for this trial; each trial will output a unique PDF
    #pdf(paste0(filename, trialStart,'_',trialEnd, jk, '.pdf'))

    #View inputs over time
    #par(mfrow = c(1,1))
    #inputFit <- data.frame(seq(trialStart, trialEnd), predict(lm(inputs$inputs~log(inputs$Year)), data.frame(x =seq(trialStart, trialEnd))))
    #plot(inputs$Year, inputs$inputs, type = 'l', main = 'Annual Inputs', lwd = 2, col = 'steelblue 4', xlab = 'Year', ylab = 'Mg ha-1 yr-1', ylim = c(0.8, 6.5))

    #Just to loop this for manual checks
    if(1 > 0){

      if(trialStart == 1958){
        Cfit = Cwindow(soilC, 1959, trialStart, trialEnd)
        Cmod0 = filter(Cfit[,1:2], years == 1959)$predC
      } else {
        Cfit = Cwindow(soilC, trialStart, trialStart - 2, trialEnd)
        Cmod0 = filter(Cfit[,1:2], years == trialStart)$predC
      }
}}}
      iniPools <- function(YOI = trialStart, par){
        return(filter(steadyFrame, Year == YOI)[which.min(abs(filter(steadyFrame, Year == YOI)$`1/k` - par)),]$`14C`)
      }

      if(trialStart == 1958){
        winchFunc =function(pars){
          slow14 = -42
          fast14 = iniPools(trialStart, 1/pars[1])
          mod = SoilR::TwopSeriesModel14(
            t = years,
            ks=c(pars[1], pars[2]),
            C0= Cmod0 * c(1-pars[4], pars[4]),
            F0_Delta14C = c(fast14, slow14),
            In= inputs,
            a21=pars[3]*(pars[1]),
            inputFc = atm_in,
            lag = lags)
          R14m = getF14R(mod)
          C14m = getF14C(mod)
          C14t = getF14(mod)
          Ct = getC(mod)
          return(data.frame(time=years, soilc14 = C14m, soilC = rowSums(Ct)))
        }} else {
          winchFunc =function(pars){
            slow14 = iniPools(trialStart, 1/pars[2])
            fast14 = iniPools(trialStart, 1/pars[1])
            mod = SoilR::TwopSeriesModel14(
              t = years,
              ks=c(pars[1], pars[2]),
              C0= Cmod0 * c(1-pars[4], pars[4]),
              F0_Delta14C = c(fast14, slow14),
              In= inputs,
              a21=pars[3]*(pars[1]),
              inputFc = atm_in,
              lag = lags)
            R14m = getF14R(mod)
            C14m = getF14C(mod)
            C14t = getF14(mod)
            Ct = getC(mod)
            return(data.frame(time=years, soilc14 = C14m, soilC = rowSums(Ct)))
          }
        }

      #Range of 1/k values, which equal turnover time in years at steady state, e.g. 1.5 years to 20 years for the fast pool = range1 <- c(1.5, 20)
      range1 <- c(1.5, 20)
      range2 <- c(20, 600)

      p1max <- p1min <- p2max <- p2min <-  c()

      for(y in seq(trialStart, trialStart+4)){
        #Pool 1 min and max 14C values
        p1 <- filter(data.frame(unlist(plist[as.character(y)]), unlist(plist$ks)), unlist.plist.ks. <= range1[2] & unlist.plist.ks. >= range1[1])
        names(p1) <- c('c14', 'ks')
        p1max <- c(p1max, max(p1$c14))
        p1min <- c(p1min, min(p1$c14))
        #Pool 2 min and max 14C values
        p2 <- filter(data.frame(unlist(plist[as.character(y)]), unlist(plist$ks)), unlist.plist.ks. <= range2[2] & unlist.plist.ks. >= range2[1])
        names(p2) <- c('c14', 'ks')
        p2max <- c(p2max, max(p2$c14))
        p2min <- c(p2min, min(p2$c14))
      }

      #Starting parameters
      inipars=c(p1 = 0.3, p2 = 0.01, a21= 0.1,  slowProp = 0.6)

      #Plot high and low k value pool 14C curves
      slow14_h <- suppressMessages(steadyMod(1/range2[1], trialStart + 4, outLength = 5))
      title(paste('Slow 14C High ::', range2[1], 'Years'))
      slow14_l <- suppressMessages(steadyMod(1/range2[2], trialStart + 4, outLength = 5))
      title(paste('Slow 14C Low ::', range2[2], 'Years'))

      fast14_h <- suppressMessages(steadyMod(1/5, trialStart + 4, outLength = 5))
      title(paste('Fast 14C High ::', range1[1], 'Years'))
      fast14_l <- suppressMessages(steadyMod(1/10, trialStart + 4, outLength = 5))
      title(paste('Fast 14C Low ::', range1[2], 'Years'))

      # Define cost function(s)
      winchCost = function(pars){
        modelOutput=winchFunc(pars)
        cost1 = modCost(model=modelOutput, obs = soilC, err = 'err')
        cost2 = modCost(model=modelOutput, obs = dat, cost = cost1, err = 'err')
        return(cost2)
      }

      # Initial parameter values
      lowers = c(1/range1[2], 1/range2[2], .0001,   0.01) #Fast pool: 2 year and 10 year turnover for endpoints, slow pool: 40 and 300 years
      uppers = c(1/range1[1], 1/range2[1],     1,      1)
      inipars = c(k1 = 0.25, k2 = 0.02, a21 = 0.015, slowProp = 0.7)


      # Fit model!
      cat('Running first fit for', nospace, '\n')
      winchFit = modFit(f=winchCost, p=inipars, method='Pseudo', upper=uppers, lower=lowers,
                        control = list(numiter = pseudo_n))

      #Initial pool 14C values using fitted parameters in one pool at steady state
      fast14 <- steadyMod(winchFit$par[1], trialStart)$preBomb14C[6]
      title('t0 Fast Pool 14C')

      if(trialStart == 1958){
        slow14 = -42
      } else {
        slow14 = steadyMod(winchFit$par[2], trialStart)$preBomb14C[6]
        title('t0 Slow Pool 14C')
      }

      # Re-run model with fitted parameters
      fitmod = SoilR::TwopSeriesModel14(
        t=years,
        ks = c(winchFit$par[1], winchFit$par[2]),
        a21 = winchFit$par[3]*winchFit$par[1],
        C0 = Cmod0 * c(1-winchFit$par[4], winchFit$par[4]),
        inputFc = atm_in,
        F0_Delta14C = c(fast14, slow14),
        lag = lags,
        In=inputs
      )

      # Modelled system 14C
      C14m = getF14C(fitmod) # Atmospheric 14C
      C14t = getF14(fitmod) # Pool 14C
      R14m = getF14R(fitmod) # Respired 14C
      Ct = getC(fitmod) # Pool C content
      wp <- winchFit$par

      par(mfrow = c(1,1))

      #Calculate range of 14C values for each pool, for plotting later
      p1lag <- data.frame(slow14_h$time, (p1max + p1min) / 2, (p1max - p1min) /2 )
      colnames(p1lag) <- c('time', 'p1lag', 'p1err')
      p2lag <- data.frame(slow14_h$time, (p2max + p2min) / 2, (p2max - p2min) /2 )
      colnames(p2lag) <- c('time', 'p2lag', 'p2err')

      ## Plot 14C and C to visualize model fit
      #Pool 14C plot
      plot(dat$time, dat$soilc14, lwd = 3, col = 'steelblue', xlim = c(years[1] - 2, trialEnd + 10), ylim = c(-100, 500),
           main = paste(trial), xlab = 'Year', ylab = expression(paste(Delta^{14}, "C")), font.lab = 2)
      arrows(dat$time, dat$soilc14- dat$err,dat$time, dat$soilc14+ dat$err,length=0.05, angle=90, code=3)
      lines(atm14C$year, atm14C$atm14C, col = 'orange', lwd = 3)
      lines(p2lag$time, p2lag$p2lag, col = 'red', lwd = 4)
      lines(p1lag$time, p1lag$p1lag, col = 'red', lwd = 4)
      arrows(p2lag$time, p2lag$p2lag-p2lag$p2err, p2lag$time, p2lag$p2lag + p2lag$p2err, length=0.05, angle=90, code=3)
      arrows(p1lag$time, p1lag$p1lag-p1lag$p1err, p1lag$time, p1lag$p1lag + p1lag$p1err, length=0.05, angle=90, code=3)
      lines(years, C14t[,1], col = 'brown3', lwd = 3)
      lines(years, C14t[,2], col = 'cyan4', lwd = 3)
      lines(years, C14m, col = 'darkorchid', lwd = 3)
      lines(years, R14m, col = 'chartreuse3', lwd = 3, lty = 2)
      points(dat, col = 'steelblue', lwd = 4)
      legend('topright', legend = c('Atmosphere', 'Bulk Soil', 'Fast Pool', 'Slow Pool', 'Respired', 'Observed'),
             col = c('orange', 'darkorchid', 'brown3', 'cyan4', 'chartreuse3', 'steelblue'), lty = c(1, 1,1,1,2,NA),
             lwd = c(3, 2, 3, 2,3,3), pch = c(NA,NA,NA,NA,NA,1), cex = 1.0)

      #Pool 14C plot
      par(mfrow = c(1,1))
      plot(dat$time, dat$soilc14, lwd = 3, col = 'steelblue', xlim = c(years[1], 2010), ylim = c(-100, 500),
           main = paste(trial), xlab = 'Year', ylab = expression(paste(Delta^{14}, "C")), font.lab = 2)
      arrows(dat$time, dat$soilc14- dat$err,dat$time, dat$soilc14+ dat$err,length=0.05, angle=90, code=3)
      lines(atm14C$year, atm14C$atm14C, col = 'orange', lwd = 3)
      lines(p2lag$time, p2lag$p2lag, col = 'red', lwd = 4)
      arrows(p2lag$time, p2lag$p2lag-p2lag$p2err, p2lag$time, p2lag$p2lag + p2lag$p2err, length=0.05, angle=90, code=3)
      lines(p1lag$time, p1lag$p1lag, col = 'red', lwd = 4)
      arrows(p1lag$time, p1lag$p1lag-p1lag$p1err, p1lag$time, p1lag$p1lag + p1lag$p1err, length=0.05, angle=90, code=3)
      lines(years, C14t[,1], col = 'brown3', lwd = 3)
      lines(years, C14t[,2], col = 'cyan4', lwd = 3)
      lines(years, C14m, col = 'darkorchid', lwd = 3)
      lines(years, R14m, col = 'chartreuse3', lwd = 3, lty = 2)
      points(dat, col = 'steelblue', lwd = 4)
      legend('topright', legend = c('Atmosphere', 'Bulk Soil', 'Fast Pool', 'Slow Pool', 'Respired', 'Observed'),
             col = c('orange', 'darkorchid', 'brown3', 'cyan4', 'chartreuse3', 'steelblue'), lty = c(1, 1,1,1,2,NA),
             lwd = c(3, 2, 3, 2,3,3), pch = c(NA,NA,NA,NA,NA,1), cex = 1.0)

      # Pool C content
      Ctot <- Ct[,1] + Ct[,2]
      plot(soilC$time, soilC$soilC, ylim = c(0, 45), col = 'black',bg = 'steelblue2', pch = 24,
           main = paste(trial, ":: Modelled Pool C Stocks"), xlab = 'Years', ylab = 'Mg C ha-1',
           xlim = c(years[1], trialEnd))
      arrows(soilC$time, soilC$soilC- soilC$err,soilC$time, soilC$soilC+ soilC$err,length=0.05, angle=90, code=3)
      points(soilC$time, soilC$soilC, col = 'black',bg = 'steelblue2',pch = 24)
      lines(years, Ct[,1], col = 'brown3', lwd = 3)
      lines(years, Ct[,2], col = 'cyan4', lwd = 3)
      lines(years, Ctot, col = 'darkorchid', lwd = 4)
      lines(years, inputs[,2], col = 'red', lty = 2, lwd = 2)
      legend('topleft', legend = c('Bulk Soil', 'Fast Pool', 'Slow Pool', 'Measured','Inputs'),
             col=c('darkorchid', 'brown3', 'cyan4', 'black','red'), pt.bg = c(NA, NA, NA, 'steelblue2',NA),lty = c(1,1,1,NA,2),
             pch = c(NA, NA, NA, 24,NA), lwd = c(3,3,3,NA,2), cex = 1)

      # Info on model
      par(mfrow = c(3,1))
      modelFacts <- data.frame(soilMass, round(mean(inputs$inputs), 2), paste(range1[1], '-', range1[2], 'years'),
                               paste(range2[1], '-', range2[2], 'years'),trialStart, trialEnd)
      colnames(modelFacts) <- c("Soil Mass",'Mean Inputs', "P1 Transit (y)", "P2 Transit", 'Trial Start','Trial End')
      textplot(modelFacts, halign = 'right')
      title(paste(trial,':: Model Inputs'), cex.main = 2)
      names(uppers) <- c('k1', 'k2', 'a21', 'slowProp')
      names(lowers) <- c('k1', 'k2', 'a21', 'slowProp')
      textplot(round(uppers,3))
      title('Model Parameter Maximums', cex.main = 2)
      textplot(round(lowers,3))
      title('Model Parameter Minimums', cex.main = 2)

      par(mfrow = c(4,1))
      textplot(data.frame(SSR = round(winchFit$ssr, 2), Res.Length = length(winchFit$residuals)))
      title('Sum Squared Residuals')
      textplot(data.frame(MSR = round(winchFit$ms, 4), AIC = round((2*length(winchFit$par))-(2*log(winchFit$ms)),3)))
      title('Mean Squared Residuals and AIC')
      textplot(round(winchFit$var_ms, 4))
      title('Mean Squared Residuals, per variable')
      textplot(round(winchFit$var_ms_unweighted, 4))
      title('Mean Squared Residuals, Unweighted')

      par(mfrow = c(1,1))

      # Construct matrices and calculate transit time and system age functions
      ks <- c(winchFit$par[1], winchFit$par[2])
      as <- c(winchFit$par[3] * (winchFit$par[1]), 0)
      u2 = matrix(c(mean(inputs$inputs), 0), ncol = 1)
      A2=diag(-ks)
      A2[2,1] = as[1]

      # Add plenty of extra time to capture distribution tails
      SA2 = systemAge(A = A2, u = u2, a = seq(0,1000))
      TT2 = transitTime(A = A2, u = u2, a = seq(0,1000))


      # Save and plot model info
      par(mfrow = c(2,1))
      winchPar4Saving <- winchFit$par
      modOut <- data.frame(round(winchFit$par[1], 3), round(winchFit$par[2], 4),
                           round(winchFit$par[3], 3), round(winchFit$par[4], 2),
                           round(winchFit$var_ms, 2)[2],
                           round(winchFit$var_ms, 2)[1])
      colnames(modOut) <- c('k1', 'k2', 'a21', 'slow','C MSE', 'C14 MSE')
      textplot(modOut)
      title(paste(trial, ":: WinchFit Parameters"))

      turnovers <- data.frame(round(1/winchFit$par[1],1), round(fast14,1), round(1/winchFit$par[2],1),
                              round(slow14,1))
      colnames(turnovers) <- c('P1 Turnover', 'P1 14C', 'P2 Turnover', 'P2 14C')

      textplot(turnovers)
      title('Pool Turnovers from Modeled \n Pool 14C Values')

      #Steady state C stocks
      steadyRun <- TwopSeriesModel(
        t = seq(trialStart, trialStart + 5000),
        ks = c(winchFit$par[1], winchFit$par[2]),
        a21 = winchFit$par[1] * winchFit$par[3],
        C0 = Cmod0 * c(1-winchFit$par[4], winchFit$par[4]),
        In = mean(inputs$inputs)
      )

      SCt = getC(steadyRun) # Pool C content
      Csum <- data.frame(years = seq(trialStart, trialStart + 5000), totC = SCt[,1] + SCt[,2])

      SSyear = Csum[which.min(abs(Csum$totC - TT2$meanTransitTime * mean(inputs$inputs))),1]
      SSyear95 = Csum[which.min(abs(Csum$totC - (TT2$meanTransitTime * mean(inputs$inputs) * 0.95))),1]
      SSyear90 = Csum[which.min(abs(Csum$totC - (TT2$meanTransitTime * mean(inputs$inputs) * 0.9))),1]

      par(mfrow = c(1,1))
      SSStock <- mean(TT2$meanTransitTime * inputs[length(inputs$inputs),]$inputs)

      Dynamics <- data.frame(round(SA2$meanSystemAge,2), round(SA2$quantilesSystemAge[2], 2),
                             round(SA2$meanPoolAge[1], 2), round(SA2$meanPoolAge[2], 2),
                             round(TT2$meanTransitTime, 2), round(TT2$quantiles[2], 2), round(SSStock, 2),
                             round(SSyear95))
      colnames(Dynamics) <- c('Mean SA', 'Median SA', 'P1 Age', 'P2 Age', 'Mean TT', 'Median TT', 'SS Stock',
                              '95% Stock Yr.')
      textplot(Dynamics, halign = 'right')
      title(paste(trial, 'Simple Fit Dynamics', cex.main = 2))
      print(paste(trial, "Mean TT:",round(TT2$meanTransitTime,2), "Mean SA:", round(SA2$meanSystemAge,2)))
      print(wp)

      plot(seq(trialStart, trialStart+5000), SCt[,1] + SCt[,2], type = 'l', xlim = c(1950, SSyear95 + 100),
           ylab = 'Stocks (Tonnes C ha)', xlab = 'Year', lwd = 2, col = 'purple', ylim = c(10, 55))
      points(soilC$time, soilC$soilC, col = 'steelblue', pch = 16)
      lines(seq(trialStart, trialStart+5000), SCt[,1], col = 'brown3')
      lines(seq(trialStart, trialStart+5000), SCt[,2], col = 'cyan4')
      abline(,,,SSyear90, lty = 2, lwd = 2)
      abline(,,,SSyear95, lty = 3, lwd = 3)
      legend('bottom', legend = c('Total C', 'Data', '90%', '95%'), lty = c(1,NA,2,3), lwd =2,
             col = c('purple', 'steelblue',1,1), pch = c(NA, 16, NA, NA))

      plot(seq(0,1000), SA2$poolAgeDensity[,1], type = 'l', xlim = c(0,250), col = 'cyan4', lwd = 1.5, main = 'Pool Age Distribution', xlab = 'Years')
      lines(seq(0,1000), SA2$poolAgeDensity[,2], col = 'brown3', lwd = 1.5)

      plot(seq(0,1000), TT2$transitTimeDensity, xlim = c(0,100), type = 'l', col = 'cyan4', lwd =1.5, xlab = 'Years', main = 'Transit Time Distribution')
    }
    dev.off()
  }}
cat('About to Start Bayesian Estimation for', trial, '\n')
print("Estimating parameters...")

bayes_fit <- modMCMC(f=winchCost, p=winchFit$par, niter = niter, updatecov = updatecov, var0 = winchFit$var_ms_unweighted,
                     upper=uppers, lower= lowers, burninlength = burnin, verbose = 500)

# Info output for MCMC and some plots
bayesFacts <- data.frame(niter, burnin, bayes_fit$naccepted)
colnames(bayesFacts) <- c('# Iterations', "# Burn-In", "# Accepted")
textplot(bayesFacts)
title("Parameter Optimization Stats", cex.main = 2)

fit_summary <- summary(bayes_fit) #for table
print(fit_summary)
hist(bayes_fit)
par(mfrow = c(1,1))

#Check that parameters have stabilized after burnin
plot(bayes_fit)
pairs(bayes_fit)

parRange <- data.frame(lowers, uppers)
# Output object with stats on model variability
pred_uncert <- sensRange(winchFunc, parInput = bayes_fit$pars, num = outLength, parRange = parRange,
                         parms = bayes_fit$bestpar)

# Plotting variability: radiocarbon over time
par(mfrow = c(1,1))
plot(atm14C$year, atm14C$atm14C, type = 'l', col = 'orange', lwd = 3, axes = F, xlim = c(1959, 2012), ylim = c(-50, 250), ann = F)
par(new = T)
plot(summary(pred_uncert), which = 1, main = paste(trial, ":: Bayes 14C"), xlab = 'Year', ylab = expression(paste(Delta^"14","C")),
     col = c(cols[1],cols[2], cols[3]), xlim = c(1959, 2012), ylim = c(-50, 250),
     legpos = 'topright', quant = TRUE)
points(dat$time, dat$soilc14, pch = 21, bg = cols[3], cex = 1.2, col = 'steelblue', lwd = 4)
arrows(dat$time, dat$soilc14- dat$err,dat$time, dat$soilc14+ dat$err,length=0.05, angle=90, code=3)

# Plotting variability: C stocks over time
plot(summary(pred_uncert), which = 2, main = paste(trial, ':: Bayes C'), xlab = 'Year', , legpos = 'topright',
     ylab = expression(paste('C stock'," (g C kg"^"-1", ")"), legpos = 'topleft'),
     col = c(cols[1], cols[2], cols[3]), xlim = c(1959, 2012), ylim = c(0, 50), quant = TRUE)
arrows(soilC$time, soilC$soilC- soilC$err,soilC$time, soilC$soilC+ soilC$err,length=0.05, angle=90, code=3)
points(soilC$time, soilC$soilC, pch = 21, bg = cols[3], col = 'steelblue', lwd = 1.5)

# pred_uncert_save <- pred_uncert
# pred_uncert <- pred_uncert_save
# pred_uncert <- filter(pred_uncert, k2 > quantile(pred_uncert$k2, 0.05) & k2 < quantile(pred_uncert$k2, 0.95))

# Loop through all accepted parameter sets and calculate each TT and SA for statistics
testlength <- seq(1, length(pred_uncert$k1))
if(testlength[1] == 0){
  testlength <- seq(1, tail(testlength, n = 1))
}
SAlist <- as.numeric(testlength)
SAmedlist <- as.numeric(testlength)
TTlist <- as.numeric(testlength)
TTmedlist <- as.numeric(testlength)
pool1agelist <- as.numeric(testlength)
pool2agelist <- as.numeric(testlength)
TTdist <- as.data.frame(matrix(seq(0,500, by = 0.5), ncol = testlength, nrow = 1001))
SAdist <- as.data.frame(matrix(seq(0,1000, by = 0.5), ncol = testlength, nrow = 2001))
SAquantlist <- as.data.frame(matrix(seq(1,5), ncol = 5, nrow = testlength))
names(SAquantlist) <- c('q05', 'q25', 'q50', 'q75', 'q95')
TTquantlist <- as.data.frame(matrix(seq(1,5), ncol = 5, nrow = testlength))
names(TTquantlist) <- c('q05', 'q25', 'q50', 'q75', 'q95')
colnames(TTdist) <- 'years'
colnames(SAdist) <- 'years'

Ct1list <- as.data.frame(matrix(years, ncol = testlength, nrow = length(years)))
Ct2list <- as.data.frame(matrix(years, ncol = testlength, nrow = length(years)))
R14list <- as.data.frame(matrix(years, ncol = testlength, nrow = length(years)))
C14mlist <- as.data.frame(matrix(years, ncol = testlength, nrow = length(years)))
C141list <- as.data.frame(matrix(years, ncol = testlength, nrow = length(years)))
C142list <- as.data.frame(matrix(years, ncol = testlength, nrow = length(years)))

cat('Running all parameter combos through SA and TT for', trial, '\n')

unfertcols <- c(rgb(254,178,76,alphaVal,,255), rgb(254,178,76,,,255))
rescols <- c(rgb(252,78,42,alphaVal,,255), rgb(252,78,42,,,255))
highcols <- c(rgb(177,0,38,alphaVal,,255), rgb(177,0,38,,,255))

drycols <- c(rgb(127,205,187,alphaVal,,255), rgb(127,205,187,,,255))
irr10cols <- c(rgb(29,145,192,alphaVal,,255), rgb(29,145,192,,,255))
irr20cols <- c(rgb(12,44,132,alphaVal,,255), rgb(12,44,132,,,255))

for (a in testlength){
  if(a > 0){
    #Calculate for each parameter combination
    kstest <- c(pred_uncert$k1[a], pred_uncert$k2[a])
    a21test <- kstest[1] * pred_uncert$a21[a]
    slowVar <- pred_uncert$slowProp[a]

    #Fit model to parameter set
    p2var = iniPools(trialStart, 1/kstest[2])
    p1var = iniPools(trialStart, 1/kstest[1])
    fitmodtest = SoilR::TwopSeriesModel14(
      t=years,
      ks = kstest,
      In= inputs,
      a21 = a21test,
      C0= Cmod0 * c(1-slowVar, slowVar),
      inputFc = atm_in,
      F0_Delta14C = c(p1var, p2var),
      lag = lags
    )

    #Calculate values for SA, TT, Pool Age
    A2=diag(-kstest)
    u2 = matrix(c(mean(inputs$inputs),0), ncol=1)
    A2[2,1] = a21test
    SA3bayes = systemAge(A = A2, u = u2, a = seq(0, 1000, by = 0.5), q = c(0.05, 0.25, 0.5, 0.75, 0.95))
    TT3bayes = transitTime(A = A2, u = u2, a = seq(0, 500, by = 0.5),q = c(0.05, 0.25, 0.5, 0.75, 0.95))

    C14m = getF14C(fitmodtest) # Bulk 14C
    C14t = getF14(fitmodtest) # Pool 14C
    R14m = getF14R(fitmodtest) # Respired 14C
    Ct = getC(fitmodtest) # Pool C content

    #Name column in TTdist
    colHeadTT <- paste0('TT',a)
    colHeadSA <- paste0('SA',a)
    colHead <- a

    #Append to list
    SAlist[a] <- as.numeric(SA3bayes$meanSystemAge[1])
    SAmedlist[a] <- as.numeric(SA3bayes$quantilesSystemAge[2])
    TTlist[a] <- as.numeric(TT3bayes$meanTransitTime[1])
    TTmedlist[a] <- as.numeric(TT3bayes$quantiles[2])
    pool1agelist[a] <- as.numeric(SA3bayes$meanPoolAge[1])
    pool2agelist[a] <- as.numeric(SA3bayes$meanPoolAge[2])
    TTdist[[colHeadTT]] <- TT3bayes$transitTimeDensity
    SAdist[[colHeadSA]] <- SA3bayes$systemAgeDensity
    SAquantlist <- rbind(SAquantlist, c(q05 = SA3bayes$quantilesSystemAge[1], q25 = SA3bayes$quantilesSystemAge[2],
                                        q50 = SA3bayes$quantilesSystemAge[3], q75 = SA3bayes$quantilesSystemAge[4],
                                        q95 = SA3bayes$quantilesSystemAge[5]))
    TTquantlist <- rbind(TTquantlist, c(q05 = TT3bayes$quantiles[1], q25 = TT3bayes$quantiles[2],
                                        q50 = TT3bayes$quantiles[3], q75 = TT3bayes$quantiles[4],
                                        q95 = TT3bayes$quantiles[5]))

    Ct1list[[colHead]] <- Ct[,1]
    Ct2list[[colHead]] <- Ct[,2]
    R14list[[colHead]] <- R14m
    C14mlist[[colHead]] <- C14m
    C141list[[colHead]] <- C14t[,1]
    C142list[[colHead]] <- C14t[,2]


    #Progess tracker, because this can take a while
    if(a %% 10 == 0){
      per_complete <- (a/length(testlength)) * 100
      print(paste(round(per_complete,1), "% complete", sep=""))
    }
  }
}

poolOutputs <- list(Ct1 = Ct1list, Ct2 = Ct2list, R14 = R14list, C14b = C14mlist, C141 = C141list, C142 =C142list)



#Make lists into data frames
par(mfrow = c(1,1))
SAdf <- data.frame(Parameter = 'MeanSA', Value = SAlist)
SAmeddf <- data.frame(Parameter = 'MedSA', Value = SAmedlist)
TTdf <- data.frame(Parameter = 'MeanTT', Value = TTlist)
TTmeddf <- data.frame(Parameter = 'MedTT', Value = TTmedlist)
pool1agedf <- data.frame(Parameter = 'Pool1Age', Value = pool1agelist)
pool2agedf <- data.frame(Parameter = 'Pool2Age', Value = pool2agelist)

#Box plots of SA, TT, Pool Age model certainty
sys.plot.data <- rbind(SAdf, SAmeddf, TTdf, TTmeddf, pool1agedf, pool2agedf)
print(ggplot2::ggplot(sys.plot.data, ggplot2::aes(x=Parameter, y = Value)) +
        ggplot2::geom_boxplot(color = 'steelblue4', fill = 'darkgray') + theme_bw() +
        ggplot2::ggtitle(paste(trial, 'C Dynamics, upper limit = SA 75th percentile')) +
        ylim(0, boxplot.stats(pool2agedf$Value)$stats[5]))
SAsd <- sd(SAdf$Value)
TTsd <- sd(TTdf$Value)
p1sd <- sd(pool1agedf$Value)
p2sd <- sd(pool2agedf$Value)
SAmean <- mean(SAdf$Value)
SAmed <- median(SAdf$Value)
TTmean <- mean(TTdf$Value)
TTmed <- median(TTdf$Value)
p1mean <- mean(pool1agedf$Value)
p1med <- median(pool1agedf$Value)
p2mean <- mean(pool2agedf$Value)
p2med <- median(pool2agedf$Value)
sdDF <- c(SAsd, TTsd, p1sd, p2sd)
names(sdDF) <- c('System Age SD', 'Transit Time SD', 'P1 Age SD', 'P2 Age SD')
meanDF <- c(SAmean, TTmean, p1mean, p2mean)
names(meanDF) <- c('System Age', 'Transit Time', 'P1 Age', 'P2 Age')
medDF <- c(SAmed, TTmed, p1med, p2med)
names(medDF) <- c('System Age', 'Transit Time', 'P1 Age', 'P2 Age')


TTquantlist$dyn <- 'TT'
SAquantlist$dyn <- 'SA'
Cquants <- rbind(TTquantlist[-1,], SAquantlist[-1,])

par(mfrow = c(3,1))
textplot(round(meanDF, 2), cex=1.0, hmar = 3)
title('C Dynamic Means', cex.main = 1.8)

textplot(round(medDF,2), cex = 1, hmar = 3)
title('C Dynamic Medians', cex.main = 2)

textplot(round(sdDF,2), cex = 1, hmar = 3)
title('C Dynamic SDs', cex.main = 2)


#bayes_fit <- resfert_bayes_fit

#Re-run fitted model with best pars from Bayesian fit
ks <- c(bayes_fit$bestpar[1], bayes_fit$bestpar[2])
a21 <- ks[1]*bayes_fit$bestpar[3]
names(a21) <- 'a21'
bestSlow <- bayes_fit$bestpar[4]
names(bestSlow) <- 'SlowProp'


p2var = iniPools(trialStart, 1/ks[2])
p1var = iniPools(trialStart, 1/ks[1])
bayes_fitmod = SoilR::TwopSeriesModel14(
  t=years,
  ks = ks,
  a21 = a21,
  C0= Cmod0  * c(1-bestSlow, bestSlow),
  inputFc = atm_in,
  F0_Delta14C = c(p1var, p2var),
  lag = lags,
  In=inputs
)

R14m = getF14R(bayes_fitmod)
C14m = getF14C(bayes_fitmod)
C14t = getF14(bayes_fitmod)
Ct = getC(bayes_fitmod)

bayesCs <- data.frame(years,R14m, C14m, C14t, Ct)
colnames(bayesCs) <- c('years', 'Resp14C', 'Bulk14C', 'P1_14C', 'P2_14C', 'P1_C', 'P2_C')

# Plot best fit 14C
par(mfrow = c(1,1))
plot(dat$time, dat$soilc14, lwd = 3, col = 'steelblue', xlim = c(years[1], 2010), ylim = c(-100, 500),
     main = paste("Best Fit 14C ::",trial), xlab = 'Year', ylab = expression(paste(Delta^{14}, "C")), font.lab = 2)
lines(p2lag$time, p2lag$p2lag, col = 'red', lwd = 4)
lines(p1lag$time, p1lag$p1lag, col = 'red', lwd = 4)
arrows(p2lag$time, p2lag$p2lag-p2lag$p2err, p2lag$time, p2lag$p2lag + p2lag$p2err, length=0.05, angle=90, code=3)
arrows(p1lag$time, p1lag$p1lag-p1lag$p1err, p1lag$time, p1lag$p1lag + p1lag$p1err, length=0.05, angle=90, code=3)
lines(atm14C$year, atm14C$atm14C, col = 'orange', lwd = 3)
lines(years, C14t[,1], col = 'cyan4', lwd = 3)
lines(years, C14t[,2], col = 'brown3', lwd = 3)
lines(years, C14m, col = ' darkorchid', lwd = 3)
lines(years, R14m, col = 'chartreuse2', lwd = 2, lty = 2)
points(dat, col = 'steelblue', lwd = 4)
legend('topright', legend = c('Atmosphere', 'Modelled', 'Fast Pool', 'Slow Pool', 'Respired', 'Observed'),
       col = c('orange', 'darkorchid', 'cyan4', 'brown3', 'chartreuse2', 'steelblue'), lty = c(1, 1,1,1,2,NA),
       lwd = c(3, 2, 3, 2,2,2), pch = c(NA,NA,NA,NA,NA,1), cex = 1.0)

# Plot best fit pool C content
Ctot <- Ct[,1] + Ct[,2]
plot(soilC$time, soilC$soilC, ylim = c(0, 45), col = 'black',bg = 'steelblue2', pch = 24,
     main = paste("Best Fit Pool C Stocks ::",trial), xlab = 'Years', ylab = 'Tonnes C',
     xlim = c(years[1], tail(dat$time,1)))
arrows(soilC$time, soilC$soilC- soilC$err,soilC$time, soilC$soilC+ soilC$err,length=0.05, angle=90, code=3)
lines(years, Ct[,1], col = 'red', lwd = 3)
lines(years, Ct[,2], col = 'blue', lwd = 3)
lines(years, Ctot, col = 'darkorchid', lwd = 4)
legend('bottomright', legend = c('Total C', 'Fast Pool', 'Slow Pool', 'Measured'),
       col=c('darkorchid', 'red', 'blue', 'black'), pt.bg = c(NA, NA, NA, 'steelblue2'),lty = c(1,1,1,NA),
       pch = c(NA, NA, NA, 24), lwd = c(3,3,3,NA), cex = 0.7)

# Calculate best fit SA and TT
par(mfrow = c(2,1))
A2=diag(-ks)
u2 = matrix(c(mean(inputs$inputs),0), ncol=1)
A2[2,1] = a21
SA3best = systemAge(A=A2, u = u2, a = seq(0, 1000, by = 0.5))
TT3best = transitTime(A=A2, u=u2, a = seq(0, 1000, by = 0.5), q = c(0.05, 0.25, 0.5, 0.75, 0.95))
BestSSStock <- TT3best$meanTransitTime * mean(inputs$inputs)
names(BestSSStock) <- 'Best SS Stock'

textplot(c(round(ks, 4), round(a21,3),round(bestSlow,3),
           round(BestSSStock,1)), halign = NULL, valign = NULL, cex=0.8)
title("Best Fit Parameters from modMCMC")

bestDynamics <- c(round(SA3best$meanSystemAge,2), round(TT3best$meanTransitTime,2), round(SA3best$meanPoolAge[1], 2), round(SA3best$meanPoolAge[2],2))
names(bestDynamics) <- c('SA', 'TT', 'p1 Age', 'p2 Age')
textplot(bestDynamics)
title("C Dynamics From Best Fit Parameters")

# Construct a data file with all important information so that it can be loaded and referred to later
heads <- c('k1', 'k2', 'a21', 'SlowProp', 'p1 14C', 'p2 14C')
colnames(dfs) <- c('num')
for (a in heads){
  if (a == 'k1'){
    k1 <- data.frame(Parameter = 'k1', Value = pred_uncert$k1)
  }
  if (a == 'k2'){
    k2 <- data.frame(Parameter = 'k2', Value = pred_uncert$k2)
  }
  if (a == 'a21'){
    a21 <- data.frame(Parameter = 'a21', Value = pred_uncert$a21)
  }
  if (a == 'SlowProp'){
    slowProp <- data.frame(Parameter = 'SlowProp', Value = pred_uncert$slowProp)
  }
  # if (a == 'p1 14C'){
  #   p1_14C <- data.frame(Parameter = 'p1_14C', Value = pred_uncert$p1)
  # }
  # if (a == 'p2 14C'){
  #   p2_14C <- data.frame(Parameter = 'p2_14C', Value = pred_uncert$p2)
  # }
}

#Calculate 1/k (turnover time) for initial pool radiocarbon
#turnp1 <- filter(steadyFrame, Year == trialStart)$`1/k`[which.min(abs(filter(steadyFrame, Year == trialStart)$`14C` - bayes_fit$bestpar[4]))]
#turnp2 <- filter(steadyFrame, Year == trialStart)$`1/k`[which.min(abs(filter(steadyFrame, Year == trialStart)$`14C` - bayes_fit$bestpar[5]))]

#Boxplot of pool 14C at t0
fitP1t0 <- data.frame(Pool = 'Pool 1', C14 = unlist(c(head(C141list, 1))))
fitP2t0 <- data.frame(Pool = 'Pool 2', C14 = unlist(c(head(C142list, 1))))
fitT0 <- rbind(fitP1t0, fitP2t0)

print(
  ggplot(fitT0, aes(x = Pool, y = C14, fill = Pool)) + geom_boxplot() +
    ggtitle("Pool Radiocarbon at Trial Start") + theme_bw() +
    scale_fill_manual(values = c(rgb(197,27,138,220,,255),rgb(254,178,76,220,,255)))
)

# Plots of parameter values
par(mfrow = c(1,1))
plot.data <- rbind(k1, k2, a21, slowProp)
print(ggplot2::ggplot(plot.data, ggplot2::aes(x=Parameter, y = Value)) + ggplot2::geom_boxplot(color = 'steelblue4', fill = 'darkgray') +
        ggplot2::ggtitle(paste(trial, ':: Bayesian Fit Parameter Distributions')) + theme_bw())

# pool14 <- rbind(p1_14C, p2_14C)
# print(ggplot(pool14, aes(y = Value, fill = Parameter))+geom_boxplot() + facet_wrap(pool14$Parameter, scales = 'free_y')) +
#   scale_fill_manual(values = c(rgb(197,27,138,220,,255),rgb(254,178,76,220,,255)))

par(mfrow = c(3,1))
names(bayes_fit$bestpar) <- c('k1', 'k2', 'a21', 'slowProp')
textplot(round(bayes_fit$bestpar[1:6],4), cex = 1)
title('Best fit from parameter optimization', cex.main = 2)
textplot(round(summary(bayes_fit)[1:6], 4), cex=1, hmar = 3)
title('Parameter distribution from optimization', cex.main = 1.8)
# textplot(c('Fit P1 TO' = turnp1,'Fit P2 TO' = turnp2,
#            'Out P1 TO' = c(min(round(steadyTT(summary(bayes_fit$pars[,1])[4], trialStart,5)$`1/k`,2)), max(round(steadyTT(summary(bayes_fit$pars[,1])[4], trialStart,5)$`1/k`,2))),
#            'Out P2 TO' = c(min(round(steadyTT(summary(bayes_fit$pars[,2])[4], trialStart,5)$`1/k`,2)), max(round(steadyTT(summary(bayes_fit$pars[,2])[4], trialStart,5)$`1/k`,2)))), cex=0.9, halign = 'center', valign = 'center')
# title('How well do model input and output turnover times compare? \n
#       1 = Min, 2 = Max')

par(mfrow = c(1,1))
# steadyForThisYear <- filter(steadyFrame, Year == trialStart)
# plot(steadyForThisYear$`1/k`, steadyForThisYear$`14C`, xlab = 'Turnover Time', ylab = 'Pool 14C', type = 'l', col = 'forestgreen', lwd = 3,
#      main = 'Turnover (TO) of Pools With Modeled 14C \n for First Year of Trial')
# abline(,,median(bayes_fit$pars[,4]), col = 'darkorchid', lwd = 2, lty =2)
# abline(,,median(bayes_fit$pars[,5]), col = 'goldenrod', lwd = 2, lty =2)
# abline(,,,(1/mean(bayes_fit$pars[,1])), col = 'darkorchid', lwd = 3, lty = 3)
# abline(,,,(1/mean(bayes_fit$pars[,2])), col = 'goldenrod', lwd = 3, lty = 3)
# legend('topright', legend = c(paste('Year', trialStart), 'P1 14C Fit', 'P1 TO Output', 'P2 14C Fit', 'P2 TO Output'),
#        col = c('forestgreen', 'darkorchid', 'darkorchid', 'goldenrod', 'goldenrod'), lty = c(1, 2, 3, 2, 3), lwd = 3)

#Environment variables to save
envParms <- c(rootmod = rootmod, AGmod = AGmod, soilMass = soilMass)

# Save data for each trial
if(trial == "Unfert"){
  unPars <- plot.data
  unfertSA3 <- SA3best
  unfertTT3 <- TT3best
  unfert_bayes_fit <- bayes_fit
  unfert_pred_uncert <- pred_uncert
  unfertWinchPars <- winchFit$par
  unfertBayesDynamics <- sys.plot.data
  unfertBayesCs <- bayesCs
  unfertTTdist <- TTdist
  unfertSAdist <- SAdist
  unfertpool14 <- fitT0
  unfert_poolouts <- poolOutputs
  unfertquants <- Cquants
}

if(trial == "Res. Fert"){
  resPars <- plot.data
  resfertSA3 <- SA3best
  resfertTT3 <- TT3best
  resfert_bayes_fit <- bayes_fit
  resfert_pred_uncert <- pred_uncert
  resfertWinchPars <- winchFit$par
  resfertBayesDynamics <- sys.plot.data
  resfertBayesCs <- bayesCs
  resfertTTdist <- TTdist
  resfertSAdist <- SAdist
  resfertpool14 <- fitT0
  resfert_poolouts <- poolOutputs
  resfertquants <- Cquants
}

if(trial == "High Fert"){
  highPars <- plot.data
  highfertSA3 <- SA3best
  highfertTT3 <- TT3best
  highfert_bayes_fit <- bayes_fit
  highfert_pred_uncert <- pred_uncert
  highfertWinchPars <- winchFit$par
  highfertBayesDynamics <- sys.plot.data
  highfertBayesCs <- bayesCs
  highfertTTdist <- TTdist
  highfertSAdist <- SAdist
  highfertpool14 <- fitT0
  highfert_poolouts <- poolOutputs
  highfertquants <- Cquants
}

if(trial == "Dry"){
  dryPars <- plot.data
  drySA3 <- SA3best
  dryTT3 <- TT3best
  dry_bayes_fit <- bayes_fit
  dry_pred_uncert <- pred_uncert
  dryWinchPars <- winchFit$par
  dryBayesDynamics <- sys.plot.data
  dryBayesCs <- bayesCs
  dryTTdist <- TTdist
  drySAdist <- SAdist
  drypool14 <- fitT0
  dry_poolouts <- poolOutputs
  dryquants <- Cquants
}

if(trial == "Irr. 10"){
  irr10Pars <- plot.data
  irr10SA3 <- SA3best
  irr10TT3 <- TT3best
  irr10_bayes_fit <- bayes_fit
  irr10_pred_uncert <- pred_uncert
  irr10WinchPars <- winchFit$par
  irr10BayesDynamics <- sys.plot.data
  irr10BayesCs <- bayesCs
  irr10TTdist <- TTdist
  irr10SAdist <- SAdist
  irr10pool14 <- fitT0
  irr10_poolouts <- poolOutputs
  irr10quants <- Cquants
}

if(trial == "Irr. 20"){
  irr20Pars <- plot.data
  irr20SA3 <- SA3best
  irr20TT3 <- TT3best
  irr20_bayes_fit <- bayes_fit
  irr20_pred_uncert <- pred_uncert
  irr20WinchPars <- winchFit$par
  irr20BayesDynamics <- sys.plot.data
  irr20BayesCs <- bayesCs
  irr20TTdist <- TTdist
  irr20SAdist <- SAdist
  irr20pool14 <- fitT0
  irr20_poolouts <- poolOutputs
  irr20quants <- Cquants
}

cat('Saving outputs for', trial, '\n')
saveName <- paste0('./',filename,'_',nospace, trialStart, '_', trialEnd, '_OutData.RData')
save(saveName, plot.data, SAlist, TTlist, bayes_fit, pred_uncert, winchPar4Saving, sys.plot.data, poolOutputs,
     SA3best, TT3best, TTdist, SAdist, bayesCs, fitT0, envParms, Cquants, file = saveName)

p = grep(trial, trialList)
MeanSAlist[p] <- SA3best$meanSystemAge[1]
MedSAlist[p] <- SA3best$quantilesSystemAge[2]
MeanTTlist[p] <- TT3best$meanTransitTime[1]
k1list[p] <- winchPar4Saving[1]
k2list[p] <- winchPar4Saving[2]
a21list[p] <- winchPar4Saving[3]
P1agelist[p] <- SA3best$meanPoolAge[1]
P2agelist[p] <- SA3best$meanPoolAge[2]

print(paste(trial, 'finished at:',Sys.time()))

dev.off()


print(paste0('Finished with ', trialStart, ' through ', trialEnd, ' for ', trial))
}
#dev.off()
}

nospace_list <- c('dry', 'irr10', 'irr20', 'unfert', 'resFert', 'highFert')
## Time to create comparison plots


if(pdfout == TRUE){
  pdf(paste0('plots_',pdfname))
}

unfertcols <- c(rgb(254,178,76,alphaVal,,255), rgb(254,178,76,,,255))
rescols <- c(rgb(252,78,42,alphaVal,,255), rgb(252,78,42,,,255))
highcols <- c(rgb(177,0,38,alphaVal,,255), rgb(177,0,38,,,255))

drycols <- c(rgb(127,205,187,alphaVal,,255), rgb(127,205,187,,,255))
irr10cols <- c(rgb(29,145,192,alphaVal,,255), rgb(29,145,192,,,255))
irr20cols <- c(rgb(12,44,132,alphaVal,,255), rgb(12,44,132,,,255))

colz <- c('k1', 'k2', 'a21','slowProp', 'trial', 'trialStart','trialEnd')
irrPars <- data.frame(matrix(ncol = 7))
fertPars <- data.frame(matrix(ncol = 7))
colnames(irrPars) <- colz
colnames(fertPars) <- colz

colz <- c('years', 'trial', 'trialStart', 'mean', 'Q95', 'Q05','wmean', 'wQ95', 'wQ05')
irrTTz <- data.frame(matrix(ncol = 9))
fertTTz <- data.frame(matrix(ncol = 9))
colnames(fertTTz) <- colz
colnames(irrTTz) <- colz

colz <- c('years', 'trial', 'trialStart', 'mean', 'Q95', 'Q05','wmean', 'wQ95', 'wQ05')
irrSAz <- data.frame(matrix(ncol = 9))
fertSAz <- data.frame(matrix(ncol = 9))
colnames(fertSAz) <- colz
colnames(irrSAz) <- colz

bootz <- FALSE

#filename <- 'NewIns_Full_May12_'
#filename <- 'NewIns_Try_May27_'
#filename <- "NewIns_Full_June4_"
#filename <- "HighDens_Full_June5_"
filename <- "NewTBinputs_2020-10-05_"


starts <- c(1958, 1985, 1958)
ends <- c(1992, 2010, 2010)

starts <- c(1958, 1985)
ends <- c(1992, 2010)

starts <- c(1958, 1985)
ends <- c(1992, 2010)

xs <- seq(1, length(starts))
for(x in xs){

  trialStart <- starts[x]
  trialEnd <- ends[x]

  pdf(paste0(filename, trialStart, '_', trialEnd, 'Plots.pdf'))

  # Load and bring in data from each trial into environment
  for(nospace in nospace_list){

    jk <- substr(trial, 1, 3)
    if(trial == 'Irr. 10'){
      jk = 'Ir1'
    }

    #Load data for each trial and save as objects in the environment
    p = grep(nospace, nospace_list)
    print(p)

    filenameP = paste0(filename,'_',nospace, trialStart, '_', trialEnd)

    print(filenameP)

    if(nospace == 'unfert'){
      load(paste0('./',filenameP,'_OutData.RData'))
      unPars <- plot.data
      unfertSA3 <- SA3best
      unfertTT3 <- TT3best
      unfert_bayes_fit <- bayes_fit
      unfert_pred_uncert <- pred_uncert
      unfertWinchPars <- winchPar4Saving
      unfertBayesDynamics <- sys.plot.data
      unfertTTdist <- TTdist
      unfertSAdist <- SAdist
      unfertBayesCs <- bayesCs
      unfertPoolOutputs <- poolOutputs
      unfertquants <- Cquants
      winchFertC <- read.csv('./WinchFertC_full.csv', sep = ',')
      soilC <- data.frame(winchFertC$year, ((winchFertC$unfert / 100) *soilMass), ((winchFertC$unC_err / 100)*soilMass))
      colnames(soilC) <- c('time', 'soilC','err')

      #Manual data input
      dattime = c(Winch14C$year, trialEnd)
      datunfert<- c(Winch14C$f_con, NA)
      daterr <- c(Winch14C$con_err, NA)

      dat = data.frame(dattime, datunfert, daterr)
      colnames(dat) <- c('time', 'soilc14', 'err')

      if(roots == FALSE){
        AGprod <- filter(InFrame, Trial == 'Unfert' & Year >= trialStart & Year <= trialEnd)[,-1]
        inputs <- data.frame(Year = AGprod$Year,inputs = (1.9 * rootmod) + (AGprod$Input * AGmod))
      } else {
        inputs = 1.9
      }

      years = seq(trialStart, trialEnd)

      Ctotal= soilC[soilC$time == trialStart,][2]

      #Initial bulk soil 14C: -27.3

      nospace <- 'unfert'

      F14C0 = F14C0_fert

      unfertcols <- c(rgb(254,178,76,alphaVal,,255), rgb(254,178,76,,,255))
      cols <- c('cornsilk3', 'cornsilk4', 'cornsilk') # Unfert

      atm14Cclip = atm14C[atm14C[,1] >= trialStart - 1, 1:2]
      atm14Cclip = atm14Cclip[atm14Cclip[,1] <= trialEnd, 1:2]
      atm_in = data.frame(atm14Cclip[,1:2])

      dat<- dat[-c(4,6,8,9,10,14),]
      datunfert <- dat
      soilC <- soilC[-c(1,2),]
      soilCun <- soilC

      inputUn <- inputs

      fertInputs[1] <- mean(inputs$inputs)

    }

    if(nospace == 'resFert'){
      load(paste0('./',filenameP,'_OutData.RData'))
      resPars <- plot.data
      resfertSA3 <- SA3best
      resfertTT3 <- TT3best
      resfert_bayes_fit <- bayes_fit
      resfert_pred_uncert <- pred_uncert
      resfertWinchPars <- winchPar4Saving
      resfertBayesDynamics <- sys.plot.data
      resfertTTdist <- TTdist
      resfertSAdist <- SAdist
      resfertBayesCs <- bayesCs
      resfertPoolOutputs <- poolOutputs
      resfertquants <- Cquants

      winchFertC <- read.csv('./WinchFertC_full.csv', sep = ',')

      soilC <- data.frame(winchFertC$year, ((winchFertC$res / 100) *soilMass), (winchFertC$resC_err / 100) *soilMass)
      colnames(soilC) <- c('time', 'soilC', 'err')

      #Manual data input
      dattime = c(Winch14C$year, trialEnd)
      datresfert<- c(Winch14C$f_res, NA)
      daterr <- c(Winch14C$res_err, NA)

      dat = data.frame(dattime, datresfert, daterr)
      colnames(dat) <- c('time', 'soilc14', 'err')

      if(roots == FALSE){
        AGprod <- filter(InFrame, Trial == 'ResFert' & Year >= trialStart & Year <= trialEnd)[,-1]
        inputs <- data.frame(Year = AGprod$Year,inputs = (2.1 * rootmod) + (AGprod$Input * AGmod))
      } else {
        inputs = 1.9
      }

      years = seq(trialStart, trialEnd)

      Ctotal= soilC[soilC$time == trialStart,][2]

      #Initial bulk soil 14C: -26.4

      nospace <- 'resFert'

      F14C0 = F14C0_fert

      rescols <- c(rgb(252,78,42,alphaVal,,255), rgb(252,78,42,,,255)) # Resfert
      cols <- c('goldenrod1', 'goldenrod3') # Resfert

      atm14Cclip = atm14C[atm14C[,1] >= trialStart-1, 1:2]
      atm14Cclip = atm14Cclip[atm14Cclip[,1] <= trialEnd, 1:2]
      atm_in = data.frame(atm14Cclip[,1:2])

      dat<- dat[-c(4,6,8,10,14),]
      datresfert <- dat
      soilC <- soilC[-c(1,2),]
      soilCres <- soilC

      inputRes <- inputs

      fertInputs[2] <- mean(inputs$inputs)

    }

    if(nospace == 'highFert'){
      load(paste0('./',filenameP,'_OutData.RData'))
      highPars <- plot.data
      highfertSA3 <- SA3best
      highfertTT3 <- TT3best
      highfert_bayes_fit <- bayes_fit
      highfert_pred_uncert <- pred_uncert
      highfertWinchPars <- winchPar4Saving
      highfertBayesDynamics <- sys.plot.data
      highfertTTdist <- TTdist
      highfertSAdist <- SAdist
      highfertBayesCs <- bayesCs
      highfertPoolOutputs <- poolOutputs
      highfertquants <- Cquants

      winchFertC <- read.csv('./WinchFertC_full.csv', sep = ',')

      soilC <- data.frame(winchFertC$year, ((winchFertC$high / 100) *soilMass), ((winchFertC$highC_err / 100) *soilMass))
      colnames(soilC) <- c('time', 'soilC','err')

      #Manual data input
      dattime = c(Winch14C$year, trialEnd)
      dathighfert<- c(Winch14C$f_high, NA)
      daterr <- c(Winch14C$high_err, NA)

      dat = data.frame(dattime, dathighfert, daterr)
      colnames(dat) <- c('time', 'soilc14', 'err')

      if(roots == FALSE){
        AGprod <- filter(InFrame, Trial == 'HighFert' & Year >= trialStart & Year <= trialEnd)[,-1]
        inputs <- data.frame(Year = AGprod$Year,inputs = (2.3 * rootmod) + (AGprod$Input * AGmod))
      } else {
        inputs = 2.3
      }

      years = seq(trialStart, trialEnd)

      Ctotal= soilC[soilC$time == trialStart+1,][2]

      #Initial bulk soil 14C: -26.4

      nospace <- 'highFert'

      F14C0 = F14C0_fert

      highcols <- c(rgb(177,0,38,alphaVal,,255), rgb(177,0,38,,,255)) # Highfert
      cols <- c('salmon1', 'darkorange3') # Highfert

      atm14Cclip = atm14C[atm14C[,1] >= trialStart-1, 1:2]
      atm14Cclip = atm14Cclip[atm14Cclip[,1] <= trialEnd, 1:2]
      atm_in = data.frame(atm14Cclip[,1:2])

      # Remove rows with NAs
      dat<- dat[-c(4,6,8,10,14),]
      dathighfert <- dat
      soilC<- soilC[-c(1,2),]
      soilC$err[16] <- 1.17305
      soilChigh <- soilC

      inputHigh <- inputs

      fertInputs[3] <- mean(inputs$inputs)
    }

    if(nospace == 'dry'){
      load(paste0('./',filenameP,'_OutData.RData'))
      dryPars <- plot.data
      drySA3 <- SA3best
      dryTT3 <- TT3best
      dry_bayes_fit <- bayes_fit
      dry_pred_uncert <- pred_uncert
      dryWinchPars <- winchPar4Saving
      dryBayesDynamics <- sys.plot.data
      dryTTdist <- TTdist
      drySAdist <- SAdist
      dryBayesCs <- bayesCs
      dryPoolOutputs <- poolOutputs
      dryquants <- Cquants

      winchIrrC <- read.csv('./WinchCN_timeCondron855.csv', sep = ',')
      soilC <- data.frame(winchIrrC$year, ((winchIrrC$perC_dry / 100) *soilMass), ((winchIrrC$dryC_err / 100) *soilMass))
      colnames(soilC) <- c('time', 'soilC','err')

      #Data input
      dattime = c(Winch14C$year, trialEnd)
      datdry<- c(Winch14C$i_dry, NA)
      daterr <- c(Winch14C$dry_err, NA)

      dat = data.frame(dattime, datdry, daterr)
      colnames(dat) <- c('time', 'soilc14', 'err')

      if(roots == FALSE){
        AGprod <- filter(InFrame, Trial == 'Dry' & Year >= trialStart & Year <= trialEnd)[,-1]
        inputs <- data.frame(Year = AGprod$Year, inputs = (2.0 * rootmod) + (AGprod$Input * AGmod))
      } else {
        inputs = 2.0
      }

      #From Kelliher et al. 2012
      # root <- 1.9 * rootmod
      # inputs = 3.6 + root
      years = seq(trialStart, trialEnd)

      #Initial C content
      Ctotal= soilC[soilC$time == trialStart+1,][2]


      #Initial bulk soil 14C: -43.9

      nospace <- 'dry'

      F14C0 = F14C0_irr

      drycols <- c(rgb(127,205,187,alphaVal,,255), rgb(127,205,187,,,255)) #Dry
      cols <- c("lightsteelblue1", 'lightsteelblue3') #Dry

      atm14Cclip = atm14C[atm14C[,1] >= trialStart-1, 1:2]
      atm14Cclip = atm14Cclip[atm14Cclip[,1] <= trialEnd, 1:2]
      atm_in = data.frame(atm14Cclip[,1:2])

      dat <- dat[-c(9),]
      datdry <- dat
      soilC <- soilC[-c(1,2,3,4,5,6,7,8,9,10,12,14,15,16,17,18,28,29,30,33,34,36,37,39,40,44,46,47,55,56,57,58,59,60),]
      soilCdry <- soilC

      irrInputs[1] <- mean(inputs$inputs)
      inputDry <- inputs
    }

    if(nospace=='irr10'){
      if(trialEnd == 2010){
        filenameP = paste0(filename,'_',nospace, trialStart, '_')
        load(paste0('./',filenameP,'2003_OutData.RData'))
      } else {
        load(paste0('./',filenameP,'_OutData.RData'))
      }
      irr10Pars <- plot.data
      irr10SA3 <- SA3best
      irr10TT3 <- TT3best
      irr10_bayes_fit <- bayes_fit
      irr10_pred_uncert <- pred_uncert
      irr10WinchPars <- winchPar4Saving
      irr10BayesDynamics <- sys.plot.data
      irr10TTdist <- TTdist
      irr10SAdist <- SAdist
      irr10BayesCs <- bayesCs
      irr10PoolOutputs <- poolOutputs
      irr10quants <- Cquants
      soilC <- data.frame(winchIrrC$year, ((winchIrrC$perC_irr10 / 100) *soilMass), ((winchIrrC$irr10C_err /100) *soilMass))
      colnames(soilC) <- c('time', 'soilC','err')


      #Manual data input
      time10 = c(1949,1959, 1961, 1967, 1971, 1975, 1980, 1986, 1991, 1997, 2002, 2003)
      dattime = c(1949,1959, 1961, 1967, 1971, 1975, 1980, 1986, 1991, 1997, 2002, 2003)
      dat10 = c(NA, -26.2, 4.8, 138.8, 201.5, 216.3, 193.9, 167.1, 144.3, 123.8, 104.3, NA)

      dattime = c(Winch14C$year, 2003)
      dat10<- c(Winch14C$i_10, NA)
      daterr <- c(Winch14C$i10_err, NA)

      dat = data.frame(dattime, dat10, daterr)
      colnames(dat) <- c('time', 'soilc14', 'err')

      if(roots == FALSE){
        AGprod <- filter(InFrame, Trial == 'Irr10' & Year >= trialStart & Year <= trialEnd)[,-1]
        inputs <- data.frame(Year = AGprod$Year,inputs = (2.0 * rootmod) + (AGprod$Input * AGmod))
      } else {
        inputs = 2.0
      }

      years = seq(trialStart, trialEnd)

      #Initial C content
      Ctotal= soilC[soilC$time == trialStart+1,][2]

      #Initial bulk soil 14C: -26.2

      nospace <- 'irr10'

      F14C0 = F14C0_irr

      irr10cols <- c(rgb(29,145,192,alphaVal,,255), rgb(29,145,192,,,255)) # Irr 10
      cols <- c("skyblue1",'skyblue3') # Irr 10

      atm14Cclip = atm14C[atm14C[,1] >= trialStart-1, 1:2]
      atm14Cclip = atm14Cclip[atm14Cclip[,1] <= trialEnd, 1:2]
      atm_in = data.frame(atm14Cclip[,1:2])
      dat<- dat[-c(6,8,10),]
      dat10 <- dat
      soilC <- soilC[-c(1,2,3,4,5,6,7,8,9,10,12,14,15,16,17,18,28,29,30,33,34,36,37,39,40,44,46,47,55,56,57,58,59,60),]
      soilC10 <- soilC

      irrInputs[2] <- mean(inputs$inputs)
      input10 <- inputs
    }

    if(nospace == 'irr20'){
      load(paste0('./',filenameP,'_OutData.RData'))
      irr20Pars <- plot.data
      irr20SA3 <- SA3best
      irr20TT3 <- TT3best
      irr20_bayes_fit <- bayes_fit
      irr20_pred_uncert <- pred_uncert
      irr20WinchPars <- winchPar4Saving
      irr20BayesDynamics <- sys.plot.data
      irr20TTdist <- TTdist
      irr20SAdist <- SAdist
      irr20BayesCs <- bayesCs
      irr20PoolOutputs <- poolOutputs
      irr20quants <- Cquants

      soilC <- data.frame(winchIrrC$year, ((winchIrrC$perC_irr20 / 100) *soilMass), ((winchIrrC$irr20C_err / 100) *soilMass))
      colnames(soilC) <- c('time', 'soilC', 'err')


      #Manual data input
      time20 = c(1949,1959, 1961, 1967, 1971, 1975, 1980, 1986, 1991, 1997, 2002, 2003)
      dattime = c(1949,1959, 1961, 1967, 1971, 1975, 1980, 1986, 1991, 1997, 2002, 2003)
      dat20 = c(NA,-36.6, -10.1, 156.5, 203.3, 203.3, 185.9, 156.1, 135.2, 109, 93.7, NA)
      dat20_13 = c(NA,-26.99,-27.26,-27.42, -27.63,-27.45,-27.64,-27.54,-27.96,-27.86,-28.25,NA)

      dattime = c(Winch14C$year, trialEnd)
      dat20<- c(Winch14C$i_20, NA)
      daterr <- c(Winch14C$i20_err, NA)

      dat = data.frame(dattime, dat20, daterr)
      colnames(dat) <- c('time', 'soilc14', 'err')

      if(roots == FALSE){
        AGprod <- filter(InFrame, Trial == 'Irr20' & Year >= trialStart & Year <= trialEnd)[,-1]
        inputs <- data.frame(Year = AGprod$Year,inputs = (2.1 * rootmod) + (AGprod$Input * AGmod))
      } else {
        inputs = 2.1
      }

      #From Kelliher et al. 2012
      # root <- 1.6 * rootmod
      # inputs = 5.9 + root

      years = seq(trialStart, trialEnd)

      #Initial C content
      Ctotal= 32.2 #mg C g soil-1
      #Ctotal = 26.676
      Ctotal= soilC[soilC$time == trialStart+1,][2]

      #Initial bulk soil 14C: -36.6

      nospace <- 'irr20'

      F14C0 = F14C0_irr

      irr20cols <- c(rgb(12,44,132,alphaVal,,255), rgb(12,44,132,,,255)) #Irr 20
      cols <- c('lightblue1', 'dodgerblue2') #Irr 20

      atm14Cclip = atm14C[atm14C[,1] >= trialStart-1, 1:2]
      atm14Cclip = atm14Cclip[atm14Cclip[,1] <= trialEnd, 1:2]
      atm_in = data.frame(atm14Cclip[,1:2])

      dat<- dat[-c(6,8,10),]
      dat20 <- dat
      soilC <- soilC[-c(1,2,3,4,5,6,7,8,9,10,12,14,15,16,17,18,28,29,30,33,34,36,37,39,40,44,46,47,55,56,57,58,59,60),]
      soilC20 <- soilC

      irrInputs[3] <- mean(inputs$inputs)
      input20 <- inputs

    }

    MeanSAlist[p] <- SA3best$meanSystemAge[1]
    MedSAlist[p] <- SA3best$quantilesSystemAge[2]
    MeanTTlist[p] <- TT3best$meanTransitTime[1]
    k1list[p] <- winchPar4Saving[1]
    k2list[p] <- winchPar4Saving[2]
    a21list[p] <- winchPar4Saving[3]
    slowlist[p] <- winchPar4Saving[4]
    P1agelist[p] <- SA3best$meanPoolAge[1]
    P2agelist[p] <- SA3best$meanPoolAge[2]
  }

  #Annual Inputs, adjusted for AG and BG return/production
  InputWin <- data.frame(Trial = character(), Input = double(), Year = integer())
  for(t in c("Dry","Irr10","Irr20","Unfert","ResFert","HighFert")){
    InputWin <-
      rbind(InputWin, data.frame(Trial = t,
                                 Input = (filter(InFrame, Trial == t & Year >= trialStart & Year <= trialEnd)$Input * AGmod) +
                                   (rootProd[t] * rootmod),
                                 Year = seq(trialStart, trialEnd)))
  }

  InputWin$Trial <- as.character(factor(InputWin$Trial, levels = c("Dry","Irr10","Irr20","Unfert","ResFert","HighFert"), ordered = TRUE))

  InputsMean <- aggregate(InputWin$Input~InputWin$Trial, FUN = mean)
  InputsSD <- aggregate(InputWin$Input~InputWin$Trial, FUN = sd)
  irrInputs <- filter(InputsMean,`InputWin$Trial` == "Dry" | `InputWin$Trial` ==  "Irr10" | `InputWin$Trial` == "Irr20")[,2]
  irrInputsSD <- filter(InputsSD,`InputWin$Trial` == "Dry" | `InputWin$Trial` ==  "Irr10" | `InputWin$Trial` == "Irr20")[,2]
  fertInputs <- filter(InputsMean,`InputWin$Trial` == "Unfert" | `InputWin$Trial` ==  "ResFert" | `InputWin$Trial` == "HighFert")
  fertInputs <- arrange(fertInputs, -xtfrm(fertInputs$`InputWin$Trial`))[,2]
  fertInputsSD <- filter(InputsSD,`InputWin$Trial` == "Unfert" | `InputWin$Trial` ==  "ResFert" | `InputWin$Trial` == "HighFert")
  fertInputsSD <- arrange(fertInputsSD, -xtfrm(fertInputsSD$`InputWin$Trial`))[,2]

  dry_pred_uncert_p <- data.frame(dry_pred_uncert, trial = 'Dry', trialStart = factor(paste0(trialStart, ' - ', trialEnd)), trialEnd = trialEnd)
  irr10_pred_uncert_p <- data.frame(irr10_pred_uncert, trial = 'Irr. 10', trialStart = factor(paste0(trialStart, ' - ', trialEnd)), trialEnd = trialEnd)
  irr20_pred_uncert_p <- data.frame(irr20_pred_uncert, trial = 'Irr. 20', trialStart = factor(paste0(trialStart, ' - ', trialEnd)), trialEnd = trialEnd)
  unfert_pred_uncert_p <- data.frame(unfert_pred_uncert, trial = 'Unfert', trialStart = factor(paste0(trialStart, ' - ', trialEnd)), trialEnd = trialEnd)
  resfert_pred_uncert_p <- data.frame(resfert_pred_uncert, trial = 'Res. Fert', trialStart = factor(paste0(trialStart, ' - ', trialEnd)), trialEnd = trialEnd)
  highfert_pred_uncert_p <- data.frame(highfert_pred_uncert, trial = 'High Fert', trialStart = factor(paste0(trialStart, ' - ', trialEnd)), trialEnd = trialEnd)

  colz <- c('k1', 'k2', 'a21','slowProp', 'trial', 'trialStart','trialEnd')

  irrPars <- rbind(irrPars[complete.cases(irrPars),], select(dry_pred_uncert_p, colz), select(irr10_pred_uncert_p, colz),
                   select(irr20_pred_uncert_p, colz))

  fertPars <- rbind(fertPars[complete.cases(fertPars),], select(unfert_pred_uncert_p, colz), select(resfert_pred_uncert_p, colz),
                    select(highfert_pred_uncert_p, colz))

  fertPars$trial <- factor(fertPars$trial, levels = c('Unfert', 'Res. Fert', 'High Fert'), ordered = TRUE)

  ### Irrigation mass weighted transit times - tidy style

  dryTTdist %>%
    gather(rep, Dry, -years) %>%
    left_join(irr10TTdist %>%
                gather(rep, 'Irr. 10', -years), by= c("years", "rep")
    ) %>%
    left_join(irr20TTdist %>%
                gather(rep, 'Irr. 20', -years), by = c('years', 'rep')
    ) %>%
    gather(trial, TTdist, Dry:'Irr. 20', factor_key = TRUE) %>%
    mutate(
      mass = case_when(trial == 'Dry' ~ TTdist * mean(
        filter(InputWin, InputWin$Trial == 'Dry' & Year >= trialStart & Year <= trialEnd)$Input),
        trial == 'Irr. 10' ~ TTdist * mean(
          filter(InputWin, InputWin$Trial == 'Irr10' & Year >= trialStart & Year <= trialEnd)$Input),
        trial == 'Irr. 20' ~ TTdist * mean(
          filter(InputWin, InputWin$Trial == 'Irr20' & Year >= trialStart & Year <= trialEnd)$Input))) %>%
    group_by(years, trial) %>%
    summarise(
      quantile_005_mass = quantile(mass, probs = 0.05),
      quantile_095_mass = quantile(mass, probs = 0.95),
      mean_mass = mean(mass)
    ) %>% {. ->> finalIrrDist} %>%
    ggplot(aes(x=years, y=mean_mass, col=trial, fill = trial,
               ymin = quantile_005_mass, ymax = quantile_095_mass)) + xlim(0, 25) + geom_line(size = 1.5) +
    geom_ribbon(alpha = 0.6) + #ggtitle('Mass-weighted Transit Time Distribution') +
    scale_color_manual(values=c(drycols[1], irr10cols[1], irr20cols[1])) +
    scale_fill_manual(values=c(drycols[2], irr10cols[2], irr20cols[2])) +
    ylab('Mg C ha') + xlab('Years') + ylim(0,0.85) +
    theme(text=element_text(size=21), panel.background = element_blank(), legend.position = 'none',
          axis.line = element_line(color = 'black', size = 1)) +
    guides(fill=guide_legend(title="Trial"))


  ### Fertilizer mass weighted transit times - tidy style

  unfertTTdist %>%
    gather(rep, 'Unfert', -years) %>%
    left_join(resfertTTdist %>%
                gather(rep, 'Res. Fert', -years), by= c("years", "rep")
    ) %>%
    left_join(highfertTTdist %>%
                gather(rep, 'High Fert', -years), by = c('years', 'rep')
    ) %>%
    gather(trial, TTdist, Unfert:'High Fert', factor_key = TRUE) %>%
    mutate(
      mass = case_when(trial == 'Unfert' ~ TTdist * mean(
        filter(InputWin, InputWin$Trial == 'Unfert' & Year >= trialStart & Year <= trialEnd)$Input),
        trial == 'Res. Fert' ~ TTdist * mean(
          filter(InputWin, InputWin$Trial == 'ResFert' & Year >= trialStart & Year <= trialEnd)$Input),
        trial == 'High Fert' ~ TTdist * mean(
          filter(InputWin, InputWin$Trial == 'HighFert' & Year >= trialStart & Year <= trialEnd)$Input))) %>%
    group_by(years, trial) %>%
    summarise(
      quantile_005_mass = quantile(mass, probs = 0.025),
      quantile_095_mass = quantile(mass, probs = 0.975),
      mean_mass = mean(mass)
    ) %>% {. ->> finalFertDist} %>%
    ggplot(aes(x=years, y=mean_mass, col=trial, fill = trial,
               ymin = quantile_005_mass, ymax = quantile_095_mass)) + xlim(0, 25) + geom_line(size = 1.5) +
    geom_ribbon(alpha = 0.6) + #ggtitle('Mass-weighted Transit Time Distribution') +
    scale_color_manual(values=c(unfertcols[1], rescols[1], highcols[1])) +
    scale_fill_manual(values=c(unfertcols[2], rescols[2], highcols[2])) +
    ylab('Mg C ha') + xlab('Years') + ylim(0,0.85) +
    theme(text=element_text(size=21), panel.background = element_blank(),
          axis.line = element_line(color = 'black', size = 1), legend.position = 'none') +
    guides(fill=guide_legend(title="Trial"))


  #Calculate 5 and 95th percentile uncertainties
  dryTTdist$Q95 <- apply(dryTTdist[,!(names(dryTTdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  dryTTdist$Q05 <- apply(dryTTdist[,!(names(dryTTdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  dryTTdist$mean <- apply(dryTTdist[,!(names(dryTTdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  #dryTTdist$median <- apply(dryTTdist[,!(names(dryTTdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) median(x))
  irr10TTdist$Q95 <- apply(irr10TTdist[,!(names(irr10TTdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  irr10TTdist$Q05 <- apply(irr10TTdist[,!(names(irr10TTdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  irr10TTdist$mean <- apply(irr10TTdist[,!(names(irr10TTdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  irr20TTdist$Q95 <- apply(irr20TTdist[,!(names(irr20TTdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  irr20TTdist$Q05 <- apply(irr20TTdist[,!(names(irr20TTdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  irr20TTdist$mean <- apply(irr20TTdist[,!(names(irr20TTdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  #irr20TTdist$median <- apply(irr20TTdist[,!(names(irr20TTdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median'))], 1, function(x) median(x))


  #Calculate 5 and 95th percentile uncertainties for transit times
  unfertTTdist$Q95 <- apply(unfertTTdist[,!(names(unfertTTdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart'))], 1, function(x) quantile(x, probs = 0.95))
  unfertTTdist$Q05 <- apply(unfertTTdist[,!(names(unfertTTdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart'))], 1, function(x) quantile(x, probs = 0.05))
  unfertTTdist$mean <- apply(unfertTTdist[,!(names(unfertTTdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart'))], 1, function(x) mean(x))
  resfertTTdist$Q95 <- apply(resfertTTdist[,!(names(resfertTTdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart'))], 1, function(x) quantile(x, probs = 0.95))
  resfertTTdist$Q05 <- apply(resfertTTdist[,!(names(resfertTTdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart'))], 1, function(x) quantile(x, probs = 0.05))
  resfertTTdist$mean <- apply(resfertTTdist[,!(names(resfertTTdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart'))], 1, function(x) mean(x))
  highfertTTdist$Q95 <- apply(highfertTTdist[,!(names(highfertTTdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart'))], 1, function(x) quantile(x, probs = 0.95))
  highfertTTdist$Q05 <- apply(highfertTTdist[,!(names(highfertTTdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart'))], 1, function(x) quantile(x, probs = 0.05))
  highfertTTdist$mean <- apply(highfertTTdist[,!(names(highfertTTdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart'))], 1, function(x) mean(x))

  #Input Weighted (When steady state is assumed)
  unfertTTdist$wQ95 <- apply(unfertTTdist[,!(names(unfertTTdist) %in% c('years', 'Q95', 'Q05', 'mean','wQ95', 'wQ05', 'wmean','trial','trialStart'))], 1, function(x) quantile(x, probs = 0.95)) * fertInputs[1]
  unfertTTdist$wQ05 <- apply(unfertTTdist[,!(names(unfertTTdist) %in% c('years', 'Q95', 'Q05', 'mean','wQ95', 'wQ05', 'wmean','trial','trialStart'))], 1, function(x) quantile(x, probs = 0.05)) * fertInputs[1]
  unfertTTdist$wmean <- apply(unfertTTdist[,!(names(unfertTTdist) %in% c('years', 'Q95', 'Q05', 'mean','wQ95', 'wQ05', 'wmean','trial','trialStart'))], 1, function(x) mean(x)) * fertInputs[1]
  resfertTTdist$wQ95 <- apply(resfertTTdist[,!(names(resfertTTdist) %in% c('years', 'Q95', 'Q05', 'mean','wQ95', 'wQ05', 'wmean','trial','trialStart'))], 1, function(x) quantile(x, probs = 0.95)) * fertInputs[2]
  resfertTTdist$wQ05 <- apply(resfertTTdist[,!(names(resfertTTdist) %in% c('years', 'Q95', 'Q05', 'mean','wQ95', 'wQ05', 'wmean','trial','trialStart'))], 1, function(x) quantile(x, probs = 0.05)) * fertInputs[2]
  resfertTTdist$wmean <- apply(resfertTTdist[,!(names(resfertTTdist) %in% c('years', 'Q95', 'Q05', 'mean','wQ95', 'wQ05', 'wmean','trial','trialStart'))], 1, function(x) mean(x)) * fertInputs[2]
  highfertTTdist$wQ95 <- apply(highfertTTdist[,!(names(highfertTTdist) %in% c('years', 'Q95', 'Q05', 'mean','wQ95', 'wQ05', 'wmean','trial','trialStart'))], 1, function(x) quantile(x, probs = 0.95)) * fertInputs[3]
  highfertTTdist$wQ05 <- apply(highfertTTdist[,!(names(highfertTTdist) %in% c('years', 'Q95', 'Q05', 'mean','wQ95', 'wQ05', 'wmean','trial','trialStart'))], 1, function(x) quantile(x, probs = 0.05)) * fertInputs[3]
  highfertTTdist$wmean <- apply(highfertTTdist[,!(names(highfertTTdist) %in% c('years', 'Q95', 'Q05', 'mean','wQ95', 'wQ05', 'wmean','trial','trialStart'))], 1, function(x) mean(x)) * fertInputs[3]

  dryTTdist$wQ95 <- apply(dryTTdist[,!(names(dryTTdist) %in% c('years', 'Q95', 'Q05', 'mean','wQ95', 'wQ05', 'wmean','trial','trialStart'))], 1, function(x) quantile(x, probs = 0.95)) * irrInputs[1]
  dryTTdist$wQ05 <- apply(dryTTdist[,!(names(dryTTdist) %in% c('years', 'Q95', 'Q05', 'mean','wQ95', 'wQ05', 'wmean','trial','trialStart'))], 1, function(x) quantile(x, probs = 0.05)) * irrInputs[1]
  dryTTdist$wmean <- apply(dryTTdist[,!(names(dryTTdist) %in% c('years', 'Q95', 'Q05', 'mean','wQ95', 'wQ05', 'wmean','trial','trialStart'))], 1, function(x) mean(x)) * irrInputs[1]
  irr10TTdist$wQ95 <- apply(irr10TTdist[,!(names(irr10TTdist) %in% c('years', 'Q95', 'Q05', 'mean','wQ95', 'wQ05', 'wmean','trial','trialStart'))], 1, function(x) quantile(x, probs = 0.95)) * irrInputs[2]
  irr10TTdist$wQ05 <- apply(irr10TTdist[,!(names(irr10TTdist) %in% c('years', 'Q95', 'Q05', 'mean','wQ95', 'wQ05', 'wmean','trial','trialStart'))], 1, function(x) quantile(x, probs = 0.05)) * irrInputs[2]
  irr10TTdist$wmean <- apply(irr10TTdist[,!(names(irr10TTdist) %in% c('years', 'Q95', 'Q05', 'mean','wQ95', 'wQ05', 'wmean','trial','trialStart'))], 1, function(x) mean(x)) * irrInputs[2]
  irr20TTdist$wQ95 <- apply(irr20TTdist[,!(names(irr20TTdist) %in% c('years', 'Q95', 'Q05', 'mean','wQ95', 'wQ05', 'wmean','trial','trialStart'))], 1, function(x) quantile(x, probs = 0.95)) * irrInputs[3]
  irr20TTdist$wQ05 <- apply(irr20TTdist[,!(names(irr20TTdist) %in% c('years', 'Q95', 'Q05', 'mean','wQ95', 'wQ05', 'wmean','trial','trialStart'))], 1, function(x) quantile(x, probs = 0.05)) * irrInputs[3]
  irr20TTdist$wmean <- apply(irr20TTdist[,!(names(irr20TTdist) %in% c('years', 'Q95', 'Q05', 'mean','wQ95', 'wQ05', 'wmean','trial','trialStart'))], 1, function(x) mean(x)) * irrInputs[3]

  #Now for system age
  #Calculate 5 and 95th percentile uncertainties
  drySAdist$Q95 <- apply(drySAdist[,!(names(drySAdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  drySAdist$Q05 <- apply(drySAdist[,!(names(drySAdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  drySAdist$mean <- apply(drySAdist[,!(names(drySAdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  #drySAdist$median <- apply(drySAdist[,!(names(drySAdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) median(x))
  irr10SAdist$Q95 <- apply(irr10SAdist[,!(names(irr10SAdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  irr10SAdist$Q05 <- apply(irr10SAdist[,!(names(irr10SAdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  irr10SAdist$mean <- apply(irr10SAdist[,!(names(irr10SAdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  irr20SAdist$Q95 <- apply(irr20SAdist[,!(names(irr20SAdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  irr20SAdist$Q05 <- apply(irr20SAdist[,!(names(irr20SAdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  irr20SAdist$mean <- apply(irr20SAdist[,!(names(irr20SAdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  #irr20SAdist$median <- apply(irr20SAdist[,!(names(irr20SAdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median'))], 1, function(x) median(x))


  #Calculate 5 and 95th percentile uncertainties for transit times
  unfertSAdist$Q95 <- apply(unfertSAdist[,!(names(unfertSAdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart'))], 1, function(x) quantile(x, probs = 0.95))
  unfertSAdist$Q05 <- apply(unfertSAdist[,!(names(unfertSAdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart'))], 1, function(x) quantile(x, probs = 0.05))
  unfertSAdist$mean <- apply(unfertSAdist[,!(names(unfertSAdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart'))], 1, function(x) mean(x))
  resfertSAdist$Q95 <- apply(resfertSAdist[,!(names(resfertSAdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart'))], 1, function(x) quantile(x, probs = 0.95))
  resfertSAdist$Q05 <- apply(resfertSAdist[,!(names(resfertSAdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart'))], 1, function(x) quantile(x, probs = 0.05))
  resfertSAdist$mean <- apply(resfertSAdist[,!(names(resfertSAdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart'))], 1, function(x) mean(x))
  highfertSAdist$Q95 <- apply(highfertSAdist[,!(names(highfertSAdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart'))], 1, function(x) quantile(x, probs = 0.95))
  highfertSAdist$Q05 <- apply(highfertSAdist[,!(names(highfertSAdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart'))], 1, function(x) quantile(x, probs = 0.05))
  highfertSAdist$mean <- apply(highfertSAdist[,!(names(highfertSAdist) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart'))], 1, function(x) mean(x))

  dryTTdist$trial <- 'Dry'
  irr10TTdist$trial <- 'Irr. 10'
  irr20TTdist$trial <- 'Irr. 20'
  dryTTdist$trialStart <-  factor(paste0(trialStart, ' - ', trialEnd))
  irr10TTdist$trialStart <-  factor(paste0(trialStart, ' - ', trialEnd))
  irr20TTdist$trialStart <- factor(paste0(trialStart, ' - ', trialEnd))

  unfertTTdist$trial <- 'Unfert'
  resfertTTdist$trial <- 'Res. Fert'
  highfertTTdist$trial <- 'High Fert'
  unfertTTdist$trialStart <-  factor(paste0(trialStart, ' - ', trialEnd))
  resfertTTdist$trialStart <-  factor(paste0(trialStart, ' - ', trialEnd))
  highfertTTdist$trialStart <-  factor(paste0(trialStart, ' - ', trialEnd))

  drySAdist$trial <- 'Dry'
  irr10SAdist$trial <- 'Irr. 10'
  irr20SAdist$trial <- 'Irr. 20'
  drySAdist$trialStart <-  factor(paste0(trialStart, ' - ', trialEnd))
  irr10SAdist$trialStart <-  factor(paste0(trialStart, ' - ', trialEnd))
  irr20SAdist$trialStart <- factor(paste0(trialStart, ' - ', trialEnd))

  unfertSAdist$trial <- 'Unfert'
  resfertSAdist$trial <- 'Res. Fert'
  highfertSAdist$trial <- 'High Fert'
  unfertSAdist$trialStart <-  factor(paste0(trialStart, ' - ', trialEnd))
  resfertSAdist$trialStart <-  factor(paste0(trialStart, ' - ', trialEnd))
  highfertSAdist$trialStart <-  factor(paste0(trialStart, ' - ', trialEnd))

  colz <- c('years', 'trial', 'trialStart', 'mean', 'Q95', 'Q05','wmean', 'wQ95', 'wQ05')

  irrTTz <- rbind(irrTTz[complete.cases(irrTTz),], select(dryTTdist, colz), select(irr10TTdist, colz), select(irr20TTdist, colz))
  fertTTz <- rbind(fertTTz[complete.cases(fertTTz),], select(unfertTTdist, colz), select(resfertTTdist, colz), select(highfertTTdist, colz))
  fertTTz$trial <- factor(fertTTz$trial, levels = c('Unfert', 'Res. Fert', 'High Fert'), ordered = TRUE)

  colz <- c('years', 'trial', 'trialStart', 'mean', 'Q95', 'Q05')

  irrSAz <- rbind(irrSAz[complete.cases(irrSAz),], select(drySAdist, colz), select(irr10SAdist, colz), select(irr20SAdist, colz))
  fertSAz <- rbind(fertSAz[complete.cases(fertSAz),], select(unfertSAdist, colz), select(resfertSAdist, colz), select(highfertSAdist, colz))
  fertSAz$trial <- factor(fertSAz$trial, levels = c('Unfert', 'Res. Fert', 'High Fert'), ordered = TRUE)

  unfertcols <- c(rgb(254,178,76,alphaVal,,255), rgb(254,178,76,,,255))
  rescols <- c(rgb(252,78,42,alphaVal,,255), rgb(252,78,42,,,255))
  highcols <- c(rgb(177,0,38,alphaVal,,255), rgb(177,0,38,,,255))

  drycols <- c(rgb(127,205,187,alphaVal,,255), rgb(127,205,187,,,255))
  irr10cols <- c(rgb(29,145,192,alphaVal,,255), rgb(29,145,192,,,255))
  irr20cols <- c(rgb(12,44,132,alphaVal,,255), rgb(12,44,132,,,255))

  irrTTz$trial <- factor(irrTTz$trial, levels = c('Dry', 'Irr. 10', "Irr. 20"))

  print(ggplot(irrTTz, aes(x = years, y = mean, fill = trial, ymax = Q95, ymin = Q05)) + geom_line() +
          facet_grid(irrTTz$trialStart, scales = 'free_y') + theme_bw() +
          xlim(0, 25) + scale_fill_manual(values = c(drycols[2], irr10cols[2], irr20cols[2])) +
          geom_ribbon(alpha = .7) + ggtitle('Transit Time Distributions') + ylim(0, 0.32) +
          xlab('Years') + ylab('Density') + guides(fill=guide_legend(title="Trial")) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 18, face = 'bold'),
                axis.title=element_text(size=14,face="bold"),
                strip.text.y = element_text(size = 12, face = 'bold')) + theme_bw()) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

  print(ggplot(irrTTz, aes(x = years, y = wmean, fill = trial, ymax = wQ95, ymin = wQ05)) + geom_line() +
          facet_grid(irrTTz$trialStart) + theme_bw() +
          xlim(0, 25) + scale_fill_manual(values = c(drycols[2], irr10cols[2], irr20cols[2])) +
          geom_ribbon(alpha = .7) + ggtitle('Flux-Weighted Transit Time Distributions') + #ylim(0, 0.32) +
          xlab('Years') + ylab(expression(paste('T C '*ha^-1))) + guides(fill=guide_legend(title="Trial")) + #ylim(0,1.1) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 18, face = 'bold'),
                axis.title=element_text(size=14,face="bold"),
                strip.text.y = element_text(size = 12, face = 'bold'))) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

  fertTTz$trial <- factor(fertTTz$trial, levels = c('Unfert', 'Res. Fert', "High Fert"), ordered = TRUE)

  print(ggplot(fertTTz, aes(x = years, y = mean, fill = trial, ymax = Q95, ymin = Q05)) + geom_line() + facet_grid(fertTTz$trialStart, scales = 'free_y') +
          xlim(0, 25) + scale_fill_manual(values = c(unfertcols[2], rescols[2], highcols[2])) + theme_bw() +
          geom_ribbon(alpha = .7)+ ggtitle('Transit Time Distributions') + ylim(0, 0.32) +
          xlab('Years') + ylab('Density') + guides(fill=guide_legend(title="Trial")) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 18, face = 'bold'),
                axis.title=element_text(size=14,face="bold"),
                strip.text.y = element_text(size = 12,face = 'bold')))

  print(ggplot(fertTTz, aes(x = years, y = wmean, fill = trial, ymax = wQ95, ymin = wQ05)) + geom_line() + facet_grid(fertTTz$trialStart, scales = 'free') +
          xlim(0, 25) + scale_fill_manual(values = c(unfertcols[2], rescols[2], highcols[2])) + theme_bw() +
          geom_ribbon(alpha = .7)+ ggtitle('Flux-Weighted Transit Time Distributions') + #ylim(0, 1.1) +
          xlab('Years') + ylab(expression(paste('T C '*ha^-1))) + guides(fill=guide_legend(title="Trial")) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 18, face = 'bold'),
                axis.title=element_text(size=14,face="bold"),
                strip.text.y = element_text(size = 12,face = 'bold')))

  #System age distributions
  irrSAz$trial <- factor(irrSAz$trial, levels = c('Dry', 'Irr. 10', "Irr. 20"))

  print(ggplot(irrSAz, aes(x = years, y = mean, fill = trial, ymax = Q95, ymin = Q05)) + geom_line() +
          facet_grid(irrSAz$trialStart, scales = 'free_y') + theme_bw() +
          xlim(0, 50) + scale_fill_manual(values = c(drycols[2], irr10cols[2], irr20cols[2])) +
          geom_ribbon(alpha = .7) + ggtitle('System Age Distributions') + #ylim(0, 0.32) +
          xlab('Years') + ylab('Density') + guides(fill=guide_legend(title="Trial")) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 18, face = 'bold'),
                axis.title=element_text(size=14,face="bold"),
                strip.text.y = element_text(size = 12, face = 'bold')) + theme_bw())

  fertSAz$trial <- factor(fertSAz$trial, levels = c('Unfert', 'Res. Fert', "High Fert"), ordered = TRUE)

  print(ggplot(fertSAz, aes(x = years, y = mean, fill = trial, ymax = Q95, ymin = Q05)) + geom_line() + facet_grid(fertSAz$trialStart, scales = 'free_y') +
          xlim(0, 50) + scale_fill_manual(values = c(unfertcols[2], rescols[2], highcols[2])) + theme_bw() +
          geom_ribbon(alpha = .7)+ ggtitle('System Age Distributions') + #ylim(0, 0.32) +
          xlab('Years') + ylab('Density') + guides(fill=guide_legend(title="Trial")) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 18, face = 'bold'),
                axis.title=element_text(size=14,face="bold"),
                strip.text.y = element_text(size = 12,face = 'bold')))

  ### Parameter plots by year ###

  print(ggplot(irrPars, aes(x = trial, y = k1, fill = trial)) + geom_boxplot() + facet_wrap(irrPars$trialStart) + #, scales = 'free_y') +
          scale_fill_manual(values = c(drycols[2], irr10cols[2], irr20cols[2]))+ theme_bw() +
          xlab('Trial') + ylab('k1') + guides(fill=guide_legend(title="Trial")) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 18, face = 'bold'),
                axis.title=element_text(size=14,face="bold"),
                strip.text.x = element_text(size = 12, face = 'bold')))

  print(ggplot(irrPars, aes(x = trial, y = k2, fill = trial)) + geom_boxplot() + facet_wrap(irrPars$trialStart) +
          scale_fill_manual(values = c(drycols[2], irr10cols[2], irr20cols[2]))+theme_bw() +
          xlab('Trial') + ylab('k2') + guides(fill=guide_legend(title="Trial")) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 18, face = 'bold'),
                axis.title=element_text(size=14,face="bold"),
                strip.text.x = element_text(size = 12, face = 'bold')))

  print(ggplot(irrPars, aes(x = trial, y = a21, fill = trial)) + geom_boxplot() + facet_wrap(irrPars$trialStart) +
          scale_fill_manual(values = c(drycols[2], irr10cols[2], irr20cols[2]))+theme_bw() +
          xlab('Trial') + ylab('a21') + guides(fill=guide_legend(title="Trial")) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 18, face = 'bold'),
                axis.title=element_text(size=14,face="bold"),
                strip.text.x = element_text(size = 12, face = 'bold')))

  print(ggplot(irrPars, aes(x = trial, y = slowProp, fill = trial)) + geom_boxplot() + facet_wrap(irrPars$trialStart) +
          scale_fill_manual(values = c(drycols[2], irr10cols[2], irr20cols[2]))+theme_bw() +
          xlab('Trial') + ylab('slowProp') + guides(fill=guide_legend(title="Trial")) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 18, face = 'bold'),
                axis.title=element_text(size=14,face="bold"),
                strip.text.x = element_text(size = 12, face = 'bold')))

  # Fertilizer Plots
  print(ggplot(fertPars, aes(x = trial, y = k1, fill = trial)) + geom_boxplot(notch = TRUE) + facet_wrap(fertPars$trialStart) + #, scales = 'free_y'
          scale_fill_manual(values = c(unfertcols[2], rescols[2], highcols[2]))+theme_bw() +
          xlab('Trial') + ylab('k1') + guides(fill=guide_legend(title="Trial")) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 18, face = 'bold'),
                axis.title=element_text(size=14,face="bold"),
                strip.text.y = element_text(size = 12)))

  print(ggplot(fertPars, aes(x = trial, y = k2, fill = trial)) + geom_boxplot(notch = TRUE) + facet_wrap(fertPars$trialStart) +
          scale_fill_manual(values = c(unfertcols[2], rescols[2], highcols[2]))+theme_bw() +
          xlab('Trial') + ylab('k2') + guides(fill=guide_legend(title="Trial")) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 18, face = 'bold'),
                axis.title=element_text(size=14,face="bold"),
                strip.text.x = element_text(size = 12, face = 'bold')))

  print(ggplot(fertPars, aes(x = trial, y = a21, fill = trial)) + geom_boxplot(notch = TRUE) + facet_wrap(fertPars$trialStart) +
          scale_fill_manual(values = c(unfertcols[2], rescols[2], highcols[2]))+theme_bw() +
          xlab('Trial') + ylab('a21') + guides(fill=guide_legend(title="Trial")) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 18, face = 'bold'),
                axis.title=element_text(size=14,face="bold"),
                strip.text.x = element_text(size = 12, face = 'bold')))

  print(ggplot(fertPars, aes(x = trial, y = slowProp, fill = trial)) + geom_boxplot(notch = TRUE) + facet_wrap(fertPars$trialStart) +
          scale_fill_manual(values = c(unfertcols[2], rescols[2], highcols[2]))+theme_bw() +
          xlab('Trial') + ylab('slowProp') + guides(fill=guide_legend(title="Trial")) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 18, face = 'bold'),
                axis.title=element_text(size=14,face="bold"),
                strip.text.x = element_text(size = 12, face = 'bold')))

  # print(ggplot(fertPars, aes(x = trial, y = p1, fill = trial)) + geom_boxplot() + facet_wrap(fertPars$trialStart) +
  #         scale_fill_manual(values = c(unfertcols[2], rescols[2], highcols[2])))
  #
  # print(ggplot(fertPars, aes(x = trial, y = p2, fill = trial)) + geom_boxplot() + facet_wrap(fertPars$trialStart) +
  #         scale_fill_manual(values = c(unfertcols[2], rescols[2], highcols[2])))

  #}


  # Compile and plot a bunch of statistics for all irrigation trials

  atm14Cclip = atm14C[atm14C[,1] >= trialStart-1, 1:2]
  atm14Cclip = atm14Cclip[atm14Cclip[,1] <= trialEnd, 1:2]
  atm_in = data.frame(atm14Cclip[,1:2])

  soilCdry$Trial = 'Dry'
  soilC10$Trial = 'Irr. 10'
  soilC20$Trial = 'Irr. 20'
  irrC <- rbind(soilCdry, soilC10,soilC20)

  confMod <- 1.5

  if(trialEnd == 2010){
    irr10End = 2003
  } else {
    irr10End = trialEnd
  }

  fertLims_C14 <- data.frame(subset(unfert_pred_uncert, select = 7:(trialEnd - trialStart + 7)),
                             subset(resfert_pred_uncert, select = 7:(trialEnd - trialStart + 7)),
                             subset(highfert_pred_uncert, select = 7:(trialEnd - trialStart + 7)))

  irrLims_C14 <- data.frame(subset(dry_pred_uncert, select = 7:(trialEnd - trialStart + 7)),
                            subset(irr10_pred_uncert, select = 7:(irr10End - trialStart + 7)),
                            subset(irr20_pred_uncert, select = 7:(trialEnd - trialStart + 7)))

  fertLims_C <- data.frame(subset(unfert_pred_uncert, select = (trialEnd - trialStart + 8):length(unfert_pred_uncert)),
                           subset(resfert_pred_uncert, select = (trialEnd - trialStart + 8):length(resfert_pred_uncert)),
                           subset(highfert_pred_uncert, select = (trialEnd - trialStart + 8):length(highfert_pred_uncert)))

  irrLims_C <- data.frame(subset(dry_pred_uncert, select = (trialEnd - trialStart + 8):length(dry_pred_uncert)),
                          subset(irr10_pred_uncert, select = (irr10End - trialStart + 8):length(irr10_pred_uncert)),
                          subset(irr20_pred_uncert, select = (trialEnd - trialStart + 8):length(irr20_pred_uncert)))

  lim_y <- c(min(fertLims_C14 -10), max(fertLims_C14))

  plot(summary(unfert_pred_uncert), which = 1, main = 'Fertilizer Trial 14C Model Outputs', col = unfertcols, quant = TRUE, ylim = lim_y, xlim = c(trialStart - 3, trialEnd),
       legpos = NULL, lwd=2, ylab = expression(paste(Delta^{14}, "C")), xlab = 'Year', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  lines(atm14C, lwd = 2, col = rgb(74,20,134,,,255))
  par(new = T)
  plot(summary(resfert_pred_uncert), which = 1, col = rescols,quant = TRUE, ylim = lim_y, xlim = c(trialStart - 3, trialEnd), main = '', legpos = NULL, lwd=2, ylab = '', xlab ='', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  par(new = T)
  plot(summary(highfert_pred_uncert), which = 1, col = highcols ,quant = TRUE, ylim = lim_y, xlim = c(trialStart - 3, trialEnd), main = '', legpos = NULL, lwd=2, ylab = '', xlab ='', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  arrows(datunfert$time, datunfert$soilc14- datunfert$err,datunfert$time, datunfert$soilc14+ datunfert$err,length=0.05, angle=90, code=3)
  arrows(datresfert$time, datresfert$soilc14- datresfert$err,datresfert$time, datresfert$soilc14+ datresfert$err,length=0.05, angle=90, code=3)
  arrows(dathighfert$time, dathighfert$soilc14- dathighfert$err,dathighfert$time, dathighfert$soilc14+ dathighfert$err,length=0.05, angle=90, code=3)
  points(datunfert$time, datunfert$soilc14, bg = unfertcols[2], pch = 21, cex = 2)
  points(datresfert$time, datresfert$soilc14, bg = rescols[2], pch = 22, cex = 2)
  points(dathighfert$time, dathighfert$soilc14, bg = highcols[2], pch = 24, cex = 2)
  legend('bottom', legend = c('Unfert.', 'Res. Fert', 'High Fert', "IQR", '95% CI'), pch = c(21,22,24, NA,NA), pt.bg = c(unfertcols[2], rescols[2], highcols[2],NA,NA),
         pt.cex = 2, border = NA, fill = c(NA, NA, NA, 'grey50','grey80'), bty = 'n')

  lim_y <- c(min(irrLims_C14 - 10), max(irrLims_C14))

  plot(summary(dry_pred_uncert), which = 1, main = 'Irrigation Trial 14C Model Outputs', col = drycols, quant = TRUE, ylim = lim_y, xlim = c(trialStart - 3, trialEnd),
       legpos = NULL, lwd=2, ylab = expression(paste(Delta^{14}, "C")), xlab = 'Year',cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  lines(atm14C, lwd = 2, col = rgb(74,20,134,,,255))
  par(new = T)
  plot(summary(irr10_pred_uncert), which = 1, col = irr10cols,quant = TRUE, ylim = lim_y, xlim = c(trialStart - 3, trialEnd), main = '', legpos = NULL, lwd=2, ylab = '', xlab = '', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  par(new = T)
  plot(summary(irr20_pred_uncert), which = 1, col = irr20cols ,quant = TRUE, ylim = lim_y, xlim = c(trialStart - 3, trialEnd), main = '', legpos = NULL, lwd=2, ylab = '', xlab = '', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  arrows(datdry$time, datdry$soilc14- datdry$err,datdry$time, datdry$soilc14+ datdry$err,length=0.05, angle=90, code=3)
  arrows(dat10$time, dat10$soilc14- dat10$err,dat10$time, dat10$soilc14+ dat10$err,length=0.05, angle=90, code=3)
  arrows(dat20$time, dat20$soilc14- dat20$err,dat20$time, dat20$soilc14+ dat20$err,length=0.05, angle=90, code=3)
  points(datdry$time, datdry$soilc14, bg = drycols[2], pch = 21, cex = 2)
  points(dat10$time, dat10$soilc14, bg = irr10cols[2], pch = 22, cex = 2)
  points(dat20$time, dat20$soilc14, bg = irr20cols[2], pch = 24, cex = 2)
  legend('bottom', legend = c('Dryland', 'Irr. 10', 'Irr. 20', "IQR", '95% CI'), pch = c(21,22,24, NA,NA), pt.bg = c(drycols[2], irr10cols[2], irr20cols[2],NA,NA),
         pt.cex = 2, border = NA, fill = c(NA, NA, NA, 'grey50','grey80'), bty = 'n')


  ######## Spliced 14C plots with model fits ########
  ### Fert
  if(trialStart == 1958 & trialEnd == 1992){
    dry_pred_uncert_58 <- dry_pred_uncert
    irr10_pred_uncert_58 <- irr10_pred_uncert
    irr20_pred_uncert_58 <- irr20_pred_uncert
    unfert_pred_uncert_58 <- unfert_pred_uncert
    resfert_pred_uncert_58 <- resfert_pred_uncert
    highfert_pred_uncert_58 <- highfert_pred_uncert
  }
  if(trialStart == 1985 & trialEnd == 2010){
    dry_pred_uncert_85 <- dry_pred_uncert
    irr10_pred_uncert_85 <- irr10_pred_uncert
    irr20_pred_uncert_85 <- irr20_pred_uncert
    unfert_pred_uncert_85 <- unfert_pred_uncert
    resfert_pred_uncert_85 <- resfert_pred_uncert
    highfert_pred_uncert_85 <- highfert_pred_uncert
  }
  xlim = c(1958, 2010)
  lim_y = c(-55, 230)
  ### 1958
  plot(summary(unfert_pred_uncert_58), which = 1, main = 'Fertilizer Trial 14C Model Outputs',
       col = unfertcols, quant = TRUE, ylim = lim_y, xlim = xlim,
       legpos = NULL, lwd=2, ylab = expression(paste(Delta^{14}, "C")), xlab = 'Year',
       cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  lines(atm14C, lwd = 2, col = rgb(74,20,134,,,255))
  par(new = T)
  plot(summary(resfert_pred_uncert_58), which = 1, col = rescols,quant = TRUE,
       ylim = lim_y, xlim = xlim, main = '', axes = FALSE,
       legpos = NULL, lwd=2, ylab = '', xlab ='', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  par(new = T)
  plot(summary(highfert_pred_uncert_58), which = 1, col = highcols ,quant = TRUE, ylim = lim_y,
       xlim = xlim, main = '', legpos = NULL, lwd=2, ylab = '', xlab ='', axes = FALSE,
       cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  par(new = T)
  ### 1985
  plot(summary(unfert_pred_uncert_85), which = 1, main = 'Fertilizer Trial 14C Model Outputs',
       col = unfertcols, quant = TRUE, ylim = lim_y, xlim = xlim,axes = FALSE,
       legpos = NULL, lwd=2, ylab = expression(paste(Delta^{14}, "C")), xlab = 'Year',
       cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  lines(atm14C, lwd = 2, col = rgb(74,20,134,,,255))
  par(new = T)
  plot(summary(resfert_pred_uncert_85), which = 1, col = rescols,quant = TRUE,
       ylim = lim_y, xlim = xlim, main = '', axes = FALSE,
       legpos = NULL, lwd=2, ylab = '', xlab ='', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  par(new = T)
  plot(summary(highfert_pred_uncert_85), which = 1, col = highcols ,quant = TRUE, ylim = lim_y,
       xlim = xlim, main = '', legpos = NULL, lwd=2, ylab = '', xlab ='', axes = FALSE,
       cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  arrows(datunfert$time, datunfert$soilc14- datunfert$err,datunfert$time, datunfert$soilc14+ datunfert$err,length=0.05, angle=90, code=3)
  arrows(datresfert$time, datresfert$soilc14- datresfert$err,datresfert$time, datresfert$soilc14+ datresfert$err,length=0.05, angle=90, code=3)
  arrows(dathighfert$time, dathighfert$soilc14- dathighfert$err,dathighfert$time, dathighfert$soilc14+ dathighfert$err,length=0.05, angle=90, code=3)
  points(datunfert$time, datunfert$soilc14, bg = unfertcols[2], pch = 21, cex = 2)
  points(datresfert$time, datresfert$soilc14, bg = rescols[2], pch = 22, cex = 2)
  points(dathighfert$time, dathighfert$soilc14, bg = highcols[2], pch = 24, cex = 2)
  legend('bottom', legend = c('Unfert.', 'Res. Fert', 'High Fert', "IQR", '95% CI'), pch = c(21,22,24, NA,NA), pt.bg = c(unfertcols[2], rescols[2], highcols[2],NA,NA),
         pt.cex = 2, border = NA, fill = c(NA, NA, NA, 'grey50','grey80'), bty = 'n')

  ### Irr
  #1958
  plot(summary(dry_pred_uncert_58), which = 1, main = 'Irrigation Trial 14C Model Outputs',
       col = drycols, quant = TRUE, ylim = lim_y, xlim = xlim,
       legpos = NULL, lwd=2, ylab = expression(paste(Delta^{14}, "C")), xlab = 'Year',
       cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  lines(atm14C, lwd = 2, col = rgb(74,20,134,,,255))
  par(new = T)
  plot(summary(irr10_pred_uncert_58), which = 1, col = irr10cols,quant = TRUE, ylim = lim_y,
       xlim = xlim, main = '', legpos = NULL, lwd=2, ylab = '', xlab = '', cex.axis = 1.2, axes = FALSE,
       cex.lab = 1.4, cex.main = 1.5)
  par(new = T)
  plot(summary(irr20_pred_uncert_58), which = 1, col = irr20cols ,quant = TRUE, ylim = lim_y,
       xlim = xlim, main = '', legpos = NULL, lwd=2, ylab = '', xlab = '', cex.axis = 1.2, axes = FALSE,
       cex.lab = 1.4, cex.main = 1.5)
  #1985
  par(new = T)
  plot(summary(dry_pred_uncert_85), which = 1, main = 'Irrigation Trial 14C Model Outputs',
       col = drycols, quant = TRUE, ylim = lim_y, xlim = xlim, axes = FALSE,
       legpos = NULL, lwd=2, ylab = expression(paste(Delta^{14}, "C")), xlab = 'Year',
       cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  lines(atm14C, lwd = 2, col = rgb(74,20,134,,,255))
  par(new = T)
  plot(summary(irr10_pred_uncert_85), which = 1, col = irr10cols,quant = TRUE, ylim = lim_y,
       xlim = xlim, main = '', legpos = NULL, lwd=2, ylab = '', xlab = '', cex.axis = 1.2, axes = FALSE,
       cex.lab = 1.4, cex.main = 1.5)
  par(new = T)
  plot(summary(irr20_pred_uncert_85), which = 1, col = irr20cols ,quant = TRUE, ylim = lim_y,axes = FALSE,
       xlim = xlim, main = '', legpos = NULL, lwd=2, ylab = '', xlab = '', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  arrows(datdry$time, datdry$soilc14- datdry$err,datdry$time, datdry$soilc14+ datdry$err,length=0.05, angle=90, code=3)
  arrows(dat10$time, dat10$soilc14- dat10$err,dat10$time, dat10$soilc14+ dat10$err,length=0.05, angle=90, code=3)
  arrows(dat20$time, dat20$soilc14- dat20$err,dat20$time, dat20$soilc14+ dat20$err,length=0.05, angle=90, code=3)
  points(datdry$time, datdry$soilc14, bg = drycols[2], pch = 21, cex = 2)
  points(dat10$time, dat10$soilc14, bg = irr10cols[2], pch = 22, cex = 2)
  points(dat20$time, dat20$soilc14, bg = irr20cols[2], pch = 24, cex = 2)
  legend('bottom', legend = c('Dryland', 'Irr. 10', 'Irr. 20', "IQR", '95% CI'), pch = c(21,22,24, NA,NA), pt.bg = c(drycols[2], irr10cols[2], irr20cols[2],NA,NA),
         pt.cex = 2, border = NA, fill = c(NA, NA, NA, 'grey50','grey80'), bty = 'n')



  ### C stock: 1958
  #Fert
  par(mar = c(4.2,4.2,1.5,1))
  lim_y = c(20, 42)
  xlim = c(1958, 2010)
  plot(summary(unfert_pred_uncert_58), which = 2, main = '',
       col = unfertcols, quant = TRUE, ylim = lim_y, xlim = xlim,
       legpos = NULL, lwd=2, ylab = "Soil C Stock (Mg ha)", xlab = 'Year',
       cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5, bty ='n', frame.plot=FALSE)
  lines(c(1958, 1992), c(20,20), lwd = 3)
  lines(c(1985, 2010), c(21.5,21.5), lwd = 3)
  lines(c(1985, 1985), c(21.5,22), lwd = 3)
  lines(c(2010, 2010), c(21.5,22), lwd = 3)
  lines(c(1958, 1958), c(20.5,20), lwd = 3)
  lines(c(1992, 1992), c(20.5,20), lwd = 3)
  par(new = T)
  plot(summary(unfert_pred_uncert_58), which = 2, col = unfertcols,quant = TRUE,
       ylim = lim_y, xlim = xlim, main = '', axes = FALSE,
       legpos = NULL, lwd=2, ylab = '', xlab ='', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  par(new = T)
  plot(summary(resfert_pred_uncert_58), which = 2, col = rescols,quant = TRUE,
       ylim = lim_y, xlim = xlim, main = '', axes = FALSE,
       legpos = NULL, lwd=2, ylab = '', xlab ='', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  par(new = T)
  plot(summary(highfert_pred_uncert_58), which = 2, col = highcols,quant = TRUE, ylim = lim_y,
       xlim = xlim, main = '', legpos = NULL, lwd=2, ylab = '', xlab ='', axes = FALSE,
       cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)

  par(new = T)
  ### 1985
  #xlim = c(1985, 2010)
  plot(summary(unfert_pred_uncert_85), which = 2, #main = 'Fertilizer Trial 14C Model Outputs',
       col = unfertcols, quant = TRUE, ylim = lim_y, xlim = xlim, axes = FALSE,
       legpos = NULL, lwd=2, ylab = '', xlab = '',
       cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  #lines(atm14C, lwd = 2, col = rgb(74,20,134,,,255))
  par(new = T)
  plot(summary(resfert_pred_uncert_85), which = 2, col = rescols,quant = TRUE,
       ylim = lim_y, xlim = xlim, main = '', axes = FALSE,
       legpos = NULL, lwd=2, ylab = '', xlab ='', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  par(new = T)
  plot(summary(highfert_pred_uncert_85), which = 2, col = highcols,quant = TRUE, ylim = lim_y,
       xlim = xlim, main = '', legpos = NULL, lwd=2, ylab = '', xlab ='', axes = FALSE,
       cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  #abline(v = c(1958,1992), h = c(20,40), lty =3, lwd =2)
  #abline(v = c(1985,2010), lty =4, lwd = 2)
  points(soilCun$time, soilCun$soilC , bg = unfertcols[2], pch = 21, cex = 1.4)
  points(soilCres$time, soilCres$soilC , bg = rescols[2], pch = 22, cex = 1.4)
  points(soilChigh$time, soilChigh$soilC , bg = highcols[2], pch = 24, cex = 1.4)
  text(1975, y = 21, "Window 1")
  text(1997.5, y = 22.5, "Window 2")

  legend('topleft', legend = c('Unfert.', 'Res. Fert', 'High Fert'), pch = c(21,22,24), pt.bg = c(unfertcols[2], rescols[2], highcols[2]),
         pt.cex = 2, border = NA, fill = c(NA, NA, NA), bty = 'n')

  legend('topleft', legend = c('Unfert.', 'Res. Fert', 'High Fert', "IQR", '95% CI'), pch = c(21,22,24, NA,NA), pt.bg = c(unfertcols[2], rescols[2], highcols[2],NA,NA),
         pt.cex = 2, border = NA, fill = c(NA, NA, NA, 'grey50','grey80'), bty = 'n')


  # arrows(soilCun$time, soilCun$soilC- soilCun$err,soilCun$time, soilCun$soilC+ soilCun$err,length=0.05, angle=90, code=3)
  # arrows(datresfert$time, datresfert$soilc14- datresfert$err,datresfert$time, datresfert$soilc14+ datresfert$err,length=0.05, angle=90, code=3)
  # arrows(dathighfert$time, dathighfert$soilc14- dathighfert$err,dathighfert$time, dathighfert$soilc14+ dathighfert$err,length=0.05, angle=90, code=3)
  # points(datunfert$time, datunfert$soilc14, bg = unfertcols[2], pch = 21, cex = 2)
  # points(datresfert$time, datresfert$soilc14, bg = rescols[2], pch = 22, cex = 2)
  # points(dathighfert$time, dathighfert$soilc14, bg = highcols[2], pch = 24, cex = 2)

  ### Irr C stock
  #1958
  par(mar = c(5,4.9,1.5,1))
  lim_y = c(20, 42)
  xlim = c(1958, 2010)
  plot(summary(dry_pred_uncert_58), which = 2, main = '',
       col = drycols, quant = TRUE, ylim = lim_y, xlim = xlim,
       legpos = NULL, lwd=2, ylab = expression(paste('Soil C Stock (Mg ', ha^{-1}, ')')), xlab = 'Year',
       cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5, bty ='n', frame.plot=FALSE)
  lines(c(1958, 1992), c(20,20), lwd = 3)
  lines(c(1985, 2010), c(21.5,21.5), lwd = 3)
  lines(c(1985, 1985), c(21.5,22), lwd = 3)
  lines(c(2010, 2010), c(21.5,22), lwd = 3)
  lines(c(1958, 1958), c(20.5,20), lwd = 3)
  lines(c(1992, 1992), c(20.5,20), lwd = 3)
  par(new = T)
  plot(summary(dry_pred_uncert_58), which = 2, col = drycols,quant = TRUE,
       ylim = lim_y, xlim = xlim, main = '', axes = FALSE,
       legpos = NULL, lwd=2, ylab = '', xlab ='', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  par(new = T)
  plot(summary(irr10_pred_uncert_58), which = 2, col = irr10cols,quant = TRUE,
       ylim = lim_y, xlim = xlim, main = '', axes = FALSE,
       legpos = NULL, lwd=2, ylab = '', xlab ='', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  par(new = T)
  plot(summary(irr20_pred_uncert_58), which = 2, col = irr20cols,quant = TRUE, ylim = lim_y,
       xlim = xlim, main = '', legpos = NULL, lwd=2, ylab = '', xlab ='', axes = FALSE,
       cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)

  par(new = T)
  ### 1985
  #xlim = c(1985, 2010)
  plot(summary(dry_pred_uncert_85), which = 2, #main = 'Fertilizer Trial 14C Model Outputs',
       col = drycols, quant = TRUE, ylim = lim_y, xlim = xlim, axes = FALSE,
       legpos = NULL, lwd=2, ylab = '', xlab = '',
       cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  #lines(atm14C, lwd = 2, col = rgb(74,20,134,,,255))
  par(new = T)
  plot(summary(irr10_pred_uncert_85), which = 2, col = irr10cols,quant = TRUE,
       ylim = lim_y, xlim = xlim, main = '', axes = FALSE,
       legpos = NULL, lwd=2, ylab = '', xlab ='', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  par(new = T)
  plot(summary(irr20_pred_uncert_85), which = 2, col = irr20cols,quant = TRUE, ylim = lim_y,
       xlim = xlim, main = '', legpos = NULL, lwd=2, ylab = '', xlab ='', axes = FALSE,
       cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  #abline(v = c(1958,1992), h = c(20,40), lty =3, lwd =2)
  #abline(v = c(1985,2010), lty =4, lwd = 2)
  points(soilCdry$time, soilCdry$soilC , bg = drycols[2], pch = 21, cex = 1.4)
  points(soilC10$time, soilC10$soilC , bg = irr10cols[2], pch = 22, cex = 1.4)
  points(soilC20$time, soilC20$soilC , bg = irr20cols[2], pch = 24, cex = 1.4)
  text(1975, y = 21, "Window 1")
  text(1997.5, y = 22.5, "Window 2")

  legend('topleft', legend = c('Dry', 'Irr. 10', 'Irr. 20'), pch = c(21,22,24), pt.bg = c(drycols[2], irr10cols[2], irr20cols[2]),
         pt.cex = 2, border = NA, fill = c(NA, NA, NA), bty = 'n')

  legend('topleft', legend = c('Unfert.', 'Res. Fert', 'High Fert', "IQR", '95% CI'), pch = c(21,22,24, NA,NA), pt.bg = c(drycols[2], irr10cols[2], irr20cols[2],NA,NA),
         pt.cex = 2, border = NA, fill = c(NA, NA, NA, 'grey50','grey80'), bty = 'n')


  ### Irr 14C
  #1958
  par(mar = c(4.2,4.5,1.5,1))
  lim_y = c(-55, 225)
  xlim = c(1955, 2010)
  plot(summary(dry_pred_uncert_58), which = 1, main = '',
       col = drycols, quant = TRUE, ylim = lim_y, xlim = xlim,
       legpos = NULL, lwd=2, ylab = expression(paste(Delta^{14}, "C")), xlab = 'Year',
       cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5, bty ='n', frame.plot=FALSE)
  lines(atm14C, lwd = 3, col = rgb(74,20,134,,,255))
  lines(c(1958, 1992), c(-55,-55), lwd = 3)
  lines(c(1985, 2010), c(-37.5,-37.5), lwd = 3)
  lines(c(1985, 1985), c(-37, -32), lwd = 3)
  lines(c(2010, 2010), c(-37,-32), lwd = 3)
  lines(c(1958, 1958), c(-55,-50), lwd = 3)
  lines(c(1992, 1992), c(-55,-50), lwd = 3)
  arrows(datdry$time, datdry$soilc14- datdry$err,datdry$time, datdry$soilc14+ datdry$err,length=0.05, angle=90, code=3)
  arrows(dat10$time, dat10$soilc14- dat10$err,dat10$time, dat10$soilc14+ dat10$err,length=0.05, angle=90, code=3)
  arrows(dat20$time, dat20$soilc14- dat20$err,dat20$time, dat20$soilc14+ dat20$err,length=0.05, angle=90, code=3)
  par(new = T)
  plot(summary(dry_pred_uncert_58), which = 1, col = drycols,quant = TRUE,
       ylim = lim_y, xlim = xlim, main = '', axes = FALSE,
       legpos = NULL, lwd=2, ylab = '', xlab ='', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  par(new = T)
  plot(summary(irr10_pred_uncert_58), which = 1, col = irr10cols,quant = TRUE,
       ylim = lim_y, xlim = xlim, main = '', axes = FALSE,
       legpos = NULL, lwd=2, ylab = '', xlab ='', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  par(new = T)
  plot(summary(irr20_pred_uncert_58), which = 1, col = irr20cols,quant = TRUE, ylim = lim_y,
       xlim = xlim, main = '', legpos = NULL, lwd=2, ylab = '', xlab ='', axes = FALSE,
       cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)

  par(new = T)
  ### 1985
  #xlim = c(1985, 2010)
  plot(summary(dry_pred_uncert_85), which = 1, #main = 'Fertilizer Trial 14C Model Outputs',
       col = drycols, quant = TRUE, ylim = lim_y, xlim = xlim, axes = FALSE,
       legpos = NULL, lwd=2, ylab = '', xlab = '',
       cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  #lines(atm14C, lwd = 2, col = rgb(74,20,134,,,255))
  par(new = T)
  plot(summary(irr10_pred_uncert_85), which = 1, col = irr10cols,quant = TRUE,
       ylim = lim_y, xlim = xlim, main = '', axes = FALSE,
       legpos = NULL, lwd=2, ylab = '', xlab ='', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  par(new = T)
  plot(summary(irr20_pred_uncert_85), which = 1, col = irr20cols,quant = TRUE, ylim = lim_y,
       xlim = xlim, main = '', legpos = NULL, lwd=2, ylab = '', xlab ='', axes = FALSE,
       cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  #abline(v = c(1958,1992), h = c(20,40), lty =3, lwd =2)
  #abline(v = c(1985,2010), lty =4, lwd = 2)
  points(datdry$time, datdry$soilc14 , bg = drycols[2], pch = 21, cex = 1.7)
  points(dat10$time, dat10$soilc14 , bg = irr10cols[2], pch = 22, cex = 1.7)
  points(dat20$time, dat20$soilc14 , bg = irr20cols[2], pch = 24, cex = 1.7)
  text(1975, y = -43, "Window 1")
  text(1997.5, y = -25.5, "Window 2")

  legend('topright', legend = c('Atm.','Dry', 'Irr. 10', 'Irr. 20'), pch = c(NA,21,22,24), pt.bg = c(NA,drycols[2], irr10cols[2], irr20cols[2]),
         pt.cex = 2, lty = c(1,NA,NA,NA), col = c(col = rgb(74,20,134,,,255), 1,1,1), lwd = c(3,NA,NA,NA), border = NA,  bty = 'n')


  ### Fert 14C
  #1958
  par(mar = c(4.2,4.5,1.5,1))
  lim_y = c(-55, 225)
  xlim = c(1955, 2010)
  plot(summary(unfert_pred_uncert_58), which = 1, main = '',
       col = unfertcols, quant = TRUE, ylim = lim_y, xlim = xlim,
       legpos = NULL, lwd=2, ylab = expression(paste(Delta^{14}, "C")), xlab = 'Year',
       cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5, bty ='n', frame.plot=FALSE)
  lines(atm14C, lwd = 3, col = rgb(74,20,134,,,255))
  lines(c(1958, 1992), c(-55,-55), lwd = 3)
  lines(c(1985, 2010), c(-37.5,-37.5), lwd = 3)
  lines(c(1985, 1985), c(-37, -32), lwd = 3)
  lines(c(2010, 2010), c(-37,-32), lwd = 3)
  lines(c(1958, 1958), c(-55,-50), lwd = 3)
  lines(c(1992, 1992), c(-55,-50), lwd = 3)
  arrows(datunfert$time, datunfert$soilc14- datunfert$err,datunfert$time, datunfert$soilc14+ datunfert$err,length=0.05, angle=90, code=3)
  arrows(datresfert$time, datresfert$soilc14- datresfert$err,datresfert$time, datresfert$soilc14+ datresfert$err,length=0.05, angle=90, code=3)
  arrows(dathighfert$time, dathighfert$soilc14- dathighfert$err,dathighfert$time, dathighfert$soilc14+ dathighfert$err,length=0.05, angle=90, code=3)
  par(new = T)
  plot(summary(unfert_pred_uncert_58), which = 1, col = unfertcols,quant = TRUE,
       ylim = lim_y, xlim = xlim, main = '', axes = FALSE,
       legpos = NULL, lwd=2, ylab = '', xlab ='', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  par(new = T)
  plot(summary(resfert_pred_uncert_58), which = 1, col = rescols,quant = TRUE,
       ylim = lim_y, xlim = xlim, main = '', axes = FALSE,
       legpos = NULL, lwd=2, ylab = '', xlab ='', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  par(new = T)
  plot(summary(highfert_pred_uncert_58), which = 1, col = highcols,quant = TRUE, ylim = lim_y,
       xlim = xlim, main = '', legpos = NULL, lwd=2, ylab = '', xlab ='', axes = FALSE,
       cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)

  par(new = T)
  ### 1985
  #xlim = c(1985, 2010)
  plot(summary(unfert_pred_uncert_85), which = 1, #main = 'Fertilizer Trial 14C Model Outputs',
       col = unfertcols, quant = TRUE, ylim = lim_y, xlim = xlim, axes = FALSE,
       legpos = NULL, lwd=2, ylab = '', xlab = '',
       cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  #lines(atm14C, lwd = 2, col = rgb(74,20,134,,,255))
  par(new = T)
  plot(summary(resfert_pred_uncert_85), which = 1, col = rescols,quant = TRUE,
       ylim = lim_y, xlim = xlim, main = '', axes = FALSE,
       legpos = NULL, lwd=2, ylab = '', xlab ='', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  par(new = T)
  plot(summary(highfert_pred_uncert_85), which = 1, col = highcols,quant = TRUE, ylim = lim_y,
       xlim = xlim, main = '', legpos = NULL, lwd=2, ylab = '', xlab ='', axes = FALSE,
       cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  #abline(v = c(1958,1992), h = c(20,40), lty =3, lwd =2)
  #abline(v = c(1985,2010), lty =4, lwd = 2)
  points(datunfert$time, datunfert$soilc14 , bg = unfertcols[2], pch = 21, cex = 1.7)
  points(datresfert$time, datresfert$soilc14 , bg = rescols[2], pch = 22, cex = 1.7)
  points(dathighfert$time, dathighfert$soilc14 , bg = highcols[2], pch = 24, cex = 1.7)
  text(1975, y = -43, "Window 1")
  text(1997.5, y = -25.5, "Window 2")

  legend('topright', legend = c('Atm.','Unfert', 'Res. Fert', 'High Fert'), pch = c(NA,21,22,24), pt.bg = c(NA,unfertcols[2], rescols[2], highcols[2]),
         pt.cex = 2, lty = c(1,NA,NA,NA), col = c(col = rgb(74,20,134,,,255), 1,1,1), lwd = c(3,NA,NA,NA), border = NA,  bty = 'n')

  #### End splice ####

  lim_y <- c(min(fertLims_C - 2), max(fertLims_C + 1))

  plot(summary(dry_pred_uncert), which = 2, main = 'Fertilizer Trial C Stock Model Outputs', col = drycols, quant = TRUE,  xlim = c(trialStart - 3, trialEnd), ylim = lim_y,
       legpos = NULL, lwd=2, ylab = 'Tonnes C per Hectare', xlab = 'Year', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  par(new = T)
  plot(summary(irr10_pred_uncert), which = 2, col = irr10cols,quant = TRUE, ylim = lim_y,  xlim = c(trialStart - 3, trialEnd), main = '', legpos = NULL, lwd=2, ylab = '', xlab = '', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  par(new = T)
  plot(summary(irr20_pred_uncert), which = 2, col = irr20cols ,quant = TRUE, xlim = c(trialStart - 3, trialEnd), ylim = lim_y, main = '', legpos = NULL, lwd=2, ylab = '', xlab = '', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  points(soilCun$time, soilCun$soilC, bg = drycols[2], pch = 21, cex = 2)
  points(soilCres$time, soilCres$soilC, bg = irr10cols[2], pch = 22, cex = 2)
  points(soilChigh$time, soilChigh$soilC, bg = irr20cols[2], pch = 24, cex = 2)
  # legend('topleft', legend = c('Unfert.', 'Res. Fert', 'High Fert', "IQR", '95% CI'), pch = c(21,22,24, NA,NA), pt.bg = c(drycols[2], irr10cols[2], irr20cols[2],NA,NA),
  #        pt.cex = 2, border = NA, fill = c(NA, NA, NA, 'grey50','grey80'))

  lim_y <- c(min(irrLims_C- 2), max(irrLims_C))

  #par(mgp=c(axis.title.position, axis.label.position, axis.line.position))

  plot(summary(dry_pred_uncert), which = 2, main = 'Irrigation Trial C Stock Model Outputs', col = drycols, quant = TRUE, ylim = lim_y, xlim = c(trialStart - 3, trialEnd),
       legpos = NULL, lwd=2, ylab = 'Tonnes C per Hectare', xlab = 'Year', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  par(new = T)
  plot(summary(irr10_pred_uncert), which = 2, col = irr10cols,quant = TRUE, ylim = lim_y, xlim = c(trialStart - 3, trialEnd), main = '', legpos = NULL, lwd=2, ylab = '', xlab = '', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  par(new = T)
  plot(summary(irr20_pred_uncert), which = 2, col = irr20cols ,quant = TRUE, ylim = lim_y, xlim = c(trialStart - 3, trialEnd), main = '', legpos = NULL, lwd=2, ylab = '', xlab = '', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  points(soilCdry$time, soilCdry$soilC, bg = drycols[2], pch = 21, cex = 2)
  points(soilC10$time, soilC10$soilC, bg = irr10cols[2], pch = 22, cex = 2)
  points(soilC20$time, soilC20$soilC, bg = irr20cols[2], pch = 24, cex = 2)
  # legend('topleft', legend = c('Dryland', 'Irr. 10', 'Irr. 20', "IQR", '95% CI'), pch = c(21,22,24, NA,NA), pt.bg = c(drycols[2], irr10cols[2], irr20cols[2],NA,NA),
  #        pt.cex = 2, border = NA, fill = c(NA, NA, NA, 'grey50','grey80'))

  lim_y = c(min(irrLims_C14, fertLims_C14), max(irrLims_C14, fertLims_C14))

  plot(summary(unfert_pred_uncert), which = 1, main = 'All Trial 14C Model Outputs', col = unfertcols, quant = TRUE, ylim = lim_y, xlim = c(trialStart - 3, trialEnd),
       legpos = NULL, lwd=2, ylab = expression(paste(Delta^{14}, "C")), xlab = 'Year', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  lines(atm14C, lwd = 2, col = rgb(74,20,134,,,255))
  par(new = T)
  plot(summary(resfert_pred_uncert), which = 1, col = rescols,quant = TRUE, ylim = lim_y, xlim = c(trialStart - 3, trialEnd), main = '', legpos = NULL, lwd=2, ylab = '', xlab ='', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  par(new = T)
  plot(summary(highfert_pred_uncert), which = 1, col = highcols ,quant = TRUE, ylim = lim_y, xlim = c(trialStart - 3, trialEnd), main = '', legpos = NULL, lwd=2, ylab = '', xlab ='', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  par(new = T)
  plot(summary(dry_pred_uncert), which = 1, col = drycols, quant = TRUE, ylim = lim_y, xlim = c(trialStart - 3, trialEnd),
       legpos = NULL, lwd=2, ylab = expression(paste(Delta^{14}, "C")), xlab = 'Year', main = '', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  par(new = T)
  plot(summary(irr10_pred_uncert), which = 1, col = irr10cols,quant = TRUE, ylim = lim_y, xlim = c(trialStart - 3, trialEnd), main = '', legpos = NULL, lwd=2, ylab = '', xlab = '', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  par(new = T)
  plot(summary(irr20_pred_uncert), which = 1, col = irr20cols ,quant = TRUE, ylim = lim_y, xlim = c(trialStart - 3, trialEnd), main = '', legpos = NULL, lwd=2, ylab = '', xlab = '', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  # legend('topleft', legend = c('Dryland', 'Irr. 10', 'Irr. 20','Unfert.', 'Res. Fert', 'High Fert', "IQR", '95% CI'),
  #        fill = c(drycols[2], irr10cols[2], irr20cols[2],unfertcols[2],rescols[2],highcols[2],'grey50','grey80'),
  #        pt.cex = 2, border = NA)

  plot(dat10$time, dat10$soilc14, type = 'l', lwd =3, col = irr10cols[2], xlim = c(1959, 2010), ylim = c(-50, 230),
       ylab = expression(paste(Delta^{14}, "C")), xlab = 'Year',main = expression(paste('Irrigation Trial ',Delta^{14}, "C")), cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  lines(atm14C, lwd = 2, col = rgb(74,20,134,,,255))
  lines(dat20$time, dat20$soilc14, lwd = 3, col = irr20cols[2])
  lines(datdry$time, datdry$soilc14, lwd = 3, col = drycols[2])
  points(datdry$time, datdry$soilc14, bg = drycols[2], pch = 21, cex = 2)
  points(dat10$time, dat10$soilc14, bg = irr10cols[2], pch = 22, cex = 2)
  points(dat20$time, dat20$soilc14, bg = irr20cols[2], pch = 24, cex = 2)
  legend('bottomright', legend = c('Atmos.','Dryland', 'Irr. 10', 'Irr. 20'),
         pch = c(NA, 21,22,24), pt.bg = c(drycols[2], irr10cols[2], irr20cols[2]),
         pt.cex = 2, border = NA, lty = c(1, NA, NA, NA), lwd = c(3, NA, NA, NA),
         col = c(rgb(74,20,134,,,255), 1,1,1))

  plot(dathighfert$time, dathighfert$soilc14, type = 'l', lwd =3, col = highcols[2], xlim = c(1959, 2010), ylim = c(-50, 230),
       ylab = expression(paste(Delta^{14}, "C")), xlab = 'Year',main = expression(paste('Fertilizer Trial ',Delta^{14}, "C")), cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  lines(atm14C, lwd = 3, col = rgb(74,20,134,,,255))
  lines(datresfert$time, datresfert$soilc14, lwd = 3, col = rescols[2])
  lines(datunfert$time, datunfert$soilc14, lwd = 3, col = unfertcols[2])
  points(dathighfert$time, dathighfert$soilc14, bg = highcols[2], pch = 24, cex = 2)
  points(datresfert$time, datresfert$soilc14, bg = rescols[2], pch = 22, cex = 2)
  points(datunfert$time, datunfert$soilc14, bg = unfertcols[2], pch = 21, cex = 2)
  legend('bottomright', legend = c('Atmos.','Unfert', 'Res. Fert', 'High Fert'), pch = c(NA, 21,22,24), pt.bg = c(unfertcols[2], rescols[2], highcols[2]),
         pt.cex = 2, border = NA, lty = c(1, NA, NA, NA), lwd = c(3, NA, NA, NA), col = c(rgb(74,20,134,,,255), 1,1,1))


  plot(dat10$time, dat10$soilc14, type = 'l', lwd =3, col = irr10cols[2], xlim = c(1959, 2010), ylim = c(-50, 230), cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5,
       ylab = expression(paste(Delta^{14}, "C")), xlab = 'Year',main = expression(paste('All Trial ',Delta^{14}, "C")))
  lines(atm14C, lwd = 3, col = rgb(74,20,134,,,255))
  lines(datresfert$time, datresfert$soilc14, lwd = 3, col = rescols[2])
  lines(datunfert$time, datunfert$soilc14, lwd = 3, col = unfertcols[2])
  lines(dat20$time, dat20$soilc14, lwd = 3, col = irr20cols[2])
  lines(datdry$time, datdry$soilc14, lwd = 3, col = drycols[2])
  lines(dathighfert$time, dathighfert$soilc14, lwd = 3, col = highcols[2])
  points(datdry$time, datdry$soilc14, bg = drycols[2], pch = 21, cex = 2)
  points(dat10$time, dat10$soilc14, bg = irr10cols[2], pch = 22, cex = 2)
  points(dat20$time, dat20$soilc14, bg = irr20cols[2], pch = 24, cex = 2)
  points(datunfert$time, datunfert$soilc14, bg = unfertcols[2], pch = 21, cex = 2)
  points(datresfert$time, datresfert$soilc14, bg = rescols[2], pch = 22, cex = 2)
  points(dathighfert$time, dathighfert$soilc14, bg = highcols[2], pch = 24, cex = 2)
  legend('bottom', legend = c('Atmos.','Unfert', 'Res. Fert', 'High Fert', 'Dryland', 'Irr. 10', "Irr. 20"), pch = c(NA, 21,22,24, 21, 22, 24), ,
         pt.cex = 2, border = NA, lty = c(1, NA, NA, NA), lwd = c(3, NA, NA, NA, NA, NA, NA), col = c(rgb(74,20,134,,,255), 1,1,1,1,1,1), ncol = 2,
         pt.bg = c(NA, unfertcols[2], rescols[2], highcols[2], drycols[2], irr10cols[2], irr20cols[2]), box.lwd = 0)




  plot(summary(unfert_pred_uncert), which = 1, main = 'Fertilizer Trial 14C Model Outputs', col = unfertcols, quant = TRUE, ylim = c(-50, 230), xlim = c(1958, 2010),
       legpos = NULL, lwd=2, ylab = expression(paste(Delta^{14}, "C")), xlab = 'Year', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  lines(atm14C, lwd = 2, col = rgb(74,20,134,,,255))
  par(new = T)
  plot(summary(resfert_pred_uncert), which = 1, col = rescols,quant = TRUE, ylim = c(-50, 230), xlim = c(1958, 2010), main = '', legpos = NULL, lwd=2, ylab = '', xlab ='')
  par(new = T)
  plot(summary(highfert_pred_uncert), which = 1, col = highcols ,quant = TRUE, ylim = c(-50, 230), xlim = c(1958, 2010), main = '', legpos = NULL, lwd=2, ylab = '', xlab ='')
  points(datunfert$time, datunfert$soilc14, bg = unfertcols[2], pch = 21, cex = 2)
  points(datresfert$time, datresfert$soilc14, bg = rescols[2], pch = 22, cex = 2)
  points(dathighfert$time, dathighfert$soilc14, bg = highcols[2], pch = 24, cex = 2)
  legend('topright', legend = c('Unfert.', 'Res. Fert', 'High Fert', "IQR", '95% CI'), pch = c(21,22,24, NA,NA), pt.bg = c(unfertcols[2], rescols[2], highcols[2],NA,NA),
         pt.cex = 2, border = NA, fill = c(NA, NA, NA, 'grey50','grey80'))

  plot(summary(dry_pred_uncert), which = 1, main = 'Irrigation Trial 14C Model Outputs', col = drycols, quant = TRUE, ylim = c(-50, 230), xlim = c(1958, 2010),
       legpos = NULL, lwd=2, ylab = expression(paste(Delta^{14}, "C")), xlab = 'Year', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  lines(atm14C, lwd = 2, col = rgb(74,20,134,,,255))
  par(new = T)
  plot(summary(irr10_pred_uncert), which = 1, col = irr10cols,quant = TRUE, ylim = c(-50, 230), xlim = c(1958, 2010), main = '', legpos = NULL, lwd=2, ylab = '', xlab = '')
  par(new = T)
  plot(summary(irr20_pred_uncert), which = 1, col = irr20cols ,quant = TRUE, ylim = c(-50, 230), xlim = c(1958, 2010), main = '', legpos = NULL, lwd=2, ylab = '', xlab = '')
  points(datdry$time, datdry$soilc14, bg = drycols[2], pch = 21, cex = 2)
  points(dat10$time, dat10$soilc14, bg = irr10cols[2], pch = 22, cex = 2)
  points(dat20$time, dat20$soilc14, bg = irr20cols[2], pch = 24, cex = 2)
  legend('topright', legend = c('Dryland', 'Irr. 10', 'Irr. 20', "IQR", '95% CI'), pch = c(21,22,24, NA,NA), pt.bg = c(drycols[2], irr10cols[2], irr20cols[2],NA,NA),
         pt.cex = 2, border = NA, fill = c(NA, NA, NA, 'grey50','grey80'))

  plot(summary(unfert_pred_uncert), which = 2, main = 'Fertilizer Trial C Stock Model Outputs', col = unfertcols, quant = TRUE,  xlim = c(1951, 2010), ylim = c(20, 42),
       legpos = NULL, lwd=2, ylab = 'Tonnes C per Hectare', xlab = 'Year', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  par(new = T)
  plot(summary(resfert_pred_uncert), which = 2, col = rescols,quant = TRUE,ylim = c(20, 42),  xlim = c(1951, 2010), main = '', legpos = NULL, lwd=2, ylab = '', xlab = '')
  par(new = T)
  plot(summary(highfert_pred_uncert), which = 2, col = highcols ,quant = TRUE, xlim = c(1951, 2010), ylim = c(20, 42), main = '', legpos = NULL, lwd=2, ylab = '', xlab = '')
  points(soilCun$time, soilCun$soilC, bg = unfertcols[2], pch = 21, cex = 2)
  points(soilCres$time, soilCres$soilC, bg = rescols[2], pch = 22, cex = 2)
  points(soilChigh$time, soilChigh$soilC, bg = highcols[2], pch = 24, cex = 2)
  legend('topleft', legend = c('Unfert.', 'Res. Fert', 'High Fert', "IQR", '95% CI'), pch = c(21,22,24, NA,NA), pt.bg = c(unfertcols[2], rescols[2], highcols[2],NA,NA),
         pt.cex = 2, border = NA, fill = c(NA, NA, NA, 'grey50','grey80'))

  plot(summary(dry_pred_uncert), which = 2, main = 'Irrigation Trial C Stock Model Outputs', col = drycols, quant = TRUE, ylim = c(23, 42), xlim = c(1958, 2010),
       legpos = NULL, lwd=2, ylab = 'Tonnes C per Hectare', xlab = 'Year', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  par(new = T)
  plot(summary(irr10_pred_uncert), which = 2, col = irr10cols,quant = TRUE, ylim = c(23, 42), xlim = c(1958, 2010), main = '', legpos = NULL, lwd=2, ylab = '', xlab = '')
  par(new = T)
  plot(summary(irr20_pred_uncert), which = 2, col = irr20cols ,quant = TRUE, ylim = c(23, 42), xlim = c(1958, 2010), main = '', legpos = NULL, lwd=2, ylab = '', xlab = '')
  points(soilCdry$time, soilCdry$soilC, bg = drycols[2], pch = 21, cex = 2)
  points(soilC10$time, soilC10$soilC, bg = irr10cols[2], pch = 22, cex = 2)
  points(soilC20$time, soilC20$soilC, bg = irr20cols[2], pch = 24, cex = 2)
  legend('topleft', legend = c('Dryland', 'Irr. 10', 'Irr. 20', "IQR", '95% CI'), pch = c(21,22,24, NA,NA), pt.bg = c(drycols[2], irr10cols[2], irr20cols[2],NA,NA),
         pt.cex = 2, border = NA, fill = c(NA, NA, NA, 'grey50','grey80'))

  plot(summary(unfert_pred_uncert), which = 1, main = 'All Trial 14C Model Outputs', col = unfertcols, quant = TRUE, ylim = c(-50, 230), xlim = c(1958, 2010),
       legpos = NULL, lwd=2, ylab = expression(paste(Delta^{14}, "C")), xlab = 'Year', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  lines(atm14C, lwd = 2, col = rgb(74,20,134,,,255))
  par(new = T)
  plot(summary(resfert_pred_uncert), which = 1, col = rescols,quant = TRUE, ylim = c(-50, 230), xlim = c(1958, 2010), main = '', legpos = NULL, lwd=2, ylab = '', xlab ='', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  par(new = T)
  plot(summary(highfert_pred_uncert), which = 1, col = highcols ,quant = TRUE, ylim = c(-50, 230), xlim = c(1958, 2010), main = '', legpos = NULL, lwd=2, ylab = '', xlab ='', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  par(new = T)
  plot(summary(dry_pred_uncert), which = 1, col = drycols, quant = TRUE, ylim = c(-50, 230), xlim = c(1958, 2010),
       legpos = NULL, lwd=2, ylab = expression(paste(Delta^{14}, "C")), xlab = 'Year', main = '', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  par(new = T)
  plot(summary(irr10_pred_uncert), which = 1, col = irr10cols,quant = TRUE, ylim = c(-50, 230), xlim = c(1958, 2010), main = '', legpos = NULL, lwd=2, ylab = '', xlab = '', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  par(new = T)
  plot(summary(irr20_pred_uncert), which = 1, col = irr20cols ,quant = TRUE, ylim = c(-50, 230), xlim = c(1958, 2010), main = '', legpos = NULL, lwd=2, ylab = '', xlab = '', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  legend('bottomright', legend = c('Dryland', 'Irr. 10', 'Irr. 20','Unfert.', 'Res. Fert', 'High Fert', "IQR", '95% CI'),
         fill = c(drycols[2], irr10cols[2], irr20cols[2],unfertcols[2],rescols[2],highcols[2],'grey50','grey80'),
         pt.cex = 2, border = NA)

  soilCun$Trial = 'Unfert'
  soilCres$Trial = 'Res. Fert'
  soilChigh$Trial = 'High Fert'
  fertC <- rbind(soilCun, soilCres, soilChigh)
  fertC$Trial <- factor(fertC$Trial, levels = c('Unfert', 'Res. Fert','High Fert'))

  print(ggplot(fertC, aes(x = time, y = soilC, color = Trial, fill = Trial, shape = Trial)) +
          xlab('Year') + geom_smooth(method = glm) + scale_shape_manual(values = c(21, 22, 24)) +
          scale_fill_manual(values = c(unfertcols[2], rescols[2], highcols[2])) +
          scale_color_manual(values = c(unfertcols[2], rescols[2], highcols[2]))  + theme_bw() +
          geom_point(color = 1, size = 4) +
          ylim(min(as.numeric(na.omit(fertC$soilC)), as.numeric(na.omit(fertC$soilC))), max(as.numeric(na.omit(fertC$soilC)), as.numeric(na.omit(irrC$soilC)))+2) +
          ylab(expression(bold(paste('Soil C Stock (T ha'^{-1},')')))) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 18, face = 'bold'),
                axis.title=element_text(size=14,face="bold"),
                strip.text.y = element_text(size = 12)))

  print(ggplot(irrC, aes(x = time, y = soilC, color = Trial, fill = Trial, shape = Trial)) +
          xlab('Year') + geom_smooth(method = glm) + scale_shape_manual(values = c(21, 22, 24)) +
          scale_fill_manual(values = c(drycols[2], irr10cols[2], irr20cols[2])) + theme_bw() +
          scale_color_manual(values = c(drycols[2], irr10cols[2], irr20cols[2])) +
          geom_point(size = 4, color = 1) + labs(fill = 'Trial') +
          ylim(min(as.numeric(na.omit(fertC$soilC)), as.numeric(na.omit(irrC$soilC))), max(as.numeric(na.omit(fertC$soilC)), as.numeric(na.omit(irrC$soilC)))+2) +
          ylab(expression(bold(paste('Soil C Stock (T ha'^{-1},')')))) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 18, face = 'bold'),
                axis.title=element_text(size=14,face="bold"),
                strip.text.y = element_text(size = 12)))

  datdry$trial = 'Dry'
  dat10$trial = 'Irr. 10'
  dat20$trial = 'Irr. 20'
  irr14C <- na.omit(rbind(datdry, dat10, dat20))

  datunfert$trial = 'Unfert'
  datresfert$trial = 'Res. Fert'
  dathighfert$trial = 'High Fert'
  fert14C <- na.omit(rbind(datunfert, datresfert, dathighfert))
  fert14C$trial = factor(fert14C$trial, levels = c("Unfert", 'Res. Fert', 'High Fert'), ordered = TRUE)

  print(ggplot(irr14C, aes(x = time, y = soilc14, color = trial, fill = trial, shape = trial)) +
          xlab('Year')+ scale_shape_manual(values = c(21, 22, 24)) + geom_line(size = 1.5) +
          scale_fill_manual(values = c(drycols[2], irr10cols[2], irr20cols[2])) + theme_bw() +
          scale_color_manual(values = c(drycols[2], irr10cols[2], irr20cols[2])) +
          geom_errorbar(aes(ymin = soilc14 - err, ymax = soilc14 + err), width=2, position = position_dodge(0.05)) +
          geom_point(size = 4, color = 1) + labs(fill = 'Trial') +
          ylim(min(as.numeric(na.omit(fert14C$soilc14)), as.numeric(na.omit(irr14C$soilc14))), max(as.numeric(na.omit(fert14C$soilc14)), as.numeric(na.omit(irr14C$soilc14)))+2) +
          ylab(expression(bold(paste('Soil C Stock (T ha'^{-1},')')))) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 18, face = 'bold'),
                axis.title=element_text(size=14,face="bold"),
                strip.text.y = element_text(size = 12)))

  print(ggplot(fert14C, aes(x = time, y = soilc14, color = trial, fill = trial, shape = trial)) +
          xlab('Year') + scale_shape_manual(values = c(21, 22, 24)) + geom_line(size = 1.5) +
          scale_fill_manual(values = c(unfertcols[2], rescols[2], highcols[2])) +
          scale_color_manual(values = c(unfertcols[2], rescols[2], highcols[2]))  + theme_bw() +
          geom_errorbar(aes(ymin = soilc14 - err, ymax = soilc14 + err), width=2, position = position_dodge(0.05)) +
          geom_point(color = 1, size = 4) +
          ylim(min(as.numeric(na.omit(fert14C$soilc14)), as.numeric(na.omit(fert14C$soilc14))), max(as.numeric(na.omit(fert14C$soilc14)), as.numeric(na.omit(irr14C$soilc14)))+2) +
          ylab(expression(bold(paste('Soil C Stock (T ha'^{-1},')')))) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 18, face = 'bold'),
                axis.title=element_text(size=14,face="bold"),
                strip.text.y = element_text(size = 12)))

  winch13 <- read.csv('/Users/shane/14Constraint Dropbox/Shane Stoner/IMPRS/NZ/Winch13C.csv')

  winch13 <- melt(winch13, id.vars = 'year')
  colnames(winch13) <- c('year', 'trial', 'd13C')

  irr13 <- filter(winch13, trial == 'dry' | trial == 'irr10' | trial == 'irr20')
  irr13err <- filter(winch13, trial == 'dry_err' | trial == 'irr10_err' | trial == 'irr20_err')

  print(ggplot(irr13, aes(x = year, y = d13C, color = trial, fill = trial, shape = trial)) + theme_bw() +
          xlab('Year') + geom_smooth(method = 'lm', se = FALSE) + scale_shape_manual(values = c(21, 22, 24)) +
          scale_fill_manual(values = c(drycols[2], irr10cols[2], irr20cols[2])) +
          scale_color_manual(values = c(drycols[2], irr10cols[2], irr20cols[2])) +
          geom_errorbar(aes(ymin=irr13$d13C- irr13err$d13C, ymax=irr13$d13C + irr13err$d13C), width=2,
                        position=position_dodge(0.05)) +
          ylim(-28.55, -26.5) +
          geom_point(size = 5, color = 1) + labs(fill = 'Trial') +
          ylab(expression(bold(paste('Soil ',delta^{-13},'C')))) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 18, face = 'bold'),
                axis.title=element_text(size=14,face="bold"),
                strip.text.y = element_text(size = 12)))

  fert13 <- filter(winch13, trial == 'control' | trial == 'res' | trial == 'high')
  fert13err <- filter(winch13, trial == 'con_err' | trial == 'res_err' | trial == 'high_err')

  print(ggplot(fert13, aes(x = year, y = d13C, color = trial, fill = trial, shape = trial)) + theme_bw()+
          xlab('Year') + geom_smooth(method = 'glm', se = FALSE) + scale_shape_manual(values = c(21, 22, 24)) +
          scale_fill_manual(values = c(unfertcols[2], rescols[2], highcols[2])) +
          scale_color_manual(values = c(unfertcols[2], rescols[2], highcols[2])) +
          geom_errorbar(aes(ymin=fert13$d13C- fert13err$d13C, ymax=fert13$d13C + fert13err$d13C), width=2,
                        position=position_dodge(0.05)) +
          ylim(-28.55, -26.5) +
          geom_point(size = 5, color = 1) + labs(fill = 'Trial') +
          ylab(expression(bold(paste('Soil ',delta^{-13},'C')))) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 18, face = 'bold'),
                axis.title=element_text(size=14,face="bold"),
                strip.text.y = element_text(size = 12)))


  # Combining window outputs and data into one plot - may get sloppy
  ### Fert 14C
  lim_y = c(-50, 230)


  # plot(summary(unfert_pred_uncert_p), which = 1, main = 'Fertilizer Trial 14C Model Outputs',
  #      col = unfertcols, quant = TRUE, ylim = lim_y, xlim = c(1958, 2010),
  #      legpos = NULL, lwd=2, ylab = expression(paste(Delta^{14}, "C")), xlab = 'Year',
  #      cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  # lines(atm14C, lwd = 2, col = rgb(74,20,134,,,255))
  # par(new = T)
  # plot(summary(resfert_pred_uncert_p), which = 1, col = rescols,quant = TRUE,
  #      ylim = lim_y, xlim = c(1958, 2010), main = '', legpos = NULL,
  #      lwd=2, ylab = '', xlab ='', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  # par(new = T)
  # plot(summary(highfert_pred_uncert_p), which = 1, col = highcols ,quant = TRUE,
  #      ylim = lim_y, xlim = c(1958, 2010), main = '', legpos = NULL,
  #      lwd=2, ylab = '', xlab ='', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)

  plot(summary(unfert_pred_uncert), which = 1, main = 'Fertilizer Trial 14C Model Outputs',
       col = unfertcols, quant = TRUE, ylim = lim_y, xlim = c(1958, 2010),
       legpos = NULL, lwd=2, ylab = expression(paste(Delta^{14}, "C")), xlab = 'Year',
       cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  lines(atm14C, lwd = 2, col = rgb(74,20,134,,,255))
  par(new = T)
  plot(summary(resfert_pred_uncert), which = 1, col = rescols,quant = TRUE,
       ylim = lim_y, xlim = c(1958, 2010), main = '', legpos = NULL,
       lwd=2, ylab = '', xlab ='', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  par(new = T)
  plot(summary(highfert_pred_uncert), which = 1, col = highcols ,quant = TRUE,
       ylim = lim_y, xlim = c(1958, 2010), main = '', legpos = NULL,
       lwd=2, ylab = '', xlab ='', cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)

  points(datunfert$time, datunfert$soilc14, bg = unfertcols[2], pch = 21, cex = 2)
  points(datresfert$time, datresfert$soilc14, bg = rescols[2], pch = 22, cex = 2)
  points(dathighfert$time, dathighfert$soilc14, bg = highcols[2], pch = 24, cex = 2)

  #Irr. 14C
  lim_y = c(-50, 230)

  plot(summary(dry_pred_uncert), which = 1, main = 'Irrigation Trial 14C Model Outputs',
       col = drycols, quant = TRUE, ylim = lim_y, xlim = c(1958, 2010),
       legpos = NULL, lwd=2, ylab = expression(paste(Delta^{14}, "C")), xlab = 'Year',
       cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  lines(atm14C, lwd = 2, col = rgb(74,20,134,,,255))
  par(new = T)
  plot(summary(irr10_pred_uncert), which = 1, col = irr10cols,quant = TRUE, ylim = lim_y,
       xlim = c(1958, 2010), main = '', legpos = NULL, lwd=2, ylab = '', xlab = '',
       cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)
  par(new = T)
  plot(summary(irr20_pred_uncert), which = 1, col = irr20cols ,quant = TRUE, ylim = lim_y,
       xlim = c(1958, 2010), main = '', legpos = NULL, lwd=2, ylab = '', xlab = '',
       cex.axis = 1.2, cex.lab = 1.4, cex.main = 1.5)

  points(datdry$time, datdry$soilc14, bg = drycols[2], pch = 21, cex = 2)
  points(dat10$time, dat10$soilc14, bg = irr10cols[2], pch = 22, cex = 2)
  points(dat20$time, dat20$soilc14, bg = irr20cols[2], pch = 24, cex = 2)

  # datunfert$trial = 'Unfert'
  # datresfert$trial = 'Res. Fert'
  # dathighfert$trial = 'High Fert'
  # atm14C$trial = "Atm."
  # atm14C$err <- 0
  # colnames(atm14C) <- c('time', 'soilc14','trial','err')
  # fertdat <- rbind(datunfert, datresfert, dathighfert,atm14C)
  #
  # p = print(ggplot(fertdat, aes(x = time, y = soilc14)) + geom_point(aes(color = trial),size = 4) +
  #         xlab('Year') + ylim(-25, 250) + xlim(1957, 2011) + geom_line(aes(color = trial)) +
  #         scale_color_manual(values = c(unfertcols[2], rescols[2], highcols[2],rgb(74,20,134,,,255))) +
  #         ylab(expression(bold(paste('Soil C Stock (T ha'^{-1},')')))) +
  #         theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
  #               legend.title = element_text(size = 14, face = 'bold'),
  #               plot.title = element_text(size = 18, face = 'bold'),
  #               axis.title=element_text(size=14,face="bold"),
  #               strip.text.y = element_text(size = 12)))
  #


  unfertcols <- c(rgb(254,178,76,alphaVal,,255), rgb(254,178,76,,,255))
  rescols <- c(rgb(252,78,42,alphaVal,,255), rgb(252,78,42,,,255))
  highcols <- c(rgb(177,0,38,alphaVal,,255), rgb(177,0,38,,,255))

  drycols <- c(rgb(127,205,187,alphaVal,,255), rgb(127,205,187,,,255))
  irr10cols <- c(rgb(29,145,192,alphaVal,,255), rgb(29,145,192,,,255))
  irr20cols <- c(rgb(12,44,132,alphaVal,,255), rgb(12,44,132,,,255))

  print(ggplot(irrTTz, aes(x = years, y = mean, fill = trial, ymax = Q95, ymin = Q05)) + geom_line() + facet_grid(irrTTz$trialStart, scales = 'free_y') +
          xlim(0, 20) + scale_fill_manual(values = c(drycols[2], irr10cols[2], irr20cols[2])) +
          geom_ribbon(alpha = .5) + theme_bw() +
          xlab('Years') + ylab('Density') + guides(fill=guide_legend(title="Trial")) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 18, face = 'bold'),
                axis.title=element_text(size=14,face="bold"),
                strip.text.y = element_text(size = 12)))

  print(ggplot(fertTTz, aes(x = years, y = mean, fill = trial, ymax = Q95, ymin = Q05)) + geom_line() + facet_grid(fertTTz$trialStart, scales = 'free_y') +
          xlim(0, 20) + scale_fill_manual(values = c(unfertcols[2], rescols[2], highcols[2])) +
          geom_ribbon(alpha = .5)+ theme_bw() +
          xlab('Years') + ylab('Density') + guides(fill=guide_legend(title="Trial")) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 18, face = 'bold'),
                axis.title=element_text(size=14,face="bold"),
                strip.text.y = element_text(size = 12)))

  ### Parameter plots by year ###

  print(ggplot(irrPars, aes(x = trial, y = k1, fill = trial)) + geom_boxplot() + facet_wrap(irrPars$trialStart) + #, scales = 'free_y'
          scale_fill_manual(values = c(drycols[2], irr10cols[2], irr20cols[2]))+ theme_bw() +
          xlab('Trial') + ylab('k1') + guides(fill=guide_legend(title="Trial")) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 18, face = 'bold'),
                axis.title=element_text(size=14,face="bold"),
                strip.text.y = element_text(size = 12)))

  print(ggplot(irrPars, aes(x = trial, y = k2, fill = trial)) + geom_boxplot() + facet_wrap(irrPars$trialStart) +
          scale_fill_manual(values = c(drycols[2], irr10cols[2], irr20cols[2]))+ theme_bw() +
          xlab('Trial') + ylab('k2') + guides(fill=guide_legend(title="Trial")) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 18, face = 'bold'),
                axis.title=element_text(size=14,face="bold"),
                strip.text.y = element_text(size = 12)))


  print(ggplot(irrPars, aes(x = trial, y = a21, fill = trial)) + geom_boxplot() + facet_wrap(irrPars$trialStart) +
          scale_fill_manual(values = c(drycols[2], irr10cols[2], irr20cols[2]))+ theme_bw() +
          xlab('Trial') + ylab('a21') + guides(fill=guide_legend(title="Trial")) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 18, face = 'bold'),
                axis.title=element_text(size=14,face="bold"),
                strip.text.y = element_text(size = 12)))


  print(ggplot(irrPars, aes(x = trial, y = slowProp, fill = trial)) + geom_boxplot() + facet_wrap(irrPars$trialStart) +
          scale_fill_manual(values = c(drycols[2], irr10cols[2], irr20cols[2])) + theme_bw() +
          xlab('Trial') + ylab('slowProp') + guides(fill=guide_legend(title="Trial")) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 18, face = 'bold'),
                axis.title=element_text(size=14,face="bold"),
                strip.text.y = element_text(size = 12)))


  # print(ggplot(irrPars, aes(x = trial, y = p1, fill = trial)) + geom_boxplot() + facet_wrap(irrPars$trialStart) +
  #         scale_fill_manual(values = c(drycols[2], irr10cols[2], irr20cols[2])))
  #
  # print(ggplot(irrPars, aes(x = trial, y = p2, fill = trial)) + geom_boxplot() + facet_wrap(irrPars$trialStart) +
  #         scale_fill_manual(values = c(drycols[2], irr10cols[2], irr20cols[2])))

  # Fertilizer Plots
  print(ggplot(fertPars, aes(x = trial, y = k1, fill = trial)) + geom_boxplot() + facet_wrap(fertPars$trialStart) + #, scales = 'free_y'
          scale_fill_manual(values = c(unfertcols[2], rescols[2], highcols[2]))+ theme_bw() +
          xlab('Trial') + ylab('k1') + guides(fill=guide_legend(title="Trial")) +
          theme(axis.text=element_text(size=12), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 18, face = 'bold'),
                axis.title=element_text(size=14,face="bold"),
                strip.text.x = element_text(size = 12, face = 'bold')))


  print(ggplot(fertPars, aes(x = trial, y = k2, fill = trial)) + geom_boxplot() + facet_wrap(fertPars$trialStart) +
          scale_fill_manual(values = c(unfertcols[2], rescols[2], highcols[2]))+theme_bw() +
          xlab('Trial') + ylab('k2') + guides(fill=guide_legend(title="Trial")) +
          theme(axis.text=element_text(size=12), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 18, face = 'bold'),
                axis.title=element_text(size=14,face="bold"),
                strip.text.x = element_text(size = 12, face = 'bold')))


  print(ggplot(fertPars, aes(x = trial, y = a21, fill = trial)) + geom_boxplot() + facet_wrap(fertPars$trialStart) +
          scale_fill_manual(values = c(unfertcols[2], rescols[2], highcols[2]))+theme_bw() +
          xlab('Trial') + ylab('a21') + guides(fill=guide_legend(title="Trial")) +
          theme(axis.text=element_text(size=12), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 18, face = 'bold'),
                axis.title=element_text(size=14,face="bold"),
                strip.text.x = element_text(size = 12, face = 'bold')))


  print(ggplot(fertPars, aes(x = trial, y = slowProp, fill = trial)) + geom_boxplot() + facet_wrap(fertPars$trialStart) +
          scale_fill_manual(values = c(unfertcols[2], rescols[2], highcols[2]))+theme_bw() +
          xlab('Trial') + ylab('slowProp') + guides(fill=guide_legend(title="Trial")) +
          theme(axis.text=element_text(size=12), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 18, face = 'bold'),
                axis.title=element_text(size=14,face="bold"),
                strip.text.x = element_text(size = 12, face = 'bold')))


  # print(ggplot(fertPars, aes(x = trial, y = p1, fill = trial)) + geom_boxplot() + facet_wrap(fertPars$trialStart) +
  #         scale_fill_manual(values = c(unfertcols[2], rescols[2], highcols[2])))
  #
  # print(ggplot(fertPars, aes(x = trial, y = p2, fill = trial)) + geom_boxplot() + facet_wrap(fertPars$trialStart) +
  #         scale_fill_manual(values = c(unfertcols[2], rescols[2], highcols[2])))



  #pred_uncert <- filter(pred_uncert, k2 > quantile(pred_uncert$k2, 0.05) & k2 < quantile(pred_uncert$k2, 0.95))



  # k1 stats
  dryk1s <- dryPars %>% dplyr::filter(Parameter == 'k1')
  dryk1s$trial <- 'Dry'
  irr10k1s <- irr10Pars %>% dplyr::filter(Parameter == 'k1')
  irr10k1s$trial <- 'Irr. 10'
  irr20k1s <- irr20Pars %>% dplyr::filter(Parameter == 'k1')
  irr20k1s$trial <- "Irr. 20"
  k1zi <- rbind(dryk1s, irr10k1s, irr20k1s)
  ylimMax <- max(boxplot.stats(dryk1s$Value)$stats[5], boxplot.stats(irr20k1s$Value)$stats[5], boxplot.stats(irr10k1s$Value)$stats[5])
  ylimMin <- min(boxplot.stats(dryk1s$Value)$stats[1], boxplot.stats(irr20k1s$Value)$stats[1], boxplot.stats(irr10k1s$Value)$stats[1])

  limy <- c(ylimMin, ylimMax)

  print(ggplot2::ggplot(k1zi, ggplot2::aes(x=trial, y = Value)) + ggplot2::geom_boxplot(fill = c(drycols[2], irr10cols[2], irr20cols[2]), notch = TRUE) +
          ggplot2::ggtitle('Irrigation :: k1 :: Parameter Distributions') + theme_bw() +
          xlab('Trial') + ylab('k1') + guides(fill=guide_legend(title="Trial")) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold"),
                strip.text.y = element_text(size = 12)))

  print(ggplot2::ggplot(k1zi, ggplot2::aes(x=trial, y = Value, fill = trial)) + ggplot2::geom_violin() + theme_bw() +
          geom_boxplot(width=0.1, fill = 'white') +scale_fill_manual(values =c(drycols[2], irr10cols[2], irr20cols[2]))+
          xlab('Trial') + ylab('k1') + guides(fill=guide_legend(title="Trial")) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))
  # + ylim(limy)

  k1pzks <- c('Dry-Irr10' = ks.test(filter(k1zi, trial == 'Dry')$Value, filter(k1zi, trial == 'Irr. 10')$Value)$p.value,
              'Dry-Irr20' = ks.test(filter(k1zi, trial == 'Dry')$Value, filter(k1zi, trial == 'Irr. 20')$Value)$p.value,
              'Irr10-Irr20' = ks.test(filter(k1zi, trial == 'Irr. 10')$Value, filter(k1zi, trial == 'Irr. 20')$Value)$p.value)
  k1pzwx <- c('Dry-Irr10' = wilcox.test(filter(k1zi, trial == 'Dry')$Value, filter(k1zi, trial == 'Irr. 10')$Value)$p.value,
              'Dry-Irr20' = wilcox.test(filter(k1zi, trial == 'Dry')$Value, filter(k1zi, trial == 'Irr. 20')$Value)$p.value,
              'Irr10-Irr20' = wilcox.test(filter(k1zi, trial == 'Irr. 10')$Value, filter(k1zi, trial == 'Irr. 20')$Value)$p.value)

  par(mfrow = c(2,1))
  textplot(round(k1pzks,10))
  title('k1 K-Smirnov p-values', cex.main = 2)
  textplot(round(k1pzwx,10))
  title('k1 Wilcox Test p-values', cex.main = 2)
  par(mfrow = c(1,1))

  # k2 stats
  dryk2s <- dryPars %>% dplyr::filter(Parameter == 'k2')
  dryk2s$trial <- 'Dry'
  irr10k2s <- irr10Pars %>% dplyr::filter(Parameter == 'k2')
  irr10k2s$trial <- 'Irr. 10'
  irr20k2s <- irr20Pars %>% dplyr::filter(Parameter == 'k2')
  irr20k2s$trial <- "Irr. 20"
  k2zi <- rbind(dryk2s, irr10k2s, irr20k2s)

  ylimMax <- max(boxplot.stats(dryk2s$Value)$stats[5], boxplot.stats(irr20k2s$Value)$stats[5], boxplot.stats(irr10k2s$Value)$stats[5])
  ylimMin <- min(boxplot.stats(dryk2s$Value)$stats[1], boxplot.stats(irr20k2s$Value)$stats[1], boxplot.stats(irr10k2s$Value)$stats[1])

  limy <- c(ylimMin, ylimMax)

  print(ggplot2::ggplot(k2zi, ggplot2::aes(x=trial, y = Value)) + ggplot2::geom_boxplot(fill = c(drycols[2], irr10cols[2], irr20cols[2]), notch = TRUE) +
          ggplot2::ggtitle('Irrigation :: k2 :: Parameter Distributions') + theme_bw() +
          xlab('Trial') + ylab('k2') + guides(fill=guide_legend(title="Trial")) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))

  print(ggplot2::ggplot(k2zi, ggplot2::aes(x=trial, y = Value, fill = trial)) + ggplot2::geom_violin() + theme_bw() +
          geom_boxplot(width=0.1, fill = 'white') +scale_fill_manual(values =c(drycols[2], irr10cols[2], irr20cols[2]))+
          xlab('Trial') + ylab('k2') + guides(fill=guide_legend(title="Trial")) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))

  k2pzks <- c('Dry-Irr10' = ks.test(filter(k2zi, trial == 'Dry')$Value, filter(k2zi, trial == 'Irr. 10')$Value)$p.value,
              'Dry-Irr20' = ks.test(filter(k2zi, trial == 'Dry')$Value, filter(k2zi, trial == 'Irr. 20')$Value)$p.value,
              'Irr10-Irr20' = ks.test(filter(k2zi, trial == 'Irr. 10')$Value, filter(k2zi, trial == 'Irr. 20')$Value)$p.value)
  k2pzwx <- c('Dry-Irr10' = wilcox.test(filter(k2zi, trial == 'Dry')$Value, filter(k2zi, trial == 'Irr. 10')$Value)$p.value,
              'Dry-Irr20' = wilcox.test(filter(k2zi, trial == 'Dry')$Value, filter(k2zi, trial == 'Irr. 20')$Value)$p.value,
              'Irr10-Irr20' = wilcox.test(filter(k2zi, trial == 'Irr. 10')$Value, filter(k2zi, trial == 'Irr. 20')$Value)$p.value)

  par(mfrow = c(2,1))
  textplot(round(k2pzks,12))
  title('k2 K-Smirnov p-values', cex.main = 2)
  textplot(round(k2pzwx,12))
  title('k2 Wilcox Test p-values', cex.main = 2)
  par(mfrow = c(1,1))

  # a21 stats
  drya21s <- dryPars %>% dplyr::filter(Parameter == 'a21')
  drya21s$trial <- 'Dry'
  irr10a21s <- irr10Pars %>% dplyr::filter(Parameter == 'a21')
  irr10a21s$trial <- 'Irr. 10'
  irr20a21s <- irr20Pars %>% dplyr::filter(Parameter == 'a21')
  irr20a21s$trial <- "Irr. 20"
  a21zi <- rbind(drya21s, irr10a21s, irr20a21s)

  ylimMax <- max(boxplot.stats(drya21s$Value)$stats[5], boxplot.stats(irr20a21s$Value)$stats[5], boxplot.stats(irr10a21s$Value)$stats[5])
  ylimMin <- min(boxplot.stats(drya21s$Value)$stats[1], boxplot.stats(irr20a21s$Value)$stats[1], boxplot.stats(irr10a21s$Value)$stats[1])

  limy <- c(ylimMin, ylimMax)

  print(ggplot2::ggplot(a21zi, ggplot2::aes(x=trial, y = Value)) + ggplot2::geom_boxplot(fill = c(drycols[2], irr10cols[2], irr20cols[2]), notch = TRUE) +
          ggplot2::ggtitle('Irrigation Trials :: a21 :: Bayesian Fit Parameter Distributions') + theme_bw() +
          xlab('Trial') + ylab('a21') + guides(fill=guide_legend(title="Trial")) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))

  print(ggplot2::ggplot(a21zi, ggplot2::aes(x=trial, y = Value, fill = trial)) + ggplot2::geom_violin() + theme_bw() +
          geom_boxplot(width=0.1, fill = 'white') +scale_fill_manual(values =c(drycols[2], irr10cols[2], irr20cols[2]))+
          xlab('Trial') + ylab('a21') + guides(fill=guide_legend(title="Trial")) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))

  a21pzks <- c('Dry-Irr10' = ks.test(filter(a21zi, trial == 'Dry')$Value, filter(a21zi, trial == 'Irr. 10')$Value)$p.value,
               'Dry-Irr20' = ks.test(filter(a21zi, trial == 'Dry')$Value, filter(a21zi, trial == 'Irr. 20')$Value)$p.value,
               'Irr10-Irr20' = ks.test(filter(a21zi, trial == 'Irr. 10')$Value, filter(a21zi, trial == 'Irr. 20')$Value)$p.value)
  a21pzwx <- c('Dry-Irr10' = wilcox.test(filter(a21zi, trial == 'Dry')$Value, filter(a21zi, trial == 'Irr. 10')$Value)$p.value,
               'Dry-Irr20' = wilcox.test(filter(a21zi, trial == 'Dry')$Value, filter(a21zi, trial == 'Irr. 20')$Value)$p.value,
               'Irr10-Irr20' = wilcox.test(filter(a21zi, trial == 'Irr. 10')$Value, filter(a21zi, trial == 'Irr. 20')$Value)$p.value)

  par(mfrow = c(2,1))
  textplot(round(a21pzks,10))
  title('a21 K-Smirnov p-values', cex.main = 2)
  textplot(round(a21pzwx,10))
  title('a21 Wilcox Test p-values', cex.main = 2)
  par(mfrow = c(1,1))

  #Transfer coefficient - Percent/proportion of pool 1 C that is transferred annually to pool 2
  dryInT <- a21zi %>% filter(trial == 'Dry')
  dryInT$Parameter <- 'PerTransfer'
  dryInT$Value <- drya21s$Value * dryk1s$Value

  irr10InT <- a21zi %>% filter(trial == 'Irr. 10')
  irr10InT$Parameter <- 'PerTransfer'
  irr10InT$Value <- irr10a21s$Value * irr10k1s$Value

  irr20InT <- a21zi %>% filter(trial == 'Irr. 20')
  irr20InT$Parameter <- 'PerTransfer'
  irr20InT$Value <- irr20a21s$Value * irr20k1s$Value

  tranz <- rbind(dryInT, irr10InT, irr20InT)
  tranz$trial <- as.factor(tranz$trial)

  print(ggplot2::ggplot(tranz, ggplot2::aes(x=trial, y = Value)) +theme_bw() +
          ggplot2::geom_boxplot(fill = c(drycols[2], irr10cols[2], irr20cols[2]), notch = TRUE) + #ylim(limy) +
          ggplot2::ggtitle('Irrigation Trials :: Transfer (k1 * a21) \nBayesian Fit Parameter Distributions') + #ggplot2::scale_x_discrete(limits =rev(levels(tranz$trial))) +
          xlab('Trial') + ylab('Transfer Fraction') + guides(fill=guide_legend(title="Trial")) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))

  # slowProp stats
  drySlows <- dryPars %>% dplyr::filter(Parameter == 'SlowProp')
  drySlows$trial <- 'Dry'
  irr10Slows <- irr10Pars %>% dplyr::filter(Parameter == 'SlowProp')
  irr10Slows$trial <- 'Irr. 10'
  irr20Slows <- irr20Pars %>% dplyr::filter(Parameter == 'SlowProp')
  irr20Slows$trial <- "Irr. 20"
  slowzi <- rbind(drySlows, irr10Slows, irr20Slows)

  ylimMax <- max(boxplot.stats(drySlows$Value)$stats[5], boxplot.stats(irr20Slows$Value)$stats[5], boxplot.stats(irr10Slows$Value)$stats[5])
  ylimMin <- min(boxplot.stats(drySlows$Value)$stats[1], boxplot.stats(irr20Slows$Value)$stats[1], boxplot.stats(irr10Slows$Value)$stats[1])

  limy <- c(ylimMin, ylimMax)

  print(ggplot2::ggplot(slowzi, ggplot2::aes(x=trial, y = Value)) + theme_bw() +
          ggplot2::geom_boxplot(fill = c(drycols[2], irr10cols[2], irr20cols[2]), notch = TRUE) +
          ggplot2::ggtitle('Irrigation :: SlowProp :: Parameter Distributions') +
          ggplot2::scale_x_discrete(limits =rev(levels(slowzi$trial)))+# + ylim(limy)
          xlab('Trial') + ylab('slowProp') + guides(fill=guide_legend(title="Trial")) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))

  print(ggplot2::ggplot(slowzi, ggplot2::aes(x=trial, y = Value, fill = trial)) +
          ggplot2::geom_violin() + theme_bw() +
          geom_boxplot(width=0.1, fill = 'white') +scale_fill_manual(values =c(drycols[2], irr10cols[2], irr20cols[2]))+
          xlab('Trial') + ylab('slowProp') + guides(fill=guide_legend(title="Trial")) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))

  slowpzks <- c('Dry-Irr10' = ks.test(filter(slowzi, trial == 'Dry')$Value, filter(slowzi, trial == 'Irr. 10')$Value)$p.value,
                'Dry-Irr20' = ks.test(filter(slowzi, trial == 'Dry')$Value, filter(slowzi, trial == 'Irr. 20')$Value)$p.value,
                'Irr10-Irr20' = ks.test(filter(slowzi, trial == 'Irr. 10')$Value, filter(slowzi, trial == 'Irr. 20')$Value)$p.value)
  slowpzwx <- c('Dry-Irr10' = wilcox.test(filter(slowzi, trial == 'Dry')$Value, filter(slowzi, trial == 'Irr. 10')$Value)$p.value,
                'Dry-Irr20' = wilcox.test(filter(slowzi, trial == 'Dry')$Value, filter(slowzi, trial == 'Irr. 20')$Value)$p.value,
                'Irr10-Irr20' = wilcox.test(filter(slowzi, trial == 'Irr. 10')$Value, filter(slowzi, trial == 'Irr. 20')$Value)$p.value)

  par(mfrow = c(2,1))
  textplot(round(slowpzks,10))
  title('slow K-Smirnov p-values', cex.main = 2)
  textplot(round(slowpzwx,10))
  title('slow Wilcox Test p-values', cex.main = 2)
  par(mfrow = c(1,1))


  irrParms <- rbind(k1zi, k2zi, a21zi, slowzi)
  colnames(irrParms) <- c("Parameter", "Value", 'Trial')
  irrParms$Trial <- factor(irrParms$Trial, levels = c('Dry', 'Irr. 10', "Irr. 20"))

  print(ggplot2::ggplot(irrParms, ggplot2::aes(x = Trial, y = Value, fill = Trial)) + theme_bw() +
          ggplot2::facet_wrap(~Parameter, scales = 'free_y') + ggplot2::theme(legend.position = 'none') +
          ggplot2::geom_boxplot(notch = TRUE) + ggplot2::xlab('') + ggplot2::ggtitle('Irrigation Trials :: Model Parameters')+
          scale_fill_manual(values = c(drycols[2], irr10cols[2], irr20cols[2]))+
          guides(fill=guide_legend(title="Trial")) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                strip.text.x = element_text(size = 12, face = "bold.italic"),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))

  print(ggplot2::ggplot(dryPars, ggplot2::aes(x=Parameter, y = Value)) + theme_bw() +
          ggplot2::geom_boxplot(fill = drycols[2], notch = TRUE) +
          ggplot2::ggtitle('Dryland :: Bayesian Fit Parameter Distributions') +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                strip.text.x = element_text(size = 12, face = "bold.italic"),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))

  print(ggplot2::ggplot(irr10Pars, ggplot2::aes(x=Parameter, y = Value)) + ggplot2::geom_boxplot(fill = irr10cols[2], notch = TRUE) +
          ggplot2::ggtitle('Irr. 10 :: Bayesian Fit Parameter Distributions') +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                strip.text.x = element_text(size = 12, face = "bold.italic"),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))

  print(ggplot2::ggplot(irr20Pars, ggplot2::aes(x=Parameter, y = Value)) + ggplot2::geom_boxplot(fill = irr20cols[2], notch = TRUE) +
          ggplot2::ggtitle('Irr. 20 :: Bayesian Fit Parameter Distributions') +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                strip.text.x = element_text(size = 12, face = "bold.italic"),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))

  drySA <- dryBayesDynamics %>% filter(Parameter == "MeanSA")
  irr10SA <- irr10BayesDynamics %>% filter(Parameter == "MeanSA")
  irr20SA <- irr20BayesDynamics %>% filter(Parameter == "MeanSA")
  drySA$name <- 'Dry'
  irr10SA$name <- "Irr. 10"
  irr20SA$name <- "Irr. 20"
  drySA <-data.frame(drySA$name, drySA$Value)
  irr10SA <-data.frame(irr10SA$name, irr10SA$Value)
  irr20SA <-data.frame(irr20SA$name, irr20SA$Value)
  colnames(drySA) <- c('name', 'MeanSA')
  colnames(irr10SA) <- c('name', 'MeanSA')
  colnames(irr20SA) <- c('name', 'MeanSA')
  drySA$log <- log(drySA$MeanSA)
  irr10SA$log <- log(irr10SA$MeanSA)
  irr20SA$log <- log(irr20SA$MeanSA)
  irrSAs <- rbind(drySA, irr10SA, irr20SA)

  drySAclip <- filter(drySA, log > (median(log(drySA$MeanSA)) - (confMod * (boxplot.stats(drySA$log)[['stats']][4] - boxplot.stats(drySA$log)[['stats']][2]))) &
                        log < (median(log(drySA$MeanSA)) + (confMod * (boxplot.stats(drySA$log)[['stats']][4] - boxplot.stats(drySA$log)[['stats']][2]))))
  irr10SAclip <- filter(irr10SA, log > (median(log(irr10SA$MeanSA)) - (confMod * (boxplot.stats(irr10SA$log)[['stats']][4] - boxplot.stats(irr10SA$log)[['stats']][2]))) &
                          log < (median(log(irr10SA$MeanSA)) + (confMod * (boxplot.stats(irr10SA$log)[['stats']][4] - boxplot.stats(irr10SA$log)[['stats']][2]))))
  irr20SAclip <- filter(irr20SA, log > (median(log(irr20SA$MeanSA)) - (confMod * (boxplot.stats(irr20SA$log)[['stats']][4] - boxplot.stats(irr20SA$log)[['stats']][2]))) &
                          log < (median(log(irr20SA$MeanSA)) + (confMod * (boxplot.stats(irr20SA$log)[['stats']][4] - boxplot.stats(irr20SA$log)[['stats']][2]))))

  irrSAclips <- rbind(drySAclip, irr10SAclip, irr20SAclip)

  print(ggplot2::ggplot(irrSAclips, ggplot2::aes(x=name, y = MeanSA)) + theme_bw() +
          ggplot2::geom_boxplot(fill = c(drycols[2], irr10cols[2], irr20cols[2]), notch = TRUE) +
          ggplot2::ggtitle(paste('Irrigation SA :: Without Outliers')) + ggplot2::ylab('Years') +xlab('') + #ylim(0,limy[2]) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                strip.text.x = element_text(size = 12, face = "bold.italic"),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))


  irrSAclippks <- c('Dry-Irr10' = ks.test(filter(irrSAclips, name == 'Dry')$MeanSA, filter(irrSAclips, name == 'Irr. 10')$MeanSA)$p.value,
                    'Dry-Irr20' = ks.test(filter(irrSAclips, name == 'Dry')$MeanSA, filter(irrSAclips, name == 'Irr. 20')$MeanSA)$p.value,
                    'Irr10-Irr20' = ks.test(filter(irrSAclips, name == 'Irr. 10')$MeanSA, filter(irrSAclips, name == 'Irr. 20')$MeanSA)$p.value)
  irrsSAclippwx <- c('Dry-Irr10' = wilcox.test(filter(irrSAclips, name == 'Dry')$MeanSA, filter(irrSAclips, name == 'Irr. 10')$MeanSA)$p.value,
                     'Dry-Irr20' = wilcox.test(filter(irrSAclips, name == 'Dry')$MeanSA, filter(irrSAclips, name == 'Irr. 20')$MeanSA)$p.value,
                     'Irr10-Irr20' = wilcox.test(filter(irrSAclips, name == 'Irr. 10')$MeanSA, filter(irrSAclips, name == 'Irr. 20')$MeanSA)$p.value)

  par(mfrow = c(2,1))
  textplot(round(irrSAclippks ,10))
  title('slow K-Smirnov p-values', cex.main = 2)
  textplot(round(irrsSAclippwx,10))
  title('slow Wilcox Test p-values', cex.main = 2)
  par(mfrow = c(1,1))


  ylimMax <- max(boxplot.stats(drySA$MeanSA)$stats[5], boxplot.stats(irr20SA$MeanSA)$stats[5], boxplot.stats(irr10SA$MeanSA)$stats[5])
  ylimMin <- min(boxplot.stats(drySA$MeanSA)$stats[1], boxplot.stats(irr20SA$MeanSA)$stats[1], boxplot.stats(irr10SA$MeanSA)$stats[1])

  limy <- c(ylimMin, ylimMax)

  print(ggplot2::ggplot(irrSAs, ggplot2::aes(x=name, y = MeanSA)) + theme_bw() +
          ggplot2::geom_boxplot(fill = c(drycols[2], irr10cols[2], irr20cols[2]), notch = TRUE) +
          ggplot2::ggtitle(paste('Irrigation SA :: C Dynamics')) + ggplot2::xlab('') + ggplot2::ylab('Years') +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                strip.text.x = element_text(size = 12, face = "bold.italic"),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))

  dryTT <- dryBayesDynamics %>% filter(Parameter == "MeanTT")
  irr10TT <- irr10BayesDynamics %>% filter(Parameter == "MeanTT")
  irr20TT <- irr20BayesDynamics %>% filter(Parameter == "MeanTT")
  dryTT <-data.frame(dryTT$Parameter, dryTT$Value)
  irr10TT <-data.frame(irr10TT$Parameter, irr10TT$Value)
  irr20TT <-data.frame(irr20TT$Parameter, irr20TT$Value)
  colnames(dryTT) <- c('name', 'MeanTT')
  colnames(irr10TT) <- c('name', 'MeanTT')
  colnames(irr20TT) <- c('name', 'MeanTT')
  dryTT$log <- log(dryTT$MeanTT)
  irr10TT$log <- log(irr10TT$MeanTT)
  irr20TT$log <- log(irr20TT$MeanTT)
  dryTT$name <- 'Dry'
  irr10TT$name <- "Irr. 10"
  irr20TT$name <- "Irr. 20"

  irrTTs <- rbind(dryTT, irr10TT, irr20TT)

  irrStocks <- rbind(data.frame(name = dryTT$name, stock = dryTT$MeanTT * mean(inputDry$inputs)),
                     data.frame(name = irr10TT$name, stock = irr10TT$MeanTT * mean(input10$inputs)),
                     data.frame(name = irr20TT$name, stock = irr20TT$MeanTT * mean(input20$inputs)))

  dryTTclip <- filter(dryTT, log > (median(log(dryTT$MeanTT)) - (confMod * (boxplot.stats(dryTT$log)[['stats']][4] - boxplot.stats(dryTT$log)[['stats']][2]))) &
                        log < (median(log(dryTT$MeanTT)) + (confMod * (boxplot.stats(dryTT$log)[['stats']][4] - boxplot.stats(dryTT$log)[['stats']][2]))))
  irr10TTclip <- filter(irr10TT, log > (median(log(irr10TT$MeanTT)) - (confMod * (boxplot.stats(irr10TT$log)[['stats']][4] - boxplot.stats(irr10TT$log)[['stats']][2]))) &
                          log < (median(log(irr10TT$MeanTT)) + (confMod * (boxplot.stats(irr10TT$log)[['stats']][4] - boxplot.stats(irr10TT$log)[['stats']][2]))))
  irr20TTclip <- filter(irr20TT, log > (median(log(irr20TT$MeanTT)) - (confMod * (boxplot.stats(irr20TT$log)[['stats']][4] - boxplot.stats(irr20TT$log)[['stats']][2]))) &
                          log < (median(log(irr20TT$MeanTT)) + (confMod * (boxplot.stats(irr20TT$log)[['stats']][4] - boxplot.stats(irr20TT$log)[['stats']][2]))))

  irrTTclips <- rbind(dryTTclip, irr10TTclip, irr20TTclip)

  print(ggplot2::ggplot(irrTTclips, ggplot2::aes(x=name, y = MeanTT)) + theme_bw() +
          ggplot2::geom_boxplot(fill = c(drycols[2], irr10cols[2], irr20cols[2]), notch = TRUE) +
          ggplot2::ggtitle(paste('Irrigation TT :: Without Outliers')) + ggplot2::ylab('Years') + xlab('') + #ylim(0,limy[2]) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                strip.text.x = element_text(size = 12, face = "bold.italic"),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))


  irrTTclippks <- c('Dry-Irr10' = ks.test(filter(irrTTclips, name == 'Dry')$MeanTT, filter(irrTTclips, name == 'Irr. 10')$MeanTT)$p.value,
                    'Dry-Irr20' = ks.test(filter(irrTTclips, name == 'Dry')$MeanTT, filter(irrTTclips, name == 'Irr. 20')$MeanTT)$p.value,
                    'Irr10-Irr20' = ks.test(filter(irrTTclips, name == 'Irr. 10')$MeanTT, filter(irrTTclips, name == 'Irr. 20')$MeanTT)$p.value)
  irrsTTclippwx <- c('Dry-Irr10' = wilcox.test(filter(irrTTclips, name == 'Dry')$MeanTT, filter(irrTTclips, name == 'Irr. 10')$MeanTT)$p.value,
                     'Dry-Irr20' = wilcox.test(filter(irrTTclips, name == 'Dry')$MeanTT, filter(irrTTclips, name == 'Irr. 20')$MeanTT)$p.value,
                     'Irr10-Irr20' = wilcox.test(filter(irrTTclips, name == 'Irr. 10')$MeanTT, filter(irrTTclips, name == 'Irr. 20')$MeanTT)$p.value)

  par(mfrow = c(2,1))
  textplot(round(irrTTclippks ,10))
  title('slow K-Smirnov p-values', cex.main = 2)
  textplot(round(irrsTTclippwx,10))
  title('slow Wilcox Test p-values', cex.main = 2)
  par(mfrow = c(1,1))


  ylimMax <- max(boxplot.stats(dryTT$MeanTT)$stats[5], boxplot.stats(irr20TT$MeanTT)$stats[5], boxplot.stats(irr10TT$MeanTT)$stats[5])
  ylimMin <- min(boxplot.stats(dryTT$MeanTT)$stats[1], boxplot.stats(irr20TT$MeanTT)$stats[1], boxplot.stats(irr10TT$MeanTT)$stats[1])

  limy <- c(ylimMin, ylimMax)

  print(ggplot2::ggplot(irrTTs, ggplot2::aes(x=name, y = MeanTT)) + theme_bw() +
          ggplot2::geom_boxplot(fill = c(drycols[2], irr10cols[2], irr20cols[2]), notch = TRUE) +
          ggplot2::ggtitle(paste('Irrigation TT :: C Dynamics')) + ggplot2::ylab('Years') + xlab('')+ #ylim(0,limy[2]) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                strip.text.x = element_text(size = 12, face = "bold.italic"),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))

  print(ggplot2::ggplot(irrStocks, ggplot2::aes(x=name, y = stock)) + theme_bw() +
          ggplot2::geom_boxplot(fill = c(drycols[2], irr10cols[2], irr20cols[2]), notch = TRUE) +
          ggplot2::ggtitle(paste('Irrigation TT :: C Dynamics')) + ggplot2::ylab('Years') + xlab('')+ #ylim(0,limy[2]) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                strip.text.x = element_text(size = 12, face = "bold.italic"),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))

  dryp1 <- dryBayesDynamics %>% filter(Parameter == "Pool1Age"& Value < 2000)
  irr10p1 <- irr10BayesDynamics %>% filter(Parameter == "Pool1Age"& Value < 2000)
  irr20p1 <- irr20BayesDynamics %>% filter(Parameter == "Pool1Age"& Value < 2000)
  dryp1$name <- 'Dry'
  irr10p1$name <- "Irr. 10"
  irr20p1$name <- "Irr. 20"
  dryp1 <-data.frame(dryp1$name, dryp1$Value)
  irr10p1 <-data.frame(irr10p1$name, irr10p1$Value)
  irr20p1 <-data.frame(irr20p1$name, irr20p1$Value)
  colnames(dryp1) <- c('name', 'Meanp1')
  colnames(irr10p1) <- c('name', 'Meanp1')
  colnames(irr20p1) <- c('name', 'Meanp1')
  irrp1s <- rbind(dryp1, irr10p1, irr20p1)

  ylimMax <- max(boxplot.stats(dryp1$Meanp1)$stats[5], boxplot.stats(irr20p1$Meanp1)$stats[5], boxplot.stats(irr10p1$Meanp1)$stats[5])
  ylimMin <- min(boxplot.stats(dryp1$Meanp1)$stats[1], boxplot.stats(irr20p1$Meanp1)$stats[1], boxplot.stats(irr10p1$Meanp1)$stats[1])

  limy <- c(ylimMin, ylimMax)

  print(ggplot2::ggplot(irrp1s, ggplot2::aes(x=name, y = Meanp1)) + theme_bw() +
          ggplot2::geom_boxplot(fill = c(drycols[2], irr10cols[2], irr20cols[2]), notch = TRUE) +
          ggplot2::ylab('Years') + #ggplot2::ylim(0,limy[2]) +
          ggplot2::ggtitle(paste('Irrigation p1 Age :: C Dynamics')) + #+ ylim(0,200)
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                strip.text.x = element_text(size = 12, face = "bold.italic"),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))

  dryp2 <- dryBayesDynamics %>% filter(Parameter == "Pool2Age" & Value < 500)
  irr10p2 <- irr10BayesDynamics %>% filter(Parameter == "Pool2Age"& Value < 400)
  irr20p2 <- irr20BayesDynamics %>% filter(Parameter == "Pool2Age"& Value < 400)
  dryp2$name <- 'Dry'
  irr10p2$name <- "Irr. 10"
  irr20p2$name <- "Irr. 20"
  dryp2 <-data.frame(dryp2$name, dryp2$Value)
  irr10p2 <-data.frame(irr10p2$name, irr10p2$Value)
  irr20p2 <-data.frame(irr20p2$name, irr20p2$Value)
  colnames(dryp2) <- c('name', 'Meanp2')
  colnames(irr10p2) <- c('name', 'Meanp2')
  colnames(irr20p2) <- c('name', 'Meanp2')
  irrp2s <- rbind(dryp2, irr10p2, irr20p2)

  p2means <- data.frame(mean(dryp2$Meanp2), mean(irr10p2$Meanp2), mean(irr20p2$Meanp2))
  colnames(p2means) <- c('Dry', 'Irr10', "Irr20")

  ylimMax <- max(boxplot.stats(dryp2$Meanp2)$stats[5], boxplot.stats(irr20p2$Meanp2)$stats[5], boxplot.stats(irr10p2$Meanp2)$stats[5])
  ylimMin <- min(boxplot.stats(dryp2$Meanp2)$stats[1], boxplot.stats(irr20p2$Meanp2)$stats[1], boxplot.stats(irr10p2$Meanp2)$stats[1])

  limy <- c(ylimMin, ylimMax)

  print(ggplot2::ggplot(irrp2s, ggplot2::aes(x=name, y = Meanp2)) + theme_bw() +
          ggplot2::geom_boxplot(fill = c(drycols[2], irr10cols[2], irr20cols[2]), notch = TRUE) +
          ggtitle(paste('Irrigation p2 Age :: C Dynamics')) + ylab('Mean P2 Age') + xlab('') +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                strip.text.x = element_text(size = 12, face = "bold.italic"),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))
  #Pool Size Comparisons
  irrP1size <- c(dryBayesCs$P1_C[length(dryBayesCs$P1_C)], irr10BayesCs$P1_C[length(irr10BayesCs$P1_C)], irr20BayesCs$P1_C[length(irr20BayesCs$P1_C)])
  irrP2size <- c(dryBayesCs$P2_C[length(dryBayesCs$P2_C)], irr10BayesCs$P2_C[length(irr10BayesCs$P2_C)], irr20BayesCs$P2_C[length(irr20BayesCs$P2_C)])
  irrPsizes <- data.frame(c("Dry","Irr. 10",'Irr. 20',"Dry","Irr. 10",'Irr. 20'), c('P1','P1','P1','P2','P2','P2'), c(irrP1size, irrP2size))
  colnames(irrPsizes) <- c('Trial', "Pool", 'Stock')
  irrPsizes$Trial <- factor(irrPsizes$Trial, levels = c("Dry","Irr. 10",'Irr. 20'), ordered = TRUE)
  print(ggplot(irrPsizes, aes(x = Trial, y = Stock, fill = Pool)) + geom_bar(stat = 'identity',position = 'dodge') +
          scale_fill_manual(values = c('cyan4', 'brown3')) + ylab('C Stock (Tonnes C per Ha)') + ggtitle('Irrigation Trials :: Final Pool Sizes') +
          ggplot2::theme(axis.text = ggplot2::element_text(size = 14), axis.title = ggplot2::element_text(size = 15),
                         plot.title = ggplot2::element_text(size =16),axis.title.x=ggplot2::element_blank()))

  irrP2sizeM <- data.frame(Mean = c(apply(dryPoolOutputs$Ct2[1:1000],1, mean)[length(dryPoolOutputs$Ct2[,1])],
                                    apply(irr10PoolOutputs$Ct2[1:1000],1, mean)[length(irr10PoolOutputs$Ct2[,1])],
                                    apply(irr20PoolOutputs$Ct2[1:1000],1, mean)[length(irr20PoolOutputs$Ct2[,1])]),
                           SD = c(apply(dryPoolOutputs$Ct2[1:1000],1, sd)[length(dryPoolOutputs$Ct2[,1])],
                                  apply(irr10PoolOutputs$Ct2[1:1000],1, sd)[length(irr10PoolOutputs$Ct2[,1])],
                                  apply(irr20PoolOutputs$Ct2[1:1000],1, sd)[length(irr20PoolOutputs$Ct2[,1])]),
                           trial = c('Dry', 'Irr. 10', 'Irr. 20'), Pool = 'P2')

  print(ggplot(irrP2sizeM, aes(x = trial, y = Mean)) + geom_bar(stat = 'identity') +
          geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),width = 0.2) )

  irrP1sizeM <- data.frame(Mean = c(apply(dryPoolOutputs$Ct1,1, mean)[length(dryPoolOutputs$Ct1[,1])],
                                    apply(irr10PoolOutputs$Ct1,1, mean)[length(irr10PoolOutputs$Ct1[,1])],
                                    apply(irr20PoolOutputs$Ct1,1, mean)[length(irr20PoolOutputs$Ct1[,1])]),
                           SD = c(apply(dryPoolOutputs$Ct1,1, sd)[length(dryPoolOutputs$Ct1[,1])],
                                  apply(irr10PoolOutputs$Ct1,1, sd)[length(irr10PoolOutputs$Ct1[,1])],
                                  apply(irr20PoolOutputs$Ct1,1, sd)[length(irr20PoolOutputs$Ct1[,1])]),
                           trial = c('Dry', 'Irr. 10', 'Irr. 20'), Pool = 'P1')

  irrPsizeM <- rbind(irrP1sizeM, irrP2sizeM)

  print(ggplot(irrPsizeM, aes(x = trial, y = Mean, fill = Pool)) + geom_bar(stat = 'identity', position = 'dodge') +
          geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD),width = 0.2, position = 'dodge') )

  #with(tranz, tapply(tranz$Value, tranz$trial, mean))


  #Replace with 0-1000 year sequence
  #TT comparison
  plot(seq(0, 1000, by = 0.5), irr20TT3$transitTimeDensity, type = 'l', col = irr20cols[2], lwd = 3, main = 'Irrigation Treatment Transit Times',
       xlab = 'Years', ylab = 'Density', xlim = c(0,100))
  abline(,,0)
  abline(,,,irr20TT3$quantiles[3], col = irr20cols[2], lty = 2, lwd = 3)
  lines(seq(0,1000, by = 0.5), irr10TT3$transitTimeDensity, col =irr10cols[2], lwd = 3)
  abline(,,,irr10TT3$quantiles[3], col = irr10cols[2], lty = 2, lwd = 3)
  lines(seq(0,1000, by = 0.5), dryTT3$transitTimeDensity, col = drycols[2], lwd = 3)
  abline(,,,dryTT3$quantiles[3], col = drycols[2], lty = 2, lwd = 3)
  legend('topright', legend = c('Dry', 'Irr. 10', 'Irr. 20'), lwd = 3, col = c(drycols[2], irr10cols[2], irr20cols[2]))

  #TT by mass comparison
  plot(seq(0,1000, by = 0.5), irr20TT3$transitTimeDensity * mean(irrInputs[3]), type = 'l', col = irr20cols[2], lwd = 3,
       main = 'Irrigation Transit Times, by Soil C Mass', xlab = 'Years', ylab = 'Tonnes C per Hectare', xlim = c(0,100))
  abline(,,0)
  abline(,,,irr20TT3$quantiles[3], col = irr20cols[2], lty = 2, lwd = 3)
  lines(seq(0,1000, by = 0.5), irr10TT3$transitTimeDensity * mean(irrInputs[2]), col =irr10cols[2], lwd = 3)
  abline(,,,irr10TT3$quantiles[3] * mean(irrInputs[2]), col = irr10cols[2], lty = 2, lwd = 3)
  lines(seq(0,1000, by = 0.5), dryTT3$transitTimeDensity * mean(irrInputs[1]), col = drycols[2], lwd = 3)
  abline(,,,dryTT3$quantiles[3] * mean(irrInputs[1]), col = drycols[2], lty = 2, lwd = 3)
  legend('topright', legend = c('Dry', 'Irr. 10', 'Irr. 20'), lwd = 3, col = c(drycols[2], irr10cols[2], irr20cols[2]))

  plot(seq(0,1000, by = 0.5), irr20TT3$transitTimeDensity - dryTT3$transitTimeDensity, type = 'l', col = irr20cols[2], lwd = 2,
       ylim = c(-0.008,0.006), xlim = c(0, 100), ylab = 'Density Difference (Treatment - Control)', xlab = 'Transit Time (Years)',
       main = 'Irrigation :: Treatment Effects on TT Density')#,
  #ylim = c(-.0001, 0.0001))
  abline(,,0, lwd = 2)
  abline(,,,irr20TT3$quantiles[3], col = irr20cols[2], lty = 2, lwd = 3)
  lines(seq(0,1000, by = 0.5), irr10TT3$transitTimeDensity - dryTT3$transitTimeDensity, col = irr10cols[2], lwd = 2)
  abline(,,,irr10TT3$quantiles[3], col = irr10cols[2], lty = 2, lwd = 3)
  abline(,,,dryTT3$quantiles[3], col = drycols[2], lty = 2, lwd = 3)
  legend('bottomright', legend = c('Control', 'Irr. 10', 'Irr. 20'), col = c(1, irr10cols[2], irr20cols[2]), lwd = 2)

  dryTT3df <- data.frame(seq(0, (length(dryTT3$transitTimeDensity)-1)/2, by = 0.5), unlist(dryTT3$transitTimeDensity))
  colnames(dryTT3df) <- c('years','TTdens')
  irr10TT3df <- data.frame(seq(0, (length(unlist(irr10TT3$transitTimeDensity))-1)/2, by = 0.5), unlist(irr10TT3$transitTimeDensity))
  colnames(irr10TT3df) <- c('years','TTdens')
  irr20TT3df <- data.frame(seq(0, (length(irr20TT3$transitTimeDensity)-1)/2, by = 0.5), unlist(irr20TT3$transitTimeDensity))
  colnames(irr20TT3df) <- c('years','TTdens')

  dry100 <- filter(dryTT3df, years < 100)
  irr10100 <- filter(irr10TT3df, years < 100)
  irr20100 <- filter(irr20TT3df, years < 100)

  #Percents under age x 0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8, 0.9,
  ages <- c(0,1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 6,7,8,9, 10,11,12,13,14,15, 20,25,
            30, 35, 40, 45, 50, 100, 200,300,400, 500, 600,700,800,900, 1000)

  irrAgeDist <- data.frame(ages, seq(1,length(ages)),seq(1,length(ages)),seq(1,length(ages)))
  colnames(irrAgeDist) <- c("Age",'Dry', 'Irr10', 'Irr20')
  y = 0
  for(a in ages){
    y = y + 1
    irrAgeDist$Dry[y] <-  round(AUC(dryTT3df$years, dryTT3df$TTdens, from = 0, to = a, method = 'spline') * 100, 1)
    irrAgeDist$Irr10[y] <- round(AUC(irr10TT3df$years, irr10TT3df$TTdens, from = 0, to = a, method = 'spline') * 100, 1)
    irrAgeDist$Irr20[y] <- round(AUC(irr20TT3df$years, irr20TT3df$TTdens, from = 0, to = a, method = 'spline') * 100, 1)
  }

  plot(irrAgeDist$Age, irrAgeDist$Dry, ylim = c(0,100), type = 'l', col = drycols[2], lwd = 3, xlim = c(0,10),
       xlab = 'Age', ylab = 'Cumulative C (%)', main = 'Fast C Transit Time Distribution')
  lines(irrAgeDist$Age, irrAgeDist$Irr10, col = irr10cols[2], lwd = 3)
  lines(irrAgeDist$Age, irrAgeDist$Irr20, col = irr20cols[2], lwd = 3)
  legend('topleft', legend = c('Dry', 'Irr. 10', 'Irr. 20'), col=c(drycols[2], irr10cols[2], irr20cols[2]), lwd = 3)

  plot(irrAgeDist$Age, irrAgeDist$Dry, ylim = c(0,100), type = 'l', col = drycols[2], lwd = 3, log = 'x',
       xlab = 'Age', ylab = 'Cumulative C (%)', main = 'Irrigation Transit Time Distribution')
  lines(irrAgeDist$Age, irrAgeDist$Irr10, col = irr10cols[2], lwd = 3)
  lines(irrAgeDist$Age, irrAgeDist$Irr20, col = irr20cols[2], lwd = 3)
  legend('topleft', legend = c('Dry', 'Irr. 10', 'Irr. 20'), col=c(drycols[2], irr10cols[2], irr20cols[2]), lwd = 3)

  #Use quantile and apply to find 95% confidence intervals (0.05 and 0.95) for each trial to constrain TT distributions
  #Calculate 5 and 95th percentile uncertainties
  # dryTTdist$Q95 <- apply(dryTTdist[,!(names(dryTTdist) %in% 'years')], 1, function(x) quantile(x, probs = 0.95))
  # dryTTdist$Q05 <- apply(dryTTdist[,!(names(dryTTdist) %in% 'years')], 1, function(x) quantile(x, probs = 0.05))
  # dryTTdist$mean <- apply(dryTTdist[,!(names(dryTTdist) %in% 'years')], 1, function(x) mean(x))
  # irr10TTdist$Q95 <- apply(irr10TTdist[,!(names(irr10TTdist) %in% 'years')], 1, function(x) quantile(x, probs = 0.95))
  # irr10TTdist$Q05 <- apply(irr10TTdist[,!(names(irr10TTdist) %in% 'years')], 1, function(x) quantile(x, probs = 0.05))
  # irr10TTdist$mean <- apply(irr10TTdist[,!(names(irr10TTdist) %in% 'years')], 1, function(x) mean(x))
  # irr20TTdist$Q95 <- apply(irr20TTdist[,!(names(irr20TTdist) %in% 'years')], 1, function(x) quantile(x, probs = 0.95))
  # irr20TTdist$Q05 <- apply(irr20TTdist[,!(names(irr20TTdist) %in% 'years')], 1, function(x) quantile(x, probs = 0.05))
  # irr20TTdist$mean <- apply(irr20TTdist[,!(names(irr20TTdist) %in% 'years')], 1, function(x) mean(x))
  # dryTTdist$Q75 <- apply(dryTTdist[,!(names(dryTTdist) %in% 'years')], 1, function(x) quantile(x, probs = 0.75))
  # dryTTdist$Q25 <- apply(dryTTdist[,!(names(dryTTdist) %in% 'years')], 1, function(x) quantile(x, probs = 0.25))
  # dryTTdist$mean <- apply(dryTTdist[,!(names(dryTTdist) %in% 'years')], 1, function(x) mean(x))
  # irr10TTdist$Q75 <- apply(irr10TTdist[,!(names(irr10TTdist) %in% 'years')], 1, function(x) quantile(x, probs = 0.75))
  # irr10TTdist$Q25 <- apply(irr10TTdist[,!(names(irr10TTdist) %in% 'years')], 1, function(x) quantile(x, probs = 0.25))
  # irr10TTdist$mean <- apply(irr10TTdist[,!(names(irr10TTdist) %in% 'years')], 1, function(x) mean(x))
  # irr20TTdist$Q75 <- apply(irr20TTdist[,!(names(irr20TTdist) %in% 'years')], 1, function(x) quantile(x, probs = 0.75))
  # irr20TTdist$Q25 <- apply(irr20TTdist[,!(names(irr20TTdist) %in% 'years')], 1, function(x) quantile(x, probs = 0.25))
  # irr20TTdist$mean <- apply(irr20TTdist[,!(names(irr20TTdist) %in% 'years')], 1, function(x) mean(x))
  # dryTTdist$sd <- apply(dryTTdist[,!(names(dryTTdist) %in% 'years')], 1, function(x) sd(x))
  # irr10TTdist$sd <- apply(irr10TTdist[,!(names(irr10TTdist) %in% 'years')], 1, function(x) sd(x))
  # irr20TTdist$sd <- apply(irr20TTdist[,!(names(irr20TTdist) %in% 'years')], 1, function(x) sd(x))

  #Calculate 5 and 95th percentile uncertainties - System Age
  drySAdist$Q95 <- apply(drySAdist[,!(names(drySAdist) %in% c('years', 'mean', 'Q05', 'Q95','trial','trialStart'))], 1, function(x) quantile(x, probs = 0.95))
  drySAdist$Q05 <- apply(drySAdist[,!(names(drySAdist) %in% c('years', 'mean', 'Q05', 'Q95','trial','trialStart'))], 1, function(x) quantile(x, probs = 0.05))
  drySAdist$mean <- apply(drySAdist[,!(names(drySAdist) %in% c('years', 'mean', 'Q05', 'Q95','trial','trialStart'))], 1, function(x) mean(x))
  irr10SAdist$Q95 <- apply(irr10SAdist[,!(names(irr10SAdist) %in% c('years', 'mean', 'Q05', 'Q95','trial','trialStart'))], 1, function(x) quantile(x, probs = 0.95))
  irr10SAdist$Q05 <- apply(irr10SAdist[,!(names(irr10SAdist) %in% c('years', 'mean', 'Q05', 'Q95','trial','trialStart'))], 1, function(x) quantile(x, probs = 0.05))
  irr10SAdist$mean <- apply(irr10SAdist[,!(names(irr10SAdist) %in% c('years', 'mean', 'Q05', 'Q95','trial','trialStart'))], 1, function(x) mean(x))
  irr20SAdist$Q95 <- apply(irr20SAdist[,!(names(irr20SAdist) %in% c('years', 'mean', 'Q05', 'Q95','trial','trialStart'))], 1, function(x) quantile(x, probs = 0.95))
  irr20SAdist$Q05 <- apply(irr20SAdist[,!(names(irr20SAdist) %in% c('years', 'mean', 'Q05', 'Q95','trial','trialStart'))], 1, function(x) quantile(x, probs = 0.05))
  irr20SAdist$mean <- apply(irr20SAdist[,!(names(irr20SAdist) %in% c('years', 'mean', 'Q05', 'Q95','trial','trialStart'))], 1, function(x) mean(x))

  #Relative TT density with 95% confidence intervals
  plot(dryTTdist$years, irr20TTdist$mean - dryTTdist$mean, type = 'l', col = irr20cols[2], lwd = 2,
       ylim = c(min(irr20TTdist$Q05 - dryTTdist$mean, irr10TTdist$Q05 - dryTTdist$mean),0.006), xlim = c(0, 100),
       ylab = 'Density Difference (Treatment - Control)', xlab = 'Transit Time (Years)',
       main = 'Irrigation Trials :: Transit Time \n Relative Difference in Density')
  polygon(c(dryTTdist$years, rev(dryTTdist$years)),c(dryTTdist$Q95 - dryTTdist$mean, rev(dryTTdist$Q05 - dryTTdist$mean)),
          col = drycols[1], border = NA)
  polygon(c(irr10TTdist$years, rev(irr10TTdist$years)), c(irr10TTdist$Q95 - dryTTdist$mean, rev(irr10TTdist$Q05 - dryTTdist$mean)),
          col = irr10cols[1], border = NA)
  polygon(c(irr20TTdist$years, rev(irr20TTdist$years)), c(irr20TTdist$Q05 - dryTTdist$mean, rev(irr20TTdist$Q95 - dryTTdist$mean)),
          col = irr20cols[1], border = NA)
  lines(dryTTdist$years, irr20TTdist$Q95 - dryTTdist$mean, col = irr20cols[2])
  lines(dryTTdist$years, irr20TTdist$Q05 - dryTTdist$mean, col = irr20cols[2])
  lines(dryTTdist$years, irr20TTdist$mean - dryTTdist$mean,col = irr20cols[2], lwd = 2)
  lines(dryTTdist$years, irr10TTdist$Q95 - dryTTdist$mean, col = irr10cols[2])
  lines(dryTTdist$years, irr10TTdist$Q05 - dryTTdist$mean, col = irr10cols[2])
  lines(dryTTdist$years, irr10TTdist$mean - dryTTdist$mean, col = irr10cols[2], lwd = 2)
  lines(dryTTdist$years, dryTTdist$Q95 - dryTTdist$mean, col = drycols[2])
  lines(dryTTdist$years, dryTTdist$Q05 - dryTTdist$mean, col = drycols[2])
  lines(dryTTdist$years, dryTTdist$mean - dryTTdist$mean, col = drycols[2], lwd =2)
  legend('topright', legend = c('Dry', 'Irr. 10', 'Irr. 20'), col=c(drycols[2], irr10cols[2], irr20cols[2]), lwd = 3)

  #By mass
  plot(dryTTdist$years, (irr20TTdist$mean  * mean(irrInputs[3]) - dryTTdist$mean  * mean(irrInputs[1])) * 100, type = 'l', col = irr20cols[2], lwd = 2,
       ylim = c(min(irr20TTdist$Q05 - dryTTdist$Q95, irr10TTdist$Q05 - dryTTdist$Q95)*100,
                (max(irr20TTdist$Q95) * 100 * mean(irrInputs[3]) - max(dryTTdist$mean) * 100 * mean(irrInputs[1]))), xlim = c(0, 30),
       ylab = 'Mass Difference (Treatment - Control) (g C m-2)', xlab = 'Transit Time (Years)', main = 'Carbon Flux Transit Time :: Irrigation \n With 95% CI, Scaled by Inputs')
  polygon(c(dryTTdist$years, rev(dryTTdist$years)),c(dryTTdist$Q95  * mean(irrInputs[1]) - dryTTdist$mean  * mean(irrInputs[1]), rev(dryTTdist$Q05  * mean(irrInputs[1]) - dryTTdist$mean  * mean(irrInputs[1]))) * 100,
          col = drycols[1], border = NA)
  polygon(c(irr10TTdist$years, rev(irr10TTdist$years)), c(irr10TTdist$Q95 * mean(irrInputs[2]) - dryTTdist$mean * mean(irrInputs[1]), rev(irr10TTdist$Q05 * mean(irrInputs[2]) - dryTTdist$mean * mean(irrInputs[1]))) * 100,
          col = irr10cols[1], border = NA)
  polygon(c(irr20TTdist$years, rev(irr20TTdist$years)), c(irr20TTdist$Q05 * mean(irrInputs[3]) - dryTTdist$mean * mean(irrInputs[1]), rev(irr20TTdist$Q95 * mean(irrInputs[3]) - dryTTdist$mean * mean(irrInputs[1]))) * 100,
          col = irr20cols[1], border = NA)
  #lines(dryTTdist$years, (dryTTdist$Q95 * irrInputs[1] - dryTTdist$mean * irrInputs[1]) * 100, col = drycols[2])
  #lines(dryTTdist$years, (dryTTdist$Q05 * irrInputs[1] - dryTTdist$mean * irrInputs[1]) * 100, col = drycols[2])
  lines(dryTTdist$years, (dryTTdist$mean * mean(irrInputs[1]) - dryTTdist$mean * mean(irrInputs[1])) * 100, col = drycols[2], lwd =3)
  #lines(dryTTdist$years, (irr20TTdist$Q95 * irrInputs[3] - dryTTdist$mean * irrInputs[1]) * 100, col = irr20cols[2])
  #lines(dryTTdist$years, (irr20TTdist$Q05 * irrInputs[3] - dryTTdist$mean * irrInputs[1]) * 100, col = irr20cols[2])
  lines(dryTTdist$years, (irr20TTdist$mean * mean(irrInputs[3]) - dryTTdist$mean * mean(irrInputs[1])) * 100,col = irr20cols[2], lwd = 3)
  #lines(dryTTdist$years, (irr10TTdist$Q95 * irrInputs[2] - dryTTdist$mean * irrInputs[1]) * 100, col = irr10cols[2])
  #lines(dryTTdist$years, (irr10TTdist$Q05 * irrInputs[2] - dryTTdist$mean * irrInputs[1]) * 100, col = irr10cols[2])
  lines(dryTTdist$years, (irr10TTdist$mean * mean(irrInputs[2]) - dryTTdist$mean * mean(irrInputs[1])) * 100, col = irr10cols[2], lwd = 3)
  # lines(dryTTdist$years, dryTTdist$Q95 * irrInputs[1] - dryTTdist$mean * irrInputs[1], col = drycols[2])
  # lines(dryTTdist$years, dryTTdist$Q05 * irrInputs[1] - dryTTdist$mean * irrInputs[1], col = drycols[2])
  legend('topright', legend = c('Dry', 'Irr. 10', 'Irr. 20'), col=c(drycols[2], irr10cols[2], irr20cols[2]),
         fill = c(drycols[1], irr10cols[1], irr20cols[1]), lwd = 3)


  plot(seq(0,1000, by = 0.5), irr20TT3$transitTimeDensity - dryTT3$transitTimeDensity, type = 'l', col = irr20cols[2], lwd = 2,
       ylim = c(-0.008,0.006), xlim = c(0, 100), ylab = 'Density Difference (Treatment - Control)', xlab = 'Transit Time (Years)')#,
  #ylim = c(-.0001, 0.0001))
  abline(,,0, lwd = 2)
  abline(,,,irr20TT3$quantiles[3], col = irr20cols[2], lty = 2, lwd = 3)
  lines(seq(0,1000, by = 0.5), irr10TT3$transitTimeDensity - dryTT3$transitTimeDensity, col = irr10cols[2], lwd = 2)
  abline(,,,irr10TT3$quantiles[3], col = irr10cols[2], lty = 2, lwd = 3)
  abline(,,,dryTT3$quantiles[3], col = drycols[2], lty = 2, lwd = 3)
  legend('bottomright', legend = c('Control', 'Irr. 10', 'Irr. 20'), col = c(1, irr10cols[2], irr20cols[2]), lwd = 2)

  plot(irr20TTdist$years, irr20TTdist$mean * mean(irrInputs[3]) * 100, type = 'l', xlim = c(0,30), col = irr20cols[2], lwd = 3,
       ylab = 'g C m-2', xlab = 'Years', main = 'Transit Time Distribution :: Irrigation \n Scaled by Inputs', ylim = c(0, 75))
  polygon(c(dryTTdist$years, rev(dryTTdist$years)),c(dryTTdist$Q95  * mean(irrInputs[1]), rev(dryTTdist$Q05  * mean(irrInputs[1]))) * 100,
          col = drycols[1], border = NA)
  polygon(c(irr10TTdist$years, rev(irr10TTdist$years)),c(irr10TTdist$Q95  * mean(irrInputs[2]), rev(irr10TTdist$Q05  * mean(irrInputs[2]))) * 100,
          col = irr10cols[1], border = NA)
  polygon(c(irr20TTdist$years, rev(irr20TTdist$years)),c(irr20TTdist$Q95  * mean(irrInputs[3]), rev(irr20TTdist$Q05  * mean(irrInputs[3]))) * 100,
          col = irr20cols[1], border = NA)
  lines(irr20TTdist$years, irr20TTdist$mean * mean(irrInputs[3]) * 100, col = irr20cols[2], lwd = 3)
  lines(irr10TTdist$years, irr10TTdist$mean * mean(irrInputs[2]) * 100, col = irr10cols[2], lwd = 3)
  lines(dryTTdist$years, dryTTdist$mean * mean(irrInputs[1]) * 100, col = drycols[2], lwd = 3)
  legend('topright', legend = c('Dry', 'Irr. 10', 'Irr. 20'), col=c(drycols[2], irr10cols[2], irr20cols[2]),
         fill = c(drycols[1], irr10cols[1], irr20cols[1]), lwd = 3)

  # unMTX <- as.matrix(unfertTTdist[,!(names(unfertTTdist) %in% 'years')])
  # apply(unMTX, 1, quantile, c(0.05, 0.95))

  plot(dryTTdist$years, ((irr20TTdist$mean - dryTTdist$mean)/dryTTdist$mean) * 100, type = 'l', col = irr20cols[2], lwd = 3,
       xlab = 'Transit Time (years)', ylab = 'Percent Difference', main = 'Irrigation Treatment Effects on Transit Time', , xlim = c(0,500), ylim = c(-100, 250))
  # ylim = c(min(((irr20TTdist$mean - dryTTdist$mean)/dryTTdist$mean) * 100, ((irr10TTdist$mean - dryTTdist$mean)/dryTTdist$mean) * 100),
  #          max(((irr20TTdist$mean - dryTTdist$mean)/dryTTdist$mean) * 100, ((irr10TTdist$mean - dryTTdist$mean)/dryTTdist$mean) * 100)))
  abline(,,0, lwd = 3, col = drycols[2], lty = 2)
  lines(dryTTdist$years, ((irr10TTdist$mean - dryTTdist$mean)/dryTTdist$mean) * 100, col = irr10cols[2], lwd = 3)
  legend('topright', legend = c('Dry', 'Irr. 10', 'Irr. 20'), col=c(drycols[2], irr10cols[2], irr20cols[2]), lwd = 3)

  #Plot with 95% confidence intervals
  plot(drySAdist$years, irr20SAdist$mean - drySAdist$mean, type = 'l', col = irr20cols[2], lwd = 2,
       ylim = c(min(drySAdist$Q05 - drySAdist$mean, irr10SAdist$Q05 - drySAdist$mean),
                max(irr20SAdist$Q95 - drySAdist$mean)), xlim = c(0, 100),
       ylab = 'Density Difference (Treatment - Control)', xlab = 'System Age (Years)',
       main = paste('Treatment Effects on System Age Distribution\n', 'Fertilization Trial'))
  polygon(c(drySAdist$years, rev(drySAdist$years)),c(drySAdist$Q95 - drySAdist$mean, rev(drySAdist$Q05 - drySAdist$mean)),
          col = drycols[1], border = NA)
  polygon(c(irr10SAdist$years, rev(irr10SAdist$years)), c(irr10SAdist$Q95 - drySAdist$mean, rev(irr10SAdist$Q05 - drySAdist$mean)),
          col = irr10cols[1], border = NA)
  polygon(c(irr20SAdist$years, rev(irr20SAdist$years)), c(irr20SAdist$Q05 - drySAdist$mean, rev(irr20SAdist$Q95 - drySAdist$mean)),
          col = irr20cols[1], border = NA)
  lines(drySAdist$years, irr20SAdist$mean - drySAdist$mean,col = irr20cols[2], lwd = 2)
  lines(drySAdist$years, irr20SAdist$Q95 - drySAdist$mean, col = irr20cols[2])
  lines(drySAdist$years, irr20SAdist$Q05 - drySAdist$mean, col = irr20cols[2])
  lines(drySAdist$years, irr10SAdist$mean - drySAdist$mean, col = irr10cols[2], lwd = 2)
  lines(drySAdist$years, irr10SAdist$Q95 - drySAdist$mean, col = irr10cols[2])
  lines(drySAdist$years, irr10SAdist$Q05 - drySAdist$mean, col = irr10cols[2])
  lines(drySAdist$years, drySAdist$mean - drySAdist$mean, col = drycols[2], lwd =2)
  lines(drySAdist$years, drySAdist$Q95 - drySAdist$mean, col = drycols[2])
  lines(drySAdist$years, drySAdist$Q05 - drySAdist$mean, col = drycols[2])
  legend('topright', legend = c('Dry', 'Irr. 10', 'Irr. 20'), lwd = 3, col = c(drycols[2], irr10cols[2], irr20cols[2]))

  plot(irr20SAdist$years, irr20SAdist$mean, col = irr20cols[2], lwd = 3, type = 'l', xlim= c(0,50), xlab = 'Years',
       ylab = 'Density', main = 'Irrigation Effects on System C Age Distribution', ylim = c(0, max(irr20SAdist$Q95)))
  polygon(c(irr20SAdist$years, rev(irr20SAdist$years)), c(irr20SAdist$Q95, rev(irr20SAdist$Q05)), col = irr20cols[1])
  polygon(c(irr10SAdist$years, rev(irr10SAdist$years)), c(irr10SAdist$Q95, rev(irr10SAdist$Q05)), col = irr10cols[1])
  polygon(c(drySAdist$years, rev(drySAdist$years)), c(drySAdist$Q95, rev(drySAdist$Q05)), col = drycols[1])
  lines(irr20SAdist$years, irr20SAdist$mean, col = irr20cols[2], lwd = 3)
  lines(irr10SAdist$years, irr10SAdist$mean, col = irr10cols[2], lwd = 3)
  lines(drySAdist$years, drySAdist$mean, col = drycols[2], lwd = 3)
  abline(,,0,lty = 2, lwd = 2)
  legend('topright', legend = c('Dry', 'Irr. 10', 'Irr. 20'), col=c(drycols[2], irr10cols[2], irr20cols[2]),
         fill = c(drycols[1], irr10cols[1], irr20cols[1]), lwd = 3)

  dryTTdist$mean <- apply(dryTTdist[,!(names(dryTTdist) %in% 'years')], 1, function(x) mean(x))

  dryPoolOutputs$Ct1$mean <- apply(dryPoolOutputs$Ct1, 1, function(x) mean(x))
  dryPoolOutputs$Ct1$median <- apply(dryPoolOutputs$Ct1, 1, function(x) median(x))
  dryPoolOutputs$Ct1$high <- apply(dryPoolOutputs$Ct1, 1, function(x) quantile(x, probs = .975))
  dryPoolOutputs$Ct1$low <- apply(dryPoolOutputs$Ct1, 1, function(x) quantile(x, probs = .025))

  dryPoolOutputs$Ct2$mean <- apply(dryPoolOutputs$Ct2, 1, function(x) mean(x))
  dryPoolOutputs$Ct2$median <- apply(dryPoolOutputs$Ct2, 1, function(x) median(x))
  dryPoolOutputs$Ct2$high <- apply(dryPoolOutputs$Ct2, 1, function(x) quantile(x, probs = .975))
  dryPoolOutputs$Ct2$low <- apply(dryPoolOutputs$Ct2, 1, function(x) quantile(x, probs = .025))

  # plot(dryPoolOutputs$Ct2$mean, ylim = c(10, 30))
  # lines(dryPoolOutputs$Ct2$high)
  # lines(dryPoolOutputs$Ct2$low)
  # lines(dryPoolOutputs$Ct2$median)

  dryp1F <- data.frame(t(dryPoolOutputs$Ct1[trialEnd - trialStart + 1,]), 'Dry')
  colnames(dryp1F) <- c('p1Size', 'trial')
  irr10p1F <- data.frame(t(irr10PoolOutputs$Ct1[irr10End - trialStart + 1,]), 'Irr. 10')
  colnames(irr10p1F) <- c('p1Size', 'trial')
  irr20p1F <- data.frame(t(irr20PoolOutputs$Ct1[trialEnd - trialStart + 1,]), 'Irr. 20')
  colnames(irr20p1F) <- c('p1Size', 'trial')
  irrp1F <- rbind(dryp1F, irr10p1F, irr20p1F)

  dryp2F <- data.frame(t(dryPoolOutputs$Ct2[trialEnd - trialStart + 1,]), 'Dry')
  colnames(dryp2F) <- c('p2Size', 'trial')
  irr10p2F <- data.frame(t(irr10PoolOutputs$Ct2[irr10End - trialStart + 1,]), 'Irr. 10')
  colnames(irr10p2F) <- c('p2Size', 'trial')
  irr20p2F <- data.frame(t(irr20PoolOutputs$Ct2[trialEnd - trialStart + 1,]), 'Irr. 20')
  colnames(irr20p2F) <- c('p2Size', 'trial')
  irrp2F <- rbind(dryp2F, irr10p2F, irr20p2F)

  print(ggplot2::ggplot(irrp1F, aes(trial, irrp1F$p1Size)) + theme_bw() +
          ggplot2::geom_boxplot(fill = c(drycols[2], irr10cols[2], irr20cols[2]), notch = TRUE) +
          ggtitle(paste('Irrigation p1 Size')) + ylab('T C ha-1') + xlab('') +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                strip.text.x = element_text(size = 12, face = "bold.italic"),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))

  print(ggplot2::ggplot(irrp2F, aes(irrp2F$trial, irrp2F$p2Size)) + theme_bw() +
          ggplot2::geom_boxplot(fill = c(drycols[2], irr10cols[2], irr20cols[2]), notch = TRUE) +
          ggtitle(paste('Irrigation p2 Size')) + ylab('T C ha-1') + xlab('') +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                strip.text.x = element_text(size = 12, face = "bold.italic"),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))

  # Histogram Grey Color
  # hist(h1, col=rgb(0.1,0.1,0.1,0.5),xlim=c(0,10), ylim=c(0,200), main=”Overlapping Histogram”)
  # hist(h2, col=rgb(0.8,0.8,0.8,0.5), add=T)
  # box()
  #
  # # Histogram Colored (blue and red)
  # hist(h1, col=rgb(1,0,0,0.5),xlim=c(0,10), ylim=c(0,200), main=”Overlapping Histogram”, xlab=”Variable”)
  # hist(h2, col=rgb(0,0,1,0.5), add=T)
  # box()

  alphaVal = 170

  unfertcols <- c(rgb(254,178,76,alphaVal,,255), rgb(254,178,76,,,255))
  rescols <- c(rgb(252,78,42,alphaVal,,255), rgb(252,78,42,,,255))
  highcols <- c(rgb(177,0,38,alphaVal,,255), rgb(177,0,38,,,255))

  drycols <- c(rgb(127,205,187,alphaVal,,255), rgb(127,205,187,,,255))
  irr10cols <- c(rgb(29,145,192,alphaVal,,255), rgb(29,145,192,,,255))
  irr20cols <- c(rgb(12,44,132,alphaVal,,255), rgb(12,44,132,,,255))

  hist(dryk1s$Value, col = drycols[1], breaks = 20, ylim = c(0, 200), xlab='k1',
       xlim=c(min(dryk1s$Value, irr10k1s$Value, irr20k1s$Value),
              max(dryk1s$Value, irr10k1s$Value, irr20k1s$Value)))
  hist(irr10k1s$Value, col= irr10cols[1], add=T,breaks = 20)
  hist(irr20k1s$Value, col= irr20cols[1], add=T,breaks = 20)
  legend('topright', legend = c('Dryland', 'Irr. 10', 'Irr. 20'), fill = c(drycols[1], irr10cols[1], irr20cols[2]))

  hist(dryk2s$Value, col = drycols[1], breaks = 20, ylim = c(0, 200),  xlab='k2',
       xlim=c(min(dryk2s$Value, irr10k2s$Value, irr20k2s$Value),
              max(dryk2s$Value, irr10k2s$Value, irr20k2s$Value)))
  hist(irr10k2s$Value, col= irr10cols[1], add=T,breaks = 20)
  hist(irr20k2s$Value, col= irr20cols[1], add=T,breaks = 20)
  legend('topright', legend = c('Dryland', 'Irr. 10', 'Irr. 20'), fill = c(drycols[1], irr10cols[1], irr20cols[2]))

  hist(drya21s$Value, col = drycols[1], breaks = 20, ylim = c(0, 300),  xlab='a21',
       xlim=c(min(drya21s$Value, irr10a21s$Value, irr20a21s$Value),
              max(drya21s$Value, irr10a21s$Value, irr20a21s$Value)))
  hist(irr10a21s$Value, col= irr10cols[1], add=T,breaks = 20)
  hist(irr20a21s$Value, col= irr20cols[1], add=T,breaks = 20)
  legend('topright', legend = c('Dryland', 'Irr. 10', 'Irr. 20'), fill = c(drycols[1], irr10cols[1], irr20cols[2]))

  hist(drySlows$Value, col = drycols[1], breaks = 20, ylim = c(0, 200), xlab='slowProp',
       xlim=c(min(drySlows$Value, irr10Slows$Value, irr20Slows$Value),
              max(drySlows$Value, irr10Slows$Value, irr20Slows$Value)))
  hist(irr10Slows$Value, col= irr10cols[1], add=T,breaks = 20)
  hist(irr20Slows$Value, col= irr20cols[1], add=T,breaks = 20)
  legend('topright', legend = c('Dryland', 'Irr. 10', 'Irr. 20'), fill = c(drycols[1], irr10cols[1], irr20cols[2]))

  hist(dryTT$MeanTT, col = drycols[1], breaks = 50, ylim = c(0, 100), xlab='Mean TT',
       xlim=c(min(dryTT$MeanTT, irr10TT$MeanTT, irr20TT$MeanTT),
              max(dryTT$MeanTT, irr10TT$MeanTT, irr20TT$MeanTT)))
  hist(irr10TT$MeanTT, col= irr10cols[1],add=T, breaks = 50)
  hist(irr20TT$MeanTT, col= irr20cols[1], add=T,breaks = 50)
  legend('topright', legend = c('Dryland', 'Irr. 10', 'Irr. 20'), fill = c(drycols[1], irr10cols[1], irr20cols[2]))

  hist(drySA$MeanSA, col = drycols[1], breaks = 50, ylim = c(0, 100), xlab='Mean SA',
       xlim=c(min(drySA$MeanSA, irr10SA$MeanSA, irr20SA$MeanSA),
              max(drySA$MeanSA, irr10SA$MeanSA, irr20SA$MeanSA)))
  hist(irr10SA$MeanSA, col= irr10cols[1],add=T, breaks = 50)
  hist(irr20SA$MeanSA, col= irr20cols[1], add=T,breaks = 50)
  legend('topright', legend = c('Dryland', 'Irr. 10', 'Irr. 20'), fill = c(drycols[1], irr10cols[1], irr20cols[2]))

  print(ggplot(irrPars, aes(k1,  fill = trial)) + geom_density() + theme_bw() +
          facet_grid(irrPars$trialStart, scales = 'free_x') +
          scale_fill_manual(values = c(drycols[1], irr10cols[1], irr20cols[1])))
  print(ggplot(irrPars, aes(k2,  fill = trial)) + geom_density() +  theme_bw() +
          facet_grid(irrPars$trialStart, scales = 'free_x') +
          scale_fill_manual(values = c(drycols[1], irr10cols[1], irr20cols[1])))
  print(ggplot(irrPars, aes(a21,  fill = trial)) + geom_density() +  theme_bw() +
          facet_grid(irrPars$trialStart, scales = 'free_x') +
          scale_fill_manual(values = c(drycols[1], irr10cols[1], irr20cols[1])))
  print(ggplot(irrPars, aes(slowProp,  fill = trial)) + geom_density() +  theme_bw() +
          facet_grid(irrPars$trialStart, scales = 'free_x') +
          scale_fill_manual(values = c(drycols[1], irr10cols[1], irr20cols[1])))

  print(ggplot(irrTTs, aes(MeanTT, fill = name)) + geom_density() + theme_bw() +
          scale_fill_manual(values = c(drycols[1], irr10cols[1], irr20cols[1])))
  print(ggplot(irrSAs, aes(MeanSA, fill = name)) + geom_density() + theme_bw() +
          scale_fill_manual(values = c(drycols[1], irr10cols[1], irr20cols[1])))

  print(ggplot(irrp1F, aes(fill = trial, x = p1Size)) + geom_density() + theme_bw() +
          scale_fill_manual(values = c(drycols[1], irr10cols[1], irr20cols[1])))
  print(ggplot(irrp2F, aes(fill = trial, x = p2Size)) + geom_density() + theme_bw() +
          scale_fill_manual(values = c(drycols[1], irr10cols[1], irr20cols[1])))

  # Irrigation
  k1 <- c(dry_bayes_fit$bestpar[1], irr20_bayes_fit$bestpar[1])
  k2 <- c(dry_bayes_fit$bestpar[2], irr20_bayes_fit$bestpar[2])
  a21 <- c(dry_bayes_fit$bestpar[3], irr20_bayes_fit$bestpar[3])

  irrP1size <- c(dryBayesCs$P1_C[length(dryBayesCs$P1_C)], irr20BayesCs$P1_C[length(irr20BayesCs$P1_C)])
  irrP1size <- c(irrP1sizeM$Mean[1], irrP1sizeM$Mean[3])
  irrP2size <- c(dryBayesCs$P2_C[length(dryBayesCs$P2_C)], irr20BayesCs$P2_C[length(irr20BayesCs$P2_C)])
  irrP2size <- c(irrP2sizeM$Mean[1], irrP2sizeM$Mean[3])

  infernoCols ='d3.scaleOrdinal () .range(["#7FCDBB", "#0C2C84"])'
  links <- data.frame(
    source = c(
      'Dry Inputs', 'Dry Inputs',
      'Dry P2', 'Dry P2',
      'Irr. 20 Inputs', 'Irr. 20 Inputs',
      'Irr. 20 P2', 'Irr. 20 P2'
    ),
    target = c('Dry Respiration', 'Dry P2',
               'Dry P2 Storage', 'Dry Respiration',
               'Irr. 20 Respiration', 'Irr. 20 P2',
               'Irr. 20 P2 Storage', 'Irr. 20 Respiration'
    ),
    value = c( (irrP1size[1] * k1[1]) - (irrP1size[1] * k1[1] * a21[1]),irrP1size[1] * k1[1] * a21[1],
               (irrP1size[1] * k1[1] * a21[1]) - k2[1] * irrP2size[1], k2[1] * irrP2size[1],
               (irrP1size[2] * k1[2]) - (irrP1size[2] * k1[2] * a21[2]),irrP1size[2] * k1[2] * a21[2],
               (irrP1size[2] * k1[2] * a21[2]) - k2[2] * irrP2size[2], k2[2] * irrP2size[2]

    )
  )

  nodes <- data.frame(
    name = c(as.character(links$source),
             as.character(links$target)) %>% unique()
  )

  links$IDsource <- match(links$source, nodes$name)-1
  links$IDtarget <- match(links$target, nodes$name)-1

  print(sankeyNetwork(Links = links, Nodes = nodes,
                      Source = "IDsource", Target = "IDtarget",
                      Value = 'value', NodeID = 'name',
                      sinksRight = F,
                      fontSize = 20,
                      nodePadding = 20,
                      colourScale = infernoCols)
  )


  nPlotIrr$norm = nPlotIrr$norm - 1

  print(ggplot(nPlotIrr, aes(x = source, y = norm, fill = Trial)) + geom_abline(slope = 0, intercept = 1, linetype = 'dashed') +
          geom_errorbar(aes(ymin = norm - normSD, ymax = norm + normSD), width = 0.4, position = position_dodge(0.9)) +
          geom_bar(position = 'dodge', stat = 'identity') +
          scale_fill_manual(values = c(drycols[2], irr10cols[2], irr20cols[2]))+ theme_bw() +
          ylab('Proportional Difference')+ xlab('') +
          scale_x_discrete(labels = c('Inputs','P1 Size', 'P2 Size', 'k1', 'k2','Respiration','Capture\nEfficiency'))+
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 18, face = 'bold'),
                axis.title=element_text(size=14,face="bold"),
                strip.text.y = element_text(size = 12, face = 'bold')) + theme_bw())



  unk1s <- unPars %>% dplyr::filter(Parameter == 'k1')
  unk1s$trial <- 'Unfert'
  resk1s <- resPars %>% dplyr::filter(Parameter == 'k1')
  resk1s$trial <- 'Res. Fert'
  highk1s <- highPars %>% dplyr::filter(Parameter == 'k1')
  highk1s$trial <- "High Fert"
  k1zf <- rbind(unk1s, resk1s, highk1s)
  k1zf$trial <- as.factor(k1zf$trial)

  ylimMax <- max(boxplot.stats(unk1s$Value)$stats[5], boxplot.stats(resk1s$Value)$stats[5], boxplot.stats(highk1s$Value)$stats[5])
  ylimMin <- min(boxplot.stats(unk1s$Value)$stats[1], boxplot.stats(resk1s$Value)$stats[1], boxplot.stats(highk1s$Value)$stats[1])

  k1zf$trial <- factor(k1zf$trial, levels = c('Unfert', 'Res. Fert', 'High Fert'), ordered = TRUE)

  limy <- c(ylimMin, ylimMax)

  print(ggplot2::ggplot(k1zf, ggplot2::aes(x=trial, y = Value, fill = trial)) +  theme_bw() +
          ggplot2::geom_boxplot(notch = TRUE) + xlab('') +
          ggplot2::ggtitle('Fertilizer :: k1 :: Parameter Distributions') + #ylim(limy) +
          guides(fill=guide_legend(title="Trial")) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                strip.text.x = element_text(size = 12, face = "bold.italic"),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold"))+
          scale_fill_manual(values = c(unfertcols[2], rescols[2], highcols[2])))

  #k2
  unfertk2s <- unPars %>% dplyr::filter(Parameter == 'k2')
  unfertk2s$trial <- 'Unfert'
  resk2s <- resPars %>% dplyr::filter(Parameter == 'k2')

  resk2s$trial <- 'Res. Fert'
  highk2s <- highPars %>% dplyr::filter(Parameter == 'k2')
  highk2s$trial <- "High Fert"
  k2zf <- rbind(unfertk2s, resk2s, highk2s)
  k2zf$trial <- as.factor(k2zf$trial)

  unfertk2s$log <- log(unfertk2s$Value)
  resk2s$log <- log(resk2s$Value)
  highk2s$log <- log(highk2s$Value)

  fertk2s <- rbind(unfertk2s, resk2s, highk2s)

  unfertk2clip <- filter(unfertk2s, log > (median(log(unfertk2s$Value)) - (confMod * (boxplot.stats(unfertk2s$log)[['stats']][4] - boxplot.stats(unfertk2s$log)[['stats']][2]))) &
                           log < (median(log(unfertk2s$Value)) + (confMod * (boxplot.stats(unfertk2s$log)[['stats']][4] - boxplot.stats(unfertk2s$log)[['stats']][2]))))
  resk2clip <- filter(resk2s, log > (median(log(resk2s$Value)) - (confMod * (boxplot.stats(resk2s$log)[['stats']][4] - boxplot.stats(resk2s$log)[['stats']][2]))) &
                        log < (median(log(resk2s$Value)) + (confMod * (boxplot.stats(resk2s$log)[['stats']][4] - boxplot.stats(resk2s$log)[['stats']][2]))))
  highk2clip <- filter(highk2s, log > (median(log(highk2s$Value)) - (confMod * (boxplot.stats(highk2s$log)[['stats']][4] - boxplot.stats(highk2s$log)[['stats']][2]))) &
                         log < (median(log(highk2s$Value)) + (confMod * (boxplot.stats(highk2s$log)[['stats']][4] - boxplot.stats(highk2s$log)[['stats']][2]))))

  fertk2clips <- rbind(unfertk2clip, resk2clip, highk2clip)
  fertk2clips$trial <- factor(fertk2clips$trial, levels = c('Unfert', 'Res. Fert', 'High Fert'), ordered = TRUE)

  print(ggplot2::ggplot(fertk2clips, ggplot2::aes(x=trial, y =Value)) +  theme_bw() +
          ggplot2::geom_boxplot(fill = c(unfertcols[2], rescols[2], highcols[2]), notch = TRUE) +
          ggplot2::ggtitle(paste('Fertilizer k2 :: With Outliers Removed')) + ggplot2::ylab('k2') + #ylim(0,limy[2]) +
          guides(fill=guide_legend(title="Trial")) + xlab('') +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                strip.text.x = element_text(size = 12, face = "bold.italic"),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))

  ylimMax <- max(boxplot.stats(unfertk2s$Value)$stats[5], boxplot.stats(resk2s$Value)$stats[5], boxplot.stats(highk2s$Value)$stats[5])
  ylimMin <- min(boxplot.stats(unfertk2s$Value)$stats[1], boxplot.stats(resk2s$Value)$stats[1], boxplot.stats(highk2s$Value)$stats[1])

  # ks.test(filter(k2zf, trial == 'Unfert')$Value, filter(k2zf, trial == 'High Fert')$Value)
  # boxplot(filter(k2zf, trial == 'Unfert')$Value, filter(k2zf, trial == 'High Fert')$Value)
  # ks.test(filter(k2zf, trial == 'Unfert')$Value, filter(k2zf, trial == 'Res. Fert')$Value)
  # boxplot(filter(k2zf, trial == 'Unfert')$Value, filter(k2zf, trial == 'Res. Fert')$Value)
  # ks.test(filter(k2zf, trial == 'High Fert')$Value, filter(k2zf, trial == 'Res. Fert')$Value)
  # boxplot(filter(k2zf, trial == 'High Fert')$Value, filter(k2zf, trial == 'Res. Fert')$Value)

  limy <- c(ylimMin, ylimMax)

  print(ggplot2::ggplot(k2zf, ggplot2::aes(x=trial, y = Value, fill = trial)) +  theme_bw() +
          ggplot2::geom_boxplot(notch = TRUE) + ggplot2::scale_x_discrete(limits =rev(levels(k2zf$trial))) +
          ggplot2::ggtitle('Fertilizer :: k2 :: Parameter Distributions') + #ylim(limy) +
          guides(fill=guide_legend(title="Trial")) + xlab('') +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                strip.text.x = element_text(size = 12, face = "bold.italic"),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")) +
          scale_fill_manual(values = c(highcols[2], rescols[2], unfertcols[2])))


  #a21
  unferta21s <- unPars %>% dplyr::filter(Parameter == 'a21')
  unferta21s$trial <- 'Unfert'
  resa21s <- resPars %>% dplyr::filter(Parameter == 'a21')
  resa21s$trial <- 'Res. Fert'
  higha21s <- highPars %>% dplyr::filter(Parameter == 'a21')
  higha21s$trial <- "High Fert"
  a21zf <- rbind(unferta21s, resa21s, higha21s)
  a21zf$trial <- as.factor(a21zf$trial)

  ylimMax <- max(boxplot.stats(unferta21s$Value)$stats[5], boxplot.stats(resa21s$Value)$stats[5], boxplot.stats(higha21s$Value)$stats[5])
  ylimMin <- min(boxplot.stats(unferta21s$Value)$stats[1], boxplot.stats(resa21s$Value)$stats[1], boxplot.stats(higha21s$Value)$stats[1])

  limy <- c(ylimMin, ylimMax)

  print(ggplot2::ggplot(a21zf, ggplot2::aes(x=trial, y = Value, fill = trial)) +  theme_bw() +
          ggplot2::geom_boxplot(notch = TRUE) + #ylim(limy) +
          ggplot2::ggtitle('Fertilizer Trials :: a21 :: Bayesian Fit Parameter Distributions') + ggplot2::scale_x_discrete(limits =rev(levels(k2zf$trial))) +
          scale_fill_manual(values = c(highcols[2], rescols[2], unfertcols[2])) +
          guides(fill=guide_legend(title="Trial")) + xlab('') +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                strip.text.x = element_text(size = 12, face = "bold.italic"),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))


  #Transfer coefficient - Percent/proportion of pool 1 C that is transferred annually to pool 2
  unInT <- a21zf %>% filter(trial == 'Unfert')
  unInT$Parameter <- 'PerTransfer'
  unInT$Value <- unferta21s$Value * unk1s$Value

  resInT <- a21zf %>% filter(trial == 'Res. Fert')
  resInT$Parameter <- 'PerTransfer'
  resInT$Value <- resa21s$Value * resk1s$Value

  highInT <- a21zf %>% filter(trial == 'High Fert')
  highInT$Parameter <- 'PerTransfer'
  highInT$Value <- higha21s$Value * highk1s$Value

  tranz <- rbind(unInT, resInT, highInT)
  tranz$trial <- as.factor(tranz$trial)

  print(ggplot2::ggplot(tranz, ggplot2::aes(x=trial, y = Value, fill = trial)) +
          theme_bw() + ggplot2::geom_boxplot(notch = TRUE) + #ylim(limy) +
          ggplot2::ggtitle('Irrigation Trials :: Transfer (k1 * a21) \nBayesian Fit Parameter Distributions') + ggplot2::scale_x_discrete(limits =rev(levels(k2zf$trial))) +
          scale_fill_manual(values = c(highcols[2], rescols[2], unfertcols[2])) +
          guides(fill=guide_legend(title="Trial")) + xlab('') + ylab('Transfer Fraction') +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                strip.text.x = element_text(size = 12, face = "bold.italic"),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))


  #Pool Size Comparisons
  fertP1size <- c(unfertBayesCs$P1_C[length(unfertBayesCs$P1_C)], resfertBayesCs$P1_C[length(resfertBayesCs$P1_C)], highfertBayesCs$P1_C[length(highfertBayesCs$P1_C)])
  fertP2size <- c(unfertBayesCs$P2_C[length(unfertBayesCs$P2_C)], resfertBayesCs$P2_C[length(resfertBayesCs$P2_C)], highfertBayesCs$P2_C[length(highfertBayesCs$P2_C)])
  fertPsizes <- data.frame(c("Unfert","Res. Fert",'High Fert',"Unfert","Res. Fert",'High Fert'), c('P1','P1','P1','P2','P2','P2'), c(fertP1size, fertP2size))
  colnames(fertPsizes) <- c('Trial', "Pool", 'Stock')
  fertPsizes$Trial <- factor(fertPsizes$Trial, levels = c("Unfert", 'Res. Fert','High Fert'), ordered = TRUE)
  print(ggplot(fertPsizes, aes(x = Trial, y = Stock, fill = Pool)) +  theme_bw() +
          geom_bar(stat = 'identity',position = 'dodge') +
          scale_fill_manual(values = c('cyan4', 'brown3')) + ylab('C Stock (Tonnes C per Ha)') + ggtitle('Fertilizer Trials :: Final Pool Sizes') +
          ggplot2::theme(axis.text = ggplot2::element_text(size = 14),
                         axis.title = ggplot2::element_text(size = 15),
                         plot.title = ggplot2::element_text(size =16),
                         axis.title.x=ggplot2::element_blank()))

  fertP1sizeM <- data.frame(Mean = c(apply(unfertPoolOutputs$Ct1,1, mean)[length(unfertPoolOutputs$Ct1[,1])],
                                     apply(resfertPoolOutputs$Ct1,1, mean)[length(resfertPoolOutputs$Ct1[,1])],
                                     apply(highfertPoolOutputs$Ct1,1, mean)[length(highfertPoolOutputs$Ct1[,1])]),
                            SD = c(apply(unfertPoolOutputs$Ct1,1, sd)[length(unfertPoolOutputs$Ct1[,1])],
                                   apply(resfertPoolOutputs$Ct1,1, sd)[length(resfertPoolOutputs$Ct1[,1])],
                                   apply(highfertPoolOutputs$Ct1,1, sd)[length(highfertPoolOutputs$Ct1[,1])]),
                            trial = c('Unfert', 'Res. Fert', 'High Fert'), Pool = 'P1')

  fertP2sizeM <- data.frame(Mean = c(apply(unfertPoolOutputs$Ct2,1, mean)[length(unfertPoolOutputs$Ct2[,1])],
                                     apply(resfertPoolOutputs$Ct2,1, mean)[length(resfertPoolOutputs$Ct2[,1])],
                                     apply(highfertPoolOutputs$Ct2,1, mean)[length(highfertPoolOutputs$Ct2[,1])]),
                            SD = c(apply(unfertPoolOutputs$Ct2,1, sd)[length(unfertPoolOutputs$Ct2[,1])],
                                   apply(resfertPoolOutputs$Ct2,1, sd)[length(resfertPoolOutputs$Ct2[,1])],
                                   apply(highfertPoolOutputs$Ct2,1, sd)[length(highfertPoolOutputs$Ct2[,1])]),
                            trial = c('Unfert', 'Res. Fert', 'High Fert'), Pool = 'P2')

  fertMassTransfers <- data.frame(c("Unfert", "Res. Fert", "High Fert"),
                                  c(mean(filter(tranz, trial == "Unfert")$Value) * fertP1size[1],
                                    mean(filter(tranz, trial == "Res. Fert")$Value) * fertP1size[2],
                                    mean(filter(tranz, trial == "High Fert")$Value) * fertP1size[3]),
                                  c(mean(filter(tranz, trial == "Unfert")$Value) * fertP1size[1] / fertInputs[1],
                                    mean(filter(tranz, trial == "Res. Fert")$Value) * fertP1size[2] / fertInputs[2],
                                    mean(filter(tranz, trial == "High Fert")$Value) * fertP1size[3] / fertInputs[3]))
  colnames(fertMassTransfers) <- c('Trial', "MassT", "InputNorm")
  fertMassTransfers$Trial <- factor(fertMassTransfers$Trial, levels = c('Unfert', 'Res. Fert', 'High Fert'), ordered = TRUE)

  print(ggplot(fertMassTransfers, aes(x = Trial, y = MassT, fill = Trial)) + geom_bar(stat = 'identity') +
          scale_fill_manual(values = c(unfertcols[2], rescols[2], highcols[2])) + ggtitle('Mass Transfer from P1 Annually') +
          guides(fill=guide_legend(title="Trial")) + xlab('') + ylab('Transfer Fraction') +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                strip.text.x = element_text(size = 12, face = "bold.italic"),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))


  print(ggplot(fertMassTransfers, aes(x = Trial, y = InputNorm, fill = Trial)) + geom_bar(stat = 'identity') +
          scale_fill_manual(values = c(unfertcols[2], rescols[2], highcols[2]))+ ggtitle('Mass Transfer from P1, normalized by Inputs')+
          guides(fill=guide_legend(title="Trial")) + xlab('') + ylab('Transfer Fraction') +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                strip.text.x = element_text(size = 12, face = "bold.italic"),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))

  #tapply()
  #summarize_at()

  #Slow proportion
  unSlows <- unPars %>% dplyr::filter(Parameter == 'SlowProp')
  unSlows$trial <- 'Unfert'
  resSlows <- resPars %>% dplyr::filter(Parameter == 'SlowProp')
  resSlows$trial <- 'Res. Fert'
  highSlows <- highPars %>% dplyr::filter(Parameter == 'SlowProp')
  highSlows$trial <- "High Fert"
  slowzf <- rbind(unSlows, resSlows, highSlows)

  ylimMax <- max(boxplot.stats(unSlows$Value)$stats[5], boxplot.stats(resSlows$Value)$stats[5], boxplot.stats(highSlows$Value)$stats[5])
  ylimMin <- min(boxplot.stats(unSlows$Value)$stats[1], boxplot.stats(resSlows$Value)$stats[1], boxplot.stats(highSlows$Value)$stats[1])

  limy <- c(ylimMin, ylimMax)

  print(ggplot2::ggplot(slowzf, ggplot2::aes(x=trial, y = Value, fill = trial)) +  theme_bw() +
          ggplot2::geom_boxplot(notch = TRUE) + #ylim(limy) +
          ggplot2::ggtitle('Fertilizer :: SlowProp :: Parameter Distributions') + ggplot2::scale_x_discrete(limits =rev(levels(k2zf$trial))) +
          ggplot2::ylab("SlowProp") + ggplot2::xlab('') + ggplot2::theme(axis.text=ggplot2::element_text(size=12), axis.title=ggplot2::element_text(size=14,face="bold")) +
          scale_fill_manual(values = c(highcols[2], rescols[2], unfertcols[2])) +
          guides(fill=guide_legend(title="Trial")) + xlab('') +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                strip.text.x = element_text(size = 12, face = "bold.italic"),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))



  fertParms <- rbind(k1zf, k2zf, a21zf, slowzf)
  colnames(fertParms) <- c("Parameter", "Value", 'Trial')
  fertParms$Trial <- factor(fertParms$Trial, levels = c('Unfert', 'Res. Fert', "High Fert"))

  print(ggplot2::ggplot(fertParms, ggplot2::aes(x = fertParms$Trial, y = Value, fill = Trial)) +  theme_bw() +
          ggplot2::facet_wrap(~Parameter, scales = 'free_y') + ggplot2::theme(legend.position = 'none') +
          ggplot2::geom_boxplot(notch = TRUE) + ggplot2::xlab('') + ggplot2::ggtitle('Fertilizer Trials :: Model Parameters') +
          scale_fill_manual(values = c(unfertcols[2], rescols[2], highcols[2])) +
          guides(fill=guide_legend(title="Trial")) + xlab('') + ylab('') +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                strip.text.x = element_text(size = 12, face = "bold.italic"),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))

  print(ggplot2::ggplot(unPars, ggplot2::aes(x=Parameter, y = Value)) +  theme_bw() +
          ggplot2::geom_boxplot(fill = unfertcols[2]) +
          ggplot2::ggtitle('Unfert :: Bayesian Fit Parameter Distributions') +
          guides(fill=guide_legend(title="Trial")) + xlab('') +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                strip.text.x = element_text(size = 12, face = "bold.italic"),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))

  print(ggplot2::ggplot(resPars, ggplot2::aes(x=Parameter, y = Value)) +  theme_bw() +
          ggplot2::geom_boxplot(fill = rescols[2]) +
          ggplot2::ggtitle('Res. Fert :: Bayesian Fit Parameter Distributions') +
          guides(fill=guide_legend(title="Trial")) + xlab('') +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                strip.text.x = element_text(size = 12, face = "bold.italic"),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))


  print(ggplot2::ggplot(highPars, ggplot2::aes(x=Parameter, y = Value)) +  theme_bw() +
          ggplot2::geom_boxplot(fill = highcols[2]) +
          ggplot2::ggtitle('High Fert :: Bayesian Fit Parameter Distributions') +
          guides(fill=guide_legend(title="Trial")) + xlab('') +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                strip.text.x = element_text(size = 12, face = "bold.italic"),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))



  unSA <- unfertBayesDynamics %>% filter(Parameter == "MeanSA")
  resSA <- resfertBayesDynamics %>% filter(Parameter == "MeanSA")
  highSA <- highfertBayesDynamics %>% filter(Parameter == "MeanSA")
  unSA$name <- 'Unfert'
  resSA$name <- "Res. Fert"
  highSA$name <- "High Fert"
  unSA <-data.frame(unSA$name, unSA$Value)
  resSA <-data.frame(resSA$name, resSA$Value)
  highSA <-data.frame(highSA$name, highSA$Value)
  colnames(unSA) <- c('name', 'MeanSA')
  colnames(resSA) <- c('name', 'MeanSA')
  colnames(highSA) <- c('name', 'MeanSA')
  fertSAs <- rbind(unSA, resSA, highSA)

  unSA$log <- log(unSA$MeanSA)
  resSA$log <- log(resSA$MeanSA)
  highSA$log <- log(highSA$MeanSA)

  fertSAs <- rbind(unSA, resSA, highSA)

  unSAclip <- filter(unSA, log > (median(log(unSA$MeanSA)) - (confMod * (boxplot.stats(unSA$log)[['stats']][4] - boxplot.stats(unSA$log)[['stats']][2]))) &
                       log < (median(log(unSA$MeanSA)) + (confMod * (boxplot.stats(unSA$log)[['stats']][4] - boxplot.stats(unSA$log)[['stats']][2]))))
  resSAclip <- filter(resSA, log > (median(log(resSA$MeanSA)) - (confMod * (boxplot.stats(resSA$log)[['stats']][4] - boxplot.stats(resSA$log)[['stats']][2]))) &
                        log < (median(log(resSA$MeanSA)) + (confMod * (boxplot.stats(resSA$log)[['stats']][4] - boxplot.stats(resSA$log)[['stats']][2]))))
  highSAclip <- filter(highSA, log > (median(log(highSA$MeanSA)) - (confMod * (boxplot.stats(highSA$log)[['stats']][4] - boxplot.stats(highSA$log)[['stats']][2]))) &
                         log < (median(log(highSA$MeanSA)) + (confMod * (boxplot.stats(highSA$log)[['stats']][4] - boxplot.stats(highSA$log)[['stats']][2]))))

  fertSAclips <- rbind(unSAclip, resSAclip, highSAclip)

  print(ggplot2::ggplot(fertSAclips, ggplot2::aes(x=name, y = MeanSA)) +  theme_bw() +
          ggplot2::geom_boxplot(fill = c(unfertcols[2], rescols[2], highcols[2]), notch = TRUE) +
          ggplot2::ggtitle(paste('Fertilizer SA :: Outliers Removed')) + ggplot2::ylab('Years') + #ylim(0,limy[2]) +
          guides(fill=guide_legend(title="Trial")) + xlab('') +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                strip.text.x = element_text(size = 12, face = "bold.italic"),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))

  ylimMax <- max(boxplot.stats(unSA$MeanSA)$stats[5], boxplot.stats(resSA$MeanSA)$stats[5], boxplot.stats(highSA$MeanSA)$stats[5])
  ylimMin <- min(boxplot.stats(unSA$MeanSA)$stats[1], boxplot.stats(resSA$MeanSA)$stats[1], boxplot.stats(resSA$MeanSA)$stats[1])

  limy <- c(ylimMin, ylimMax)

  print(ggplot2::ggplot(fertSAs, ggplot2::aes(x=name, y = MeanSA)) +  theme_bw() +
          ggplot2::geom_boxplot(fill = c(unfertcols[2], rescols[2], highcols[2]), notch = TRUE) +
          ggplot2::ggtitle(paste('Fertilizer SA :: C Dynamics')) + ggplot2::xlab('') + ggplot2::ylab('Years') + #ylim(0,limy[2]) +
          guides(fill=guide_legend(title="Trial")) + xlab('') +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                strip.text.x = element_text(size = 12, face = "bold.italic"),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))


  unTT <- unfertBayesDynamics %>% filter(Parameter == "MeanTT")
  resTT <- resfertBayesDynamics %>% filter(Parameter == "MeanTT")
  highTT <- highfertBayesDynamics %>% filter(Parameter == "MeanTT")
  unTT$name <- 'Unfert'
  resTT$name <- "Res. Fert"
  highTT$name <- "High Fert"
  unTT <-data.frame(unTT$name, unTT$Value)
  resTT <-data.frame(resTT$name, resTT$Value)
  highTT <-data.frame(highTT$name, highTT$Value)
  colnames(unTT) <- c('name', 'MeanTT')
  colnames(resTT) <- c('name', 'MeanTT')
  colnames(highTT) <- c('name', 'MeanTT')
  fertTTs <- rbind(unTT, resTT, highTT)

  unTT$log <- log(unTT$MeanTT)
  resTT$log <- log(resTT$MeanTT)
  highTT$log <- log(highTT$MeanTT)

  fertStocks <- rbind(data.frame(name = unTT$name, stock = unTT$MeanTT * mean(inputUn$inputs)),
                      data.frame(name = resTT$name, stock = resTT$MeanTT * mean(inputRes$inputs)),
                      data.frame(name = highTT$name, stock = highTT$MeanTT * mean(inputHigh$inputs)))



  fertTTs <- rbind(unTT, resTT, highTT)

  unTTclip <- filter(unTT, log > (median(log(unTT$MeanTT)) - (confMod * (boxplot.stats(unTT$log)[['stats']][4] - boxplot.stats(unTT$log)[['stats']][2]))) &
                       log < (median(log(unTT$MeanTT)) + (confMod * (boxplot.stats(unTT$log)[['stats']][4] - boxplot.stats(unTT$log)[['stats']][2]))))
  resTTclip <- filter(resTT, log > (median(log(resTT$MeanTT)) - (confMod * (boxplot.stats(resTT$log)[['stats']][4] - boxplot.stats(resTT$log)[['stats']][2]))) &
                        log < (median(log(resTT$MeanTT)) + (confMod * (boxplot.stats(resTT$log)[['stats']][4] - boxplot.stats(resTT$log)[['stats']][2]))))
  highTTclip <- filter(highTT, log > (median(log(highTT$MeanTT)) - (confMod * (boxplot.stats(highTT$log)[['stats']][4] - boxplot.stats(highTT$log)[['stats']][2]))) &
                         log < (median(log(highTT$MeanTT)) + (confMod * (boxplot.stats(highTT$log)[['stats']][4] - boxplot.stats(highTT$log)[['stats']][2]))))

  fertTTclips <- rbind(unTTclip, resTTclip, highTTclip)

  print(ggplot2::ggplot(fertTTclips, ggplot2::aes(x=name, y = MeanTT)) +  theme_bw() +
          ggplot2::geom_boxplot(fill = c(unfertcols[2], rescols[2], highcols[2]), notch = TRUE) +
          ggplot2::ggtitle(paste('Fertilizer TT :: With Outliers Removed')) + ggplot2::ylab('Years') + #ylim(0,limy[2]) +
          guides(fill=guide_legend(title="Trial")) + xlab('') +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                strip.text.x = element_text(size = 12, face = "bold.italic"),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))



  ylimMax <- max(boxplot.stats(unTT$MeanTT)$stats[5], boxplot.stats(resTT$MeanTT)$stats[5], boxplot.stats(highTT$MeanTT)$stats[5])
  ylimMin <- min(boxplot.stats(unTT$MeanTT)$stats[1], boxplot.stats(resTT$MeanTT)$stats[1], boxplot.stats(resTT$MeanTT)$stats[1])

  limy <- c(ylimMin, ylimMax)

  print(ggplot2::ggplot(fertTTs, ggplot2::aes(x=name, y = MeanTT)) +  theme_bw() +
          ggplot2::geom_boxplot(fill = c(unfertcols[2], rescols[2], highcols[2]), notch = TRUE) +
          ggplot2::ggtitle(paste('Fertilizer TT :: C Dynamics')) +
          guides(fill=guide_legend(title="Trial")) + xlab('') + ylab('Years') +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                strip.text.x = element_text(size = 12, face = "bold.italic"),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))

  print(ggplot2::ggplot(fertStocks, ggplot2::aes(x=name, y = stock)) +  theme_bw() +
          ggplot2::geom_boxplot(fill = c(unfertcols[2], rescols[2], highcols[2]), notch = TRUE) +
          ggplot2::ggtitle(paste('Fertilizer Steady State Stocks')) +
          guides(fill=guide_legend(title="Trial")) + xlab('') + ylab('T C ha-1') +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                strip.text.x = element_text(size = 12, face = "bold.italic"),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))
  # + ylim(0, limy[2])

  unp1 <- unfertBayesDynamics %>% filter(Parameter == "Pool1Age" & Value < 1000)
  resp1 <- resfertBayesDynamics %>% filter(Parameter == "Pool1Age" & Value < 1000)
  highp1 <- highfertBayesDynamics %>% filter(Parameter == "Pool1Age" & Value < 1000)
  unp1$name <- 'Unfert'
  resp1$name <- "Res. Fert"
  highp1$name <- "High Fert"
  unp1 <-data.frame(unp1$name, unp1$Value)
  resp1 <-data.frame(resp1$name, resp1$Value)
  highp1 <-data.frame(highp1$name, highp1$Value)
  colnames(unp1) <- c('name', 'Meanp1')
  colnames(resp1) <- c('name', 'Meanp1')
  colnames(highp1) <- c('name', 'Meanp1')
  fertp1s <- rbind(unp1, resp1, highp1)

  ylimMax <- max(boxplot.stats(unp1$Meanp1)$stats[5], boxplot.stats(resp1$Meanp1)$stats[5], boxplot.stats(highp1$Meanp1)$stats[5])
  ylimMin <- min(boxplot.stats(unp1$Meanp1)$stats[1], boxplot.stats(resp1$Meanp1)$stats[1], boxplot.stats(resp1$Meanp1)$stats[1])

  limy <- c(ylimMin, ylimMax)

  print(ggplot2::ggplot(fertp1s, ggplot2::aes(x=name, y = Meanp1, fill = name)) +  theme_bw() +
          ggplot2::geom_boxplot(notch = TRUE) +
          ggplot2::ggtitle(paste('Fertilizer p1 Age :: C Dynamics')) + #ylim(0, limy[2]) +
          scale_fill_manual(values = c(unfertcols[2], rescols[2], highcols[2])) + ylab('Years') +
          ggplot2::scale_x_discrete(limits =rev(levels(k2zf$trial))) +
          guides(fill=guide_legend(title="Trial")) + xlab('') +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                strip.text.x = element_text(size = 12, face = "bold.italic"),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))


  unp2 <- unfertBayesDynamics %>% filter(Parameter == "Pool2Age")
  resp2 <- resfertBayesDynamics %>% filter(Parameter == "Pool2Age")
  highp2 <- highfertBayesDynamics %>% filter(Parameter == "Pool2Age")
  unp2$name <- 'Unfert'
  resp2$name <- "Res. Fert"
  highp2$name <- "High Fert"
  unp2 <-data.frame(unp2$name, unp2$Value)
  resp2 <-data.frame(resp2$name, resp2$Value)
  highp2 <-data.frame(highp2$name, highp2$Value)
  colnames(unp2) <- c('name', 'Pool2Age')
  colnames(resp2) <- c('name', 'Pool2Age')
  colnames(highp2) <- c('name', 'Pool2Age')
  fertp2s <- rbind(unp2, resp2, highp2)

  unp2$log <- log(unp2$Pool2Age)
  resp2$log <- log(resp2$Pool2Age)
  highp2$log <- log(highp2$Pool2Age)


  fertp2s <- rbind(unp2, resp2, highp2)

  unp2clip <- filter(unp2, log > (median(log(unp2$Pool2Age)) - (confMod * (boxplot.stats(unp2$log)[['stats']][4] - boxplot.stats(unp2$log)[['stats']][2]))) &
                       log < (median(log(unp2$Pool2Age)) + (confMod * (boxplot.stats(unp2$log)[['stats']][4] - boxplot.stats(unp2$log)[['stats']][2]))))
  resp2clip <- filter(resp2, log > (median(log(resp2$Pool2Age)) - (confMod * (boxplot.stats(resp2$log)[['stats']][4] - boxplot.stats(resp2$log)[['stats']][2]))) &
                        log < (median(log(resp2$Pool2Age)) + (confMod * (boxplot.stats(resp2$log)[['stats']][4] - boxplot.stats(resp2$log)[['stats']][2]))))
  highp2clip <- filter(highp2, log > (median(log(highp2$Pool2Age)) - (confMod * (boxplot.stats(highp2$log)[['stats']][4] - boxplot.stats(highp2$log)[['stats']][2]))) &
                         log < (median(log(highp2$Pool2Age)) + (confMod * (boxplot.stats(highp2$log)[['stats']][4] - boxplot.stats(highp2$log)[['stats']][2]))))

  fertp2clips <- rbind(unp2clip, resp2clip, highp2clip)

  print(ggplot2::ggplot(fertp2clips, ggplot2::aes(x=name, y = Pool2Age)) +  theme_bw() +
          ggplot2::geom_boxplot(fill = c(unfertcols[2], rescols[2], highcols[2]), notch = TRUE) +
          ggplot2::ggtitle(paste('Fertilizer p2 Age :: With Outliers Removed')) + ggplot2::ylab('Years') + #ylim(0,limy[2]) +
          guides(fill=guide_legend(title="Trial")) + xlab('') +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                strip.text.x = element_text(size = 12, face = "bold.italic"),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))



  ylimMax <- max(boxplot.stats(unp2$Meanp2)$stats[5], boxplot.stats(resp2$Meanp2)$stats[5], boxplot.stats(highp2$Meanp2)$stats[5])
  ylimMin <- min(boxplot.stats(unp2$Meanp2)$stats[1], boxplot.stats(resp2$Meanp2)$stats[1], boxplot.stats(resp2$Meanp2)$stats[1])

  limy <- c(ylimMin, ylimMax)

  print(ggplot2::ggplot(fertp2s, ggplot2::aes(x=name, y = Pool2Age)) +  theme_bw() +
          ggplot2::geom_boxplot(fill = c(unfertcols[2], rescols[2], highcols[2]), notch = TRUE) +
          ggplot2::ggtitle(paste('Fertilizer p2 Age :: C Dynamics')) + #ylim(0, limy[2]) +
          guides(fill=guide_legend(title="Trial")) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                strip.text.x = element_text(size = 12, face = "bold.italic"),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")) +
          ggplot2::xlab('') + ggplot2::ylab('Years'))

  #TT comparison
  plot(seq(0,1000, by = 0.5), highfertTT3$transitTimeDensity, type = 'l', col = highcols[2], lwd = 3,
       main = 'Fertilizer Treatment Transit Times with Medians',
       xlab = 'Years', ylab = 'Density', xlim = c(0,100))
  abline(,,0)
  abline(,,,highfertTT3$quantiles[3], col = highcols[2], lty = 2, lwd = 3)
  lines(seq(0,1000, by = 0.5), resfertTT3$transitTimeDensity, col =rescols[2], lwd = 3)
  abline(,,,resfertTT3$quantiles[3], col = rescols[2], lty = 2, lwd = 3)
  lines(seq(0,1000, by = 0.5), unfertTT3$transitTimeDensity, col = unfertcols[2], lwd = 3)
  abline(,,,unfertTT3$quantiles[3], col = unfertcols[2], lty = 2, lwd = 3)
  legend('topright', legend = c('Unfert.', 'Res. Fert', 'High Fert'), lwd = 3, col = c(unfertcols[2], rescols[2], highcols[2]))

  #TT by mass comparison
  plot(seq(0,1000, by = 0.5), highfertTT3$transitTimeDensity * fertInputs[3], type = 'l', col = highcols[2], lwd = 3, log = 'y',
       main = 'Fertilization Transit Times, by Soil C Mass', xlab = 'Years', ylab = 'Tonnes C per Hectare', xlim = c(0,100))
  abline(,,0)
  abline(,,,highfertTT3$quantiles[3], col = highcols[2], lty = 2, lwd = 3)
  lines(seq(0,1000, by = 0.5), resfertTT3$transitTimeDensity * fertInputs[2], col = rescols[2], lwd = 3)
  abline(,,,resfertTT3$quantiles[3], col = rescols[2], lty = 2, lwd = 3)
  lines(seq(0,1000, by = 0.5), unfertTT3$transitTimeDensity * fertInputs[1], col = unfertcols[2], lwd = 3)
  abline(,,,unfertTT3$quantiles[3], col = unfertcols[2], lty = 2, lwd = 3)
  legend('topright', legend = c('Unfert.', 'Res. Fert', 'High Fert'), lwd = 3, col = c(unfertcols[2], rescols[2], highcols[2]))

  plot(seq(0,1000, by = 0.5), highfertTT3$transitTimeDensity - unfertTT3$transitTimeDensity, type = 'l', col = highcols[2], lwd = 2, xlim = c(00,100),
       ylim = c(-0.02, 0.016), ylab = 'Density Difference (Treatment - Control)', xlab = 'Transit Time (Years)')#,
  #ylim = c(-.00005, 0.0001))
  abline(,,0, lwd = 2, col = unfertcols[2])
  lines(seq(0,1000, by = 0.5),resfertTT3$transitTimeDensity - unfertTT3$transitTimeDensity, col = rescols[2], lwd = 2)
  abline(,,,highfertTT3$quantiles[3], col = highcols[2], lty = 2, lwd = 3)
  abline(,,,resfertTT3$quantiles[3], col = rescols[2], lty = 2, lwd = 3)
  abline(,,,unfertTT3$quantiles[3], col = unfertcols[2], lty = 2, lwd = 3)

  legend('bottomright', legend = c('Unfert.','Res. Fert', 'High Fert','50th Percentile'), lwd = 3, col = c(unfertcols[2], rescols[2], highcols[2], 1), lty= c(1,1,1,2))
  #abline(,,,boxplot.stats(highfertTT3$transitTimeDensity - unfertTT3$transitTimeDensity)$stats)

  # TT3best$years <- seq(0, 1000, by = 0.5)
  # TT3best <- data.frame(matrix(unlist(TT3best), nrow = 2000, byrow = T))
  # filter(TT3best, TT3best < 100)
  #
  # fart <- TT3best
  # fart$years <- seq(0,1000,by = 0.5)
  # fart <- data.frame(unlist(TT3best$transitTimeDensity), unlist(fart$years))
  # colnames(fart) <- c('TTdens', 'years')
  #
  # z <- filter(fart, years < 100)
  #
  # df <- data.frame(matrix(unlist(l), nrow=length(l), byrow=T))

  unfertTT3df <- data.frame(seq(0, (length(unfertTT3$transitTimeDensity)-1)/2, by = 0.5), unlist(unfertTT3$transitTimeDensity))
  colnames(unfertTT3df) <- c('years','TTdens')
  resfertTT3df <- data.frame(seq(0, (length(unlist(resfertTT3$transitTimeDensity))-1)/2, by = 0.5), unlist(resfertTT3$transitTimeDensity))
  colnames(resfertTT3df) <- c('years','TTdens')
  highfertTT3df <- data.frame(seq(0, (length(highfertTT3$transitTimeDensity)-1)/2, by = 0.5), unlist(highfertTT3$transitTimeDensity))
  colnames(highfertTT3df) <- c('years','TTdens')

  alphaVal = 170

  unfertcols <- c(rgb(254,178,76,alphaVal,,255), rgb(254,178,76,,,255))
  rescols <- c(rgb(252,78,42,alphaVal,,255), rgb(252,78,42,,,255))
  highcols <- c(rgb(177,0,38,alphaVal,,255), rgb(177,0,38,,,255))

  drycols <- c(rgb(127,205,187,alphaVal,,255), rgb(127,205,187,,,255))
  irr10cols <- c(rgb(29,145,192,alphaVal,,255), rgb(29,145,192,,,255))
  irr20cols <- c(rgb(12,44,132,alphaVal,,255), rgb(12,44,132,,,255))

  hist(unk1s$Value, col = unfertcols[1], breaks = 20, ylim = c(0, 180),
       xlim=c(min(unk1s$Value, resk1s$Value, highk1s$Value),
              max(unk1s$Value, resk1s$Value, highk1s$Value)), xlab = 'k1')
  hist(resk1s$Value, col= rescols[1], add=T,breaks = 20)
  hist(highk1s$Value, col= highcols[1], add=T,breaks = 20)
  legend('topright', legend = c('Unfert', 'Res. Fert', 'High Fert'), fill = c(unfertcols[1], rescols[1], highcols[2]))

  hist(unfertk2s$Value, col = unfertcols[1], breaks = 20, ylim = c(0, 250),
       xlim=c(min(unfertk2s$Value, resk2s$Value, highk2s$Value),
              max(unfertk2s$Value, resk2s$Value, highk2s$Value)), xlab = 'k2')
  hist(resk2s$Value, col= rescols[1], add=T,breaks = 20)
  hist(highk2s$Value, col= highcols[1], add=T,breaks =20)
  legend('topright', legend = c('Unfert', 'Res. Fert', 'High Fert'), fill = c(unfertcols[1], rescols[1], highcols[2]))

  hist(unferta21s$Value, col = unfertcols[1], breaks = 20, ylim = c(0, 200),
       xlim=c(min(unferta21s$Value, resa21s$Value, higha21s$Value),
              max(unferta21s$Value, resa21s$Value, higha21s$Value)), xlab = 'a21')
  hist(resa21s$Value, col= rescols[1], add=T,breaks = 20)
  hist(higha21s$Value, col= highcols[1], add=T,breaks = 20)
  legend('topright', legend = c('Unfert', 'Res. Fert', 'High Fert'), fill = c(unfertcols[1], rescols[1], highcols[2]))

  hist(unSlows$Value, col = unfertcols[1], breaks = 20, ylim = c(0, 200),
       xlim=c(min(unSlows$Value, resSlows$Value, highSlows$Value),
              max(unSlows$Value, resSlows$Value, highSlows$Value)), xlab = 'slowProp')
  hist(resSlows$Value, col= rescols[1], add=T,breaks = 20)
  hist(highSlows$Value, col= highcols[1], add=T,breaks = 20)
  legend('topleft', legend = c('Unfert', 'Res. Fert', 'High Fert'), fill = c(unfertcols[1], rescols[1], highcols[2]))

  hist(unTT$MeanTT, col = unfertcols[1], breaks = 20, ylim = c(0, 200),
       xlim=c(min(unTT$MeanTT, resTT$MeanTT, highTT$MeanTT),
              max(unTT$MeanTT, resTT$MeanTT, highTT$MeanTT)), xlab = 'Mean TT')
  hist(resTT$MeanTT, col= rescols[1],add=T, breaks = 20)
  hist(highTT$MeanTT, col= highcols[1], add=T,breaks = 20)
  legend('topright', legend = c('Unfert', 'Res. Fert', 'High Fert'), fill = c(unfertcols[1], rescols[1], highcols[2]))

  hist(unSA$MeanSA, col = unfertcols[1], breaks = 20, ylim = c(0, 250),
       xlim=c(min(unSA$MeanSA, resSA$MeanSA, highSA$MeanSA),
              max(unSA$MeanSA, resSA$MeanSA, highSA$MeanSA)), xlab = 'Mean SA')
  hist(resSA$MeanSA, col= rescols[1],add=T, breaks = 20)
  hist(highSA$MeanSA, col= highcols[1], add=T,breaks = 20)
  legend('topright', legend = c('Unfert', 'Res. Fert', 'High Fert'), fill = c(unfertcols[1], rescols[1], highcols[2]))

  print(ggplot(fertPars, aes(k1,  fill = trial)) + geom_density() +  theme_bw() +
          facet_grid(fertPars$trialStart, scales = 'free_x') +
          scale_fill_manual(values = c(unfertcols[1], rescols[1], highcols[1])))
  print(ggplot(fertPars, aes(k2,  fill = trial)) + geom_density() +  theme_bw() +
          facet_grid(fertPars$trialStart, scales = 'free_x') +
          scale_fill_manual(values = c(unfertcols[1], rescols[1], highcols[1])))
  print(ggplot(fertPars, aes(a21,  fill = trial)) + geom_density()+  theme_bw() +
          facet_grid(fertPars$trialStart, scales = 'free_x') +
          scale_fill_manual(values = c(unfertcols[1], rescols[1], highcols[1])))
  print(ggplot(fertPars, aes(slowProp,  fill = trial)) + geom_density() +  theme_bw() +
          facet_grid(fertPars$trialStart, scales = 'free_x') +
          scale_fill_manual(values = c(unfertcols[1], rescols[1], highcols[1])))

  print(ggplot(fertTTs, aes(MeanTT, fill = name)) + geom_density() + theme_bw() +
          scale_fill_manual(values = c(unfertcols[1], rescols[1], highcols[1])))
  print(ggplot(fertSAs, aes(MeanSA, fill = name)) + geom_density() + theme_bw() +
          scale_fill_manual(values = c(unfertcols[1], rescols[1], highcols[1])))

  print(ggplot(fertp1F, aes(fill = trial, x = p1Size)) + geom_density() + theme_bw() +
          scale_fill_manual(values = c(unfertcols[1], rescols[1], highcols[1])))
  print(ggplot(fertp2F, aes(fill = trial, x = p2Size)) + geom_density() + theme_bw() +
          scale_fill_manual(values = c(unfertcols[1], rescols[1], highcols[1])))


  un100 <- filter(unfertTT3df, years < 100)
  res100 <- filter(resfertTT3df, years < 100)
  high100 <- filter(highfertTT3df, years < 100)

  #Percents under age x
  ages <- c(0,1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 6,7,8,9, 10,11,12,13,14,15, 20,25,
            30, 35, 40, 45, 50, 100, 200,300,400, 500, 600,700,800,900, 1000)

  fertAgeDist <- data.frame(ages, seq(1,length(ages)),seq(1,length(ages)),seq(1,length(ages)))
  colnames(fertAgeDist) <- c("Age",'Unfert', 'Res', 'High')
  y = 0
  for(a in ages){
    y = y + 1
    fertAgeDist$Unfert[y] <-  round(AUC(unfertTT3df$years, unfertTT3df$TTdens, from = 0, to = a, method = 'spline') * 100, 1)
    fertAgeDist$Res[y] <- round(AUC(resfertTT3df$years, resfertTT3df$TTdens, from = 0, to = a, method = 'spline') * 100, 1)
    fertAgeDist$High[y] <- round(AUC(highfertTT3df$years, highfertTT3df$TTdens, from = 0, to = a, method = 'spline') * 100, 1)
  }

  plot(fertAgeDist$Age, fertAgeDist$Unfert, ylim = c(0,100), type = 'l', col = unfertcols[2], lwd = 3, xlim = c(0,10),
       xlab = 'Age', ylab = 'Cumulative C (%)', main = 'Fast C Transit Time Distribution')
  lines(fertAgeDist$Age, fertAgeDist$Res, col = rescols[2], lwd = 3)
  lines(fertAgeDist$Age, fertAgeDist$High, col = highcols[2], lwd = 3)
  legend('bottomright', legend = c('Unfert.', 'Res. Fert.', 'High Fert.'), col=c(unfertcols[2], rescols[2], highcols[2]), lwd = 3)

  plot(fertAgeDist$Age, fertAgeDist$Unfert, ylim = c(0,100), type = 'l', col = unfertcols[2], lwd = 3, log = 'x',
       xlab = 'Age', ylab = 'Cumulative C (%)', main = 'System Transit Time Distribution')
  lines(fertAgeDist$Age, fertAgeDist$Res, col = rescols[2], lwd = 3)
  lines(fertAgeDist$Age, fertAgeDist$High, col = highcols[2], lwd = 3)
  legend('bottomright', legend = c('Unfert.', 'Res. Fert.', 'High Fert.'), col=c(unfertcols[2], rescols[2], highcols[2]), lwd = 3)

  #Calculate 5 and 95th percentile uncertainties for transit times
  # unfertTTdist$Q95 <- apply(unfertTTdist[,!(names(unfertTTdist) %in% 'years')], 1, function(x) quantile(x, probs = 0.95))
  # unfertTTdist$Q05 <- apply(unfertTTdist[,!(names(unfertTTdist) %in% 'years')], 1, function(x) quantile(x, probs = 0.05))
  # unfertTTdist$mean <- apply(unfertTTdist[,!(names(unfertTTdist) %in% 'years')], 1, function(x) mean(x))
  # resfertTTdist$Q95 <- apply(resfertTTdist[,!(names(resfertTTdist) %in% 'years')], 1, function(x) quantile(x, probs = 0.95))
  # resfertTTdist$Q05 <- apply(resfertTTdist[,!(names(resfertTTdist) %in% 'years')], 1, function(x) quantile(x, probs = 0.05))
  # resfertTTdist$mean <- apply(resfertTTdist[,!(names(resfertTTdist) %in% 'years')], 1, function(x) mean(x))
  # highfertTTdist$Q95 <- apply(highfertTTdist[,!(names(highfertTTdist) %in% 'years')], 1, function(x) quantile(x, probs = 0.95))
  # highfertTTdist$Q05 <- apply(highfertTTdist[,!(names(highfertTTdist) %in% 'years')], 1, function(x) quantile(x, probs = 0.05))
  # highfertTTdist$mean <- apply(highfertTTdist[,!(names(highfertTTdist) %in% 'years')], 1, function(x) mean(x))

  #Plot with 95% confidence intervals
  plot(unfertTTdist$years, highfertTTdist$mean - unfertTTdist$mean, type = 'l', col = highcols[2], lwd = 2,
       ylim = c(min(highfertTTdist$Q05 - unfertTTdist$mean, resfertTTdist$Q05 - unfertTTdist$mean),0.008), xlim = c(0, 100),
       ylab = 'Density Difference (Treatment - Control)', xlab = 'Transit Time (Years)',
       main = paste('Treatment Effects on Transit Time Distribution\n', 'Fertilization Trial'))
  polygon(c(unfertTTdist$years, rev(unfertTTdist$years)),c(unfertTTdist$Q95 - unfertTTdist$mean, rev(unfertTTdist$Q05 - unfertTTdist$mean)),
          col = unfertcols[1], border = NA)
  polygon(c(resfertTTdist$years, rev(resfertTTdist$years)), c(resfertTTdist$Q95 - unfertTTdist$mean, rev(resfertTTdist$Q05 - unfertTTdist$mean)),
          col = rescols[1], border = NA)
  polygon(c(highfertTTdist$years, rev(highfertTTdist$years)), c(highfertTTdist$Q05 - unfertTTdist$mean, rev(highfertTTdist$Q95 - unfertTTdist$mean)),
          col = highcols[1], border = NA)
  lines(unfertTTdist$years, highfertTTdist$mean - unfertTTdist$mean,col = highcols[2], lwd = 2)
  lines(unfertTTdist$years, highfertTTdist$Q95 - unfertTTdist$mean, col = highcols[2])
  lines(unfertTTdist$years, highfertTTdist$Q05 - unfertTTdist$mean, col = highcols[2])
  lines(unfertTTdist$years, resfertTTdist$mean - unfertTTdist$mean, col = rescols[2], lwd = 2)
  lines(unfertTTdist$years, resfertTTdist$Q95 - unfertTTdist$mean, col = rescols[2])
  lines(unfertTTdist$years, resfertTTdist$Q05 - unfertTTdist$mean, col = rescols[2])
  lines(unfertTTdist$years, unfertTTdist$mean - unfertTTdist$mean, col = unfertcols[2], lwd =2)
  lines(unfertTTdist$years, unfertTTdist$Q95 - unfertTTdist$mean, col = unfertcols[2])
  lines(unfertTTdist$years, unfertTTdist$Q05 - unfertTTdist$mean, col = unfertcols[2])
  legend('topright', legend = c('Unfert.', 'Res. Fert', 'High Fert'), lwd = 3, col = c(unfertcols[2], rescols[2], highcols[2]))

  #Converted to mass
  plot(unfertTTdist$years, ((highfertTTdist$mean * fertInputs[3])*100 - (unfertTTdist$mean * fertInputs[1])*100), type = 'l', col = highcols[2], lwd = 2,
       ylim = c(min(highfertTTdist$Q05 * fertInputs[3] - unfertTTdist$mean * fertInputs[1],
                    resfertTTdist$Q05 * fertInputs[2] - unfertTTdist$mean * fertInputs[1])*100, (max(highfertTTdist$Q95) * 100 * fertInputs[3] - max(unfertTTdist$mean) * 100 * fertInputs[1])),
       xlim = c(0, 30), ylab = 'Transit Time Difference (Treatment - Control) (g C m-2)', xlab = 'Transit Time (Years)',
       main = paste('Treatment Effects on Transit Time Distribution\n', 'Fertilization Trial'))
  polygon(c(unfertTTdist$years, rev(unfertTTdist$years)),c((unfertTTdist$Q95 * fertInputs[1] *100) - (unfertTTdist$mean * fertInputs[1] * 100), rev((unfertTTdist$Q05 * fertInputs[1]*100) - (unfertTTdist$mean * fertInputs[1] * 100))),
          col = unfertcols[1], border = NA)
  polygon(c(resfertTTdist$years, rev(resfertTTdist$years)), c((resfertTTdist$Q95 * fertInputs[2] * 100) - (unfertTTdist$mean * fertInputs[1] *100), rev((resfertTTdist$Q05 * fertInputs[2] *100) - (unfertTTdist$mean * fertInputs[1]*100))),
          col = rescols[1], border = NA)
  polygon(c(highfertTTdist$years, rev(highfertTTdist$years)), c((highfertTTdist$Q05 * fertInputs[3] * 100) - (unfertTTdist$mean * fertInputs[1] * 100), rev((highfertTTdist$Q95 * fertInputs[3] * 100) - (unfertTTdist$mean * fertInputs[1] * 100))),
          col = highcols[1], border = NA)
  lines(unfertTTdist$years, (highfertTTdist$mean * fertInputs[3] - unfertTTdist$mean * fertInputs[1])*100,col = highcols[2], lwd = 2)
  #lines(unfertTTdist$years, (highfertTTdist$Q95 * fertInputs[3] - unfertTTdist$mean * fertInputs[1])*100, col = highcols[2])
  #lines(unfertTTdist$years, (highfertTTdist$Q05 * fertInputs[3] - unfertTTdist$mean * fertInputs[1])*100, col = highcols[2])
  lines(unfertTTdist$years, (resfertTTdist$mean * fertInputs[2] - unfertTTdist$mean * fertInputs[1])*100, col = rescols[2], lwd = 2)
  #lines(unfertTTdist$years, (resfertTTdist$Q95 * fertInputs[2] - unfertTTdist$mean * fertInputs[1])*100, col = rescols[2])
  #lines(unfertTTdist$years, (resfertTTdist$Q05 * fertInputs[2] - unfertTTdist$mean * fertInputs[1])*100, col = rescols[2])
  lines(unfertTTdist$years, (unfertTTdist$mean * fertInputs[1] - unfertTTdist$mean * fertInputs[1])*100, col = unfertcols[2], lwd =2)
  #lines(unfertTTdist$years, (unfertTTdist$Q95 * fertInputs[1] - unfertTTdist$mean * fertInputs[1])*100, col = unfertcols[2])
  #lines(unfertTTdist$years, (unfertTTdist$Q05 * fertInputs[1] - unfertTTdist$mean * fertInputs[1])*100, col = unfertcols[2])
  legend('topright', legend = c('Unfert.', 'Res. Fert', 'High Fert'), lwd = 3, col = c(unfertcols[2], rescols[2], highcols[2]))

  #By mass - Quarantined because error bounds are calculated from control uncertainty, not control mean
  # plot(unfertTTdist$years, (highfertTTdist$mean  * fertInputs[3] - unfertTTdist$mean  * fertInputs[1]) * 100, type = 'l', col = highcols[2], lwd = 2,
  #      ylim = c(min(irr20TTdist$Q05 - unfertTTdist$mean, irr10TTdist$Q05 - unfertTTdist$mean)*100,0.56*100), xlim = c(0, 30),
  #      ylab = 'Mass Difference (Treatment - Control) (g C m-2)', xlab = 'Transit Time (Years)', main = 'Carbon Flux Transit Time :: Fertilization :: With 95% CI')
  # polygon(c(unfertTTdist$years, rev(unfertTTdist$years)),c(unfertTTdist$Q95  * fertInputs[1] - unfertTTdist$mean  * fertInputs[1], rev(unfertTTdist$Q05  * fertInputs[1] - unfertTTdist$mean  * fertInputs[1])) * 100,
  #         col = unfertcols[1], border = NA)
  # polygon(c(resfertTTdist$years, rev(resfertTTdist$years)), c(resfertTTdist$Q95 * fertInputs[2] - unfertTTdist$mean * fertInputs[1], rev(resfertTTdist$Q05 * fertInputs[2] - unfertTTdist$mean * fertInputs[1])) * 100,
  #         col = rescols[1], border = NA)
  # polygon(c(highfertTTdist$years, rev(highfertTTdist$years)), c(highfertTTdist$Q05 * irrInputs[3] - unfertTTdist$mean * irrInputs[1], rev(highfertTTdist$Q95 * fertInputs[3] - unfertTTdist$mean * fertInputs[1])) * 100,
  #         col = highcols[1], border = NA)
  # #lines(unfertTTdist$years, (unfertTTdist$Q95 * irrInputs[1] - unfertTTdist$mean * irrInputs[1]) * 100, col = uncols[2])
  # #lines(dryTTdist$years, (dryTTdist$Q05 * irrInputs[1] - dryTTdist$mean * irrInputs[1]) * 100, col = drycols[2])
  # lines(unfertTTdist$years, (unfertTTdist$mean * fertInputs[1] - unfertTTdist$mean * fertInputs[1]) * 100, col = unfertcols[2], lwd =3)
  # #lines(dryTTdist$years, (irr20TTdist$Q95 * irrInputs[3] - dryTTdist$mean * irrInputs[1]) * 100, col = irr20cols[2])
  # #lines(dryTTdist$years, (irr20TTdist$Q05 * irrInputs[3] - dryTTdist$mean * irrInputs[1]) * 100, col = irr20cols[2])
  # lines(dryTTdist$years, (highfertTTdist$mean * fertInputs[3] - unfertTTdist$mean * fertInputs[1]) * 100,col = highcols[2], lwd = 3)
  # #lines(dryTTdist$years, (irr10TTdist$Q95 * irrInputs[2] - dryTTdist$mean * irrInputs[1]) * 100, col = irr10cols[2])
  # #lines(dryTTdist$years, (irr10TTdist$Q05 * irrInputs[2] - dryTTdist$mean * irrInputs[1]) * 100, col = irr10cols[2])
  # lines(dryTTdist$years, (resfertTTdist$mean * fertInputs[2] - unfertTTdist$mean * fertInputs[1]) * 100, col = rescols[2], lwd = 3)
  # # lines(dryTTdist$years, dryTTdist$Q95 * irrInputs[1] - dryTTdist$mean * irrInputs[1], col = drycols[2])
  # # lines(dryTTdist$years, dryTTdist$Q05 * irrInputs[1] - dryTTdist$mean * irrInputs[1], col = drycols[2])
  # legend('topright', legend = c('Unfert.', 'Res. Fert', 'High Fert'), col=c(unfertcols[2], rescols[2], highcols[2]),
  #        fill = c(unfertcols[1], rescols[1], highcols[1]), lwd = 3)

  plot(highfertTTdist$years, highfertTTdist$mean * fertInputs[3] * 100, type = 'l', xlim = c(0,30), col = highcols[2], lwd = 3,
       ylab = 'g C m-2', xlab = 'Years', main = 'Transit Time Distribution :: Fertilization')
  lines(resfertTTdist$years, resfertTTdist$mean * fertInputs[2] * 100, col = rescols[2], lwd = 3)
  lines(unfertTTdist$years, unfertTTdist$mean * fertInputs[1] * 100, col = unfertcols[2], lwd = 3)
  legend('topright', legend = c('Unfert.', 'Res. Fert', 'High Fert'), lwd = 3, col = c(unfertcols[2], rescols[2], highcols[2]))

  plot(highfertTTdist$years, highfertTTdist$mean * fertInputs[3] * 100, type = 'l', xlim = c(0,30), col = highcols[2], lwd = 3,
       ylab = 'g C m-2', xlab = 'Years', main = 'Transit Time Distribution :: Fertilization', ylim = c(0, max(highfertTTdist$mean * fertInputs[3] * 100) + 10))
  polygon(c(unfertTTdist$years, rev(unfertTTdist$years)),c(unfertTTdist$Q95  * fertInputs[1], rev(unfertTTdist$Q05  * fertInputs[1])) * 100,
          col = unfertcols[1], border = NA)
  polygon(c(resfertTTdist$years, rev(resfertTTdist$years)),c(resfertTTdist$Q95  * fertInputs[2], rev(resfertTTdist$Q05  * fertInputs[2])) * 100,
          col = rescols[1], border = NA)
  polygon(c(highfertTTdist$years, rev(highfertTTdist$years)),c(highfertTTdist$Q95  * fertInputs[3], rev(highfertTTdist$Q05  * fertInputs[3])) * 100,
          col = highcols[1], border = NA)
  lines(highfertTTdist$years, highfertTTdist$mean * fertInputs[3] * 100, col = highcols[2], lwd = 3)
  lines(resfertTTdist$years, resfertTTdist$mean * fertInputs[2] * 100, col = rescols[2], lwd = 3)
  lines(unfertTTdist$years, unfertTTdist$mean * fertInputs[1] * 100, col =unfertcols[2], lwd = 3)
  legend('topright', legend = c('Unfert.', 'Res. Fert', 'High Fert'), col=c(unfertcols[2], rescols[2], highcols[2]),
         fill = c(unfertcols[1], rescols[1], highcols[1]), lwd = 3)

  plot(highfertTTdist$years, ((highfertTTdist$mean - unfertTTdist$mean)/unfertTTdist$mean) * 100, type = 'l', col = highcols[2], lwd = 3,
       xlab = 'Transit Time (years)', ylab = 'Percent Difference', main = 'Fertilization Treatment Effects on Transit Time', , xlim = c(0,500), ylim = c(-100, 250))
  # ylim = c(min(((highfertTTdist$mean - unfertTTdist$mean)/unfertTTdist$mean) * 100, ((resfertTTdist$mean - unfertTTdist$mean)/unfertTTdist$mean) * 100),
  #          max(((highfertTTdist$mean - unfertTTdist$mean)/unfertTTdist$mean) * 100, ((resfertTTdist$mean - unfertTTdist$mean)/unfertTTdist$mean) * 100)))
  # polygon(c(unfertTTdist$years, rev(unfertTTdist$years)),c(unfertTTdist$Q95  * fertInputs[1] * 100, rev(unfertTTdist$Q05  * fertInputs[1] * 100)),
  #         col = unfertcols[1], border = NA)
  # polygon(c(resfertTTdist$years, rev(resfertTTdist$years)),c((resfertTTdist$Q95 * fertInputs[2] - unfertTTdist$mean * fertInputs[1])/(unfertTTdist$mean) *100,
  #                                                            rev((resfertTTdist$Q05  * fertInputs[2] - unfertTTdist$mean*fertInputs[1])/(unfertTTdist$mean) * 100)),
  #         col = rescols[1], border = NA)
  # polygon(c(highfertTTdist$years, rev(highfertTTdist$years)),c(highfertTTdist$Q95  * fertInputs[3], rev(highfertTTdist$Q05  * fertInputs[3])) * 100,
  #         col = highcols[1], border = NA)
  abline(,,0, lwd = 3, col = unfertcols[2], lty = 2)
  lines(resfertTTdist$years, ((resfertTTdist$mean - unfertTTdist$mean)/unfertTTdist$mean) * 100, col = rescols[2], lwd = 3)
  legend('topright', legend = c('Unfert.', 'Res. Fert', 'High Fert'), lwd = 3, col = c(unfertcols[2], rescols[2], highcols[2]))


  #Calculate 5 and 95th percentile uncertainties
  unfertSAdist$Q95 <- apply(unfertSAdist[,!(names(unfertSAdist) %in% c('years', 'mean', 'Q05', 'Q95','trial','trialStart'))], 1, function(x) quantile(x, probs = 0.95))
  unfertSAdist$Q05 <- apply(unfertSAdist[,!(names(unfertSAdist) %in% c('years', 'mean', 'Q05', 'Q95','trial','trialStart'))], 1, function(x) quantile(x, probs = 0.05))
  unfertSAdist$mean <- apply(unfertSAdist[,!(names(unfertSAdist) %in% c('years', 'mean', 'Q05', 'Q95','trial','trialStart'))], 1, function(x) mean(x))
  resfertSAdist$Q95 <- apply(resfertSAdist[,!(names(resfertSAdist) %in% c('years', 'mean', 'Q05', 'Q95','trial','trialStart'))], 1, function(x) quantile(x, probs = 0.95))
  resfertSAdist$Q05 <- apply(resfertSAdist[,!(names(resfertSAdist) %in% c('years', 'mean', 'Q05', 'Q95','trial','trialStart'))], 1, function(x) quantile(x, probs = 0.05))
  resfertSAdist$mean <- apply(resfertSAdist[,!(names(resfertSAdist) %in% c('years', 'mean', 'Q05', 'Q95','trial','trialStart'))], 1, function(x) mean(x))
  highfertSAdist$Q95 <- apply(highfertSAdist[,!(names(highfertSAdist) %in% c('years', 'mean', 'Q05', 'Q95','trial','trialStart'))], 1, function(x) quantile(x, probs = 0.95))
  highfertSAdist$Q05 <- apply(highfertSAdist[,!(names(highfertSAdist) %in% c('years', 'mean', 'Q05', 'Q95','trial','trialStart'))], 1, function(x) quantile(x, probs = 0.05))
  highfertSAdist$mean <- apply(highfertSAdist[,!(names(highfertSAdist) %in% c('years', 'mean', 'Q05', 'Q95','trial','trialStart'))], 1, function(x) mean(x))

  #Plot with 95% confidence intervals
  plot(unfertSAdist$years, highfertSAdist$mean - unfertSAdist$mean, type = 'l', col = highcols[2], lwd = 2,
       ylim = c(min(unfertSAdist$Q05 - unfertSAdist$mean, resfertSAdist$Q05 - unfertSAdist$mean),
                max(highfertSAdist$Q95 - unfertSAdist$mean)), xlim = c(0, 100),
       ylab = 'Density Difference (Treatment - Control)', xlab = 'System Age (Years)',
       main = paste('Treatment Effects on System Age Distribution\n', 'Fertilization Trial'))
  polygon(c(unfertSAdist$years, rev(unfertSAdist$years)),c(unfertSAdist$Q95 - unfertSAdist$mean, rev(unfertSAdist$Q05 - unfertSAdist$mean)),
          col = unfertcols[1], border = NA)
  polygon(c(resfertSAdist$years, rev(resfertSAdist$years)), c(resfertSAdist$Q95 - unfertSAdist$mean, rev(resfertSAdist$Q05 - unfertSAdist$mean)),
          col = rescols[1], border = NA)
  polygon(c(highfertSAdist$years, rev(highfertSAdist$years)), c(highfertSAdist$Q05 - unfertSAdist$mean, rev(highfertSAdist$Q95 - unfertSAdist$mean)),
          col = highcols[1], border = NA)
  lines(unfertSAdist$years, highfertSAdist$mean - unfertSAdist$mean,col = highcols[2], lwd = 2)
  lines(unfertSAdist$years, highfertSAdist$Q95 - unfertSAdist$mean, col = highcols[2])
  lines(unfertSAdist$years, highfertSAdist$Q05 - unfertSAdist$mean, col = highcols[2])
  lines(unfertSAdist$years, resfertSAdist$mean - unfertSAdist$mean, col = rescols[2], lwd = 2)
  lines(unfertSAdist$years, resfertSAdist$Q95 - unfertSAdist$mean, col = rescols[2])
  lines(unfertSAdist$years, resfertSAdist$Q05 - unfertSAdist$mean, col = rescols[2])
  lines(unfertSAdist$years, unfertSAdist$mean - unfertSAdist$mean, col = unfertcols[2], lwd =2)
  lines(unfertSAdist$years, unfertSAdist$Q95 - unfertSAdist$mean, col = unfertcols[2])
  lines(unfertSAdist$years, unfertSAdist$Q05 - unfertSAdist$mean, col = unfertcols[2])
  legend('topright', legend = c('Unfert.', 'Res. Fert', 'High Fert'), lwd = 3, col = c(unfertcols[2], rescols[2], highcols[2]))

  #Converted to mass - Not meaningful with Inputs... possibly with C stock?
  # plot(unfertSAdist$years, ((highfertSAdist$mean * fertInputs[3])*100 - (unfertSAdist$mean * fertInputs[1])*100), type = 'l', col = highcols[2], lwd = 2,
  #      ylim = c(min(unfertSAdist$Q05 * fertInputs[1] - unfertSAdist$mean * fertInputs[1],
  #                   resfertSAdist$Q05 * fertInputs[2] - unfertSAdist$mean * fertInputs[1])*100, (max(highfertSAdist$Q95) * 100 * fertInputs[3] - max(unfertSAdist$mean) * 100 * fertInputs[1])),
  #      xlim = c(0, 30), ylab = 'System Age Difference (Treatment - Control) (g C m-2)', xlab = 'Transit Time (Years)',
  #      main = paste('Treatment Effects on System Age Distribution\n', 'Fertilization Trial'))
  # polygon(c(unfertSAdist$years, rev(unfertSAdist$years)),c((unfertSAdist$Q95 * fertInputs[1] *100) - (unfertSAdist$mean * fertInputs[1] * 100), rev((unfertSAdist$Q05 * fertInputs[1]*100) - (unfertSAdist$mean * fertInputs[1] * 100))),
  #         col = unfertcols[1], border = NA)
  # polygon(c(resfertSAdist$years, rev(resfertSAdist$years)), c((resfertSAdist$Q95 * fertInputs[2] * 100) - (unfertSAdist$mean * fertInputs[1] *100), rev((resfertSAdist$Q05 * fertInputs[2] *100) - (unfertSAdist$mean * fertInputs[1]*100))),
  #         col = rescols[1], border = NA)
  # polygon(c(highfertSAdist$years, rev(highfertSAdist$years)), c((highfertSAdist$Q05 * fertInputs[3] * 100) - (unfertSAdist$mean * fertInputs[1] * 100), rev((highfertSAdist$Q95 * fertInputs[3] * 100) - (unfertSAdist$mean * fertInputs[1] * 100))),
  #         col = highcols[1], border = NA)
  # lines(unfertSAdist$years, (highfertSAdist$mean * fertInputs[3] - unfertSAdist$mean * fertInputs[1])*100,col = highcols[2], lwd = 2)
  # #lines(unfertSAdist$years, (highfertSAdist$Q95 * fertInputs[3] - unfertSAdist$mean * fertInputs[1])*100, col = highcols[2])
  # #lines(unfertSAdist$years, (highfertSAdist$Q05 * fertInputs[3] - unfertSAdist$mean * fertInputs[1])*100, col = highcols[2])
  # lines(unfertSAdist$years, (resfertSAdist$mean * fertInputs[2] - unfertSAdist$mean * fertInputs[1])*100, col = rescols[2], lwd = 2)
  # #lines(unfertSAdist$years, (resfertSAdist$Q95 * fertInputs[2] - unfertSAdist$mean * fertInputs[1])*100, col = rescols[2])
  # #lines(unfertSAdist$years, (resfertSAdist$Q05 * fertInputs[2] - unfertSAdist$mean * fertInputs[1])*100, col = rescols[2])
  # lines(unfertSAdist$years, (unfertSAdist$mean * fertInputs[1] - unfertSAdist$mean * fertInputs[1])*100, col = unfertcols[2], lwd =2)
  # #lines(unfertSAdist$years, (unfertSAdist$Q95 * fertInputs[1] - unfertSAdist$mean * fertInputs[1])*100, col = unfertcols[2])
  # #lines(unfertSAdist$years, (unfertSAdist$Q05 * fertInputs[1] - unfertSAdist$mean * fertInputs[1])*100, col = unfertcols[2])
  # legend('topright', legend = c('Unfert.', 'Res. Fert', 'High Fert'), lwd = 3, col = c(unfertcols[2], rescols[2], highcols[2]))


  # plot(highfertSAdist$years, highfertSAdist$mean * fertInputs[3] * 100, type = 'l', xlim = c(0,30), col = highcols[2], lwd = 3,
  #      ylab = 'g C m-2', xlab = 'Years', main = 'System Age Distribution :: Fertilization')
  # lines(resfertSAdist$years, resfertSAdist$mean * fertInputs[2] * 100, col = rescols[2], lwd = 3)
  # lines(unfertSAdist$years, unfertSAdist$mean * fertInputs[1] * 100, col = unfertcols[2], lwd = 3)
  # legend('topright', legend = c('Unfert.', 'Res. Fert', 'High Fert'), lwd = 3, col = c(unfertcols[2], rescols[2], highcols[2]))

  plot(highfertSAdist$years, ((highfertSAdist$mean - unfertSAdist$mean)/unfertSAdist$mean) * 100, type = 'l', col = highcols[2], lwd = 3,
       xlab = 'System Age (years)', ylab = 'Percent Difference', main = 'Fertilization Treatment Effects on System Age', , xlim = c(0,1000),
       ylim = c(min((highfertSAdist$mean - unfertSAdist$mean)/unfertSAdist$mean)*100, max(((highfertSAdist$mean - unfertSAdist$mean)/unfertSAdist$mean)*100)))
  # ylim = c(min(((highfertSAdist$mean - unfertSAdist$mean)/unfertSAdist$mean) * 100, ((resfertSAdist$mean - unfertSAdist$mean)/unfertSAdist$mean) * 100),
  #          max(((highfertSAdist$mean - unfertSAdist$mean)/unfertSAdist$mean) * 100, ((resfertSAdist$mean - unfertSAdist$mean)/unfertSAdist$mean) * 100)))
  # polygon(c(unfertSAdist$years, rev(unfertSAdist$years)),c(unfertSAdist$Q95  * fertInputs[1] * 100, rev(unfertSAdist$Q05  * fertInputs[1] * 100)),
  #         col = unfertcols[1], border = NA)
  # polygon(c(resfertSAdist$years, rev(resfertSAdist$years)),c((resfertSAdist$Q95 * fertInputs[2] - unfertSAdist$mean * fertInputs[1])/(unfertSAdist$mean) *100,
  #                                                            rev((resfertSAdist$Q05  * fertInputs[2] - unfertSAdist$mean*fertInputs[1])/(unfertSAdist$mean) * 100)),
  #         col = rescols[1], border = NA)
  # polygon(c(highfertSAdist$years, rev(highfertSAdist$years)),c(highfertSAdist$Q95  * fertInputs[3], rev(highfertSAdist$Q05  * fertInputs[3])) * 100,
  #         col = highcols[1], border = NA)
  abline(,,0, lwd = 3, col = unfertcols[2], lty = 2)
  lines(resfertSAdist$years, ((resfertSAdist$mean - unfertSAdist$mean)/unfertSAdist$mean) * 100, col = rescols[2], lwd = 3)
  legend('topright', legend = c('Unfert.', 'Res. Fert', 'High Fert'), lwd = 3, col = c(unfertcols[2], rescols[2], highcols[2]))

  plot(highfertSAdist$years, ((highfertSAdist$mean - unfertSAdist$mean)/unfertSAdist$mean) * 100, type = 'l', col = highcols[2], lwd = 3,
       xlab = 'System Age (years)', ylab = 'Percent Difference', main = 'Fertilization Treatment Effects on System Age', , xlim = c(0,200),
       ylim = c(min((highfertSAdist$mean - unfertSAdist$mean)/unfertSAdist$mean)*100, max(((highfertSAdist$mean - unfertSAdist$mean)/unfertSAdist$mean)*100)))
  # ylim = c(min(((highfertSAdist$mean - unfertSAdist$mean)/unfertSAdist$mean) * 100, ((resfertSAdist$mean - unfertSAdist$mean)/unfertSAdist$mean) * 100),
  #          max(((highfertSAdist$mean - unfertSAdist$mean)/unfertSAdist$mean) * 100, ((resfertSAdist$mean - unfertSAdist$mean)/unfertSAdist$mean) * 100)))
  # polygon(c(unfertSAdist$years, rev(unfertSAdist$years)),c(unfertSAdist$Q95  * fertInputs[1] * 100, rev(unfertSAdist$Q05  * fertInputs[1] * 100)),
  #         col = unfertcols[1], border = NA)
  # polygon(c(resfertSAdist$years, rev(resfertSAdist$years)),c((resfertSAdist$Q95 * fertInputs[2] - unfertSAdist$mean * fertInputs[1])/(unfertSAdist$mean) *100,
  #                                                            rev((resfertSAdist$Q05  * fertInputs[2] - unfertSAdist$mean*fertInputs[1])/(unfertSAdist$mean) * 100)),
  #         col = rescols[1], border = NA)
  # polygon(c(highfertSAdist$years, rev(highfertSAdist$years)),c(highfertSAdist$Q95  * fertInputs[3], rev(highfertSAdist$Q05  * fertInputs[3])) * 100,
  #         col = highcols[1], border = NA)
  abline(,,0, lwd = 3, col = unfertcols[2], lty = 2)
  lines(resfertSAdist$years, ((resfertSAdist$mean - unfertSAdist$mean)/unfertSAdist$mean) * 100, col = rescols[2], lwd = 3)
  legend('topright', legend = c('Unfert.', 'Res. Fert', 'High Fert'), lwd = 3, col = c(unfertcols[2], rescols[2], highcols[2]))

  plot(highfertSAdist$years, highfertSAdist$mean, col = highcols[2], lwd = 3, type = 'l', xlim= c(0,50), xlab = 'Years',
       ylab = 'Density', main = 'Fertilizer Effects on System C Age Distribution',
       ylim = c(0, max(highfertSAdist$Q95)))
  polygon(c(highfertSAdist$years, rev(highfertSAdist$years)), c(highfertSAdist$Q95, rev(highfertSAdist$Q05)), col = highcols[1], border = NA)
  polygon(c(resfertSAdist$years, rev(resfertSAdist$years)), c(resfertSAdist$Q95, rev(resfertSAdist$Q05)), col = rescols[1], border = NA)
  polygon(c(unfertSAdist$years, rev(unfertSAdist$years)), c(unfertSAdist$Q95, rev(unfertSAdist$Q05)), col = unfertcols[1], border = NA)
  lines(highfertSAdist$years, highfertSAdist$mean, col = highcols[2], lwd = 3)
  lines(resfertSAdist$years, resfertSAdist$mean, col = rescols[2], lwd = 3)
  lines(unfertSAdist$years, unfertSAdist$mean, col = unfertcols[2], lwd = 3)
  legend('topright', legend = c('Unfert.', 'Res. Fert', 'High Fert'), lwd = 3, col = c(unfertcols[2], rescols[2], highcols[2]))

  unSAsig <- filter(unfertBayesDynamics, Parameter == 'MeanSA') %>% filter(Value > quantile(Value, 0.1) & Value < quantile(Value, 0.9))
  resSAsig <- filter(resfertBayesDynamics, Parameter == 'MeanSA') %>% filter(Value > quantile(Value, 0.1) & Value < quantile(Value, 0.9))
  highSAsig <- filter(highfertBayesDynamics, Parameter == 'MeanSA') %>% filter(Value > quantile(Value, 0.1) & Value < quantile(Value, 0.9))

  plot(highfertSAdist$years, highfertSAdist$mean, col = highcols[2], lwd = 3, type = 'l', xlim = c(0,50))
  lines(resfertSAdist$years, resfertSAdist$mean, col = rescols[2], lwd =3)
  lines(unfertSAdist$years, unfertSAdist$mean, col = unfertcols[2], lwd = 3)

  irrTTclips$Trial = "Irrigation"
  fertTTclips$Trial = "Fertilizer"

  irrSAclips$Trial = "Irrigation"
  fertSAclips$Trial = "Fertilizer"

  allTTs <- rbind(irrTTclips, fertTTclips)
  allSAs <- rbind(irrSAclips, fertSAclips)

  allTTs$Trial <- factor(allTTs$Trial, levels = c('Irrigation', "Fertilizer"), ordered = TRUE)
  allSAs$Trial <- factor(allSAs$Trial, levels = c('Irrigation', "Fertilizer"), ordered = TRUE)
  allTTs$name <- factor(allTTs$name, levels = c('Dry', "Irr. 10", "Irr. 20", "Unfert","Res. Fert","High Fert"), ordered = TRUE)
  allSAs$name <- factor(allSAs$name, levels = c('Dry', "Irr. 10", "Irr. 20", "Unfert","Res. Fert","High Fert"), ordered = TRUE)

  print(ggplot2::ggplot(allTTs, ggplot2::aes(x=name, y = MeanTT)) + theme_bw() +
          facet_wrap(allTTs$Trial ~ ., scales = "free_x") + scale_y_continuous(limits = c(0, NA)) +
          ggplot2::geom_boxplot(fill = c(drycols[2], irr10cols[2],irr20cols[2], unfertcols[2], rescols[2], highcols[2]), notch = TRUE) +
          ggplot2::ggtitle(paste('All Trial TT :: With Outliers Removed')) + ggplot2::ylab('Years') + #ylim(0,limy[2]) +
          guides(fill=guide_legend(title="Trial")) + xlab('') +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                strip.text.x = element_text(size = 12, face = "bold"),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))


  print(ggplot2::ggplot(allSAs, ggplot2::aes(x=name, y = MeanSA)) + theme_bw() +
          facet_wrap(allSAs$Trial ~ ., scales = "free_x") + scale_y_continuous(limits = c(0, NA)) +
          ggplot2::geom_boxplot(fill = c(drycols[2], irr10cols[2],irr20cols[2], unfertcols[2], rescols[2], highcols[2]), notch = TRUE) +
          ggplot2::ggtitle(paste('All Trial SA :: With Outliers Removed')) + ggplot2::ylab('Years') + #ylim(0,limy[2]) +
          guides(fill=guide_legend(title="Trial")) + xlab('') +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                strip.text.x = element_text(size = 12, face = "bold"),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")))

  #Final pool sizes
  unp1F <- data.frame(t(unfertPoolOutputs$Ct1[trialEnd - trialStart + 1,]), 'Unfert')
  colnames(unp1F) <- c('p1Size', 'trial')
  resp1F <- data.frame(t(resfertPoolOutputs$Ct1[trialEnd - trialStart + 1,]), 'Res. Fert')
  colnames(resp1F) <- c('p1Size', 'trial')
  highp1F <- data.frame(t(highfertPoolOutputs$Ct1[trialEnd - trialStart + 1,]), 'High Fert')
  colnames(highp1F) <- c('p1Size', 'trial')
  fertp1F <- rbind(unp1F, resp1F, highp1F)

  unp2F <- data.frame(t(unfertPoolOutputs$Ct2[trialEnd - trialStart + 1,]), 'Unfert')
  colnames(unp2F) <- c('p2Size', 'trial')
  resp2F <- data.frame(t(resfertPoolOutputs$Ct2[trialEnd - trialStart + 1,]), 'Res. Fert')
  colnames(resp2F) <- c('p2Size', 'trial')
  highp2F <- data.frame(t(highfertPoolOutputs$Ct2[trialEnd - trialStart + 1,]), 'High Fert')
  colnames(highp2F) <- c('p2Size', 'trial')
  fertp2F <- rbind(unp2F, resp2F, highp2F)

  ggplot(fertp1F, aes(trial, p1Size)) + geom_boxplot()
  ggplot(fertp2F, aes(trial, p2Size)) + geom_boxplot()

  print(ggplot2::ggplot(fertp1F, aes(fertp1F$trial, fertp1F$p1Size)) +  theme_bw() +
          ggplot2::geom_boxplot(fill = c(unfertcols[2], rescols[2], highcols[2]), notch = TRUE) +
          ggplot2::ggtitle(paste('Fertilizer P1 Size')) + #ylim(0, limy[2]) +
          guides(fill=guide_legend(title="Trial")) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                strip.text.x = element_text(size = 12, face = "bold.italic"),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")) +
          ggplot2::xlab('') + ggplot2::ylab('C Stock (T C ha-1)'))

  print(ggplot2::ggplot(fertp2F, aes(fertp2F$trial, fertp2F$p2Size)) +  theme_bw() +
          ggplot2::geom_boxplot(fill = c(unfertcols[2], rescols[2], highcols[2]), notch = TRUE) +
          ggplot2::ggtitle(paste('Fertilizer P2 Size')) + #ylim(0, limy[2]) +
          guides(fill=guide_legend(title="Trial")) +
          theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                strip.text.x = element_text(size = 12, face = "bold.italic"),
                legend.title = element_text(size = 14, face = 'bold'),
                plot.title = element_text(size = 14, face = 'bold'),
                axis.title=element_text(size=14,face="bold")) +
          ggplot2::xlab('') + ggplot2::ylab('C Stock (T C ha-1)'))

  ggplot(fertPars, aes(k1,  fill = trial)) + geom_density() + theme_bw() +
    scale_fill_manual(values = c(unfertcols[1], rescols[1], highcols[1]))
  ggplot(fertPars, aes(k2,  fill = trial)) + geom_density() + theme_bw() +
    scale_fill_manual(values = c(unfertcols[1], rescols[1], highcols[1]))
  ggplot(fertPars, aes(a21,  fill = trial)) + geom_density() + theme_bw() +
    scale_fill_manual(values = c(unfertcols[1], rescols[1], highcols[1]))
  ggplot(fertPars, aes(slowProp,  fill = trial)) + geom_density() + theme_bw() +
    scale_fill_manual(values = c(unfertcols[1], rescols[1], highcols[1]))

  ggplot(fertTTs, aes(MeanTT, fill = name)) + geom_density() +
    scale_fill_manual(values = c(unfertcols[1], rescols[1], highcols[1]))
  ggplot(fertSAs, aes(MeanSA, fill = name)) + geom_density() +
    scale_fill_manual(values = c(unfertcols[1], rescols[1], highcols[1]))

  ggplot(fertp1F, aes(fill = trial, x = p1Size)) + geom_density() +
    scale_fill_manual(values = c(unfertcols[1], rescols[1], highcols[1]))
  ggplot(fertp2F, aes(fill = trial, x = p2Size)) + geom_density() +
    scale_fill_manual(values = c(unfertcols[1], rescols[1], highcols[1]))


  #Sankey Diagrams
  #Fertilizer
  clipList <- c('Unfert', 'High Fert', 'Dry', 'Irr. 20')

  if(trialStart == 1958){
    Cfit = Cwindow(soilCun, 1959, trialStart, trialEnd)
    Cmod0u = filter(Cfit[,1:2], years == 1959)$predC
  } else {
    Cfit = Cwindow(soilCun, trialStart, trialStart - 2, trialEnd)
    Cmod0u = filter(Cfit[,1:2], years == trialStart)$predC
  }

  if(trialStart == 1958){
    Cfit = Cwindow(soilCres, 1959, trialStart, trialEnd)
    Cmod0r = filter(Cfit[,1:2], years == 1959)$predC
  } else {
    Cfit = Cwindow(soilCres, trialStart, trialStart - 2, trialEnd)
    Cmod0r = filter(Cfit[,1:2], years == trialStart)$predC
  }

  if(trialStart == 1958){
    Cfit = Cwindow(soilChigh, 1959, trialStart, trialEnd)
    Cmod0h = filter(Cfit[,1:2], years == 1959)$predC
  } else {
    Cfit = Cwindow(soilChigh, trialStart, trialStart - 2, trialEnd)
    Cmod0h = filter(Cfit[,1:2], years == trialStart)$predC
  }

  if(trialStart == 1958){
    Cfit = Cwindow(soilCdry, 1959, trialStart, trialEnd)
    Cmod0d = filter(Cfit[,1:2], years == 1959)$predC
  } else {
    Cfit = Cwindow(soilCdry, trialStart, trialStart - 2, trialEnd)
    Cmod0d = filter(Cfit[,1:2], years == trialStart)$predC
  }

  if(trialStart == 1958){
    Cfit = Cwindow(soilC10, 1959, trialStart, trialEnd)
    Cmod010 = filter(Cfit[,1:2], years == 1959)$predC
  } else {
    Cfit = Cwindow(soilC10, trialStart, trialStart - 2, trialEnd)
    Cmod010 = filter(Cfit[,1:2], years == trialStart)$predC
  }

  if(trialStart == 1958){
    Cfit = Cwindow(soilC20, 1959, trialStart, trialEnd)
    Cmod020 = filter(Cfit[,1:2], years == 1959)$predC
  } else {
    Cfit = Cwindow(soilC20, trialStart, trialStart - 2, trialEnd)
    Cmod020 = filter(Cfit[,1:2], years == trialStart)$predC
  }

  t0z <- c(Cmod0u, Cmod0h, Cmod0d, Cmod020)
  k1 <- c(unfertWinchPars[1], highfertWinchPars[1], dryWinchPars[1], irr20WinchPars[1])
  k2 <- c(unfertWinchPars[2], highfertWinchPars[2], dryWinchPars[2], irr20WinchPars[2])
  a21 <- c(unfertWinchPars[3], highfertWinchPars[3], dryWinchPars[3], irr20WinchPars[3])
  slowProp <- c(unfertWinchPars[4], highfertWinchPars[4], dryWinchPars[4], irr20WinchPars[4])
  inputs <- c(fertInputs[1], fertInputs[3], irrInputs[1], irrInputs[3])

  p1size <- c()
  p2size <- c()

  for(i in 1:4){
    slow14 = steadyMod(k2[i], trialStart)$preBomb14C[6]
    fast14 <- steadyMod(k1[i], trialStart)$preBomb14C[6]
    # Re-run model with fitted parameters
    fitmod = SoilR::TwopSeriesModel14(
      t=years,
      ks = c(k1[i], k2[i]),
      a21 = a21[i]*k1[i],
      C0 = t0z[i]* c(1-slowProp[i], slowProp[i]),
      inputFc = atm_in,
      F0_Delta14C = c(fast14, slow14),
      lag = lags,
      In=inputs[i]
    )


    # Modelled system 14C
    Ct = getC(fitmod) # Pool C content
    p1size <- c(p1size, Ct[,1][length(Ct[,1])])
    p2size <- c(p2size, Ct[,2][length(Ct[,2])])

  }

  fertP1size <- c(p1size[1], p1size[2])
  fertP2size <- c(p2size[1], p2size[2])
  # k1 <- c(uxP[1], hxP[1])
  # k2 <- c(uxP[2], hxP[2])
  # a21 <- c(uxP[3], hxP[3])
  infernoCols ='d3.scaleOrdinal () .range(["#FEB24C","#B10026"])'

  links <- data.frame(
    source = c(
      'Unfert Inputs', 'Unfert Inputs',
      'Unfert P2', 'Unfert P2',
      'High Fert Inputs', 'High Fert Inputs',
      'High Fert P2', 'High Fert P2'
    ),
    target = c('Unfert Respiration', 'Unfert P2',
               'Unfert P2 Storage', 'Unfert Respiration',
               'High Fert Respiration', 'High Fert P2',
               'High Fert P2 Storage', 'High Fert Respiration'
    ),
    value = c( (fertP1size[1] * k1[1]) - (fertP1size[1] * k1[1] * a21[1]),fertP1size[1] * k1[1] * a21[1],
               (fertP1size[1] * k1[1] * a21[1]) - k2[1] * fertP2size[1], k2[1] * fertP2size[1],
               (fertP1size[2] * k1[2]) - (fertP1size[2] * k1[2] * a21[2]),fertP1size[2] * k1[2] * a21[2],
               (fertP1size[2] * k1[2] * a21[2]) - k2[2] * fertP2size[2], k2[2] * fertP2size[2]

    )
  )

  nodes <- data.frame(
    name = c(as.character(links$source),
             as.character(links$target)) %>% unique()
  )

  links$IDsource <- match(links$source, nodes$name)-1
  links$IDtarget <- match(links$target, nodes$name)-1

  print(sankeyNetwork(Links = links, Nodes = nodes,
                      Source = "IDsource", Target = "IDtarget",
                      Value = 'value', NodeID = 'name',
                      sinksRight = F,
                      fontSize = 20,
                      nodePadding = 20,
                      colourScale = infernoCols))

  # Irrigation Sankey

  irrP1size <- c(p1size[3], p1size[4])
  irrP2size <- c(p2size[3], p2size[4])

  infernoCols ='d3.scaleOrdinal () .range(["#7FCDBB", "#0C2C84"])'
  links <- data.frame(
    source = c(
      'Dry Inputs', 'Dry Inputs',
      'Dry P2', 'Dry P2',
      'Irr. 20 Inputs', 'Irr. 20 Inputs',
      'Irr. 20 P2', 'Irr. 20 P2'
    ),
    target = c('Dry Respiration', 'Dry P2',
               'Dry P2 Storage', 'Dry Respiration',
               'Irr. 20 Respiration', 'Irr. 20 P2',
               'Irr. 20 P2 Storage', 'Irr. 20 Respiration'
    ),
    value = c( (irrP1size[1] * k1[3]) - (irrP1size[1] * k1[3] * a21[3]),irrP1size[1] * k1[3] * a21[3],
               (irrP1size[1] * k1[3] * a21[3]) - k2[3] * irrP2size[1], k2[3] * irrP2size[1],
               (irrP1size[2] * k1[4]) - (irrP1size[2] * k1[4] * a21[4]),irrP1size[2] * k1[4] * a21[4],
               (irrP1size[2] * k1[4] * a21[4]) - k2[4] * irrP2size[2], k2[4] * irrP2size[2]

    )
  )

  nodes <- data.frame(
    name = c(as.character(links$source),
             as.character(links$target)) %>% unique()
  )

  links$IDsource <- match(links$source, nodes$name)-1
  links$IDtarget <- match(links$target, nodes$name)-1

  print(sankeyNetwork(Links = links, Nodes = nodes,
                      Source = "IDsource", Target = "IDtarget",
                      Value = 'value', NodeID = 'name',
                      sinksRight = F,
                      fontSize = 20,
                      nodePadding = 20,
                      colourScale = infernoCols))

  unPars$trial = 'Unfert'
  resPars$trial = 'Res. Fert'
  highPars$trial = 'High Fert'

  junker <- data.frame(fertTTs$name, fertTTs$MeanTT)
  junker2 <- data.frame(fertSAs$name, fertSAs$MeanSA)
  junker3 <- data.frame(trial = fertStocks$name, Value = fertStocks$stock, Parameter = 'ssStock')
  colnames(junker) <- c('trial', 'Value')
  colnames(junker2) <- c('trial', 'Value')
  junker$Parameter <- 'MeanTT'
  junker2$Parameter <- 'MeanSA'

  allFerts <- rbind(unPars, resPars, highPars, junker, junker2, junker3)

  unfertquants$trial <- 'Unfert'
  resfertquants$trial <- 'Res. Fert'
  highfertquants$trial <- 'High Fert'

  fertquantTT <- rbind(filter(melt(unfertquants), dyn == 'TT'),
                       filter(melt(resfertquants), dyn == 'TT'),
                       filter(melt(highfertquants), dyn == 'TT'))
  fertquantTT$trial <- factor(fertquantTT$trial, levels = c('Unfert', 'Res. Fert', 'High Fert'))

  print(ggplot(fertquantTT, aes(x = value, fill = trial)) + facet_wrap(fertquantTT$variable, scales = 'free') +
          geom_density() + ggtitle('TT Quantiles') + theme_bw() +
          scale_fill_manual(values = c(alpha(unfertcols[2], 0.8), alpha(rescols[2], 0.8), alpha(highcols[2], 0.8))))

  fertquantSA <- rbind(filter(melt(unfertquants), dyn == 'SA'),
                       filter(melt(resfertquants), dyn == 'SA'),
                       filter(melt(highfertquants), dyn == 'SA'))
  fertquantSA$trial <- factor(fertquantSA$trial, levels = c('Unfert', 'Res. Fert', 'High Fert'))

  print(ggplot(fertquantSA, aes(x = value, fill = trial)) + facet_wrap(fertquantSA$variable, scales = 'free') +
          geom_density() + ggtitle('SA Quantiles') + theme_bw() +
          scale_fill_manual(values = c(alpha(unfertcols[2], 0.8), alpha(rescols[2], 0.8), alpha(highcols[2], 0.8))))

  dryquants$trial <- 'Dry'
  irr10quants$trial <- 'Irr. 10'
  irr20quants$trial <- 'Irr. 20'

  irrquantTT <- rbind(filter(melt(dryquants), dyn == 'TT'),
                      filter(melt(irr10quants), dyn == 'TT'),
                      filter(melt(irr20quants), dyn == 'TT'))
  irrquantTT$trial <- factor(irrquantTT$trial, levels = c('Dry', 'Irr. 10', 'Irr. 20'))

  print(ggplot(irrquantTT, aes(x = value, fill = trial)) + facet_wrap(irrquantTT$variable, scales = 'free') +
          geom_density() + ggtitle('TT Quantiles') + theme_bw() +
          scale_fill_manual(values = c(alpha(drycols[2], 0.8), alpha(irr10cols[2], 0.8), alpha(irr20cols[2], 0.8))))

  irrquantSA <- rbind(filter(melt(dryquants), dyn == 'SA'),
                      filter(melt(irr10quants), dyn == 'SA'),
                      filter(melt(irr20quants), dyn == 'SA'))
  irrquantSA$trial <- factor(irrquantSA$trial, levels = c('Dry', 'Irr. 10', 'Irr. 20'))

  print(ggplot(irrquantSA, aes(x = value, fill = trial)) + facet_wrap(irrquantSA$variable, scales = 'free') +
          geom_density() + ggtitle('SA Quantiles') + theme_bw() +
          scale_fill_manual(values = c(alpha(drycols[2], 0.8), alpha(irr10cols[2], 0.8), alpha(irr20cols[2], 0.8))))


  if(bootz == TRUE){
    allFerts$trial <- factor(allFerts$trial, levels = c('Unfert','Res. Fert','High Fert'), ordered = TRUE)
    fertBST <-groupwiseMean(Value~Parameter+trial,
                            data = allFerts,
                            conf = 0.99,
                            digits = 3,
                            R = 10000,
                            boot = TRUE,
                            bca = TRUE)
    fertBST <- subset(fertBST, select= c('Parameter', 'trial', 'Mean', 'Boot.mean','Trad.lower', 'Trad.upper',
                                         'Bca.lower', 'Bca.upper'))

    ggplot(allFerts, aes(x = trial, y = Value, color = trial)) + geom_boxplot() + facet_wrap(allFerts$Parameter, scales = 'free_y')

    pushViewport(viewport(y=.25,height=.5))
    jl <- tableGrob(fertBST, theme = ttheme_default(base_size = 7.5))
    grid.newpage()
    grid.draw(jl)

    fertBSTmedTT <- groupwiseMedian(value~trial+variable+dyn,
                                    data = fertquantTT,
                                    conf = 0.99,
                                    digits = 3,
                                    R = 10000,
                                    boot = TRUE)

    fertBSTmedTT <- subset(fertBSTmedTT, select= c('dyn','trial','variable', 'Median', 'Boot.median',
                                                   'Bca.lower', 'Bca.upper'))

    fertBSTmedSA <- groupwiseMedian(value~trial+variable+dyn,
                                    data = fertquantSA,
                                    conf = 0.99,
                                    digits = 3,
                                    R = 10000,
                                    boot = TRUE)

    fertBSTmedSA <- subset(fertBSTmedSA, select= c('dyn','trial','variable', 'Median', 'Boot.median',
                                                   'Bca.lower', 'Bca.upper'))

    irrBSTmedTT <- groupwiseMedian(value~trial+variable+dyn,
                                   data = irrquantTT,
                                   conf = 0.99,
                                   digits = 3,
                                   R = 10000,
                                   boot = TRUE)

    irrBSTmedTT <- subset(irrBSTmedTT, select= c('dyn','trial','variable', 'Median', 'Boot.median',
                                                 'Bca.lower', 'Bca.upper'))

    irrBSTmedSA <- groupwiseMedian(value~trial+variable+dyn,
                                   data = irrquantSA,
                                   conf = 0.99,
                                   digits = 3,
                                   R = 10000,
                                   boot = TRUE)

    irrBSTmedSA <- subset(irrBSTmedSA, select= c('dyn','trial','variable', 'Median', 'Boot.median',
                                                 'Bca.lower', 'Bca.upper'))

    pushViewport(viewport(y=.25,height=.5))
    jl <- tableGrob(fertBSTmedTT, theme = ttheme_default(base_size = 7.5))
    grid.newpage()
    grid.draw(jl)


    pushViewport(viewport(y=.25,height=.5))
    jl <- tableGrob(fertBSTmedSA, theme = ttheme_default(base_size = 7.5))
    grid.newpage()
    grid.draw(jl)

    pushViewport(viewport(y=.25,height=.5))
    jl <- tableGrob(irrBSTmedTT, theme = ttheme_default(base_size = 7.5))
    grid.newpage()
    grid.draw(jl)

    pushViewport(viewport(y=.25,height=.5))
    jl <- tableGrob(irrBSTmedSA, theme = ttheme_default(base_size = 7.5))
    grid.newpage()
    grid.draw(jl)

  }


  unfertBayesDynamics$trial = 'Unfert'
  resfertBayesDynamics$trial = 'Res. Fert'
  highfertBayesDynamics$trial = 'High Fert'

  fertMeds <- rbind(filter(unfertBayesDynamics, Parameter == 'MedTT'),
                    filter(resfertBayesDynamics, Parameter == 'MedTT'),
                    filter(highfertBayesDynamics, Parameter == 'MedTT'))
  fertMeds$trial <- factor(fertMeds$trial, levels = c('Unfert', "Res. Fert", 'High Fert'), ordered = TRUE)

  print(ggplot(fertMeds, aes(Value, fill = trial)) + geom_density() + ggtitle('Median TT') + theme_bw() +
          scale_fill_manual(values = c(unfertcols[1], rescols[1], highcols[1])))

  unfertTTdist$MassMean <- unfertTTdist$mean * (mean(unlist(unfertPoolOutputs$Ct1[dim(unfertPoolOutputs$Ct1)[1],])) +
                                                  mean(unlist(unfertPoolOutputs$Ct2[dim(unfertPoolOutputs$Ct2)[1],])))

  resfertTTdist$MassMean <- resfertTTdist$mean * (mean(unlist(resfertPoolOutputs$Ct1[dim(resfertPoolOutputs$Ct1)[1],])) +
                                                    mean(unlist(resfertPoolOutputs$Ct2[dim(resfertPoolOutputs$Ct2)[1],])))

  highfertTTdist$MassMean <- highfertTTdist$mean * (mean(unlist(highfertPoolOutputs$Ct1[dim(highfertPoolOutputs$Ct1)[1],])) +
                                                      mean(unlist(highfertPoolOutputs$Ct2[dim(highfertPoolOutputs$Ct2)[1],])))

  unfertTTdist$trial = 'Unfert'
  resfertTTdist$trial = 'Res. Fert'
  highfertTTdist$trial = 'High Fert'

  fertmTT <- rbind(unfertTTdist, resfertTTdist, highfertTTdist)
  fertmTT$trial <- factor(fertmTT$trial, levels = c('Unfert', "Res. Fert", 'High Fert'), ordered = TRUE)

  print(ggplot(fertmTT, aes(x = fertmTT$years, y = MassMean, color = trial)) + theme_bw() +
          geom_line(size = 2) + xlim(0,40) +
          scale_color_manual(values = c(unfertcols[2], rescols[2], highcols[2])) +
          ggtitle('C Stock-Weighted TT Dist'))


  dryBayesDynamics$trial = 'Dry'
  irr10BayesDynamics$trial = 'Irr. 10'
  irr20BayesDynamics$trial = 'Irr. 20'

  irrMeds <- rbind(filter(dryBayesDynamics, Parameter == 'MedTT'),
                   filter(irr10BayesDynamics, Parameter == 'MedTT'),
                   filter(irr20BayesDynamics, Parameter == 'MedTT'))


  print(ggplot(irrMeds, aes(Value, fill = trial)) + geom_density() + ggtitle('Median TT') + theme_bw() +
          scale_fill_manual(values = c(drycols[1], irr10cols[1], irr20cols[1])))

  dryTTdist$MassMean <- dryTTdist$mean * irrInputs[1]
  irr10TTdist$MassMean <- irr10TTdist$mean * irrInputs[2]
  irr20TTdist$MassMean <- irr20TTdist$mean * irrInputs[3]

  dryTTdist$trial = 'Dry'
  irr10TTdist$trial = 'Irr. 10'
  irr20TTdist$trial = 'Irr. 20'

  irrmTT <- rbind(dryTTdist, irr10TTdist, irr20TTdist)
  #irrmTT$trial <- factor(fertmTT$trial, levels = c('Dry', "Irr. 10", 'Irr. 20'), ordered = TRUE)

  print(ggplot(irrmTT, aes(x = years, y = MassMean, color = trial)) + geom_line(size = 2) + xlim(0,40) +
          scale_color_manual(values = c(drycols[1], irr10cols[1], irr20cols[1])) + theme_bw() +
          ggtitle('C Stock-Weighted TT Dist'))

  # print(ggplot(dryPoolOutputs, aes(x = , y = mean, fill = trial, ymax = Q95, ymin = Q05)) + geom_line() + xlim(0,25) +
  #         facet_grid(irrTTz$trialStart, scales = 'free_y') + theme_bw() +
  #         xlim(0, 25) + scale_fill_manual(values = c(drycols[2], irr10cols[2], irr20cols[2])) +
  #         geom_ribbon(alpha = .7) + ggtitle('Transit Time Distributions') + ylim(0, 0.32) +
  #         xlab('Years') + ylab('Density') + guides(fill=guide_legend(title="Trial")) +
  #         theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
  #               legend.title = element_text(size = 14, face = 'bold'),
  #               plot.title = element_text(size = 18, face = 'bold'),
  #               axis.title=element_text(size=14,face="bold"),
  #               strip.text.y = element_text(size = 12, face = 'bold')) + theme_bw())

  dryPoolOutputs$Ct1$Q95 <- apply(dryPoolOutputs$Ct1[,!(names(dryPoolOutputs$Ct1) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  dryPoolOutputs$Ct1$Q05 <- apply(dryPoolOutputs$Ct1[,!(names(dryPoolOutputs$Ct1) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  dryPoolOutputs$Ct1$mean <- apply(dryPoolOutputs$Ct1[,!(names(dryPoolOutputs$Ct1) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  dryPoolOutputs$Ct2$Q95 <- apply(dryPoolOutputs$Ct2[,!(names(dryPoolOutputs$Ct2) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  dryPoolOutputs$Ct2$Q05 <- apply(dryPoolOutputs$Ct2[,!(names(dryPoolOutputs$Ct2) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  dryPoolOutputs$Ct2$mean <- apply(dryPoolOutputs$Ct2[,!(names(dryPoolOutputs$Ct2) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  irr10PoolOutputs$Ct1$Q95 <- apply(irr10PoolOutputs$Ct1[,!(names(irr10PoolOutputs$Ct1) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  irr10PoolOutputs$Ct1$Q05 <- apply(irr10PoolOutputs$Ct1[,!(names(irr10PoolOutputs$Ct1) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  irr10PoolOutputs$Ct1$mean <- apply(irr10PoolOutputs$Ct1[,!(names(irr10PoolOutputs$Ct1) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  irr10PoolOutputs$Ct2$Q95 <- apply(irr10PoolOutputs$Ct2[,!(names(irr10PoolOutputs$Ct2) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  irr10PoolOutputs$Ct2$Q05 <- apply(irr10PoolOutputs$Ct2[,!(names(irr10PoolOutputs$Ct2) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  irr10PoolOutputs$Ct2$mean <- apply(irr10PoolOutputs$Ct2[,!(names(irr10PoolOutputs$Ct2) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  irr20PoolOutputs$Ct1$Q95 <- apply(irr20PoolOutputs$Ct1[,!(names(irr20PoolOutputs$Ct1) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  irr20PoolOutputs$Ct1$Q05 <- apply(irr20PoolOutputs$Ct1[,!(names(irr20PoolOutputs$Ct1) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  irr20PoolOutputs$Ct1$mean <- apply(irr20PoolOutputs$Ct1[,!(names(irr20PoolOutputs$Ct1) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  irr20PoolOutputs$Ct2$Q95 <- apply(irr20PoolOutputs$Ct2[,!(names(irr20PoolOutputs$Ct2) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  irr20PoolOutputs$Ct2$Q05 <- apply(irr20PoolOutputs$Ct2[,!(names(irr20PoolOutputs$Ct2) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  irr20PoolOutputs$Ct2$mean <- apply(irr20PoolOutputs$Ct2[,!(names(irr20PoolOutputs$Ct2) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  unfertPoolOutputs$Ct1$Q95 <- apply(unfertPoolOutputs$Ct1[,!(names(unfertPoolOutputs$Ct1) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  unfertPoolOutputs$Ct1$Q05 <- apply(unfertPoolOutputs$Ct1[,!(names(unfertPoolOutputs$Ct1) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  unfertPoolOutputs$Ct1$mean <- apply(unfertPoolOutputs$Ct1[,!(names(unfertPoolOutputs$Ct1) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  unfertPoolOutputs$Ct2$Q95 <- apply(unfertPoolOutputs$Ct2[,!(names(unfertPoolOutputs$Ct2) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  unfertPoolOutputs$Ct2$Q05 <- apply(unfertPoolOutputs$Ct2[,!(names(unfertPoolOutputs$Ct2) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  unfertPoolOutputs$Ct2$mean <- apply(unfertPoolOutputs$Ct2[,!(names(unfertPoolOutputs$Ct2) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  resfertPoolOutputs$Ct1$Q95 <- apply(resfertPoolOutputs$Ct1[,!(names(resfertPoolOutputs$Ct1) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  resfertPoolOutputs$Ct1$Q05 <- apply(resfertPoolOutputs$Ct1[,!(names(resfertPoolOutputs$Ct1) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  resfertPoolOutputs$Ct1$mean <- apply(resfertPoolOutputs$Ct1[,!(names(resfertPoolOutputs$Ct1) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  resfertPoolOutputs$Ct2$Q95 <- apply(resfertPoolOutputs$Ct2[,!(names(resfertPoolOutputs$Ct2) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  resfertPoolOutputs$Ct2$Q05 <- apply(resfertPoolOutputs$Ct2[,!(names(resfertPoolOutputs$Ct2) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  resfertPoolOutputs$Ct2$mean <- apply(resfertPoolOutputs$Ct2[,!(names(resfertPoolOutputs$Ct2) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  highfertPoolOutputs$Ct1$Q95 <- apply(highfertPoolOutputs$Ct1[,!(names(highfertPoolOutputs$Ct1) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  highfertPoolOutputs$Ct1$Q05 <- apply(highfertPoolOutputs$Ct1[,!(names(highfertPoolOutputs$Ct1) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  highfertPoolOutputs$Ct1$mean <- apply(highfertPoolOutputs$Ct1[,!(names(highfertPoolOutputs$Ct1) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  highfertPoolOutputs$Ct2$Q95 <- apply(highfertPoolOutputs$Ct2[,!(names(highfertPoolOutputs$Ct2) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  highfertPoolOutputs$Ct2$Q05 <- apply(highfertPoolOutputs$Ct2[,!(names(highfertPoolOutputs$Ct2) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  highfertPoolOutputs$Ct2$mean <- apply(highfertPoolOutputs$Ct2[,!(names(highfertPoolOutputs$Ct2) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))


  par(mfrow = c(2,3))
  plot(seq(trialStart, trialEnd), dryPoolOutputs$Ct1$mean, type = 'l', ylim = c(0, 30), col = 'brown3', lwd = 3, main = 'Dry', xlab = '', ylab = 'C Stock')
  lines(inputDry, col = 'violet', lwd = 3)
  lines(seq(trialStart, trialEnd), dryPoolOutputs$Ct2$mean, col = 'steelblue4', lwd = 3)

  plot(seq(trialStart, 2003), irr10PoolOutputs$Ct1$mean, type = 'l', ylim = c(0, 30), col = 'brown3', lwd = 3, main = 'Irr. 10', xlab = '', ylab = 'C Stock')
  lines(input10, col = 'violet', lwd = 3)
  lines(seq(trialStart, 2003), irr10PoolOutputs$Ct2$mean, col = 'steelblue4', lwd = 3)

  plot(seq(trialStart, trialEnd), irr20PoolOutputs$Ct1$mean, type = 'l', ylim = c(0, 30), col = 'brown3', lwd = 3, main = 'Irr. 20', xlab = '', ylab = 'C Stock')
  lines(input20, col = 'violet', lwd = 3)
  lines(seq(trialStart, trialEnd), irr20PoolOutputs$Ct2$mean, col = 'steelblue4', lwd = 3)

  plot(seq(trialStart, trialEnd), unfertPoolOutputs$Ct1$mean, type = 'l', ylim = c(0, 30), col = 'brown3', lwd = 3, main = 'Unfert', xlab = 'Year', ylab = 'C Stock')
  lines(inputUn, col = 'violet', lwd = 3)
  lines(seq(trialStart, trialEnd), unfertPoolOutputs$Ct2$mean, col = 'steelblue4', lwd = 3)
  legend('topleft', legend = c('Inputs', "Fast Pool", "Slow Pool"), col = c('violet','brown3','steelblue4'), lwd = 3, bty = 'n')

  plot(seq(trialStart, trialEnd), resfertPoolOutputs$Ct1$mean, type = 'l', ylim = c(0, 30), col = 'brown3', lwd = 3, main = 'Res. Fert', xlab = 'Year', ylab = 'C Stock')
  lines(inputRes, col = 'violet', lwd = 3)
  lines(seq(trialStart, trialEnd), resfertPoolOutputs$Ct2$mean, col = 'steelblue4', lwd = 3)

  plot(seq(trialStart, trialEnd), highfertPoolOutputs$Ct1$mean, type = 'l', ylim = c(0, 30), col = 'brown3', lwd = 3, main = 'High Fert', xlab = 'Year', ylab = 'C Stock')
  lines(inputHigh, col = 'violet', lwd = 3)
  lines(seq(trialStart, trialEnd), highfertPoolOutputs$Ct2$mean, col = 'steelblue4', lwd = 3)

  par(mfrow = c(3,2))
  par(mar=c(5,6,4,2)+0.1)
  plot(inputDry, col = drycols[1], lwd = 3, type = 'l', main = 'Irr. Inputs', ylim = c(0, 4),
       cex.lab = 1.4, cex.axis = 1.5, cex.main =1.5, ylab = "Inputs (Mg C / ha / yr)")
  lines(input10, col = irr10cols[1], lwd = 3)
  lines(input20, col = irr20cols[1], lwd = 3)

  plot(inputUn, col = unfertcols[1], lwd = 3, type = 'l', main = 'Fert Inputs', ylim = c(0,4),
       cex.lab = 1.4, cex.axis = 1.5, cex.main =1.5, ylab = "Inputs (Mg C / ha / yr)")
  lines(inputRes, col = rescols[1], lwd = 3)
  lines(inputHigh, col = highcols[1], lwd = 3)

  par(mfrow = c(2,2))

  plot(seq(trialStart, trialEnd), dryPoolOutputs$Ct1$mean, type = 'l', ylim = c(10, 25),
       col = drycols[2], lwd = 3, main = 'Irr. Fast Pool', xlab = '',
       cex.lab = 1.5, cex.axis = 1.5, cex.main =1.5, ylab = "C Stock (Mg C / ha)")
  lines(seq(trialStart, 2003), irr10PoolOutputs$Ct1$mean, col = irr10cols[2], lwd = 3)
  lines(c(2003, 2010), c(19.2, 19.2), col = irr10cols[2], lwd = 3)
  lines(seq(trialStart, trialEnd), irr20PoolOutputs$Ct1$mean, col = irr20cols[2], lwd = 3)

  plot(seq(trialStart, trialEnd), unfertPoolOutputs$Ct1$mean, type = 'l', ylim = c(10, 25),
       col = unfertcols[2], lwd = 3, main = 'Fert Fast Pool', xlab = '',
       cex.lab = 1.5, cex.axis = 1.5, cex.main =1.5, ylab = "C Stock (Mg C / ha)")
  lines(seq(trialStart, trialEnd), resfertPoolOutputs$Ct1$mean, col = rescols[2], lwd = 3)
  lines(seq(trialStart, trialEnd), highfertPoolOutputs$Ct1$mean, col = highcols[2], lwd = 3)

  plot(seq(trialStart, trialEnd), dryPoolOutputs$Ct2$mean, type = 'l', ylim = c(10, 25),
       col = drycols[2], lwd = 3, main = 'Irr. Slow Pool', xlab = '',
       cex.lab = 1.5, cex.axis = 1.5, cex.main =1.5, ylab = "C Stock (Mg C / ha)")
  lines(seq(trialStart, 2003), irr10PoolOutputs$Ct2$mean, col = irr10cols[2], lwd = 3)
  lines(c(2003, 2010), c(18.86, 18.86), col = irr10cols[2], lwd = 3)
  lines(seq(trialStart, trialEnd), irr20PoolOutputs$Ct2$mean, col = irr20cols[2], lwd = 3)

  plot(seq(trialStart, trialEnd), unfertPoolOutputs$Ct2$mean, type = 'l', ylim = c(10, 25),
       col = unfertcols[2], lwd = 3, main = 'Fert Slow Pool', xlab = '',
       cex.lab = 1.5, cex.axis = 1.5, cex.main =1.5, ylab = "C Stock (Mg C / ha)")
  lines(seq(trialStart, trialEnd), resfertPoolOutputs$Ct2$mean, col = rescols[2], lwd = 3)
  lines(seq(trialStart, trialEnd), highfertPoolOutputs$Ct2$mean, col = highcols[2], lwd = 3)

  par(mfrow = c(1,1))

  plot(seq(trialStart, trialEnd), dryPoolOutputs$Ct1$mean, type = 'l', ylim = c(0,26), xlab = 'Year',
       ylab = 'C Stock (T C ha-1)', main = 'Dry :: Modeled Pool Size', lwd = 2)
  lines(seq(trialStart, trialEnd), dryPoolOutputs$Ct2$mean, lwd = 2)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(dryPoolOutputs$Ct1$Q95, rev(dryPoolOutputs$Ct1$Q05)),
          col = drycols[1], border = NA)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(dryPoolOutputs$Ct2$Q95, rev(dryPoolOutputs$Ct2$Q05)),
          col = alpha(drycols[2], .3), border = NA)
  legend('topleft', legend = c('Pool 1', 'Pool 2', '95% Conf.'),
         fill = c(drycols[1], alpha(drycols[2], .4), 'gray'))

  plot(trialStart+seq(0, length(irr10PoolOutputs$Ct1[,1])-1), irr10PoolOutputs$Ct1$mean, type = 'l', ylim = c(0,26), xlab = 'Year',
       ylab = 'C Stock (T C ha-1)', main = 'Irr. 10 :: Modeled Pool Size', lwd = 2)
  lines(trialStart+seq(0, length(irr10PoolOutputs$Ct1[,1])-1), irr10PoolOutputs$Ct2$mean, lwd = 2)
  polygon(c(trialStart+seq(0, length(irr10PoolOutputs$Ct1[,1])-1), rev(trialStart+seq(0, length(irr10PoolOutputs$Ct1[,1])-1))),
          c(irr10PoolOutputs$Ct1$Q95, rev(irr10PoolOutputs$Ct1$Q05)),
          col = irr10cols[1], border = NA)
  polygon(c(trialStart+seq(0, length(irr10PoolOutputs$Ct1[,1])-1), rev(trialStart+seq(0, length(irr10PoolOutputs$Ct1[,1])-1))),
          c(irr10PoolOutputs$Ct2$Q95, rev(irr10PoolOutputs$Ct2$Q05)),
          col = alpha(irr10cols[2], .3), border = NA)
  legend('topleft', legend = c('Pool 1', 'Pool 2', '95% Conf.'),
         fill = c(irr10cols[1], alpha(irr10cols[2], .4), 'gray'))

  plot(seq(trialStart, trialEnd), irr20PoolOutputs$Ct1$mean, type = 'l', ylim = c(0,26), lwd = 2, xlab = 'Year',
       ylab = 'C Stock (T C ha-1)', main = 'Irr. 20 :: Modeled Pool Size')
  lines(seq(trialStart, trialEnd), irr20PoolOutputs$Ct2$mean, lwd = 2)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(irr20PoolOutputs$Ct1$Q95, rev(irr20PoolOutputs$Ct1$Q05)),
          col = irr20cols[1], border = NA)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(irr20PoolOutputs$Ct2$Q95, rev(irr20PoolOutputs$Ct2$Q05)),
          col = alpha(irr20cols[2], .3), border = NA)
  legend('topleft', legend = c('Pool 1', 'Pool 2', '95% Conf.'),
         fill = c(irr20cols[1], alpha(irr20cols[2], .4), 'gray'))

  plot(seq(trialStart, trialEnd), unfertPoolOutputs$Ct1$mean, type = 'l', ylim = c(0,26), xlab = 'Year', lwd = 2,
       ylab = 'C Stock (T C ha-1)', main = 'Unfert :: Modeled Pool Size')
  lines(seq(trialStart, trialEnd), unfertPoolOutputs$Ct2$mean, lwd = 2)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(unfertPoolOutputs$Ct1$Q95, rev(unfertPoolOutputs$Ct1$Q05)),
          col = unfertcols[1], border = NA)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(unfertPoolOutputs$Ct2$Q95, rev(unfertPoolOutputs$Ct2$Q05)),
          col = alpha(unfertcols[2], .3), border = NA)
  legend('topleft', legend = c('Pool 1', 'Pool 2', '95% Conf.'),
         fill = c(unfertcols[1], alpha(unfertcols[2], .4), 'gray'))

  plot(seq(trialStart, trialEnd), resfertPoolOutputs$Ct1$mean, type = 'l', ylim = c(0,26), xlab = 'Year', lwd = 2,
       ylab = 'C Stock (T C ha-1)', main = 'Res. Fert :: Modeled Pool Size')
  lines(seq(trialStart, trialEnd), resfertPoolOutputs$Ct2$mean, lwd = 2)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(resfertPoolOutputs$Ct1$Q95, rev(resfertPoolOutputs$Ct1$Q05)),
          col = rescols[1], border = NA)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(resfertPoolOutputs$Ct2$Q95, rev(resfertPoolOutputs$Ct2$Q05)),
          col = alpha(rescols[2], .3), border = NA)
  legend('topleft', legend = c('Pool 1', 'Pool 2', '95% Conf.'),
         fill = c(rescols[1], alpha(rescols[2], .2), 'gray'))

  plot(seq(trialStart, trialEnd), highfertPoolOutputs$Ct1$mean, type = 'l', ylim = c(0,26), xlab = 'Year', lwd = 2,
       ylab = 'C Stock (T C ha-1)', main = 'High Fert :: Modeled Pool Size')
  lines(seq(trialStart, trialEnd), highfertPoolOutputs$Ct2$mean, lwd = 2)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(highfertPoolOutputs$Ct1$Q95, rev(highfertPoolOutputs$Ct1$Q05)),
          col = highcols[1], border = NA)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(highfertPoolOutputs$Ct2$Q95, rev(highfertPoolOutputs$Ct2$Q05)),
          col = alpha(highcols[2], .3), border = NA)
  legend('topleft', legend = c('Pool 1', 'Pool 2', '95% Conf.'),
         fill = c(highcols[1], alpha(highcols[2], .2), 'gray'))

  dryPoolOutputs$C141$Q95 <- apply(dryPoolOutputs$C141[,!(names(dryPoolOutputs$C141) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  dryPoolOutputs$C141$Q05 <- apply(dryPoolOutputs$C141[,!(names(dryPoolOutputs$C141) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  dryPoolOutputs$C141$mean <- apply(dryPoolOutputs$C141[,!(names(dryPoolOutputs$C141) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  dryPoolOutputs$C142$Q95 <- apply(dryPoolOutputs$C142[,!(names(dryPoolOutputs$C142) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  dryPoolOutputs$C142$Q05 <- apply(dryPoolOutputs$C142[,!(names(dryPoolOutputs$C142) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  dryPoolOutputs$C142$mean <- apply(dryPoolOutputs$C142[,!(names(dryPoolOutputs$C142) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  irr10PoolOutputs$C141$Q95 <- apply(irr10PoolOutputs$C141[,!(names(irr10PoolOutputs$C141) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  irr10PoolOutputs$C141$Q05 <- apply(irr10PoolOutputs$C141[,!(names(irr10PoolOutputs$C141) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  irr10PoolOutputs$C141$mean <- apply(irr10PoolOutputs$C141[,!(names(irr10PoolOutputs$C141) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  irr10PoolOutputs$C142$Q95 <- apply(irr10PoolOutputs$C142[,!(names(irr10PoolOutputs$C142) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  irr10PoolOutputs$C142$Q05 <- apply(irr10PoolOutputs$C142[,!(names(irr10PoolOutputs$C142) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  irr10PoolOutputs$C142$mean <- apply(irr10PoolOutputs$C142[,!(names(irr10PoolOutputs$C142) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  irr20PoolOutputs$C141$Q95 <- apply(irr20PoolOutputs$C141[,!(names(irr20PoolOutputs$C141) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  irr20PoolOutputs$C141$Q05 <- apply(irr20PoolOutputs$C141[,!(names(irr20PoolOutputs$C141) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  irr20PoolOutputs$C141$mean <- apply(irr20PoolOutputs$C141[,!(names(irr20PoolOutputs$C141) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  irr20PoolOutputs$C142$Q95 <- apply(irr20PoolOutputs$C142[,!(names(irr20PoolOutputs$C142) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  irr20PoolOutputs$C142$Q05 <- apply(irr20PoolOutputs$C142[,!(names(irr20PoolOutputs$C142) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  irr20PoolOutputs$C142$mean <- apply(irr20PoolOutputs$C142[,!(names(irr20PoolOutputs$C142) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  unfertPoolOutputs$C141$Q95 <- apply(unfertPoolOutputs$C141[,!(names(unfertPoolOutputs$C141) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  unfertPoolOutputs$C141$Q05 <- apply(unfertPoolOutputs$C141[,!(names(unfertPoolOutputs$C141) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  unfertPoolOutputs$C141$mean <- apply(unfertPoolOutputs$C141[,!(names(unfertPoolOutputs$C141) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  unfertPoolOutputs$C142$Q95 <- apply(unfertPoolOutputs$C142[,!(names(unfertPoolOutputs$C142) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  unfertPoolOutputs$C142$Q05 <- apply(unfertPoolOutputs$C142[,!(names(unfertPoolOutputs$C142) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  unfertPoolOutputs$C142$mean <- apply(unfertPoolOutputs$C142[,!(names(unfertPoolOutputs$C142) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  resfertPoolOutputs$C141$Q95 <- apply(resfertPoolOutputs$C141[,!(names(resfertPoolOutputs$C141) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  resfertPoolOutputs$C141$Q05 <- apply(resfertPoolOutputs$C141[,!(names(resfertPoolOutputs$C141) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  resfertPoolOutputs$C141$mean <- apply(resfertPoolOutputs$C141[,!(names(resfertPoolOutputs$C141) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  resfertPoolOutputs$C142$Q95 <- apply(resfertPoolOutputs$C142[,!(names(resfertPoolOutputs$C142) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  resfertPoolOutputs$C142$Q05 <- apply(resfertPoolOutputs$C142[,!(names(resfertPoolOutputs$C142) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  resfertPoolOutputs$C142$mean <- apply(resfertPoolOutputs$C142[,!(names(resfertPoolOutputs$C142) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  highfertPoolOutputs$C141$Q95 <- apply(highfertPoolOutputs$C141[,!(names(highfertPoolOutputs$C141) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  highfertPoolOutputs$C141$Q05 <- apply(highfertPoolOutputs$C141[,!(names(highfertPoolOutputs$C141) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  highfertPoolOutputs$C141$mean <- apply(highfertPoolOutputs$C141[,!(names(highfertPoolOutputs$C141) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  highfertPoolOutputs$C142$Q95 <- apply(highfertPoolOutputs$C142[,!(names(highfertPoolOutputs$C142) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  highfertPoolOutputs$C142$Q05 <- apply(highfertPoolOutputs$C142[,!(names(highfertPoolOutputs$C142) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  highfertPoolOutputs$C142$mean <- apply(highfertPoolOutputs$C142[,!(names(highfertPoolOutputs$C142) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  dryPoolOutputs$C14b$Q95 <- apply(dryPoolOutputs$C14b[,!(names(dryPoolOutputs$C14b) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  dryPoolOutputs$C14b$Q05 <- apply(dryPoolOutputs$C14b[,!(names(dryPoolOutputs$C14b) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  dryPoolOutputs$C14b$mean <- apply(dryPoolOutputs$C14b[,!(names(dryPoolOutputs$C14b) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  irr10PoolOutputs$C14b$Q95 <- apply(irr10PoolOutputs$C14b[,!(names(irr10PoolOutputs$C14b) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  irr10PoolOutputs$C14b$Q05 <- apply(irr10PoolOutputs$C14b[,!(names(irr10PoolOutputs$C14b) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  irr10PoolOutputs$C14b$mean <- apply(irr10PoolOutputs$C14b[,!(names(irr10PoolOutputs$C14b) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  irr20PoolOutputs$C14b$Q95 <- apply(irr20PoolOutputs$C14b[,!(names(irr20PoolOutputs$C14b) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  irr20PoolOutputs$C14b$Q05 <- apply(irr20PoolOutputs$C14b[,!(names(irr20PoolOutputs$C14b) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  irr20PoolOutputs$C14b$mean <- apply(irr20PoolOutputs$C14b[,!(names(irr20PoolOutputs$C14b) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  unfertPoolOutputs$C14b$Q95 <- apply(unfertPoolOutputs$C14b[,!(names(unfertPoolOutputs$C14b) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  unfertPoolOutputs$C14b$Q05 <- apply(unfertPoolOutputs$C14b[,!(names(unfertPoolOutputs$C14b) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  unfertPoolOutputs$C14b$mean <- apply(unfertPoolOutputs$C14b[,!(names(unfertPoolOutputs$C14b) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  resfertPoolOutputs$C14b$Q95 <- apply(resfertPoolOutputs$C14b[,!(names(resfertPoolOutputs$C14b) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  resfertPoolOutputs$C14b$Q05 <- apply(resfertPoolOutputs$C14b[,!(names(resfertPoolOutputs$C14b) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  resfertPoolOutputs$C14b$mean <- apply(resfertPoolOutputs$C14b[,!(names(resfertPoolOutputs$C14b) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  highfertPoolOutputs$C14b$Q95 <- apply(highfertPoolOutputs$C14b[,!(names(highfertPoolOutputs$C14b) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  highfertPoolOutputs$C14b$Q05 <- apply(highfertPoolOutputs$C14b[,!(names(highfertPoolOutputs$C14b) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  highfertPoolOutputs$C14b$mean <- apply(highfertPoolOutputs$C14b[,!(names(highfertPoolOutputs$C14b) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  dryPoolOutputs$R14$Q95 <- apply(dryPoolOutputs$R14[,!(names(dryPoolOutputs$R14) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  dryPoolOutputs$R14$Q05 <- apply(dryPoolOutputs$R14[,!(names(dryPoolOutputs$R14) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  dryPoolOutputs$R14$mean <- apply(dryPoolOutputs$R14[,!(names(dryPoolOutputs$R14) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  irr10PoolOutputs$R14$Q95 <- apply(irr10PoolOutputs$R14[,!(names(irr10PoolOutputs$R14) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  irr10PoolOutputs$R14$Q05 <- apply(irr10PoolOutputs$R14[,!(names(irr10PoolOutputs$R14) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  irr10PoolOutputs$R14$mean <- apply(irr10PoolOutputs$R14[,!(names(irr10PoolOutputs$R14) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  irr20PoolOutputs$R14$Q95 <- apply(irr20PoolOutputs$R14[,!(names(irr20PoolOutputs$R14) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  irr20PoolOutputs$R14$Q05 <- apply(irr20PoolOutputs$R14[,!(names(irr20PoolOutputs$R14) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  irr20PoolOutputs$R14$mean <- apply(irr20PoolOutputs$R14[,!(names(irr20PoolOutputs$R14) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  unfertPoolOutputs$R14$Q95 <- apply(unfertPoolOutputs$R14[,!(names(unfertPoolOutputs$R14) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  unfertPoolOutputs$R14$Q05 <- apply(unfertPoolOutputs$R14[,!(names(unfertPoolOutputs$R14) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  unfertPoolOutputs$R14$mean <- apply(unfertPoolOutputs$R14[,!(names(unfertPoolOutputs$R14) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  resfertPoolOutputs$R14$Q95 <- apply(resfertPoolOutputs$R14[,!(names(resfertPoolOutputs$R14) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  resfertPoolOutputs$R14$Q05 <- apply(resfertPoolOutputs$R14[,!(names(resfertPoolOutputs$R14) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  resfertPoolOutputs$R14$mean <- apply(resfertPoolOutputs$R14[,!(names(resfertPoolOutputs$R14) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))
  highfertPoolOutputs$R14$Q95 <- apply(highfertPoolOutputs$R14[,!(names(highfertPoolOutputs$R14) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.95))
  highfertPoolOutputs$R14$Q05 <- apply(highfertPoolOutputs$R14[,!(names(highfertPoolOutputs$R14) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) quantile(x, probs = 0.05))
  highfertPoolOutputs$R14$mean <- apply(highfertPoolOutputs$R14[,!(names(highfertPoolOutputs$R14) %in% c('years', 'Q95', 'Q05', 'mean','trial','trialStart','median','MassMean'))], 1, function(x) mean(x))



  plot(seq(trialStart, trialEnd), dryPoolOutputs$C141$mean, type = 'l', xlab = 'Year', ylim = c(-42, 500),
       ylab = expression(paste(Delta^{14}, "C")), main = 'Dry :: Modeled Pool 14C', lwd = 2)
  lines(seq(trialStart, trialEnd), dryPoolOutputs$C142$mean, lwd = 2)
  lines(seq(trialStart, trialEnd), dryPoolOutputs$R14$mean, lwd = 2, lty = 2)
  lines(atm_in, col = 'purple4', lwd=3)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(dryPoolOutputs$R14$Q95, rev(dryPoolOutputs$R14$Q05)),
          col = alpha('chartreuse', 0.4), border = NA)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(dryPoolOutputs$C14b$Q95, rev(dryPoolOutputs$C14b$Q05)),
          col = alpha(drycols[2], 1), border = NA)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(dryPoolOutputs$C141$Q95, rev(dryPoolOutputs$C141$Q05)),
          col = drycols[1], border = NA)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(dryPoolOutputs$C142$Q95, rev(dryPoolOutputs$C142$Q05)),
          col = alpha(drycols[2], .3), border = NA)
  lines(seq(trialStart, trialEnd), dryPoolOutputs$C14b$mean, lwd = 2)
  legend('topright', legend = c('Pool 1', 'Pool 2','Bulk','Resp.','95% Conf.','Atm.'),
         fill = c(drycols[1], alpha(drycols[2], .4), alpha(drycols[2], 1),alpha('chartreuse', 0.4), 'gray', 'purple4'))

  plot(trialStart+seq(0, length(irr10PoolOutputs$C141[,1])-1), irr10PoolOutputs$C141$mean, type = 'l', xlab = 'Year',
       ylab = expression(paste(Delta^{14}, "C")), main = 'Irr. 10 :: Modeled Pool 14C', lwd = 2, ylim = c(-42, 500))
  lines(trialStart+seq(0, length(irr10PoolOutputs$C141[,1])-1), irr10PoolOutputs$C142$mean, lwd = 2)
  lines(atm_in, col = 'purple4', lwd=3)
  polygon(c(trialStart+seq(0, length(irr10PoolOutputs$C141[,1])-1), rev(trialStart+seq(0, length(irr10PoolOutputs$C141[,1])-1))),
          c(irr10PoolOutputs$C141$Q95, rev(irr10PoolOutputs$C141$Q05)),
          col = irr10cols[1], border = NA)
  polygon(c(trialStart+seq(0, length(irr10PoolOutputs$C141[,1])-1), rev(trialStart+seq(0, length(irr10PoolOutputs$C141[,1])-1))),
          c(irr10PoolOutputs$R14$Q95, rev(irr10PoolOutputs$R14$Q05)),
          col = alpha('chartreuse',0.4), border = NA)
  polygon(c(trialStart+seq(0, length(irr10PoolOutputs$C141[,1])-1), rev(trialStart+seq(0, length(irr10PoolOutputs$C141[,1])-1))),
          c(irr10PoolOutputs$C142$Q95, rev(irr10PoolOutputs$C142$Q05)),
          col = alpha(irr10cols[2], .3), border = NA)
  polygon(c(trialStart+seq(0, length(irr10PoolOutputs$C14b[,1])-1), rev(trialStart+seq(0, length(irr10PoolOutputs$C14b[,1])-1))),
          c(irr10PoolOutputs$C14b$Q95, rev(irr10PoolOutputs$C14b$Q05)),
          col = alpha(irr10cols[2], 1), border = NA)
  lines(trialStart+seq(0, length(irr10PoolOutputs$R14[,1])-1), irr10PoolOutputs$R14$mean, lwd = 2, lty = 2)
  lines(trialStart+seq(0, length(irr10PoolOutputs$C141[,1])-1), irr10PoolOutputs$C14b$mean, lwd = 2)
  legend('topright', legend = c('Pool 1', 'Pool 2','Bulk','Resp.','95% Conf.','Atm.'),
         fill = c(irr10cols[1], alpha(irr10cols[2], .4), alpha(irr10cols[2], 1),alpha('chartreuse', 0.4), 'gray', 'purple4'))




  plot(seq(trialStart, trialEnd), irr20PoolOutputs$C141$mean, type = 'l', xlab = 'Year', ylim = c(-42, 500),
       ylab = expression(paste(Delta^{14}, "C")), main = 'irr20 :: Modeled Pool 14C', lwd = 2)
  lines(seq(trialStart, trialEnd), irr20PoolOutputs$C142$mean, lwd = 2)
  lines(seq(trialStart, trialEnd), irr20PoolOutputs$R14$mean, lwd = 2, lty = 2)
  lines(atm_in, col = 'purple4', lwd=3)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(irr20PoolOutputs$R14$Q95, rev(irr20PoolOutputs$R14$Q05)),
          col = alpha('chartreuse', 0.4), border = NA)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(irr20PoolOutputs$C14b$Q95, rev(irr20PoolOutputs$C14b$Q05)),
          col = alpha(irr20cols[2], 1), border = NA)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(irr20PoolOutputs$C141$Q95, rev(irr20PoolOutputs$C141$Q05)),
          col = irr20cols[1], border = NA)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(irr20PoolOutputs$C142$Q95, rev(irr20PoolOutputs$C142$Q05)),
          col = alpha(irr20cols[2], .3), border = NA)
  lines(seq(trialStart, trialEnd), irr20PoolOutputs$C14b$mean, lwd = 2)
  legend('topright', legend = c('Pool 1', 'Pool 2','Bulk','Resp.','95% Conf.','Atm.'),
         fill = c(irr20cols[1], alpha(irr20cols[2], .4), alpha(irr20cols[2], 1),alpha('chartreuse', 0.4), 'gray', 'purple4'))

  plot(seq(trialStart, trialEnd), unfertPoolOutputs$C141$mean, type = 'l', xlab = 'Year', ylim = c(-42, 500),
       ylab = expression(paste(Delta^{14}, "C")), main = 'Unfert :: Modeled Pool 14C', lwd = 2)
  lines(seq(trialStart, trialEnd), unfertPoolOutputs$C142$mean, lwd = 2)
  lines(seq(trialStart, trialEnd), unfertPoolOutputs$R14$mean, lwd = 2, lty = 2)
  lines(atm_in, col = 'purple4', lwd=3)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(unfertPoolOutputs$R14$Q95, rev(unfertPoolOutputs$R14$Q05)),
          col = alpha('chartreuse', 0.4), border = NA)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(unfertPoolOutputs$C14b$Q95, rev(unfertPoolOutputs$C14b$Q05)),
          col = alpha(unfertcols[2], 1), border = NA)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(unfertPoolOutputs$C141$Q95, rev(unfertPoolOutputs$C141$Q05)),
          col = unfertcols[1], border = NA)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(unfertPoolOutputs$C142$Q95, rev(unfertPoolOutputs$C142$Q05)),
          col = alpha(unfertcols[2], .3), border = NA)
  lines(seq(trialStart, trialEnd), unfertPoolOutputs$C14b$mean, lwd = 2)
  legend('topright', legend = c('Pool 1', 'Pool 2','Bulk','Resp.','95% Conf.','Atm.'),
         fill = c(unfertcols[1], alpha(unfertcols[2], .4), alpha(unfertcols[2], 1),alpha('chartreuse', 0.4), 'gray', 'purple4'))



  plot(seq(trialStart, trialEnd), resfertPoolOutputs$C141$mean, type = 'l', xlab = 'Year', ylim = c(-42, 500),
       ylab = expression(paste(Delta^{14}, "C")), main = 'res :: Modeled Pool 14C', lwd = 2)
  lines(seq(trialStart, trialEnd), resfertPoolOutputs$C142$mean, lwd = 2)
  lines(seq(trialStart, trialEnd), resfertPoolOutputs$R14$mean, lwd = 2, lty = 2)
  lines(atm_in, col = 'purple4', lwd=3)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(resfertPoolOutputs$R14$Q95, rev(resfertPoolOutputs$R14$Q05)),
          col = alpha('chartreuse', 0.4), border = NA)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(resfertPoolOutputs$C14b$Q95, rev(resfertPoolOutputs$C14b$Q05)),
          col = alpha(rescols[2], 1), border = NA)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(resfertPoolOutputs$C141$Q95, rev(resfertPoolOutputs$C141$Q05)),
          col = rescols[1], border = NA)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(resfertPoolOutputs$C142$Q95, rev(resfertPoolOutputs$C142$Q05)),
          col = alpha(rescols[2], .3), border = NA)
  lines(seq(trialStart, trialEnd), resfertPoolOutputs$C14b$mean, lwd = 2)
  legend('topright', legend = c('Pool 1', 'Pool 2','Bulk','Resp.','95% Conf.','Atm.'),
         fill = c(rescols[1], alpha(rescols[2], .4), alpha(rescols[2], 1),alpha('chartreuse', 0.4), 'gray', 'purple4'))


  plot(seq(trialStart, trialEnd), highfertPoolOutputs$C141$mean, type = 'l', xlab = 'Year', ylim = c(-42, 500),
       ylab = expression(paste(Delta^{14}, "C")), main = 'High :: Modeled Pool 14C', lwd = 2)
  lines(seq(trialStart, trialEnd), highfertPoolOutputs$C142$mean, lwd = 2)
  lines(seq(trialStart, trialEnd), highfertPoolOutputs$R14$mean, lwd = 2, lty = 2)
  lines(atm_in, col = 'purple4', lwd=3)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(highfertPoolOutputs$R14$Q95, rev(highfertPoolOutputs$R14$Q05)),
          col = alpha('chartreuse', 0.4), border = NA)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(highfertPoolOutputs$C14b$Q95, rev(highfertPoolOutputs$C14b$Q05)),
          col = alpha(highcols[2], 1), border = NA)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(highfertPoolOutputs$C141$Q95, rev(highfertPoolOutputs$C141$Q05)),
          col = highcols[1], border = NA)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(highfertPoolOutputs$C142$Q95, rev(highfertPoolOutputs$C142$Q05)),
          col = alpha(highcols[2], .3), border = NA)
  lines(seq(trialStart, trialEnd), highfertPoolOutputs$C14b$mean, lwd = 2)
  legend('topright', legend = c('Pool 1', 'Pool 2','Bulk','Resp.','95% Conf.','Atm.'),
         fill = c(highcols[1], alpha(highcols[2], .4), alpha(highcols[2], 1),alpha('chartreuse', 0.4), 'gray', 'purple4'))

  #Pool 1 14C comparison
  plot(seq(trialStart, trialEnd), irr20PoolOutputs$C141$mean, type = 'l', xlab = 'Year', #ylim = c(10, 300),
       ylab = expression(paste(Delta^{14}, "C")), main = 'P1 14C', lwd = 2)
  lines(atm_in, col = 'purple4', lwd=3)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(irr20PoolOutputs$C141$Q95, rev(irr20PoolOutputs$C141$Q05)),
          col = irr20cols[1], border = NA)
  polygon(c(trialStart+seq(0, length(irr10PoolOutputs$C141[,1])-1), rev(trialStart+seq(0, length(irr10PoolOutputs$C141[,1])-1))),
          c(irr10PoolOutputs$C141$Q95, rev(irr10PoolOutputs$C141$Q05)),
          col = irr10cols[1], border = NA)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(dryPoolOutputs$C141$Q95, rev(dryPoolOutputs$C141$Q05)),
          col = drycols[1], border = NA)
  lines(trialStart+seq(0, length(irr10PoolOutputs$C141[,1])-1), irr10PoolOutputs$C141$mean, lwd = 2, col = irr20cols[2])
  lines(seq(trialStart, trialEnd), irr20PoolOutputs$C141$mean, lwd = 2, col = irr10cols[2])
  lines(seq(trialStart, trialEnd), dryPoolOutputs$C141$mean, lwd = 2, col = drycols[2])


  plot(seq(trialStart, trialEnd), highfertPoolOutputs$C141$mean, type = 'l', xlab = 'Year', #ylim = c(10, 300),
       ylab = expression(paste(Delta^{14}, "C")), main = 'P1 14C', lwd = 2)
  lines(atm_in, col = 'purple4', lwd=3)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(highfertPoolOutputs$C141$Q95, rev(highfertPoolOutputs$C141$Q05)),
          col = highcols[1], border = NA)
  polygon(c(trialStart+seq(0, length(resfertPoolOutputs$C141[,1])-1), rev(trialStart+seq(0, length(resfertPoolOutputs$C141[,1])-1))),
          c(resfertPoolOutputs$C141$Q95, rev(resfertPoolOutputs$C141$Q05)),
          col = rescols[1], border = NA)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(unfertPoolOutputs$C141$Q95, rev(unfertPoolOutputs$C141$Q05)),
          col = unfertcols[1], border = NA)
  lines(trialStart+seq(0, length(resfertPoolOutputs$C141[,1])-1), resfertPoolOutputs$C141$mean, lwd = 2, col = highcols[2])
  lines(seq(trialStart, trialEnd), highfertPoolOutputs$C141$mean, lwd = 2, col = rescols[2])
  lines(seq(trialStart, trialEnd), unfertPoolOutputs$C141$mean, lwd = 2, col = unfertcols[2])


  #Pool 2 14C comparison
  plot(seq(trialStart, trialEnd), irr20PoolOutputs$C142$mean, type = 'l', xlab = 'Year', #ylim = c(10, 300),
       ylab = expression(paste(Delta^{14}, "C")), main = 'P2 14C', lwd = 2, ylim = c(-45,200))
  lines(atm_in, col = 'purple4', lwd=3)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(irr20PoolOutputs$C142$Q95, rev(irr20PoolOutputs$C142$Q05)),
          col = irr20cols[1], border = NA)
  polygon(c(trialStart+seq(0, length(irr10PoolOutputs$C141[,1])-1), rev(trialStart+seq(0, length(irr10PoolOutputs$C141[,1])-1))),
          c(irr10PoolOutputs$C142$Q95, rev(irr10PoolOutputs$C142$Q05)),
          col = irr10cols[1], border = NA)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(dryPoolOutputs$C142$Q95, rev(dryPoolOutputs$C142$Q05)),
          col = drycols[1], border = NA)
  lines(trialStart+seq(0, length(irr10PoolOutputs$C141[,1])-1), irr10PoolOutputs$C142$mean, lwd = 2, col = irr20cols[2])
  lines(seq(trialStart, trialEnd), irr20PoolOutputs$C142$mean, lwd = 2, col = irr10cols[2])
  lines(seq(trialStart, trialEnd), dryPoolOutputs$C142$mean, lwd = 2, col = drycols[2])


  plot(seq(trialStart, trialEnd), highfertPoolOutputs$C142$mean, type = 'l', xlab = 'Year', #ylim = c(10, 300),
       ylab = expression(paste(Delta^{14}, "C")), main = 'P2 14C', lwd = 2, ylim = c(-45,200))
  lines(atm_in, col = 'purple4', lwd=3)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(highfertPoolOutputs$C142$Q95, rev(highfertPoolOutputs$C142$Q05)),
          col = highcols[1], border = NA)
  polygon(c(trialStart+seq(0, length(resfertPoolOutputs$C141[,1])-1), rev(trialStart+seq(0, length(resfertPoolOutputs$C141[,1])-1))),
          c(resfertPoolOutputs$C142$Q95, rev(resfertPoolOutputs$C142$Q05)),
          col = rescols[1], border = NA)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(unfertPoolOutputs$C142$Q95, rev(unfertPoolOutputs$C142$Q05)),
          col = unfertcols[1], border = NA)
  lines(trialStart+seq(0, length(resfertPoolOutputs$C141[,1])-1), resfertPoolOutputs$C142$mean, lwd = 2, col = highcols[2])
  lines(seq(trialStart, trialEnd), highfertPoolOutputs$C142$mean, lwd = 2, col = rescols[2])
  lines(seq(trialStart, trialEnd), unfertPoolOutputs$C142$mean, lwd = 2, col = unfertcols[2])

  #Respired 14C comparison
  plot(seq(trialStart, trialEnd), irr20PoolOutputs$R14$mean, type = 'l', xlab = 'Year', #ylim = c(10, 300),
       ylab = expression(paste(Delta^{14}, "C")), main = 'Respired 14C', lwd = 2)
  lines(atm_in, col = 'purple4', lwd=3)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(irr20PoolOutputs$R14$Q95, rev(irr20PoolOutputs$R14$Q05)),
          col = irr20cols[1], border = NA)
  polygon(c(trialStart+seq(0, length(irr10PoolOutputs$R14[,1])-1), rev(trialStart+seq(0, length(irr10PoolOutputs$R14[,1])-1))),
          c(irr10PoolOutputs$R14$Q95, rev(irr10PoolOutputs$R14$Q05)),
          col = irr10cols[1], border = NA)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(dryPoolOutputs$R14$Q95, rev(dryPoolOutputs$R14$Q05)),
          col = drycols[1], border = NA)
  lines(trialStart+seq(0, length(irr10PoolOutputs$R14[,1])-1), irr10PoolOutputs$R14$mean, lwd = 2, col = irr20cols[2])
  lines(seq(trialStart, trialEnd), irr20PoolOutputs$R14$mean, lwd = 2, col = irr10cols[2])
  lines(seq(trialStart, trialEnd), dryPoolOutputs$R14$mean, lwd = 2, col = drycols[2])

  plot(seq(trialStart, trialEnd), highfertPoolOutputs$R14$mean, type = 'l', xlab = 'Year', #ylim = c(10, 300),
       ylab = expression(paste(Delta^{14}, "C")), main = 'Respired 14C', lwd = 2)
  lines(atm_in, col = 'purple4', lwd=3)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(highfertPoolOutputs$R14$Q95, rev(highfertPoolOutputs$R14$Q05)),
          col = highcols[1], border = NA)
  polygon(c(trialStart+seq(0, length(resfertPoolOutputs$R14[,1])-1), rev(trialStart+seq(0, length(resfertPoolOutputs$R14[,1])-1))),
          c(resfertPoolOutputs$R14$Q95, rev(resfertPoolOutputs$R14$Q05)),
          col = rescols[1], border = NA)
  polygon(c(seq(trialStart, trialEnd), rev(seq(trialStart, trialEnd))),
          c(unfertPoolOutputs$R14$Q95, rev(unfertPoolOutputs$R14$Q05)),
          col = unfertcols[1], border = NA)
  lines(trialStart+seq(0, length(resfertPoolOutputs$R14[,1])-1), resfertPoolOutputs$R14$mean, lwd = 2, col = highcols[2])
  lines(seq(trialStart, trialEnd), highfertPoolOutputs$R14$mean, lwd = 2, col = rescols[2])
  lines(seq(trialStart, trialEnd), unfertPoolOutputs$R14$mean, lwd = 2, col = unfertcols[2])


  aggregate(irrParms$Value~irrParms$Parameter+irrParms$Trial, FUN = sd)

  # Plot with normalized values for paper
  AppendMe <- function(dfNames) {
    do.call(rbind, lapply(dfNames, function(x) {
      cbind(get(x), source = x)
    }))
  }

  np1 <- data.frame(
    Trial = c('Unfert', 'Res. Fert', 'High Fert'),
    Value = c(mean(unlist(unfertPoolOutputs$Ct1[(trialEnd-trialStart-4),1:1000])),
              mean(unlist(resfertPoolOutputs$Ct1[(trialEnd-trialStart-4),1:1000])),
              mean(unlist(highfertPoolOutputs$Ct1[(trialEnd-trialStart-4),1:1000]))),
    SD = c(sd(unlist(unfertPoolOutputs$Ct1[(trialEnd-trialStart+1),1:1000])),
           sd(unlist(resfertPoolOutputs$Ct1[(trialEnd-trialStart+1),1:1000])),
           sd(unlist(highfertPoolOutputs$Ct1[(trialEnd-trialStart+1),1:1000]))))
  np1$norm <- np1$Value / np1$Value[1]
  np1$normSD <- np1$SD / np1$Value

  np2 <- data.frame(
    Trial = c('Unfert', 'Res. Fert', 'High Fert'),
    Value = c(mean(unlist(unfertPoolOutputs$Ct2[(trialEnd-trialStart-4),1:1000])),
              mean(unlist(resfertPoolOutputs$Ct2[(trialEnd-trialStart-4),1:1000])),
              mean(unlist(highfertPoolOutputs$Ct2[(trialEnd-trialStart-4),1:1000]))),
    SD = c(sd(unlist(unfertPoolOutputs$Ct2[(trialEnd-trialStart+1),1:1000])),
           sd(unlist(resfertPoolOutputs$Ct2[(trialEnd-trialStart+1),1:1000])),
           sd(unlist(highfertPoolOutputs$Ct2[(trialEnd-trialStart+1),1:1000]))))
  np2$norm <- np2$Value / np2$Value[1]
  np2$normSD <- np2$SD / np2$Value

  #Sequestration rate
  seqRlm1 <- summary(lm(soilC~time, data = filter(soilCun, time >= trialStart & time <= trialEnd)))
  seqRlm2 <- summary(lm(soilC~time, data = filter(soilCres, time >= trialStart & time <= trialEnd)))
  seqRlm3 <- summary(lm(soilC~time, data = filter(soilChigh, time >= trialStart & time <= trialEnd)))

  seqR <- data.frame(
    Trial = c('Unfert', 'Res. Fert', 'High Fert'),
    Value = c(seqRlm1$coefficients[2,1],
              seqRlm2$coefficients[2,1],
              seqRlm3$coefficients[2,1]),
    SD = c(seqRlm1$coefficients[2,2],
           seqRlm2$coefficients[2,2],
           seqRlm3$coefficients[2,2])
  )
  seqR$norm <- seqR$Value / seqR$Value[1]
  seqR$normSD <- seqR$SD / seqR$Value[1]

  k1zf$trial <- factor(k2zf$trial, levels = c('Unfert', 'Res. Fert', 'High Fert'), ordered = TRUE)
  nk1 <- aggregate(k1zf$Value~k1zf$trial, FUN = mean)
  colnames(nk1) <- c('Trial','Value')
  nk1$norm <- nk1$Value / nk1[1,2]
  nk1$SD <- aggregate(k1zf$Value~k1zf$trial, FUN = sd)[,2]
  nk1$normSD <- nk1$SD / nk1$Value

  k2zf$trial <- factor(k2zf$trial, levels = c('Unfert', 'Res. Fert', 'High Fert'), ordered = TRUE)
  nk2 <- aggregate(k2zf$Value~k2zf$trial, FUN = mean)
  colnames(nk2) <- c('Trial','Value')
  nk2$norm <- nk2$Value / nk2[1,2]
  nk2$SD <- aggregate(k2zf$Value~k2zf$trial, FUN = sd)[,2]
  nk2$normSD <- nk2$SD / nk2$Value

  a21zf$trial <- factor(a21zf$trial, levels = c('Unfert', 'Res. Fert', 'High Fert'), ordered = TRUE)
  na21 <- aggregate(a21zf$Value~a21zf$trial, FUN = mean)
  colnames(na21) <- c('Trial','Value')
  na21$norm <- na21$Value / na21[1,2]
  na21$SD <- aggregate(a21zf$Value~a21zf$trial, FUN = sd)[,2]
  na21$normSD <- na21$SD / na21$Value

  inNorm <- data.frame(Trial = c('Unfert', 'Res. Fert', 'High Fert'),
                       Value = c(mean(inputUn$inputs), mean(inputRes$inputs), mean(inputHigh$inputs)),
                       SD = c(sd(inputUn$inputs), sd(inputRes$inputs), sd(inputHigh$inputs)))
  inNorm$norm <- inNorm$Value / inNorm$Value[1]
  inNorm$normSD <- inNorm$SD / inNorm$Value

  capEff <- data.frame(
    Trial = c('Unfert', 'Res. Fert', 'High Fert'),
    Value = seqR$Value / inNorm$Value)
  capEff$norm <- capEff$Value / capEff$Value[1]
  capEff$SD <- capEff$normSD <- NA

  p1Res <- data.frame(
    Trial = c('Unfert', 'Res. Fert', 'High Fert'),
    Value = (np1$Value * nk1$Value) - (np1$Value * nk1$Value * na21$Value),
    SD = NA,
    normSD = NA #(np1$normSD + nk1$normSD) /2
  )
  p1Res$norm <- p1Res$Value / p1Res$Value[1]

  p2Res <- data.frame(
    Trial = c('Unfert', 'Res. Fert', 'High Fert'),
    Value = np2$Value * nk2$Value,
    SD = NA,
    normSD = NA #(np2$normSD + nk2$normSD) /2
  )
  p2Res$norm <- p2Res$Value / p2Res$Value[1]

  sysRes <- data.frame(
    Trial = c('Unfert', 'Res. Fert', 'High Fert'),
    Value = p2Res$Value + p1Res$Value,
    SD = NA,
    normSD = NA #(p1Res$normSD + p2Res$normSD) /2
  )
  sysRes$norm <- sysRes$Value / sysRes$Value[1]

  nPlotFert <- AppendMe(c('inNorm', 'np1','np2','nk1', 'nk2', 'sysRes', 'capEff','seqR'))
  nPlotFert <- AppendMe(c('inNorm', 'np1','np2','nk1', 'nk2', 'sysRes', 'capEff'))
  nPlotFert$Trial <- factor(nPlotFert$Trial, levels = c('Unfert', 'Res. Fert','High Fert'), ordered = TRUE)

  # print(ggplot(nPlotFert, aes(x = source, y = norm, color = Trial)) + geom_abline(slope = 0, intercept = 1, linetype = 'dashed') +
  #         geom_errorbar(aes(ymin = norm - normSD, ymax = norm + normSD), width = 0.4, position = position_dodge(0.9)) +
  #         geom_errorbar(aes(ymin = norm, ymax = norm), width = 0.4, position = position_dodge(0.9)) +
  #         geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 0.03,shape = nPlotFert$Trial, size = 2) +
  #         scale_color_manual(values = c(unfertcols[2], rescols[2], highcols[2])) + theme_bw() +
  #         scale_fill_manual(values = c(unfertcols[2], rescols[2], highcols[2])) +
  #         scale_shape_manual(values = c(21, 22, 24)) +
  #         ylab('Proportional Difference')+ xlab('') +
  #         scale_x_discrete(labels = c('Inputs','P1 Size', 'P2 Size',
  #                                     'k1', 'k2','Respiration','Capture\nEfficiency','Sequestration\nRate'))+
  #         theme(axis.text=element_text(size=18), legend.text = element_text(size = 12),
  #               legend.title = element_text(size = 14, face = 'bold'),
  #               plot.title = element_text(size = 18, face = 'bold'),
  #               axis.title=element_text(size=14,face="bold"),
  #               strip.text.y = element_text(size = 12, face = 'bold')) + theme_bw())

  nPlotFert$norm <- nPlotFert$norm - 1

  nPlotFert$source = factor(nPlotFert$source, levels = c("inNorm", 'np1', 'np2', 'nk1', 'nk2','sysRes','capEff'))

  ggplot(nPlotFert, aes(x = source, y = norm, fill = Trial)) +
    geom_abline(slope = 0, intercept = 0, linetype = 'dashed') +
    geom_errorbar(aes(ymin = norm - normSD, ymax = norm + normSD), width = 0.35, position = position_dodge(0.9)) +
    geom_dotplot(aes(y = norm), position = position_dodge2(preserve = 'single'), binaxis = 'y',
                 method = "histodot", binwidth = 0.03, dotsize = 4, stackdir = 'center') +
    scale_fill_manual(values = c(unfertcols[2], rescols[2], highcols[2]))+ theme_bw() +
    ylab('Proportional Difference')+ xlab('') +
    scale_x_discrete(labels = c('Inputs','P1 Size', 'P2 Size', 'k1', 'k2','Respiration','Capture\nEfficiency','Sequestration\nRate'))+
    theme(axis.text=element_text(size=14),
          legend.text = element_text(size = 130),
          legend.title = element_text(size = 14, face = 'bold'),
          plot.title = element_text(size = 18, face = 'bold'),
          axis.title=element_text(size=14,face="bold"),
          strip.text.y = element_text(size = 12, face = 'bold')) + theme_bw()


  ### Custom flow diagrams
  sets <- c('irr', 'fert')

  # includes arrows between boxes
  pm <- par(mfrow = c(1, 1))
  M  <- matrix(nrow = 5, ncol = 5, byrow = TRUE, data = 0)
  arrlwd <- matrix(nrow = 5, ncol = 5, byrow = TRUE, data = 0)
  arr.length <- matrix(nrow = 5, ncol = 5, byrow = TRUE, data = 0)
  names = c("Inputs", 'P1','P2', 'P1 Resp.','P2 Resp')
  names = c('')

  arrlwd[2,1] <- Ins[i]
  arrlwd[3,2] <- (p1size[i] * k1[i] * a21[i])
  arrlwd[4,2] <- (p1size[i] * k1[i]) - (p1size[i] * k1[i] * a21[i])
  arrlwd[5,3] <- p2size[i] * k2[i]

  M[2,1] <- M[3,2] <- M[4,2] <- M[5,3] <- ''

  poz <- matrix(ncol = 2, nrow = 5)
  poz[,1] <- c(0.3,  0.3, 0.3, 0.8, 0.8)
  poz[,2] <- c(0.95, 0.6, 0.15, 0.6, 0.15)

  for(set in sets){

    q = 0.05
    if(set == 'irr'){
      p1size <- irrP1sizeM$Mean
      p2size <- irrP2sizeM$Mean

      k1 <- aggregate(filter(irrParms, Parameter == 'k1')[,2]~filter(irrParms, Parameter == 'k1')[,3], FUN = mean)[,2]
      k2 <- aggregate(filter(irrParms, Parameter == 'k2')[,2]~filter(irrParms, Parameter == 'k2')[,3], FUN = mean)[,2]
      a21 <- aggregate(filter(irrParms, Parameter == 'a21')[,2]~filter(irrParms, Parameter == 'a21')[,3], FUN = mean)[,2]
      slow <- aggregate(filter(irrParms, Parameter == 'SlowProp')[,2]~filter(irrParms, Parameter == 'SlowProp')[,3], FUN = mean)[,2]

      firstCz <- c(Cmod0d, Cmod010, Cmod020)

      boxcols <- c(drycols[1], irr10cols[1], irr20cols[1])
      #Ins <- list(inputDry, input10,input20)
      Ins <- irrInputs

      boxcols = list(c(drycols[1], drycols[1], drycols[1], drycols[1], drycols[1]),
                     c(irr10cols[1], irr10cols[1], irr10cols[1], irr10cols[1], irr10cols[1]),
                     c(irr20cols[1], irr20cols[1], irr20cols[1], irr20cols[1], irr20cols[1]))

    }

    if(set == 'fert'){

      p1size <- fertP1sizeM$Mean
      p2size <- fertP2sizeM$Mean

      k1 <- aggregate(filter(fertParms, Parameter == 'k1')[,2]~filter(fertParms, Parameter == 'k1')[,3], FUN = mean)[,2]
      k2 <- aggregate(filter(fertParms, Parameter == 'k2')[,2]~filter(fertParms, Parameter == 'k2')[,3], FUN = mean)[,2]
      a21 <- aggregate(filter(fertParms, Parameter == 'a21')[,2]~filter(fertParms, Parameter == 'a21')[,3], FUN = mean)[,2]
      slow <- aggregate(filter(fertParms, Parameter == 'SlowProp')[,2]~filter(fertParms, Parameter == 'SlowProp')[,3], FUN = mean)[,2]

      firstCz <- c(Cmod0u, Cmod0r, Cmod0h)

      # firstCz <- c(
      #   mean(as.numeric(unfertPoolOutputs$Ct1[1,])) + mean(as.numeric(unfertPoolOutputs$Ct2[1,])),
      #   mean(as.numeric(resfertPoolOutputs$Ct1[1,])) + mean(as.numeric(resfertPoolOutputs$Ct2[1,])),
      #   mean(as.numeric(highfertPoolOutputs$Ct1[1,])) + mean(as.numeric(highfertPoolOutputs$Ct2[1,]))
      # )

      boxcols <- c(unfertcols[1], rescols[1], highcols[1])
      Ins <- fertInputs

    }

    #Ins_std <- Ins/max(Ins)
    p1_std <- p1size/max(p2size)
    p2_std <- p2size/max(p2size)

    #Ins = c(mean(Ins[[1]][,2]),mean(Ins[[2]][,2]),mean(Ins[[3]][,2]))

    Insn <- (((Ins - min(Ins)) * 0.5) / (max(Ins) - min(Ins)) + 0.5)

    p1r <- ((p1size * k1) - (p1size * k1 * a21))
    p1rn <- (((p1r - min(p1r)) * 0.5) / (max(p1r) - min(p1r)) + 0.5)

    p2r <- p2size * k2
    p2rn <- (((p2r - min(p2r)) * 0.5) / (max(p2r) - min(p2r)) + 0.5)

    a21r <- (p1size * k1 * a21)

    k2n <- (((k2 - min(k2)) * 0.5) / (max(k2) - min(k2)) + 0.5)
    a21n <- (((a21 - min(a21)) * 0.5) / (max(a21) - min(a21)) + 0.5)
    p1sizen <- (((p1size - min(p1size,p2size)) * 0.5) / (max(p1size,p2size) - min(p1size,p2size)) + 0.5)
    p2sizen <- (((p2size - min(p2size,p1sizen)) * 0.5) / (max(p2size,p1sizen) - min(p2size,p1sizen)) + 0.5)

    p1r <- ((p1size * k1) - (p1size * k1 * a21))
    p2r <- p2size * k2

    scaler <- 0.7

    Insn <- (((Ins - min(Ins,p1r , p2r,a21r)) * scaler) / (max(Ins,p1r, p2r,a21r) - min(Ins,p1r , p2r,a21r)) + (1-scaler))
    p1rn <- (((p1r - min(Ins,p1r , p2r,a21r)) * scaler) / (max(Ins,p1r, p2r,a21r) - min(Ins,p1r , p2r,a21r)) + (1-scaler))
    p2rn <- (((p2r - min(Ins,p1r , p2r,a21r)) * scaler) / (max(Ins,p1r, p2r,a21r) - min(Ins,p1r , p2r,a21r)) + (1-scaler))
    a21n <- (((a21r - min(Ins,p1r, p2r,a21r)) * scaler) / (max(Ins,p1r, p2r,a21r) - min(Ins,p1r , p2r,a21r)) + (1-scaler))

    if(set == 'fert'){scaler = 0.1} else {scaler = 0.3}
    p1sizen <- (((p1size - min(p1size,p2size)) * scaler) / (max(p1size,p2size) - min(p1size,p2size)) + (1-scaler))
    p2sizen <- (((p2size - min(p2size,p1size)) * scaler) / (max(p2size,p1size) - min(p2size,p1size)) + (1-scaler))

    prop <- 'n'

    #par(mfrow = c(1,1))
    for(i in c(1,3)){

      arrlwd <- matrix(nrow = 5, ncol = 5, byrow = TRUE, data = 0)

      arrscale <- 4
      ### Arrow sizes
      arrlwd[2,1] <- (1+Insn[i])^arrscale
      arrlwd[3,2] <- (1+a21n[i])^arrscale
      arrlwd[4,2] <- (1+p1rn[i])^arrscale
      arrlwd[5,3] <- (1+p2rn[i])^arrscale

      arr.width <- arrlwd
      arr.width[] <- NA
      arr.width[2,1] <- (1+Insn[i])^arrscale
      arr.width[3,2] <- (10+a21n[i]^arrscale)

      arr.pos <- arrlwd
      arr.pos[] <- NA
      arr.pos[2,1] <- 0.5
      arr.pos[3,2] <- 0.48
      arr.pos[4,2] <- 0.7
      arr.pos[5,3] <- 0.7

      if(prop == 'y'){
        box.size <- c(sqrt(Insn[i]/pi)*0.15,
                      sqrt(p1sizen[i])*0.22,
                      sqrt(p2sizen[i])*0.22,
                      sqrt(p1rn[i]/pi) * 0.15,
                      sqrt(p2rn[i]/pi)* 0.15)
      } else {
        box.size <- c(Insn[i]*0.11^0.97,
                      p1sizen[i]*0.29,
                      p2sizen[i]*0.29,
                      (p1rn[i] * 0.11)^0.97,
                      (p2rn[i]*0.11)^0.97)
      }
      # boxscale = 0.005
      # box.size <- c((Insn[i]+1)^boxscale,
      #               (1+p1sizen[i])^boxscale,
      #               (p2sizen[i]+1)^boxscale,
      #               (1+p1rn[i])^boxscale,
      #               (p2rn[i])^boxscale)
      par(mar=c(1,1,1,1))
      par(mar=c(0,0,0,0))


      print(plotmat(M, curve = 0, pos = poz, arr.lwd = arrlwd, box.size = box.size, #main = 'Flow Diagram: Check Color for trial name',
                    box.type = c('multi', 'rect','rect','circle','circle'), relsize = 0.7,
                    name = names, txt.yadj = c(0.5,0.5,0.5,-05,-01.5), box.col = unlist(boxcols[i]), shadow.size = 0.001,
                    arr.width = arrlwd/20, arr.length = arrlwd/20, arr.pos = arr.pos, arr.type = 'triangle'))
    }
  }


  dryPars$trial = 'Dry'
  irr10Pars$trial = 'Irr. 10'
  irr20Pars$trial = 'Irr. 20'

  junker <- data.frame(irrTTs$name, irrTTs$MeanTT)
  junker2 <- data.frame(irrSAs$name, irrSAs$MeanSA)
  junker3 <- data.frame(trial = irrStocks$name, Value = irrStocks$stock, Parameter = 'ssStock')
  colnames(junker) <- c('trial', 'Value')
  colnames(junker2) <- c('trial', 'Value')
  junker$Parameter <- 'MeanTT'
  junker2$Parameter <- 'MeanSA'

  allIrrs <- rbind(dryPars, irr10Pars, irr20Pars, junker, junker2, junker3)
  if(bootz == TRUE){
    irrBST <-groupwiseMean(Value~Parameter+trial,
                           data = allIrrs,
                           conf = 0.99,
                           digits = 3,
                           R = 1000,
                           boot = TRUE,
                           bca = TRUE)
    irrBST <- subset(irrBST, select= c('Parameter', 'trial', 'Mean', 'Boot.mean','Trad.lower', 'Trad.upper',
                                       'Bca.lower', 'Bca.upper'))

    pushViewport(viewport(y=.25,height=.5))
    jl <- tableGrob(irrBST, theme = ttheme_default(base_size = 7.5))
    grid.newpage()
    grid.draw(jl)
  }

  # Plot with normalized values for paper
  np1 <- data.frame(
    Trial = c('Dry', 'Irr. 10', 'Irr. 20'),
    Value = c(mean(unlist(dryPoolOutputs$Ct1[length(trialEnd-trialStart+1),1:1000])),
              mean(unlist(irr10PoolOutputs$Ct1[length(trialEnd-trialStart+1),1:1000])),
              mean(unlist(irr20PoolOutputs$Ct1[length(trialEnd-trialStart+1),1:1000]))),
    SD = c(sd(unlist(dryPoolOutputs$Ct1[length(trialEnd-trialStart+1),1:1000])),
           sd(unlist(irr10PoolOutputs$Ct1[length(trialEnd-trialStart+1),1:1000])),
           sd(unlist(irr20PoolOutputs$Ct1[length(trialEnd-trialStart+1),1:1000]))))
  np1$norm <- np1$Value / np1$Value[1]
  np1$normSD <- np1$SD / np1$Value

  np2 <- data.frame(
    Trial = c('Dry', 'Irr. 10', 'Irr. 20'),
    Value = c(mean(unlist(dryPoolOutputs$Ct2[length(trialEnd-trialStart+1),1:1000])),
              mean(unlist(irr10PoolOutputs$Ct2[length(trialEnd-trialStart+1),1:1000])),
              mean(unlist(irr20PoolOutputs$Ct2[length(trialEnd-trialStart+1),1:1000]))),
    SD = c(sd(unlist(dryPoolOutputs$Ct2[length(trialEnd-trialStart+1),1:1000])),
           sd(unlist(irr10PoolOutputs$Ct2[length(trialEnd-trialStart+1),1:1000])),
           sd(unlist(irr20PoolOutputs$Ct2[length(trialEnd-trialStart+1),1:1000]))))
  np2$norm <- np2$Value / np2$Value[1]
  np2$normSD <- np2$SD / np2$Value

  #Sequestration rate
  seqRlm1 <- summary(lm(soilC~time, data = filter(soilCdry, time >= trialStart & time <= trialEnd)))
  seqRlm2 <- summary(lm(soilC~time, data = filter(soilC10, time >= trialStart & time <= trialEnd)))
  seqRlm3 <- summary(lm(soilC~time, data = filter(soilC20, time >= trialStart & time <= trialEnd)))

  #seqRlm1 <- summary(lm(soilC~poly(time,2), data = filter(soilC10, time >= trialStart & time <= trialEnd)))

  seqR <- data.frame(
    Trial = c('Dry', 'Irr. 10', 'Irr. 20'),
    Value = c(seqRlm1$coefficients[2,1],
              seqRlm2$coefficients[2,1],
              seqRlm3$coefficients[2,1]),
    SD = c(seqRlm1$coefficients[2,2],
           seqRlm2$coefficients[2,2],
           seqRlm3$coefficients[2,2])
  )
  seqR$norm <- seqR$Value / seqR$Value[1]
  seqR$normSD <- seqR$SD / seqR$Value[1]

  nk1 <- aggregate(k1zi$Value~k1zi$trial, FUN = mean)
  colnames(nk1) <- c('Trial','Value')
  nk1$norm <- nk1$Value / nk1[1,2]
  nk1$SD <- aggregate(k1zi$Value~k1zi$trial, FUN = sd)[,2]
  nk1$normSD <- nk1$SD / nk1$Value

  nk2 <- aggregate(k2zi$Value~k2zi$trial, FUN = mean)
  colnames(nk2) <- c('Trial','Value')
  nk2$norm <- nk2$Value / nk2[1,2]
  nk2$SD <- aggregate(k2zi$Value~k2zi$trial, FUN = sd)[,2]
  nk2$normSD <- nk2$SD / nk2$Value

  #irrInputs <- aggregate(InputWin$Input~InputWin$Trial, FUN = mean)

  inNorm <- data.frame(Trial = c('Dry', 'Irr. 10', 'Irr. 20'),
                       Value = irrInputs,
                       SD = irrInputsSD)
  inNorm$norm <- inNorm$Value / inNorm$Value[1]
  inNorm$normSD <- inNorm$SD / inNorm$Value

  capEff <- data.frame(
    Trial = c('Dry', 'Irr. 10', 'Irr. 20'),
    Value = c(7.8, 5.3, 4.6))#seqR$Value / inNorm$Value)

  capEff$norm <- capEff$Value / capEff$Value[1]
  capEff$SD <- capEff$normSD <- NA

  p1Res <- data.frame(
    Trial = c('Dry', 'Irr. 10', 'Irr. 20'),
    Value = np1$Value * nk1$Value,
    SD = NA,
    normSD = NA #(np1$normSD + nk1$normSD) /2
  )
  p1Res$norm <- p1Res$Value / p1Res$Value[1]

  p2Res <- data.frame(
    Trial = c('Dry', 'Irr. 10', 'Irr. 20'),
    Value = np2$Value * nk2$Value,
    SD = NA,
    normSD = NA #(np2$normSD + nk2$normSD) /2
  )
  p2Res$norm <- p2Res$Value / p2Res$Value[1]

  sysRes <- data.frame(
    Trial = c('Dry', 'Irr. 10', 'Irr. 20'),
    Value = p2Res$Value + p1Res$Value,
    SD = NA,
    normSD = NA #(p1Res$normSD + p2Res$normSD) /2
  )
  sysRes$norm <- sysRes$Value / sysRes$Value[1]

  nPlotIrr <- AppendMe(c('inNorm', 'np1','np2','nk1', 'nk2', 'sysRes', 'capEff','seqR'))
  nPlotIrr <- AppendMe(c('inNorm', 'np1','np2','nk1', 'nk2', 'sysRes', 'capEff'))

  nPlotIrr$norm <- nPlotIrr$norm - 1

  ggplot(nPlotIrr, aes(x = source, y = norm, fill = Trial)) + geom_abline(slope = 0, intercept = 1, linetype = 'dashed') +
    geom_errorbar(aes(ymin = norm - normSD, ymax = norm + normSD), width = 0.4, position = position_dodge(0.9)) +
    geom_bar(position = 'dodge', stat = 'identity') +
    #geom_point(aes(nPlotIrr$source, nPlotIrr$norm, position_dodge(width = 0.1)), shape =nPlotIrr$Trial,  na.rm=TRUE) +
    scale_fill_manual(values = c(drycols[2], irr10cols[2], irr20cols[2]))+ theme_bw() +
    ylab('Proportional Difference')+ xlab('') +
    scale_x_discrete(labels = c('Inputs','P1 Size', 'P2 Size', 'k1', 'k2','Respiration','Capture\nEfficiency','Sequestration\nRate'))+
    theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
          legend.title = element_text(size = 14, face = 'bold'),
          plot.title = element_text(size = 18, face = 'bold'),
          axis.title=element_text(size=14,face="bold"),
          strip.text.y = element_text(size = 12, face = 'bold')) + theme_bw()

  print(irrNormgg)

  nPlotIrr$source = factor(nPlotIrr$source, levels = c("inNorm", 'np1', 'np2', 'nk1', 'nk2','sysRes','capEff'))

  ggplot(nPlotIrr, aes(x = source, y = norm, fill = Trial)) + ylim(-0.5,0.535) +
    geom_abline(slope = 0, intercept = 0, linetype = 'dashed') +
    geom_errorbar(aes(ymin = norm - normSD, ymax = norm + normSD), width = 0.4, position = position_dodge(0.9)) +
    geom_dotplot(aes(y = norm), position = position_dodge2(preserve = 'single'), binaxis = 'y',
                 method = "histodot", binwidth = 0.03, dotsize = 3, stackdir = 'center') +
    scale_fill_manual(values = c(drycols[2], irr10cols[2], irr20cols[2]))+ theme_bw() +
    ylab('Proportional Difference')+ xlab('') +
    scale_x_discrete(labels = c('Inputs','P1 Size', 'P2 Size', 'k1', 'k2','Respiration','Capture\nEfficiency','Sequestration\nRate'))+
    theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
          legend.title = element_text(size = 14, face = 'bold'),
          plot.title = element_text(size = 18, face = 'bold'),
          axis.title=element_text(size=14,face="bold"),
          strip.text.y = element_text(size = 12, face = 'bold')) + theme_bw()


  # dryTTdist %>%
  #   gather(rep, Dry, -years) %>%
  #   left_join(irr10TTdist %>%
  #               gather(rep, 'Irr. 10', -years), by= c("years", "rep")
  #   ) %>%
  #   left_join(irr20TTdist %>%
  #               gather(rep, 'Irr. 20', -years), by = c('years', 'rep')
  #   ) %>%
  #   gather(trial, TTdist, Dry:'Irr. 20', factor_key = TRUE) %>%
  #   mutate(
  #     mass = case_when(trial == 'Dry' ~ TTdist * mean(
  #       filter(InputWin, InputWin$Trial == 'Dry' & Year >= trialStart & Year <= trialEnd)$Input),
  #       trial == 'Irr. 10' ~ TTdist * mean(
  #         filter(InputWin, InputWin$Trial == 'Irr10' & Year >= trialStart & Year <= trialEnd)$Input),
  #       trial == 'Irr. 20' ~ TTdist * mean(
  #         filter(InputWin, InputWin$Trial == 'Irr20' & Year >= trialStart & Year <= trialEnd)$Input))) %>% glimpse()
  #   group_by(years, trial) %>% glimpse()
  #   summarise(
  #     quantile_005_mass = quantile(mass, probs = 0.05),
  #     quantile_095_mass = quantile(mass, probs = 0.95),
  #     mean_mass = mean(mass)
  #   ) %>%
  #   ggplot(aes(x=years, y=mean_mass, col=trial, fill = trial,
  #              ymin = quantile_005_mass, ymax = quantile_095_mass)) + xlim(0, 25) + geom_line(size = 1.5) +
  #   geom_ribbon(alpha = 0.6) + ggtitle('Mass-weighted Transit Time Distribution') +
  #   scale_color_manual(values=c(drycols[1], irr10cols[1], irr20cols[1])) +
  #   scale_fill_manual(values=c(drycols[2], irr10cols[2], irr20cols[2])) +
  #   ylab('Mg C ha') + xlab('Years') +
  #   theme(text=element_text(size=21), panel.background = element_blank(), legend.position = 'none',
  #         axis.line = element_line(color = 'black', size = 1)) +
  #   guides(fill=guide_legend(title="Trial"))


  if(trialStart == 1985){
    irrSAz85 <- filter(irrSAz, fertSAz$trialStart == '1985 - 2010')
    meanStocks <- aggregate(irrStocks$stock~irrStocks$name, FUN = mean)[,2]

    irrSAz85$wmean <- c(drySAdist$mean * meanStocks[1],
                        irr10SAdist$mean * meanStocks[2],
                        irr20SAdist$mean * meanStocks[3])
    irrSAz85$wQ95 <- c(drySAdist$Q95 * meanStocks[1],
                       irr10SAdist$Q95 * meanStocks[2],
                       irr20SAdist$Q95 * meanStocks[3])
    irrSAz85$wQ05 <- c(drySAdist$Q05 * meanStocks[1],
                       irr10SAdist$Q05 * meanStocks[2],
                       irr20SAdist$Q05 * meanStocks[3])

    print(ggplot(irrStocks, aes(x = stock, fill=name)) + geom_histogram() + facet_grid(irrStocks$name))
    print(ggplot(fertStocks, aes(x = stock, fill=name)) + geom_histogram() + facet_grid(fertStocks$name))

    print(ggplot(irrSAz85, aes(x = years, y = wmean, fill = trial, ymax = wQ95, ymin = wQ05)) + geom_line() +
            facet_grid(irrSAz85$trialStart) + theme_bw() + scale_y_log10() +
            xlim(0, 1000) + scale_fill_manual(values = c(drycols[2], irr10cols[2], irr20cols[2])) +
            geom_ribbon(alpha = .7) + ggtitle('Carbon Stock Age Distributions') + #ylim(0, 0.32) +
            xlab('Years') + ylab('T C ha-1') + guides(fill=guide_legend(title="Trial")) + #ylim(0,1.1) +
            theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                  legend.title = element_text(size = 14, face = 'bold'),
                  plot.title = element_text(size = 18, face = 'bold'),
                  axis.title=element_text(size=14,face="bold"),
                  strip.text.y = element_text(size = 12, face = 'bold')) + theme_bw())

    print(ggplot(irrSAz85, aes(x = years, y = wmean, fill = trial, ymax = wQ95, ymin = wQ05)) + geom_line() +
            facet_grid(irrSAz85$trialStart) + theme_bw() +
            xlim(0, 50) + scale_fill_manual(values = c(drycols[2], irr10cols[2], irr20cols[2])) +
            geom_ribbon(alpha = .7) + ggtitle('Carbon Stock Age Distributions') + #ylim(0, 0.32) +
            xlab('Years') + ylab('T C ha-1') + guides(fill=guide_legend(title="Trial")) + #ylim(0,1.1) +
            theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                  legend.title = element_text(size = 14, face = 'bold'),
                  plot.title = element_text(size = 18, face = 'bold'),
                  axis.title=element_text(size=14,face="bold"),
                  strip.text.y = element_text(size = 12, face = 'bold')) + theme_bw())


    meanStocks <- aggregate(fertStocks$stock~fertStocks$name, FUN = mean)[,2]
    sdStocks <- aggregate(fertStocks$stock~fertStocks$name, FUN = sd)[,2]


    fertSAz85 <- filter(fertSAz, fertSAz$trialStart == '1985 - 2010')

    fertSAz85$wmean <- c(unfertSAdist$mean * meanStocks[1],
                         resfertSAdist$mean * meanStocks[2],
                         highfertSAdist$mean * meanStocks[3])
    fertSAz85$wQ95 <- c(unfertSAdist$Q95 * meanStocks[1],
                        resfertSAdist$Q95 * meanStocks[2],
                        highfertSAdist$Q95 * meanStocks[3])
    fertSAz85$wQ05 <- c(unfertSAdist$Q05 * meanStocks[1],
                        resfertSAdist$Q05 * meanStocks[2],
                        highfertSAdist$Q05 * meanStocks[3])

    print(ggplot(fertSAz85, aes(x = years, y = wmean, fill = trial, ymax = wQ95, ymin = wQ05)) + geom_line() +
            facet_grid(fertSAz85$trialStart) + theme_bw() +
            xlim(0, 50) + scale_fill_manual(values = c(unfertcols[2], rescols[2], highcols[2])) +
            geom_ribbon(alpha = .7) + ggtitle('Carbon Stock Age Distributions') + #ylim(0, 0.32) +
            xlab('Years') + ylab('T C ha-1') + guides(fill=guide_legend(title="Trial")) + #ylim(0,1.1) +
            theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                  legend.title = element_text(size = 14, face = 'bold'),
                  plot.title = element_text(size = 18, face = 'bold'),
                  axis.title=element_text(size=14,face="bold"),
                  strip.text.y = element_text(size = 12, face = 'bold')) + theme_bw())

    print(ggplot(fertSAz85, aes(x = years, y = wmean, fill = trial, ymax = wQ95, ymin = wQ05)) + geom_line() +
            facet_grid(fertSAz85$trialStart) + theme_bw() + scale_y_log10() +
            xlim(0, 1000) + scale_fill_manual(values = c(unfertcols[2], rescols[2], highcols[2])) +
            geom_ribbon(alpha = .7) + ggtitle('Carbon Stock Age Distributions') + #ylim(0, 0.32) +
            xlab('Years') + ylab('T C ha-1') + guides(fill=guide_legend(title="Trial")) + #ylim(0,1.1) +
            theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
                  legend.title = element_text(size = 14, face = 'bold'),
                  plot.title = element_text(size = 18, face = 'bold'),
                  axis.title=element_text(size=14,face="bold"),
                  strip.text.y = element_text(size = 12, face = 'bold')) + theme_bw())
  }



  ssYearVar <- data.frame(trial = character(), ssYear95 = integer(), Ctotal = double())

  AllIn <- aggregate(InputWin$Input~InputWin$Trial, FUN = mean)
  AllIn <- c(AllIn[4:6,2], AllIn[1:3,2])

  SS95z <- c()
  Csumz <- c()

  uncerts <- list(data.frame(unfert_pred_uncert_85), data.frame(resfert_pred_uncert_85), data.frame(highfert_pred_uncert_85),
                  data.frame(dry_pred_uncert_85), data.frame(irr10_pred_uncert_85), data.frame(irr20_pred_uncert_85))

  C0z <- c(Cmod0u, Cmod0r, Cmod0h, Cmod0d, Cmod010, Cmod020)

  trialList <- c('Unfert',"ResFert",'HighFert',"Dry",'Irr10','Irr20')

  for(x in seq(1,6)){

    stuff = data.frame(uncerts[x])
    inputs <- AllIn[x]
    trial = trialList[x]

    SS95z <- c()
    Csumz <- c()


    for(i in seq(1,1000)){
      # Construct matrices and calculate transit time and system age functions
      ks <- c(stuff$k1[i], stuff$k2[i])
      as <- c(stuff$a21[i] * stuff$k1[i], 0)
      u2 = matrix(c(inputs, 0), ncol = 1)
      A2=diag(-ks)
      A2[2,1] = as[1]

      # Add plenty of extra time to capture distribution tails
      SA2 = systemAge(A = A2, u = u2)
      TT2 = transitTime(A = A2, u = u2)


      #Steady state C stocks
      steadyRun <- TwopSeriesModel(
        t = seq(trialStart, trialStart + 1500),
        ks = c(stuff$k1[i], stuff$k2[i]),
        a21 = stuff$k1[i] * stuff$a21[i],
        C0 = Cmod0 * c(1-stuff$slowProp[i], stuff$slowProp[i]),
        In = inputs
      )

      SCt = getC(steadyRun) # Pool C content
      Csum <- data.frame(years = seq(trialStart, trialStart + 1500), totC = SCt[,1] + SCt[,2])


      SSyear95 = Csum[which.min(abs(Csum$totC - (TT2$meanTransitTime * inputs * 0.95))),1]

      SS95z <- c(SS95z, SSyear95 - 2010)
      Csumz <- c(Csumz, Csum$totC[1500])
      if(i%%50==0){
        print(paste(trial, i))
      }

    }
    dumb <- data.frame(ssYear95 = SS95z, Ctotal = Csumz)
    dumb$trial = trial

    ssYearVar <- rbind(ssYearVar, dumb)


  }

  dim(ssYearVar)

  ssYearVar$trial <- factor(ssYearVar$trial, levels = c("Dry","Irr10","Irr20",'Unfert','ResFert','HighFert'))

  ssYearVar <- subset(ssYearVar, ssYearVar$ssYear95 != 1475)

  ssYearVar %>% filter(ssYear95 > 10) -> ssY
  aggregate(ssY$ssYear95~ssY$trial, FUN = mean)
  aggregate(ssY$ssYear95~ssY$trial, FUN = median)
  aggregate(ssY$ssYear95~ssY$trial, FUN = sd)
  aggregate(ssY$ssYear95~ssY$trial, FUN = quantile)
  ggplot(ssY, aes(x = trial, y = ssYear95)) + geom_boxplot()

  ssYearVar %>% filter(Ctotal < 70 & >30) -> ssC
  aggregate(ssC$Ctotal~ssC$trial, FUN = mean)
  aggregate(ssC$Ctotal~ssC$trial, FUN = median)
  aggregate(ssC$Ctotal~ssC$trial, FUN = sd)
  aggregate(ssC$Ctotal~ssC$trial, FUN = quantile)

  ggplot(ssC, aes(color = trial, x = Ctotal)) + geom_boxplot()
  ggplot(ssY, aes(x = trial, y = ssYear95)) + geom_boxplot()
  ggplot(ssYearVar, aes(x = trial, y = Ctotal)) + geom_boxplot()

  pairwise.t.test(ssC$Ctotal,ssC$trial, pool.sd = FALSE)
  pairwise.t.test(ssC$ssYear95,ssC$trial, pool.sd = FALSE)

  pairwise.wilcox.test(ssC$Ctotal,ssC$trial)
  pairwise.wilcox.test(ssC$ssYear95,ssC$trial)


  trialList = c('Dry', 'Irr. 20', 'Unfert', 'Res. Fert', 'High Fert', 'Irr. 10')
  dev.off()





}

if(trialStart == 1958){
  fertP1958 <- fertParms
  irrP1958 <- irrParms
  allTTs1958 <- allTTs
  allSAs1958 <- allSAs
}
if(trialStart == 1985){
  fertP1985 <- fertParms
  irrP1985 <- irrParms
  allTTs1985 <- allTTs
  allSAs1985 <- allSAs
}
library(BEST)
library(car)
library(ggpubr)

ggplot(k1zf, aes(x = trial, y = Value)) + geom_boxplot()

t.test(resfert_pred_uncert$k1, highfert_pred_uncert$k1)
pairwise.t.test(k1zf$Value, k1zf$trial, p.adjust.method = 'BH')

yov <-  aov(MeanSA ~ name, data = allSAs1985)
summary(yov)
TukeyHSD(yov)

ggplot(allSAs1985, aes(x = name, y = MeanSA)) + geom_boxplot()
pairwise.t.test(allSAs1985$MeanSA, allSAs1985$name, p.adjust.method = 'BH')

for(i in levels(k2z$trial)){
  wilcox.test(filter(k1zf, trial==i)$Value)
  shapiro.test(filter(k1zf, trial==i)$Value)
  jarque.bera.test(filter(k1zf, trial == i)$Value)
  hist(filter(k1zf, trial==i)$Value, breaks = 20)
}

t.test(irr20_pred_uncert$a21, irr10_pred_uncert$a21)
hist(irr20_pred_uncert$a21)
hist(irr10_pred_uncert$a21)
boxplot(dry_pred_uncert$k2,irr10_pred_uncert$k2)

qt(.999, df = 999)*sd(irr10_pred_uncert$k1)/sqrt(999)

n = 1000

boxplot(irr10_pred_uncert$a21, irr20_pred_uncert$a21)

a = mean(irr10_pred_uncert$a21)
s = sd(irr10_pred_uncert$a21)
error = qt(.975, df = n -1)*sd(irr10_pred_uncert$a21)/sqrt(n-1)
left <- a-error
right <- a+error

abline(,,left, col = 'blue')
abline(,,right, col = 'blue')
abline(,,(sd(irr10_pred_uncert$a21) + a), col = 'blue', lty = 3)
abline(,,(a - sd(irr10_pred_uncert$a21)), col = 'blue', lty = 3)
abline(,,a, col = 'blue', lty = 2)

a = mean(irr20_pred_uncert$a21)
s = sd(irr20_pred_uncert$a21)
error = qt(.975, df = n -1)*sd(irr20_pred_uncert$a21)/sqrt(n-1)
left <- a-error
right <- a+error

abline(,,left, col = 'red')
abline(,,right, col = 'red')
abline(,,a, col = 'red', lty = 2)

ks.test(irr20_pred_uncert$a21,  irr10_pred_uncert$a21)

wilcox.test(irr20_pred_uncert$a21,  irr10_pred_uncert$a21,alternative = "two.sided")
pairwise.t.test(k2z$Value, k2z$trial, p.adjust.method = 'BH')

library(tseries)

1 - pt(1:5, df = 1)
qt(.975, df = c(1:10,20,50,100,1000))

tt <- seq(0, 10, len = 21)
ncp <- seq(0, 6, len = 31)
ptn <- outer(tt, ncp, function(t, d) pt(t, df = 3, ncp = d))
t.tit <- "Non-central t - Probabilities"
image(tt, ncp, ptn, zlim = c(0,1), main = t.tit)
persp(tt, ncp, ptn, zlim = 0:1, r = 2, phi = 20, theta = 200, main = t.tit,
      xlab = "t", ylab = "non-centrality parameter",
      zlab = "Pr(T <= t)")

plot(function(x) dt(x, df = 3, ncp = 2), -3, 11, ylim = c(0, 0.32),
     main = "Non-central t - Density", yaxs = "i")

#https://rcompanion.org/handbook/C_03.html
install.packages('rcompanion')
library(rcompanion)

fart <- groupwiseMean(Value~trial+Parameter,
                      data = allIrrs,
                      conf = 0.99,
                      digits = 4,
                      R = 10000,
                      boot = TRUE,
                      bca = TRUE)

Mboot <- boot(filter(k1zf, trial =='Unfert')$Value,
              function(x,i) mean(x[i]),
              R=10000)

mean(Mboot$t[,1])

boot.ci(Mboot,
        conf = 0.99,
        type = c('norm', 'basic','perc','bca'))

groupwiseGeometric(Value~trial,
                   data = k1zf,
                   digits = 3,
                   na.rm =  T)

ggplot(k1zf, aes(y = Value, x = trial)) + geom_boxplot()

m1 = mean(dry_pred_uncert$k1)
m2 = mean(irr10_pred_uncert$k1)


sd1 <- sd(dry_pred_uncert$k1)
sd2 <- sd(irr10_pred_uncert$k1)
num1 <- 1000
num2 <- 1000
se <- sqrt(sd1*sd1/num1+sd2*sd2/num2)
error <- qt(0.975,df=pmin(num1,num2)-1)*se

left <- (m1-m2)-error
right <- (m1-m2)+error

?SoilR::CenturyModel()

library(Rmisc)

fart <- 0

for(i in seq(2, ncol(TTdist))){
  fart1 <- sum(TTdist[,i])
  fart = 0
  j = TTdist[,i]
  k = 1

  while(fart < fart1/2){
    l <- j[k]

    fart = fart + l
    k = k + 1
  }

  print(k)
}

AUC(TTdist$years, TTdist[,i], from = 0, to = k, method = 'spline')


plot(datunfert$time, datunfert$soilc14, col = unfertcols[2], pch = 16, cex = 1)
arrows(datunfert$time, datunfert$soilc14- datunfert$err,datunfert$time, datunfert$soilc14+ datunfert$err,length=0.05, angle=90, code=3)
arrows(datresfert$time, datresfert$soilc14- datresfert$err,datresfert$time, datresfert$soilc14+ datresfert$err,length=0.05, angle=90, code=3)
arrows(dathighfert$time, dathighfert$soilc14- dathighfert$err,dathighfert$time, dathighfert$soilc14+ dathighfert$err,length=0.05, angle=90, code=3)
points(datresfert$time, datresfert$soilc14, col = rescols[2], pch = 16)
points(dathighfert$time, dathighfert$soilc14, col = highcols[2], pch = 16)





for(x in seq(1, length(unfertPoolOutputs))){

  d10 <- c()
  d20 <- c()
  ten20 <- c()

  for(i in seq(1, length(years))){
    ld <- length(c(unlist(data.frame(unfertPoolOutputs[x])[i,])))
    l10 <- length(c(unlist(data.frame(resfertPoolOutputs[x])[i,])))
    l20 <- length(c(unlist(data.frame(highfertPoolOutputs[x])[i,])))

    fart <- data.frame(trial = factor(c(replicate(ld, 'un'),
                                        replicate(l10, 'res'),
                                        replicate(l20, 'high')), ordered = TRUE))

    fart$data<- c(c(unlist(data.frame(unfertPoolOutputs[x])[i,])),
                  c(unlist(data.frame(resfertPoolOutputs[x])[i,])),
                  c(unlist(data.frame(highfertPoolOutputs[x])[i,])))

    #print(tapply(fart$data, fart$trial, FUN = mean))

    ju <- pairwise.t.test(fart$data, fart$trial)

    d10 <- c(d10, ju$p.value[1])
    d20 <- c(d20, ju$p.value[2])
    ten20 <- c(ten20, ju$p.value[4])
  }
  print(x)
  d10years <- years[which(d10 > 0.05)]
  d20years <- years[which(d20 > 0.05)]
  ten20years <- years[which(ten20 > 0.05)]
  print(paste('For', names(dryPoolOutputs)[x], 'Con and mid diff:', d10years))
  print(paste('For', names(dryPoolOutputs)[x], 'Con and high diff:', d20years))
  print(paste('For', names(dryPoolOutputs)[x], 'mid and high diff:', ten20years))s


}

CNfertin <- read.csv('/Users/shane/14Constraint Dropbox/Shane Stoner/IMPRS/NZ/Winchmore_litter_CN.csv')
CNfertin$Trial <- factor(CNfertin$Trial, levels = c("Unfert", 'Res. Fert', "High Fert"), ordered = TRUE)
ggplot(CNfertin, aes(x = Year, y=CN, shape = Trial, fill = Trial)) + geom_point(size = 5) + theme_bw() +
  scale_fill_manual(values = c(unfertcols[2], rescols[2], highcols[2])) + scale_shape_manual(values = c(21, 22, 24)) + ylab('C:N') +
  theme(axis.text=element_text(size=14), legend.text = element_text(size = 12), panel.background = element_blank(),
        legend.title = element_text(size = 14, face = 'bold'),
        plot.title = element_text(size = 18, face = 'bold'),
        axis.title=element_text(size=14,face="bold"),
        strip.text.y = element_text(size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))



print(ggplot(fertC, aes(x = time, y = soilC, color = Trial, fill = Trial, shape = Trial)) +
        xlab('Year') + geom_smooth(method = glm) + scale_shape_manual(values = c(21, 22, 24)) +
        scale_fill_manual(values = c(unfertcols[2], rescols[2], highcols[2])) +
        scale_color_manual(values = c(unfertcols[2], rescols[2], highcols[2]))  + theme_bw() +
        geom_point(color = 1, size = 4) +
        ylim(min(as.numeric(na.omit(fertC$soilC)), as.numeric(na.omit(fertC$soilC))), max(as.numeric(na.omit(fertC$soilC)), as.numeric(na.omit(irrC$soilC)))+2) +
        ylab(expression(bold(paste('Soil C Stock (T ha'^{-1},')')))) +
        theme(axis.text=element_text(size=14), legend.text = element_text(size = 12),
              legend.title = element_text(size = 14, face = 'bold'),
              plot.title = element_text(size = 18, face = 'bold'),
              axis.title=element_text(size=14,face="bold"),
              strip.text.y = element_text(size = 12), panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")))

group_by(irrquantTT, irrquantTT$trial, variable) %>%
  summarise(across(value, sd))

group_by(fertquantTT, fertquantTT$trial, variable) %>%
  summarise(across(value, sd))

irrquantTT2 <- irrquantTT
fertquantTT2 <- fertquantTT

irrquantTT1$year = factor(1958)
irrquantTT2$year = factor(1985)

fart <- filter(irrquantTT1, variable == 'q50')
fart <- rbind(fart, filter(irrquantTT2, variable == 'q50'))
fart <- rbind(fart, filter(fertquantTT1, variable == 'q50'))
fart <- rbind(fart, filter(fertquantTT2, variable == 'q50'))

fart$group <- factor(paste0(fart$trial, fart$year))

fertquantTT1$year = factor(1958)
fertquantTT2$year = factor(1985)

fart$group <- factor(paste0(fart$trial, fart$year))



fart$group <- factor(paste0(fart$trial, fart$year))

pairwise.t.test(fart$value, fart$group)

