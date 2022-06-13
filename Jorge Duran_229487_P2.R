#*** Jorge Eduardo Durán Vásquez***
#*** Case Studies SS 2022 ***
#*** Project 2: One-Quarter-Ahead Forecasts of US GDP Growth A Machine Learning Approach 

###########***Upload and processing data***####################################

# Upload the data set
Data <- read.table(file = "2022-02.csv", header = TRUE, sep = ',')

# Pre-processing the data
NewData <- data.frame(Data$sasdate,Data$GDPC1,Data$CUMFNS,Data$UNRATESTx,Data$CPIAUCSL,Data$FEDFUNDS,Data$M1REAL,Data$S.P.500)

#Save the data without the first to rows
finaldataset <- NewData[3:254,]
class(finaldataset)

# Transforming Growth Gross Domestic Product (GDP) 
GDPgrowth = (diff(finaldataset$Data.GDPC1,1)/finaldataset$Data.GDPC1[1:251])*100

#Transforming Consumer Price Index (CPIAUCSL)
CPIAUCSLgrowth = (diff(finaldataset$Data.CPIAUCSL,1)/finaldataset$Data.CPIAUCSL[1:251])*100

# Transforming M1 money stock (M1REAL)
M1REALgrowth = (diff(finaldataset$Data.M1REAL,1)/finaldataset$Data.M1REAL[1:251])*100

#Transforming to Growth S&P 500 (S.P.500)
S.P.500growth = (diff(finaldataset$Data.S.P.500,1)/finaldataset$Data.S.P.500[1:251])*100

Analysisdata <- data.frame(GDPgrowth,
                          finaldataset$Data.CUMFNS[2:252],
                          finaldataset$Data.UNRATESTx[2:252],
                          CPIAUCSLgrowth,
                          finaldataset$Data.FEDFUNDS[2:252],
                          M1REALgrowth,
                          S.P.500growth) 

colnames(Analysisdata) <- c('GDP_growth',
                           'CUMFNS',
                           'UNRATESTx',
                           'CPIAUCSL',
                           'FEDFUNDS',
                           'M1REAL_growth',
                           'S.P.500_growth')


########## load libraries ##########

library(rpart)
library(rpart.plot)
library(ranger)

########## a. Regression Trees, GDP ##########
#Now we crate the variables that will be used in the regression tree

regtrees_data <- data.frame()

for (i in 1:(length(Analysisdata$GDP_growth)-10)) {

  regtrees_data <- rbind(regtrees_data, Analysisdata$GDP_growth[i:(10+i)])

}

colnames(regtrees_data) <- c('lag_10','lag_9','lag_8',
                            'lag_7','lag_6','lag_5','lag_4',
                            'lag_3','lag_2','lag_1','Pred')

reg_tree <-rpart(Pred ~ .,  method="anova", data=regtrees_data)
predic_trees <- predict(reg_tree)

GDPgrowth_graph_RT <- ts(GDPgrowth[-c(1:10)], start= c(1961,4), frequency = 4, end=c(2021,4))
Forecastingtis_RT <- ts(predic_trees, start= c(1961,4), frequency = 4, end=c(2021,4))


pdf(file = "fig/RT_GDP.pdf",   # The directory you want to save the file in
    width = 9, # The width of the plot in inches
    height = 5) # The height of the plot in inches

par(mfrow=c(1,1),mar=c(5,5,1,1),mgp=c(3,1,0))
ts.plot(GDPgrowth_graph_RT, type = 'l', xlab="Time [Years]", ylab="GDP Growth [%]",lwd = 2, ylim=c(-9,8))
 points(Forecastingtis_RT, col= 'red', type = 'l', lty= 1,lwd = 2)
 legend("topleft", legend=c("GDP growth","RT prediction GDP growth"),
        col=c("black", "red"), lty=c("solid","solid"),lwd = c(2,2),cex=0.8)

## Run dev.off() to create the PDF file
dev.off()

RMSE_RT_GDP <- sqrt(mean((regtrees_data[,11]-predic_trees)^2))
RMSE_RT_GDP
#0.901862

RMSE_RT_GDP_2to241 <- sqrt(mean((regtrees_data[2:241,11]-predic_trees[2:241])^2))
RMSE_RT_GDP_2to241
#0.9033652

pdf(file = "fig/reg_tree.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 5) # The height of the plot in inches
rpart.plot(reg_tree, digits = 3)
## Run dev.off() to create the PDF file
dev.off()

pdf(file = "fig/reg_treeprp.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 5) # The height of the plot in inches
prp(reg_tree) 
## Run dev.off() to create the PDF file
dev.off()

########## b. Regression Trees, all variables ##########

regtrees_data_av <- data.frame()
 
for (i in 1:(length(Analysisdata$GDP_growth)-10)) {
   
   regtrees_data_av <- rbind(regtrees_data_av, 
                             c(Analysisdata$GDP_growth[i:(10+i)],
                              Analysisdata$CUMFNS[i:(9+i)],
                              Analysisdata$UNRATESTx[i:(9+i)],
                              Analysisdata$CPIAUCSL[i:(9+i)],
                              Analysisdata$FEDFUNDS[i:(9+i)],
                              Analysisdata$M1REAL_growth[i:(9+i)],
                             Analysisdata$S.P.500_growth[i:(9+i)]))
   
}
 
colnames(regtrees_data_av) <- c('lag_10_GDP','lag_9_GDP','lag_8_GDP',
                              'lag_7_GDP','lag_6_GDP','lag_5_GDP','lag_4_GDP',
                              'lag_3_GDP','lag_2_GDP','lag_1_GDP','Pred_GDP',
                              'lag_10_CUMFNS','lag_9_CUMFNS','lag_8_CUMFNS',
                              'lag_7_CUMFNS','lag_6_CUMFNS','lag_5_CUMFNS','lag_4_CUMFNS',
                              'lag_3_CUMFNS','lag_2_CUMFNS','lag_1_CUMFNS',
                              'lag_10_UNRATESTx','lag_9_UNRATESTx','lag_8_UNRATESTx',
                              'lag_7_UNRATESTx','lag_6_UNRATESTx','lag_5_UNRATESTx','lag_4_UNRATESTx',
                              'lag_3_UNRATESTx','lag_2_UNRATESTx','lag_1_UNRATESTx',
                              'lag_10_CPIAUCSL','lag_9_CPIAUCSL','lag_8_CPIAUCSL',
                              'lag_7_CPIAUCSL','lag_6_CPIAUCSL','lag_5_CPIAUCSL','lag_4_CPIAUCSL',
                              'lag_3_CPIAUCSL','lag_2_CPIAUCSL','lag_1_CPIAUCSL',
                              'lag_10_FEDFUNDS','lag_9_FEDFUNDS','lag_8_FEDFUNDS',
                              'lag_7_FEDFUNDS','lag_6_FEDFUNDS','lag_5_FEDFUNDS','lag_4_FEDFUNDS',
                              'lag_3_FEDFUNDS','lag_2_FEDFUNDS','lag_1_FEDFUNDS',
                              'lag_10_M1REAL_growth','lag_9_M1REAL_growth','lag_8_M1REAL_growth',
                              'lag_7_M1REAL_growth','lag_6_M1REAL_growth','lag_5_M1REAL_growth','lag_4_M1REAL_growth',
                              'lag_3_M1REAL_growth','lag_2_M1REAL_growth','lag_1_M1REAL_growth',
                              'lag_10_S.P.500_growth','lag_9_S.P.500_growth','lag_8_S.P.500_growth',
                              'lag_7_S.P.500_growth','lag_6_S.P.500_growth','lag_5_S.P.500_growth','lag_4_S.P.500_growth',
                              'lag_3_S.P.500_growth','lag_2_S.P.500_growth','lag_1_S.P.500_growth')

reg_tree_av <-rpart(Pred_GDP ~ .,  method="anova", data= regtrees_data_av)
predic_trees_av <- predict(reg_tree_av)
 
 
GDPgrowth_graph_RT_AV <- ts(GDPgrowth[-c(1:10)], start= c(1961,4), frequency = 4, end=c(2021,4))
Forecastingtis_RT_AV <- ts(predic_trees_av, start= c(1961,4), frequency = 4, end=c(2021,4))
 
 
pdf(file = "fig/RT_AV_GDP.pdf",   # The directory you want to save the file in
    width = 9, # The width of the plot in inches
    height = 5) # The height of the plot in inches

par(mfrow=c(1,1),mar=c(5,5,1,1),mgp=c(3,1,0))
ts.plot(GDPgrowth_graph_RT, type = 'l', xlab="Time [Years]", ylab="GDP Growth [%]",lwd = 2, ylim=c(-9,8))
points(Forecastingtis_RT_AV, col= 'red', type = 'l', lty= 1,lwd = 2)
legend("topleft", legend=c("GDP growth","RT All variables prediction GDP growth"),
        col=c("black", "red"), lty=c("solid","solid"),lwd = c(2,2),cex=0.8)

## Run dev.off() to create the PDF file
dev.off()
 
RMSE_RT_av <- sqrt(mean((regtrees_data_av[,11] - predic_trees_av)^2))
RMSE_RT_av
# 0.8631611

RMSE_RT_av_2to241 <- sqrt(mean((regtrees_data_av[2:241,11] - predic_trees_av[2:241])^2))
RMSE_RT_av_2to241
# 0.8629651

pdf(file = "fig/reg_tree_av.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 5) # The height of the plot in inches
rpart.plot(reg_tree_av, digits=4)
## Run dev.off() to create the PDF file
dev.off()

pdf(file = "fig/reg_tree_avprp.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 5) # The height of the plot in inches
prp(reg_tree_av)
## Run dev.off() to create the PDF file
dev.off()

########## c. Comparison Regression Trees, Only GDP growth vs. All Variables ##########
 
forecasting_oqa <- data.frame()
 
for (i in 1:(nrow(regtrees_data_av))) {
   regtrees_data_oqa <- rpart(Pred ~ .,  method="anova", data=regtrees_data[1:i,])
   reg_tree_av_oqa <-rpart(Pred_GDP ~ .,  method="anova", data= regtrees_data_av[1:i,])
   forecasting_oqa = rbind(forecasting_oqa, c(predict(regtrees_data_oqa,regtrees_data[i+1,1:10]),predict(reg_tree_av_oqa,regtrees_data_av[i+1,c(-11)])))
}
 
colnames(forecasting_oqa) <- c('forecasting_GDP_oqa','forecasting_av_oqa')

RMSE_RT_GDP_oqa <- sqrt(mean((regtrees_data[2:241,11]- forecasting_oqa$forecasting_GDP_oqa[1:240])^2))
RMSE_RT_GDP_oqa
#1.258732

RMSE_RT_av_oqa <- sqrt(mean((regtrees_data_av[2:241,11] 
                             - forecasting_oqa$forecasting_av_oqa[1:240])^2))
RMSE_RT_av_oqa
#1.232489

GDPgrowth_graph <- ts(GDPgrowth[-c(1:10)], start= c(1961,4), frequency = 4, end=c(2021,4))
Forecasting_RT_av_oqa <- ts(forecasting_oqa$forecasting_av_oqa[-c(241)], start= c(1962,1), frequency = 4, end=c(2021,4))
Forecasting_RT_GDP_oqa<- ts(forecasting_oqa$forecasting_GDP_oqa[-c(241)], start= c(1962,1), frequency = 4, end=c(2021,4))

pdf(file = "fig/RTAV_VS_RT_GDP.pdf",   # The directory you want to save the file in
    width = 9, # The width of the plot in inches
    height = 5) # The height of the plot in inches

par(mfrow=c(1,1),mar=c(5,5,1,1),mgp=c(3,1,0))
ts.plot(GDPgrowth_graph, type = 'l', xlab="Time [Years]", ylab="GDP Growth [%]",lwd = 2, ylim=c(-9,8))
points(Forecasting_RT_av_oqa, col= 'blue', type = 'l', lty=1,lwd = 2)
points(Forecasting_RT_GDP_oqa, col= 'red', type = 'l', lty=1,lwd = 2)
legend("topleft", legend=c("GDP growth","RT all variables forecast GDP growth","RT forecast GDP growth"),
       col=c("black", "blue",'red'), lty=c("solid","solid","solid"),lwd = c(2,2,2),cex=0.8)

## Run dev.off() to create the PDF file
dev.off()

######### d. Random Forest ############################

ranger_forest_permutation <- ranger(Pred_GDP ~ ., data= regtrees_data_av, importance = "permutation", replace= TRUE, seed = 420)

predic_ranger_forest_permutation<- predict(ranger_forest_permutation, regtrees_data_av)
predic_ranger_forest_permutation$predictions

Prediction_RF_av <- ts(predic_ranger_forest_permutation$predictions, start= c(1961,4), frequency = 4, end=c(2021,4))

ranger_importance_permutation <- sort(importance(ranger_forest_permutation))
#ranger_importance_permutation

## Writing ranger_importance_permutation data
#write.table(ranger_importance_permutation, file = "ranger_importance_permutation.txt", sep = "\t",
#            row.names = TRUE, col.names = NA)

RMSE_RF_av <- sqrt(mean((regtrees_data_av[,11] - Prediction_RF_av)^2))
RMSE_RF_av
# 0.5182484

pdf(file = "fig/RF_AV.pdf",   # The directory you want to save the file in
    width = 9, # The width of the plot in inches
    height = 5) # The height of the plot in inches

par(mfrow=c(1,1),mar=c(5,5,1,1),mgp=c(3,1,0))
ts.plot(GDPgrowth_graph, type = 'l', xlab="Time [Years]", ylab="GDP Growth [%]",lwd = 2, ylim=c(-9,8))
points(Prediction_RF_av, col= 'magenta2', type = 'l', lty=1,lwd = 2)
legend("topleft", legend=c("GDP growth","RF all variables prediction GDP growth"),
       col=c("black", "magenta2"), lty=c("solid","solid"),lwd = c(2,2),cex=0.8)

## Run dev.off() to create the PDF file
dev.off()

pdf(file = "fig/V_impo.pdf",   # The directory you want to save the file in
     width = 10, # The width of the plot in inches
     height = 5) # The height of the plot in inches


par(mfrow=c(1,1),mar=c(5,10,1,1),mgp=c(3,1,0))
barplot(ranger_importance_permutation[c(-6:-65)],
        xlab = "Variable importance",
        col = "coral3",
        horiz = TRUE,las=1, xlim=c(-0.04,0.07),cex.names=1)
grid(nx = NULL, ny = NULL,
     lty = 2,      # Tipo de ínea
     col = "gray", # Color
     lwd = 1)      # Ancho de línea
## Run dev.off() to create the PDF file
dev.off()

######### e. Random Forest ############################


forecasting_RF_oqa <- data.frame()

for (i in 1:(nrow(regtrees_data_av)-1)) {
  RF_oqa <- ranger(Pred_GDP ~ .,data= regtrees_data_av[1:i,], seed = 420)
  forecasting_RF_oqa = rbind(forecasting_RF_oqa, predict(RF_oqa,regtrees_data_av[(i+1),c(-11)]))
}

colnames(forecasting_RF_oqa) <- c('forecasting_RF_oqa')

RMSE_RF_av_oqa <- sqrt(mean((regtrees_data_av[2:241,11] - forecasting_RF_oqa$forecasting_RF_oqa)^2))
RMSE_RF_av_oqa

#1.204087


GDPgrowth_graph <- ts(GDPgrowth[-c(1:10)], start= c(1961,4), frequency = 4, end=c(2021,4))
Forecasting_RF_av_oqa <- ts(forecasting_RF_oqa$forecasting_RF_oqa[1:240], start= c(1962,1), frequency = 4, end=c(2021,4))

#Data of forecast model AR1 needs to be imported from the previous report
Forecastingtis_AR1 <- c(0.263861588276335,0.181039888531392,1.69198882682005,0.668664536929842,0.907197531467347,0.227406583364471,0.134237668715623,0.469888701179617,0.764472918981084,0.922366917058267,
                        0.84582821052319,0.902269195245723,0.792802885015752,0.867024258757363,0.885285258094985,1.04913192306745,0.916299293814138,1.02759247592445,1.00252503766473,1.04433917140098,
                        0.985340982476046,1.00058515655702,1.06217031888435,1.09339527237188,1.20477608593083,1.32653493425563,1.12840382541294,1.14116624667191,1.12721632508217,1.12263827399228,
                        1.01982938988857,1.08806952409853,1.06136396161796,1.18722439954604,1.18178934631188,1.09529869430553,1.03912130691585,1.15183060520248,1.04455467102248,1.05325015514666,
                        0.893911840752006,0.842192379701009,0.846889685825159,0.982407020813968,0.586786136894319,1.10659142740697,0.963342264498924,0.972276377800851,0.929040976062064,1.00972139590371,
                        1.07760420027517,1.00186194218394,1.05138327694435,1.14579989229721,1.04747904024342,0.910400276048117,1.01118484342863,0.852763462715894,0.902915466623188,0.715410372361471,
                        0.713906423571965,0.464757882868523,0.847349353313539,1.04508845404855,0.983863536424384,1.18330148733584,0.881103447135971,0.841185894991798,0.872607442401966,0.963148756421752,
                        1.12400734100017,1.11494223075812,0.749257704007981,0.799560668014965,1.44913488048872,0.965034061068829,1.02107344156039,0.83754055836256,0.811415419783437,0.908679551312401,
                        0.822895695585815,0.824105542931526,0.339854936786114,0.675462812173568,1.07050591838491,1.1079443720729,0.595418261330116,0.936605882379503,0.55172739780769,0.354811472874983,
                        0.756724721754493,0.574359558046201,0.648687211103443,0.917802078838269,1.13419697294799,1.09836743665826,1.13985044468025,1.12735825135941,1.08407431347686,0.896786994845772,
                        0.861286694677079,0.898296067007949,0.876628267818873,1.03965560776497,0.847018497982658,0.89364520010433,0.773257825613242,0.896825094769737,0.792835665520301,0.842264337158223,
                        0.924410844990944,0.873283446726952,1.0840189482783,0.793275538908419,0.984722988001105,0.811622619237032,0.988862262822468,0.916158668684853,0.856379708342613,0.85023503854473,
                        0.718642254048051,0.928946725672628,0.757250720222563,0.681402072167062,0.422751179875178,0.505820630677824,0.827158095750907,0.755245317068539,0.711881492705382,0.930525457645132,
                        0.90333881780789,0.879697960846355,0.894789132633019,0.668646985794866,0.772413001897987,0.743542800706933,0.970373992800806,0.872510751646037,0.972797598290171,0.776639442540771,
                        0.919250393125661,0.718874707815029,0.701053714249546,0.839937343473316,0.796113895079744,0.813194986571844,1.04721639089673,0.854745589419867,0.89095432444822,0.79220829830936,
                        1.04790416917592,0.948719870430492,0.851334972784829,0.886976043508517,0.869085857755111,0.95225631798608,1.04768773933179,0.879649997058057,0.852352237825528,0.976373718039626,
                        1.06161789449753,0.739287149381928,1.10057263359678,0.683508391323121,0.799006353146775,0.573162053178064,0.798435969105487,0.546022974440233,0.705339292978691,0.839440613570357,
                        0.78674217842811,0.73301122040519,0.662008482138432,0.75522943985773,0.848289967226254,1.04243722188333,0.917622753411378,0.773147517628538,0.823584733333821,0.864025314367587,
                        0.884688954806113,0.907762691314478,0.752681550990974,0.825950592639181,0.771340335807652,0.964689305024121,0.694093281146564,0.666668227684211,0.836087118564806,0.70003780562246,
                        0.783169226935029,0.773412692127004,0.774285161435486,0.518729228803993,0.75903369342629,0.480664305604797,-0.0175540858394353,0.208605798819605,0.495858925322726,0.65397408870201,
                        0.867818907036527,0.696228459353851,0.837128526699441,0.778812728363188,0.701972985173731,0.469032926257314,0.745620245771519,0.527559262174382,0.876185885535627,0.786051171447397,
                        0.677460953807007,0.591415410067949,0.5686456424986,0.796151528885222,0.57685335209278,0.771448752519819,0.747350621883681,0.427744919663897,0.912956212617651,0.879802764692158,
                        0.669671134872985,0.77603004732543,0.7075234788412,0.631227290079733,0.576827079076825,0.706074701197388,0.620474266026161,0.707620398407724,0.67620541663892,0.667930536215193,
                        0.693129255149089,0.739913818553742,0.805541942913665,0.753384653966536,0.774524088430845,0.670309546067477,0.592306714388249,0.702224971102177,0.759833097200806,0.728026929815835,
                        0.663477017125406,0.128374117733783,-3.25047129205474,0.782343903680596,0.734560179290091,0.745062114972997,0.752522713032511,0.731800649558945)

Forecastingtis_AR1 <- ts(Forecastingtis_AR1, start= c(1960,1), frequency = 4, end=c(2021,4))

pdf(file = "fig/RF-GDP.pdf",   # The directory you want to save the file in
    width = 10, # The width of the plot in inches
    height = 5) # The height of the plot in inches

par(mfrow=c(1,1),mar=c(5,5,1,1),mgp=c(3,1,0))
ts.plot(GDPgrowth_graph, type = 'l', xlab="Time [Years]", ylab="GDP Growth [%]",lwd = 2, ylim=c(-9,8))
points(Forecasting_RF_av_oqa, col= 'magenta2', type = 'l', lty=1,lwd = 2)
legend("topleft", legend=c("GDP growth","RF all variables forecast GDP growth"),
       col=c("black", "magenta2"), lty=c("solid","solid"),lwd = c(2,2),cex=0.8)

## Run dev.off() to create the PDF file
dev.off()

GDPgrowth_graph_AR1 <- ts(GDPgrowth[-c(1:3)], start= c(1960,1), frequency = 4, end=c(2021,4))

pdf(file = "fig/RF_vs_AR.pdf",   # The directory you want to save the file in
    width = 10, # The width of the plot in inches
    height = 5) # The height of the plot in inches

par(mfrow=c(1,1),mar=c(5,5,1,1),mgp=c(3,1,0))
ts.plot(GDPgrowth_graph_AR1, type = 'l', xlab="Time [Years]", ylab="GDP Growth [%]",lwd = 2, ylim=c(-9,8))
points(Forecastingtis_AR1, col= 'green3', type = 'l', lty=1,lwd = 3)
points(Forecasting_RF_av_oqa, col= 'magenta2', type = 'l', lty=1,lwd = 3)
legend("topleft", legend=c("GDP growth","RF all variables forecast GDP growth","AR(1) forecast GDP growth"),
       col=c("black", "magenta2","green3"), lty=c("solid","solid","solid"),lwd = c(2,3,3),cex=0.8)

## Run dev.off() to create the PDF file
dev.off()

## Comparison of the models RF_av and AR 

RMSE_RF_av_oqa_com <- sqrt(mean((regtrees_data_av[2:234,11] - forecasting_RF_oqa$forecasting_RF_oqa[1:233])^2))
RMSE_RF_av_oqa_com
# 0.761525

RMSE_AR_com <- sqrt(mean((regtrees_data_av[2:234,11] - Forecastingtis_AR1[9:241])^2))
RMSE_AR_com

# 0.7812506
