library(dplyr)
library("FactoMineR")
library("factoextra")
library(extrafont)
library(ggplot2)
library(pastecs)
library(corrplot)
library(ppcor)
library(factoextra)
library(psych)
library(GPArotation)
library(Hmisc)
library(dplyr)
library(ape)
library(psych)
library(psychometric)
library(maptools)
library(RColorBrewer)
library(rgeos)
library(rgdal)
library(sp)
library(sf)
library(ggrepel)
library(ggplot2)
library(tidyverse)

setwd('E:\\ResearchProject\\Najmul Bhai\\CHIKV')
ChikData <- read.csv("CHIKV_Outbreak.csv")

library(pastecs)

stat.desc(na.omit(ChikData$Test_delay))
IQR(na.omit(ChikData$Test_delay))


NROW(ChikData$Test_delay)
x <- table(ChikData$Test_delay)
x
round(prop.table(x),4)*100

ChikData$Test_delay_cat[ChikData$Test_delay_cat == ""] <- NA
NROW(ChikData$Test_delay_cat)
x <- table(ChikData$Test_delay_cat)
x
round(prop.table(x),4)*100


NROW(ChikData$Hospital_cat)
x <- table(ChikData$Hospital_cat)
x
round(prop.table(x),4)*100


x <- table(ChikData$Age_cat2)
x
round(prop.table(x),4)*100


x <- table(ChikData$Sex_cat)
x
round(prop.table(x),4)*100

ChikData$Emp_status[ChikData$Emp_status == ""] <- NA
x <- table(ChikData$Emp_status)
x
round(prop.table(x),4)*100

ChikData$Occu_cat[ChikData$Occu_cat == ""] <- NA
x <- table(ChikData$Occu_cat)
x
round(prop.table(x),4)*100

ChikData$DCC[ChikData$DCC == ""] <- NA
x <- table(ChikData$DCC)
x
round(prop.table(x),4)*100


ChikData$Fever[ChikData$Fever == ""] <- NA
x <- table(ChikData$Fever)
x
round(prop.table(x),4)*100

ChikData$Generalized.Rash[ChikData$Generalized.Rash == ""] <- NA
x <- table(ChikData$Generalized.Rash)
x
round(prop.table(x),4)*100

ChikData$Arthralgia[ChikData$Arthralgia == ""] <- NA
x <- table(ChikData$Arthralgia)
x
round(prop.table(x),4)*100


ChikData$Arthritis[ChikData$Arthritis == ""] <- NA
x <- table(ChikData$Arthritis)
x
round(prop.table(x),4)*100


ChikData$Conjunctivitis[ChikData$Conjunctivitis == ""] <- NA
x <- table(ChikData$Conjunctivitis)
x
round(prop.table(x),4)*100


ChikData$Myalgia[ChikData$Myalgia == ""] <- NA
x <- table(ChikData$Myalgia)
x
round(prop.table(x),4)*100


ChikData$Headeche[ChikData$Headeche == ""] <- NA
x <- table(ChikData$Headeche)
x
round(prop.table(x),4)*100


ChikData$Vomitting[ChikData$Vomitting == ""] <- NA
x <- table(ChikData$Vomitting)
x
round(prop.table(x),4)*100


ChikData$Diarrhea[ChikData$Diarrhea == ""] <- NA
x <- table(ChikData$Diarrhea)
x
round(prop.table(x),4)*100


ChikData$Others_cat[ChikData$Others_cat == ""] <- NA
x <- table(ChikData$Others_cat)
x
round(prop.table(x),4)*100


median(ChikData$Symp_count)
x <- table(ChikData$Symp_cat)
x
round(prop.table(x),4)*100


ChikData$Patient.Status..W.C.I.H.O.[ChikData$Patient.Status..W.C.I.H.O. == ""] <- NA
x <- table(ChikData$Patient.Status..W.C.I.H.O.)
x
round(prop.table(x),4)*100


ChikData$COPD[ChikData$COPD == ""] <- NA
x <- table(ChikData$COPD)
x
round(prop.table(x),4)*100

ChikData$Asthma[ChikData$Asthma == ""] <- NA
x <- table(ChikData$Asthma)
x
round(prop.table(x),4)*100

ChikData$ILD[ChikData$ILD == ""] <- NA
x <- table(ChikData$ILD)
x
round(prop.table(x),4)*100


ChikData$DM[ChikData$DM == ""] <- NA
x <- table(ChikData$DM)
x
round(prop.table(x),4)*100


ChikData$IHD[ChikData$IHD == ""] <- NA
x <- table(ChikData$IHD)
x
round(prop.table(x),4)*100


ChikData$HTN[ChikData$HTN == ""] <- NA
x <- table(ChikData$HTN)
x
round(prop.table(x),4)*100


ChikData$CLD[ChikData$CLD == ""] <- NA
x <- table(ChikData$CLD)
x
round(prop.table(x),4)*100


ChikData$Cancer[ChikData$Cancer == ""] <- NA
x <- table(ChikData$Cancer)
x
round(prop.table(x),4)*100


ChikData$Pregnancy[ChikData$Pregnancy == ""] <- NA
x <- table(ChikData$Pregnancy)
x
round(prop.table(x),4)*100


ChikData$CKD[ChikData$CKD == ""] <- NA
x <- table(ChikData$CKD)
x
round(prop.table(x),4)*100

ChikData$Any_comorb[ChikData$Any_comorb == ""] <- NA
x <- table(ChikData$Any_comorb)
x
round(prop.table(x),4)*100




#Crosstabs

x <- table(ChikData$Hospital_cat)
x
round(prop.table(x),4)*100


c <- table(ChikData$Age_cat2 , ChikData$Hospital_cat)
c
round(prop.table(c,1)*100,2)
summary(c)

c <- table(ChikData$Sex_cat , ChikData$Hospital_cat)
c
round(prop.table(c,1)*100,2)
summary(c)

c <- table(ChikData$Emp_status , ChikData$Hospital_cat)
c
round(prop.table(c,1)*100,2)
summary(c)

median(ChikData$Symp_count)

c <- table(ChikData$Symp_cat , ChikData$Hospital_cat)
c
round(prop.table(c,1)*100,2)
summary(c)


c <- table(ChikData$Symp_cat , ChikData$Hospital_cat)
c
round(prop.table(c,1)*100,2)
summary(c)

c <- table(ChikData$Test_delay_cat , ChikData$Hospital_cat)
c
round(prop.table(c,1)*100,2)
summary(c)


ChikData$Any_comorb[ChikData$Any_comorb == ""] <- NA
c <- table(ChikData$Any_comorb , ChikData$Hospital_cat)
c
round(prop.table(c,1)*100,2)
summary(c)


ChikData$Any_comorb[ChikData$Any_comorb == ""] <- NA
c <- table(ChikData$DCC , ChikData$Hospital_cat)
c
round(prop.table(c,1)*100,2)
summary(c)

model <- glm(N_Hospital_cat ~ factor(N_Age_cat2) + factor(N_Sex_cat) + factor(N_Emp_status) + factor(ChikData$DCC) + factor(N_Symp_cat)  + factor(N_Test_delay_cat) + factor(Any_comorb), data= ChikData)

summary(model)
exp(cbind(coef(model), confint(model)))



shp <- readOGR(dsn = "E:\\ResearchProject\\Najmul Bhai\\CHIKV\\bgd_adm_bbs_20201113_shp", "bgd_admbnda_adm4_bbs_20201113")
head(shp@data)
shp <- shp[shp$ADM2_EN=="Dhaka",]

# shp <- shp[shp$ADM4_EN=="Ward No-01" | shp$ADM4_EN=="Ward No-02" | shp$ADM4_EN=="Ward No-03" 
#            | shp$ADM4_EN=="Ward No-04" | shp$ADM4_EN=="Ward No-05" | shp$ADM4_EN=="Ward No-06"
#            | shp$ADM4_EN=="Ward No-07 (Part)" | shp$ADM4_EN=="Ward No-08" | shp$ADM4_EN=="Ward No-09"
#            | shp$ADM4_EN=="Ward No-10" | shp$ADM4_EN=="Ward No-11" | shp$ADM4_EN=="Ward No-12"
#            | shp$ADM4_EN=="Ward No-13" | shp$ADM4_EN=="Ward No-14 (part)" | shp$ADM4_EN=="Ward No-15 (part)"
#            | shp$ADM4_EN=="Ward No-16" | shp$ADM4_EN=="Ward No-17 (Part)" | shp$ADM4_EN=="Ward No-18"
#            | shp$ADM4_EN=="Ward No-19" | shp$ADM4_EN=="Ward No-20 (Part)" | shp$ADM4_EN=="Ward No-21"
#            | shp$ADM4_EN=="Ward No-22" | shp$ADM4_EN=="Ward No-23" | shp$ADM4_EN=="Ward No-24"
#            | shp$ADM4_EN=="Ward No-25" | shp$ADM4_EN=="Ward No-26" | shp$ADM4_EN=="Ward No-27"
#            | shp$ADM4_EN=="Ward No-28" | shp$ADM4_EN=="Ward No-29" | shp$ADM4_EN=="Ward No-30"
#            | shp$ADM4_EN=="Ward No-31" | shp$ADM4_EN=="Ward No-32" | shp$ADM4_EN=="Ward No-33"
#            | shp$ADM4_EN=="Ward No-34" | shp$ADM4_EN=="Ward No-35" | shp$ADM4_EN=="Ward No-36"
#            | shp$ADM4_EN=="Ward No-37" | shp$ADM4_EN=="Ward No-38 (Part)" | shp$ADM4_EN=="Ward No-39"
#            | shp$ADM4_EN=="Ward No-40 (Part)" | shp$ADM4_EN=="Ward No-41" | shp$ADM4_EN=="Ward No-42"
#            | shp$ADM4_EN=="Ward No-43" | shp$ADM4_EN=="Ward No-44" | shp$ADM4_EN=="Ward No-45"
#            | shp$ADM4_EN=="Ward No-46" | shp$ADM4_EN=="Ward No-46 (Part)" | shp$ADM4_EN=="Ward No-47 (part)" 
#            | shp$ADM4_EN=="Ward No-48 (Part)"
#            | shp$ADM4_EN=="Ward No-49" | shp$ADM4_EN=="Ward No-50" | shp$ADM4_EN=="Ward No-51 (part)"
#            | shp$ADM4_EN=="Ward No-52" | shp$ADM4_EN=="Ward No-53" | shp$ADM4_EN=="Ward No-54"
#            | shp$ADM4_EN=="Ward No-55" | shp$ADM4_EN=="Ward No-56 (part)" | shp$ADM4_EN=="Ward No-57"
#            | shp$ADM4_EN=="Ward No-58" | shp$ADM4_EN=="Ward No-59" | shp$ADM4_EN=="Ward No-60"
#            | shp$ADM4_EN=="Ward No-61" | shp$ADM4_EN=="Ward No-62" | shp$ADM4_EN=="Ward No-63 (Part)"
#            | shp$ADM4_EN=="Ward No-64" | shp$ADM4_EN=="Ward No-65" | shp$ADM4_EN=="Ward No-66 (Part)"
#            | shp$ADM4_EN=="Ward No-67 (part)" | shp$ADM4_EN=="Ward No-68 (part)" | shp$ADM4_EN=="Ward No-69"
#            | shp$ADM4_EN=="Ward No-70" | shp$ADM4_EN=="Ward No-71 (Part)" | shp$ADM4_EN=="Ward No-72"
#            | shp$ADM4_EN=="Ward No-73" | shp$ADM4_EN=="Ward No-74" | shp$ADM4_EN=="Ward No-75"
#            | shp$ADM4_EN=="Ward No-76 (Part)" | shp$ADM4_EN=="Ward No-77" | shp$ADM4_EN=="Ward No-78"
#            | shp$ADM4_EN=="Ward No-79" | shp$ADM4_EN=="Ward No-80 (Part)" | shp$ADM4_EN=="Ward No-81"
#            | shp$ADM4_EN=="Ward No-82" | shp$ADM4_EN=="Ward No-83" | shp$ADM4_EN=="Ward No-83"
#            | shp$ADM4_EN=="Ward No-84" | shp$ADM4_EN=="Ward No-85" | shp$ADM4_EN=="Ward No-86"
#            | shp$ADM4_EN=="Ward No-87" | shp$ADM4_EN=="Ward No-88" | shp$ADM4_EN=="Ward No-89"
#            | shp$ADM4_EN=="Ward No-90" | shp$ADM4_EN=="Ward No-91" | shp$ADM4_EN=="Ward No-92"
#            | shp$ADM4_EN=="Ward No-98 (rest. Area)",]
shp$ADM4_EN
head(shp@data)
xLon = ChikData$Longitude
xLat = ChikData$Latitude

SL.map <- fortify(shp, region = "ADM2_EN")

map1 <- ggplot() + 
  geom_polygon(data = SL.map, aes(x = long, y = lat, group = group), colour = "cadetblue", fill = "azure2") +
  labs(title = "Location of Chikungunya patients ") +
  xlab(label="Longitute") + ylab(label="Latitute")
map1

map2 <- map1 +  geom_point(data=ChikData, aes(x=Longitude, y=Latitude), colour = "darkgreen", size = 2)+
  theme(axis.text = element_text(size = 30),
        axis.title = element_text(size = 30),
        plot.title = element_text(size = 30))


map2


library(gridExtra)
tiff("ChikungunyaMap.tiff", units="in", width=10, height=8, res=300)
gridExtra::grid.arrange(map2, nrow=1, ncol=1)
dev.off()






#######Count GLM
options(scipen=999)
## Chikungunya - BD data ##
rm(list=ls())
library(MASS)
library(tscount)

chikts <- read.csv("ChikTS.csv", header=T)  
names(chikts)


#ARIMA
library(tseries)
library(forecast)
library(zoo)
d1=zoo(chikts$Pos, seq(from = as.Date("2024-10-19"), to = as.Date("2024-12-11"), by = 1))
tsdata <- ts(d1, frequency = 365)

auto.arima(tsdata)

Fit<-Arima(tsdata,order=c(2,1,1))
summary(Fit)

fcast <- forecast(Fit, h=10)

library(ggfortify)
z <- autoplot(fcast, size = 2) +
  xlab("Days") + ylab("Number of Chikungunya cases") +ggtitle("ARIMA Model")+
  guides(colour=guide_legend(title="Observed data"),
         fill=guide_legend(title="Prediction interval"))+ theme(legend.position="bottom") + theme_bw()+
  theme( legend.text = element_text(color = "Black", size = 18),
         text = element_text(size = 18))
z

tiff("arima.tiff", units="in", width=12, height=6, res=300)
gridExtra::grid.arrange(z)
dev.off()

#Menn kendal
library(Kendall)
library(trend)

t.test(tsdata)$"conf.int"
mean(tsdata)

library(trend)
MannKendall(tsdata)
sens.slope(tsdata, conf.level = 0.95)


chikdat <- read.csv("CHIKV_Count.csv", header=T)  
names(chikdat)

fitglm <- glm(Chik_pos ~ Test_delay + Age_avg + Male_per + Emp_stat_yes + 
                Com_yes + T2M + PRECTOTCORR, data=chikdat)

library(car)
# Calculating VIF
vif_values <- vif(fitglm)
vif_values


fitglm <- glm(Chik_pos ~ Test_delay + Age_avg + Male_per + Emp_stat_yes + 
                  Com_yes + T2M + PRECTOTCORR + 
                offset(log(Days+1)), data=chikdat, 
              family=poisson(link = "log"))
summary(fitglm)
library(car)
performance::performance(fitglm)
round(exp(coef(fitglm)),2)
round(exp(confint(fitglm)),2)





#Figure 1
library(ggplot2)
library("stringr") 

df <- data.frame(Date=c("2024-10-19",
                        "2024-10-24",
                        "2024-10-26",
                        "2024-10-31",
                        "2024-11-07",
                        "2024-11-10",
                        "2024-11-11",
                        "2024-11-12",
                        "2024-11-13",
                        "2024-11-14",
                        "2024-11-17",
                        "2024-11-18",
                        "2024-11-19",
                        "2024-11-20",
                        "2024-11-21",
                        "2024-11-24",
                        "2024-11-25",
                        "2024-11-26",
                        "2024-11-27",
                        "2024-11-28",
                        "2024-12-01",
                        "2024-12-02",
                        "2024-12-03",
                        "2024-12-04",
                        "2024-12-05",
                        "2024-12-08",
                        "2024-12-09",
                        "2024-12-10",
                        "2024-12-11"),
Count=c(2,
        1,
        1,
        1,
        1,
        3,
        3,
        5,
        5,
        6,
        2,
        3,
        2,
        5,
        5,
        5,
        2,
        3,
        6,
        2,
        7,
        2,
        1,
        6,
        9,
        11,
        4,
        8,
        1))
head(df)

b<-ggplot(data=df, aes(x=Date, y=Count)) +
  geom_bar(stat="identity", fill="darkgreen")+
  theme_minimal()  +
  scale_fill_brewer() + geom_text(aes(label=Count), vjust=-0.5, color="black",
                                  position = position_dodge(1), size=5)+
  xlab("Date") + ylab("Chikungunya Cases") + 
  theme(axis.text = element_text(size = 15,angle = 90, vjust = 1, hjust=0.5),
        axis.title = element_text(size = 15),
        plot.title = element_text(size = 15),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15))
b 
library(gridExtra)
tiff("ChikungunyaCount.tiff", units="in", width=10, height=8, res=300)
gridExtra::grid.arrange(b, nrow=1, ncol=1)
dev.off()


#Pie chart
df <- data.frame(
  Sex = c("Female", "Male"),
  value = c(36.61, 63.39)
)
head(df)

library(ggplot2)

x <- ggplot(df, aes(x = "", y = value, fill = Sex)) +
  geom_col(color = "black") +
  geom_text(aes(label = value),cex=15,
            position = position_stack(vjust = 0.5))  + ggtitle("Chikungunya Cases") +
  coord_polar(theta = "y") +
  scale_fill_brewer() +
  theme_void()+ ylab("") + theme(legend.title = element_text(size=15),
                                 legend.text = element_text(size=15),
                                 plot.title = element_text(hjust = 0.5),
                                 text=element_text(size=15))

x


library(gridExtra)
tiff("CCSex.tiff", units="in", width=8, height=6, res=300)
gridExtra::grid.arrange(x, ncol=1)
dev.off()





#Figure 1
library(ggplot2)
library("stringr") 

c <- table(ChikData$Age_cat)
c

df <- data.frame(Age=c("<=19", "20-29", "30-39", "40-49", "50-59", "60+"),
                 Count=c(5, 13, 30, 26, 18, 20))
head(df)

b<-ggplot(data=df, aes(x=Age, y=Count)) +
  geom_bar(stat="identity", fill="darkgreen")+
  theme_minimal()  +
  scale_fill_brewer() + geom_text(aes(label=Count), vjust=-0.5, color="black",
                                  position = position_dodge(1), size=5)+
  xlab("Age Group") + ylab("Chikungunya Cases") + 
  theme(axis.text = element_text(size = 15,angle = 90, vjust = 1, hjust=0.5),
        axis.title = element_text(size = 15),
        plot.title = element_text(size = 15),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15))
b 
library(gridExtra)
tiff("ChikungunyaAge.tiff", units="in", width=10, height=8, res=300)
gridExtra::grid.arrange(b, nrow=1, ncol=1)
dev.off()


#Figure 1
library(ggplot2)
library("stringr") 

df <- data.frame(Symptoms=c("Fever", "Rash", "Arthralgia", "Arthritis", "Conjunctivitis",
                        "Myalgia", "Headache", "Vomiting", "Diarrhea", "Others"),
                 Percentage=c(100.00,
                              23.42,
                              98.20,
                              2.02,
                              48.65,
                              84.68,
                              68.47,
                              33.33,
                              1.80,
                              3.57))
head(df)

b<-ggplot(data=df, aes(x=reorder(Symptoms, -Percentage, sum), y=Percentage)) +
  geom_bar(stat="identity", fill="darkgreen")+
  theme_minimal()  +
  scale_fill_brewer() + geom_text(aes(label=Percentage), vjust=-0.5, color="black",
                                  position = position_dodge(1), size=5)+
  xlab("Clinical Symptoms") + ylab("Percentage") + 
  theme(axis.text = element_text(size = 15,angle = 90, vjust = 1, hjust=0.5),
        axis.title = element_text(size = 15),
        plot.title = element_text(size = 15),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15))
b 



#Pie chart
df <- data.frame(
  Symptoms = c("Low", "High"),
  value = c(41.07, 58.93)
)
head(df)

library(ggplot2)

c <- ggplot(df, aes(x = "", y = value, fill = Symptoms)) +
  geom_col(color = "black") +
  geom_text(aes(label = value),cex=15,
            position = position_stack(vjust = 0.5))  + ggtitle("Clinical signs on chikungunya patients") +
  coord_polar(theta = "y") +
  scale_fill_brewer() +
  theme_void()+ ylab("") + theme(legend.title = element_text(size=15),
                                 legend.text = element_text(size=15),
                                 plot.title = element_text(hjust = 0.5),
                                 text=element_text(size=15))

c

library(gridExtra)
tiff("ChikungunyaSymptoms.tiff", units="in", width=16, height=8, res=300)
gridExtra::grid.arrange(b,c, nrow=1, ncol=2)
dev.off()






#Figure 1
library(ggplot2)
library("stringr") 

df <- data.frame(Comorbidities=c("COPD", "Asthma", "ILD", "DM", "IHD",
                            "HTN", "CLD", "Cancer", "Pregnancy","CKD"),
                 Percentage=c(4.50,
                              7.21,
                              0.90,
                              25.23,
                              9.01,
                              27.93,
                              4.50,
                              0.90,
                              0.90,
                              1.8))
head(df)

b<-ggplot(data=df, aes(x=reorder(Comorbidities, -Percentage, sum), y=Percentage)) +
  geom_bar(stat="identity", fill="darkgreen")+
  theme_minimal()  +
  scale_fill_brewer() + geom_text(aes(label=Percentage), vjust=-0.5, color="black",
                                  position = position_dodge(1), size=5)+
  xlab("Co-morbidities") + ylab("Percentage") + 
  theme(axis.text = element_text(size = 15,angle = 90, vjust = 1, hjust=0.5),
        axis.title = element_text(size = 15),
        plot.title = element_text(size = 15),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15))
b 



#Pie chart
df <- data.frame(
  Comorbidity = c("Yes", "No"),
  value = c(54.95, 45.05)
)
head(df)

library(ggplot2)

y <- ggplot(df, aes(x = "", y = value, fill = Comorbidity)) +
  geom_col(color = "black") +
  geom_text(aes(label = value),cex=15,
            position = position_stack(vjust = 0.5))  + ggtitle("Co-morbidities on chikungunya patients") +
  coord_polar(theta = "y") +
  scale_fill_brewer() +
  theme_void()+ ylab("") + theme(legend.title = element_text(size=15),
                                 legend.text = element_text(size=15),
                                 plot.title = element_text(hjust = 0.5),
                                 text=element_text(size=15))

y


library(gridExtra)
tiff("ChikungunyaComorbidities.tiff", units="in", width=16, height=8, res=300)
gridExtra::grid.arrange(b,y, nrow=1, ncol=2)
dev.off()

