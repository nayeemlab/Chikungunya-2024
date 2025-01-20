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

stat.desc(na.omit(ChikData$Age))
IQR(na.omit(ChikData$Age))


stat.desc(na.omit(ChikData$Age[ChikData$Hospital_cat == "Yes"]))
IQR(na.omit(ChikData$Age))

x <- table(ChikData$Months)
x
round(prop.table(x),4)*100

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




#Hospital_cat == "Yes"
ChikData$COPD[ChikData$COPD == ""] <- NA
x <- table(ChikData$COPD[ChikData$Hospital_cat == "Yes"])
x
round(prop.table(x),4)*100

ChikData$Asthma[ChikData$Asthma == ""] <- NA
x <- table(ChikData$Asthma[ChikData$Hospital_cat == "Yes"])
x
round(prop.table(x),4)*100

ChikData$ILD[ChikData$ILD == ""] <- NA
x <- table(ChikData$ILD[ChikData$Hospital_cat == "Yes"])
x
round(prop.table(x),4)*100


ChikData$DM[ChikData$DM == ""] <- NA
x <- table(ChikData$DM[ChikData$Hospital_cat == "Yes"])
x
round(prop.table(x),4)*100


ChikData$IHD[ChikData$IHD == ""] <- NA
x <- table(ChikData$IHD[ChikData$Hospital_cat == "Yes"])
x
round(prop.table(x),4)*100


ChikData$HTN[ChikData$HTN == ""] <- NA
x <- table(ChikData$HTN[ChikData$Hospital_cat == "Yes"])
x
round(prop.table(x),4)*100


ChikData$CLD[ChikData$CLD == ""] <- NA
x <- table(ChikData$CLD[ChikData$Hospital_cat == "Yes"])
x
round(prop.table(x),4)*100


ChikData$Cancer[ChikData$Cancer == ""] <- NA
x <- table(ChikData$Cancer[ChikData$Hospital_cat == "Yes"])
x
round(prop.table(x),4)*100


ChikData$Pregnancy[ChikData$Pregnancy == ""] <- NA
x <- table(ChikData$Pregnancy[ChikData$Hospital_cat == "Yes"])
x
round(prop.table(x),4)*100


ChikData$CKD[ChikData$CKD == ""] <- NA
x <- table(ChikData$CKD[ChikData$Hospital_cat == "Yes"])
x
round(prop.table(x),4)*100

ChikData$Any_comorb[ChikData$Any_comorb == ""] <- NA
x <- table(ChikData$Any_comorb[ChikData$Hospital_cat == "Yes"])
x
round(prop.table(x),4)*100





#Followup
ChikData$FU_Current_Status[ChikData$FU == ""] <- NA
x <- table(ChikData$FU)
x
round(prop.table(x),4)*100

ChikData$FU_Current_Status[ChikData$FU_Current_Status == ""] <- NA
x <- table(ChikData$FU_Current_Status)
x
round(prop.table(x),4)*100


ChikData$FU_Sym_Joint_Pain[ChikData$FU_Sym_Joint_Pain == ""] <- NA
x <- table(ChikData$FU_Sym_Joint_Pain)
x
round(prop.table(x),4)*100


ChikData$FU_Sym_Joint_Swelling[ChikData$FU_Sym_Joint_Swelling == ""] <- NA
x <- table(ChikData$FU_Sym_Joint_Swelling)
x
round(prop.table(x),4)*100

ChikData$FU_Sym_Fatigue[ChikData$FU_Sym_Fatigue == ""] <- NA
x <- table(ChikData$FU_Sym_Fatigue)
x
round(prop.table(x),4)*100

ChikData$FU_Sym_Others2[ChikData$FU_Sym_Others2 == ""] <- NA
x <- table(ChikData$FU_Sym_Others2)
x
round(prop.table(x),4)*100

ChikData$FU_Sym_Any[ChikData$FU_Sym_Any == ""] <- NA
x <- table(ChikData$FU_Sym_Any)
x
round(prop.table(x),4)*100

stat.desc(na.omit(ChikData$FU_Duration_Hosp_Stay))
IQR(na.omit(ChikData$FU_Duration_Hosp_Stay))


stat.desc(na.omit(ChikData$FU_Loss_Workdays))
IQR(na.omit(ChikData$FU_Loss_Workdays))



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

c <- table(ChikData$DCC , ChikData$Hospital_cat)
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

## Common outcome: log link, poisson family, robust estimator (modified Poisson with robust estimator by Zou)
library(geepack)

ChikData_new <- ChikData[, c("Serial", "N_Hospital_cat", "Age_cat2",
                                       "Sex_cat", "Emp_status", "DCC",
                                        "Test_delay_cat", "Any_comorb")]

ChikData_nomiss = na.omit(ChikData_new)
geeglm.log.poisson <- geeglm(formula = N_Hospital_cat ~ Age_cat2 + Sex_cat + Emp_status + Test_delay_cat + Any_comorb,
                             data    = ChikData_nomiss,
                             id      = Serial,
                             corstr  = "exchangeable")
summary(geeglm.log.poisson)

cc <- coef(summary(geeglm.log.poisson))
citab <- with(as.data.frame(cc),
              cbind(lwr=Estimate-1.96*Std.err,
                    upr=Estimate+1.96*Std.err))
rownames(citab) <- rownames(cc)
exp(cbind(cc, citab))

library(car)
performance::performance(geeglm.log.poisson)


shp <- readOGR(dsn = "E:\\ResearchProject\\Najmul Bhai\\CHIKV\\bgd_adm_bbs_20201113_shp", "bgd_admbnda_adm4_bbs_20201113")
head(shp@data)
SL.map <- fortify(shp, region = "ADM2_EN")
map1 <- ggplot() + 
  geom_polygon(data = SL.map, aes(x = long, y = lat, group = group), colour = "cadetblue", fill = "azure2") +
  geom_polygon(data = shp[shp$ADM2_EN=="Dhaka",], aes(x = long, y = lat, group = group), colour = "red", fill = "red") +
  theme(axis.title.x = element_blank()) + theme(axis.title.y = element_blank()) +
  labs(title = "Map of Bangladesh")+
  scale_y_continuous(breaks=NULL) + scale_x_continuous(breaks=NULL)+
  theme(plot.title = element_text(size = 12))

map1

library(grid)
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

map2 <- ggplot() + 
  geom_polygon(data = SL.map, aes(x = long, y = lat, group = group), colour = "cadetblue", fill = "azure2") +
  labs(title = "Location of Chikungunya patients ") +
  xlab(label="Longitute") + ylab(label="Latitute")
map2

map3 <- map2 +  geom_point(data=ChikData, aes(x=Longitude, y=Latitude), colour = "darkgreen", size = 2)+
  theme(axis.text = element_text(size = 30),
        axis.title = element_text(size = 30),
        plot.title = element_text(size = 30))


map3

# Add scale and North arrow
library(ggspatial)
map4 <- map3 +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"
    )
  )

map5 <- map4 + annotation_custom(ggplotGrob(map1), xmin = 90.4, xmax = 90.53, 
                       ymin = 23.91, ymax = 24.06) 

map6 <- map5 + annotate(geom="text", x=90.25, y=23.50, label="Map of Dhaka",
                              color="black", size=10)
map6
library(gridExtra)
tiff("ChikungunyaMap.tiff", units="in", width=10, height=8, res=300)
gridExtra::grid.arrange(map6, nrow=1, ncol=1)
dev.off()





# #######Count GLM
# options(scipen=999)
# ## Chikungunya - BD data ##
# rm(list=ls())
# library(MASS)
# library(tscount)
# 
# chikts <- read.csv("ChikTS.csv", header=T)  
# names(chikts)
# 
# 
# #ARIMA
# library(tseries)
# library(forecast)
# library(zoo)
# d1=zoo(chikts$Pos, seq(from = as.Date("2024-10-19"), to = as.Date("2024-12-11"), by = 1))
# tsdata <- ts(d1, frequency = 365)
# 
# auto.arima(tsdata)
# 
# Fit<-Arima(tsdata,order=c(2,1,1))
# summary(Fit)
# 
# fcast <- forecast(Fit, h=10)
# 
# library(ggfortify)
# z <- autoplot(fcast, size = 2) +
#   xlab("Days") + ylab("Number of Chikungunya cases") +ggtitle("ARIMA Model")+
#   guides(colour=guide_legend(title="Observed data"),
#          fill=guide_legend(title="Prediction interval"))+ theme(legend.position="bottom") + theme_bw()+
#   theme( legend.text = element_text(color = "Black", size = 18),
#          text = element_text(size = 18))
# z
# 
# tiff("arima.tiff", units="in", width=12, height=6, res=300)
# gridExtra::grid.arrange(z)
# dev.off()
# 
# #Menn kendal
# library(Kendall)
# library(trend)
# 
# t.test(tsdata)$"conf.int"
# mean(tsdata)
# 
# library(trend)
# MannKendall(tsdata)
# sens.slope(tsdata, conf.level = 0.95)
# 
# 
# chikdat <- read.csv("CHIKV_Count.csv", header=T)  
# names(chikdat)
# 
# fitglm <- glm(Chik_pos ~ Test_delay + Age_avg + Male_per + Emp_stat_yes + 
#                 Com_yes + T2M + PRECTOTCORR, data=chikdat)
# 
# library(car)
# # Calculating VIF
# vif_values <- vif(fitglm)
# vif_values
# 
# 
# fitglm <- glm(Chik_pos ~ Test_delay + Age_avg + Male_per + Emp_stat_yes + 
#                   Com_yes + T2M + PRECTOTCORR + 
#                 offset(log(Days+1)), data=chikdat, 
#               family=poisson(link = "log"))
# summary(fitglm)
# library(car)
# performance::performance(fitglm)
# round(exp(coef(fitglm)),2)
# round(exp(confint(fitglm)),2)





#Figure 1
library(ggplot2)
library("stringr") 
chikdat <- read.csv("CHIKV_Count.csv", header=T)  
df <- data.frame(Date=chikdat$Collection_Date,
Count=chikdat$Collection_Cases)
head(df)

b<-ggplot(data=df, aes(x=Date, y=Count)) +
  geom_bar(stat="identity", fill="darkgreen")+
  theme_minimal()  +
  scale_fill_brewer() + geom_text(aes(label=Count), vjust=-0.5, color="black",
                                  position = position_dodge(1), size=5)+
  xlab("Sample Collection Date") + ylab("Chikungunya Cases") + 
  theme(axis.text = element_text(size = 15,angle = 90, vjust = 1, hjust=0.5),
        axis.title = element_text(size = 15),
        plot.title = element_text(size = 15),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15))
b 


df <- data.frame(Date=chikdat$Onset_Date,
                 Count=chikdat$Onset_Cases)
head(df)

c<-ggplot(data=df, aes(x=Date, y=Count)) +
  geom_bar(stat="identity", fill="darkgreen")+
  theme_minimal()  +
  scale_fill_brewer() + geom_text(aes(label=Count), vjust=-0.5, color="black",
                                  position = position_dodge(1), size=5)+
  xlab("Onset Date") + ylab("Chikungunya Cases") + 
  theme(axis.text = element_text(size = 15,angle = 90, vjust = 1, hjust=0.5),
        axis.title = element_text(size = 15),
        plot.title = element_text(size = 15),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15))
c 




library(gridExtra)
tiff("ChikungunyaCount.tiff", units="in", width=12, height=12, res=300)
gridExtra::grid.arrange(b, c, nrow=2, ncol=1)
dev.off()


#Pie chart
df <- data.frame(
  Sex = c("Female", "Male"),
  value = c(35.51,
            64.49
  )
)
head(df)

library(ggplot2)

x <- ggplot(df, aes(x = "", y = value, fill = Sex)) +
  geom_col(color = "black") +
  geom_text(aes(label = value),cex=8,
            position = position_stack(vjust = 0.5))  + ggtitle("Chikungunya Cases") +
  coord_polar(theta = "y") +
  scale_fill_brewer() +
  theme_void()+ ylab("") + theme(legend.title = element_text(size=15),
                                 legend.text = element_text(size=15),
                                 plot.title = element_text(hjust = 0.5),
                                 text=element_text(size=15))

x


df <- data.frame(
  Age = c("<30", "≥30"),
  value = c(16.67,
            83.33
  )
)
head(df)

library(ggplot2)

y <- ggplot(df, aes(x = "", y = value, fill = Age)) +
  geom_col(color = "black") +
  geom_text(aes(label = value),cex=8,
            position = position_stack(vjust = 0.5))  + ggtitle("Chikungunya Cases") +
  coord_polar(theta = "y") +
  scale_fill_brewer() +
  theme_void()+ ylab("") + theme(legend.title = element_text(size=15),
                                 legend.text = element_text(size=15),
                                 plot.title = element_text(hjust = 0.5),
                                 text=element_text(size=15))

y


library(gridExtra)
tiff("CCSex.tiff", units="in", width=10, height=5, res=300)
gridExtra::grid.arrange(x, y, ncol=2, nrow=1)
dev.off()





# #Figure 1
# library(ggplot2)
# library("stringr") 
# 
# c <- table(ChikData$Age_cat)
# c
# 
# df <- data.frame(Age=c("<=19", "20-29", "30-39", "40-49", "50-59", "60+"),
#                  Count=c(5, 13, 30, 26, 18, 20))
# head(df)
# 
# b<-ggplot(data=df, aes(x=Age, y=Count)) +
#   geom_bar(stat="identity", fill="darkgreen")+
#   theme_minimal()  +
#   scale_fill_brewer() + geom_text(aes(label=Count), vjust=-0.5, color="black",
#                                   position = position_dodge(1), size=5)+
#   xlab("Age Group") + ylab("Chikungunya Cases") + 
#   theme(axis.text = element_text(size = 15,angle = 90, vjust = 1, hjust=0.5),
#         axis.title = element_text(size = 15),
#         plot.title = element_text(size = 15),
#         legend.title = element_text(size=15),
#         legend.text = element_text(size=15))
# b 
# library(gridExtra)
# tiff("ChikungunyaAge.tiff", units="in", width=10, height=8, res=300)
# gridExtra::grid.arrange(b, nrow=1, ncol=1)
# dev.off()


#Figure 1
library(ggplot2)
library("stringr") 

df <- data.frame(Symptoms=c("Fever", "Rash", "Arthralgia", "Arthritis", "Conjunctivitis",
                        "Myalgia", "Headache", "Vomiting", "Diarrhea", "Others"),
                 Percentage=c(100.00,
                              24.09,
                              97.81,
                              2.02,
                              46.72,
                              83.21,
                              64.96,
                              29.93,
                              1.46,
                              2.90
                 ))
head(df)

b<-ggplot(data=df, aes(x=reorder(Symptoms, -Percentage, sum), y=Percentage)) +
  geom_bar(stat="identity", fill="tomato4")+
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
  value = c(46.38,
            53.62
  )
)
head(df)

library(ggplot2)

c <- ggplot(df, aes(x = "", y = value, fill = Symptoms)) +
  geom_col(color = "black") +
  geom_text(aes(label = value),cex=15,
            position = position_stack(vjust = 0.5))  + ggtitle("Clinical signs on chikungunya patients") +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette="cyan") +
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
                 Percentage=c(5.11,
                              7.30,
                              0.73,
                              26.28,
                              9.49,
                              26.28,
                              3.65,
                              1.46,
                              0.73,
                              1.46
                 ))
head(df)

b<-ggplot(data=df, aes(x=reorder(Comorbidities, -Percentage, sum), y=Percentage)) +
  geom_bar(stat="identity", fill="tomato4")+
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
  value = c(47.45,
            52.55
  )
)
head(df)

library(ggplot2)

y <- ggplot(df, aes(x = "", y = value, fill = Comorbidity)) +
  geom_col(color = "black") +
  geom_text(aes(label = value),cex=15,
            position = position_stack(vjust = 0.5))  + ggtitle("Co-morbidities on chikungunya patients") +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette="cyan") +
  theme_void()+ ylab("") + theme(legend.title = element_text(size=15),
                                 legend.text = element_text(size=15),
                                 plot.title = element_text(hjust = 0.5),
                                 text=element_text(size=15))

y


library(gridExtra)
tiff("ChikungunyaComorbidities.tiff", units="in", width=16, height=8, res=300)
gridExtra::grid.arrange(b,y, nrow=1, ncol=2)
dev.off()

