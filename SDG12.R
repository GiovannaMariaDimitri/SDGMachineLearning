
library(eurostat)
library(dplyr)
library(arsenal)
library(ggplot2)
library(apcluster)
library(plotly)
library(plot.matrix)

## Getting data from Eurostat and selecting indicators ##

# SDG 12

CMUR <- get_eurostat(id = "sdg_12_41", time_format = "num", type = "code") # Circular Material Use Rate 
CMUR$values = (CMUR$values - min(CMUR$values)) / (max(CMUR$values) - min(CMUR$values))


GW <- get_eurostat(id = "sdg_12_50", time_format = "num", type = "code") # Generation of Waste (No mineral wastes, ALL + household, 
# kg per habitant)
GW <- subset(GW, GW$geo != 'EE')
GW_TOT <- subset(GW, GW$hazard == 'HAZ_NHAZ')
GW_TOT$values = (GW_TOT$values - min(GW_TOT$values)) / (max(GW_TOT$values) - min(GW_TOT$values))

GW_HAZ <- subset(GW, GW$hazard == 'HAZ')
GW_HAZ$values = (GW_HAZ$values - min(GW_HAZ$values)) / (max(GW_HAZ$values) - min(GW_HAZ$values))

GW_NHAZ <- subset(GW, GW$hazard == 'NHAZ')
GW_NHAZ$values = (GW_NHAZ$values - min(GW_NHAZ$values)) / (max(GW_NHAZ$values) - min(GW_NHAZ$values))



GVA <- get_eurostat(id = "sdg_12_61", time_format = "num", type = "code") # Gross Value Added

GVA_MI <- subset(GVA, GVA$unit == 'MEUR_CLV10') # Million of euros (Chain linked volume RY 2010)
GVA_MI <- subset(GVA_MI, GVA_MI$geo != 'EU27_2020')
GVA_MI$values[is.na(GVA_MI$values)] <- mean(GVA_MI$values, na.rm = TRUE)
GVA_MI$values = (GVA_MI$values - min(GVA_MI$values)) / (max(GVA_MI$values) - min(GVA_MI$values))
GVA_PC <- subset(GVA, GVA$unit == 'PC_GDP') # Percentage of GDP
GVA_PC$values[is.na(GVA_PC$values)] <- mean(GVA_PC$values, na.rm = TRUE)
GVA_PC$values = (GVA_PC$values - min(GVA_PC$values)) / (max(GVA_PC$values) - min(GVA_PC$values))


CCH <- get_eurostat(id = "sdg_12_10", time_format = "num", type = "code") # Hazardous Chemicals (not useful, not divided in countries!)
CCH$values = (CCH$values - min(CCH$values)) / (max(CCH$values) - min(CCH$values))


RMC <- get_eurostat(id = "sdg_12_21", time_format = "num", type = "code")  # Raw Material Consumption
RMC <- subset(RMC, RMC$unit == 'T_HAB')
RMC$values = (RMC$values - min(RMC$values)) / (max(RMC$values) - min(RMC$values))


## Removing useless columns ## 

# CMUR

CMUR <- CMUR[, -1]

# GW

GW_HAZ <- GW_HAZ[, -1:-4]
GW_NHAZ <- GW_NHAZ[, -1:-4]
GW_TOT <- GW_TOT[, -1:-4]

# GVA 

GVA_MI <- GVA_MI[, -1:-5]
GVA_PC <- GVA_PC[, -1:-5]

# CCH 

CCH <- CCH[,-1:-3]

# RMC

RMC <- RMC[,-1:-3]


## Dividing by years ##

# CMUR

CMUR$time <- as.numeric(CMUR$time)
CMUR_ <- CMUR
for(i in CMUR$time){
  assign(paste0("CMUR", i), CMUR_)
  CMUR_<- subset(CMUR, CMUR$time == i)
  #row.names(CMUR_) <- CMUR_$geo
}

# GW

GW_HAZ$time <- as.numeric(GW_HAZ$time)
GW_HAZ_ <- GW_HAZ
for(i in GW_HAZ$time){
  assign(paste0("GW_HAZ", i), GW_HAZ_)
  GW_HAZ_<- subset(GW_HAZ, GW_HAZ$time == i)
  #row.names(GW_HAZ_) <- GW_HAZ_$geo
}


GW_NHAZ$time <- as.numeric(GW_NHAZ$time)
GW_NHAZ_ <- GW_NHAZ
for(i in GW_NHAZ$time){
  assign(paste0("GW_NHAZ", i), GW_NHAZ_)
  GW_NHAZ_<- subset(GW_NHAZ, GW_NHAZ$time == i)
  #row.names(GW_NHAZ_) <- GW_NHAZ_$geo
}


GW_TOT$time <- as.numeric(GW_TOT$time)
GW_TOT_ <- GW_TOT
for(i in GW_TOT$time){
  assign(paste0("GW_TOT", i), GW_TOT_)
  GW_TOT_<- subset(GW_TOT, GW_TOT$time == i)
  #row.names(GW_TOT_) <- GW_TOT_$geo
}


# GVA

GVA_PC$time <- as.numeric(GVA_PC$time)
GVA_PC_ <- GVA_PC
for(i in GVA_PC$time){
  assign(paste0("GVA_PC", i), GVA_PC_)
  GVA_PC_<- subset(GVA_PC, GVA_PC$time == i)
  #row.names(GVA_PC_) <- GVA_PC_$geo
}


GVA_MI$time <- as.numeric(GVA_MI$time)
GVA_MI_ <- GVA_MI
for(i in GVA_MI$time){
  assign(paste0("GVA_MI", i), GVA_MI_)
  GVA_MI_<- subset(GVA_MI, GVA_MI$time == i)
  #row.names(GVA_MI_) <- GVA_MI_$geo
  
}


# RMC

RMC$time <- as.numeric(RMC$time)
RMC_ <- RMC
for(i in RMC$time){
  assign(paste0("RMC", i), RMC_)
  RMC_<- subset(RMC, RMC$time == i)
  #row.names(RMC_) <- RMC_$geo
}


# Deleting the columns that are not meaningful

# CMUR

for(i in CMUR$time){
  CMUR_T <- eval(parse( text = paste0("CMUR", i)))
  #CMUR_T <- CMUR_T[, 3]
  assign(paste0("CMUR_FINAL", i), CMUR_T)
}

# Deleting all the countries that appear only in some years and not in others

for (i in CMUR$time){
  CMUR_T <- eval(parse( text = paste0("CMUR", i)))
  CMUR_T = CMUR_T[(CMUR_T$geo %in% CMUR2010$geo), ]
  CMUR_T <- CMUR_T[, -2]
  assign(paste0("CMUR_FINAL", i), CMUR_T)
}


# GW

for(i in GW_HAZ$time){
  GW_HAZ_T <- eval(parse( text = paste0("GW_HAZ", i)))
  #GW_HAZ_T <- GW_HAZ_T[, 3]
  assign(paste0("GW_HAZ_FINAL", i), GW_HAZ_T)
}

# Deleting all the countries that appear only in some years and not in others

for (i in GW_HAZ$time){
  GW_HAZ_T <- eval(parse( text = paste0("GW_HAZ", i)))
  GW_HAZ_T = GW_HAZ_T[(GW_HAZ_T$geo %in% GW_HAZ2008$geo), ]
  GW_HAZ_T <- GW_HAZ_T[, -2]
  assign(paste0("GW_HAZ_FINAL", i), GW_HAZ_T)
}

for(i in GW_NHAZ$time){
  GW_NHAZ_T <- eval(parse( text = paste0("GW_NHAZ", i)))
  #GW_NHAZ_T <- GW_NHAZ_T[, 3]
  assign(paste0("GW_NHAZ_FINAL", i), GW_NHAZ_T)
}

# Deleting all the countries that appear only in some years and not in others

for (i in GW_NHAZ$time){
  GW_NHAZ_T <- eval(parse( text = paste0("GW_NHAZ", i)))
  GW_NHAZ_T = GW_NHAZ_T[(GW_NHAZ_T$geo %in% GW_NHAZ2008$geo), ]
  GW_NHAZ_T <- GW_NHAZ_T[, -2]
  assign(paste0("GW_NHAZ_FINAL", i), GW_NHAZ_T)
}

for(i in GW_TOT$time){
  GW_TOT_T <- eval(parse( text = paste0("GW_TOT", i)))
  #GW_TOT_T <- GW_TOT_T[, 3]
  assign(paste0("GW_TOT_FINAL", i), GW_TOT_T)
}

# Deleting all the countries that appear only in some years and not in others

for (i in GW_TOT$time){
  GW_TOT_T <- eval(parse( text = paste0("GW_TOT", i)))
  GW_TOT_T = GW_TOT_T[(GW_TOT_T$geo %in% GW_TOT2008$geo), ]
  GW_TOT_T <- GW_TOT_T[, -2]
  assign(paste0("GW_TOT_FINAL", i), GW_TOT_T)
}


# GVA

for(i in GVA_MI$time){
  GVA_MI_T <- eval(parse( text = paste0("GVA_MI", i)))
  #GVA_MI_T <- GVA_MI_T[, 3]
  assign(paste0("GVA_MI_FINAL", i), GVA_MI_T)
}


for(i in GVA_PC$time){
  GVA_PC_T <- eval(parse( text = paste0("GVA_PC", i)))
  #GVA_PC_T <- GVA_PC_T[, 3]
  assign(paste0("GVA_PC_FINAL", i), GVA_PC_T)
}



# CCH

for(i in CCH$time){
  CCH_T <- eval(parse( text = paste0("CCH", i)))
  #CCH_T <- CCH_T[, 3]
  assign(paste0("CCH_FINAL", i), CCH_T)
}

# Deleting all the countries that appear only in some years and not in others

for (i in CCH$time){
  CCH_T <- eval(parse( text = paste0("CCH", i)))
  CCH_T = CCH_T[(CCH_T$geo %in% CCH2003$geo), ]
  CCH_T <- CCH_T[, -2]
  assign(paste0("CCH_FINAL", i), CCH_T)
}


# RMC

for(i in RMC$time){
  RMC_T <- eval(parse( text = paste0("RMC", i)))
  #RMC_T <- RMC_T[, 3]
  assign(paste0("RMC_FINAL", i), RMC_T)
}

# Deleting all the countries that appear only in some years and not in others

for (i in RMC$time){
  RMC_T <- eval(parse( text = paste0("RMC", i)))
  RMC_T = RMC_T[(RMC_T$geo %in% RMC2010$geo), ]
  RMC_T <- RMC_T[, -2]
  assign(paste0("RMC_FINAL", i), RMC_T)
}


### CLUSTERING ###

# CMUR

for(i in (2004:2020)){
  CMUR_CLUSTER <- eval(parse(text = paste0('CMUR_FINAL', i)))
  CMUR_CLUSTER_ <- apcluster(negDistMat(r=2), CMUR_CLUSTER)
  idx = as.factor(CMUR_CLUSTER_@idx)
  a = CMUR_CLUSTER
  a$geo <- as.factor(a$geo)
  a = cbind(a, idx)
  fig <- plot_ly(data = a, x=a$geo, y=a$values, color=a$idx, type='scatter',
                 mode = 'markers', text = ~paste("Country: ", a$geo, '<br>Value:', a$values), size = a$values)%>%
    layout(title = paste0('Circular Material Use Rate \n',i), 
           xaxis = list(title = 'Countries'),
           yaxis = list(title = 'Percentage'))
  
  fig <- fig %>% layout(autosize = F)
  print(fig)
  
  plotly::export(fig, file = paste0('CMUR', i, '.png'))
  assign(paste0('CMUR_AP', i), CMUR_CLUSTER_)
}

m = list()

for(f in (2010:2019)){
  
  l_CMUR = eval(parse(text = (paste0('CMUR_AP', f))))
  l_CMUR = as.list(l_CMUR@clusters)
  m[f-2000] = list(GlobalMultiplexityFun(l_CMUR)) 
}

M_CMUR <- matrix (0, ncol = 30, nrow = 30)
for (i in 10:19){
  M_CMUR = M_CMUR + m[[i]]
}

rownames(M_CMUR) <- CMUR_FINAL2010$geo
colnames(M_CMUR) <- CMUR_FINAL2010$geo

par(mar=c(5.1, 4.1, 4.1, 4.1))
plot(M_CMUR, col = c( 'red', 'orange', 'yellow', 'darkgreen' ), axis.row = (list(side = 4, las = 2, cex = 0.5)), 
     key = list(side = 3, cex.axis = 0.5), axis.col = list(las = 2, cex = 0.3),
     xlab = 'Countries', ylab = 'Countries', main = 'CMUR', text.cell = list( cex = 0.5), digits = 0, breaks = c(0, 3, 6, 9, 12))



# GW_HAZ

for(i in seq(2004, 2020, by = 2)){
  GW_HAZ_CLUSTER <- eval(parse(text = paste0('GW_HAZ_FINAL', i)))
  GW_HAZ_CLUSTER_ <- apcluster(negDistMat(r=2), GW_HAZ_CLUSTER)
  idx = as.factor(GW_HAZ_CLUSTER_@idx)
  a = GW_HAZ_CLUSTER
  a$geo <- as.factor(a$geo)
  a = cbind(a, idx)
  fig <- plot_ly(data = a, x=a$geo, y=a$values, color=a$idx, type='scatter',
                 mode = 'markers', text = ~paste("Country: ", a$geo, '<br>Value:', a$values), size = a$values)%>%
    layout(title = paste0('Generation of Hazardous Waste \n',i), 
           xaxis = list(title = 'Countries'),
           yaxis = list(title = 'Kg per habitant'))
  fig <- fig %>% layout(autosize = F)
  print(fig)
  
  plotly::export(fig, file = paste0('GW_HAZ', i, '.png'))
  assign(paste0('GW_HAZ_AP', i), GW_HAZ_CLUSTER_)
}

m = list()

for(f in seq(2008, 2018, by = 2)){
  
  l_GW_HAZ = eval(parse(text = (paste0('GW_HAZ_AP', f))))
  l_GW_HAZ = as.list(l_GW_HAZ@clusters)
  m[f-2000] = list(GlobalMultiplexityFun(l_GW_HAZ)) 
}

M_GW_HAZ <- matrix (0, ncol = 34, nrow = 34)
for (i in seq(8, 18, by = 2)){
  M_GW_HAZ = M_GW_HAZ + m[[i]]
}

rownames(M_GW_HAZ) <- GW_HAZ_FINAL2008$geo
colnames(M_GW_HAZ) <- GW_HAZ_FINAL2008$geo

par(mar=c(5.1, 4.1, 4.1, 4.1))
plot(M_GW_HAZ, col = c( 'red', 'orange', 'yellow', 'darkgreen' ), axis.row = (list(side = 4, las = 2, cex = 0.5)), 
     key = list(side = 3, cex.axis = 0.5), axis.col = list(las = 2, cex = 0.3),
     xlab = 'Countries', ylab = 'Countries', main = 'Hazarous Wastes', text.cell = list( cex = 0.5), digits = 0, breaks = c(0, 2, 4, 6, 8))



# GW_NHAZ

for(i in seq(2004, 2020, by = 2)){
  GW_NHAZ_CLUSTER <- eval(parse(text = paste0('GW_NHAZ_FINAL', i)))
  GW_NHAZ_CLUSTER_ <- apcluster(negDistMat(r=2), GW_NHAZ_CLUSTER)
  idx = as.factor(GW_NHAZ_CLUSTER_@idx)
  a = GW_NHAZ_CLUSTER
  a$geo <- as.factor(a$geo)
  a = cbind(a, idx)
  fig <- plot_ly(data = a, x=a$geo, y=a$values, color=a$idx, type='scatter',
                 mode = 'markers', text = ~paste("Country: ", a$geo, '<br>Value:', a$values), size = a$values)%>%
    layout(title = paste0('Generation of Non-Hazardous Waste \n',i), 
           xaxis = list(title = 'Countries'),
           yaxis = list(title = 'Kg per habitants'))
  
  fig <- fig %>% layout(autosize = F)
  print(fig)
  
  plotly::export(fig, file = paste0('GW_NHAZ', i, '.png'))
  assign(paste0('GW_NHAZ_AP', i), GW_NHAZ_CLUSTER_)
}

m = list()

for(f in seq(2008, 2018, by = 2)){
  
  l_GW_NHAZ = eval(parse(text = (paste0('GW_NHAZ_AP', f))))
  l_GW_NHAZ = as.list(l_GW_NHAZ@clusters)
  m[f-2000] = list(GlobalMultiplexityFun(l_GW_NHAZ)) 
}

M_GW_NHAZ <- matrix (0, ncol = 34, nrow = 34)
for (i in seq(8, 18, by = 2)){
  M_GW_NHAZ = M_GW_NHAZ + m[[i]]
}

rownames(M_GW_NHAZ) <- GW_NHAZ_FINAL2008$geo
colnames(M_GW_NHAZ) <- GW_NHAZ_FINAL2008$geo

par(mar=c(5.1, 4.1, 4.1, 4.1))
plot(M_GW_NHAZ, col = c( 'red', 'orange', 'yellow', 'darkgreen' ), axis.row = (list(side = 4, las = 2, cex = 0.5)), 
     key = list(side = 3, cex.axis = 0.5), axis.col = list(las = 2, cex = 0.3),
     xlab = 'Countries', ylab = 'Countries', main = 'Non-Hazardous Waste', text.cell = list( cex = 0.5), digits = 0, breaks = c(0, 2, 4, 6, 8))



# GW_TOT

for(i in seq(2004, 2020, by = 2)){
  GW_TOT_CLUSTER <- eval(parse(text = paste0('GW_TOT_FINAL', i)))
  GW_TOT_CLUSTER_ <- apcluster(negDistMat(r=2), GW_TOT_CLUSTER)
  idx = as.factor(GW_TOT_CLUSTER_@idx)
  a = GW_TOT_CLUSTER
  a$geo <- as.factor(a$geo)
  a = cbind(a, idx)
  fig <- plot_ly(data = a, x=a$geo, y=a$values, color=a$idx, type='scatter',
                 mode = 'markers', text = ~paste("Country: ", a$geo, '<br>Value:', a$values), size = a$values)%>%
    layout(title = paste0('Generation of Total Waste \n',i), 
           xaxis = list(title = 'Countries'),
           yaxis = list(title = 'Kg per habitant'))
  
  fig <- fig %>% layout(autosize = F)
  print(fig)
  
  plotly::export(fig, file = paste0('GW_TOT', i, '.png'))
  assign(paste0('GW_TOT_AP', i), GW_TOT_CLUSTER_)
}

# m = list()
# 
# for(f in seq(2012, 2018, by = 2)){
#   
#   l_GW_TOT = eval(parse(text = (paste0('AFI_I10_AP', f))))
#   l_GW_TOT = as.list(l_GW_TOT@clusters)
#   m[f-2000] = list(GlobalMultiplexityFun(l_GW_TOT)) 
# }
# 
# M_GW_TOT <- matrix (0, ncol = 38, nrow = 38)
# for (i in seq(12, 18, by = 2)){
#   M_GW_TOT = M_GW_TOT + m[[i]]
# }
# 
# rownames(M_GW_TOT) <- GW_TOT_FINAL2008$geo
# colnames(M_GW_TOT) <- GW_TOT_FINAL2008$geo
# print(M_GW_TOT)

# GVA_MI

for(i in (2010:2020)){
  GVA_MI_CLUSTER <- eval(parse(text = paste0('GVA_MI_FINAL', i)))
  GVA_MI_CLUSTER_ <- apcluster(negDistMat(r=2), GVA_MI_CLUSTER)
  idx = as.factor(GVA_MI_CLUSTER_@idx)
  a = GVA_MI_CLUSTER
  a$geo <- as.factor(a$geo)
  a = cbind(a, idx)
  fig <- plot_ly(data = a, x=a$geo, y=a$values, color=a$idx, type='scatter',
                 mode = 'markers', text = ~paste("Country: ", a$geo, '<br>Value:', a$values), size = a$values)%>%
    layout(title = paste0('Gross Value Added \n',i), 
           xaxis = list(title = 'Countries'),
           yaxis = list(title = 'Million of Euros'))
  
  fig <- fig %>% layout(autosize = F)
  print(fig)
  
  plotly::export(fig, file = paste0('GVA_MI', i, '.png'))
  assign(paste0('GVA_MI_AP', i), GVA_MI_CLUSTER_)
}

# m = list()
# 
# for(f in (2002:2021)){
#   
#   l_GVA_MI = eval(parse(text = (paste0('AFI_I10_AP', f))))
#   l_GVA_MI = as.list(l_GVA_MI@clusters)
#   m[f-2000] = list(GlobalMultiplexityFun(l_GVA_MI)) 
# }
# 
# M_GVA_MI <- matrix (0, ncol = 32, nrow = 32)
# for (i in 1:20){
#   M_GVA_MI = M_GVA_MI + m[[i]]
# }
# print(M_GVA_MI)

# GVA_PC

for(i in (2010:2020)){
  GVA_PC_CLUSTER <- eval(parse(text = paste0('GVA_PC_FINAL', i)))
  GVA_PC_CLUSTER_ <- apcluster(negDistMat(r=2), GVA_PC_CLUSTER)
  idx = as.factor(GVA_PC_CLUSTER_@idx)
  a = GVA_PC_CLUSTER
  a$geo <- as.factor(a$geo)
  a = cbind(a, idx)
  fig <- plot_ly(data = a, x=a$geo, y=a$values, color=a$idx, type='scatter',
                 mode = 'markers', text = ~paste("Country: ", a$geo, '<br>Value:', a$values), size = a$values)%>%
    layout(title = paste0('Gross Value Added \n',i), 
           xaxis = list(title = 'Countries'),
           yaxis = list(title = 'Percentage of GDP'))
  fig <- fig %>% layout(autosize = F)
  print(fig)
  
  plotly::export(fig, file = paste0('GVA_PC', i, '.png'))
  assign(paste0('GVA_PC_AP', i), GVA_PC_CLUSTER_)
}



# m = list()
# 
# for(f in (2002:2021)){
#   
#   l_GVA_PC = eval(parse(text = (paste0('AFI_I10_AP', f))))
#   l_GVA_PC = as.list(l_GVA_PC@clusters)
#   m[f-2000] = list(GlobalMultiplexityFun(l_GVA_PC)) 
# }
# 
# M_GVA_PC <- matrix (0, ncol = 32, nrow = 32)
# for (i in 1:20){
#   M_GVA_PC = M_GVA_PC + m[[i]]
# }
# print(M_GVA_PC)

# RMC

for(i in (2000:2020)){
  RMC_CLUSTER <- eval(parse(text = paste0('RMC_FINAL', i)))
  RMC_CLUSTER_ <- apcluster(negDistMat(r=2), RMC_CLUSTER)
  idx = as.factor(RMC_CLUSTER_@idx)
  a = RMC_CLUSTER
  a$geo <- as.factor(a$geo)
  a = cbind(a, idx)
  fig <- plot_ly(data = a, x=a$geo, y=a$values, color=a$idx, type='scatter',
                 mode = 'markers', text = ~paste("Country: ", a$geo, '<br>Value:', a$values), size = a$values)%>%
    layout(title = paste0('Raw Material Consumption \n',i), 
           xaxis = list(title = 'Countries'),
           yaxis = list(title = 'Tonnes per Habitant'))
  fig <- fig %>% layout(autosize = F)
  print(fig)
  
  plotly::export(fig, file = paste0('RMC', i, '.png'))
  assign(paste0('RMC_AP', i), RMC_CLUSTER_)
}

m = list()

for(f in (2008:2019)){
  
  l_RMC = eval(parse(text = (paste0('RMC_AP', f))))
  l_RMC = as.list(l_RMC@clusters)
  m[f-2000] = list(GlobalMultiplexityFun(l_RMC)) 
}

M_RMC <- matrix (0, ncol = 29, nrow = 29)
for (i in 8:19){
  M_RMC = M_RMC + m[[i]]
}

rownames(M_RMC) <- RMC_FINAL2010$geo
colnames(M_RMC) <- RMC_FINAL2010$geo

par(mar=c(5.1, 4.1, 4.1, 4.1))
plot(M_RMC, col = c( 'red', 'orange', 'yellow', 'darkgreen' ), axis.row = (list(side = 4, las = 2, cex = 0.5)), 
     key = list(side = 3, cex.axis = 0.5), axis.col = list(las = 2, cex = 0.3),
     xlab = 'Countries', ylab = 'Countries', main = 'RMC', text.cell = list( cex = 0.5), digits = 0, breaks = c(0, 3, 6, 9, 12))



