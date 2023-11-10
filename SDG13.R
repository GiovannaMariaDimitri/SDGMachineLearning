
library(eurostat)
library(dplyr)
library(arsenal)
library(ggplot2)
library(apcluster)
library(plotly)
library(plot.matrix)

## Getting data from Eurostat and selecting indicators ##

# SDG 13

CC <- get_eurostat(id = "sdg_13_50", time_format = "num", type = "code") # Contribution to Commitment
CC$values = (CC$values - min(CC$values)) / (max(CC$values) - min(CC$values))

EL_ENV <- get_eurostat(id = "sdg_13_40", time_format = "num", type = "code") # Economic Losses
EL_ENV <- subset(EL_ENV, EL_ENV$unit == 'EUR_HAB')

EL_ENV_VAL_A <- subset(EL_ENV, EL_ENV$statinfo == 'VAL_A') 
EL_ENV_VAL_A$values = (EL_ENV_VAL_A$values - min(EL_ENV_VAL_A$values)) / (max(EL_ENV_VAL_A$values) - min(EL_ENV_VAL_A$values))
EL_ENV_AVG <- subset(EL_ENV, EL_ENV$statinfo == 'AVG_30Y')
EL_ENV_AVG$values = (EL_ENV_AVG$values - min(EL_ENV_AVG$values)) / (max(EL_ENV_AVG$values) - min(EL_ENV_AVG$values))

GAS <- get_eurostat(id = "sdg_13_10", time_format = "num", type = "code") # Gas Emissions
GAS <- subset(GAS, GAS$src_crf == 'TOTX4_MEMONIA')

GAS_T_HAB <- subset(GAS, GAS$unit == 'T_HAB')
GAS_T_HAB$values = (GAS_T_HAB$values - min(GAS_T_HAB$values)) / (max(GAS_T_HAB$values) - min(GAS_T_HAB$values))
GAS_I90 <- subset(GAS, GAS$unit == 'I90')
GAS_I90$values = (GAS_I90$values - min(GAS_I90$values)) / (max(GAS_I90$values) - min(GAS_I90$values))


LULUCF <- get_eurostat(id = "sdg_13_21", time_format = "num", type = "code") # LULUCF
LULUCF <- subset(LULUCF, LULUCF$geo != 'IS')
LULUCF_T_HAB <- subset(LULUCF, LULUCF$unit == 'T_HAB') # Tonnes per Habitant 
LULUCF_T_HAB$values = (LULUCF_T_HAB$values - min(LULUCF_T_HAB$values)) / (max(LULUCF_T_HAB$values) - min(LULUCF_T_HAB$values))
LULUCF_T_KM2 <- subset(LULUCF, LULUCF$unit == 'T_KM2') # Tonnes per km2
LULUCF_T_KM2$values = (LULUCF_T_KM2$values - min(LULUCF_T_KM2$values)) / (max(LULUCF_T_KM2$values) - min(LULUCF_T_KM2$values))

CMCE <- get_eurostat(id = "sdg_13_60", time_format = "num", type = "code") # Population Covered 
CMCE <- subset(CMCE, CMCE$unit == 'PC_POP')
CMCE$values = (CMCE$values - min(CMCE$values)) / (max(CMCE$values) - min(CMCE$values))

## Removing useless columns ## 

# CC 

CC <- CC[, -1]

# EL_ENV 

EL_ENV_VAL_A <- EL_ENV_VAL_A[, -1:-3]
EL_ENV_AVG <- EL_ENV_AVG[, -1:-3]

# GAS 

GAS_T_HAB <- GAS_T_HAB[, -1:-3]
GAS_I90 <- GAS_I90[, -1:-3]


# LULUCF 

LULUCF_T_HAB <- LULUCF_T_HAB[, -1:-3]
LULUCF_T_KM2 <- LULUCF_T_KM2[, -1:-3]


# CMCE

CMCE <- CMCE[, -1]


## Dividing each dataset by year ## 

# CC

CC$time <- as.numeric(CC$time)
CC_ <- CC
for(i in CC$time){
  assign(paste0("CC", i), CC_)
  CC_<- subset(CC, CC$time == i)
  row.names(CC_) <- CC_$geo
}

# EL_ENV

EL_ENV_VAL_A$time <- as.numeric(EL_ENV_VAL_A$time)
EL_ENV_VAL_A_ <- EL_ENV_VAL_A
for(i in EL_ENV_VAL_A$time){
  assign(paste0("EL_ENV_VAL_A", i), EL_ENV_VAL_A_)
  EL_ENV_VAL_A_<- subset(EL_ENV_VAL_A, EL_ENV_VAL_A$time == i)
  row.names(EL_ENV_VAL_A_) <- EL_ENV_VAL_A_$geo
}


EL_ENV_AVG$time <- as.numeric(EL_ENV_AVG$time)
EL_ENV_AVG_ <- EL_ENV_AVG
for(i in EL_ENV_AVG$time){
  assign(paste0("EL_ENV_AVG", i), EL_ENV_AVG_)
  EL_ENV_AVG_<- subset(EL_ENV_AVG, EL_ENV_AVG$time == i)
  row.names(EL_ENV_AVG_) <- EL_ENV_AVG_$geo
}

# GAS

GAS_I90$time <- as.numeric(GAS_I90$time)
GAS_I90_ <- GAS_I90
for(i in GAS_I90$time){
  assign(paste0("GAS_I90", i), GAS_I90_)
  GAS_I90_<- subset(GAS_I90, GAS_I90$time == i)
  row.names(GAS_I90_) <- GAS_I90_$geo
}


GAS_T_HAB$time <- as.numeric(GAS_T_HAB$time)
GAS_T_HAB_ <- GAS_T_HAB
for(i in GAS_T_HAB$time){
  assign(paste0("GAS_T_HAB", i), GAS_T_HAB_)
  GAS_T_HAB_<- subset(GAS_T_HAB, GAS_T_HAB$time == i)
  row.names(GAS_T_HAB_) <- GAS_T_HAB_$geo
}

# LULUCF

LULUCF_T_KM2$time <- as.numeric(LULUCF_T_KM2$time)
LULUCF_T_KM2_ <- LULUCF_T_KM2
for(i in LULUCF_T_KM2$time){
  assign(paste0("LULUCF_T_KM2", i), LULUCF_T_KM2_)
  LULUCF_T_KM2_<- subset(LULUCF_T_KM2, LULUCF_T_KM2$time == i)
  row.names(LULUCF_T_KM2_) <- LULUCF_T_KM2_$geo
}

LULUCF_T_HAB$time <- as.numeric(LULUCF_T_HAB$time)
LULUCF_T_HAB_ <- LULUCF_T_HAB
for(i in LULUCF_T_HAB$time){
  assign(paste0("LULUCF_T_HAB", i), LULUCF_T_HAB_)
  LULUCF_T_HAB_<- subset(LULUCF_T_HAB, LULUCF_T_HAB$time == i)
  row.names(LULUCF_T_HAB_) <- LULUCF_T_HAB_$geo
}

# CMCE

CMCE$time <- as.numeric(CMCE$time)
CMCE_ <- CMCE
for(i in CMCE$time){
  assign(paste0("CMCE", i), CMCE_)
  CMCE_<- subset(CMCE, CMCE$time == i)
  row.names(CMCE_) <- CMCE_$geo
}


# Deleting columns that are not meaningful

# CC

for(i in CC$time){
  CC_T <- eval(parse( text = paste0("CC", i)))
  #CC_T <- CC_T[, 3]
  assign(paste0("CC_FINAL", i), CC_T)
}

# Deleting all the countries that appear only in some years and not in others

for (i in CC$time){
  CC_T <- eval(parse( text = paste0("CC", i)))
  CC_T = CC_T[(CC_T$geo %in% CC2015$geo), ]
  CC_T <- CC_T[, -2]
  assign(paste0("CC_FINAL", i), CC_T)
}

# ALMOST IMPOSSIBLE TO PRODUCE THE GLOBAL MULTIPLEXITY MATRIX
# SMALL DATASET WITH DISCONTINUE DATA INDEXING

# EL_ENV

for(i in EL_ENV_VAL_A$time){
  EL_ENV_VAL_A_T <- eval(parse( text = paste0("EL_ENV_VAL_A", i)))
  #EL_ENV_VAL_A_T <- EL_ENV_VAL_A_T[, 3]
  assign(paste0("EL_ENV_VAL_A_FINAL", i), EL_ENV_VAL_A_T)
}

# Deleting all the countries that appear only in some years and not in others

for (i in EL_ENV_VAL_A$time){
  EL_ENV_VAL_A_T <- eval(parse( text = paste0("EL_ENV_VAL_A", i)))
  EL_ENV_VAL_A_T = EL_ENV_VAL_A_T[(EL_ENV_VAL_A_T$geo %in% EL_ENV_VAL_A2016$geo), ]
  EL_ENV_VAL_A_T <- EL_ENV_VAL_A_T[, -2]
  assign(paste0("EL_ENV_VAL_A_FINAL", i), EL_ENV_VAL_A_T)
}


for(i in EL_ENV_AVG$time){
  EL_ENV_AVG_T <- eval(parse( text = paste0("EL_ENV_AVG", i)))
  #EL_ENV_AVG_T <- EL_ENV_AVG_T[, 3]
  assign(paste0("EL_ENV_AVG_FINAL", i), EL_ENV_AVG_T)
}

# Deleting all the countries that appear only in some years and not in others

for (i in EL_ENV_AVG$time){
  EL_ENV_AVG_T <- eval(parse( text = paste0("EL_ENV_AVG", i)))
  EL_ENV_AVG_T = EL_ENV_AVG_T[(EL_ENV_AVG_T$geo %in% EL_ENV_AVG2010$geo), ]
  EL_ENV_AVG_T <- EL_ENV_AVG_T[, -2]
  assign(paste0("EL_ENV_AVG_FINAL", i), EL_ENV_AVG_T)
}

# PERFECT


# GAS

for(i in GAS_T_HAB$time){
  GAS_T_HAB_T <- eval(parse( text = paste0("GAS_T_HAB", i)))
  #GAS_T_HAB_T <- GAS_T_HAB_T[, 3]
  assign(paste0("GAS_T_HAB_FINAL", i), GAS_T_HAB_T)
}

# Deleting all the countries that appear only in some years and not in others

for (i in GAS_T_HAB$time){
  GAST_T_HAB_T <- eval(parse( text = paste0("GAS_T_HAB", i)))
  GAST_T_HAB_T = GAST_T_HAB_T[(GAST_T_HAB_T$geo %in% GAS_T_HAB2010$geo), ]
  GAST_T_HAB_T <- GAST_T_HAB_T[, -2]
  assign(paste0("GAS_T_FINAL", i), GAST_T_HAB_T)
}


for(i in GAS_I90$time){
  GAS_I90_T <- eval(parse( text = paste0("GAS_I90", i)))
  #GAS_I90_T <- GAS_I90_T[, 3]
  assign(paste0("GAS_I90_FINAL", i), GAS_I90_T)
}

# Deleting all the countries that appear only in some years and not in others

for (i in GAS_I90$time){
  GAS_I90_T <- eval(parse( text = paste0("GAS_I90", i)))
  GAS_I90_T = GAS_I90_T[(GAS_I90_T$geo %in% GAS_I902010$geo), ]
  GAS_I90_T <- GAS_I90_T[, -2]
  assign(paste0("GAS_I90_FINAL", i), GAS_I90_T)
}


# LULUCF

for(i in LULUCF_T_HAB$time){
  LULUCF_T_HAB_T <- eval(parse( text = paste0("LULUCF_T_HAB", i)))
  #LULUCF_T_HAB_T <- LULUCF_T_HAB_T[, 3]
  assign(paste0("LULUCF_T_HAB_FINAL", i), LULUCF_T_HAB_T)
}

# Deleting all the countries that appear only in some years and not in others

for (i in LULUCF_T_HAB$time){
  LULUCF_T_HAB_T <- eval(parse( text = paste0("LULUCF_T_HAB", i)))
  LULUCF_T_HAB_T = LULUCF_T_HAB_T[(LULUCF_T_HAB_T$geo %in% LULUCF_T_HAB2010$geo), ]
  LULUCF_T_HAB_T <- LULUCF_T_HAB_T[, -2]
  assign(paste0("LULUCF_T_HAB_FINAL", i), LULUCF_T_HAB_T)
}


for(i in LULUCF_T_KM2$time){
  LULUCF_T_KM2_T <- eval(parse( text = paste0("LULUCF_T_KM2", i)))
  #LULUCF_T_KM2_T <- LULUCF_T_KM2_T[, 3]
  assign(paste0("LULUCF_T_KM2_FINAL", i), LULUCF_T_KM2_T)
}

# Deleting all the countries that appear only in some years and not in others

for (i in LULUCF_T_KM2$time){
  LULUCF_T_KM2_T <- eval(parse( text = paste0("LULUCF_T_KM2", i)))
  LULUCF_T_KM2_T = LULUCF_T_KM2_T[(LULUCF_T_KM2_T$geo %in% LULUCF_T_KM22010$geo), ]
  LULUCF_T_KM2_T <- LULUCF_T_KM2_T[, -2]
  assign(paste0("LULUCF_T_KM2_FINAL", i), LULUCF_T_KM2_T)
}


# CMCE

for(i in CMCE$time){
  CMCE_T <- eval(parse( text = paste0("CMCE", i)))
  #CMCE_T <- CMCE_T[, 3]
  assign(paste0("CMCE_FINAL", i), CMCE_T)
}

# Deleting all the countries that appear only in some years and not in others

for (i in CMCE$time){
  CMCE_T <- eval(parse( text = paste0("CMCE", i)))
  CMCE_T = CMCE_T[(CMCE_T$geo %in% CMCE2019$geo), ]
  CMCE_T <- CMCE_T[, -2]
  assign(paste0("CMCE_FINAL", i), CMCE_T)
}



### CLUSTERING ###

# CC

for(i in (2014:2021)){
  CC_CLUSTER <- eval(parse(text = paste0('CC_FINAL', i)))
  CC_CLUSTER_ <- apcluster(negDistMat(r=2), CC_CLUSTER)
  idx = as.factor(CC_CLUSTER_@idx)
  a = CC_CLUSTER
  a$geo <- as.factor(a$geo)
  a = cbind(a, idx)
  fig <- plot_ly(data = a, x=a$geo, y=a$values, color=a$idx, type='scatter',
                 mode = 'markers', text = ~paste("Country: ", a$geo, '<br>Value:', a$values), size = a$values)%>%
    layout(title = paste0('Contribution To Commitment \n',i), 
           xaxis = list(title = 'Countries'),
           yaxis = list(title = 'Million of Euros'))
  
  fig <- fig %>% layout(autosize = F)
  print(fig)
  
  plotly::export(fig, file = paste0('CC', i, '.png'))
  assign(paste0('CC_AP', i), CC_CLUSTER_)
}

# m = list()
# 
# for(f in (2015:2017)){
#   
#   l_CC = eval(parse(text = (paste0('CC_AP', f))))
#   l_CC = as.list(l_CC@clusters)
#   m[f-2000] = list(GlobalMultiplexityFun(l_CC)) 
# }
# 
# M_CC <- matrix (0, ncol = 29, nrow = 29)
# for (i in 1:3){
#   M_CC = M_CC + m[[i]]
# }
# print(M_CC)

# EL_ENV_VAL_A

for(i in (1980:2020)){
  EL_ENV_VAL_A_CLUSTER <- eval(parse(text = paste0('EL_ENV_VAL_A', i)))
  EL_ENV_VAL_A_CLUSTER_ <- apcluster(negDistMat(r=2), EL_ENV_VAL_A_CLUSTER)
  idx = as.factor(EL_ENV_VAL_A_CLUSTER_@idx)
  a = EL_ENV_VAL_A_CLUSTER
  a$geo <- as.factor(a$geo)
  a = cbind(a, idx)
  fig <- plot_ly(data = a, x=a$geo, y=a$values, color=a$idx, type='scatter',
                 mode = 'markers', text = ~paste("Country: ", a$geo, '<br>Value:', a$values), size = a$values)%>%
    layout(title = paste0('Economic Losses Due to Environmental Disaster \n',i), 
           xaxis = list(title = 'Countries'),
           yaxis = list(title = 'Million of Euros'))
  
  fig <- fig %>% layout(autosize = F)
  print(fig)
  
  plotly::export(fig, file = paste0('EL_ENV_VAL_A', i, '.png'))
  assign(paste0('EL_ENV_VAL_A_AP', i), EL_ENV_VAL_A_CLUSTER_)
}

# m = list()
# 
# for(f in (2002:2021)){
#   
#   l_EL_ENV_A = eval(parse(text = (paste0('EL_ENV_VAL_A_AP', f))))
#   l_EL_ENV_A = as.list(l_EL_ENV_A@clusters)
#   m[f-2000] = list(GlobalMultiplexityFun(l_EL_ENV_A)) 
# }
# 
# M_EL_ENV_VAL_A <- matrix (0, ncol = 32, nrow = 32)
# for (i in 1:20){
#   M_EL_ENV_VAL_A = M_EL_ENV_VAL_A + m[[i]]
# }
# print(M_EL_ENV_VAL_A)

# EL_ENV_AVG

for(i in (2009:2020)){
  EL_ENV_AVG_CLUSTER <- eval(parse(text = paste0('EL_ENV_AVG_FINAL', i)))
  EL_ENV_AVG_CLUSTER_ <- apcluster(negDistMat(r=2), EL_ENV_AVG_CLUSTER)
  idx = as.factor(EL_ENV_AVG_CLUSTER_@idx)
  a = EL_ENV_AVG_CLUSTER
  a$geo <- as.factor(a$geo)
  a = cbind(a, idx)
  fig <- plot_ly(data = a, x=a$geo, y=a$values, color=a$idx, type='scatter',
                 mode = 'markers', text = ~paste("Country: ", a$geo, '<br>Value:', a$values), size = a$values)%>%
    layout(title = paste0('Economic Losses Due To Environmental Disaster \n',i), 
           xaxis = list(title = 'Countries'),
           yaxis = list(title = 'Average Over 30 Years'))
  
  fig <- fig %>% layout(autosize = F)
  print(fig)
  plotly::export(fig, file = paste0('EL_ENV_AVG', i, '.png'))
  assign(paste0('EL_ENV_AVG_AP', i), EL_ENV_AVG_CLUSTER_)
}

m = list()

for(f in (2009:2020)){
  
  l_EL_ENV_AVG = eval(parse(text = (paste0('EL_ENV_AVG_AP', f))))
  l_EL_ENV_AVG = as.list(l_EL_ENV_AVG@clusters)
  m[f-2000] = list(GlobalMultiplexityFun(l_EL_ENV_AVG)) 
}

M_EL_ENV_AVG <- matrix (0, ncol = 33, nrow = 33)
for (i in 9:20){
  M_EL_ENV_AVG = M_EL_ENV_AVG + m[[i]]
}

rownames(M_EL_ENV_AVG) <- EL_ENV_AVG_FINAL2010$geo
colnames(M_EL_ENV_AVG) <- EL_ENV_AVG_FINAL2010$geo

par(mar=c(5.1, 4.1, 4.1, 4.1))
plot(M_EL_ENV_AVG, col = c( 'red', 'orange', 'yellow', 'darkgreen' ), axis.row = (list(side = 4, las = 2, cex = 0.5)), 
     key = list(side = 3, cex.axis = 0.5), axis.col = list(las = 2, cex = 0.3),
     xlab = 'Countries', ylab = 'Countries', main = 'Economic Losses 30Y Average', text.cell = list( cex = 0.5), digits = 0, breaks = c(0, 3, 6, 9, 12))



# GAS_T_HAB

for(i in (1990:2020)){
  GAS_T_HAB_CLUSTER <- eval(parse(text = paste0('GAS_T_HAB_FINAL', i)))
  GAS_T_HAB_CLUSTER_ <- apcluster(negDistMat(r=2), GAS_T_HAB_CLUSTER)
  idx = as.factor(GAS_T_HAB_CLUSTER_@idx)
  a = GAS_T_HAB_CLUSTER
  a$geo <- as.factor(a$geo)
  a = cbind(a, idx)
  fig <- plot_ly(data = a, x=a$geo, y=a$values, color=a$idx, type='scatter',
                 mode = 'markers', text = ~paste("Country: ", a$geo, '<br>Value:', a$values), size = a$values)%>%
    layout(title = paste0('Gas Emission \n',i), 
           xaxis = list(title = 'Countries'),
           yaxis = list(title = 'Tonnes per Habitant'))
  
  fig <- fig %>% layout(autosize = F)
  print(fig)
  
  plotly::export(fig, file = paste0('GAS_T_HAB', i, '.png'))
  assign(paste0('GAS_T_HAB_AP', i), GAS_T_HAB_CLUSTER_)
}

m = list()

for(f in (2000:2018)){
  
  l_GAS_T_HAB = eval(parse(text = (paste0('GAS_T_HAB_AP', f))))
  l_GAS_T_HAB = as.list(l_GAS_T_HAB@clusters)
  m[f-2000] = list(GlobalMultiplexityFun(l_GAS_T_HAB)) 
}

M_GAS_T_HAB <- matrix (0, ncol = 35, nrow = 35)
for (i in 1:18){
  M_GAS_T_HAB = M_GAS_T_HAB + m[[i]]
}

rownames(M_GAS_T_HAB) <- GAS_T_HAB_FINAL2010$geo
colnames(M_GAS_T_HAB) <- GAS_T_HAB_FINAL2010$geo

par(mar=c(5.1, 4.1, 4.1, 4.1))
plot(M_GAS_T_HAB, col = c( 'red', 'orange', 'yellow', 'darkgreen' ), axis.row = (list(side = 4, las = 2, cex = 0.5)), 
     key = list(side = 3, cex.axis = 0.5), axis.col = list(las = 2, cex = 0.3),
     xlab = 'Countries', ylab = 'Countries', main = 'Gas Emissions Habitants', text.cell = list( cex = 0.5), digits = 0, breaks = c(0, 5, 10, 15, 20))


# GAS_I90

for(i in (1990:2020)){
  GAS_I90_CLUSTER <- eval(parse(text = paste0('GAS_I90_FINAL', i)))
  GAS_I90_CLUSTER_ <- apcluster(negDistMat(r=2), GAS_I90_CLUSTER)
  idx = as.factor(GAS_I90_CLUSTER_@idx)
  a = GAS_I90_CLUSTER
  a$geo <- as.factor(a$geo)
  a = cbind(a, idx)
  fig <- plot_ly(data = a, x=a$geo, y=a$values, color=a$idx, type='scatter',
                 mode = 'markers', text = ~paste("Country: ", a$geo, '<br>Value:', a$values), size = a$values)%>%
    layout(title = paste0('Gas Emissions \n',i), 
           xaxis = list(title = 'Countries'),
           yaxis = list(title = '1990 Index'))
  
  fig <- fig %>% layout(autosize = F)
  print(fig)
  
  plotly::export(fig, file = paste0('GAS_I90', i, '.png'))
  assign(paste0('GAS_I90_AP', i), GAS_I90_CLUSTER_)
}

m = list()

for(f in (2000:2018)){
  
  l_GAS_I90 = eval(parse(text = (paste0('GAS_I90_AP', f))))
  l_GAS_I90 = as.list(l_GAS_I90@clusters)
  m[f-2000] = list(GlobalMultiplexityFun(l_GAS_I90)) 
}

M_GAS_I90 <- matrix (0, ncol = 35, nrow = 35)
for (i in 1:18){
  M_GAS_I90 = M_GAS_I90 + m[[i]]
}

rownames(M_GAS_I90) <- GAS_I90_FINAL2010$geo
colnames(M_GAS_I90) <- GAS_I90_FINAL2010$geo

par(mar=c(5.1, 4.1, 4.1, 4.1))
plot(M_GAS_I90, col = c( 'red', 'orange', 'yellow', 'darkgreen' ), axis.row = (list(side = 4, las = 2, cex = 0.5)), 
     key = list(side = 3, cex.axis = 0.5), axis.col = list(las = 2, cex = 0.3),
     xlab = 'Countries', ylab = 'Countries', main = 'Gas Emission Index 90', text.cell = list( cex = 0.5), digits = 0, breaks = c(0, 5, 10, 15, 20))


# LULUCF_T_HAB

for(i in (2000:2019)){
  LULUCF_T_HAB_CLUSTER <- eval(parse(text = paste0('LULUCF_T_HAB_FINAL', i)))
  LULUCF_T_HAB_CLUSTER_ <- apcluster(negDistMat(r=2), LULUCF_T_HAB_CLUSTER)
  idx = as.factor(LULUCF_T_HAB_CLUSTER_@idx)
  a = LULUCF_T_HAB_CLUSTER
  a$geo <- as.factor(a$geo)
  a = cbind(a, idx)
  fig <- plot_ly(data = a, x=a$geo, y=a$values, color=a$idx, type='scatter',
                 mode = 'markers', text = ~paste("Country: ", a$geo, '<br>Value:', a$values), size = a$values)%>%
    layout(title = paste0('LULUCF \n',i), 
           xaxis = list(title = 'Countries'),
           yaxis = list(title = 'Tonnes per Habitant'))
  
  fig <- fig %>% layout(autosize = F)
  print(fig)
  # IS outlier
  
  plotly::export(fig, file = paste0('LULUCF_T_HAB', i, '.png'))
  assign(paste0('LULUCF_T_HAB_AP', i), LULUCF_T_HAB_CLUSTER_)
}

m = list()

for(f in (2000:2019)){
  
  l_LULUCF_T_HAB = eval(parse(text = (paste0('LULUCF_T_HAB_AP', f))))
  l_LULUCF_T_HAB = as.list(l_LULUCF_T_HAB@clusters)
  m[f-2000] = list(GlobalMultiplexityFun(l_LULUCF_T_HAB)) 
}

M_LULUCF_T_HAB <- matrix (0, ncol = 34, nrow = 34)
for (i in 1:19){
  M_LULUCF_T_HAB = M_LULUCF_T_HAB + m[[i]]
}

rownames(M_LULUCF_T_HAB) <- LULUCF_T_HAB_FINAL2010$geo
colnames(M_LULUCF_T_HAB) <- LULUCF_T_HAB_FINAL2010$geo


par(mar=c(5.1, 4.1, 4.1, 4.1))
plot(M_LULUCF_T_HAB, col = c( 'red', 'orange', 'yellow', 'darkgreen' ), axis.row = (list(side = 4, las = 2, cex = 0.5)), 
     key = list(side = 3, cex.axis = 0.5), axis.col = list(las = 2, cex = 0.3),
     xlab = 'Countries', ylab = 'Countries', main = 'LULUCF HAB', text.cell = list( cex = 0.5), digits = 0, breaks = c(0, 5, 10, 15, 20))



# LULUCF_T_KM2

for(i in (2000:2018)){
  LULUCF_T_KM2_CLUSTER <- eval(parse(text = paste0('LULUCF_T_KM2_FINAL', i)))
  LULUCF_T_KM2_CLUSTER_ <- apcluster(negDistMat(r=2), LULUCF_T_KM2_CLUSTER)
  idx = as.factor(LULUCF_T_KM2_CLUSTER_@idx)
  a = LULUCF_T_KM2_CLUSTER
  a$geo <- as.factor(a$geo)
  a = cbind(a, idx)
  fig <- plot_ly(data = a, x=a$geo, y=a$values, color=a$idx, type='scatter',
                 mode = 'markers', text = ~paste("Country: ", a$geo, '<br>Value:', a$values), size = a$values)%>%
    layout(title = paste0('LULUCF \n',i), 
           xaxis = list(title = 'Countries'),
           yaxis = list(title = 'Tonnes per square km'))
  
  fig <- fig %>% layout(autosize = F)
  print(fig)
  
  plotly::export(fig, file = paste0('LULUCF_T_KM2', i, '.png'))
  assign(paste0('LULUCF_T_KM2_AP', i), LULUCF_T_KM2_CLUSTER_)
}

m = list()

for(f in (2000:2018)){
  
  l_LULUCF_T_KM2 = eval(parse(text = (paste0('LULUCF_T_KM2_AP', f))))
  l_LULUCF_T_KM2 = as.list(l_LULUCF_T_KM2@clusters)
  m[f-2000] = list(GlobalMultiplexityFun(l_LULUCF_T_KM2)) 
}

M_LULUCF_T_KM2 <- matrix (0, ncol = 33, nrow = 33)
for (i in 1:20){
  M_LULUCF_T_KM2 = M_LULUCF_T_KM2 + m[[i]]
}

rownames(M_LULUCF_T_KM2) <- LULUCF_T_KM2_FINAL2010$geo
colnames(M_LULUCF_T_KM2) <- LULUCF_T_KM2_FINAL2010$geo

par(mar=c(5.1, 4.1, 4.1, 4.1))
plot(M_LULUCF_T_KM2, col = c( 'red', 'orange', 'yellow', 'darkgreen' ), axis.row = (list(side = 4, las = 2, cex = 0.5)), 
     key = list(side = 3, cex.axis = 0.5), axis.col = list(las = 2, cex = 0.3),
     xlab = 'Countries', ylab = 'Countries', main = 'LULUCF KM2', text.cell = list( cex = 0.5), digits = 0, breaks = c(0, 5, 10, 15, 20))



# CMCE

for(i in (2010:2021)){
  CMCE_CLUSTER <- eval(parse(text = paste0('CMCE_FINAL', i)))
  CMCE_CLUSTER_ <- apcluster(negDistMat(r=2), CMCE_CLUSTER)
  idx = as.factor(CMCE_CLUSTER_@idx)
  a = CMCE_CLUSTER
  a$geo <- as.factor(a$geo)
  a = cbind(a, idx)
  fig <- plot_ly(data = a, x=a$geo, y=a$values, color=a$idx, type='scatter',
                 mode = 'markers', text = ~paste("Country: ", a$geo, '<br>Value:', a$values), size = a$values)%>%
    layout(title = paste0('Population covered by the Covenant of Mayors for Climate & Energy signatories \n',i), 
           xaxis = list(title = 'Countries'),
           yaxis = list(title = 'Percentage of Population'))
  
  fig <- fig %>% layout(autosize = F)
  print(fig)
  
  plotly::export(fig, file = paste0('CMCE', i, '.png'))
  assign(paste0('CMCE_AP', i), CMCE_CLUSTER_)
}

m = list()

for(f in (2010:2021)){
  
  l_CMCE = eval(parse(text = (paste0('CMCE_AP', f))))
  l_CMCE = as.list(l_CMCE@clusters)
  m[f-2000] = list(GlobalMultiplexityFun(l_CMCE)) 
}

M_CMCE <- matrix (0, ncol = 28, nrow = 28)
for (i in 10:21){
  M_CMCE = M_CMCE + m[[i]]
}

rownames(M_CMCE) <- CMCE_FINAL2019$geo
colnames(M_CMCE) <- CMCE_FINAL2019$geo

par(mar=c(5.1, 4.1, 4.1, 4.1))
plot(M_CMCE, col = c( 'red', 'orange', 'yellow', 'darkgreen' ), axis.row = (list(side = 4, las = 2, cex = 0.5)), 
     key = list(side = 3, cex.axis = 0.5), axis.col = list(las = 2, cex = 0.3),
     xlab = 'Countries', ylab = 'Countries', main = 'CMCE', text.cell = list( cex = 0.5), digits = 0, breaks = c(0, 3, 6, 9, 12))
