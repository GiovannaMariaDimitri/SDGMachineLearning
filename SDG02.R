
library(eurostat)
library(dplyr)
library(arsenal)
library(plotly)
library(apcluster)
library(plot.matrix)



## Getting data from Eurostat and selecting indicators ##


# SDG 02

AFI <- get_eurostat(id = "sdg_02_20", time_format = "num", type = "code") # Agricultural Factor Income (annual work unit)

AFI_CLV10_EUR_AWU <- subset(AFI, AFI$unit == 'CLV10_EUR_AWU')
AFI_CLV10_EUR_AWU$values = (AFI_CLV10_EUR_AWU$values - min(AFI_CLV10_EUR_AWU$values)) / (max(AFI_CLV10_EUR_AWU$values) - min(AFI_CLV10_EUR_AWU$values))
AFI_I10 <- subset(AFI, AFI$unit == 'I10')
AFI_I10$values = (AFI_I10$values - min(AFI_I10$values)) / (max(AFI_I10$values) - min(AFI_I10$values))

GSRD <- get_eurostat(id = "sdg_02_30", time_format = "num", type = "code") # Gov Support to R&D
GSRD <- subset(GSRD, GSRD$unit == 'EUR_HAB')
GSRD$values = (GSRD$values - min(GSRD$values)) / (max(GSRD$values) - min(GSRD$values))

AOF <- get_eurostat(id = "sdg_02_40", time_format = "num", type = "code") # Area under Organic Farming
AOF$values = (AOF$values - min(AOF$values)) / (max(AOF$values) - min(AOF$values))

## Removing useless columns ## 

# AFI

AFI_CLV10_EUR_AWU <- AFI_CLV10_EUR_AWU[, -1]
AFI_I10 <- AFI_I10[, -1] 

# GSRD

GSRD <- GSRD[, -1:-2]

# AOF

AOF <- AOF[, -1:-3]

## Dividing each dataset by year ## 
 
# AFI_CLV10_EUR_AWU 

AFI_CLV10_EUR_AWU$time <- as.numeric(AFI_CLV10_EUR_AWU$time)
AFI_CLV10_EUR_AWU_ <- AFI_CLV10_EUR_AWU
for(i in AFI_CLV10_EUR_AWU$time){
  assign(paste0("AFI_CLV10_EUR_AWU", i), AFI_CLV10_EUR_AWU_)
  AFI_CLV10_EUR_AWU_<- subset(AFI_CLV10_EUR_AWU, AFI_CLV10_EUR_AWU$time == i)
  #row.names(AFI_CLV10_EUR_AWU_) <- AFI_CLV10_EUR_AWU_$geo
  
}


# AFI_I10

AFI_I10$time <- as.numeric(AFI_I10$time)
AFI_I10_ <- AFI_I10
for(i in AFI_I10$time){
  assign(paste0("AFI_I10", i), AFI_I10_)
  AFI_I10_<- subset(AFI_I10, AFI_I10$time == i)
  #row.names(AFI_I10_) <- AFI_I10_$geo
}


# GSRD

GSRD$time <- as.numeric(GSRD$time)
GSRD_ <- GSRD
for(i in GSRD$time){
  assign(paste0("GSRD", i), GSRD_)
  GSRD_<- subset(GSRD, GSRD$time == i)
  #row.names(GSRD_) <- GSRD_$geo
}


# AOF

AOF$time <- as.numeric(AOF$time)
AOF_ <- AOF
for(i in AOF$time){
  assign(paste0("AOF", i), AOF_)
  AOF_<- subset(AOF, AOF$time == i)
  #row.names(AOF_) <- AOF_$geo
}

# Deleting the columns that are not meaningful 

# AFI_CLV10_EUR_AWU


for(i in AFI_CLV10_EUR_AWU$time){
  AFI_T <- eval(parse( text = paste0("AFI_CLV10_EUR_AWU", i)))
  AFI_T <- AFI_T[, -2]
  assign(paste0("AFI_CLV10_EUR_AWU_FINAL", i), AFI_T)
}

# Deleting all the countries that appear only in some years and not in others

for (i in AFI_CLV10_EUR_AWU$time){
  AFI_T <- eval(parse( text = paste0("AFI_CLV10_EUR_AWU", i)))
  AFI_T = AFI_T[(AFI_T$geo %in% AFI_CLV10_EUR_AWU2003$geo), ]
  AFI_T <- AFI_T[, -2]
  assign(paste0("AFI_CLV10_EUR_AWU_FINAL", i), AFI_T)
}



# AFI_I10

for(i in AFI_I10$time){
  AFI_T <- eval(parse( text = paste0("AFI_I10", i)))
  AFI_T <- AFI_T[, -2]
  assign(paste0("AFI_I10_FINAL", i), AFI_T)
}

# Deleting all the countries that appear only in some years and not in others

for (i in AFI_I10$time){
  AFI_T <- eval(parse( text = paste0("AFI_I10", i)))
  AFI_T = AFI_T[(AFI_T$geo %in% AFI_I102005$geo), ]
  AFI_T <- AFI_T[, -2]
  assign(paste0("AFI_I10_FINAL", i), AFI_T)
}

# NOT GOING TO WORK

# GSRD

for(i in GSRD$time){
  GSRD_T <- eval(parse( text = paste0("GSRD", i)))
  GSRD_T <- GSRD_T[, -2]
  assign(paste0("GSRD_FINAL", i), GSRD_T)
}

# Deleting all the countries that appear only in some years and not in others

for (i in GSRD$time){
  GSRD_T <- eval(parse( text = paste0("GSRD", i)))
  GSRD_T = GSRD_T[(GSRD_T$geo %in% GSRD2009$geo), ]
  GSRD_T <- GSRD_T[, -2]
  GSRD_T <- subset(GSRD_T, (GSRD_T$geo != 'EA19' & GSRD_T$geo != 'EA20'))
  assign(paste0("GSRD_FINAL", i), GSRD_T)
}

# Additional problem: some years have more countries but different so the actual comparison doesnt
# produce a constant number fo countries 

# AOF

for(i in AOF$time){
  AOF_T <- eval(parse( text = paste0("AOF", i)))
  AOF_T <- AOF_T[, -2]
  #rownames(AOF_T) <- AOF_T$geo
  assign(paste0("AOF_FINAL", i), AOF_T)
}

# Deleting all the countries that appear only in some years and not in others

for (i in AOF$time){
  AOF_T <- eval(parse( text = paste0("AOF", i)))
  AOF_T = AOF_T[(AOF_T$geo %in% AOF2010$geo), ]
  AOF_T <- AOF_T[, -2]
  assign(paste0("AOF_FINAL", i), AOF_T)
}


### CLUSTERING ###

#AFI_CLV10_EUR_AWU

for(i in (2001:2021)){
  AFI_CLV10_EUR_AWU_CLUSTER <- eval(parse(text = paste0('AFI_CLV10_EUR_AWU_FINAL', i)))
  AFI_AWU_CLUSTER <- apcluster(negDistMat(r=2), AFI_CLV10_EUR_AWU_CLUSTER)
  idx = as.factor(AFI_AWU_CLUSTER@idx)
  a = AFI_CLV10_EUR_AWU_CLUSTER
  a$geo <- as.factor(a$geo)
  a = cbind(a, idx)
  
  fig <- plot_ly(data = a, x=a$geo, y=a$values, color=a$idx, type='scatter',
          mode = 'markers', text = ~paste("Country: ", a$geo, '<br>Value:', a$values), size = 8*a$values)%>%
                layout(title = paste0('Agricultural Factor Income (Annual Work Unit) \n',i), 
                       xaxis = list(title = 'Countries', size = 20),
                       yaxis = list(title = 'Chain Linked Volumes', size = 20))
  fig <- fig %>% layout(autosize = F)
  
 
  print(fig)
  
  plotly::export(fig, file = paste0('AFI_CLV10_AWU', i, '.png'))
  assign(paste0('AFI_CLV10_AP', i), AFI_AWU_CLUSTER)
  
}

m = list()

for(f in (2001:2021)){
  
  l_AFI_CLV10 = eval(parse(text = (paste0('AFI_CLV10_AP', f))))
  l_AFI_CLV10 = as.list(l_AFI_CLV10@clusters)
  m[f-2000] = list(GlobalMultiplexityFun(l_AFI_CLV10)) 
}

M_AFI_CLV <- matrix (0, ncol = 26, nrow = 26)
for (i in 1:20){
  M_AFI_CLV = M_AFI_CLV + m[[i]]
}
rownames(M_AFI_CLV) <- AFI_CLV10_EUR_AWU_FINAL2003$geo
colnames(M_AFI_CLV) <- AFI_CLV10_EUR_AWU_FINAL2003$geo
par(mar=c(5.1, 4.1, 4.1, 4.1))
plot(M_AFI_CLV, col = c( 'red', 'orange', 'yellow', 'darkgreen' ), axis.row = (list(side = 4, las = 2, cex = 0.5)), 
     key = list(side = 3, cex.axis = 0.5), axis.col = list(las = 2, cex = 0.3),
     xlab = 'Countries', ylab = 'Countries', main = 'AFI', text.cell = list( cex = 0.5), digits = 0, breaks = c(0, 5, 10, 15, 20))


# AFI_I10

for(i in (2002:2022)){
  AFI_I10_CLUSTER <- eval(parse(text = paste0('AFI_I10_FINAL', i)))
  AFI_I10_CLUSTER_ <- apcluster(negDistMat(r=2), AFI_I10_CLUSTER)
  idx = as.factor(AFI_I10_CLUSTER_@idx)
  a = AFI_I10_CLUSTER
  a$geo <- as.factor(a$geo)
  a = cbind(a, idx)
  fig <- plot_ly(data = a, x=a$geo, y=a$values, color=a$idx, type='scatter',
                 mode = 'markers', text = ~paste("Country: ", a$geo, '<br>Value:', a$values))%>%
    layout(title = paste0('Agricultural Factor Income (Annual Work Unit) \n',i), 
           xaxis = list(title = 'Countries'),
           yaxis = list(title = '2010 Index'))
  
  fig <- fig %>% layout(autosize = F)

  print(fig)
  
  plotly::export(fig, file = paste0('AFI_I10', i, '.png'))
  assign(paste0('AFI_I10_AP', i), AFI_I10_CLUSTER_)
}

# m = list()
# 
# for(f in (2002:2021)){
#   
#   l_AFI_I10 = eval(parse(text = (paste0('AFI_I10_AP', f))))
#   l_AFI_I10 = as.list(l_AFI_I10@clusters)
#   m[f-2000] = list(GlobalMultiplexityFun(l_AFI_I10)) 
# }
# 
# M_AFI_I10 <- matrix (0, ncol = 32, nrow = 32)
# for (i in 1:20){
#   M_AFI_I10 = M_AFI_I10 + m[[i]]
# }
# print(M_AFI_I10)


# GSRD

for(i in (2004:2021)){
  GSRD_CLUSTER <- eval(parse(text = paste0('GSRD_FINAL', i)))
  GSRD_CLUSTER_ <- apcluster(negDistMat(r=2), GSRD_CLUSTER)
  idx = as.factor(GSRD_CLUSTER_@idx)
  a = GSRD_CLUSTER
  a$geo <- as.factor(a$geo)
  a = cbind(a, idx)
  fig <- plot_ly(data = a, x=a$geo, y=a$values, color=a$idx, type='scatter',
                 mode = 'markers', text = ~paste("Country: ", a$geo, '<br>Value:', a$values), size = a$values)%>%
    layout(title = paste0('Government Support To R&D \n',i), 
           xaxis = list(title = 'Countries'),
           yaxis = list(title = 'Million of Euros'))
  
  fig <- fig %>% layout(autosize = F)
  
  print(fig)
  plotly::export(fig, file = paste0('GSRD', i, '.png'))
  assign(paste0('GSRD_AP', i), GSRD_CLUSTER_)
}

m = list()

for(f in (2010:2019)){

  l_GSRD = eval(parse(text = (paste0('GSRD_AP', f))))
  l_GSRD = as.list(l_GSRD@clusters)
  m[f-2000] = list(GlobalMultiplexityFun(l_GSRD))
}


M_GSRD <- matrix (0, ncol = 31, nrow = 31)
for (i in 10:19){
  M_GSRD = M_GSRD + m[[i]]
}

rownames(M_GSRD) <- GSRD_FINAL2010$geo
colnames(M_GSRD) <- GSRD_FINAL2010$geo

par(mar=c(5.1, 4.1, 4.1, 4.1))
plot(M_GSRD, col = c( 'red','orange', 'yellow', 'darkgreen' ), axis.row = (list(side = 4, las = 2, cex = 0.5)), 
     key = list(side = 3, cex.axis = 0.5), axis.col = list(las = 2, cex = 0.3),
     xlab = 'Countries', ylab = 'Countries', main = 'GSRD', text.cell = list( cex = 0.5), digits = 0, breaks = c(0, 3, 6, 9, 10))






# COULD BE WRONG IN THIS CASE (IT IS)

# AOF

for(i in (2000:2019)){
  AOF_CLUSTER <- eval(parse(text = paste0('AOF_FINAL', i)))
  AOF_CLUSTER_ <- apcluster(negDistMat(r=2), AOF_CLUSTER)
  idx = as.factor(AOF_CLUSTER_@idx)
  a = AOF_CLUSTER
  a$geo <- as.factor(a$geo)
  a = cbind(a, idx)
  fig <- plot_ly(data = a, x=a$geo, y=a$values, color=a$idx, type='scatter',
                 mode = 'markers', text = ~paste("Country: ", a$geo, '<br>Value:', a$values), size = a$values)%>%
    layout(title = paste0('Area Under Organic Farming \n',i), 
           xaxis = list(title = 'Countries'),
           yaxis = list(title = '% of total utilised agricultural area'))
  
  print(fig)
  
  fig <- fig %>% layout(autosize = F)
  
  plotly::export(fig, file = paste0('AOF', i, '.png'))
  assign(paste0('AOF_AP', i), AOF_CLUSTER_)
}

m = list()

for(f in (2005:2019)){
  
  l_AOF = eval(parse(text = (paste0('AOF_AP', f))))
  l_AOF = as.list(l_AOF@clusters)
  m[f-2000] = list(GlobalMultiplexityFun(l_AOF)) 
}

M_AOF <- matrix (0, ncol = 28, nrow = 28)
for (i in 5:19){
  M_AOF = M_AOF + m[[i]]
}

rownames(M_AOF) <- AOF_FINAL2010$geo
colnames(M_AOF) <- AOF_FINAL2010$geo

par(mar=c(5.1, 4.1, 4.1, 4.1))
plot(M_AOF, col = c( 'red', 'orange', 'yellow', 'darkgreen' ), axis.row = (list(side = 4, las = 2, cex = 0.5)), 
     key = list(side = 3, cex.axis = 0.5), axis.col = list(las = 2, cex = 0.3),
     xlab = 'Countries', ylab = 'Countries', main = 'AOF', text.cell = list( cex = 0.5), digits = 0, breaks = c(0, 4, 8, 12, 16))




