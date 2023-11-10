# Indicatore di virtuosità come somma delle performances (1-val) per gli indicatori di negatività

### PERFORMANCES MEASURES ###
library(eurostat)
library(dplyr)
library(arsenal)
library(plotly)
library(apcluster)
library(tidyverse)
library(plot.matrix)


### PERFORMANCES ###

P <- matrix(NA, ncol = 2020, nrow = 28)
#rownames(P) <- c('2014', '2015', '2016', '2017', '2018', '2019', '2020')

for (i in (2014: 2020)){
  
  # Generating the datasets needed in order to compute the performance measure
  
  AFI_P = eval(parse(text = paste0('AFI_CLV10_EUR_AWU', i)))
  GSRD_P = eval(parse(text = paste0('GSRD', i)))
  AOF_P = eval(parse(text = paste0('AOF', i)))
  CMUR_P = eval(parse(text = paste0('CMUR', i)))
  #GW_P = eval(parse(text = paste0('GW_TOT', i)))
  GVA_P = eval(parse(text = paste0('GVA_PC', i)))
  RMC_P = eval(parse(text = paste0('RMC', i)))
  CC_P = eval(parse(text = paste0('CC', i)))
  GAS_P = eval(parse(text = paste0('GAS_T_HAB', i)))
  LULUCF_P = eval(parse(text = paste0('LULUCF_T_HAB', i)))
  CMCE_P = eval(parse(text = paste0('CMCE', i)))
  
  # Eliminating the Countries of which we are not interested
  
  GSRD_P = GSRD_P[(GSRD_P$geo %in% AFI_P$geo), ]
  AOF_P = AOF_P[(AOF_P$geo %in% AFI_P$geo), ]
  CMUR_P = CMUR_P[(CMUR_P$geo %in% AFI_P$geo), ]
  GAS_P = GAS_P[(GAS_P$geo %in% AFI_P$geo), ]
  RMC_P = RMC_P[(RMC_P$geo %in% AFI_P$geo), ]
  CC_P = CC_P[(CC_P$geo %in% AFI_P$geo), ]
  LULUCF_P = LULUCF_P[(LULUCF_P$geo %in% AFI_P$geo), ]
  GSRD_P = GSRD_P[(GSRD_P$geo %in% AFI_P$geo), ]
  CMCE_P = CMCE_P[(CMCE_P$geo %in% AFI_P$geo), ]
  # GVA has 27 countries and the UK, the other 28 without UK!
  
  # Generating the dataframe containing all the indicator values in order to simplify the computation of the sum 
  
  df_list <- list(AFI_P, GSRD_P, AOF_P, CMUR_P, GVA_P, RMC_P, CC_P, GAS_P, LULUCF_P, CMCE_P)
  PERF <- df_list %>% reduce(inner_join, by = 'geo')
  PERF <- PERF[ , -c(1, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20)]
  colnames(PERF) <- c('AFI', 'GSRD', 'AOF', 'CMUR', 'GVA', 'RMC', 'CC', 'GAS', 'LULUCF', 'CMCE')
  assign(paste0('PERFORMANCES', i), PERF)
  
  # Computing and assigning the performances metric to each state for each year considered (many indicators are missing for 
  # years before 2014 so the analysis will be restricted to the range 2014 - 2020)
  
  for (j in 1:27){
    P[j, i] = PERF$AFI[j] + PERF$GSRD[j] + PERF$AOF[j] + PERF$CMUR[j] +
      PERF$GVA[j] + (1 - PERF$RMC[j]) + PERF$CC[j] + (1 - PERF$GAS[j]) +
      PERF$LULUCF[j] + PERF$CMCE[j]
  }
}

P <- P[, 2014:2020]
rownames(P) <- AFI_P$geo
colnames(P) <- c('2014', '2015', '2016', '2017', '2018', '2019', '2020')
P <- P[1:25, ]


#print(fig)
# par(mar = c(6, 6, 6, 6))
plot(P, na.col = 'white', border = NA, axis.row = (list(side = 4, las = 2)), key = list(side = 3, cex.axis = 0.5),
     col = heat.colors(10),
     xlab = 'Years', ylab = 'Countries', digits = 2, text.cell = list(cex = 0.5))


# clustering on the dataframe generate to obtain a clustering on the summary indicator
P <- as.data.frame(P)
for (i in (1:7)){
  SUMMARY_CLUSTER <- apcluster(negDistMat(r=2), P)
  assign(paste0('SUMMARY_CLUSTER', as.character(i +2013)), SUMMARY_CLUSTER)
}

### PLOTTING ###

fig <- plot_ly(data = P, x = rownames(P), y = P$'2014', color = as.factor(SUMMARY_CLUSTER2014@idx), type ='scatter',
               mode = 'markers', size =P$'2014', text = ~paste("Country: ", rownames(P), '<br>Value:', P$'2014'))%>%
  layout(title = 'Summary Indicator Performances 2014 \n', 
                xaxis = list(title = 'Countries'),
                yaxis = list(title = 'Summary Value'))

fig <- fig %>% layout(autosize = F)
print(fig)
plotly::export(fig, file = paste0('Summary2014.png'))


fig1 <- plot_ly(data = P, x = rownames(P), y = P$'2015', color = as.factor(SUMMARY_CLUSTER2015@idx), type ='scatter',
               mode = 'markers', size =P$'2015', text = ~paste("Country: ", rownames(P), '<br>Value:', P$'2015'))%>%
  layout(title = 'Summary Indicator Performances 2015 \n', 
         xaxis = list(title = 'Countries'),
         yaxis = list(title = 'Summary Value'))
print(fig1)
plotly::export(fig1, file = paste0('Summary2015.jpeg'))


fig2 <- plot_ly(data = P, x = rownames(P), y = P$'2016', color = as.factor(SUMMARY_CLUSTER2016@idx), type ='scatter',
               mode = 'markers', size =P$'2016', text = ~paste("Country: ", rownames(P), '<br>Value:', P$'2016'))%>%
  layout(title = 'Summary Indicator Performances 2016 \n', 
         xaxis = list(title = 'Countries'),
         yaxis = list(title = 'Summary Value'))
print(fig2)
plotly::export(fig2, file = paste0('Summary2016.jpeg'))

fig3 <- plot_ly(data = P, x = rownames(P), y = P$'2017', color = as.factor(SUMMARY_CLUSTER2017@idx), type ='scatter',
               mode = 'markers', size =P$'2017', text = ~paste("Country: ", rownames(P), '<br>Value:', P$'2017'))%>%
  layout(title = 'Summary Indicator Performances 2017 \n', 
         xaxis = list(title = 'Countries'),
         yaxis = list(title = 'Summary Value'))
print(fig3)
plotly::export(fig3, file = paste0('Summary2017.jpeg'))


fig4 <- plot_ly(data = P, x = rownames(P), y = P$'2018', color = as.factor(SUMMARY_CLUSTER2018@idx), type ='scatter',
               mode = 'markers', size =P$'2018', text = ~paste("Country: ", rownames(P), '<br>Value:', P$'2018'))%>%
  layout(title = 'Summary Indicator Performances 2018 \n', 
         xaxis = list(title = 'Countries'),
         yaxis = list(title = 'Summary Value'))
print(fig4)
plotly::export(fig4, file = paste0('Summary2018.jpeg'))

fig5 <- plot_ly(data = P, x = rownames(P), y = P$'2019', color = as.factor(SUMMARY_CLUSTER2019@idx), type ='scatter',
               mode = 'markers', size =P$'2019', text = ~paste("Country: ", rownames(P), '<br>Value:', P$'2019'))%>%
  layout(title = 'Summary Indicator Performances 2019 \n', 
         xaxis = list(title = 'Countries'),
         yaxis = list(title = 'Summary Value'))
print(fig5)
plotly::export(fig5, file = paste0('Summary2019.jpeg'))


fig6 <- plot_ly(data = P, x = rownames(P), y = P$'2020', color = as.factor(SUMMARY_CLUSTER2020@idx), type ='scatter',
               mode = 'markers', size =P$'2020', text = ~paste("Country: ", rownames(P), '<br>Value:', P$'2020'))%>%
  layout(title = 'Summary Indicator Performances 2020 \n', 
         xaxis = list(title = 'Countries'),
         yaxis = list(title = 'Summary Value'))
fig <- fig %>% layout(autosize = F)
print(fig6)
plotly::export(fig6, file = paste0('Summary2020.png'))
