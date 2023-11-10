GlobalMultiplexityFun <- function(l){
  #l = as.list(AFI_CLV10_AP2003@clusters)
counting = matrix(0, ncol = max(unlist(l)), nrow = max(unlist(l)))
for(i in (1:(length(l)))){ # Opening the list of lists
  if (length(l[[i]]) == 1){
    counting[l[[i]][[1]], l[[i]][[1]]] = 1
  }
  else{
     for (k in (1:(length(l[[i]])- 1))){ # Cycling amongs the elements of the first list 
       d = l[[i]][[k]] # Assigning to d the values of the list iteratevely
       #print(d)
        for (m in ((k+1):(length(l[[i]])))){ # Looping on the same list but for all the other values 
          n = l[[i]][[m]] #assigning to n each value of the sublist
          #print(m)
          #print(length(l[[i]]))
          counting[d, n] =+ 1
        }
      }
# Current error on n, it goes out of the limit 
# composed of only two countries 
  }
}
  
#for (j in c(1:length(l[[i]]))){ # Selecting the list
#print(counting)
 return(counting)
}
# if (length(l[[j]]) == 2){
#   counting[l[[j]][[1]], l[[j]][[2]]] = 1
#   print(counting)
# }
# else if (length(l[[j]]) == 1){
#   counting[l[[j]][[1]], l[[j]][[1]]] = 1
# }
# else{