for (i in length(weather)){
  for (j in nrow(weather[[,i]])){
    if(is.na(weather[[j, i]])){
      weather[[j, i]] <- summarize(by_month[[j, i]], group_mean = mean(by_month[[j, i]], na.rm=TRUE))
    }
    else{
      weather[[j, i]] <- weather[[j, i]]
    }
  }
}

for (i in length(weather)){
  weather$i[is.na(weather$i)] <- summarize(by_month[i], group_mean = mean(by_month[i], na.rm=TRUE))
}
