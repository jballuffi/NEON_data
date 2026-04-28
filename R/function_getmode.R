#function to calculate mode

#use like you would the mean() function. Only argument is v- an object or string

#this removes any NAs that appear when you collect unique values
#so you are only taking the mode of non-NA values

getmode <- function(v) {
  uniqv <- data.table(unique(v))
  uniqv <- uniqv[!is.na(V1)]
  uniqv <- uniqv$V1
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

