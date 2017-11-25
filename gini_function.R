Gini <- function(a,p){
  df <- data.frame(a,p,i = 1:length(a))
  df <- as.data.frame(df %>% arrange(-p, i))
  ginisum <- sum(cumsum(df$a/sum(df$a)))
  ginisum <- ginisum - (length(a)+1)/2
  gini <- ginisum/length(a)
  return(gini)
}

nGini <- function(a, p){
  Gini(a,p) / Gini(a,a)
}

a <- c(1, 1, 0, 1)
p <- c(0.86, 0.26, 0.52, 0.32)
Gini(a,p)
nGini(a,p)


