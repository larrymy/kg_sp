clean_method_1 <- function(df){
  #TEST 1a: REMOVE COLUMNS WITH >5% missing data
  df[df == -1] <- NA
  na_stat <- sapply(df, function(x) sum(is.na(x)))/nrow(df)
  na_stat <- data.table(var = names(na_stat), val = na_stat) %>% setorder(-val)
  r <- as.vector(as.data.frame(na_stat %>% filter(val > 0.05) %>% select(var))[,1])
  u1 <- colnames(df) %in% r; df2 <- as.data.frame(df)[,!u1]
  
  #TEST 1b: IMPUTE WITH MEAN AND MODE
  #train
  cname <- colnames(df2)
  all <- grep("^ps", colnames(df2), value = T)
  binary <- grep("^ps.*bin$", colnames(df2), value = T)
  categ <- grep("^ps.*cat$", colnames(df2), value = T)
  contn <- all[!all %in% c(binary, categ)]
  
  others <- cname[!cname %in% all]
  
  getmode <- function(x) {
    uniqv <- unique(x)
    uniqv[which.max(tabulate(match(x, uniqv)))]
  }
  
  impute <- function(df, fun2){
    q <- apply(df, 2, FUN = function(j){
      j[is.na(j)] <- fun2(j[!is.na(j)])
      return(j)
    })
    data.frame(q)
  }
  
  contn_df <- impute(df2[,contn], mean)
  categ_df <- impute(df2[,categ], getmode); 
  for(j in 1:ncol(categ_df)){
    categ_df[,j] <- as.factor( categ_df[,j])
  }
  
  #One hot encoding
  t2 <- dummyVars('~ .', data = categ_df, fullrank = TRUE)
  t2 <- (predict(t2, newdata = categ_df))
  
  binary_df <- impute(df2[,binary], getmode)
  
  other_df <- data.frame(df2[,others])
  
  return( data.frame(other_df, contn_df, t2, binary_df ))
  
}