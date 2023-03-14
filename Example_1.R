# Compare GAFS with normal wrapper
future::plan(future::multisession(workers = 10))

## Simulated Data: p50,n50, no int
### GAFS
res_gafs = pbapply::pblapply(1:1, function(seeder){
  varnum=50
  covariate= c(1)
  main_var = 10
  df = AIFS::dataset(varnum = varnum, setting = "Correlation", var = "onlyMar", seed=seeder, main_var=main_var, var_effect=c(0.5, -0.5), correlation_var=15, correlation_val=5, high_dim=T, train_sample=50)
  oridf = df

  para = list(interactions=F, int_term=2, ncomp=5, intercept=T, out_type="continuous", outcome_var="y")
  output = AIFS::trainfs(ml_train = T, Best_Para_train= rep(1,6) ,covariate=covariate, sp=para, dr_tech = c("No_DR"), ml_tech = c("rf"), optim_tech = "GA", df_p=df, std_techlist = "lasso", list_size = 15, maxiter = 100, run = 20, pop = 200)
#AIFS::
  output$selfeatperf = list()
  for(i in c("F1", "AUC", "Accuracy")){
    output$selfeatperf[[i]] = AIFS:::sfperf(score = i, selfeat = output$feature[-1], tarnumb = 10, varnum = varnum, int = F)
  }
  return(output)
})
res_gafs_ridge = pbapply::pblapply(1:1, function(seeder){
  varnum=50
  covariate= c(1)
  main_var = 10
  df = AIFS::dataset(varnum = varnum, setting = "Correlation", var = "onlyMar", seed=seeder, main_var=main_var, var_effect=c(0.5, -0.5), correlation_var=15, correlation_val=5, high_dim=T, train_sample=50)
  oridf = df

  para = list(interactions=F, int_term=2, ncomp=5, intercept=T, out_type="continuous", outcome_var="y")
  output = AIFS::trainfs(ml_train = T, Best_Para_train= rep(1,6) ,covariate=covariate, sp=para, dr_tech = c("No_DR"), ml_tech = c("ridge"), optim_tech = "GA", df_p=df, std_techlist = "lasso", list_size = 15, maxiter = 100, run = 20, pop = 200)
  #AIFS::
  output$selfeatperf = list()
  for(i in c("F1", "AUC", "Accuracy")){
    output$selfeatperf[[i]] = AIFS:::sfperf(score = i, selfeat = output$feature[-1], tarnumb = 10, varnum = varnum, int = F)
  }
  return(output)
})
### Wrapper
res_std = pbapply::pblapply(1:1, function(seeder){
  varnum=50
  covariate= c(1)
  main_var = 10

  df = AIFS::dataset(varnum = varnum, setting = "Correlation", var = "onlyMar", seed=seeder, main_var=main_var, var_effect=c(0.5, -0.5), correlation_var=15, correlation_val=5, high_dim=T, train_sample=50)
  oridf = df

  para = list(interactions=F, int_term=2, ncomp=5, intercept=T, out_type="continuous", outcome_var="y")
  output = AIFS::stdwrap(df_p = df, std_techlist = "lasso", sp=para, list_size = 15, maxiter = 100, run = 20, pop = 200)

  output$selfeatperf = list()
  for(i in c("F1", "AUC", "Accuracy")){
    output$selfeatperf[[i]] = AIFS:::sfperf(score = i, selfeat = output$feature[-1], tarnumb = 10, varnum = varnum, int = F)
  }

  return(output)
})
### Hybrid
res_hyb = pbapply::pblapply(1:1, function(seeder){
  varnum=50
  covariate= c(1)
  main_var = 10

  df = AIFS::dataset(varnum = varnum, setting = "Correlation", var = "onlyMar", seed=seeder, main_var=main_var, var_effect=c(0.5, -0.5), correlation_var=15, correlation_val=5, high_dim=T, train_sample=50)
  oridf = df

  para = list(interactions=F, int_term=2, ncomp=5, intercept=T, out_type="continuous", outcome_var="y")
  output = FSJMI(traindf = df$train, std_techlist = 'lasso', output = "y", testdf = df$test, covariate = c(1), k =10)

  # output = AIFS::stdwrap(df_p = df, std_techlist = "lasso", sp=para, list_size = 15, maxiter = 100, run = 20, pop = 200)

  # output$selfeatperf = list()
  # for(i in c("F1", "AUC", "Accuracy")){
  #   output$selfeatperf[[i]] = AIFS:::sfperf(score = i, selfeat = output$feature[-1], tarnumb = 10, varnum = varnum, int = F)
  # }

  return(output)
})
res_hyb_cont = pbapply::pblapply(1:1, function(seeder){
  varnum=50
  covariate= c(1)
  main_var = 10

  df = AIFS::dataset(varnum = varnum, setting = "Correlation", var = "onlyMar", seed=seeder, main_var=main_var, var_effect=c(0.5, -0.5), correlation_var=15, correlation_val=5, high_dim=T, train_sample=50)
  oridf = df

  para = list(interactions=F, int_term=2, ncomp=5, intercept=T, out_type="continuous", outcome_var="y")
  output = FSJMI_cont(traindf = df$train, std_techlist = 'lasso', output = "y", testdf = df$test, covariate = c(1), k =10)

  # output = AIFS::stdwrap(df_p = df, std_techlist = "lasso", sp=para, list_size = 15, maxiter = 100, run = 20, pop = 200)

  # output$selfeatperf = list()
  # for(i in c("F1", "AUC", "Accuracy")){
  #   output$selfeatperf[[i]] = AIFS:::sfperf(score = i, selfeat = output$feature[-1], tarnumb = 10, varnum = varnum, int = F)
  # }

  return(output)
})
#### Summary of Results
df5050 = data.frame(tech = c("aifs", "std", "hybrid", "hyb_cont", "aifs_ridge"))
df5050["RMSE"] = c(res_gafs[[1]]$performance$RMSE[res_gafs[[1]]$performance$datatype == "test"]
                   ,res_std[[1]]$performance$RMSE[res_std[[1]]$performance$datatype == "test"]
                   ,res_hyb[[1]]$performance$RMSE[res_hyb[[1]]$performance$datatype == "test"]
                   ,res_hyb_cont[[1]]$performance$RMSE[res_hyb_cont[[1]]$performance$datatype == "test"]
                   , res_gafs_ridge[[1]]$performance$RMSE[res_gafs_ridge[[1]]$performance$datatype == "test"])
df5050["F1"] = c(res_gafs[[1]]$selfeatperf$F1, res_std[[1]]$selfeatperf$F1, res_hyb[[1]]$selfeatperf$F1, res_hyb_cont[[1]]$selfeatperf$F1, res_gafs_ridge[[1]]$selfeatperf$F1)

## Simulated Data: p50,n100, no int
### GAFS
res_gafs = pbapply::pblapply(1:1, function(seeder){
  varnum=50
  covariate= c(1)
  main_var = 10
  df = AIFS::dataset(varnum = varnum, setting = "Correlation", var = "onlyMar", seed=seeder, main_var=main_var, var_effect=c(0.5, -0.5), correlation_var=15, correlation_val=5, high_dim=T, train_sample=100)
  oridf = df

  para = list(interactions=F, int_term=2, ncomp=5, intercept=T, out_type="continuous", outcome_var="y")
  output = AIFS::trainfs(ml_train = T, Best_Para_train= rep(1,6) ,covariate=covariate, sp=para, dr_tech = c("No_DR"), ml_tech = c("rf"), optim_tech = "GA", df_p=df, std_techlist = "lasso", list_size = 15, maxiter = 100, run = 20, pop = 200)
  output$selfeatperf = list()
  for(i in c("F1", "AUC", "Accuracy")){
    output$selfeatperf[[i]] = AIFS:::sfperf(score = i, selfeat = output$feature[-1], tarnumb = 10, varnum = varnum, int = F)
  }
  return(output)
})
res_gafs_ridge = pbapply::pblapply(1:1, function(seeder){
  varnum=50
  covariate= c(1)
  main_var = 10
  df = AIFS::dataset(varnum = varnum, setting = "Correlation", var = "onlyMar", seed=seeder, main_var=main_var, var_effect=c(0.5, -0.5), correlation_var=15, correlation_val=5, high_dim=T, train_sample=100)
  oridf = df

  para = list(interactions=F, int_term=2, ncomp=5, intercept=T, out_type="continuous", outcome_var="y")
  output = AIFS::trainfs(ml_train = T, Best_Para_train= rep(1,6) ,covariate=covariate, sp=para, dr_tech = c("No_DR"), ml_tech = c("ridge"), optim_tech = "GA", df_p=df, std_techlist = "lasso", list_size = 15, maxiter = 100, run = 20, pop = 200)
  #AIFS::
  output$selfeatperf = list()
  for(i in c("F1", "AUC", "Accuracy")){
    output$selfeatperf[[i]] = AIFS:::sfperf(score = i, selfeat = output$feature[-1], tarnumb = 10, varnum = varnum, int = F)
  }
  return(output)
})
### Wrapper
res_std = pbapply::pblapply(1:1, function(seeder){
  varnum=50
  covariate= c(1)
  main_var = 10
  df = AIFS::dataset(varnum = varnum, setting = "Correlation", var = "onlyMar", seed=seeder, main_var=main_var, var_effect=c(0.5, -0.5), correlation_var=15, correlation_val=5, high_dim=T, train_sample=100)
  oridf = df

  para = list(interactions=F, int_term=2, ncomp=5, intercept=T, out_type="continuous", outcome_var="y")
  output = AIFS::stdfs(ml_train = T, Best_Para_train= rep(1,6) ,covariate=covariate, sp=para, dr_tech = c("No_DR"), ml_tech = c("rf"), optim_tech = "GA", df_p=df, std_techlist = "lasso", list_size = 15, maxiter = 100, run = 20, pop = 200)

  output$selfeatperf = list()
  for(i in c("F1", "AUC", "Accuracy")){
    output$selfeatperf[[i]] = AIFS:::sfperf(score = i, selfeat = output$feature[-1], tarnumb = 10, varnum = varnum, int = F)
  }
  return(output)
})
### Hybrid
res_hyb = pbapply::pblapply(1:1, function(seeder){
  varnum=50
  covariate= c(1)
  main_var = 10

  df = AIFS::dataset(varnum = varnum, setting = "Correlation", var = "onlyMar", seed=seeder, main_var=main_var, var_effect=c(0.5, -0.5), correlation_var=15, correlation_val=5, high_dim=T, train_sample=100)
  oridf = df

  para = list(interactions=F, int_term=2, ncomp=5, intercept=T, out_type="continuous", outcome_var="y")
  output = FSJMI(traindf = df$train, std_techlist = 'lasso', output = "y", testdf = df$test, covariate = c(1), k =10)

  # output = AIFS::stdwrap(df_p = df, std_techlist = "lasso", sp=para, list_size = 15, maxiter = 100, run = 20, pop = 200)

  # output$selfeatperf = list()
  # for(i in c("F1", "AUC", "Accuracy")){
  #   output$selfeatperf[[i]] = AIFS:::sfperf(score = i, selfeat = output$feature[-1], tarnumb = 10, varnum = varnum, int = F)
  # }

  return(output)
})
res_hyb_cont = pbapply::pblapply(1:1, function(seeder){
  varnum=50
  covariate= c(1)
  main_var = 10

  df = AIFS::dataset(varnum = varnum, setting = "Correlation", var = "onlyMar", seed=seeder, main_var=main_var, var_effect=c(0.5, -0.5), correlation_var=15, correlation_val=5, high_dim=T, train_sample=100)
  oridf = df

  para = list(interactions=F, int_term=2, ncomp=5, intercept=T, out_type="continuous", outcome_var="y")
  output = FSJMI_cont(traindf = df$train, std_techlist = 'lasso', output = "y", testdf = df$test, covariate = c(1), k =10)

  # output = AIFS::stdwrap(df_p = df, std_techlist = "lasso", sp=para, list_size = 15, maxiter = 100, run = 20, pop = 200)

  # output$selfeatperf = list()
  # for(i in c("F1", "AUC", "Accuracy")){
  #   output$selfeatperf[[i]] = AIFS:::sfperf(score = i, selfeat = output$feature[-1], tarnumb = 10, varnum = varnum, int = F)
  # }

  return(output)
})
#### Summary of Results
df50100 = data.frame(tech = c("aifs", "std", "hybrid", "hyb_cont", "aifs_ridge"))
df50100["RMSE"] = c(res_gafs[[1]]$performance$RMSE[res_gafs[[1]]$performance$datatype == "test"]
                    ,res_std[[1]]$performance$RMSE[res_std[[1]]$performance$datatype == "test"]
                    ,res_hyb[[1]]$performance$RMSE[res_hyb[[1]]$performance$datatype == "test"]
                    ,res_hyb_cont[[1]]$performance$RMSE[res_hyb_cont[[1]]$performance$datatype == "test"]
                    , res_gafs_ridge[[1]]$performance$RMSE[res_gafs_ridge[[1]]$performance$datatype == "test"])
df50100["F1"] = c(res_gafs[[1]]$selfeatperf$F1, res_std[[1]]$selfeatperf$F1, res_hyb[[1]]$selfeatperf$F1, res_hyb_cont[[1]]$selfeatperf$F1, res_gafs_ridge[[1]]$selfeatperf$F1)



## Simulated Data: p100,n75, no int
### GAFS
res_gafs = pbapply::pblapply(1:1, function(seeder){
  varnum=100
  covariate= c(1)
  main_var = 10
  df = AIFS::dataset(varnum = varnum, setting = "Correlation", var = "onlyMar", seed=seeder, main_var=main_var, var_effect=c(0.5, -0.5), correlation_var=15, correlation_val=5, high_dim=T, train_sample=75)
  oridf = df

  para = list(interactions=F, int_term=2, ncomp=5, intercept=T, out_type="continuous", outcome_var="y")
  output = AIFS::trainfs(ml_train = T, Best_Para_train= rep(1,6) ,covariate=covariate, sp=para, dr_tech = c("No_DR"), ml_tech = c("rf"), optim_tech = "GA", df_p=df, std_techlist = "lasso", list_size = 15, maxiter = 100, run = 20, pop = 200)

  output$selfeatperf = list()
  for(i in c("F1", "AUC", "Accuracy")){
    output$selfeatperf[[i]] = AIFS:::sfperf(score = i, selfeat = output$feature[-1], tarnumb = 10, varnum = varnum, int = F)
  }
  return(output)
})
res_gafs_ridge = pbapply::pblapply(1:1, function(seeder){
  varnum=100
  covariate= c(1)
  main_var = 10
  df = AIFS::dataset(varnum = varnum, setting = "Correlation", var = "onlyMar", seed=seeder, main_var=main_var, var_effect=c(0.5, -0.5), correlation_var=15, correlation_val=5, high_dim=T, train_sample=75)
  oridf = df

  para = list(interactions=F, int_term=2, ncomp=5, intercept=T, out_type="continuous", outcome_var="y")
  output = AIFS::trainfs(ml_train = T, Best_Para_train= rep(1,6) ,covariate=covariate, sp=para, dr_tech = c("No_DR"), ml_tech = c("ridge"), optim_tech = "GA", df_p=df, std_techlist = "lasso", list_size = 15, maxiter = 100, run = 20, pop = 200)
  #AIFS::
  output$selfeatperf = list()
  for(i in c("F1", "AUC", "Accuracy")){
    output$selfeatperf[[i]] = AIFS:::sfperf(score = i, selfeat = output$feature[-1], tarnumb = 10, varnum = varnum, int = F)
  }
  return(output)
})
# res_gafs
### Wrapper
res_std = pbapply::pblapply(1:1, function(seeder){
  varnum=100
  covariate= c(1)
  main_var = 10
  df = AIFS::dataset(varnum = varnum, setting = "Correlation", var = "onlyMar", seed=seeder, main_var=main_var, var_effect=c(0.5, -0.5), correlation_var=15, correlation_val=5, high_dim=T, train_sample=75)
  oridf = df

  para = list(interactions=F, int_term=2, ncomp=5, intercept=T, out_type="continuous", outcome_var="y")
  output = AIFS::stdfs(ml_train = T, Best_Para_train= rep(1,6) ,covariate=covariate, sp=para, dr_tech = c("No_DR"), ml_tech = c("rf"), optim_tech = "GA", df_p=df, std_techlist = "lasso", list_size = 15, maxiter = 100, run = 20, pop = 200)

  output$selfeatperf = list()
  for(i in c("F1", "AUC", "Accuracy")){
    output$selfeatperf[[i]] = AIFS:::sfperf(score = i, selfeat = output$feature[-1], tarnumb = 10, varnum = varnum, int = F)
  }
  return(output)
})
# res_std
### Hybrid
res_hyb = pbapply::pblapply(1:1, function(seeder){
  varnum=100
  covariate= c(1)
  main_var = 10

  df = AIFS::dataset(varnum = varnum, setting = "Correlation", var = "onlyMar", seed=seeder, main_var=main_var, var_effect=c(0.5, -0.5), correlation_var=15, correlation_val=5, high_dim=T, train_sample=75)
  oridf = df

  para = list(interactions=F, int_term=2, ncomp=5, intercept=T, out_type="continuous", outcome_var="y")
  output = FSJMI(traindf = df$train, std_techlist = 'lasso', output = "y", testdf = df$test, covariate = c(1), k =10)

  # output = AIFS::stdwrap(df_p = df, std_techlist = "lasso", sp=para, list_size = 15, maxiter = 100, run = 20, pop = 200)

  # output$selfeatperf = list()
  # for(i in c("F1", "AUC", "Accuracy")){
  #   output$selfeatperf[[i]] = AIFS:::sfperf(score = i, selfeat = output$feature[-1], tarnumb = 10, varnum = varnum, int = F)
  # }

  return(output)
})
res_hyb_cont = pbapply::pblapply(1:1, function(seeder){
  varnum=100
  covariate= c(1)
  main_var = 10

  df = AIFS::dataset(varnum = varnum, setting = "Correlation", var = "onlyMar", seed=seeder, main_var=main_var, var_effect=c(0.5, -0.5), correlation_var=15, correlation_val=5, high_dim=T, train_sample=75)
  oridf = df

  para = list(interactions=F, int_term=2, ncomp=5, intercept=T, out_type="continuous", outcome_var="y")
  output = FSJMI_cont(traindf = df$train, std_techlist = 'lasso', output = "y", testdf = df$test, covariate = c(1), k =10)

  # output = AIFS::stdwrap(df_p = df, std_techlist = "lasso", sp=para, list_size = 15, maxiter = 100, run = 20, pop = 200)

  # output$selfeatperf = list()
  # for(i in c("F1", "AUC", "Accuracy")){
  #   output$selfeatperf[[i]] = AIFS:::sfperf(score = i, selfeat = output$feature[-1], tarnumb = 10, varnum = varnum, int = F)
  # }

  return(output)
})
#### Summary of Results
df10075 = data.frame(tech = c("aifs", "std", "hybrid", "hyb_cont", "aifs_ridge"))
df10075["RMSE"] = c(res_gafs[[1]]$performance$RMSE[res_gafs[[1]]$performance$datatype == "test"]
                    ,res_std[[1]]$performance$RMSE[res_std[[1]]$performance$datatype == "test"]
                    ,res_hyb[[1]]$performance$RMSE[res_hyb[[1]]$performance$datatype == "test"]
                    ,res_hyb_cont[[1]]$performance$RMSE[res_hyb_cont[[1]]$performance$datatype == "test"]
                    , res_gafs_ridge[[1]]$performance$RMSE[res_gafs_ridge[[1]]$performance$datatype == "test"])
df10075["F1"] = c(res_gafs[[1]]$selfeatperf$F1, res_std[[1]]$selfeatperf$F1, res_hyb[[1]]$selfeatperf$F1, res_hyb_cont[[1]]$selfeatperf$F1, res_gafs_ridge[[1]]$selfeatperf$F1)



## Simulated Data: p100,n100, no int
### GAFS
res_gafs = pbapply::pblapply(1:1, function(seeder){
  varnum=100
  covariate= c(1)
  main_var = 10
  df = AIFS::dataset(varnum = varnum, setting = "Correlation", var = "onlyMar", seed=seeder, main_var=main_var, var_effect=c(0.5, -0.5), correlation_var=15, correlation_val=5, high_dim=T, train_sample=100)
  oridf = df

  para = list(interactions=F, int_term=2, ncomp=5, intercept=T, out_type="continuous", outcome_var="y")
  output = AIFS::trainfs(ml_train = T, Best_Para_train= rep(1,6) ,covariate=covariate, sp=para, dr_tech = c("No_DR"), ml_tech = c("rf"), optim_tech = "GA", df_p=df, std_techlist = "lasso", list_size = 15, maxiter = 100, run = 20, pop = 200)

  output$selfeatperf = list()
  for(i in c("F1", "AUC", "Accuracy")){
    output$selfeatperf[[i]] = AIFS:::sfperf(score = i, selfeat = output$feature[-1], tarnumb = 10, varnum = varnum, int = F)
  }
  return(output)
})
res_gafs_ridge = pbapply::pblapply(1:1, function(seeder){
  varnum=100
  covariate= c(1)
  main_var = 10
  df = AIFS::dataset(varnum = varnum, setting = "Correlation", var = "onlyMar", seed=seeder, main_var=main_var, var_effect=c(0.5, -0.5), correlation_var=15, correlation_val=5, high_dim=T, train_sample=100)
  oridf = df

  para = list(interactions=F, int_term=2, ncomp=5, intercept=T, out_type="continuous", outcome_var="y")
  output = AIFS::trainfs(ml_train = T, Best_Para_train= rep(1,6) ,covariate=covariate, sp=para, dr_tech = c("No_DR"), ml_tech = c("ridge"), optim_tech = "GA", df_p=df, std_techlist = "lasso", list_size = 15, maxiter = 100, run = 20, pop = 200)

  output$selfeatperf = list()
  for(i in c("F1", "AUC", "Accuracy")){
    output$selfeatperf[[i]] = AIFS:::sfperf(score = i, selfeat = output$feature[-1], tarnumb = 10, varnum = varnum, int = F)
  }
  return(output)
})
# res_gafs
### Wrapper
res_std = pbapply::pblapply(1:1, function(seeder){
  varnum=100
  covariate= c(1)
  main_var = 10
  df = AIFS::dataset(varnum = varnum, setting = "Correlation", var = "onlyMar", seed=seeder, main_var=main_var, var_effect=c(0.5, -0.5), correlation_var=15, correlation_val=5, high_dim=T, train_sample=100)
  oridf = df

  para = list(interactions=F, int_term=2, ncomp=5, intercept=T, out_type="continuous", outcome_var="y")
  output = AIFS::stdfs(ml_train = T, Best_Para_train= rep(1,6) ,covariate=covariate, sp=para, dr_tech = c("No_DR"), ml_tech = c("rf"), optim_tech = "GA", df_p=df, std_techlist = "lasso", list_size = 15, maxiter = 100, run = 20, pop = 200)

  output$selfeatperf = list()
  for(i in c("F1", "AUC", "Accuracy")){
    output$selfeatperf[[i]] = AIFS:::sfperf(score = i, selfeat = output$feature[-1], tarnumb = 10, varnum = varnum, int = F)
  }
  return(output)
})
# res_std
### Hybrid
res_hyb = pbapply::pblapply(1:1, function(seeder){
  varnum=100
  covariate= c(1)
  main_var = 10

  df = AIFS::dataset(varnum = varnum, setting = "Correlation", var = "onlyMar", seed=seeder, main_var=main_var, var_effect=c(0.5, -0.5), correlation_var=15, correlation_val=5, high_dim=T, train_sample=100)
  oridf = df

  para = list(interactions=F, int_term=2, ncomp=5, intercept=T, out_type="continuous", outcome_var="y")
  output = FSJMI(traindf = df$train, std_techlist = 'lasso', output = "y", testdf = df$test, covariate = c(1), k =10)

  # output = AIFS::stdwrap(df_p = df, std_techlist = "lasso", sp=para, list_size = 15, maxiter = 100, run = 20, pop = 200)

  # output$selfeatperf = list()
  # for(i in c("F1", "AUC", "Accuracy")){
  #   output$selfeatperf[[i]] = AIFS:::sfperf(score = i, selfeat = output$feature[-1], tarnumb = 10, varnum = varnum, int = F)
  # }

  return(output)
})
res_hyb_cont = pbapply::pblapply(1:1, function(seeder){
  varnum=100
  covariate= c(1)
  main_var = 10

  df = AIFS::dataset(varnum = varnum, setting = "Correlation", var = "onlyMar", seed=seeder, main_var=main_var, var_effect=c(0.5, -0.5), correlation_var=15, correlation_val=5, high_dim=T, train_sample=100)
  oridf = df

  para = list(interactions=F, int_term=2, ncomp=5, intercept=T, out_type="continuous", outcome_var="y")
  output = FSJMI_cont(traindf = df$train, std_techlist = 'lasso', output = "y", testdf = df$test, covariate = c(1), k =10)

  # output = AIFS::stdwrap(df_p = df, std_techlist = "lasso", sp=para, list_size = 15, maxiter = 100, run = 20, pop = 200)

  # output$selfeatperf = list()
  # for(i in c("F1", "AUC", "Accuracy")){
  #   output$selfeatperf[[i]] = AIFS:::sfperf(score = i, selfeat = output$feature[-1], tarnumb = 10, varnum = varnum, int = F)
  # }

  return(output)
})
#### Summary of Results
df100100 = data.frame(tech = c("aifs", "std", "hybrid", "hyb_cont"))
df100100["RMSE"] = c(res_gafs[[1]]$performance$RMSE[res_gafs[[1]]$performance$datatype == "test"]
                     ,res_std[[1]]$performance$RMSE[res_std[[1]]$performance$datatype == "test"]
                     ,res_hyb[[1]]$performance$RMSE[res_hyb[[1]]$performance$datatype == "test"]
                     ,res_hyb_cont[[1]]$performance$RMSE[res_hyb_cont[[1]]$performance$datatype == "test"]
                     , res_gafs_ridge[[1]]$performance$RMSE[res_gafs_ridge[[1]]$performance$datatype == "test"])
df100100["F1"] = c(res_gafs[[1]]$selfeatperf$F1, res_std[[1]]$selfeatperf$F1, res_hyb[[1]]$selfeatperf$F1, res_hyb_cont[[1]]$selfeatperf$F1, res_gafs_ridge[[1]]$selfeatperf$F1)



# Interaction Effects
## Simulated Data: p15,n50, Int
### GAFS
res_gafs = pbapply::pblapply(1:1, function(seeder){
  varnum=15
  covariate= c(1)
  main_var = 10
  df = AIFS::dataset(varnum = varnum, setting = "Correlation", var = "Mar", seed=seeder, main_var=main_var, var_effect=c(0.5, -0.5), correlation_var=15, correlation_val=5, high_dim=T, train_sample=50)
  oridf = df

  para = list(interactions=T, int_term=2, ncomp=5, intercept=T, out_type="continuous", outcome_var="y")
  output = AIFS::trainfs(ml_train = T, Best_Para_train= rep(1,6) ,covariate=covariate, sp=para, dr_tech = c("No_DR"), ml_tech = c("rf"), optim_tech = "GA", df_p=df, std_techlist = "lasso", list_size = 15, maxiter = 100, run = 20, pop = 200)
  output$selfeatperf = list()
  for(i in c("F1", "AUC", "Accuracy")){
    output$selfeatperf[[i]] = AIFS:::sfperf(score = i, selfeat = output$feature[-1], tarnumb = 10, varnum = varnum, int = T)
  }
  return(output)
})
res_gafs
## Wrapper
res_std = pbapply::pblapply(1:1, function(seeder){
  varnum=15
  covariate= c(1)
  main_var = 10
  df = AIFS::dataset(varnum = varnum, setting = "Correlation", var = "Mar", seed=seeder, main_var=main_var, var_effect=c(0.5, -0.5), correlation_var=15, correlation_val=5, high_dim=T, train_sample=50)
  oridf = df

  para = list(interactions=T, int_term=2, ncomp=5, intercept=T, out_type="continuous", outcome_var="y")
  output = AIFS::stdfs(ml_train = T, Best_Para_train= rep(1,6) ,covariate=covariate, sp=para, dr_tech = c("No_DR"), ml_tech = c("rf"), optim_tech = "GA", df_p=df, std_techlist = "lasso", list_size = 15, maxiter = 100, run = 20, pop = 20)

  output$selfeatperf = list()
  for(i in c("F1", "AUC", "Accuracy")){
    output$selfeatperf[[i]] = AIFS:::sfperf(score = i, selfeat = output$feature[-1], tarnumb = 10, varnum = varnum, int = T)
  }

  return(output)
})
res_std

## Simulated Data: p25,n50, Int
### GAFS
res_gafs = pbapply::pblapply(1:1, function(seeder){
  varnum=25
  covariate= c(1)
  main_var = 10
  df = AIFS::dataset(varnum = varnum, setting = "Correlation", var = "Mar", seed=seeder, main_var=main_var, var_effect=c(0.5, -0.5), correlation_var=15, correlation_val=5, high_dim=T, train_sample=50)
  oridf = df

  para = list(interactions=T, int_term=2, ncomp=5, intercept=T, out_type="continuous", outcome_var="y")
  output = AIFS::trainfs(ml_train = T, Best_Para_train= rep(1,6) ,covariate=covariate, sp=para, dr_tech = c("No_DR"), ml_tech = c("rf"), optim_tech = "GA", df_p=df, std_techlist = "lasso", list_size = 15, maxiter = 100, run = 20, pop = 200)
  output$selfeatperf = list()
  for(i in c("F1", "AUC", "Accuracy")){
    output$selfeatperf[[i]] = AIFS:::sfperf(score = i, selfeat = output$feature[-1], tarnumb = 10, varnum = varnum, int = T)
  }
  return(output)
})
res_gafs
## Wrapper
res_std = pbapply::pblapply(1:1, function(seeder){
  varnum=25
  covariate= c(1)
  main_var = 10
  df = AIFS::dataset(varnum = varnum, setting = "Correlation", var = "Mar", seed=seeder, main_var=main_var, var_effect=c(0.5, -0.5), correlation_var=15, correlation_val=5, high_dim=T, train_sample=50)
  oridf = df

  para = list(interactions=T, int_term=2, ncomp=5, intercept=T, out_type="continuous", outcome_var="y")
  output = AIFS::stdfs(ml_train = T, Best_Para_train= rep(1,6) ,covariate=covariate, sp=para, dr_tech = c("No_DR"), ml_tech = c("rf"), optim_tech = "GA", df_p=df, std_techlist = "lasso", list_size = 15, maxiter = 100, run = 20, pop = 20)

  output$selfeatperf = list()
  for(i in c("F1", "AUC", "Accuracy")){
    output$selfeatperf[[i]] = AIFS:::sfperf(score = i, selfeat = output$feature[-1], tarnumb = 10, varnum = varnum, int = T)
  }

  return(output)
})
res_std

## Simulated Data: p15,n100, Int
### GAFS
res_gafs = pbapply::pblapply(1:1, function(seeder){
  varnum=15
  covariate= c(1)
  main_var = 10
  df = AIFS::dataset(varnum = varnum, setting = "Correlation", var = "Mar", seed=seeder, main_var=main_var, var_effect=c(0.5, -0.5), correlation_var=15, correlation_val=5, high_dim=T, train_sample=100)
  oridf = df

  para = list(interactions=T, int_term=2, ncomp=5, intercept=T, out_type="continuous", outcome_var="y")
  output = AIFS::trainfs(ml_train = T, Best_Para_train= rep(1,6) ,covariate=covariate, sp=para, dr_tech = c("No_DR"), ml_tech = c("rf"), optim_tech = "GA", df_p=df, std_techlist = "lasso", list_size = 15, maxiter = 100, run = 20, pop = 200)
  output$selfeatperf = list()
  for(i in c("F1", "AUC", "Accuracy")){
    output$selfeatperf[[i]] = AIFS:::sfperf(score = i, selfeat = output$feature[-1], tarnumb = 10, varnum = varnum, int = T)
  }
  return(output)
})
res_gafs
## Wrapper
res_std = pbapply::pblapply(1:1, function(seeder){
  varnum=15
  covariate= c(1)
  main_var = 10
  df = AIFS::dataset(varnum = varnum, setting = "Correlation", var = "Mar", seed=seeder, main_var=main_var, var_effect=c(0.5, -0.5), correlation_var=15, correlation_val=5, high_dim=T, train_sample=100)
  oridf = df

  para = list(interactions=T, int_term=2, ncomp=5, intercept=T, out_type="continuous", outcome_var="y")
  output = AIFS::stdfs(ml_train = T, Best_Para_train= rep(1,6) ,covariate=covariate, sp=para, dr_tech = c("No_DR"), ml_tech = c("rf"), optim_tech = "GA", df_p=df, std_techlist = "lasso", list_size = 15, maxiter = 100, run = 20, pop = 20)

  output$selfeatperf = list()
  for(i in c("F1", "AUC", "Accuracy")){
    output$selfeatperf[[i]] = AIFS:::sfperf(score = i, selfeat = output$feature[-1], tarnumb = 10, varnum = varnum, int = T)
  }

  return(output)
})
res_std

## Simulated Data: p25,n100, Int
### GAFS
res_gafs = pbapply::pblapply(1:1, function(seeder){
  varnum=25
  covariate= c(1)
  main_var = 10
  df = AIFS::dataset(varnum = varnum, setting = "Correlation", var = "Mar", seed=seeder, main_var=main_var, var_effect=c(0.5, -0.5), correlation_var=15, correlation_val=5, high_dim=T, train_sample=100)
  oridf = df

  para = list(interactions=T, int_term=2, ncomp=5, intercept=T, out_type="continuous", outcome_var="y")
  output = AIFS::trainfs(ml_train = T, Best_Para_train= rep(1,6) ,covariate=covariate, sp=para, dr_tech = c("No_DR"), ml_tech = c("rf"), optim_tech = "GA", df_p=df, std_techlist = "lasso", list_size = 15, maxiter = 100, run = 20, pop = 200)
  output$selfeatperf = list()
  for(i in c("F1", "AUC", "Accuracy")){
    output$selfeatperf[[i]] = AIFS:::sfperf(score = i, selfeat = output$feature[-1], tarnumb = 10, varnum = varnum, int = T)
  }
  return(output)
})
res_gafs
## Wrapper
res_std = pbapply::pblapply(1:1, function(seeder){
  varnum=25
  covariate= c(1)
  main_var = 10
  df = AIFS::dataset(varnum = varnum, setting = "Correlation", var = "Mar", seed=seeder, main_var=main_var, var_effect=c(0.5, -0.5), correlation_var=15, correlation_val=5, high_dim=T, train_sample=100)
  oridf = df

  para = list(interactions=T, int_term=2, ncomp=5, intercept=T, out_type="continuous", outcome_var="y")
  output = AIFS::stdfs(ml_train = T, Best_Para_train= rep(1,6) ,covariate=covariate, sp=para, dr_tech = c("No_DR"), ml_tech = c("rf"), optim_tech = "GA", df_p=df, std_techlist = "lasso", list_size = 15, maxiter = 100, run = 20, pop = 20)

  output$selfeatperf = list()
  for(i in c("F1", "AUC", "Accuracy")){
    output$selfeatperf[[i]] = AIFS:::sfperf(score = i, selfeat = output$feature[-1], tarnumb = 10, varnum = varnum, int = T)
  }

  return(output)
})
res_std
