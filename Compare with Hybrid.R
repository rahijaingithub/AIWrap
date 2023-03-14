# Create the function
library(praznik)
library(caret) # Load the caret package
library(FSelector)
# AIFS::stdwrap(df_p = df, std_techlist = "lasso", sp=para, list_size = 15, maxiter = 100, run = 20, pop = 200)

jmiscore_list = function(featlist, final_feat, traindf, output){
  jmi_score = sapply(featlist, function(i){
    tdf = traindf[,c(final_feat,i)]
    outdf = traindf[,output]
    score = praznik::jmiScores(X = tdf[,final_feat], Z = tdf[,i], Y = outdf)
    score[length(score)]
  })
  jmidf = data.frame(feature = featlist, score = jmi_score)
  jmidf = jmidf[order(jmidf$score, decreasing = T),]
  return(jmidf)
}

jmiscore_list_cont = function(featlist, final_feat, traindf, output){
  jmi_score = sapply(featlist, function(i){
    tdf = traindf[,c(final_feat,i)]
    outdf = traindf[,output]
    feat_list = rep(0, ncol(traindf)-1)
    names(feat_list) = names(traindf)[names(traindf) != output]
    feat_list[names(feat_list) %in% c(final_feat,i)] = 1

    sp = list(interactions=F, int_term=2, ncomp=5, intercept=T, out_type="continuous", outcome_var= output)
    std_input = list(std_techlist = "ridge", list(traindf, traindf) , covariate = c(1), sp)
    # print(sp)
    # print(feat_list)
    score = AIFS:::main_fun(x = feat_list, std_input=std_input, varnum = ncol(traindf)-1)
    # str(score)
    s = score$performance$RMSE[score$performance$datatype == "test"]
    # print(c(final_feat,i,1/s))
    1/s
  })
  jmidf = data.frame(feature = featlist, score = jmi_score)
  jmidf = jmidf[order(jmidf$score, decreasing = T),]
  return(jmidf)
}

cv_perf = function(folds, traindf, sp, std_techlist, covariate, final_feat){

  std_perf = sapply(folds, function(x){
    valdf = traindf[x,]
    traindf = traindf[-x,]
    # print(c(nrow(traindf),nrow(valdf)))
    std_input = list(std_techlist, list(traindf, valdf) , covariate, sp)
    res = AIFS:::main_fun(x = final_feat, std_input=std_input, varnum = ncol(traindf)-1)
    df = res$performance
    # print(x)
    res_train = df$RMSE[df$datatype == "train"]
    res_test = df$RMSE[df$datatype == "test"]
  })
  # std_perf = mean(std_perf)
  return(std_perf)
}

FSJMI = function(traindf, std_techlist = 'lasso', output = "y", testdf, covariate = c(1), k =10){

  # Define class labels vector
  out_value <- traindf[[output]]
  final_feat = rep(0, ncol(traindf)-1)
  names(final_feat) = names(traindf)[names(traindf) != output]
  # Create k-folds
  set.seed(1)
  folds <- createFolds(out_value, k = k)

  # Step 1: Calculate Mutual Information
  inputfeatures = setdiff(names(traindf), output)
  mis = sapply(inputfeatures, function(x) {set.seed(1); praznik::miScores(X = traindf[,x], Y = traindf[,output])})
  mi_feat = data.frame(feature = inputfeatures, score = mis)
  mi_feat = mi_feat[order(mi_feat$score, decreasing = T),]
  # str(mi_feat)

  # Step 1: Estimate best performance
  sp = list(interactions=F, int_term=2, ncomp=5, intercept=T, out_type="continuous", outcome_var= output)
  final_feat[which(inputfeatures == mi_feat$feature[1])] = 1
  # print(final_feat)
  ## Run CV performance
  str(folds)
  best_perf = cv_perf(folds = folds, traindf = traindf, sp = sp, std_techlist = std_techlist, covariate = covariate, final_feat = final_feat)
  # print(best_perf)

  # Step 2: Conditional Mutual Information Measures (CMIM)
  rem_feat = mi_feat$feature[-1]
  counter = 0

  while (length(rem_feat) != 0){
    # cat(length(rem_feat), " ")
    jmi_df = jmiscore_list(featlist = rem_feat, final_feat = names(final_feat)[final_feat ==1], traindf = traindf, output = output)
    # str(jmi_df)
    # Select the feature with the highest interaction score
    tmp_list = final_feat
    tmp_list[names(tmp_list) == jmi_df$feature[1]] = 1
    # print(tmp_list)
    # Temp_list performance
    temp_perf = cv_perf(folds = folds, traindf = traindf, sp = sp, std_techlist = std_techlist, covariate = covariate, final_feat = tmp_list)

    # print(c(temp_perf, best_perf))

    # Perform a right-tailed paired t-test
    t_test <- t.test(temp_perf, best_perf, paired = TRUE, alternative = "less")

    # View the results
    # print(t_test)

    pval = t_test$p.value
    # cat(pval, " ")
    if(is.na(pval)){break}
    else if(pval < 0.1){
      # print("Enter")
      final_feat = tmp_list;
      best_perf = temp_perf
      rem_feat = setdiff(rem_feat, names(final_feat)[final_feat == 1])
      # print(length(rem_feat))
      }
    else{break}

    # if(counter <2){counter = counter +1}else{break}
  }

  # Step 3: Get performance on Test data: Use ridge instead of std_Techlist as final model is dropping all variables
  std_input = list(std_techlist = "ridge", list(traindf, testdf) , covariate, sp)
  print(final_feat)
  res = AIFS:::main_fun(x = final_feat, std_input=std_input, varnum = ncol(traindf)-1)
  output = res
  # print(output)
  output$selfeatperf = list()
  for(i in c("F1", "AUC", "Accuracy")){
    print(i)
    selfeat = names(final_feat)[final_feat == 1]
    perfval = AIFS:::sfperf(score = i, selfeat = selfeat, tarnumb = 10, varnum = 50, int = F)
    if(is.nan(perfval)){output$selfeatperf[[i]] = 0}
    else{output$selfeatperf[[i]] = perfval}
  }
  return(output)
}
FSJMI_cont = function(traindf, std_techlist = 'lasso', output = "y", testdf, covariate = c(1), k =10){

  # Define class labels vector
  out_value <- traindf[[output]]
  final_feat = rep(0, ncol(traindf)-1)
  names(final_feat) = names(traindf)[names(traindf) != output]
  # Create k-folds
  set.seed(1)
  folds <- createFolds(out_value, k = k)

  # Step 1: Calculate Mutual Information
  inputfeatures = setdiff(names(traindf), output)
  mis = sapply(inputfeatures, function(x) {set.seed(1); abs(cor(x = traindf[,x], y = traindf[,output], method = "spearman"))})
  mi_feat = data.frame(feature = inputfeatures, score = mis)
  mi_feat = mi_feat[order(mi_feat$score, decreasing = T),]
  # str(mi_feat)

  # Step 1: Estimate best performance
  sp = list(interactions=F, int_term=2, ncomp=5, intercept=T, out_type="continuous", outcome_var= output)
  final_feat[which(inputfeatures == mi_feat$feature[1])] = 1
  # print(final_feat)
  ## Run CV performance
  str(folds)
  best_perf = cv_perf(folds = folds, traindf = traindf, sp = sp, std_techlist = std_techlist, covariate = covariate, final_feat = final_feat)
  # print(best_perf)

  # Step 2: Conditional Mutual Information Measures (CMIM)
  rem_feat = mi_feat$feature[-1]
  counter = 0

  while (length(rem_feat) != 0){
    # cat(length(rem_feat), " ")
    jmi_df = jmiscore_list_cont(featlist = rem_feat, final_feat = names(final_feat)[final_feat ==1], traindf = traindf, output = output)
    # str(jmi_df)
    # Select the feature with the highest interaction score
    tmp_list = final_feat
    tmp_list[names(tmp_list) == jmi_df$feature[1]] = 1
    # print(tmp_list)
    # Temp_list performance
    temp_perf = cv_perf(folds = folds, traindf = traindf, sp = sp, std_techlist = std_techlist, covariate = covariate, final_feat = tmp_list)

    # print(c(temp_perf, best_perf))

    # Perform a right-tailed paired t-test
    t_test <- t.test(temp_perf, best_perf, paired = TRUE, alternative = "less")

    # View the results
    # print(t_test)

    pval = t_test$p.value
    # cat(pval, " ")
    if(is.na(pval)){break}
    else if(pval < 0.1){
      # print("Enter")
      final_feat = tmp_list;
      best_perf = temp_perf
      rem_feat = setdiff(rem_feat, names(final_feat)[final_feat == 1])
      # print(length(rem_feat))
    }
    else{break}

    # if(counter <2){counter = counter +1}else{break}
  }

  # Step 3: Get performance on Test data: Use ridge instead of std_Techlist as final model is dropping all variables
  std_input = list(std_techlist = "ridge", list(traindf, testdf) , covariate, sp)
  print(final_feat)
  res = AIFS:::main_fun(x = final_feat, std_input=std_input, varnum = ncol(traindf)-1)
  output = res
  # print(output)
  output$selfeatperf = list()
  for(i in c("F1", "AUC", "Accuracy")){
    print(i)
    selfeat = names(final_feat)[final_feat == 1]
    perfval = AIFS:::sfperf(score = i, selfeat = selfeat, tarnumb = 10, varnum = 50, int = F)
    if(is.nan(perfval)){output$selfeatperf[[i]] = 0}
    else{output$selfeatperf[[i]] = perfval}
  }
  return(output)
}

# # Generate data
# seeder = 1
# varnum=50
# covariate= c(1)
# main_var = 10
#
# df = AIFS::dataset(varnum = varnum, setting = "Correlation", var = "onlyMar", seed=seeder, main_var=main_var, var_effect=c(0.5, -0.5), correlation_var=15, correlation_val=5, high_dim=T, train_sample=50)
#
# mi = FSJMI(traindf = df$train, testdf = df$test, k=10)
