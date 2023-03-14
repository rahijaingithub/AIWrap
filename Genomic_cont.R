# Functions
## Remove the variables with zero or NA sd
sdrem = function(inputdf, minsd = 0){

  df = apply(inputdf, 2, function(x){ y = sd(x, na.rm = T); if(!is.na(y) & y>minsd){as.numeric(x)}else{NULL}})

  if(is.list(df)){inputdf = do.call(cbind, df)}
  else{inputdf = df}
  inputdf = data.frame(inputdf)

  return(inputdf)
}
## Remove the variables with low cv
cvrem = function(inputdf, mincv = 0.1){

  df = apply(inputdf, 2, function(x){ y = sd(x, na.rm = T); my = mean(x, na.rm = T); if(!is.na(y/my) & y/my>mincv){x}else{NULL}})
  if(is.list(df)){inputdf = do.call(cbind, df)}
  else{inputdf = df}
  inputdf = data.frame(inputdf)

  return(inputdf)
}
## Remove the variables with low variation
lowvar=function(inputdf, minfreq=100/10){
  al= unlist(apply(inputdf,2,function(x) caret::nearZeroVar(x, freqCut = minfreq, saveMetrics = T)[4]))
  drop_var_2=names(al)[which(al==TRUE)]
  drop_var_2 = stringr::str_replace(drop_var_2,".nzv","")
  newdf=inputdf[,setdiff(names(inputdf),drop_var_2)]
  newdf = data.frame(newdf)
  names(newdf) = setdiff(names(inputdf),drop_var_2)
  return(newdf)
}
## Remove correlated features: Use if features arranged in important sequence
fastCorrel=function(inputdf, cutoff){
  f = function(df, cut = cutoff){
    dfcor=cor(df, use = "pairwise.complete.obs", method = "spearman")
    highcor=apply(dfcor,1,function(x) length(which(x>=cut | x<= (-1*cut))))
    var=attributes(highcor)$names
    # Select variables with no correlation
    selvar = names(df)[which(highcor == 1)]

    # Keep variables with high correlation
    highvar = setdiff(var, selvar)

    varseq = data.frame(variable = attributes(highcor)$names, ncor = highcor)
    varseq = varseq[varseq$ncor > 1, ]
    # varseq = varseq[order(varseq$ncor, decreasing = T),]

    return(list(selvar,varseq))
  }

  outf = f(df = inputdf)
  selvar = outf[[1]]
  varseq = outf[[2]]

  for(i in 1:nrow(varseq)){
    # cat(i, " ")
    if(nrow(varseq) <3){
      selvar = c(selvar, varseq$variable[-1])
      break}
    newvar = varseq$variable[-nrow(varseq)]
    newdf = inputdf[,newvar]
    outf = f(df = newdf)
    selvar = c(selvar,outf[[1]])
    varseq = outf[[2]]
  }

  finaldf = inputdf[,selvar]
  return(finaldf)
}
fastCorrelv2 = function(inputdf, cutoff, dsplit = F, bin = 100){
  f = function(df, cut = cutoff){
    dfcor=cor(df, use = "pairwise.complete.obs", method = "spearman")
    dfcor = data.frame(dfcor)
    dfcor[dfcor>= cut | dfcor<= (-1*cut)] = 1
    dfcor[dfcor<1] = 0
    highcorvar = colSums(dfcor)
    highvar = highcorvar[highcorvar >1]
    varseq = data.frame(variable = names(highvar), ncor = highvar)
    selvar = setdiff(names(dfcor), names(highvar))
    return(list(selvar,varseq))
  }
  fdf = function(df, cutoff){
    outf = f(df = inputdf, cut = cutoff)
    selvar = outf[[1]]
    varseq = outf[[2]]
    for(i in 1:nrow(varseq)){
      # cat(i, " ")
      if(nrow(varseq) <3){
        selvar = c(selvar, varseq$variable[-nrow(varseq)])
        break}
      newvar = varseq$variable[-nrow(varseq)]
      newdf = inputdf[,newvar]
      outf = f(df = newdf, cut = cutoff)
      selvar = c(selvar,outf[[1]])
      varseq = outf[[2]]
    }
    finaldf = inputdf[,selvar]
    return(finaldf)
  }

  if(!dsplit){
    finaldf = fdf(df = inputdf, cutoff = cutoff)
  }
  else{
    # Bin Evaluation
    nbin = ceiling(ncol(inputdf)/bin)
    df = inputdf
    iter = 0
    for(j in 1:nbin){
      newcols = max(ncol(df)-(bin*j),1)
      if(newcols == 1){iter = 1+iter}
      if(iter >1){break}
      newdf = df[,newcols:ncol(df)]
      revdf = fdf(df = newdf, cutoff = cutoff)
      dropvar = setdiff(names(newdf), names(revdf))
      selvar = setdiff(names(df), dropvar)
      df = df[,selvar]
    }
    finaldf = revdf
  }
  return(finaldf)
}


# Get genomic data
filelist = list.files(path="D:/RProjects/genomicdata/Datasets/Featurelist", pattern = "*.csv", full.names = T)
allfiles <- lapply(filelist, read.csv)

filename = list.files(path="D:/RProjects/genomicdata/Datasets/Featurelist", pattern = "*.csv")
filename = stringi::stri_replace_all_fixed(filename, ".csv","")
filename = stringi::stri_replace_all_fixed(filename, " _","_")
names(allfiles) = filename

# Drop datasets with no continuous outcome
tarfiles = lapply(allfiles, function(x) {
  y = length(x$cig[!is.na(x$cig) & x$cig !=0])
  if(y<10){return(NA)}
  else{
    df = x[,c(2:which(names(x) == "cig"))]
    df = df[complete.cases(df),]
    df = df[!duplicated(df),]

    if(sd(df$cig) == 0){return(NA)}
    # # Remove Columns with zero sd
    df =  sdrem(inputdf = df, minsd = 0)
    # str(df)
    # # Remove columns with low coefficient of variation
    df =  lowvar(inputdf = df)
    if(all(!names(df) %in% "cig")){return(NA)}
    cat("Smoking: ", length(which(df$cig != 0))/nrow(df), "Not Smoking: ", length(which(df$cig == 0))/nrow(df))

    print(c(mean(df$cig), sd(df$cig)))
    ## Z scale the dataset
    df = data.frame(scale(df))
    df
  }
})
tarfiles = tarfiles[!is.na(tarfiles)]

{
  # Some Summary results
  genecol = sapply(tarfiles, function(x) ncol(x)-1)
  generow = sapply(tarfiles, function(x) nrow(x))
  geneval = lapply(tarfiles, function(x) c(mean(x$cig), sd(x$cig)))

  # prior malignancy
  tarfiles = lapply(allfiles, function(x) {
    y = length(x$cig[!is.na(x$cig) & x$cig !=0])
    if(y<10){return(NA)}
    else{
      df = x[,c(2:which(names(x) == "cig"))]
      df = df[complete.cases(df),]
      df = df[!duplicated(df),]

      if(sd(df$cig) == 0){return(NA)}
      # # Remove Columns with zero sd
      df =  sdrem(inputdf = df, minsd = 0)
      # str(df)
      # # Remove columns with low coefficient of variation
      df =  lowvar(inputdf = df)
      if(all(!names(df) %in% "cig")){return(NA)}
      cat("Smoking: ", length(which(df$cig != 0))/nrow(df), "Not Smoking: ", length(which(df$cig == 0))/nrow(df))
      print(c(mean(df$cig), sd(df$cig)))
      print(table(x$priormalignancy)/nrow(x))
      print(table(x$OS_Event)/nrow(x))

      # xsurv = x[,c("OS_Time", "OS_Event")]
      # xsurv = xsurv[complete.cases(xsurv[,"OS_Event"]),]
      #
      # # Time 2 years
      # year_vital2 = ifelse(xsurv$OS_Time < 365.25*2)
      # Time 5 years


      ## Z scale the dataset
      df = data.frame(scale(df))
      df
    }
  })
}

# Find top 500 features in each dataset
dflist = pbapply::pblapply(tarfiles[!is.na(tarfiles)], function(x)
  {
    # print(T)
    # str(x[,1:5])
    outvar= "cig"
    inputdf = x[, !names(x) %in% outvar]
    # Perform PLS
    plsreg = plsdepot::plsreg1(predictors = inputdf, response = x[,ncol(x)],
                               comps = 2, crosval = T)
    # Get the features importance sequence
    featseq = names(sort(abs(plsreg$x.loads[,1])))

    # Remove features with high correlation
    df = fastCorrelv2(inputdf = inputdf[,featseq[1:min(1000, length(featseq))]], cutoff = 0.8, dsplit = F)
    # str(df)
    # ## Remove correlated features
    print(ncol(df))
    # str(df)
    df = x[, c(names(df)[1:min(500,ncol(df))], outvar)]#
    names(df)[ncol(df)] = "y"
    df
})



# TRIAL RUN
### GAFS
res_gafs = pbapply::pblapply(1:1, function(seeder){
  df = dflist[[1]]
  fakenames = paste("X", 1:(ncol(df)-1), sep = "")
  oriname = names(df)[-ncol(df)]
  names(df)[-ncol(df)] = fakenames
  cut = floor(nrow(df)*0.8)
  df = list(train = df[1:cut,], test = df[-1:-cut,])
  oridf = df

  para = list(interactions=F, int_term=2, ncomp=5, intercept=T, out_type="continuous", outcome_var="y")
  etime = system.time({
    output = AIFS::aiwrafs(df_p = df, std_techlist = "lasso", sp=para, list_size = 15, maxiter = 100, run = 20, pop = 200, ml_tech = c("rf"),Best_Para_train = rep(1,6), updatePPM = T, dr_tech = c("No_DR"))
  })

  perfdf = output$performance
  feat = t(res_std[[1]]$feature)[-1]
  orifeat = oriname[which(fakenames %in% feat)]
  orifeat = paste(orifeat, collapse = "_")
  perfdf$feat = orifeat
  perfdf$time = etime[3]
  return(perfdf)
})
### Wrapper
res_std = pbapply::pblapply(1:1, function(seeder){
  df = dflist[[1]]
  fakenames = paste("X", 1:(ncol(df)-1), sep = "")
  oriname = names(df)[-ncol(df)]
  names(df)[-ncol(df)] = fakenames
  cut = floor(nrow(df)*0.8)
  df = list(train = df[1:cut,], test = df[-1:-cut,])
  oridf = df

  para = list(interactions=F, int_term=2, ncomp=5, intercept=T, out_type="continuous", outcome_var="y")
  etime = system.time({
    output = AIFS::stdwrap(df_p = df, std_techlist = "lasso", sp=para, list_size = 15, maxiter = 100, run = 20, pop = 200)
  })

  perfdf = output$performance
  feat = t(res_std[[1]]$feature)[-1]
  orifeat = oriname[which(fakenames %in% feat)]
  orifeat = paste(orifeat, collapse = "_")
  perfdf$feat = orifeat
  perfdf$time = etime[3]
  return(perfdf)
})


# MAIN RUN
res = lapply(names(dflist)[1], function(i)
  {
    df = dflist[[i]]
    fakenames = paste("X", 1:(ncol(df)-1), sep = "")
    oriname = names(df)[-ncol(df)]
    names(df)[-ncol(df)] = fakenames
    set.seed(1)
    folds = splitTools::create_folds(y = df$y, k = 10, type = "basic", m_rep = 1)

    ### GAFS
    res_gafs = pbapply::pblapply(folds, function(seeder){

      df = list(train = df[seeder,], test = df[-seeder,])
      oridf = df

      para = list(interactions=F, int_term=2, ncomp=5, intercept=T, out_type="continuous", outcome_var="y")
      etime = system.time({
        output = AIFS::aiwrafs(df_p = df, std_techlist = "lasso", sp=para, list_size = 15, maxiter = 100, run = 20, pop = 200, ml_tech = c("rf"),Best_Para_train = rep(1,6), updatePPM = T, dr_tech = c("No_DR"))
      })

      perfdf = output$performance
      feat = t(output$feature)[-1]
      orifeat = oriname[which(fakenames %in% feat)]
      orifeat = paste(orifeat, collapse = "_")
      perfdf$feat = orifeat
      perfdf$time = etime[3]
      return(perfdf)
    })
    res_gafs = do.call(rbind, res_gafs)
    write.csv(res_gafs, paste(i,"_aifs.csv", sep = ""))

    ### Wrapper
    res_std = pbapply::pblapply(folds, function(seeder){
      df = list(train = df[seeder,], test = df[-seeder,])
      oridf = df

      para = list(interactions=F, int_term=2, ncomp=5, intercept=T, out_type="continuous", outcome_var="y")

      etime = system.time({
        output = AIFS::stdwrap(df_p = df, std_techlist = "lasso", sp=para, list_size = 15, maxiter = 100, run = 20, pop = 200)
      })
      perfdf = output$performance
      feat = t(output$feature)[-1]
      orifeat = oriname[which(fakenames %in% feat)]
      orifeat = paste(orifeat, collapse = "_")
      perfdf$feat = orifeat
      perfdf$time = etime[3]
      return(perfdf)
    })
    res_std = do.call(rbind, res_std)
    write.csv(res_std, paste(i,"_std.csv", sep = ""))
})

# Data Analysis
library(magrittr)
library(dplyr)
Genomic_Results <- read.csv("D:/MegaSync/MEGAsync/UHN/Theory 5/BMC Bioinformatics/Genomic_Results.csv")
gr = Genomic_Results[Genomic_Results$Source.Name != "",]
finalres = gr %>%
  select(Source.Name, Method, RMSE, time, MV) %>%
  group_by(Source.Name, Method) %>%
  summarise(meanrmse = mean(RMSE),
            marginrmse = qt(0.975,df=10-1)*sd(RMSE)/sqrt(10),
            lcirmse = meanrmse - marginrmse,
            ucirmse = meanrmse + marginrmse,
            meantime = round(mean(time),0),
            margintime = qt(0.975,df=10-1)*sd(time)/sqrt(10),
            lcitime = round(meantime - margintime,0),
            ucitime = round(meantime + margintime,0),
            meanmv = round(mean(MV),0),
            marginmv = qt(0.975,df=10-1)*sd(MV)/sqrt(10),
            lciMV = round(meanmv - marginmv,0),
            uciMV = round(meanmv + marginmv,0),
            )
write.csv(finalres, "Final_res.csv")

# Input Data Analysis
## get each dataset: p,n, smoking summary, binary summary, time to event summary


# Gene data
genedf = gr[,c("Source.Name", "Method", "feat")]

genelist = lapply(1:nrow(genedf), function(x) {
  y = genedf$feat[x]
  glist = stringi::stri_split_fixed(y, "_")[[1]]
  df = data.frame(sample = genedf$Source.Name[x], method = genedf$Method[x], gene = glist, stringsAsFactors = F)
})

genelistdf = do.call(rbind, genelist)
genelistdf$gene[genelistdf$gene == ""] = NA
genelistdf = genelistdf[complete.cases(genelistdf),]
genelistwide = reshape2::dcast(genelistdf, sample + method ~gene)

# Get Frequency
genefreq = lapply(1:nrow(genelistwide), function(x){
  y = genelistwide[x,]

  if(any(y[-1:-2]>8)){
    z = y[y > 8]
    names(z) = names(genelistwide)[which(y>8)]
  }
  else{
    z = c(y[1:2], y[y == max(y[-1:-2])])
    names(z) = names(genelistwide)[c(1:2,which(y == max(y[-1:-2])))]
    z = unlist(z)
  }
  z
})

genebind = do.call(bind_rows, genefreq)
write.csv(genebind, "genebind.csv")
