readLendFile = function(fileName, mult = 10, k = 10, paramTakeAway = 5, readAll = TRUE, type = "accepted"){
  nLines = R.utils::countLines(fileName)
  print(nLines)
  if(readAll){
    d0 = read.table(file = fileName, sep = ",", skip = 1, header = TRUE, stringsAsFactors = F)  
  } else
  {
    # d = read.table(file = fileName, sep = ",",nrows = 1)
    # k = 10
    chunk = as.numeric(round(nLines/k,0))
    # mult = 10
    # if(ncol(d)==1){
    d = read.table(file = fileName, sep = ",",nrows = 10000, skip = 1, header = TRUE, stringsAsFactors = F)  
    colClasses = unlist(lapply(names(d), function(s)class(d[[s]])))
    colNames = names(d)
    d0 = d[-(1:nrow(d)),]
    takeAway = 0
    for(i in 1:k){
      if(i == k){
        takeAway = paramTakeAway
      }
      print(1+chunk*(i-1))
      print(chunk - takeAway)
      d = try({read.table(file = fileName, sep = ",",
                          nrows = chunk - takeAway, 
                          skip = 2+chunk*(i-1), stringsAsFactors = F)})
      if(class(d)!="try-error"){
        if(type == "rejected"){
          ind = which(!is.na(d[[4]]))
          ind = sample(ind,size = length(ind)/mult)
          d0 = rbind(d0,d[ind,]) #Risk_Score field is not NA
        }  else
        {
          d0 = rbind(d0,d[sample(1:chunk,size = chunk/mult),])  
        }
        print(i)
      } else
      {
        print(paste0("error in ",i))
      }
    }
    # }
    
    names(d0) = colNames
    
  }
  
  
  d0
}

preProcess = function(d0, strict = TRUE, isAdjPurpose = TRUE){
  d0$term = as.numeric(gsub(" months","",d0$term))
  d0$int_rate = as.numeric(gsub("%","",d0$int_rate))/100
  d0$revol_util = as.numeric(gsub("%","",d0$revol_util))/100
  for(x in c("loan_amnt","funded_amnt","funded_amnt_inv","installment","dti","delinq_2yrs","open_acc","inq_last_6mths",
             "mths_since_last_delinq","mths_since_last_record","mths_since_last_record","pub_rec",
             "revol_bal","revol_util","total_acc","out_prncp","out_prncp_inv","total_pymnt",
             "total_pymnt_inv","total_pymnt_inv","total_rec_prncp","total_rec_int","total_rec_late_fee",
             "recoveries","collection_recovery_fee","last_pymnt_amnt","collections_12_mths_ex_med",
             "mths_since_last_major_derog",
             "acc_now_delinq","tot_coll_amt","tot_cur_bal","open_acc_6m","open_act_il","open_il_12m","open_il_24m",
             "mths_since_rcnt_il","total_bal_il","il_util","open_rv_12m","open_rv_24m","max_bal_bc","all_util",
             "total_rev_hi_lim","inq_fi","total_cu_tl","inq_last_12m","acc_open_past_24mths","avg_cur_bal","bc_open_to_buy",
             "bc_util","chargeoff_within_12_mths","delinq_amnt","mo_sin_old_il_acct","mo_sin_old_rev_tl_op","mo_sin_rcnt_rev_tl_op",
             "mo_sin_rcnt_tl","mort_acc","mths_since_recent_bc","mths_since_recent_bc_dlq","mths_since_recent_inq",
             "mths_since_recent_revol_delinq","num_accts_ever_120_pd","num_actv_bc_tl","num_actv_rev_tl","num_bc_sats","num_bc_tl",
             "num_il_tl","num_op_rev_tl","num_rev_accts","num_rev_tl_bal_gt_0","num_sats","num_tl_120dpd_2m","num_tl_30dpd",
             "num_tl_90g_dpd_24m","num_tl_op_past_12m","pct_tl_nvr_dlq","percent_bc_gt_75","pub_rec_bankruptcies","tax_liens",
             "tot_hi_cred_lim","total_bal_ex_mort","total_bc_limit","total_il_high_credit_limit","revol_bal_joint",
             "sec_app_earliest_cr_line","sec_app_inq_last_6mths","sec_app_mort_acc","sec_app_open_acc","sec_app_revol_util","sec_app_open_act_il",
             "sec_app_num_rev_accts","sec_app_chargeoff_within_12_mths","sec_app_collections_12_mths_ex_med","sec_app_mths_since_last_major_derog","hardship_flag",
             "deferral_term","hardship_amount","hardship_length","hardship_dpd","hardship_loan_status",
             "orig_projected_additional_accrued_interest","hardship_payoff_balance_amount","hardship_last_payment_amount","debt_settlement_flag","settlement_amount")){
    print(x)
    d0[[paste0(x)]] = as.numeric(d0[[x]])
  }
  d0$emp_length[d0$emp_length == "< 1 year"] = "0" 
  d0$emp_length[d0$emp_length == "10+ years"] = "10" 
  d0$emp_length = gsub("year|years","",d0$emp_length)
  d0$emp_length = as.numeric(d0$emp_length)
  d0$pti = d0$installment/(d0$annual_inc/12)
  d0$amount2term = d0$loan_amnt/ d0$term
  d0$issue_d = dtMaker(d0$issue_d)
  d0$purpose = tolower(d0$purpose)
  
  if(isAdjPurpose){
    d0 = adjPurpose(d0)
  }
  # bad_indicators <- c("Charged Off",
  #                     "Default",
  #                     "Does not meet the credit policy. Status:Charged Off",
  #                     "In Grace Period", 
  #                     "Default Receiver", 
  #                     "Late (16-30 days)",
  #                     "Late (31-120 days)")
  if(strict){
    d1 = d0[d0$loan_status %in% c("Charged Off","Fully Paid","Default","Late (31-120 days)"),]
    d1$def = as.numeric(d1$loan_status %in% c("Charged Off","Default","Late (31-120 days)"))
  } else
  {
    d1 = d0[!d0$loan_status %in% c("In Grace Period"),] #c("Charged Off","Default","Fully Paid","Late (31-120 days)","Late (16-30 days)")  
    d1$def = as.numeric(d1$loan_status %in% c("Charged Off","Default","Late (31-120 days)","Late (16-30 days)"))
  }
  
  
  d1
}


preProcessRejects = function(d0){
  names(d0)[names(d0)=="Amount.Requested"] = "loan_amnt"
  
  names(d0)[names(d0)=="Application.Date"] = "issue_d"
  d0$issue_d = as.Date(d0$issue_d)
  
  names(d0)[names(d0)=="Zip.Code"] = "zip_code"
  names(d0)[names(d0)=="State"] = "addr_state"
  
  names(d0)[names(d0)=="Debt.To.Income.Ratio"] = "dti"
  d0$dti = as.numeric(gsub("%","",d0$dti))
  
  names(d0)[names(d0)=="Employment.Length"] = "emp_length" 
  d0$emp_length[d0$emp_length == "< 1 year"] = "0" 
  d0$emp_length[d0$emp_length == "10+ years"] = "10" 
  d0$emp_length = gsub("year|years","",d0$emp_length)
  d0$emp_length = as.numeric(d0$emp_length)
  
  names(d0)[names(d0)=="Loan.Title"] = "purpose" 
  d0$purpose = tolower(d0$purpose)
  d0$purpose[grep("medical",d0$purpose)] = "medical"
  d0$purpose[grep("business",d0$purpose)] = "small_business"
  d0$purpose[grep("moving",d0$purpose)] = "moving"
  d0$purpose[d0$purpose=="debt consolidation"] = "debt_consolidation"
  d0$purpose[d0$purpose=="major purchase"] = "major_purchase"
  d0$purpose[d0$purpose=="car financing"] = "car"
  d0$purpose[d0$purpose=="credit card refinancing"] = "credit_card"
  d0$purpose[intersect(grep("credit",d0$purpose),
                       grep("card",d0$purpose))] = "credit_card"
  
  d0$purpose[intersect(grep("home",d0$purpose),
                       grep("buying",d0$purpose))] = "house"
  d0$purpose[intersect(grep("home",d0$purpose),
                       grep("improvement",d0$purpose))] = "home_improvement"
  d0$purpose[grep("bath|kitchen|pool",d0$purpose)] = "home_improvement"
  d0$purpose[intersect(grep("car",d0$purpose),
                       grep("repair",d0$purpose))] = "car"
  d0$purpose[intersect(grep("auto",d0$purpose),
                       grep("repair",d0$purpose))] = "car"
  d0$purpose[grep("consolidat",d0$purpose)] = "debt_consolidation"
  
  
  d0$purpose[!d0$purpose %in% c("debt_consolidation","credit_card","other","home_improvement",
                                "house","small_business","medical","car","moving","major_purchase",
                                "vacation","renewable_energy")] = "other"
  
  names(d0)[names(d0)=="Policy.Code"] = "policy_code" 
  
  
  d0
}

dtMaker = function(x){
  mnths = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  for(i in 1:length(mnths)){
    x = gsub(mnths[i],i,x)
  }
  as.Date(paste0("01-",x),format = "%d-%m-%Y")
}

monthMaker = function(x){
  y = as.character(x)
  as.Date(paste0(substr(y,1,8),"01"))
}

qrtMaker = function(x){
  y = as.character(x)
  paste0(year(x),quarters(x))
}



adjPurpose = function(d0){
  # d0$purpose = tolower(d0$purpose)
  d0$purpose[grep("medical",d0$purpose)] = "medical"
  d0$purpose[grep("business",d0$purpose)] = "small_business"
  d0$purpose[grep("moving",d0$purpose)] = "moving"
  d0$purpose[d0$purpose=="debt consolidation"] = "debt_consolidation"
  d0$purpose[d0$purpose=="major purchase"] = "major_purchase"
  d0$purpose[d0$purpose=="car financing"] = "car"
  d0$purpose[d0$purpose=="credit card refinancing"] = "credit_card"
  d0$purpose[intersect(grep("credit",d0$purpose),
                       grep("card",d0$purpose))] = "credit_card"
  
  d0$purpose[intersect(grep("home",d0$purpose),
                       grep("buying",d0$purpose))] = "house"
  d0$purpose[intersect(grep("home",d0$purpose),
                       grep("improvement",d0$purpose))] = "home_improvement"
  d0$purpose[grep("bath|kitchen|pool",d0$purpose)] = "home_improvement"
  d0$purpose[intersect(grep("car",d0$purpose),
                       grep("repair",d0$purpose))] = "car"
  d0$purpose[intersect(grep("auto",d0$purpose),
                       grep("repair",d0$purpose))] = "car"
  d0$purpose[grep("consolidat",d0$purpose)] = "debt_consolidation"
  
  
  d0$purpose[!d0$purpose %in% c("debt_consolidation","credit_card","other","home_improvement",
                                "house","small_business","medical","car","moving","major_purchase",
                                "vacation","renewable_energy")] = "other"
  d0
}


perfEval = function(d1, timeField = "issue_d"){
  library(pROC)
  rocs = data.frame(fld = 0, gini = 0, nobs = 0, bads = 0, dr = 0, stringsAsFactors = F)[-1,]
  names(rocs) = c(timeField, "gini", "nobs","bads","dr")
  for(x in sort(unique(d1[[timeField]]))){
    rocs[nrow(rocs)+1,timeField] = x
    ind = (d1[[timeField]]==x)
    rocs[nrow(rocs),"gini"] = 2*roc(predictor = d1$score_norm[ind], 
                                    response = d1$def[ind])$auc - 1
    rocs[nrow(rocs),"nobs"] = sum(ind)
    rocs[nrow(rocs),"bads"] = sum(d1$def[ind])
  }
  rocs$dr = rocs$bads / rocs$nobs
  # rocs[[timeField]] = as.Date(rocs[[timeField]], origin = "1970-01-01")
  rocs
}


finRes = function(acc, rej, card, mdlCalib, mdlRBP, typeCutoff = c("AR","PNL","MANUAL")[2],
                  cutoff = 0, ar = 0.4, noise = 0, calcProfile = TRUE,
                  includeRBP = TRUE){
  
  acc$finResStatus = "accepted"
  rej$finResStatus = "rejected"
  
  acc = do.score(acc, card)
  rej = do.score(rej, card)
  # browser()
  # devScore = sd(c(acc$score_norm,rej$score_norm))
  
  acc$PD_true = predict(mdlCalib,newdata = acc, type = "response") 
  rej$PD_true = predict(mdlCalib,newdata = rej, type = "response")
  
  if(noise>0){
    acc$score_norm[sample(1:nrow(acc), size = noise*nrow(acc))] = acc$score_norm[sample(1:nrow(acc), size = noise*nrow(acc))]
    rej$score_norm[sample(1:nrow(rej), size = noise*nrow(rej))] = rej$score_norm[sample(1:nrow(rej), size = noise*nrow(rej))]
  }
  
  acc$PD = predict(mdlCalib,newdata = acc, type = "response") 
  rej$PD = predict(mdlCalib,newdata = rej, type = "response")
  
  acc$int_rate = predict(mdlRBP, newdata = acc, type = "vector")
  rej$int_rate = predict(mdlRBP, newdata = rej, type = "vector")  
  
  if(!includeRBP){
    rbpRate = mean(c(acc$int_rate,rej$int_rate))
    acc$int_rate = rbpRate
    rej$int_rate = rbpRate
  }
  
  
  acc$pnl = acc$def*(-acc$loan_amnt) + (1-acc$def)*acc$loan_amnt*acc$int_rate
  rej$pnl = rej$PD_true*(-rej$loan_amnt) + (1-rej$PD_true)*rej$loan_amnt*rej$int_rate
  
  rej$def = NA
  
  cmb = rbind(acc[,c("loan_amnt","score_norm","PD","PD_true","def","pnl","finResStatus")],
              rej[,c("loan_amnt","score_norm","PD","PD_true","def","pnl","finResStatus")])
  
  names(cmb) = c("loan_amnt","score_norm","PD","PD_true","def","pnl","finResStatus")
  
  h = hist(cmb$score_norm, breaks = 100, plot = F)
  
  tab = data.frame(cutoff = 0, AR = 0, PNL = 0, amount = 0,
                   PD_acc = 0, PD_rej = 0,
                   PD_true_acc = 0, PD_true_rej = 0,
                   score_acc = 0, 
                   score_rej = 0, 
                   # DR_acc = 0,
                   stringsAsFactors = F)[-1,]
  # browser()
  if(calcProfile){
    xrange = h$breaks
  } else
  {
    xrange = cutoff
  }
  
  for(x in xrange){
    print(x)
    tab[nrow(tab)+1,"cutoff"] = x
    tab[nrow(tab),"AR"] = sum(cmb$score_norm >= x)/nrow(cmb)
    tab[nrow(tab),"PNL"] = sum(cmb$pnl[cmb$score_norm >= x])
    tab[nrow(tab),"PD_acc"] = mean(cmb$PD[cmb$score_norm >= x])
    tab[nrow(tab),"PD_rej"] = mean(cmb$PD[cmb$score_norm < x])
    tab[nrow(tab),"PD_true_acc"] = mean(cmb$PD_true[cmb$score_norm >= x])
    tab[nrow(tab),"PD_true_rej"] = mean(cmb$PD_true[cmb$score_norm < x])
    tab[nrow(tab),"score_acc"] = mean(cmb$score_norm[cmb$score_norm >= x])
    tab[nrow(tab),"score_rej"] = mean(cmb$score_norm[cmb$score_norm < x])
    # tab[nrow(tab),"DR_acc"] = mean(cmb$def[cmb$score_norm >= x], na.rm = T)
    tab[nrow(tab),"amount"] = sum(cmb$loan_amnt[cmb$score_norm >= x])
  }
  
  
  if(typeCutoff == "PNL"){
    optCutoff = tab$cutoff[which.max(tab$PNL)]
  }
  if(typeCutoff == "AR"){
    optCutoff = tab$cutoff[tab$AR >= ar][which.min(tab$AR[tab$AR >= ar])]
  }
  if(typeCutoff == "MANUAL"){
    optCutoff = cutoff
  }
  
  
  return(list(pnlProfile = tab,
              optCutoff = optCutoff,
              pnl = tab$PNL[tab$cutoff == optCutoff],
              ar = tab$AR[tab$cutoff == optCutoff],
              amount = tab$amount[tab$cutoff == optCutoff],
              type = typeCutoff, 
              noise = noise,
              gini_acc = 2*roc(predictor = acc$score_norm,
                               response = acc$def)$auc-1)
  )
}


cardEval = function(data, card, target = "def"){
  x = do.score(data, card)
  gini.calc(x$score_norm, x[[target]], aggr = TRUE)$AR
}

calibModel = function(data, card, target = "def", predict = "score_norm"){
  clb = do.score(data, card)
  mdl = glm(as.formula(paste0(target,"~",predict)), data = clb, family = binomial(link = "logit"))
  mdl
}

calcFPFN = function(data, amt = 14739.99, rate = 0.127231, lgd = 0.45, targetFld = "def", predictFld = "pd", step = 0.025, quant = TRUE, 
                    alternative = numeric(0), subGradeK = 10){
  if(quant){
    hBreaks =  quantile(data[[predictFld]], 
                        probs = seq(0,1,step)) 
  } else
  {
    mx = max(data[[predictFld]])
    mn = min(data[[predictFld]])
    hBreaks = seq(mn, mx, (mx-mn)*step)
    hBreaks = c(mn,hBreaks,mx)
    hBreaks = sort(unique(hBreaks))
  }
  # browser()
  br = hist(data[[predictFld]],
            breaks = hBreaks, 
            plot = T)$breaks
  
  tab_model = calcProfile(data = data, br = br, predictFld = predictFld, target = predictFld, amt = amt, rate = rate, lgd = lgd, subGradeK = subGradeK)
  tab_real = calcProfile(data = data, br = br, predictFld = predictFld, target = targetFld, amt = amt, rate = rate, lgd = lgd, subGradeK = subGradeK)
  # browser()
  if(length(alternative)!=nrow(data)){
    return(list(model = tab_model, real = tab_real))
  } else
  {
    tab_alter = calcProfile(data = data, br = br, predictFld = alternative, target = predictFld, amt = amt, rate = rate, lgd = lgd, subGradeK = subGradeK)
    return(list(model = tab_model, real = tab_real, alter = tab_alter))
  }
  
}


calcProfile = function(data,br,predictFld = "pd", targetFld = "def", amt = 14739.99, rate = 0.127231, lgd = 0.45,
                       globalDefFld = "def", subGradeK = 10){
  if(class(predictFld)=="numeric" & length(predictFld)==nrow(data)){
    tmp = predictFld
    predictFld = "customFieldInCalcProfile"
    data[[predictFld]] = tmp
  }
  
  #take real flags where loans are granted:
  data[data$sub_grade_num <= subGradeK, targetFld] = data[data$sub_grade_num <= subGradeK, globalDefFld]
  
  tab = data.frame(cutoff = 0,
                   ar = 0,
                   fpr = 0,
                   fnr = 0,
                   tpr = 0,
                   tnr = 0,
                   fp_pnl = 0,
                   fn_pnl = 0,
                   total_amt = 0,
                   pnl = 0,
                   stringsAsFactors = F)[-1,]
  
  for(x in br){
    print(x)
    cutoff = x
    ar = sum(data[[predictFld]] <= cutoff) / nrow(data)
    fpr = (sum(data[[predictFld]] > cutoff) - sum(data[[targetFld]][data[[predictFld]] > cutoff]))/ (nrow(data) - sum(data[[targetFld]]))
    fnr = sum(data[[targetFld]][data[[predictFld]] <= cutoff]) / sum(data[[targetFld]])
    tpr = sum(data[[targetFld]][data[[predictFld]] > cutoff])/ sum(data[[targetFld]])
    tnr = (sum(data[[predictFld]] <= cutoff) - sum(data[[targetFld]][data[[predictFld]] <= cutoff])) / (nrow(data) - sum(data[[targetFld]]))
    
    fp_pnl = (sum(data[[predictFld]] > cutoff) - sum(data[[targetFld]][data[[predictFld]] > cutoff]))*amt*rate
    fn_pnl = sum(data[[targetFld]][data[[predictFld]] <= cutoff])*amt*lgd
    
    total_amt = sum(data[[predictFld]] <= cutoff)*amt
    
    pnl = (sum(data[[predictFld]] <= cutoff) - sum(data[[targetFld]][data[[predictFld]] <= cutoff]))*amt*rate - sum(data[[targetFld]][data[[predictFld]] <= cutoff])*amt*lgd  
    
    tab[nrow(tab)+1,] = c(cutoff,
                          ar,
                          fpr,
                          fnr,
                          tpr,
                          tnr,
                          fp_pnl,
                          fn_pnl,
                          total_amt,
                          pnl)
  }
  tab
}
