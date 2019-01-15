library(R.utils)
library(LaF)
library(data.table)
library(rpart)
library(rpart.plot)
library(SberPDVal)
library(sqldf)
library(pROC)

setwd("C:\\Users\\aamas\\Documents\\Статья по модельному риску\\")

list.files()

# fileName = "LoanStats3d.csv"
fileName = "LoanStats_2017Q1.csv"

source("reading funcs.R")
source("WOER.R")


d1 = readLendFile(fileName = fileName, readAll = F,mult = 8, k = 10, type = "accepted")
d1 = preProcess(d1)
# d1$issue_d = dtMaker(d1$issue_d)


rej = readLendFile(fileName = "RejectStatsD.csv", k = 20, mult = 100, readAll = F, type = "rejected")
rej1 = preProcessRejects(rej)

# regressors = c("loan_amnt","purpose","dti","addr_state","emp_length")#,"policy_code")
regressors = c("purpose","addr_state","grade","home_ownership",
               "dti","pti","funded_amnt","annual_inc_joint","amount2term","emp_length","term","inq_last_6mths","mths_since_recent_inq",
               "delinq_2yrs","chargeoff_within_12_mths","num_accts_ever_120_pd","num_tl_90g_dpd_24m","acc_open_past_24mths","avg_cur_bal","tot_hi_cred_lim","delinq_amnt")



myCard = make.scorecard(d = d1[,c(regressors,"def")],target = "def",
                        varsBining = data.frame(var = c("emp_length","purpose","addr_state","loan_amnt"),
                                                bins = c(2,10,20,3),
                                                stringsAsFactors = F))

myCard = make.scorecard(d = d1[,c(allVars,"def")],target = "def"
                        # varsBining = data.frame(var = c("emp_length","purpose","addr_state","loan_amnt"),
                                                # bins = c(2,10,20,3),
                                                # stringsAsFactors = F)
                        )

rej1 = do.score(rej1,myCard)
View(rej1[is.na(rej1$score_norm),])
d1 = d1[,-c((which(names(d1)=="def")+1):ncol(d1))]
d1 = do.score(d1,myCard)
unique(d1$issue_d)


mdl = train_glm_models(TEST_mode = FALSE, target_field = "def", 
                       data_train = d3, 
                       data_test = NULL, 
                       data_score = d3, min_bin_rate = c(0.05, 0.35), 
                       bin_monotone = 1,
                       min_model_factors_rate = c(0.5, 1),
                       min_gini_threshlod = 0.05, 
                       model_vars_z_sign_threshold = 0.05,
                       tune_vars_z_sign_threshold = FALSE, 
                       models_create_num = 1, core_num = 3, verbose = FALSE,
                       learner_object = NULL, parset_object = NULL)





allVars =  c("loan_amnt","funded_amnt","funded_amnt_inv","installment","dti","delinq_2yrs","open_acc","inq_last_6mths",
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
             "orig_projected_additional_accrued_interest","hardship_payoff_balance_amount","hardship_last_payment_amount","debt_settlement_flag","settlement_amount",
             "purpose", "addr_state", "emp_length","grade","home_ownership","term")


allVars = allVars[!allVars %in% c("total_pymnt","total_pymnt_inv","total_rec_int","total_rec_late_fee",
                  "total_rec_prncp","recoveries",
                  "debt_settlement_flag","debt_settlement_flag_date",
                  "settlement_status","settlement_date","settlement_amount","settlement_percentage","settlement_term",
                  "out_prncp_inv","out_prncp","collection_recovery_fee","last_pymnt_amnt")]  


d = read.table("LoanStatsWebArchive.csv", sep = ",",quote = '"', skip =1 , header = T, nrows = 326771, stringsAsFactors = F)
rej0712 = readLendFile(fileName = "RejectStatsA.csv", k = 3, mult = 4, readAll = F, type = "rejected")
rej1 = preProcessRejects(rej0712)
rej1 = rej1[!is.na(rej1$Risk_Score),]
rej1 = rej1[rej1$Risk_Score!=0,]
rej1314 = readLendFile(fileName = "RejectStatsB.csv", k = 8, mult = 50, readAll = F, type = "rejected")
rej2 = preProcessRejects(rej1314)

rej0714 = rbind(rej1,rej2)

# nrow(rej0714)



names(d)
table(d$Status)
summary(as.Date(d$Application.Date))
dnew = d[d$Status %in% c("Charged Off","Default","Fully Paid","Current","Late (31-120 days)"),]
dnew = dnew[!(dnew$Status == "Current" & as.Date(dnew$Application.Date) >= as.Date("2012-10-01")),]
dnew$def = ifelse(as.numeric(dnew$Status %in% c("Charged Off","Default","Late (31-120 days)")),1,0)

dnew = dnew[,c("Total.Amount.Funded","Application.Date","Loan.Purpose","FICO.Range",
               "Debt.To.Income.Ratio","State","Employment.Length","Interest.Rate","def")]

names(dnew) = c("loan_amnt","issue_d","purpose","Risk_Score","dti","addr_state","emp_length","int_rate","def")
rej0714 = rej0714[,c("loan_amnt","issue_d","purpose","Risk_Score","dti","addr_state","emp_length")]
rej0714$month = month(rej0714$issue_d)
rej0714$qrt = qrtMaker(rej0714$issue_d)
rej0714 = rej0714[!is.na(rej0714$addr_state),]
rej0714 = rej0714[rej0714$addr_state!="ND",]
rej0714 = rej0714[rej0714$addr_state!="",]

ll = strsplit(dnew$Risk_Score,split = "-")
ll = lapply(ll, function(s)mean(as.numeric(c(s[1],s[2]))))
dnew$Risk_Score = unlist(ll)

dnew$dti = as.numeric(gsub("%","",dnew$dti))/100
dnew$emp_length[dnew$emp_length == "10+ years"] = "10"
dnew$emp_length[dnew$emp_length == "< 1 year"] = "0"
dnew$emp_length = gsub("years|year","",dnew$emp_length)
dnew$emp_length = as.numeric(dnew$emp_length)
dnew$issue_d = monthMaker(dnew$issue_d)
dnew$qrt = qrtMaker(dnew$issue_d)
dnew$month = month(dnew$issue_d)
dnew$int_rate = as.numeric(gsub("%","",dnew$int_rate))/100

myCard = make.scorecard(d = dnew[,!names(dnew)%in% c("issue_d", "qrt","int_rate")],target = "def",
                        n_WOE = 50,
                        varsBining = data.frame(var = c("emp_length","purpose","addr_state","loan_amnt","Risk_Score", "month","dti"),
                        bins = c(1,14,50,5,10,12,6),
                        stringsAsFactors = F)
)


# dnew = dnew[,-c((which(names(dnew)=="def")+1):ncol(dnew))]
# dnew = dnew[,names(dnew)[c(1:8,10,9)]]
dnew = do.score(dnew,myCard)

#Calibration
mdlCalib = glm(def~score_norm, data = dnew, family = binomial(link = "logit"))
# RBP
mdlRBP = rpart(int_rate~score_norm,data = dnew, control = rpart.control(minbucket = 100, maxdepth = 5, cp = 0.001))

giniDynamics = perfEval(dnew[dnew$qrt!="2007Q2",], timeField = "qrt")




x = finRes(acc = dnew, rej = rej0714, card = myCard, mdlCalib = mdlCalib, mdlRBP = mdlRBP)
myCutoff = x$optCutoff

noises = seq(0,1,0.05)

library(doParallel)
cl = makeCluster(2)
registerDoParallel(cl = cl)

ll3 = foreach(s = noises, .combine = rbind, .export = c("dnew","rej0714","myCard","mdlRBP","myCutoff","finRes","roc")) %dopar% {
  print(s)
  l = finRes(acc = dnew[!dnew$qrt %in% c("2012Q4","2013Q1","2013Q2","2013Q3","2013Q4","2014Q1","2014Q2","2014Q3","2014Q4"),],
             rej = rej0714[!rej0714$qrt %in% c("2012Q4","2013Q1","2013Q2","2013Q3","2013Q4","2014Q1","2014Q2","2014Q3","2014Q4"),], 
             card = myCard, mdlCalib = mdlCalib, mdlRBP = mdlRBP, 
             noise = s,calcProfile = FALSE, typeCutoff = "MANUAL",cutoff = myCutoff, includeRBP = TRUE)
  l[-1]
}
  
stopCluster(cl)

res = as.data.frame(ll3, stringsAsFactors = F)
plot(x = res$gini_acc, y = res$pnl, type = "l")

# names(rej0714)

hist2(x = do.score(dnew,myCard)$score_norm,
      y = do.score(rej0714,myCard)$score_norm,
      graph.dir = "C:/Users/aamas/Documents/Статья по модельному риску",
      main = "Accepted vs Rejected by Model Score",
      less.bin = 2,
      name = "rej vs acc") 

# dnew$qrt = qrtMaker(dnew$issue_d)
# dnew = do.score(dnew, myCard)
# rej0714 = do.score(rej0714, myCard)
# rej0714$PD = predict(mdlCalib,newdata = rej0714, type = "response")

sqldf("select qrt, count(*) as cnt, avg(def) as DR, 'accepted' as type from dnew group by qrt
      union all 
      select qrt, count(*) as cnt, avg(PD) as DR, 'rejected' as type from rej0714 group by qrt")



sqldf("select qrt, sum(loan_amnt) as amount, avg(int_rate) as rate, sum((1-def)*2*int_rate*loan_amnt - def*loan_amnt) as pnl
      from dnew
      group by qrt")

l1 = finRes(acc = dnew[!dnew$qrt %in% c("2012Q4","2013Q1","2013Q2","2013Q3","2013Q4","2014Q1","2014Q2","2014Q3","2014Q4"),],
            rej = rej0714[!rej0714$qrt %in% c("2012Q4","2013Q1","2013Q2","2013Q3","2013Q4","2014Q1","2014Q2","2014Q3","2014Q4"),], 
            card = myCard, mdlCalib = mdlCalib, mdlRBP = mdlRBP, 
            noise = 0,calcProfile = TRUE, typeCutoff = "PNL",cutoff = myCutoff, includeRBP = TRUE)

l2 = finRes(acc = dnew[!dnew$qrt %in% c("2012Q4","2013Q1","2013Q2","2013Q3","2013Q4","2014Q1","2014Q2","2014Q3","2014Q4"),],
            rej = rej0714[!rej0714$qrt %in% c("2012Q4","2013Q1","2013Q2","2013Q3","2013Q4","2014Q1","2014Q2","2014Q3","2014Q4"),], 
            card = myCard, mdlCalib = mdlCalib, mdlRBP = mdlRBP, 
            noise = 0.1,calcProfile = TRUE, typeCutoff = "PNL",cutoff = myCutoff, includeRBP = TRUE)

l3 = finRes(acc = dnew[!dnew$qrt %in% c("2012Q4","2013Q1","2013Q2","2013Q3","2013Q4","2014Q1","2014Q2","2014Q3","2014Q4"),],
            rej = rej0714[!rej0714$qrt %in% c("2012Q4","2013Q1","2013Q2","2013Q3","2013Q4","2014Q1","2014Q2","2014Q3","2014Q4"),], 
            card = myCard, mdlCalib = mdlCalib, mdlRBP = mdlRBP, 
            noise = 0.2,calcProfile = TRUE, typeCutoff = "PNL",cutoff = myCutoff, includeRBP = TRUE)
