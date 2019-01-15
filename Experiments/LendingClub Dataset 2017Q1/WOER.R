WOER<-function(x,target,n=20,k=5,interval=TRUE, override=FALSE, nomUniq = 6) { 
  if(length(x)!=length(target)){warning("diff lengths") 
    stop}
  
  # Check for override
  if(override==FALSE){
    if(length(unique(x))<= nomUniq || class(x)=="character" || class(x)=="factor"){
      interval<-FALSE
      x<-as.character(x)
    }
    else {interval<- TRUE}
  }
  
  # Initialize empty WOE_x variable
  WOE_x<-rep(NA,length(x))
  
  # Drop observations with non-definied target (the order of statements is crucial)
  x<-x[!is.na(target)]
  target<-target[!is.na(target)]
  
  
  ngoods<-length(target[target==0])
  nbads<-length(target[target==1])
  
  if(any(!is.na(x))) { 
    if(interval==TRUE){
      x<-as.numeric(as.character(x))
      
      
      # Initial partition (note, that na.rm=TRUE so as not to fail if any missings)
      ticks<-unique(c(-Inf,quantile(x,probs=seq(from = 0, to =1, by = 1/n ),na.rm=TRUE)[2:n-1],Inf))
      repeat{
        t<-length(ticks)-1
        bads<-rep(NA,t)
        goods<-rep(NA,t)
        WOE<-rep(NA,t)
        for(i in 1:t) {
          bads[i]<-sum(target[x>ticks[i] & x<=ticks[i+1] & !is.na(x)]) 
          goods[i]<-length(target[x>ticks[i] & x<=ticks[i+1] & !is.na(x)]) - bads[i]
        }
        
        if(length(which(goods+bads==0))){excl<-which(goods+bads==0)
        WOE<-WOE[-excl]
        ticks<-ticks[-excl]
        t<-length(ticks)-1
        bads<-bads[-excl]
        goods<-goods[-excl]
        }
        
        WOE<-log((goods/bads)*(nbads/ngoods))
        if(any(!is.finite(WOE))){WOE<-log(((goods+0.5)/ngoods)/((bads+0.5)/nbads))}
        #   We exit the proc here, because on this step we have updated WOEs for the latest x partition 
        if(length(WOE)<=k){
          WOE_frame<-data.frame(group=paste("(",ticks[-length(ticks)],"; ",ticks[-1],"]",sep=""),lb=ticks[-length(ticks)],rb=ticks[-1],goods=goods,bads=bads,WOE=WOE)
          vallist<-list()
          break}
        #   Just in case if you are interested in the smallest diff you can uncomment the following line
        #   diff<-min(abs(WOE - c(NA,WOE[-length(WOE)])),na.rm=TRUE) 
        i0<-which.min(abs(WOE - c(NA,WOE[-length(WOE)])))
        #   By this line we kind of merge intervals
        ticks<-ticks[-i0]
      }
      
      # WOE over final partition is calculated
      for(i in 1:length(ticks)-1) { 
        WOE_x[x>ticks[i] & x<=ticks[i+1] & !is.na(x)]<-WOE[i]
      }
      
    }
    else {
      
      # vallist<-sapply(unique(x,na.rm=TRUE),function(x){list(x)})
      
      vallist <- data.frame(category=as.character(unique(x[!is.na(x)])),group=c(1:length(unique(x[!is.na(x)]))))
      
      repeat {
        
        WOE_frame<-data.frame(group=unique(vallist$group),
                              goods=rep(0,length(unique(vallist$group))),
                              bads=rep(0,length(unique(vallist$group))),
                              WOE=rep(NA,length(unique(vallist$group)))            
        )
        
        # m is the category from the aggregated group of categories c 
        # vallist is a list of lists
        for(gr in WOE_frame$group){
          bads<-0
          goods<-0  
          for(categ in vallist$category[vallist$group==gr]){
            bads <- bads + sum(target[x==categ & !is.na(x)])
            goods <- goods + length(target[x==categ & !is.na(x) & target==0])
          }    
          WOE_frame$goods[WOE_frame$group==gr] <-goods
          WOE_frame$bads[WOE_frame$group==gr] <-bads
        } 
        
        WOE_frame$WOE<-log((WOE_frame$goods/ngoods)/(WOE_frame$bads/nbads))
        if(any(!is.finite(WOE_frame$WOE))){WOE_frame$WOE<-log(((WOE_frame$goods+0.5)/ngoods)/((WOE_frame$bads+0.5)/nbads))}
        
        if(nrow(WOE_frame)<=k){break}
        
        # Find the two most close aggregated groups of categories (close in terms of WOE)
        diff<-Inf
        for(i in 1:(length(WOE_frame$WOE)-1)){
          for(j in (i+1):length(WOE_frame$WOE)){
            if(abs(WOE_frame$WOE[i]-WOE_frame$WOE[j])<diff){
              diff<-abs(WOE_frame$WOE[i]-WOE_frame$WOE[j])
              gr_i<-WOE_frame$group[i]
              gr_j<-WOE_frame$group[j]
            }
          }
        }
        # Merge these groups
        vallist$group[vallist$group==gr_j]<-gr_i
        #vallist[i0]<- paste(vallist[i0],vallist[j0],sep=";")
        # vallist[length(vallist)+1]<-list(vallist[i0],vallist[j0])
        # vallist<-vallist[-max(i0,j0)]
        # vallist<-vallist[-min(i0,j0)]
      }
      
      
      # calculate the final WOE variable, after quitting the mergeing phase
      for(categ in vallist$category){
        WOE_x[x==categ & !is.na(x)] <-WOE_frame$WOE[WOE_frame$group==vallist$group[vallist$category==categ]]      
      }
      
      #changing the output
      WOE_frame$group<-unlist(lapply(as.list(WOE_frame$group),function(x){
        paste(vallist$category[vallist$group==x],collapse="===",sep="")
      }))
      
      
    }
    
  }
  # Calculating WOE for missing values if any
  if(any(is.na(x))){
    # browser()
    bads_NA<-sum(target[is.na(x)])
    goods_NA<-length(target[is.na(x)]) - bads_NA
    WOE_NA <-ifelse(is.finite(log((goods_NA/ngoods)/(bads_NA/nbads))),log((goods_NA/ngoods)/(bads_NA/nbads)),log(((goods_NA+0.5)/ngoods)/((bads_NA+0.5)/nbads)))
    
    
    if(exists("WOE_frame")){
      WOE_frame<-rbind(WOE_frame,c(NA,NA,NA,goods_NA,bads_NA,WOE_NA))
      WOE_x[is.na(x)]<-WOE_NA}
    else{
      WOE_frame<-data.frame(group=NA,NA,NA,goods=goods_NA,bads=bads_NA,WOE=WOE_NA)
      WOE_x<-rep(WOE_NA,length(WOE_x))
      vallist<-list()
    }  
  }
  
  WOE_frame$N = WOE_frame$goods + WOE_frame$bads
  WOE_frame$DR = WOE_frame$bads / WOE_frame$N
  attr(WOE_frame,"type") = ifelse(interval,"interval","nominal")
  list(WOE_x,WOE_frame)
  #list(WOE_x,vallist,WOE_frame)
  
  
}


make.scorecard<-
  function(d,target,n_WOE=20,k_WOE=5,interval=TRUE,override=FALSE,corr.threshold=0.8,pval.threshold=NULL,number.of.vars=12,
           varsBining = data.frame(var = "", bins = 2,stringsAsFactors = F)[-1,]){
    
    d[,target]<-as.numeric(as.character(d[,target]))
    sep.cols<-lapply(1:ncol(d),function(j)unlist(d[,j]))
    WOE_fr<-d
    WOE_res<-list()
    
    i<-1
    for(elem in sep.cols){
      print(names(WOE_fr)[i])
      if(names(WOE_fr)[i] %in% varsBining$var){
        k_WOE_custom = varsBining$bins[varsBining$var==names(WOE_fr)[i]]
        WOE_res[names(WOE_fr)[i]]<- list(WOER(unlist(elem),d[,target],interval=TRUE,override=FALSE,n=n_WOE,k=k_WOE_custom))  
      } else
      {
        WOE_res[names(WOE_fr)[i]]<- list(WOER(unlist(elem),d[,target],interval=TRUE,override=FALSE,n=n_WOE,k=k_WOE))  
      }
      WOE_fr[,i]<-WOE_res[[i]][[1]]
      i<-i+1                   
    }
    
    
    #sep.cols<-lapply(1:ncol(WOE_fr),function(j)unlist(WOE_fr[,j]))
    IV_fr<-data.frame(matrix(NA,nrow=0,ncol=2))
    names(IV_fr)<-c("variable","IV")
    
    for(j in 1:ncol(WOE_fr)){
      if(names(WOE_fr[j])!=target){
        IV_fr[nrow(IV_fr)+1,"variable"]<-names(WOE_fr)[j]
        IV_fr[nrow(IV_fr),"IV"]<-IV(WOE_fr[,j],d[,target])
      }
      #    j<-j+1 #?!?!?!?!?!?
    }
    IV_fr<-IV_fr[order(IV_fr$IV,decreasing = TRUE),]
    WOE_fr[,target]<-d[,target]
    
    Mcorr<-matrix(nrow = nrow(IV_fr), ncol = nrow(IV_fr))
    
    for(i in 1:(nrow(IV_fr)-1)){
      Mcorr[i,i]<-NA
      for(j in (i+1):nrow(IV_fr)){
        Mcorr[i,j]<-cor(WOE_fr[,IV_fr[i,1]],WOE_fr[,IV_fr[j,1]])
        Mcorr[j,i]<-Mcorr[i,j]  
      }
    }
    Mcorr[nrow(Mcorr),ncol(Mcorr)]<-NA
    
    i0<-0
    regressors<-character(0)
    repeat{
      #take the desired number (number.of.vars) of uncorrelated regressors 
      for(i in (i0+1):nrow(IV_fr)){ #(i0+1) is okay even though i0 can be equal to nrow(IV_fr) 
        #at the end of repeat{} this is checked in the break statement
        if(!(any(which(abs(Mcorr[i,])>corr.threshold)<i))){
          regressors[length(regressors)+1]<-IV_fr$variable[i]
        }
        if(length(regressors)>=number.of.vars){i0<-i
        break}
      }
      
      #run the logit regression
      scfrm<-paste(target, " ~ ", paste(regressors,collapse = " + "))
      logit<-glm(scfrm, family =binomial(link="logit"), data=WOE_fr)
      sm<-summary(logit)
      
      #exclude regressors with positive coefs
      # browser()
      if(any(logit$coefficients[-1]>0)){
        regressors<-regressors[-which(logit$coefficients[-1]>0)]
      }
      
      #if in concern with significancy then exclude with high pvalue
      if(!is.null(pval.threshold)){
        repeat{
          if(max(sm$coefficients[,"Pr(>|z|)"][-1])>pval.threshold){
            regressors<-regressors[-which.max(sm$coefficients[,"Pr(>|z|)"][-1])]
            scfrm<-paste(target, " ~ ", paste(regressors,collapse = " + "))
            logit<-glm(scfrm, family =binomial(link="logit"), data=WOE_fr)
            sm<-summary(logit)
          }
          else{
            break
          }
        }
      }
      #if current model consists of sufficient number of regressors or 
      #all variables are analyzed then exit repeat{}
      #else include other (if any) regressors 
      if(length(regressors)>=number.of.vars | i>=nrow(IV_fr)){break}
    }
    
    coefs<-logit$coefficients
    #regressors<-names(coefs[-1])
    
    scorecard<-list()
    for(x in regressors){
      scorecard[[x]]<-WOE_res[[x]][[2]]
      attr(scorecard[[x]],"coef")<-coefs[names(coefs)==x]
      attr(scorecard[[x]],"pval")<-round(sm$coefficients[,"Pr(>|z|)"][x],4)
    }
    
    attr(scorecard,"cons")<-coefs[1]
    scorecard
    
  }




do.score<-
  function(data,scorecard){
    
    var.score<-data.frame(matrix(NA,nrow=nrow(data),ncol=length(scorecard)))
    
    min_score<-sum(unlist(lapply(scorecard, function(x){min(x$WOE*attr(x,"coef"))})))+attr(scorecard,"cons")
    max_score<-sum(unlist(lapply(scorecard, function(x){max(x$WOE*attr(x,"coef"))})))+attr(scorecard,"cons")
    
    
    regressors<-names(scorecard)
    names(var.score)<-paste("scr_",regressors,sep="")
    
    
    
    for(var in regressors){
      
      scr_var<-paste("scr_",var,sep="")
      
      var.card<-scorecard[[var]]
      # if(ncol(var.card)==4){type<-"nominal" 
      type = attr(var.card,"type")
      if(type == "nominal"){
        data.var<-as.character(data[,var])
      }
      if(type == "interval"){
        data.var<-as.numeric(as.character(data[,var]))
      }
      coef<-attr(scorecard[[var]],"coef")
      
      if(type=="interval"){  
        for(i in 1:nrow(var.card)){
          if(!is.na(var.card[i,1])){
            var.score[ifelse(is.na(var.card$lb[i]<data.var & data.var<=var.card$rb[i]),FALSE,var.card$lb[i]<data.var & data.var<=var.card$rb[i]),scr_var]<- var.card$WOE[i]*coef
          }
          else
          {
            var.score[is.na(data.var),scr_var]<-var.card$WOE[i]*coef
          }
        }
      }
      if(type=="nominal"){
        for(i in 1:nrow(var.card)){
          categs<-unlist(strsplit(var.card[i,1],split="==="))
          if(!is.na(var.card[i,1])){
            var.score[ifelse(is.na(data.var%in%categs),FALSE,data.var%in%categs),scr_var]<- var.card$WOE[i]*coef
          }
          else
          {
            var.score[is.na(data.var),scr_var]<-var.card$WOE[i]*coef
          }
        }
      }
    }
    
    
    var.score$scorecard_points <- rowSums(var.score[,1:length(scorecard)])+ attr(scorecard,"cons")
    var.score$score_norm<-1000-1000*(var.score$scorecard_points-min_score)/(max_score-min_score)
    
    cbind(data,var.score)
    
  }

IV<-function(x,target){
  # x is supposed to be a WOE-transofrmed input with no NAs
  x<-x[!is.na(target)]
  target <- target[!is.na(target)]
  #   fr<-data.frame(x=x,target=target)
  
  nbad<-sum(target)
  ngood<-length(target)-nbad
  
  xq<-unique(x)
  if(length(xq)>=20){warning("number of bins in x variable >=20")}
  res<-0
  for(elem in xq){
    bads<-sum(target[x==elem])
    goods<-(length(target[x==elem]) - sum(target[x==elem]))
    res<-res+(goods/ngood-bads/nbad)*log(((goods+0.5)/ngood)/((bads+0.5)/nbad))
  }
  
  res
}

calibrate = function(d, scoreField, defField){
  mdl = glm(formula = as.formula(paste0(defField,"~",scoreField)), data = d)
  d$PD = predict(mdl,newdata = d, type = "response")
  d
}



cardEval = function(data, card, target = "def"){
  x = do.score(data, card)
  library(SberPDVal)
  gini.calc(x$score_norm, x[[target]], aggr = TRUE)$AR
}

calibModel = function(data, card, target = "def", predict = "score_norm"){
  clb = do.score(data, card)
  mdl = glm(as.formula(paste0(target,"~",predict)), data = clb, family = binomial(link = "logit"))
  mdl
}

calcFPFN = function(data, amt = 14739.99, rate = 0.127231, lgd = 0.45, targetFld = "target_14_20",
                    predictFld = "pd", step = 0.025, quant = TRUE, 
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
  }
  # browser()
  # br = hist(data[[predictFld]],
  #           breaks = hBreaks, 
  #           plot = T)$breaks
  hBreaks = sort(unique(hBreaks))
  br = hBreaks
  
  # tab_model = calcProfile(data = data, br = br, predictFld = predictFld, target = predictFld, amt = amt, rate = rate, lgd = lgd, subGradeK = subGradeK)
  tab_real = calcProfile(data = data, br = br, predictFld = predictFld, target = targetFld, amt = amt, rate = rate, lgd = lgd, subGradeK = subGradeK)
  
  # if(length(alternative)!=nrow(data)){
  #   return(list(model = tab_model, real = tab_real))
  # } else
  # {
  #   tab_alter = calcProfile(data = data, br = br, predictFld = alternative, target = predictFld, amt = amt, rate = rate, lgd = lgd, subGradeK = subGradeK)
  #   return(list(model = tab_model, real = tab_real, alter = tab_alter))
  # }
  
}


calcProfile = function(data,br,predictFld = "pd", targetFld = "target_14_20", amt = 14739.99, rate = 0.127231, lgd = 0.45,
                       globalDefFld = "target_14_20", subGradeK = 10){
  # if(class(predictFld)=="numeric" & length(predictFld)==nrow(data)){
  #   tmp = predictFld
  #   predictFld = "customFieldInCalcProfile"
  #   data[[predictFld]] = tmp
  # }
  
  #take real flags where loans are granted:
  # data[data$sub_grade_num <= subGradeK, targetFld] = data[data$sub_grade_num <= subGradeK, globalDefFld]
  
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
    # print(x)
    cutoff = x
    ar = sum(data[[predictFld]] <= cutoff) / nrow(data)
    fpr = (sum(data[[predictFld]] > cutoff) - sum(data[[targetFld]][data[[predictFld]] > cutoff]))/ (nrow(data) - sum(data[[targetFld]]))
    fnr = sum(data[[targetFld]][data[[predictFld]] <= cutoff]) / sum(data[[targetFld]])
    tpr = sum(data[[targetFld]][data[[predictFld]] > cutoff])/ sum(data[[targetFld]])
    tnr = (sum(data[[predictFld]] <= cutoff) - sum(data[[targetFld]][data[[predictFld]] <= cutoff])) / (nrow(data) - sum(data[[targetFld]]))
    
    fp_pnl = (sum(data[[predictFld]] > cutoff) - sum(data[[targetFld]][data[[predictFld]] > cutoff]))*amt*rate
    fn_pnl = sum(data[[targetFld]][data[[predictFld]] <= cutoff])*amt*lgd
    
    total_amt = sum(data[[predictFld]] <= cutoff)*amt
    
    # pnl = (sum(data[[predictFld]] <= cutoff) - sum(data[[targetFld]][data[[predictFld]] <= cutoff]))*amt*rate - sum(data[[targetFld]][data[[predictFld]] <= cutoff])*amt*lgd  
    pnl = sum(((1-data[[targetFld]])*amt*rate - data[[targetFld]]*amt*lgd)[data[[predictFld]] <= cutoff])
    
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
