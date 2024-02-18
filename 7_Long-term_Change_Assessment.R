

## Script to run nonparametric trend analysis on all ponds with depth 


## Code accompanying "Satellite imagery as a management tool for monitoring water clarity across freshwater 
## ponds in Cape Cod, Massachusetts" (Coffer et al., 2024, Journal of Environmental Management). 
## R code written by first author Megan Coffer.


## Load required packages 
require(lubridate)
require(rkt)
require(stringr)


## Set main working directory 
main.dir <- "..."


## Redefine rkt function to also compute standard deviation of sen
redefine.rkt <- function (date, y, block, cv, correct = F, rep = "e"){
  ans <- list(sl = NA, S = NA, B = NA, B.SD = NA, varS = NA, sl.corrected = NA, 
              varS.corrected = NA, partial.S = NA, partial.sl = NA, 
              partial.varS = NA, partial.sl.corrected = NA, partial.varS.corrected = NA, 
              tau = NA)
  oldClass(ans) <- "rkt"
  if (missing(date) | missing(y)) {
    cat("Error: first mandatory vector should be positive: year or year+fraction\n")
    cat("       second mandatory vector should be numerical: measured data (ties allowed)\n")
    cat("       third optional vector should be positive integer: season, month, site or a unique code for season and site\n")
    cat("       fourth optional vector should be numerical: covariable\n")
    return(ans)
  }
  if (missing(block)) 
    block <- replicate(length(date), 1)
  a <- search()
  ok = 0
  if (!is.numeric(date) | !is.numeric(block) | !is.numeric(y) | 
      length(date) < 4 | length(y) != length(block) | length(y) != 
      length(date) | min(date) < 0) {
    cat("Error: first mandatory vector should be positive: year or year+fraction\n")
    cat("       second mandatory vector should be numerical: measured data (ties allowed)\n")
    cat("       third optional vector should be positive integer: season, month, site or a unique code for season and site\n")
    cat("       fourth optional vector should be numerical: covariable\n")
    cat("       all with the same length, at least 4\n")
    return(ans)
  }
  if (!missing(cv)) {
    if (!is.numeric(cv) | length(cv) != length(date)) {
      cat("Error: fourth vector should be numerical: covariable\n")
      cat("       with the same length as the others\n")
      return(ans)
    }
  }
  block <- trunc(block)
  if (min(block) < 1) {
    cat("Error: only positive values accepted for blocks\n")
    return(ans)
  }
  block <- trunc(block)
  if (correct) 
    date <- trunc(date)
  if (!missing(cv)) {
    date = date[!is.na(cv)]
    block = block[!is.na(cv)]
    cv = cv[!is.na(cv)]
    y = y[!is.na(cv)]
  }
  if (rep == "a") {
    for (i in 1:length(date)) {
      date_ties <- y[date == date[i] & block == block[i] & 
                       !is.na(y)]
      if (!missing(cv)) {
        cv_ties <- cv[date == date[i] & block == block[i] & 
                        !is.na(y)]
      }
      if (length(date_ties) > 1) {
        y[date == date[i] & block == block[i]] <- NA
        y[i] <- mean(date_ties[!is.na(date_ties)])
        if (!missing(cv)) {
          cv[date == date[i] & block == block[i]] <- NA
          cv[i] <- mean(cv_ties[!is.na(date_ties)])
        }
      }
    }
  }
  if (rep == "m") {
    for (i in 1:length(date)) {
      date_ties <- y[date == date[i] & block == block[i] & 
                       !is.na(y)]
      if (!missing(cv)) {
        cv_ties <- cv[date == date[i] & block == block[i] & 
                        !is.na(y)]
      }
      if (length(date_ties) > 1) {
        y[date == date[i] & block == block[i]] <- NA
        y[i] <- median(date_ties[!is.na(date_ties)])
        if (!missing(cv)) {
          cv[date == date[i] & block == block[i]] <- NA
          cv[i] <- median(cv_ties[!is.na(date_ties)])
        }
      }
    }
  }
  date = date[!is.na(y)]
  block = block[!is.na(y)]
  if (!missing(cv)) 
    cv = cv[!is.na(y)]
  y = y[!is.na(y)]
  for (i in max(block):1) {
    if (length(block[block == i]) == 0) {
      block[block >= i] <- block[block >= i] - 1
    }
  }
  tau <- NA
  maxblock <- max(block)
  minyear = min(date)
  ny <- max(date) - minyear + 1
  S <- 0
  Scv <- 0
  varS <- 0
  pc <- NA
  varSc <- NA
  p.S <- NA
  p.varS <- NA
  p.p <- NA
  varScv <- 0
  p.pc <- NA
  p.varSc <- NA
  sen <- date[0]
  varmix <- 0
  kd = split(date, block)
  ky = split(y, block)
  for (b in 1:maxblock) {
    kkd = kd[[b]]
    kky = ky[[b]]
    if (length(kkd) > 3) {
      for (i1 in 2:length(kkd)) {
        for (i2 in 1:(i1 - 1)) {
          sen <- c(sen, (kky[i2] - kky[i1])/(kkd[i2] - 
                                               kkd[i1]))
          S <- S + sign1((kky[i2] - kky[i1])/(kkd[i2] - 
                                                kkd[i1]))
        }
      }
      nd <- length(kky[!is.na(kky)])
      tn <- length(kky[is.na(kky)])
      SumTies <- 0
      for (i1 in 1:length(kky)) {
        if (!is.na(kky[i1])) {
          ties <- length(kky[kky == kky[i1]]) - tn
          SumTies <- SumTies + (ties - 1) * (2 * ties + 
                                               5)
        }
      }
      varS <- varS + nd * (nd - 1) * (2 * nd + 5)/18 - 
        SumTies/18
    }
    else cat("1 block with less than 4 points ignored\n")
  }
  B <- median(sen)
  B.SD <- sd(sen)
  ncomp <- length(sen)
  if (length(sen[sen == Inf]) > 0 | length(sen[is.nan(sen)]) > 
      0) {
    cat("At least two equal dates in the same block\n")
    return(ans)
  }
  if (length(sen) == 0) {
    cat("Less than 4 dates in all blocks\n")
    return(ans)
  }
  if (S == 0) {
    p = 1
  }
  else {
    zeta <- (S - sign(S))/sqrt(varS)
    p <- (1 - pnorm(abs(zeta))) * 2
  }
  if (!missing(cv)) {
    kcv = split(cv, block)
    for (b in 1:maxblock) {
      kkd = kd[[b]]
      kkcv = kcv[[b]]
      kky = ky[[b]]
      if (length(kkd) > 3) {
        for (i1 in 2:length(kkd)) {
          for (i2 in 1:(i1 - 1)) {
            Scv <- Scv + sign1((kkcv[i2] - kkcv[i1])/(kkd[i2] - 
                                                        kkd[i1]))
          }
        }
        nd <- length(kkcv[!is.na(kkcv)])
        tn <- length(kkcv[is.na(kkcv)])
        SumTies <- 0
        for (i1 in 1:length(kkcv)) {
          if (!is.na(kkcv[i1])) {
            ties <- length(kkcv[kkcv == kkcv[i1]]) - 
              tn
            SumTies <- SumTies + (ties - 1) * (2 * ties + 
                                                 5)
          }
        }
        varScv <- varScv + nd * (nd - 1) * (2 * nd + 
                                              5)/18 - SumTies/18
      }
      else cat("1 block with less than 4 points ignored\n")
      Ry <- rank(kky, na.last = "keep", ties.method = "average")
      Ry[is.na(Ry)] <- mean(Ry[!is.na(Ry)])
      Rcv <- rank(kkcv, na.last = "keep", ties.method = "average")
      Rcv[is.na(Rcv)] <- mean(Rcv[!is.na(Rcv)])
      Kmix <- 0
      for (i in 2:length(kky)) {
        for (j in 1:(i - 1)) Kmix <- Kmix + sign1((kkcv[i] - 
                                                     kkcv[j]) * (kky[i] - kky[j]))
      }
      Gmix <- 0
      for (i in 1:length(kky)) Gmix <- Gmix + Ry[i] * Rcv[i]
      varmix <- varmix + (Kmix + 4 * Gmix - length(kkd) * 
                            (length(kky[!is.na(kky)]) + 1) * (length(kkcv[!is.na(kkcv)]) + 
                                                                1))/3
    }
    p.varS = varS - varmix * varmix/varScv
    p.S <- S - varmix/varScv * Scv
    if (p.S == 0) {
      p.p = 1
    }
    else {
      zeta <- (p.S - sign(p.S))/sqrt(p.varS)
      p.p <- (1 - pnorm(abs(zeta))) * 2
    }
  }
  if (correct & abs(S) > 0 & ny > 9 & maxblock > 1) {
    X <- rep(NA, times = (ny * maxblock))
    dim(X) <- c(ny, maxblock)
    Rks <- X
    varSc <- 0
    for (i in 1:length(date)) X[date[i] - minyear + 1, block[i]] <- y[i]
    for (b in 1:maxblock) {
      R <- X[, b]
      R <- rank(R, na.last = "keep", ties.method = "average")
      R[is.na(R)] <- mean(R[!is.na(R)])
      Rks[, b] <- R
    }
    for (g in 1:maxblock) {
      for (h in 1:maxblock) {
        K <- 0
        for (i in 2:ny) {
          for (j in 1:(i - 1)) K <- K + sign1((X[j, g] - 
                                                 X[i, g]) * (X[j, h] - X[i, h]))
        }
        G <- 0
        for (i in 1:ny) G <- G + Rks[i, g] * Rks[i, h]
        xg <- X[, g]
        xh <- X[, h]
        varSc <- varSc + (K + 4 * G - length(xg) * (length(xg[!is.na(xg)]) + 
                                                      1) * (length(xh[!is.na(xh)]) + 1))/3
      }
    }
    zeta <- (S - sign(S))/sqrt(varSc)
    pc <- (1 - pnorm(abs(zeta))) * 2
    if (!missing(cv)) {
      XC <- rep(NA, times = (ny * maxblock))
      dim(XC) <- c(ny, maxblock)
      Rks <- X
      varSccv <- 0
      for (i in 1:length(date)) XC[date[i] - minyear + 
                                     1, block[i]] <- cv[i]
      for (i in 1:maxblock) {
        R <- XC[, i]
        R <- rank(R, na.last = "keep", ties.method = "average")
        R[is.na(R)] <- mean(R[!is.na(R)])
        Rks[, i] <- R
      }
      varmix <- 0
      for (g in 1:maxblock) {
        for (h in 1:maxblock) {
          kkcv = XC[, g]
          kky = X[, h]
          Ry <- rank(kky, na.last = "keep", ties.method = "average")
          Ry[is.na(Ry)] <- mean(Ry[!is.na(Ry)])
          Rcv <- rank(kkcv, na.last = "keep", ties.method = "average")
          Rcv[is.na(Rcv)] <- mean(Rcv[!is.na(Rcv)])
          Kmix <- 0
          for (i in 2:length(kky)) {
            for (j in 1:(i - 1)) Kmix <- Kmix + sign1((kkcv[i] - 
                                                         kkcv[j]) * (kky[i] - kky[j]))
          }
          Gmix <- 0
          for (i in 1:length(kky)) Gmix <- Gmix + Ry[i] * 
            Rcv[i]
          varmix <- varmix + (Kmix + 4 * Gmix - length(kky) * 
                                (length(kky[!is.na(kky)]) + 1) * (length(kkcv[!is.na(kkcv)]) + 
                                                                    1))/3
        }
      }
      for (g in 1:maxblock) {
        for (h in 1:maxblock) {
          K <- 0
          for (i in 2:ny) {
            for (j in 1:(i - 1)) K <- K + sign1((XC[j, 
                                                    g] - XC[i, g]) * (XC[j, h] - XC[i, h]))
          }
          G <- 0
          for (i in 1:ny) G <- G + Rks[i, g] * Rks[i, 
                                                   h]
          xg <- X[, g]
          xh <- X[, h]
          varSccv <- varSccv + (K + 4 * G - length(xg) * 
                                  (length(xg[!is.na(xg)]) + 1) * (length(xh[!is.na(xh)]) + 
                                                                    1))/3
        }
      }
      p.varSc <- varSc - varmix * varmix/varSccv
      zeta <- (p.S - sign(p.S))/sqrt(p.varSc)
      p.pc <- (1 - pnorm(abs(zeta))) * 2
    }
  }
  if (ncomp > 0) 
    tau <- S/ncomp
  ans = list(sl = p, S = S, B = B, B.SD = B.SD, varS = varS, sl.corrected = pc, 
             varS.corrected = varSc, partial.S = p.S, partial.sl = p.p, 
             partial.varS = p.varS, partial.sl.corrected = p.pc, partial.varS.corrected = p.varSc, 
             tau = tau)
  oldClass(ans) <- "rkt"
  return(ans)
}


## Read in satellite-predicted SDD for all sensors 
l5.list.files <- list.files(paste0(main.dir,"Output_Data/SDD_Timeseries/L5/"), "*.csv$", full.names = T)
l5 <- lapply(l5.list.files, read.csv)
l7.list.files <- list.files(paste0(main.dir,"Output_Data/SDD_Timeseries/L7/"), "*.csv$", full.names = T)
l7 <- lapply(l7.list.files, read.csv)
l8.list.files <- list.files(paste0(main.dir,"Output_Data/SDD_Timeseries/L8/"), "*.csv$", full.names = T)
l8 <- lapply(l8.list.files, read.csv)
l9.list.files <- list.files(paste0(main.dir,"Output_Data/SDD_Timeseries/L9/"), "*.csv$", full.names = T)
l9 <- lapply(l9.list.files, read.csv)
s2.list.files <- list.files(paste0(main.dir,"Output_Data/SDD_Timeseries/S2/"), "*.csv$", full.names = T)
s2 <- lapply(s2.list.files, read.csv)
ponds <- unique(c(sapply(str_split(basename(l5.list.files), "_"), "[[", 1), sapply(str_split(basename(l7.list.files), "_"), "[[", 1), sapply(str_split(basename(l8.list.files), "_"), "[[", 1), sapply(str_split(basename(l9.list.files), "_"), "[[", 1), sapply(str_split(basename(s2.list.files), "_"), "[[", 1)))


## Create an empty dataframe to populate with results 
df <- as.data.frame(matrix(nrow = length(ponds), ncol = 12))
colnames(df) <- c("CCC_GIS_ID","NAME","TOWN","N_OBS","N_YRS","TAU","SCORE","SLOPE","PVALUE","INTERCEPT","S_SIGMA","PCT_CHANGE")
  

## Loop through each CCC_GIS_ID and compute trend assessment 
for(pond in 1:length(ponds)){
  
  ## Combine all sensors for the given pond 
  l5.pond <- l5[[which(sapply(str_split(basename(l5.list.files), "_"), "[[", 1) == ponds[pond])]]
  l7.pond <- l7[[which(sapply(str_split(basename(l5.list.files), "_"), "[[", 1) == ponds[pond])]]
  s2.pond <- s2[[which(sapply(str_split(basename(l5.list.files), "_"), "[[", 1) == ponds[pond])]]
  if(!(ponds[pond] %in% sapply(str_split(basename(l8.list.files), "_"), "[[", 1)) & ponds[pond] %in% sapply(str_split(basename(l9.list.files), "_"), "[[", 1)){
    l9.pond <- l9[[which(sapply(str_split(basename(l9.list.files), "_"), "[[", 1) == ponds[pond])]]
    sat.pond <- rbind(l5.pond[c("CCC_GIS_ID","day_str","Name","Town","SDD_PREDICT")], l7.pond[c("CCC_GIS_ID","day_str","Name","Town","SDD_PREDICT")], l9.pond[c("CCC_GIS_ID","day_str","Name","Town","SDD_PREDICT")], s2.pond[c("CCC_GIS_ID","day_str","Name","Town","SDD_PREDICT")])
  } else if(ponds[pond] %in% sapply(str_split(basename(l8.list.files), "_"), "[[", 1) & !(ponds[pond] %in% sapply(str_split(basename(l9.list.files), "_"), "[[", 1))){
    l8.pond <- l8[[which(sapply(str_split(basename(l8.list.files), "_"), "[[", 1) == ponds[pond])]]
    sat.pond <- rbind(l5.pond[c("CCC_GIS_ID","day_str","Name","Town","SDD_PREDICT")], l7.pond[c("CCC_GIS_ID","day_str","Name","Town","SDD_PREDICT")], l8.pond[c("CCC_GIS_ID","day_str","Name","Town","SDD_PREDICT")], s2.pond[c("CCC_GIS_ID","day_str","Name","Town","SDD_PREDICT")])
  } else{
    l8.pond <- l8[[which(sapply(str_split(basename(l8.list.files), "_"), "[[", 1) == ponds[pond])]]
    l9.pond <- l9[[which(sapply(str_split(basename(l9.list.files), "_"), "[[", 1) == ponds[pond])]]
    sat.pond <- rbind(l5.pond[c("CCC_GIS_ID","day_str","Name","Town","SDD_PREDICT")], l7.pond[c("CCC_GIS_ID","day_str","Name","Town","SDD_PREDICT")], l8.pond[c("CCC_GIS_ID","day_str","Name","Town","SDD_PREDICT")], l9.pond[c("CCC_GIS_ID","day_str","Name","Town","SDD_PREDICT")], s2.pond[c("CCC_GIS_ID","day_str","Name","Town","SDD_PREDICT")])
  }

  ## Subset to the CCC_GIS_ID of interest
  df$CCC_GIS_ID[pond] <- unique(sat.pond$CCC_GIS_ID)
  if(length(unique(sat.pond$CCC_GIS_ID)) > 1){print(pond)}
  df$NAME[pond] <- unique(sat.pond$Name)
  df$TOWN[pond] <- unique(sat.pond$Town)
  df$N_OBS[pond] <- nrow(sat.pond)
  ## Convert date column to a date object and create column for year
  sat.pond$day_str <- as.Date(sat.pond$day_str, "%Y-%m-%d")
  sat.pond$YEAR <- year(sat.pond$day_str)
  df$N_YRS[pond] <- length(unique(sat.pond$YEAR))

  ## If there are at least 4 observations, perform a trend assessment
  if(df$N_YRS[pond] >= 4){

    ## Aggregate by year
    ponds.id.agg <- aggregate(SDD_PREDICT  ~ YEAR, data = sat.pond, FUN = mean, na.rm = TRUE, na.action = NULL)
    ## Add columns for decimal date and block
    ponds.id.agg$DECIMAL_DATE <- decimal_date(as.Date(paste0(sat.pond$YEAR,"-06-01"),"%Y-%m-%d"))
    id.rkt <- redefine.rkt(ponds.id.agg$DECIMAL_DATE, ponds.id.agg$SDD_PREDICT)
    ## Populate df with results
    df$TAU[pond] <- id.rkt$tau
    df$SCORE[pond] <- id.rkt$S
    df$PVALUE[pond] <- id.rkt$sl
    df$SLOPE[pond] <- id.rkt$B
    df$INTERCEPT[pond] <- id.rkt$B * (-mean(ponds.id.agg$DECIMAL_DATE, na.rm = T)) + mean(ponds.id.agg$SDD_PREDICT, na.rm = T)
    df$S_SIGMA[pond] <- id.rkt$B.SD
    ## Compute percent change
    results.fits <- df$INTERCEPT[pond] + df$SLOPE[pond] * ponds.id.agg$DECIMAL_DATE
    df$PCT_CHANGE[pond] <- ((results.fits[nrow(ponds.id.agg)] - results.fits[1]) / results.fits[1]) * 100
    ## Label as either increasing, decreasing or no change 
    df$TREND <- ifelse(df$PCT_CHANGE > 0 & df$PVALUE < 0.1, "Improving", ifelse(df$PCT_CHANGE < 0 & df$PVALUE < 0.1, "Deteriorating", "No change"))
    
  }
  
}
  
## Format dataframe 
df$TAU <- round(df$TAU, digits = 2)
df$PVALUE <- round(df$PVALUE, digits = 3)
df$PCT_CHANGE <- round(df$PCT_CHANGE, digits = 2)
df <- df[,c("CCC_GIS_ID","NAME","N_YRS","TAU","PVALUE","PCT_CHANGE","TREND")]
## Output results as a CSV file 
dir.create(file.path(main.dir, "Output_Data/Output_Trend_Data"), showWarnings = F)
write.csv(df, paste0(main.dir, "Output_Data/Output_Trend_Data/Satellite_MK_Trends_Monitoring_Season.csv"), row.names = F)
write.csv(subset(df, PVALUE < 0.1 & TREND == "Improving"), paste0(main.dir, "Output_Data/Output_Trend_Data/Satellite_MK_Trends_Monitoring_Season_Improving.csv"))
write.csv(subset(df, PVALUE < 0.1 & TREND == "Deteriorating"), paste0(main.dir, "Output_Data/Output_Trend_Data/Satellite_MK_Trends_Monitoring_Season_Deteriorating.csv"))


