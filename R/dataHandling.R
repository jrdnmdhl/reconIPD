#' doRiskIndex
#' @export
doRiskIndex = function(nrisk, survDf){
  nRiskIndexed = adply(
    nrisk,
    1,
    function(d){
      priorSurv = survDf %>% filter(survDf$t >= d$Time)
      nRows = nrow(priorSurv)
      d$lower = head(priorSurv, 1)$i
      return(d)
    }
  )
  nRiskIndexed = nRiskIndexed %>% mutate(
    upper = lead(lower) - 1,
    t = Time,
    i = seq_len(nrow(nRiskIndexed)),
    n = N
  )
  nRiskIndexed[nrow(nRiskIndexed), ]$upper = max(survDf$i)
  nRiskIndexed = nRiskIndexed %>% select(i, t, lower, upper, n)

  return(nRiskIndexed)
}

#' doSurvFix
#' @export
doSurvFix = function(survDf){
  nRow = nrow(survDf) + 1
  tempSurv = rbind.fill(
    survDf,
    data.frame(
      Time = 0,
      Probability = 1
    )
  ) %>%
    arrange(Time) %>%
    mutate(
      t = Time,
      i = seq_len(nRow)
    )
  tempSurv$s = 1
  for(i in seq_len(nrow(tempSurv) - 1)){
    tempSurv$s[i+1] = min(tempSurv$s[i], tempSurv$Probability[i+1])
  }
  outSurv = tempSurv %>% select(i, t, s)
  return(outSurv)
}

#' reconIPD
#' @export
reconIPD = function(path, tags = list(), surv = NULL, risk = NULL, events = NULL){
  if (missing(path)) {
    survData <- doSurvFix(surv)
    riskData <- doRiskIndex(risk, survData)
    if (!is.null(events)) {
      totEvents <- events
    } else {
      totEvents <- "NA"
    }
  } else {
    survData = read.xlsx(path, "Survival") %>% doSurvFix
    riskData = read.xlsx(path, "Number at Risk") %>% doRiskIndex(survData)
    miscInputs = read.xlsx(path, "Events")
    
    totEvents = "NA"
    if(nrow(miscInputs) > 0){
      if(!is.na(miscInputs[1,1])){
        totEvents = miscInputs[1,1]
      }
      else{
        if(!is.na(miscInputs[1,2])){
          totEvents = riskData$N - miscInputs[1,2]
        }
      }
    }
  }
  guyot(survData, riskData, tot.events = totEvents, tags)
}


