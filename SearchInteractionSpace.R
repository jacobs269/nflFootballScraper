library(leaps)
library(tidyverse)
library(modelr)
library(stringr)



#The indices from the interactions
#Given randomly selected interactions and all MEs, create a formula
createFormula <- function(MEs,inters,indices) {
  grabInters <- inters[indices]
  a <- str_c(MEs,collapse="+")
  b <- str_c(grabInters,collapse="+")
  base <- str_c(a,b,sep="+")
  result <- as.formula(str_c("sqrtNFL.fant~",base,sep="+"))
  return (result)
}

#given a model formula, produce a vector of the predictors
formulaStripper <- function(formula) {
  result <- as.character(formula)[3] %>%
    str_split("\\+") %>%
    map(str_trim) %>%
    unlist()
  return (result)
}

#regObj: a regsubsets object containing models from a forward, backward, or stepwise selection
#Returns, as a string, the formula for the best model
grabBestModel <- function(regObj) {
  modOut <- summary(regObj)
  optimalModelSize <- which.max(modOut$adjr2)
  bestModels <- as.data.frame(modOut$which)
  optimalModel <- names(bestModels)[which(as.logical(bestModels[optimalModelSize,]))][-1]
  optimalModel <- str_c(optimalModel,collapse="+") %>%
    str_replace_all("UC|JR|SR|TRUE","") %>%
    str_c("sqrtNFL.fant~",.,sep="") %>%
    as.formula()
  
  return (optimalModel)
}
 
#A function that takes a given model form, runs a best subset selection with that model form, grabs the model with highest adjusted r^2, and returns both the model and the adjusted R^2
#nvmax: Grab best models of each of these sizes
grabModPerf <- function(dat,form,size) {
  mods <- regsubsets(form, data=dat, nbest=1, nvmax = size)
  modOut <- summary(mods)
  
  #grab the best models adj r^2
  optR <- max(modOut$adjr2)
  
  #grab the predictors in the model
  optimalModel <- grabBestModel(mods)
  
  return (list(optR,optimalModel))
}

#dat: the data
#iter: How many best subset selections to do
#numInter: How many interactions should be randomely included FOR EACH best subset selection
#MEs: the main effects
#inters: the pairwise interactions
#size: the number of model sizes to get the best model for in best subset selection
#this function runs grabModPerf -- which runs a best subset selection and returns the best model according to adjusted r^2 -- iter times. Each time, it uses a different formula (by randomely choosing numInter interaction terms to include). This function is searching the interaction space for the best model
#return formHolder: A vector where a term occurs the number of times that it appears in a model FORM that is used for best subset selection
#return hitHolder: A vector where a term occurs the number of times that it appears in a bestMod on an iteration of best subset selection
exploreInters <- function(dat,iter,numInter,MEs,inters,size) {
  
  formHolder <- c() #holds the forms. Was it included?
  hitHolder <- c() #was it included in the best model?
  
  bestR <- c()
  bestMods <- c()
  for (i in 1:iter) {
    indices <- sample(x=1:length(inters),size=numInter,replace=FALSE)
    form <- createFormula(MEs,inters,indices)
    formHolder <- c(formHolder,formulaStripper(form))
    nextMod <- grabModPerf(dat,form,size)
    bestR <- c(bestR,nextMod[[1]])
    bestMods <- c(bestMods,nextMod[[2]])
    hitHolder <- c(hitHolder,formulaStripper(nextMod[[2]]))
  }
  return (list(bestR,bestMods,formHolder,hitHolder))
}

#Given a 
#3: By running this a very large number of times, and the counting the number of times each term appears in the model, we can pin down which interactions are most important


#Include 10 interactions in the model form that we will run best subset selection on
#In best subset selection, find best model of size 1,2,....,15
#Run 1000 best subset selections, Results has vector of best adjr^2, and corres models
results <- exploreInters(narrowSubDat,1000,10,MEs,inters,20)
###Load with load("simulation.RData"). Object will then be in jeff
load("simulation.RData")
results <- jeff
included <- data.frame(results[[3]] %>% as.factor() %>% table())
inBest <- data.frame(results[[4]] %>% as.factor() %>% table())
end <- merge(included,inBest,by=".")
names(end) <- c("Effect","Included","inBest")
end <- end %>% 
  mutate(hitRate = inBest/Included) %>%
  mutate(isInteraction = ifelse(str_detect(Effect,":"),"Inter","ME"))
names(end) <- c("Effect","Xincluded","XinBest","VarImp","isInteraction")
endME <- end %>% filter(isInteraction=="ME") %>%
  arrange(desc(VarImp)) %>% select(-isInteraction)
endInter <- end %>% filter(isInteraction=="Inter") %>%
  arrange(desc(VarImp)) %>% select(-isInteraction)
save(file="meImp.RData",endME)
save(file="intImp.RData",endInter)





















