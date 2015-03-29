eval_measure_glm <- function(y, varlist, dat, iter=100){
  library(caret)
  library(lavaan)
  
#   y = "any_dis_strength"
#   varlist = c("full_mbi_cont_EE", "single_item_cont_EE", "full_mbi_high_EE", "single_item_high_EE")
#   dat = dat_burn_strength
#   iter = 100
#   
  # create progress bar
  pb <- txtProgressBar(min = 0, max = iter, style = 3)
  
  props <- data.frame(prp_cor_lo = rep(NA, iter)
                      #,prp_cor_mid = rep(NA,iter)
                      ,prp_cor_hi = rep(NA, iter)
  )
  
  models_comp <- list()
  for (i in 1:length(varlist)){
    models_comp[[varlist[i]]] <- props
  }
  
  for (k in 1:iter){
    Sys.sleep(0.1)
    # update progress bar
    setTxtProgressBar(pb, k)
    
    # set seed for replicability
    set.seed(k)
    
    #partition data into training and test
    dat <- na.omit(dat)
    dat_train_select <- createDataPartition(dat[,y], p = 0.50, list = FALSE)
    training <- dat[dat_train_select, ]
    testing <- dat[-dat_train_select, ]
    #run through every possible predictor in varlist
    #using training data
#     models <- lapply(varlist, function(x) {
#       lm(substitute(training[,y] ~ i, list(i = as.name(x))), data = training)
#     })
    
    models <- lapply(varlist, function(x) {
      glm(as.formula(paste(y, "~", x)), data = training, family = "binomial")
    })
    
    #use testing_reu to test the predictive acuracy of our model
    models_pred <- predict(models, newdata = testing[,-1], type="response")
    
    for (j in 1:length(models_pred)){
  
      models_pred[[j]] <- ifelse(models_pred[[j]] > table(dat[,y])[2]/nrow(dat), 1, 0)
    
    }
#     for (j in 1:length(models_pred)){
#       
#       for (i in 1:length(models_pred[[j]]))
#         
#         if(models_pred[[j]][i] < (-1/3)){
#           models_pred[[j]][i] <- -1
#         } else if (models_pred[[j]][i] > (1/3)){
#           models_pred[[j]][i] <- 1
#         } else{
#           models_pred[[j]][i] <- 0
#         }
#       
#     }
    
    #loop through every model in our list to determine a percentage of correct predictions for each model
    
    for (h in 1:length(models_pred)){
      dat_match <- data.frame(pred = models_pred[[h]]
                              ,actual = testing[,y]
                              ,match = ifelse(models_pred[[h]] == testing[,y], 1, 0)
      )
      
      #proportion correct low
      models_comp[[h]][k,1] <- nrow(dat_match[dat_match$actual==0 & 
                                                dat_match$match==1,])/nrow(dat_match[dat_match$actual==0,])
      #proportion correct mid
      models_comp[[h]][k,2] <- nrow(dat_match[dat_match$actual==1 & 
                                                dat_match$match==1,])/nrow(dat_match[dat_match$actual==1,])
      #proportion correct high
      #models_comp[[h]][k,3] <- nrow(dat_match[dat_match$actual==1 & 
      #                                          dat_match$match==1,])/nrow(dat_match[dat_match$actual==1,])
    }
  }

  close(pb)
  
  lm_pvalue <- function(lm){
    pvalue <- coef(summary(lm))[,4][2]
    pvalue
  }

#   lm_rsquared <- function(lm){
#     rsquared <- summary(lm)$r.squared
#     rsquared
#   }
  
  lm_beta <- function(lm){
    beta <- coef(lm)
    beta[2]
  }
  
  final_summary <- data.frame(var = varlist
                              ,thresh_lo = table(dat[,y])[1]/nrow(dat)
                              ,thresh_mid = NA
                              ,thresh_hi = table(dat[,y])[2]/nrow(dat)
                              ,avg_prp_cor_lo = NA
                              ,avg_prp_cor_mid = NA
                              ,avg_prp_cor_hi = NA
                              ,beta = as.numeric(lapply(models, lm_beta))                              
                              ,rsquared = NA
                              ,pvalue = as.numeric(lapply(models, lm_pvalue))
                              ,loglik = as.numeric(lapply(models, logLik))
                              ,row.names = NULL
                              )

  final_summary[,5] <- as.numeric(lapply(varlist, function(i){mean(models_comp[[i]][,1])}))
  final_summary[,7] <- as.numeric(lapply(varlist, function(i){mean(models_comp[[i]][,2])}))
# 
#   for (j in 1:3){
#     final_summary[,(j+4)] <- as.numeric(lapply(varlist, function(i){mean(models_comp[[i]][,j])}))
#   }

  return(final_summary)
}
