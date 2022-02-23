# Main retention time prediction workflow expects
# data as dataframe with 4 columns NAME, INCHKEY,
# SMILES, RT 

# takes following optional variables:
#   method: either 'xgboost' or 'glmnet' method
#     used for training models
#   data_set_name: name of data set, appears on plots 
#   verbose: if TRUE additional prints get executed (useful for debugging) 
#   final_model: if TURE a model on all data gets trained and 
#     returned for later use on new data

# Prepare data set variables
#   preprocessed: if TRUE no additional
#     preprocessing gets done 
#   scale: if TRUE data gets scaled and scaling model gets returned
#   include_polynomial: if TRUE polynomials of each
#     columns get added as new one 
#   degree_polynomial: defines how many polynomials get added (if 3
#     quadratic and cubic terms get added)
#   interaction_terms: if T all interaction terms
#     get added to data set

# Split of data set variables 
#   split_method: Either 'CV' or 'Medoids-kmean', determines how the outer split
#     gets done 
#   nfolds: integer, that gets fowarded to create.split function 
#   reverse_split: if True train and validation data get exchanged

retip.workflow <- function(data, method = "glmnet",
                           verbose = FALSE, data_set_name = "data set", final_model = T,
                           preprocessed = F, interaction_terms = F, split_method = "CV",
                           reverse_split = F, nfolds = 2, include_polynomial = F,
                           degree_polynomial = 2, scale = T) {

  
  # calculate Chemical descriptors, clear data
  # and add additional columns (polynomials and
  # interactions)
  db_rt <- preprocess.data(data, preprocessed = preprocessed,
                           include_polynomial = include_polynomial, 
                           interaction_terms = interaction_terms,
                           degree_polynomial = degree_polynomial)

  
  # Build a model to center and scale the data
  if (scale) {
    
    preProc <- caret::preProcess(db_rt[, -1], method = c("center",
                                                         "scale"), rangeBounds = c(0, 1))
    db_rt <- predict(preProc, db_rt)
  } else {
    preProc <- 0
  }
  db_rt$NAME<-NULL
  
  # outer cross validation split (either CV
  # with n-folds or Medoids-kmean with n medoids)
  
  split <- create.split(split_method, db_rt, n = nfolds)
  
  models <- list()
  stats <- list()
  i <- 1
  
  for (inSplit in split) {
    
    if (reverse_split) {
      training <- db_rt[inSplit, ]
      testing <- db_rt[-inSplit, ]
    } else {
      training <- db_rt[-inSplit, ]
      testing <- db_rt[inSplit, ]
    }
    
    # Train Model
    model <- switch(method, 
                    xgboost = fit.xgboost(training),
                    glmnet = fit.glmnet(training), 
                    stop(paste("method \"",method, "\" is invalid ")))
    models[[i]] <- model
    
    # analyse performance of model
    title <- paste0(data_set_name, ", ", method)
    if (length(db_rt$RT) == nfolds) {
      stat <- predict(object = model, newx = as.matrix(testing[,
                                                               -1]))
    } else {
      stat <- get.stats(testing, model, name = paste0(title,
                                                      " iteration ", i))
    }
    
    stats[[i]] <- stat
    
    print(paste0("Iteration: ", i))
    
    i <- i + 1
  }
  
  return_object <- list()
  
  
  if (final_model) {
    #train model on whole data set
    model <- switch(method, 
                    xgboost = fit.xgboost(db_rt),
                    glmnet = fit.glmnet(db_rt), 
                    stop(paste("method \"",method, "\" is invalid ")))
    pred<- data.frame(predict(model,as.matrix(db_rt[,-1])))
    p<- plot(x=db_rt$RT,y=t(pred),xlab= "RT",ylab="predicted RT")
    p<- abline(a=0,b=1,col="red")
    return_object$plot<- p
  }
  
  
  # create return object and return all
  # potential interesting variables
  return_object$final_model <- model
  return_object$method <- method
  return_object$data_set_name <- data_set_name
  return_object$model <- models
  return_object$stats <- stats
  return_object$predictor_set <- db_rt
  return_object$split <- split
  return_object$scaling_model <- preProc
  
  
  print("Workflow completed")
  return(return_object)
}



