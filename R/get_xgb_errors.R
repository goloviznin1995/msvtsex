

# Returns xgboost errors

get_xgb_errors <- function(d = d, 
						   features = c(29,36),
						   nrounds = 100,
						   train_share = 0.5,
						   alpha = alpha,
						   max_depth = 6,
						   eta = 0.1,
						   min_child_weight = 2,
						   gamma = 2,
						   early_stopping_rounds = 5){
	result = list(errors = NULL, imp_matrix  = NULL)
	
	data <- data.frame(d[,features[1]], d[,features[2:length(features)]])
	
	test <- data[ceiling(nrow(data)*train_share):nrow(data),]
	train <- data[1:(floor(nrow(data)*train_share)),]
	
	fold1_n <- 1:nrow(train)%%2 == 0
	fold2_n <- 1:nrow(train)%%2 == 1
	
	fold1 <- train[fold1_n,]
	fold2 <- train[fold2_n,]
	
	fold2 <- data.matrix(fold2)
	fold1 <- data.matrix(fold1)
	test <- data.matrix(test)
	
	xgb <- xgboost::xgboost(data = fold1[,-1], 
				   label = fold1[,1],
				   nrounds = nrounds,
				   objective = 'reg:linear',
				   alpha = alpha,
				   max_depth = max_depth,
				   eta = eta,
				   min_child_weight = min_child_weight,
				   gamma = gamma,
				   early_stopping_rounds = early_stopping_rounds
	)
	
	# Return importance matrix
	result$imp_matrix <- xgboost::xgb.importance(colnames(data[2:length(features)]),
										model = xgb)
	
	
	out_of_fold1 <- predict(xgb, newdata = fold2[,-1])
	pr1_test <- predict(xgb, newdata = test[,-1])	
	
	
	xgb <- xgboost::xgboost(data = fold2[,-1], 
				   label = fold2[,1],
				   nrounds = nrounds,
				   objective = 'reg:linear',
				   alpha = alpha,
				   max_depth = max_depth,
				   eta = eta,
				   min_child_weight = min_child_weight,
				   gamma = gamma,
				   early_stopping_rounds = early_stopping_rounds
	)
	
	out_of_fold2 <- predict(xgb, newdata = fold1[,-1])
	pr2_test <- predict(xgb, newdata = test[,-1])		
	
	avg_test <- (pr1_test + pr2_test)/2
	
	a <- c(1:(nrow(test)+nrow(train)))
	
	a[1:nrow(train)][1:nrow(train)%%2 == 1] <- out_of_fold1
	a[1:nrow(train)][1:nrow(train)%%2 == 0] <- out_of_fold2
	
	a[(nrow(train)+1):nrow(data)] <- avg_test
	
	predictions <- a
	result$errors <- d[,features[1]]-predictions
	return(result)
}	