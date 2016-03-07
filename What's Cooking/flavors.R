# TODO: Add comment
# 
# Author: Radics, Attila 
###############################################################################

acquire = function(packages, quietly = T,  repos = "http://cran.rstudio.com/"){
	invisible(lapply(packages, function(package) {
						if(!require(package, quietly = quietly, character.only = TRUE)){
							install.packages(package, repos = "http://cran.rstudio.com/")
							library(package, quietly = quietly, character.only = TRUE)
						}
					}))
}
acquire(c("jsonlite", "data.table", "ggplot2", "h2o", "SnowballC"))


data.root = "C:/Users/212303932/Desktop/Data/Cooking"
data.train = fromJSON(file.path(data.root, "train.json"))
data.test = fromJSON(file.path(data.root, "test.json"))	


###############################################################################

CleanIngredients = function(ingredients){
	ingredients = iconv(ingredients, "UTF-8", "ASCII", sub="")
	ingredients = tolower(ingredients)
	ingredients = gsub("[^a-z]", " ", ingredients)
	ingredients = gsub("\\s+", " ", ingredients)
	ingredients = gsub("^\\s+", "", ingredients)
	ingredients = gsub("\\s+$", "", ingredients)
	ingredients = lapply(ingredients, function(ingredient){
				parts = strsplit(ingredient, " ")[[1]]
				parts = paste0(wordStem(parts), collapse = "_")
				gsub("_+", "_", parts)
			})
	do.call("c", ingredients)
}

FilterIngredients = function(ingredients, min.length, min.occurance){	
	if(min.length > 1){
		ingredients = ingredients[nchar(ingredients) >= min.length]	
	}
	if(min.occurance > 1){
		ingredients = data.table("ingredient" = ingredients)
		ingredients = ingredients[, .N, by = ingredient]
		setkey(ingredients, ingredient)
		ingredients = ingredients[N > min.occurance][["ingredient"]]
	}
	sort(unique(ingredients))
}

AllIngredients = function(ingredients){
	ingredient.parts = do.call("c", strsplit(ingredients, "_"))
	c(ingredients, ingredient.parts)
}

IngredientCuisineMembership = function(data, all.ingredients){	
	uniques = sort(unique(all.ingredients))
	cuisines = c("total", sort(unique(data$cuisine)))
	membership = matrix(0, 
			ncol = length(cuisines), 
			nrow = length(uniques))
	colnames(membership) = cuisines
	rownames(membership) = uniques
	lapply(cuisines, function(cuisine) {
				if(cuisine == "total"){
					by.cuisine = data$ingredients
				} else {
					by.cuisine = data$ingredients[data$cuisine == cuisine]
				}
				by.cuisine = do.call("c", by.cuisine)
				by.cuisine = by.cuisine[by.cuisine %in% uniques]
				
				by.cuisine = table(by.cuisine)
				membership[names(by.cuisine), cuisine] <<- 
						as.vector(by.cuisine)
				NULL
			})
	membership
}

CalculateMembershipRanks = function(membership){
	colnames(membership) = paste0("rank_", colnames(membership))
	apply(membership, 2, rank)
}

CalculateMembershipExclusives = function(membership){
	total = membership[,1]
	only = membership[,-1]
	never = membership[,-1]
	
	colnames(only) = paste0("only_", colnames(only))
	only = only - do.call("cbind", rep(list(total - 1), ncol(only)))
	only[only < 0] = 0
	
	colnames(never) = paste0("never_", colnames(never))
	never = (never * -1) + 1
	never[never < 0] = 0
	cbind(only, never)
}

CalculateNormalizationParameters = function(features){
	means = apply(features, 2, mean)
	sds = apply(features, 2, sd)
	return(list(mean = means, sd = sds))
}

NormalizeFeatures = function(features, normalization.parameters){
	means = normalization.parameters$mean
	sds = normalization.parameters$sd
	lapply(seq(features), function(i) {
		features[[i]] <<- (features[[i]] - means[i]) / sds[i]
	})
	features
}

CalculateIngredientsMatrix = function(data, feature.ingredients){
	ingredients.matrix = lapply(data$ingredients, function(ingredients) {
				as.integer(feature.ingredients %in% ingredients)
			})
	ingredients.matrix = do.call(rbind, ingredients.matrix)
	colnames(ingredients.matrix) = feature.ingredients
	ingredients.matrix
}

CalculateFeatures = function(data, all.ingredients, feature.ingredients, 
		membership = NULL, membership.rank = NULL, 
		membership.exclusive = NULL, normalization.parameters = NULL) {
	
	if(is.null(membership)){
		membership = IngredientCuisineMembership(data, all.ingredients)
		membership.rank = CalculateMembershipRanks(membership)
		membership.exclusive = CalculateMembershipExclusives(membership)
	}
	
	features = lapply(data$ingredients, function(ingredients) {							
				idx = which(rownames(membership) %in% ingredients)
				exclusives = membership.exclusive[idx, , drop = FALSE]							
				ranks = membership.rank[idx, , drop = FALSE]							
				
				c(
						apply(exclusives, 2, sum), 
						apply(ranks, 2, min), 
						apply(ranks, 2, max), 
						apply(ranks, 2, mean), 
						apply(ranks, 2, sum)
				)	
			})
	features = as.data.frame(do.call(rbind, features))
	
	setDT(features)	
	prefix = c(
			rep("sum_", ncol(membership.exclusive)), 
			rep("min_", ncol(membership.rank)), 
			rep("max_", ncol(membership.rank)), 
			rep("mean_", ncol(membership.rank)), 
			rep("sum_", ncol(membership.rank)))
	setnames(features, paste0(prefix, colnames(features)))
	
	if(is.null(normalization.parameters)){
		normalization.parameters = CalculateNormalizationParameters(features)
	}
	features = NormalizeFeatures(features, normalization.parameters)
	
	features = cbind(
			features, 
			CalculateIngredientsMatrix(data, feature.ingredients)
	)
	
	params = list(
			all.ingredients = all.ingredients, 
			feature.ingredients = feature.ingredients,  
			membership = membership, 
			membership.rank = membership.rank, 
			membership.exclusive = membership.exclusive, 
			normalization.parameters = normalization.parameters)
	
	list(features = features, params = params)
}

CalculateFeaturesWrapper = function(data, params){
	CalculateFeatures(data, 
			params$all.ingredients, 
			params$feature.ingredients, 
			params$membership, 
			params$membership.rank, 
			params$membership.exclusive, 
			params$normalization.parameters)
}

#Data cleaning
data.train$ingredients = lapply(data.train$ingredients, CleanIngredients)
data.test$ingredients = lapply(data.test$ingredients, CleanIngredients)
data.train$ingredients = lapply(data.train$ingredients, AllIngredients)
data.test$ingredients = lapply(data.test$ingredients, AllIngredients)

#Discarding some features
all.ingredients = do.call("c", data.train$ingredients)
feature.ingredients = FilterIngredients(all.ingredients, 1, 1)

train = data.train

#Training set
params = CalculateFeatures(train, all.ingredients, feature.ingredients)
train = params$features
params = params$params
train[, cuisine := as.factor(data.train$cuisine)]

#Test set
test = CalculateFeaturesWrapper(data.test, params)
test = test$features

save(train, test, params, file = file.path(data.root, "features.RData"))

#Grid search removed (redundancy), only random search kept in final R version
while(file.exists(paste0(data.root, "run.txt"))){
	h2o.init(max_mem_size = "10g", nthreads = 6)
	Sys.sleep(5)
	
	labelled.hex = as.h2o(train, "labelled")
	
	splits = h2o.splitFrame(labelled.hex, 
			destination_frames = c("train", "valid"))
	
	hidden.layers = as.integer(c(runif(1, 450, 600), runif(1, 75, 150))) 
	input.dropout.ratio = round(runif(1, 0.2, 0.5), 3)
	l1.regularization = round(runif(1, 1e-05, 1e-03), 6)
	
	
	model.id = paste0("dl_layers(", 
			paste0(hidden.layers, collapse = "-"), 
			")_input.dropout(", input.dropout.ratio, 
			")_l1(", l1.regularization ,")")
	
	if(file.exists(paste0(data.root, "Models/", model.id))) {
		next
	}
		
	model = h2o.deeplearning(
			model_id = model.id,
			x = seq(ncol(labelled.hex)-1),
			y = "cuisine",
			training_frame = splits[[1]],
			validation_frame = splits[[2]],
			hidden = hidden.layers,
			input_dropout_ratio = input.dropout.ratio,
			l1 = l1.regularization,
			activation = "RectifierWithDropout",
			l2 = 0,				
			overwrite_with_best_model = TRUE,
			use_all_factor_levels = TRUE,
			sparse = TRUE,		
			epochs = 150,
			score_interval = 60, 
			score_validation_samples = 0,
			stopping_tolerance = 0.025,
			score_duty_cycle = 0.2,
			fast_mode = TRUE,
			loss = "CrossEntropy",
			distribution = "multinomial", 
			ignore_const_cols = FALSE,
			shuffle_training_data = TRUE,
	)
	h2o.saveModel(object = model, 
			path = file.path("file:///", data.root, "Models"))
		
	h2o.shutdown(F)
	Sys.sleep(55)
}

#Model evaluation
model.paths = list.dirs(paste0(data.root, "Models"))[-1]
score.history = lapply(model.paths, function(model.path){
			model = h2o.loadModel(path = paste0("file:///", model.path))
			score.history = h2o.scoreHistory(model)
			score.history = score.history[c(-1, -nrow(score.history)),-1:-3]
			score.history["model"] = model@model_id
			h2o.removeAll()
			setDT(score.history)
			score.history
		})
score.history = rbindlist(score.history, TRUE)
save(score.history, file = file.path(data.root, "scoreHistory.RData"))

best.models = score.history[epochs >= 10, mean(validation_classification_error), by = model]
setnames(best.models, "V1", "mean_classification_error")
setkey(best.models, mean_classification_error)

best.parameters = lapply(best.models$model[1:3], function(model.path) {
			model = h2o.loadModel(path = paste0("file:///", data.root,  "Models/", model.path))
			list(
					hidden = model@parameters$hidden, 
					input.dropout.ratio = model@parameters$input_dropout_ratio,
					l1.regularization = model@parameters$l1
			)
})


for (model.params in best.parameters){
	for(i in seq(10)){
		TrainSubmissionModels(
				prefix = i,
				train = train,
				hidden.layers = model.params$hidden,
				input.dropout.ratio = round(model.params$input.dropout.ratio, 3),
				l1.regularization = round(model.params$l1.regularization, 6))
	}	
}

TrainSubmissionModels = function(
		prefix,
		train,
		hidden.layers,
		input.dropout.ratio,
		l1.regularization,
		epochs = c(15,5,5)){
	
	h2o.init(max_mem_size = "10g", nthreads = 6)
	Sys.sleep(15)
	
	train.hex = as.h2o(train, "train")
	
	model = NULL
	
	for (e in seq(epochs)){
		model.id = paste0(prefix,
				"_e(", cumsum(epochs)[e], 
				")_h(", paste0(hidden.layers, collapse = "-"), 
				")_in.drop(", input.dropout.ratio, 
				")_l1(", l1.regularization ,")")		
		if(!is.null(model)){
			model = model@model_id
		}
		
		model = h2o.deeplearning(
				model_id = model.id,
				checkpoint = model,
				x = seq(ncol(labelled.hex)-1),
				y = "cuisine",
				training_frame = train.hex,
				hidden = hidden.layers,
				input_dropout_ratio = input.dropout.ratio,
				l1 = l1.regularization,
				activation = "RectifierWithDropout",
				l2 = 0,				
				overwrite_with_best_model = TRUE,
				use_all_factor_levels = TRUE,
				sparse = TRUE,		
				epochs = cumsum(epochs)[e],
				fast_mode = TRUE,
				loss = "CrossEntropy",
				distribution = "multinomial", 
				ignore_const_cols = FALSE,
				shuffle_training_data = TRUE,
		)
		h2o.saveModel(object = model, path = paste0("file:///", data.root, "Submission"))	
	}
	
	h2o.shutdown(F)
	Sys.sleep(55)
}


#Submission
model.paths = list.dirs(paste0(data.root, "Submission"))[-1]

h2o.init(max_mem_size = "10g", nthreads = 6)
test.hex = as.h2o(test, "test")

submission = lapply(model.paths, function(model.path) {			
			model = h2o.loadModel(path = paste0("file:///", model.path))
			iteration = gsub(".*/(\\d+).*", "\\1", model.path)
			epochs = gsub(".*/\\d+_e.(\\d+).*", "\\1", model.path)
			name = gsub(".*/\\d+_e.\\d+)_(.*)", "\\1", model.path)
			prediction = h2o.predict(model, test.hex)
			prediction = as.data.frame(prediction)
			
			setDT(prediction)
			prediction[, epochs := epochs]
			prediction[, iteration := iteration]
			prediction[, name := name]
			prediction[, id := data.test$id]
			prediction
		})
h2o.shutdown(F)

submission = rbindlist(submission, TRUE)

for (epochs in c(15, 20, 25)){
	model.names = c("h(489-123)_in.drop(0.26)_l1(1e-05)",
			"h(493-84)_in.drop(0.29)_l1(5e-04)", 
			"h(576-123)_in.drop(0.255)_l1(1.1e-05)")
	for(model.name in model.names){
		load(paste0(data.root, "submission.RData"))
		submission = submission[name == model.name & epochs == epochs]
		submission = submission[, c(1, 22:24) := NULL]
		submission = submission[,  lapply(.SD, sum), by = id]
		
		id = submission$id
		submission[, id := NULL]
		cuisine = colnames(submission)[apply(submission, 1, which.max)]
		
		submission = data.frame(id = id, cuisine = cuisine)
		write.table(submission, file = paste(data.root, paste0(model.name)), 
				quote = F, sep = ",", row.names = F)
	}
}
