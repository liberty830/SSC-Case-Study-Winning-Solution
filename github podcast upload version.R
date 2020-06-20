##### Investigating Target variable


# Plots for target features in detail.

train <- train %>% 
  mutate(target = log1p(target))

# Log transform makes distribution symmetric, and skewness is alleviated. So, I will use log-transform target.

target_features <- train %>% 
  group_by(title) %>% 
  summarise(mean = mean(target, na.rm = T),
            median = median(target, na.rm = T),
            max = max(target, na.rm = T),
            min = min(target, na.rm = T),
            sd = sd(target, na.rm = T),
            range = max(target, na.rm = T)-min(target, na.rm = T),
            ratio = min(target,na.rm = T)/max(target, na.rm = T),
            n = n())

p1 <- target_features %>% 
  ggplot(aes(mean)) + 
  geom_histogram(fill = "blue", bins = 30) +
  labs(x = "Mean value of log transformed target")

p2 <- target_features %>% 
  ggplot(aes(median)) + 
  geom_histogram(fill = "blue", bins = 30) +
  labs(x = "Median value of log transformed target")

p3 <- target_features %>% 
  ggplot(aes(sd)) + 
  geom_histogram(fill = "blue", bins = 30) +
  labs(x = "Standard Deviation of log transformed target")

p4 <- target_features %>% 
  ggplot(aes(range)) + 
  geom_histogram(fill = "blue", bins = 30)

p5 <- target_features %>% 
  ggplot(aes(ratio)) + 
  geom_histogram(fill = "blue", bins = 30) +
  labs(x = "First day target / Last day target ratio")

p6 <- target_features %>% 
  ggplot(aes(n)) + 
  geom_histogram(fill = "blue", bins = 30) +
  labs(x = "Number of data points in time series(Hourly base)")

grid.arrange(p1,p2,p3,p4,p5,p6, layout_matrix = matrix(c(1:6), nrow = 2, byrow = T))

# Distribution is still some right skewed. Most of titles have very small variations in their time series.
# And the number of reviews on the first day is almost same as that of the last day, so time effect could be limited.
# But, 5% of the titles have large variation their time series, so for these series, I can use time variable as a offset term.





##### Glimpse of testset

sum(unique(test$title) %in% unique(train$title))
# There are 2963 titles in common both of train and test set. The prediction for these titles,
# could be based on the target in train set directly, but the problem is the other 1124 titles.
# I need to predict target for those, only using explanatory variables. So, the prediction strategy
# should be different for two cases.

# I will check some scraped date for the common titles.

foo <- test %>% 
  filter(!duplicated(title)) %>%
  filter(unique(test$title) %in% unique(train$title))

max(train %>% filter(title == "The Watch") %>% select(date) %>% unlist())
min(test %>% filter(title == "The Watch") %>% select(date) %>% unlist())
max(train %>% filter(title == "Reality Steve Podcast") %>% select(date) %>% unlist())
min(test %>% filter(title == "Reality Steve Podcast") %>% select(date) %>% unlist())
max(train %>% filter(title == "Animal Spirits Podcast") %>% select(date) %>% unlist())
min(test %>% filter(title == "Animal Spirits Podcast") %>% select(date) %>% unlist())
max(train %>% filter(title == "a16z Podcast") %>% select(date) %>% unlist())
min(test %>% filter(title == "a16z Podcast") %>% select(date) %>% unlist())

# In common titles, the last day of trainset : 2019-12-01, the first day of testset : 2020-01-10
# So there are 40 days term, and there could be increment during 40 days.





##### Feature Engineering

### on-air, number of episodes

# If a podcast is being broadcasted, then this podcast is more likely to have increasing number of reviews.
# And if a podcast has new episodes during the period, then it would have increasing number of reviews.
# So, I made these two features using "data scraped dates" and "podcast released dates" 





### High dimensional subcategory feature

# The category level of subcategory is 86, and some subcategories include very small number of samples.
# So, I am going to embed this subcategory into smaller dimension.
# To do this, I will use "gloVe Wiki 6-billion words corpus and its word embedding matrix" to leverage its dependency structure.
# And I will combine all subcategories that have small sample sizes to the large subcategories using similarity of word vetors.
# The function below process train and test set both because I have to use the same reference from train set to the test set.


feature_subcategory <- function(train, test){
  # This is a cosine_similarity Matrix
  cosine_similarity <- CosineSim(embedding_matrix)
  diag(cosine_similarity) <- NA
  
  bar <- train %>% filter(!duplicated(title)) %>% group_by(category) %>% summarise(count = n())
  
  # I will choose 20 frequency as a enough number to become a primary category that will be only remained.
  low_freq_cat <- bar %>% filter(count < 20) %>% select(category)
  low_freq_cat <- tolower(low_freq_cat$category)
  
  # From the matrix, I will remove low frequency category words to embed them into the large freq categories.
  remove <- rownames(cosine_similarity) %in% low_freq_cat
  cosine_similarity <- cosine_similarity[!remove, ]
  
  get_most_similar_cat <- function(name){
    
    changed_name <- rownames(cosine_similarity)[which.max(cosine_similarity[ ,name])]
    score <- max(cosine_similarity[ ,name], na.rm = T)
    
    return(c(name, changed_name, score))
  }
  
  a <- lapply(low_freq_cat, get_most_similar_cat)
  a <- do.call(rbind, a) %>% 
    as.tibble() %>% 
    rename(category = V1,
           new_category = V2,
           score = V3) %>% 
    select(category, new_category)
  
  train <- train %>% 
    mutate(category = tolower(category)) %>% 
    left_join(a) %>% 
    mutate(new_category = ifelse(is.na(new_category), category, new_category))
  
  test <- test %>% 
    mutate(category = tolower(category)) %>% 
    left_join(a, by = "category") %>% 
    mutate(new_category = ifelse(is.na(new_category), category, new_category))
  
  return(list(reference = a, train = train, test = test))
}


foo <- feature_subcategory(train, test)
train <- foo$train
test <- foo$test



##### Crawling and Number of Twitter followers feature

# I scraped each artist's Twitter url first. This job was done using google searching.
# I only kept the first page searched results because these artists are famous people, so
# their Twitter should on the first page. And then I cleaned these result, and use "rtweet" package
# because this is a permitted way to scrape Twitter.


# This function make sum and mean of the number of Twitter followers in each podcast title
feature_tw_follow <- function(df){
  
  # I joined artist names in each title and the number of followers(key = artist name)
  foo <- df %>% filter(!duplicated(title)) %>% rename(name = person) %>%
    unnest_tokens(person, name, token = stringr::str_split, pattern = "\r\n", to_lower = F) %>% 
    left_join(url_total %>% 
                select(person, tw_follow, google))
  
  goo <- foo %>% 
    
    mutate(tw_follow = ifelse(is.na(tw_follow), -0.001, tw_follow),
           google = ifelse(is.na(google), -0.001, google)) %>% 
    
    group_by(title) %>% summarise(sum_tw = sum(tw_follow, na.rm = T),
                                  mean_tw = mean(tw_follow, na.rm = T),
                                  max_tw = max(tw_follow, na.rm = T),
                                  sum_gle = sum(as.numeric(google), na.rm = T)) %>% 
    
    mutate(sum_tw = ifelse(sum_tw < 0, NA, sum_tw),
           mean_tw = ifelse(mean_tw < 0, NA, mean_tw),
           max_tw = ifelse(max_tw < 0, NA, max_tw),
           sum_gle = ifelse(as.numeric(sum_gle) < 0, NA, sum_gle))
  
  x <- grep(pattern = "[^[:ascii:]]", goo$title, perl = TRUE) 
  goo$non_english <- rep(0, nrow(goo))
  goo$non_english[x] <- 1
  
  df <- df %>% left_join(goo, by = "title")
  
  return(df)
}

train <- feature_tw_follow(train)
test <- feature_tw_follow(test)




### Quantile encoding of continous features.

# I transformed continuous features into ordinal categorical features using quantile cutting.
# I used 5, 10, and 20 quantile cutting to avoid overfitting.

quantile_continous <- function(train, test, col){
  
  x_5 <- quantile(train %>% select(!!sym(col)) %>% unlist(), seq(0, 1.0, 0.2), na.rm = T)
  x_5[1] <- -Inf
  x_10 <- quantile(train %>% select(!!sym(col)) %>% unlist(), seq(0, 1.0, 0.1), na.rm = T)
  x_10[1] <- -Inf
  x_20 <- quantile(train %>% select(!!sym(col)) %>% unlist(), seq(0, 1.0, 0.05), na.rm = T)
  x_20[1] <- -Inf
  
  if(n_distinct(x_20) == 21){
    
    train <- train %>% 
      mutate(x = as.numeric(cut(!!sym(col), x_5)),
             y = as.numeric(cut(!!sym(col), x_10)),
             z = as.numeric(cut(!!sym(col), x_20))
      )
    
    test <- test %>% 
      mutate(x = as.numeric(cut(!!sym(col), x_5)),
             y = as.numeric(cut(!!sym(col), x_10)),
             z = as.numeric(cut(!!sym(col), x_20))
      )
    
    colnames(train)[(ncol(train)-2):ncol(train)] <- c(str_c(col, "_5"), str_c(col, "_10"), str_c(col, "_20"))
    colnames(test)[(ncol(test)-2):ncol(test)] <- c(str_c(col, "_5"), str_c(col, "_10"), str_c(col, "_20"))
  }
  
  else if(n_distinct(x_10) == 11){
    
    train <- train %>% 
      mutate(x = as.numeric(cut(!!sym(col), x_5)),
             y = as.numeric(cut(!!sym(col), x_10))
      )
    
    test <- test %>% 
      mutate(x = as.numeric(cut(!!sym(col), x_5)),
             y = as.numeric(cut(!!sym(col), x_10))
      )
    
    colnames(train)[(ncol(train)-1):ncol(train)] <- c(str_c(col, "_5"), str_c(col, "_10"))
    colnames(test)[(ncol(test)-1):ncol(test)] <- c(str_c(col, "_5"), str_c(col, "_10"))
  }
  else if(n_distinct(x_5) == 6){
    
    train <- train %>% 
      mutate(x = as.numeric(cut(!!sym(col), x_5))
      )
    
    test <- test %>% 
      mutate(x = as.numeric(cut(!!sym(col), x_5))
      )
    
    colnames(train)[ncol(train)] <- c(str_c(col, "_5"))
    colnames(test)[ncol(test)] <- c(str_c(col, "_5"))
  }
  
  return(list(train = train, test = test))
}

foo <- quantile_continous(each_tr, each_te, "mean_tw")
each_tr <- foo$train
each_te <- foo$test



### Mean(Target) Encoding

# I used Mean Encoding for high dimensional categorical features. Some feature have more than 1000 categories
# And the sample size was just 5148, so processing these features is necessary.
# I can consider re-grouping, embedding and Mean encoding, but I used this.
# Because, first I want to avoid any manual process, and there does not seem to be a proper criteria for re-grouping.
# And there is not enough sample size to use learning procedure in the "Embedding" method.
# For Mean encoding, I used 2-phases cross-validation for its labels in order to add some noise and avoid leakage from the target.

mean_encoding <- function(df, test, col){
  
  value1 <- list()
  value2 <- list()
  for(j in 1:20){
    
    folds <- createFolds(df$target, k = 20, list = TRUE, returnTrain = FALSE)
    train_set <- df %>% dplyr::slice(-folds[[j]])
    
    for(i in 1:10){
      folds2 <- createFolds(train_set$target, k = 10, list = TRUE, returnTrain = FALSE)
      train_set2 <- train_set %>% dplyr::slice(-folds[[i]])
      value2[[i]] <- train_set2 %>% 
        group_by(!!sym(col)) %>% 
        summarise(mean = mean(target, na.rm = T)) %>% 
        select(-1)
    }
    
    value1[[j]] <- rowMeans(do.call(cbind, value2), na.rm = T)
  }
  
  value <- rowMeans(do.call(cbind, value1), na.rm = T)
  levels <- sort(unlist(unique(tr[ ,col])))
  encoding_reference <- tibble(levels = levels,
                               value = value)
  
  colnames(encoding_reference) <- c(col, str_c("enco_", col))
  train <- df %>% left_join(encoding_reference)
  test <- test %>% left_join(encoding_reference)
  
  return(list(train = train, test = test))
}

foo <- mean_encoding(tr, te, "mean_tw_5")
tr <- foo$train
te <- foo$test




### Re grouping artist column

# I need this processing before mean encoding, because there are many category levels that only has one sample size.
# So, I first embedded low frequency artist names into "etc", and then I only kept significant artist names
# by fitting linear regression with all other covariates fixed.
# So, only significant and frequent artist names are remained, and the others are embedded into "etc"

regrouping <- function(train, test, col, k){
  
  goo <- train %>% 
    group_by(!!sym(col)) %>% 
    summarise(mean = mean(target), 
              count = n()) %>%
    filter(count >= k) %>% 
    arrange(desc(mean))
  
  roo <- train
  roo[ ,col] <- ifelse(unlist(train[ ,col]) %in% unlist(goo[ ,col]), unlist(train[ ,col]), "etc")
  
  foo <- roo %>% 
    mutate(value = 1) %>% 
    spread(!!sym(col), value,  fill = 0) %>% 
    as.data.frame()
  
  begin <- ncol(roo) + 1
  
  for(i in begin:ncol(foo)){
    
    foo[ ,i] <- as.factor(foo[ ,i])
  }
  
  
  fit_reg <- function(df, col){
    
    df <- df %>% select(one_of(base), !!sym(col))
    fit <- lm(target ~ ., df)
    return(c(summary(fit)$coef[5,1], summary(fit)$coef[5,4]))
  }
  
  x <- as.character(unique(unlist(roo[ ,col])))
  p_newcategory <- do.call(rbind, lapply(x, fit_reg, df = foo))
  
  bar <- tibble(feature = x, pvalue = p_newcategory[, 2], est = p_newcategory[, 1]) %>% 
    mutate(pvalue = round(pvalue, 3),
           est = round(est, 2),
           sig = ifelse(pvalue < 0.05, 1, 0))
  
  sig_levels <- bar$feature[bar$sig == 1]
  train[ ,col] <- ifelse(unlist(train[ ,col]) %in% sig_levels, unlist(train[ ,col]), "etc")
  test[ ,col] <- ifelse(unlist(test[ ,col]) %in% sig_levels, unlist(test[ ,col]), "etc")
  
  return(list(train = train, test = test))
}

foo <- regrouping(tr, te, "artist", 10)
tr <- foo$train
te <- foo$test


### Text raw data


# I lemmatized all words in the summary variable because I am gonna investigate the effect of each word.
# So, this process can increase the sample size of each incidence.

tr$summary_lem <- lapply(tr$summary, lemmatize_strings) %>% unlist() %>% tolower()
te$summary_lem <- lapply(te$summary, lemmatize_strings) %>% unlist() %>% tolower()


# This function returns the count of each word through all podcasts' summary. But I didn't count duplicated cases.
# I mean if "Apple" appears several times in one podcast, I only count 1. 
# Because I need to know the sample size of incidence that contain "Apple".

get_word_count_matrix <- function(train, col_string, col_id, k){
  
  token <- train %>% 
    unnest_tokens(word, !!sym(col_string)) %>% 
    anti_join(stop_words)
  
  x <- unique(token$word)
  a <- list()
  word_count <- c()
  for(i in 1:length(x)){
    a[[i]] <- token %>% group_by(!!sym(col_id)) %>% summarise(count = sum(x[i] %in% word))
    word_count[i] <- sum(a[[i]]$count)
  }
  
  word_count_matrix <- tibble(word = x, count = word_count) %>% 
    arrange(desc(count))
  
  word_count_matrix <- word_count_matrix %>% filter(count >= k)
  return(word_count_matrix)
}

word_count_matrix_org <- word_count_matrix

# And then I only kept words that have enough sample size to avoid noises.
# So this words list is keywords that I will check the effects on the target variable.
word_count_matrix <- word_count_matrix_org %>% filter(count >= 25)


# After listing keywords, I checked its linear effect on the target variable.
# I added other features as covariates to only see the effect of each keyword.
# The reason why I used linear regression is that this would be enough to see whether there is effect or not,
# the relationship is so clear, and calculation is fast.

fitting_with_base <- function(df, form){
  
  result <- list()
  for(i in 1:nrow(word_count_matrix)){
    
    if(i %% 50 == 0){
      print(i)
    }
    
    df <- df %>% 
      mutate(word = ifelse(str_detect(summary_lem, word_count_matrix$word[i]), 1, 0)) %>% 
      mutate(word = as.factor(word))
    
    fit <- lm(form, data = df)
    
    result[[i]] <- c(summary(fit)$coef[length(base)+1 ,1], summary(fit)$coef[length(base)+1, 4])
  }
  
  result <- do.call(rbind, result)
  result <- result %>% as.tibble() %>% rename(est = V1, pvalue = V2) %>% 
    mutate(est = round(est, 2), 
           pvalue = round(pvalue, 3),
           sig = ifelse(pvalue <= 0.05, 1, 0),
           word = word_count_matrix$word)
  
  return(result)
}

# I only kept the significant keywords list from the linear regression.
result <- fitting_with_base(tr, form) %>% filter(sig == 1)


# Now, I made keywords effect in a continuous feature to avoid high-dimensionality.
# To do this, I used the size of beta coefficients in the previous linear regression.
# I made two columns, one is just the sum of these beta coefs, and the other is maximum value.
# These sum and maximum is calculated in all each podcast checking the summary column.
# Function below does this process for train and test set.

feature_word <- function(train, test){
  
  bar <- result %>% filter(sig == 1) %>% 
    left_join(word_count_matrix %>% select(word, count))
  
  bar <- quantile_continous(bar, bar, "est")$train %>% 
    rename(group = est_10)
  
  x <- grep(pattern = "[^[:ascii:]]", bar$word, perl = TRUE) 
  bar <- bar %>% 
    mutate(group = as.character(group))
  
  bar$group[x] <- "nonenglish"
  
  too <- bar %>% group_by(group) %>% summarise(mean = mean(est))
  bar <- bar %>% left_join(too)
  
  join <- function(df, col_id){
    
    token <- df %>% unnest_tokens(word, summary_lem)
    
    a <- token %>% 
      left_join(bar %>% select(word, mean), by = "word") %>% 
      mutate(mean = ifelse(is.na(mean), 0, mean))
    
    a <- a %>% group_by(!!sym(col_id)) %>% 
      filter(!duplicated(word)) %>% 
      summarise(score_sum = sum(mean, na.rm = T),
                score_max = max(abs(mean), na.rm = T)) %>% 
      mutate(score_max = ifelse(score_sum < 0, -score_max, score_max))
    
    df <- df %>% left_join(a)
    return(df)
  }
  train <- join(train, "title")
  test <- join(test, "title")
  
  return(list(train = train, test = test))
}


foo <- feature_word(tr, te)
tr <- foo$train
te <- foo$test

# After I get new continous columns of keywords' effect, I did quantile cutting to avoid overfitting.
# Also, I did mean encoding to reduce its dimension.
foo <- quantile_continous(tr, te, "score_sum")
tr <- foo$train
te <- foo$test

foo <- mean_encoding(tr, te, "score_sum_5")
tr <- foo$train
te <- foo$test




##### Hyper parameters tuning for Xgboost model

# I did Hyper parameters tuning using Bayesian Optimization. 
# For this competition, "Grid search" would be better because the data size is small, but I just wanted to practice Bayesian method.
# There are two parts, first one is to make the error function that is gonna be used as a reference to check the performance.
# The other one is processing bayesian searching changing hyper parameters.
# So, the optimization function searches best set of hyper parameters based on the error function as a reference.
# I used 10 out of fold errors for the reference error function.

df <- tr
base <- base
k_folds <- 10
seed1 <- 10
seed2 <- 20

# This function returns 10 out of folds errors in Xgboosting.
# I will search "max_depth, min_child_weight, and subsample" as hyper parameters.
error_cv <- function(max_depth, min_child_weight, subsample){
  
  cv_xgb <- function(df, base, k_folds, seed, max_depth, min_child_weight, subsample){
    
    prediction_1 <- list()
    new_data <- list()
    set.seed(seed)
    folds <- createFolds(df$target, k = k_folds, list = TRUE, returnTrain = FALSE)
    
    for(j in 1:k_folds){
      
      dtrain <- df %>% dplyr::slice(-folds[[j]]) %>% select(one_of(base))
      dtest <- df %>% dplyr::slice(folds[[j]]) %>% select(one_of(base))
      new_data[[j]] <- dtest
      
      
      y <- createDataPartition(dtrain$target, p = 0.8)$Resample1
      dtrain <- dtrain[y, ]
      dvalid <- dtrain[-y, ]
      
      y_train <- dtrain$target
      y_valid <- dvalid$target
      y_test <- dtest$target
      
      dtrain <- dtrain %>% select(-target)
      dvalid <- dvalid %>% select(-target)
      dtest <- dtest %>% select(-target)
      
      dtrain <- as.matrix(dtrain)
      dvalid <- as.matrix(dvalid)
      dtest <- as.matrix(dtest)
      
      dtrain <- xgb.DMatrix(data = dtrain, label = y_train)
      dval <- xgb.DMatrix(data = dvalid, label = y_valid)
      cols <- colnames(dtrain)
      
      p <- list(objective = "reg:linear",
                booster = "gbtree",
                eval_metric = "rmse",
                nthread = 4,
                eta = 0.02,
                max_depth = max_depth,
                min_child_weight = min_child_weight,
                gamma = 0,
                subsample = subsample,
                colsample_bytree = 0.8,
                colsample_bylevel = 0.8,
                alpha = 0,
                lambda = 0,
                nrounds = 500)
      
      fit_xgb <- xgb.train(p, dtrain, p$nrounds, list(val = dval), print_every_n = 30, early_stopping_rounds = 15)
      prediction_1[[j]] <- predict(fit_xgb, dtest)
      
    }
    
    whole_pred <- unlist(prediction_1)
    new_data <- do.call(rbind, new_data)
    cv_error <- rmse(new_data$target, whole_pred)
    new_data$target <- whole_pred
    
    return(list(new_data = new_data, cv_error = cv_error))
  }
  
  cv_final <- cv_xgb(tr, base, 10, 20, max_depth, min_child_weight, subsample)$cv_error
  
  return(list(Score = -cv_final, Pred = 0))
}

# Now I can process Baysian Opimization based on the reference error function above.
OPT_Res <- BayesianOptimization(second_stage_cv, 
                                bounds = list(max_depth = c(2L, 6L),
                                              min_child_weight = c(1L, 15L),
                                              subsample = c(0.6, 0.8)),            
                                init_grid_dt = NULL, init_points = 10, n_iter = 20,
                                acq = "ucb", kappa = 2.576, eps = 0.0,
                                verbose = TRUE)



##### Some works for Time Series.

# This podcasts have time series, and the series is collected hourly.
# The periods ranges from 1 day to 80 days. But most of the podcasts are observed within a day.
# But there are still many podcasts that have more than 20 days, and 5% of the podcasts have
# large variances in their times series, so I need to model this.
# I will use daily data, not hourly base, because I don't think hourly data is so meaningful to find patterns in this case.



# This process is to get "Day count" feature to use as a offset term.
# I mean, in each podcast, all hourly data in the first day are coded as 1, 
# and all hourly data in the second day are coded as 2, and on and on...

foo <- train %>% 
  group_by(title, date) %>% 
  summarise(count = n()) %>% 
  mutate(day_count = 1:n())

train <- train %>% left_join(foo, by = c("title", "date"))

foo <- test %>% 
  group_by(title, date) %>% 
  summarise(count = n()) %>% 
  mutate(day_count = 1:n())

test <- test %>% left_join(foo, by = c("title", "date"))


# I can only do time series analysis for the titles that are common in both of the train and the test sets.
title_common <- intersect(each_tr$title, each_te$title)

# And I only kept titles that their variations in times are high, and also have more than 10 days of periods.
titles <- target_features %>% filter(sd > 0.01 & n >= 360)


# So, I have now titles lits of more than 10 days & large variation in time series.
# I will make simple model for these titles using just a offset term(Day count)
# Because of physical time limit, I will not do modeling for dependency structure such as ARIMA
# And just adding "Day count", the R-squared was about 95% in most of the podcasts.
# So, this simple model would be okay.


result <- list()
for(i in 1:length(title_group_2)){
  
  foo <- train %>% 
    filter(title == title_group_2[1]) %>% 
    group_by(date) %>% 
    summarise(target_daily = max(target)) %>% 
    top_n(7, wt = date)
  
  foo <- foo %>% mutate(t = 1:nrow(foo))
  
  fit <- lm(target_daily ~ t, data = foo)
  
  result[[i]] <- tibble(coef_t = summary(fit)$coef[2,1],
                        p_t = summary(fit)$coef[2,4])
  
}

result <- do.call(rbind, result)
result <- result %>% mutate(title = title_group_2,
                            coef_t = coef_t,
                            p_t = p_t,
                            sig = ifelse(p_t < 0.05, 1, 0))

# Above, I got all beta coefficients of "Day count" feature in all podcasts.
# But, I will not use this individual effects podcast by podcast because the sample size is not enough.
# The sample size is time period(1 day to 80days). So, I will average their size of effects to make more robust.

# I used trimmed mean for "Day count" effect. So, this effect will be used to forecast the time series.
avg_t_coef <- mean(result$coef_t, na.rm = T, trim = 0.2)

