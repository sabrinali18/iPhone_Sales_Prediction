library(vars)
library(tseries)
library(forecast)

sentiment_dat <- read.csv('sentiment_score.csv', header = TRUE)
col_names <- colnames(sentiment_dat)
col_names[1] <- 'time_period'
colnames(sentiment_dat) <- col_names

normed_sentiment <- c(sentiment_dat['normed_value'])
normed_sentiment <- normed_sentiment$normed_value
adf.test(normed_sentiment)

sales_val <- c(sentiment_dat['sales']$sales)
adf.test(sales_val)
d_sales_val <- diff(sales_val)

sales_val <- ts(sales_val, frequency = 4)
fit <- tbats(sales_val)
seasonal <- !is.null(fit$seasonal)
seaadj_sales <- seasadj(fit)
d_seaadj_sales <- diff(log(seaadj_sales))
adf.test(d_seaadj_sales)

fore_vec <- c()

for(i in 15:length(d_seaadj_sales))
{
  best_aic <- 10000
  for(c1 in 0:4)
  {
    for(c2 in 0:4)
    {
      each_mod <- arima(d_seaadj_sales[1:i], order = c(c1,0,c2), method = 'ML')
      if(AIC(each_mod) < best_aic)
      {
        fore_val <- forecast(each_mod)[4]$'mean'[1]
        best_aic <- AIC(each_mod)
        best_mod <- each_mod
      }
    }
  }
  fore_vec <- c(fore_vec, fore_val)
}

fore_vec <- fore_vec[1:length(fore_vec) - 1]
real_val <- d_seaadj_sales[16:length(d_seaadj_sales)]
ar_rmse <- sqrt(mean((real_val - fore_vec)^2))

fore_vec <- c()

for(i in 15:length(d_seaadj_sales))
{
  best_aic <- 10000
  for(c1 in 0:4)
  {
    for(c2 in 0:4)
    {
      each_mod <- arima(d_seaadj_sales[1:i], order = c(c1,0,c2), method = 'ML', xreg = normed_sentiment[1:i])
      if(AIC(each_mod) < best_aic)
      {
        fore_val <- predict(each_mod, newxreg = normed_sentiment[1:i])[1]$pred[1]
        best_aic <- AIC(each_mod)
        best_mod <- each_mod
      }
    }
  }
  fore_vec <- c(fore_vec, fore_val)
}

fore_vec <- fore_vec[1:length(fore_vec) - 1]
real_val <- d_seaadj_sales[16:length(d_seaadj_sales)]
arx_rmse <- sqrt(mean((real_val - fore_vec)^2))


dat_var <- cbind(d_seaadj_sales, normed_sentiment[-1])
colnames(dat_var) <- c('adj_sales', 'sentiment_val')

fore_vec_var <- c()
for(i in 15:dim(dat_var)[1])
{
  best_aic <- 10000
  for(j in 1:4)
  {
    each_mod <- VAR(dat_var[1:i,], p = j, type = 'const')
    if(AIC(each_mod) < best_aic)
    {
      fore_val <- predict(each_mod)[[1]][[1]][1,1]
      best_aic <- AIC(each_mod)
      best_mod <- each_mod
    }
  }
  fore_vec_var <- c(fore_vec_var, fore_val)
}

fore_vec_var <- fore_vec_var[1:length(fore_vec_var) - 1]
real_val <- d_seaadj_sales[16:length(d_seaadj_sales)]
var_rmse <- sqrt(mean((real_val - fore_vec_var)^2))


num_posts <- c(sentiment_dat['number_posts'])
num_posts <- num_posts$number_posts
d_num_posts <- diff(log(num_posts))


dat_var2 <- cbind(d_seaadj_sales, normed_sentiment[-1], d_num_posts)
colnames(dat_var2) <- c('adj_sales', 'sentiment_val', 'num_posts')

fore_vec_var2 <- c()
for(i in 15:dim(dat_var2)[1])
{
  best_aic <- 10000
  for(j in 1:2)
  {
    each_mod <- VAR(dat_var2[1:i,], p = j, type = 'const')
    if(AIC(each_mod) < best_aic)
    {
      fore_val <- predict(each_mod)[[1]][[1]][1,1]
      best_aic <- AIC(each_mod)
    }
  }
  fore_vec_var2 <- c(fore_vec_var2, fore_val)
}

fore_vec_var2 <- fore_vec_var2[1:length(fore_vec_var2) - 1]
real_val <- d_seaadj_sales[16:length(d_seaadj_sales)]
var2_rmse <- sqrt(mean((real_val - fore_vec_var2)^2))
