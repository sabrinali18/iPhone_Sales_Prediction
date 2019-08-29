# Sales Prediction with Time Series Modeling

### Objective
The goal of this project is to predict the future sales of iPhones using sentiment analysis results generated from Reddit posts.

### Files
  - scrape_reddit.ipynb: Script for scraping subreddits from discussion group of iPhone on Reddit (https://www.reddit.com/r/iphone/)
  - first_run.R: Run Autoregressive moving average (ARMA) model as our baseline and compare it with Vector Autoregression (VAR) model
  - GPR.ipynb: Run Gaussian Process Regression (GPR) model and use four kernels (Squared Exponential Kernel, Matern Kernel, Radial Basis Kernel, and White Noise Kernel) altogether to explain different components of the sales growth.
  - Project_Report: Final report for project methodology and results.
