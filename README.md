# Forecast
My R code for Sales Forecasting
These are my efforts for creating R code that forecasts sales or financial instruments.

Statistical models either in plain or function form apply techniques from the forecast package and prophet from Facebook.
The example is on monthly items sales.

There is a first stage, where the techniques are trained with the 75% of the sample and then tested against the remaining hold-out sample.
Then based on RMSE we select a winner, which in this case is prophet and train it with the whole sample so as to predict the next month's sales.

The FTSE 100 code applies the same techniques on the % difference of closing from opening price of the index
It seems that prophet performs better.
