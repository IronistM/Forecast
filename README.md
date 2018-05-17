# Forecast
My R code for Forecasting

These are my efforts for creating R code that forecasts sales or financial instruments.

1)Statistical models either in plain or function form apply techniques from the forecast package and prophet from Facebook.
The example is on monthly items sales.

There is a first stage, where the techniques are trained with the 75% of the sample and then tested against the remaining hold-out sample.
Then based on RMSE we select a winner, which in this case is prophet and train it with the whole sample so as to predict the next month's sales.

2)The financial indices code applies the same techniques on the % difference of closing from opening price of those indices
It seems that prophet performs better.
