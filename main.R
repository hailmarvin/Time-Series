t <- c(5,7,12,16,20)
xt <- c(4,12,18,21,24)

# xt depends on t
ts_model <- lm(xt~t)
summary(ts_model)

# Residual Analysis - Check assumptions of linear model
# 1. Normality check on residuals - histogram, qq plot
# 2. Homoscedasticity - Residuals should have constant variance
# 3. Independence of residuals

# Fitted model: yt=1.15+1.22
# Residuals = Observed - fitted

ts_model$residuals

hist(ts_model$residuals)

# Normal QQ Plot
plot(ts_model, 2)

# Homoscedasticity / Homogeneity of variance
plot(ts_model, 3)

# independence of residuals
library(car)

durbinWatsonTest(ts_model)
