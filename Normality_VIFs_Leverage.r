---
title: ""
output:
  pdf_document: default
  word_document: default
  html_document:
    df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```



------------------------------------------------------------------------

## Problem 1

Using the sat data set (type following commands in R Console: library(faraway);data("sat")). Display any plots that are relevant. Do not provide any plots about which you have nothing to say. Suggest possible improvements or corrections to the model where appropriate. (35 points, 5 points each)

```{r}
library(faraway)
data("sat")
```

a-) Fit a model with the total SAT score as the response and expend, salary, ratio and takers as predictors.

```{r}
a7q1.reg=lm(total~expend+ratio+salary+takers,data = sat)
summary(a7q1.reg)
```

\textcolor{blue}{The fitted regression model is: Total=1045.97+4.46expend-3.62ratio+1.64salary-2.9takers.}

Perform regression diagnostics on this model to answer the following questions.

b-) Check the constant variance assumption for the errors.

```{r}
plot(a7q1.reg,which=1)
```

```{r}
# install olsrr for Breusch-pagan check
library(olsrr)
```

```{r}
ols_test_breusch_pagan(a7q1.reg)
```

\textcolor{blue}{Assume $\alpha=0.05$, P-value=0.4>0.05 not significant. Fail to reject $H_0$, errors variance are constant. And we can also see constant errors variacnes from the above Residuals vs. Fitted plot.}

c-) Check the normality assumption.

```{r}
plot(a7q1.reg,which = 2)
```

\textcolor{blue}{Q-Q residual plot looks normal distributed and it does not show heavy tails.}

d-) Check for large leverage points.

```{r}
plot(a7q1.reg,which = 5)
```

\textcolor{blue}{From the above Residuals vs Leverage plot we do not see any outliers just the point of Utah falls almost on boundary.}

```{r}
ols_plot_resid_lev(a7q1.reg)
```

e-) Check for outliers.

```{r}
library("tidyverse", "ggstatsplot")
```

```{r}
a7q1.reg$residuals
```

```{r}
boxplot(a7q1.reg$residuals,main="Boxplot of Residuals",ylab="Residuals")
```

\textcolor{blue}{By the above box plot, we can see the point of West Virginia is outlier.}

```{r}
library(car)
outlier_test1 <- outlierTest(a7q1.reg)
outlier_test1 
```

\textcolor{blue}{By Bonferroni p method we see p-value for west virginia is less than 0.05 so it is significant, then reject $H_0$, West Virginia is an outlier.}

f-) Check for influential points.

```{r}
plot(a7q1.reg,which=4)
```

\textcolor{blue}{By above Cook's distance plot, a data point with Cook's distance>1 is probably influential. Cook's distance>0.5 is at least worth investigation. For our case no point Cook's distance is greater than 0.5}

```{r}
library(car)
```

```{r}
influencePlot(a7q1.reg, id="noteworthy", main="Influence Plot",sub="Circle size is proportional to Cook's distance")
```

```{r}
influenceIndexPlot(a7q1.reg)
```

g-) Check the structure of the relationship between the predictors and the response.

```{r}
# plot the residuals against each predictor variable in the regression model to check whether a curvature effect for that variable is required in the model.
plot(sat$expend,a7q1.reg$residuals)
```

```{r}
plot(sat$ratio,a7q1.reg$residuals)
```

```{r}
plot(sat$salary,a7q1.reg$residuals)

```

```{r}
plot(sat$takers,a7q1.reg$residuals)
```

\textcolor{blue}{Per the structure of the relationship between the predictors and the response, one way is to plot the residuals against each predictor variable in the regression model to check curvature effect for that variable is required in the model.}

## Problem 2

Use the divusa data under library(faraway).(type following commands in R Console: library(faraway);data("divusa")) (35 points, 5 points each)

```{r}
library(faraway)
data("divusa")
```

a-) Fit a regression model with divorce as the response and unemployed, femlab, marriage, birth and military as predictors.

```{r}
a7q2.reg=lm(divorce~unemployed+femlab+marriage+birth+military,data = divusa)
summary(a7q2.reg)
```

\textcolor{blue}{The regression model is: divorce=2.49-0.11unemployed+0.38femlab+0.12marriage-0.13birth-0.03military}

b-) For the same model, compute the VIFs. Is there evidence that collinearity causes some predictors not to be significant? Explain.

```{r}
vif(a7q2.reg)
```

\textcolor{blue}{As all VIF values are between 1 and 5, it means variables are moderately correlated.}

c-) Does the removal of insignificant predictors from the model reduce the collinearity? Investigate.

```{r}
a7q2c.reg=lm(divorce~femlab+marriage+birth,data = divusa)
summary(a7q2c.reg)
```

```{r}
vif(a7q2c.reg)
```

\textcolor{blue}{By removing the unemployment rate and military these two factors, redo the regression model the vif goes down for femlab, marriage and birth.}

d-) Use the best subset and stepwise variable selection methodologies using the full model.

```{r}
k=ols_step_all_possible(a7q2.reg)
plot(k)
```

```{r}
k1=ols_step_best_subset(a7q2.reg)
plot(k1)
```

```{r}
#a7q2d.reg=regsubsets(divorce~unemployed+femlab+marriage+birth+military,data = divusa)
#summary(a7q2d.reg)
```

```{r}
#summary(a7q2d.reg)$cp
```

\textcolor{blue}{From the $C_p$ value we see if only one decision variable using femlab, it has highest $C_p$ value 80.5}

e-) Using the best model in part d, compute the VIFs. Explain.

```{r}
vif(a7q2.reg)
```

f-) Check for influential and outliers points for the best model in part d.

```{r}
library(car)
```

```{r}
influencePlot(a7q2.reg, id="noteworthy", main="Influence Plot",sub="Circle size is proportional to Cook's distance")
```

```{r}
influenceIndexPlot(a7q2.reg)
```

\textcolor{blue}{Per the above plots we can see the influence plot.}

```{r}
boxplot(a7q2.reg$residuals,main="Boxplot of Residuals",ylab="Residuals")
```

```{r}
a7q2.reg$residuals
```

```{r}
max(a7q2.reg$residuals)
```

```{r}
min(a7q2.reg$residuals)
```

\textcolor{blue}{From the above boxplot we can see two outliers, the point with residual value 3.83 and the point with residual -3.86 are outliers.}

```{r}
library(car)
outlier_test2 <- outlierTest(a7q2.reg)
outlier_test2 
```

\textcolor{blue}{By Bonferroni p method, we see p value is 0.005<0.05, so it is significant and we reject hypothesis $H_0$ then we see point 27 is outlier.}

g-) Check for large leverage points.

```{r}
ols_plot_resid_lev(a7q2.reg)
```

## Problem 3

Refer to Brand preference data, build a model with all independent variables. Then use the full model to solve the problems below (30 pts, 5 points each)

a-) Obtain the studentized deleted residuals and identify any outlying Y observations. State your conclusion.

```{r}
a7q3data=read.csv("/cloud/project/Brand Preference.csv")
```

```{r}
library(lmtest)
```

```{r}
model = lm(Y~.,data = a7q3data)
summary(model)
```

```{r}
# Calculate studentized residuals
studentized_residuals = rstudent(model)
```

```{r}
studentized_residuals
```

```{r}
# Get the largest absolute studentized deleted residual
largest_residual <- studentized_residuals[which.max(abs(studentized_residuals))]
largest_residual
```

```{r}
library(car)
outlier_test <- outlierTest(model)
outlier_test 
```

\textcolor{blue}{P-value is 0.057>0.05, so we cannot reject $H_0$ hypothesis, it is not an outlier point.}

```{r}
boxplot(model$residuals,main="Boxplot of Residuals",ylab="Residuals")
```

b-) Obtain the diagonal elements of the "HAT" matrix, and provide an explanation for the pattern in these elements.

```{r}
#R can directly calculate matrix quantities of interest. First, we extract the X "design" matrix
x = model.matrix(~.,a7q3data)
library(reshape2)
#install.packages("kableExtra")
library(knitr)
library(kableExtra)
kable(head(x), format = "html")
```

```{r}
#And here the response Y

y=a7q3data$Y

#Here we obtain the diagonal elements of the "HAT" matrix
H<-x%*%solve(t(x)%*%x)%*%t(x)
diag(H)
```

```{r}
#install.packages("plotly")
library(plotly)
```

```{r}
# Visualizing the pattern
data <- data.frame(x = 1:16, y = H )

pg <- ggplot(data, aes(x = x, y = y)) +
  geom_point() 

pg <- ggplot(data, aes(x = x, y = y)) +  geom_point()
print(pg)
```

\textcolor{blue}{By the above HAT figure we see they have a kind of linear relationships among the points by HAT matrix.}

c-) Are any of the observations outlying with regard to their X values according to the rule of thumb stated in chapter 10 (page #399 equation 10.28) of "Applied Linear Statistical Models, 5th Edition, by Kutner, Nachtsheim, Neter & Li"?

```{r}
#From the book: "..leverage values greater than 2p/n are considered by this rule to indicate outlying cases with regard to their X values. Another suggested guideline is that hii values exceeding .5 indicate very high leverage, whereas those between .2 and .5 indicate moderate leverage"

n <- nrow(a7q3data)
p <- length(model$coefficients)

H[H>2*p/n]
```

```{r}
# Another hat plot
hat.plot <- function(model) {
  plot(hatvalues(model), main="Another Plot of Hat Values")
# Add a reference line to indicate values between .2 and .5 (top of the graph)
  abline(h=c(2,5)*p/n, col="red", lty=5)
  # Allow interactive identification of points on the plot
  identify(1:n, hatvalues(model), names(hatvalues(model)))
}

hat.plot(model)
```

\textcolor{blue}{From the graph no point above the rule of thumb, only moderate leverage points here.}

d-) The largest absolute studentized deleted residual is for case 14. Obtain the DFFITS, DFBETAS, and Cook’s distance values for this case.

```{r}
# First we obtain the largest absolute studentized deleted residual for case 14
largest_residual
```

```{r}
# Now we obtain the DFFITS
library(stats)
#This suite of functions can be used to compute some of the regression (leave-one-out deletion) diagnostics for linear and generalized linear models discussed in Belsley, Kuh and Welsch (1980), Cook and Weisberg (1982), etc.

a=influence.measures(model)

a$infmat[14,]
```

```{r}
library(car)
outlier_test_1 <- outlierTest(model)
outlier_test_1 
```

\textcolor{blue}{By Bonferroni method we see the p-value is 0.057>0.05 so it is not significant. Then we do not reject $H_0$ that is no outlier is detected.}

e-) Remove case 14 from your data and refit the regression model and compare it against regression model with all data points. Do you see any differences?

```{r}
# New Data 

new_data = a7q3data[-14,]

# Fit model as directed
model2 <- lm(Y ~ ., data = new_data)

# Output model summary
summary(model2)
```

```{r}
plot(model2)
```

```{r}
# Confirm with a visual

library(car)
influencePlot(model2, id="noteworthy", main="Influence Plot",
              sub="Circle size is proportional to Cook's distance")
```

```{r}
ols_plot_resid_lev(model2)
```

```{r}
ols_plot_resid_lev(model)
```

\textcolor{blue}{By above outlier and leverage diagnostics for Y, we see the new regression model without outlier.}

```{r}
library(car)
outlier_test_2 <- outlierTest(model2)
outlier_test_2 
```

\textcolor{blue}{By Bonferroni method, we see p value is 0.07>0.05 not significant, so there is no outlier after moving point 14.}

```{r}
influenceIndexPlot(model2)
```

f-) Repeat the analysis in c-) but use the following rule "consider semistudentized residuals with absolute value of four or more to be outliers". Produce a list.

```{r}
n_1=nrow(a7q3data)
p_1=length(model$coefficients)
threshold=abs(qt(.05/(n*p),n-p-1))
threshold
```

```{r}
library(car)
# Extract residuals and leverage
residuals <- residuals(model)
leverage <- hatvalues(model)

# Calculate standard deviation of residuals
residual_sd <- sd(residuals)

# Compute semi-studentized residuals
semi_studentized_residuals <- residuals / (residual_sd * sqrt(1 - leverage))

# Identify observations with semi-studentized residuals larger than 3 in absolute value
outlier_indices <- which(abs(semi_studentized_residuals) > 3)

# Print the indices of potential outliers
print(outlier_indices)
```




