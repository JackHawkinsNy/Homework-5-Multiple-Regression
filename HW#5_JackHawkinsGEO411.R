#Multivariate Statistics for Geographers
#Homework #5 
#Jack Hawkins

sales <- read.csv("homeSales.csv")
names(sales)
base.model <- lm(price ~ bedrooms + bathrooms +
                   sqft_living + yr_built + HS + SC, data = sales)
summary(base.model)
plot(base.model)

ResidualVsXPlots <- function(mod.in) {
  var.names <- names(mod.in$coefficients)
  n.x.vars <- length(var.names)
  mod.e <- residuals(mod.in)
  
  par(mfrow = c(2, 2))  # adjust layout if needed
  
  for (i in 2:n.x.vars) {
    plot(mod.in$model[, var.names[i]], mod.e,
         xlab = var.names[i], ylab = "residuals")
    lines(lowess(mod.in$model[, var.names[i]], mod.e, f = 3/4),
          col = "red")
  }
}
ResidualVsXPlots(base.model)

XScaleLocationPlots <- function(mod.in){
  var.names <- names(mod.in$coefficients)
  n.x.vars <- length(var.names)
  std.residuals <- sqrt(abs(rstandard(mod.in)))
  for (i in 2:n.x.vars){
    plot (mod.in$model[,var.names[i]], std.residuals,
          xlab = var.names[i],
          ylab = "Square root of Absolute Standardized
Residuals")
    lines(lowess(mod.in$model[,var.names[i]],
                 std.residuals, f=3/4), col="red")
    locator(1)
  }
}
XScaleLocationPlots(base.model)
install.packages(c("car"))
library(car)
vif(base.model)

#Part 2 Remodeling the Function
sales$log_price <- log(sales$price)
sales$sqft2 <- sales$sqft_living^2
improved.model <- lm(log_price ~ bedrooms + bathrooms +
                       sqft_living + I(sqft_living^2) + yr_built  + latitude + longitude +
                       BACH + prop_poverty, data = sales) 
summary(improved.model)
ResidualVsXPlots(improved.model)
XScaleLocationPlots(improved.model)
##I think I can do a little better on getting a higher r squared value.

##Improved.model2:model chosen for assignment written response.
sales$log_price <- log(sales$price)
sales$sqft2 <- sales$sqft_living^2
improved.model <- lm(log_price ~ bedrooms + bathrooms +
                       sqft_living + I(sqft_living^2) + yr_built +
                       waterfront + latitude + longitude +
                       BACH + GRAD + prop_poverty, data = sales) 

summary(improved.model)
ResidualVsXPlots(improved.model)
XScaleLocationPlots(improved.model)
##Model that proved interference by prop_poverty
improved.model <- lm(log_price ~ bedrooms + bathrooms +
                       sqft_living + I(sqft_living^2) + yr_built +
                       waterfront + latitude + longitude +
                       BACH + GRAD, data = sales) 
summary(improved.model)
