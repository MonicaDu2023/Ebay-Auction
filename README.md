# Ebay-Auction

## Data Source
Lewis (2011)’s paper “Asymmetric Information, Adverse Selection and Online Disclosure: The Case of eBay Motors”

## Purpose 
find out how the auction itself would impact revenue of sellers.

## Project Overview
   * Perform Regression on all 90 variables that describes auction, we pick 25 of them as our final full set of covariates. Then, we use backwards stepwise subset selection combined        with 10-fold cross validation to further eliminate our covariates. The minimum test sample MSE lies in a model using 9 variables, inspection, featured, numbids, miles, phone,          photos, length, dealer, webpageebizauto.
   * To get a more precise inspection and prediction, we choose 5 variables about car to cluster the observations. What we find out is that bookvalue dominated the clustering process,      so we conduct clustering again only using bookvalue. The result is similar with the one above. On each group we divided, we develop a model through backward subset selection            combined with 10-fold cross validation. One common variable, photos, appears on every model and would improve revenue by about 2% overall.
   * We use both Lasso and Ridge methods to penalize the magnitude of coefficients in order to get a better prediction ability in test samples. 
## Summary
Overall, adding more covariates to our model do improve the prediction, and the more information sellers display, they would get a better price. Besides that, bookvalue do explain a lot about how much money the seller would actually get.
