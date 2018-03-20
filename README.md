# Big-Mart-Sales-Data-Prediction:

This project is part of the Analytics Vidya Compitition : Big Mart Sales Prediction III <br/>
Link to the competition: https://datahack.analyticsvidhya.com/contest/practice-problem-big-mart-sales-iii <br/>



# Language Used:
The predictive model was created using the R Statistcal Language <br/>


# Insights:

This sections I am giving the Insights that I think could help understand the data better. <br/>

### Item Outlet Sales vs Item MRP:

It can be seen in graph we can see that in sales Supermarket Type 1 dominates compared to other type of outlets. However, it is interesting to see the gaps in the prices around 60,130 and 200. There could be many reason for why there are gaps in the prices, it could be because prices for different categories differ and which led to the gaps.

<p align="center">
  <img src="https://user-images.githubusercontent.com/10596504/37639310-375edae2-2be7-11e8-95d6-ae4deaabf9e6.png" width="700"/>
</p>

### Item Visibility and Item Sales vs Item Type:

This graph indicates that when Item Visibility and for a item category is directly proportional to aggregate sales for that particular category. Those category, who had the highest visibility had the highest sales. This finding could help sell those categories which have lower sales and have high MRP. Categories like dairy could benefit as they are highly consumed by general population and have a varied range of prices.

<p align="center">
  <img src="https://user-images.githubusercontent.com/10596504/37639389-9cd9fa78-2be7-11e8-93b5-9e05faa35518.png" width="700"/>
</p>

### Item Visibility vs Item Outlet Sales

Our observations in the previous do not hold when we see at the sales of Items in different outlets. Here the grocery store does have a uniform distribution even for items which have higher visibility. Even for Supermarket Type 3 some of Items with high sales have lower visibility compared to other items.

<p align="center">
  <img src="https://user-images.githubusercontent.com/10596504/37639503-06464dd6-2be8-11e8-98f6-b4d74f4f2297.png" width="700"/>
</p>

# Model Description

The end goal of this Model is to predict the price of the Item. <br/>

Moreover, the Machine Learning algorithm that were implemented were **"Random Forest"**, **"Logisti Regression"**, **"Decision Trees"**


# Results

To finally summarize the results of the three algorithms we can that the optimal RMSE score for Model 1 and Model 2 obtained using the Decision Trees, Random Forest and Multivariate Regression are 1096.45 and 782.45, 1099.288 and 1142.778, 1136.41 and 1094.424 respectively. We can say that for Model 1 Decision Tree performs better than other algorithms and for Model 2 Multivariate Regression
performance is better compared to other algorithms. 

After implementing the algorithm’s, we submitted the solution for the Test data set that was provided by Analytics Vidya Big Mart Sales Competition. Random Forest performed better for Model 1 and Model 2 with RMSE score of 1152.916 and 1145.759
