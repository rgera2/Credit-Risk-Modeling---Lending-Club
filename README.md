# Loan_default-prediction_and_investment_strategies
# Background
Lending Club (LC) offers an online platform for matching borrowers seeking loans and lenders looking to make an investment. With lower operational costs than traditional lenders (banks), such online lending platforms leverage technology, data and analytics to bring quicker and more convenient financing for individual and small business borrowers from investors looking for attractive investment yields.

LC issues personal loans between $1000 and $40,000 for 36 to 60 month durations. Interest rates on these loans are determined based on a variety of information, including credit rating, credit history, income, etc. Based on this, LC assigns a grade for each loan, ranging from A for safest loans to G for highest risk; subgrades are also assigned within each grade. Loans are split into $25 notes, which investors can purchase. Interested investors can browse different loans the LC website, which shows the assigned loan grade and other information.

# Dataset
To facilitate investment, LC provides access to their data (https://www.lendingclub.com/info/download-data.action - sign up required).

# Analysis
In this assignment, I first explored the data on loans, developed an understanding of loan grades and subgrades and how they related to default and returns performance, loan purpose and any relation to performance, analyses of returns from loans, etc. I also handled the missing data accordingly. While the data carries information on over 100 variables, I had to determine what data would be available when looking to invest in a loan — since our goal wass to develop a model to predict loan default and then to decide which loans to invest in; such a model would thus be only able to consider variables available before a loan was issued.
The subsequent task was to develop models to identify god/bad loans, and to evaluate these. I also considered investment performance corresponding to these models and identified the best model.

# Models
Models used were - 

For prediction of defaulters: 
Decision Trees,
Random Forest,
GLM (Lasso and Ridge),
GBM
For calculation of annual returns:
RF, GLM and GBM

# Evaluation
Evaluation metrics considered were Test Accuracy, AUC and sensitivity(i.e. TPR as I wanted to catch the defaulters) to select the best model for prediction and RMSE for annual returns model.

# Strategy
On the basis of decile analysis (according to loan status and best returns), grades were divided into three types: Low Risk-Low Return, Moderate Risk-Moderate Return and High Risk-High return.

Every investor has a different risk appetite. He might be interested in either investing in loans that would give higher returns but are a little prone to risk or he might invest in low risk loans if returns are low but consistent.
