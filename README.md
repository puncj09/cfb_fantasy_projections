# College Football Fantasy Projections
All Python and R code. Scrapes college football API. Data processing, fantasy point projection models, simulation, linear optimization

TL;DR --
I used both predictive and prescriptive analytics in an attempt to predict weekly fantasy points. These predictions were used to produce optimal daily fantasy lineups to try to win the large prize on DraftKings.

**Background**
In the fall of 2021, I set out to project fantasy points for college football. Those familiar with Daily Fantasy Sports (DFS) know that optimizing lineups is crucial in order to win money on sites like DraftKings (DK). Projecting fantasy points required scraping historical data, projecting player stats using models trained on those historical stats, and producing optimal lineups using linear optimization.

**College Football Database API Scraping (Python)**
The **cfdb** package in Python provided functionality to extract data from a college football database. Additional data processing was necessary to get the data ready for model training in R.

**Player Fantasy Point Projections (R)**
In R, I read in historical data generated in Python and performed additional work. This included calculating historical fantasy points and pulling in data from other sources (i.e., injury data to exclude that week's injured players; betting spread data used as model features in linear regression model). The linear regression model was trained separately for each position (QB, RB, WR). The model's results were considered average outcomes for a given week. However, those that play DFS know that it is important to understand the full distribution of possible outcomes because we want to predict players who score in their 99th percentile. Thus, the model's results were used as the mean in the multivariate distribution calculating teammate correlation. 

If the QB scores a ton of fantasy points, it usually means that a WR also does well. I used the multivariate distribution to capture historical correlations between all combinations of teammates. Then, *n* simulations were ran for each game using the results of linear regression and the multivariate correlation matrices. The goal of the simulations was to capture each player's "ceiling" outcome. 

**Linear Programming (Python)**
After simulating games several times, the results were used in a linear programming problem. This problem was similar to the knapsack problem where I wanted to maximize fantasy points under several constraints such as DraftKings salary and positional limits. Each simulation produced 1 optimal lineup, and the optimal lineups were summarized to show how often each player appeared in the "best" lineup.

**Results and Discussion**
This was a very convuluted process that needs to be consolidated into one programming language. Also, there are some bugs including the linear regression outputting results for players that don't play (backup QB, 4th-string WR, etc.). Also, name matching between sources causes a lot of missing data when merging. However, my model's predicted fantasy points were compared to several industry-leading fantasy projection sites and I was satisfied with the similarity. I believe that the additional steps that I took to account for correlation and ceiling outcomes can help set my process apart from others who use machine learning to set their lineups.
