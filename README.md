# Overview
The dataset is cleaned and streamlined so that a Multiple Linear Regression model and four Simple Linear Regression Model can be established to analyze how the presence and the working condition of the fire alarm system (including the smoke detector) and the sprinkler system have an affect on estimated damage (in CAD) for residential fire cases.

This report aims to investigate the actual benefit of having a working fire alarm system and a sprinkler system in residential buildings by constructing a Multiple Linear Regression (MLR) model. The “benefit” of a working system is quantified and observed through the estimated loss of the fire (in CAD). Comparisons will be made between residential fire cases with/without the presence of a fire alarm system and a sprinkler system. The result of this research should raise awareness of the actual benefit of implementing such fire control mechanisms. In addition to the model, this report will provide a primary insight into 1) the most common origins of residential fire, 2) possible causes of residential fire, and 3) how the fire was eventually controlled. These three insights should better assist the residents in deciding where to install the fire system at home and how effective the sprinkler system is at controlling the fire.

This paper will include the following sections: a data section where the dataset used for the analysis will be presented and explained in detail; a model section that would provide an insight into the building process of the MLR model and justifies the model choice; a result section where the result of the model will be discussed, accompanying by appropriate visualizations and explanations of the result; finally a discussion section where implications, limitations, and future research outlook will be discussed. 

# R Packages
For analysis and visualization purposes, these r packages needs to be installed in R studio
1) gridExtra
2) tidyverse
3) dplyr
4) kableExtra
5) stringr
6) ggpubr


# File Structure
Under folder "input", you will be able to see the the raw dataset used in this report
Under folder "output" -> "paper", you will be able to find the original RMD, reference list, and the final PDF of the report
Under folder "script", you will be able to find the R code that cleans and plot the graphs and tables in the project, this is a clear stimulation process of all variables used in the analysis
