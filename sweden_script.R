# DO NOT CHANGE ANYTHING IN LINES 5-35

# The documentation relatives to the libraries used can be found at:
# - "https://cran.r-project.org/web//packages/lmtest/lmtest.pdf"
# - "https://cran.r-project.org/web/packages/rstudioapi/rstudioapi.pdf"
# - "https://cran.r-project.org/web/packages/car/car.pdf"


# RUN THIS LINE FIRST TO CLEAR YOUR ENVIRONMENT
rm(list=ls())

# you can ignore the code in lines 11-28 but make sure you run it

# when first opening the document you should be prompted to install 'rstudioapi',  'lmtest', 'car'
# otherwise install the packages manually with the commands:
# install.packages("lmtest")
# install.packages("rstudioapi")
# install.package("car")

library(rstudioapi)
library(lmtest)
library(car)


# Getting the path of your current open file - 
# I'm doing this to make this work on all our devices regardless of the path of our working directory
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
# print( getwd() )

############################################ Loading the data ############################################ 

path_list = as.list(strsplit(current_path, '')[[1]])
clean_path_list = path_list[1:(length(path_list)-16)]
clean_path = paste(clean_path_list, collapse='')

inf = read.csv(paste(clean_path, '/inflation.csv', sep=''))
unemp = read.csv(paste(clean_path, '/unemployment.csv', sep=''))

st_int = read.csv(paste(clean_path, '/ST-int.csv', sep=''))
lt_int = read.csv(paste(clean_path, '/LT-int.csv', sep=''))

avg_wages = read.csv(paste(clean_path, '/avg_wages.csv', sep=''))

gdp = read.csv(paste(clean_path, '/gdp.csv', sep=''))

############################################ Plotting raw data ############################################ 

# Plotting raw data

plot(inf$TIME,inf$Value)
plot(unemp$TIME, unemp$Value)
plot(st_int$TIME, st_int$Value)
plot(lt_int$TIME, lt_int$Value)
plot(avg_wages$TIME, avg_wages$Value)
plot(gdp$TIME, gdp$Value)

############################################ Preferences ############################################ 

par(mfrow=c(1,4)) # if uncommented this makes diagnostic plots in only one figure and not one

######################################## BASIC PHILLIPS CURVE ###########################################
# inflation and unemployment data from Jan 1983 to Dec 2020  
# -> This one uses monthly data

inf_monthly = read.csv(paste(clean_path, '/inflation-monthly.csv', sep=''))
unemp_monthly = read.csv(paste(clean_path, '/unemployment-monthly.csv', sep=''))

pc_monthly = lm(inf_monthly$Value ~ unemp_monthly$Value)
summary(pc_monthly)

coeftest(pc_monthly, vcov=hccm)

## plot ##
plot(pc_monthly)
plot(unemp_monthly$Value, inf_monthly$Value)
abline(pc_monthly, col='red')
avPlots(pc_monthly)

## testing OLS assumptions ##
resettest(pc_monthly, power=3, type="regressor")
gqtest(pc_monthly, point = 0.2)
bptest(pc_monthly)
dwtest(pc_monthly)
bgtest(pc_monthly)


#---------------------------------------------------------------------------------------------------------------------##
#                FROM HERE WE SWITCH TO YEARLY DATA DUE TO DATA CONSTRAINTS AND MISSING MONTHLY DATA                  ##
#---------------------------------------------------------------------------------------------------------------------##



##################################### BASIC PHILLIPS CURVE WITH YEARLY DATA FROM 1983 to 2019 #####################################
# This one uses yearly data and we will use it as control for our expanded models

pc_0 = lm(inf$Value ~ unemp$Value)
summary(pc_0)
plot(pc_0)
plot(unemp$Value, inf$Value, xlab = "Unemployment rate", ylab = "Inflation rate")
abline(pc_0, col='red', width=2)


##################################### ADDING THE EFFECT OF SHORT-TERM & LONG-TERM INTEREST RATES #####################################
# data adjusted to fit the period from 1987 to 2019 

inf_adjusted = data.frame(TIME = inf[c(5:37), 'TIME'], Value = inf[c(5:37), 'Value'])
st_int_adjusted = data.frame(TIME = st_int[c(5:37), 'TIME'], Value = st_int[c(5:37), 'Value'])
unemp_adjusted = data.frame(TIME = unemp[c(5:37), 'TIME'], Value = unemp[c(5:37), 'Value'])

pc_1 = lm(inf_adjusted$Value ~ unemp_adjusted$Value + st_int_adjusted$Value + lt_int$Value)
summary(pc_1)
plot(pc_1)
avPlots(pc_1, ylab="Inflation", xlab="Unemployment | Others", layout = c(1, 1))



# building the basic Phillips Curve with data from this period to see if our model is an improvement

pc_control_1 = lm(inf_adjusted$Value ~ unemp_adjusted$Value)
summary(pc_control_1)
plot(pc_control_1)

# testing OLS assumptions
resettest(pc_1, power=2, type="regressor")
resettest(pc_control_1, power=2, type="regressor")

gqtest(pc_1, point = 0.2)
gqtest(pc_control_1, point = 0.2)

bptest(pc_1)
bptest(pc_control_1)

dwtest(pc_1)
dwtest(pc_control_1)


##################################### ADDING THE EFFECT OF AVERAGE WAGES - data from 1990 to 2019 #####################################
# here we just use average wages and unemployment as explanatory variables because adding more variables
# would make the model less precise because the data for average wages is provided on a yearly basis 
# from 1990 so the sample size if too small to safely add more explanatory variables

inf_adjusted_2 = data.frame(TIME = inf[c(8:37), 'TIME'], Value = inf[c(8:37), 'Value'])
unemp_adjusted_2 = data.frame(TIME = unemp[c(8:37), 'TIME'], Value = unemp[c(8:37), 'Value'])

pc_2 = lm(inf_adjusted_2$Value ~ unemp_adjusted_2$Value + avg_wages$Value)
summary(pc_2)
plot(pc_2)
avPlots(pc_2, ylab="Inflation", xlab="Wages | Others", layout = c(1, 1))

# building the basic Phillips Curve with data from this period to see if our model is an improvement

pc_control_2 = lm(inf_adjusted_2$Value ~ unemp_adjusted_2$Value)
summary(pc_control_2)
plot(pc_control_2)

# testing OLS assumptions
resettest(pc_2, power=2, type="regressor")
resettest(pc_control_2, power=2, type="regressor")

gqtest(pc_2, point = 0.2)
gqtest(pc_control_2, point = 0.2)

bptest(pc_2)
bptest(pc_control_2)

dwtest(pc_2)
dwtest(pc_control_2)

bgtest(pc_2)
bgtest(pc_control_2)


############################################ ADDING THE EFFECT OF GDP - data from 1983 to 2019 ############################################ 
# here we use unemployment and GDP for our explanatory variables

pc_3 = lm(inf$Value ~ unemp$Value + gdp$Value)
summary(pc_3)
plot(pc_3)
avPlots(pc_3)

# We already built a basic Phillips Curve for this period in 'pc_0' so we compare our model to pc_0
# to see if it's an improvement. What I mean is, if we build a 'pc_control_3' here it would be the same as pc_0

# testing OLS assumptions
resettest(pc_3, power=2, type="regressor")
resettest(pc_0, power=2, type="regressor")

gqtest(pc_3, point = 0.2)
gqtest(pc_0, point = 0.2)

bptest(pc_3)
bptest(pc_0)

dwtest(pc_3)
dwtest(pc_0)

bgtest(pc_3)
bgtest(pc_0)


#################### ADDING THE EFFECT OF GDP, SHORT-TERM INTEREST RATES & LONG-TERM INTEREST RATES - data from 1987 to 2019 #################### 

gdp_adjusted = data.frame(TIME = inf[c(5:37), 'TIME'], Value = inf[c(5:37), 'Value'])

pc_4 = lm(inf_adjusted$Value ~ unemp_adjusted$Value + st_int_adjusted$Value + gdp_adjusted$Value + lt_int$Value)
summary(pc_4)
plot(pc_4)
avPlots(pc_4)

# We already built a basic Phillips Curve for this period in 'pc_control_1' so we compare our model to pc_control_1 to see if it's an improvement
# What I mean is, if we build a 'pc_control_4' here it would be the same as pc_control_1

## testing OLS assumptions ##
resettest(pc_4, power=2, type="regressor")
resettest(pc_control_1, power=2, type="regressor")

gqtest(pc_4, point = 0.2)
gqtest(pc_control_1, point = 0.2)

bptest(pc_4)
bptest(pc_control_1)

dwtest(pc_4)
dwtest(pc_control_1)

bgtest(pc_4)
bgtest(pc_control_1)










