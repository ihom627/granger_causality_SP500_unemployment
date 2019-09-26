
example of granger causality with SP500 and unemployment figures from 1950 to 2019

#STEP1) GET DATA

#read xlsx
install.packages("readxl")
library("readxl")

my_data <- read_excel("SP500_unemployment_historical.xlsx", sheet = "Sheet_1")

> my_data                                                                     
# A tibble: 70 x 3
   Date       Close Unemployment
   <chr>      <dbl>        <dbl>
 1 1950-01-01  17.0          6.5
 2 1951-01-01  21.7          3.7
 3 1952-01-01  24.1          3.2
 4 1953-01-01  26.4          2.9
 5 1954-01-01  26.1          4.9
 6 1955-01-01  36.6          4.9
 7 1956-01-01  43.8          4  
 8 1957-01-01  44.7          4.2
 9 1958-01-01  41.7          5.8
10 1959-01-01  55.5          6  
# … with 60 more rows


install.packages("lmtest")

SP500 <-my_data[,2]

unemployment <-my_data[,3]

plot.ts(SP500)
plot.ts(unemployment)

#need to vectorize data
vector_SP500<-unlist(SP500)
vector_enemployment<-unlist(unemployment)


#STEP2) Check for stationarity

==> by inspection are not stationary (they do not have constant mean and variance)

# check for stationarity anyways using Dickey-Fuller test
install.packages("tseries")

library(tseries)


adf.test(vector_SP500)

>	Augmented Dickey-Fuller Test
>
>data:  vector_SP500
>Dickey-Fuller = -0.069495, Lag order = 4, p-value = 0.99
>alternative hypothesis: stationary


==> p-value > .05, so accept NULL hypothesis, so it non-stationary


adf.test(vector_unemployment)

>	Augmented Dickey-Fuller Test
>
>data:  vector_unemployment
>Dickey-Fuller = -3.0295, Lag order = 4, p-value = 0.1566
>alternative hypothesis: stationary


==> p-value > .05, so accept NULL hypothesis, so it non-stationary



#STEP3) Make stationary by differencing
# now, need to know how many differencing to do

install.packages("forecast")

library(forecast)

ndiffs(vector_SP500, alpha=0.05, test=c("kpss")) 
>[1] 2 

==> need to 2 differencing

ndiffs(vector_unemployment, alpha=0.05, test=c("kpss"))
>[1] 0 

==> so just do 1 differencing

#Ok, now do differencing NOTE: doing this the same number of times to get equal lengths

d_vector_SP500 <-diff(vector_SP500)
dd_vector_SP500 <-diff(d_vector_SP500)

d_vector_unemployment <-diff(vector_unemployment)
dd_vector_unemployment <-diff(d_vector_unemployment)

#now test for stationarity again
adf.test(dd_vector_SP500)

>	Augmented Dickey-Fuller Test
>
>data:  dd_vector_SP500
>Dickey-Fuller = -6.7853, Lag order = 4, p-value = 0.01
>alternative hypothesis: stationary

==> p-value < .05, so reject NULL hypothesis, so it is stationary

adf.test(dd_vector_unemployment)

>	Augmented Dickey-Fuller Test
>
>data:  d_vector_unemployment
>Dickey-Fuller = -5.99772, Lag order = 4, p-value = 0.01
>alternative hypothesis: stationary

==> p-value < .05, so reject NULL hypothesis, so it is stationary



==> OK, now both are stationary

plot.ts(dd_vector_SP500)
plot.ts(dd_vector_unemployment)


#STEP4) Do Granger Causality testing
 
#Now, does unemployment granger cause SP500?

> grangertest(dd_vector_SP500 ~ dd_vector_unemployment, order=1)
Granger causality test

Model 1: dd_vector_SP500 ~ Lags(dd_vector_SP500, 1:1) + Lags(dd_vector_unemployment, 1:1)
Model 2: dd_vector_SP500 ~ Lags(dd_vector_SP500, 1:1)
  Res.Df Df      F Pr(>F)
1     64                 
2     65 -1 0.3563 0.5527
> grangertest(dd_vector_SP500 ~ dd_vector_unemployment, order=2)
Granger causality test

Model 1: dd_vector_SP500 ~ Lags(dd_vector_SP500, 1:2) + Lags(dd_vector_unemployment, 1:2)
Model 2: dd_vector_SP500 ~ Lags(dd_vector_SP500, 1:2)
  Res.Df Df      F Pr(>F)
1     61                 
2     63 -2 0.3709 0.6916
> grangertest(dd_vector_SP500 ~ dd_vector_unemployment, order=3)
Granger causality test

Model 1: dd_vector_SP500 ~ Lags(dd_vector_SP500, 1:3) + Lags(dd_vector_unemployment, 1:3)
Model 2: dd_vector_SP500 ~ Lags(dd_vector_SP500, 1:3)
  Res.Df Df      F Pr(>F)
1     58                 
2     61 -3 0.2783 0.8409
> grangertest(dd_vector_SP500 ~ dd_vector_unemployment, order=4)
Granger causality test

Model 1: dd_vector_SP500 ~ Lags(dd_vector_SP500, 1:4) + Lags(dd_vector_unemployment, 1:4)
Model 2: dd_vector_SP500 ~ Lags(dd_vector_SP500, 1:4)
  Res.Df Df      F Pr(>F)
1     55                 
2     59 -4 0.2603 0.9021
> grangertest(dd_vector_SP500 ~ dd_vector_unemployment, order=5)
Granger causality test

Model 1: dd_vector_SP500 ~ Lags(dd_vector_SP500, 1:5) + Lags(dd_vector_unemployment, 1:5)
Model 2: dd_vector_SP500 ~ Lags(dd_vector_SP500, 1:5)
  Res.Df Df      F Pr(>F)
1     52                 
2     57 -5 0.6563  0.658
> grangertest(dd_vector_SP500 ~ dd_vector_unemployment, order=6)
Granger causality test

Model 1: dd_vector_SP500 ~ Lags(dd_vector_SP500, 1:6) + Lags(dd_vector_unemployment, 1:6)
Model 2: dd_vector_SP500 ~ Lags(dd_vector_SP500, 1:6)
  Res.Df Df      F Pr(>F)
1     49                 
2     55 -6 0.4666 0.8296
> grangertest(dd_vector_SP500 ~ dd_vector_unemployment, order=7)
Granger causality test

Model 1: dd_vector_SP500 ~ Lags(dd_vector_SP500, 1:7) + Lags(dd_vector_unemployment, 1:7)
Model 2: dd_vector_SP500 ~ Lags(dd_vector_SP500, 1:7)
  Res.Df Df      F Pr(>F)
1     46                 
2     53 -7 0.8741 0.5341
> grangertest(dd_vector_SP500 ~ dd_vector_unemployment, order=8)
Granger causality test

Model 1: dd_vector_SP500 ~ Lags(dd_vector_SP500, 1:8) + Lags(dd_vector_unemployment, 1:8)
Model 2: dd_vector_SP500 ~ Lags(dd_vector_SP500, 1:8)
  Res.Df Df     F Pr(>F)
1     43                
2     51 -8 0.662 0.7215
> grangertest(dd_vector_SP500 ~ dd_vector_unemployment, order=9)
Granger causality test

Model 1: dd_vector_SP500 ~ Lags(dd_vector_SP500, 1:9) + Lags(dd_vector_unemployment, 1:9)
Model 2: dd_vector_SP500 ~ Lags(dd_vector_SP500, 1:9)
  Res.Df Df      F Pr(>F)
1     40                 
2     49 -9 0.6401  0.756
> grangertest(dd_vector_SP500 ~ dd_vector_unemployment, order=10)
Granger causality test

Model 1: dd_vector_SP500 ~ Lags(dd_vector_SP500, 1:10) + Lags(dd_vector_unemployment, 1:10)
Model 2: dd_vector_SP500 ~ Lags(dd_vector_SP500, 1:10)
  Res.Df  Df   F Pr(>F)
1     37               
2     47 -10 0.6 0.8034


==> NO

#Now, does SP500 granger cause unemployment?

> grangertest(dd_vector_unemployment ~ dd_vector_SP500, order=1)
Granger causality test

Model 1: dd_vector_unemployment ~ Lags(dd_vector_unemployment, 1:1) + Lags(dd_vector_SP500, 1:1)
Model 2: dd_vector_unemployment ~ Lags(dd_vector_unemployment, 1:1)
  Res.Df Df     F Pr(>F)  
1     64                  
2     65 -1 5.885 0.0181 *
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> grangertest(dd_vector_unemployment ~ dd_vector_SP500, order=2)
Granger causality test

Model 1: dd_vector_unemployment ~ Lags(dd_vector_unemployment, 1:2) + Lags(dd_vector_SP500, 1:2)
Model 2: dd_vector_unemployment ~ Lags(dd_vector_unemployment, 1:2)
  Res.Df Df      F  Pr(>F)  
1     61                    
2     63 -2 2.7128 0.07436 .
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> grangertest(dd_vector_unemployment ~ dd_vector_SP500, order=3)
Granger causality test

Model 1: dd_vector_unemployment ~ Lags(dd_vector_unemployment, 1:3) + Lags(dd_vector_SP500, 1:3)
Model 2: dd_vector_unemployment ~ Lags(dd_vector_unemployment, 1:3)
  Res.Df Df      F Pr(>F)
1     58                 
2     61 -3 1.7281 0.1712
> grangertest(dd_vector_unemployment ~ dd_vector_SP500, order=4)
Granger causality test

Model 1: dd_vector_unemployment ~ Lags(dd_vector_unemployment, 1:4) + Lags(dd_vector_SP500, 1:4)
Model 2: dd_vector_unemployment ~ Lags(dd_vector_unemployment, 1:4)
  Res.Df Df      F Pr(>F)
1     55                 
2     59 -4 1.4664 0.2249
> grangertest(dd_vector_unemployment ~ dd_vector_SP500, order=5)
Granger causality test

Model 1: dd_vector_unemployment ~ Lags(dd_vector_unemployment, 1:5) + Lags(dd_vector_SP500, 1:5)
Model 2: dd_vector_unemployment ~ Lags(dd_vector_unemployment, 1:5)
  Res.Df Df      F Pr(>F)
1     52                 
2     57 -5 1.5879 0.1799
> grangertest(dd_vector_unemployment ~ dd_vector_SP500, order=6)
Granger causality test

Model 1: dd_vector_unemployment ~ Lags(dd_vector_unemployment, 1:6) + Lags(dd_vector_SP500, 1:6)
Model 2: dd_vector_unemployment ~ Lags(dd_vector_unemployment, 1:6)
  Res.Df Df      F Pr(>F)
1     49                 
2     55 -6 1.1946 0.3249
> grangertest(dd_vector_unemployment ~ dd_vector_SP500, order=7)
Granger causality test

Model 1: dd_vector_unemployment ~ Lags(dd_vector_unemployment, 1:7) + Lags(dd_vector_SP500, 1:7)
Model 2: dd_vector_unemployment ~ Lags(dd_vector_unemployment, 1:7)
  Res.Df Df      F Pr(>F)
1     46                 
2     53 -7 0.9176 0.5017
> grangertest(dd_vector_unemployment ~ dd_vector_SP500, order=8)
Granger causality test

Model 1: dd_vector_unemployment ~ Lags(dd_vector_unemployment, 1:8) + Lags(dd_vector_SP500, 1:8)
Model 2: dd_vector_unemployment ~ Lags(dd_vector_unemployment, 1:8)
  Res.Df Df      F Pr(>F)
1     43                 
2     51 -8 0.9133 0.5148
> grangertest(dd_vector_unemployment ~ dd_vector_SP500, order=9)
Granger causality test

Model 1: dd_vector_unemployment ~ Lags(dd_vector_unemployment, 1:9) + Lags(dd_vector_SP500, 1:9)
Model 2: dd_vector_unemployment ~ Lags(dd_vector_unemployment, 1:9)
  Res.Df Df      F Pr(>F)
1     40                 
2     49 -9 0.8188 0.6023
> grangertest(dd_vector_unemployment ~ dd_vector_SP500, order=10)
Granger causality test

Model 1: dd_vector_unemployment ~ Lags(dd_vector_unemployment, 1:10) + Lags(dd_vector_SP500, 1:10)
Model 2: dd_vector_unemployment ~ Lags(dd_vector_unemployment, 1:10)
  Res.Df  Df      F Pr(>F)
1     37                  
2     47 -10 0.9563  0.496


==> YES, for lag 1  

save.image(file='r_image.RData')











