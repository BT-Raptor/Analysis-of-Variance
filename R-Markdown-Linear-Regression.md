Thingamajig
================
BT_Raptor
21/04/2022

``` r
require(data.table)
```

    ## Loading required package: data.table

``` r
require(stats)
setwd("C:/Users/ricke/Desktop/Raptor's Data Science/")
options(scipen=999)
```

Data Acquisition

``` r
all_data=fread("opp.csv")
all_data=na.omit(all_data)
head(all_data)
```

    ##    case site Pop sex age hdlngth skullw totlngth taill footlgth earconch  eye
    ## 1:    1    1 Vic   m   8    94.1   60.4     89.0  36.0     74.5     54.5 15.2
    ## 2:    2    1 Vic   f   6    92.5   57.6     91.5  36.5     72.5     51.2 16.0
    ## 3:    3    1 Vic   f   6    94.0   60.0     95.5  39.0     75.4     51.9 15.5
    ## 4:    4    1 Vic   f   6    93.2   57.1     92.0  38.0     76.1     52.2 15.2
    ## 5:    5    1 Vic   f   2    91.5   56.3     85.5  36.0     71.0     53.2 15.1
    ## 6:    6    1 Vic   f   1    93.1   54.8     90.5  35.5     73.2     53.6 14.2
    ##    chest belly
    ## 1:  28.0    36
    ## 2:  28.5    33
    ## 3:  30.0    34
    ## 4:  28.0    34
    ## 5:  28.5    33
    ## 6:  30.0    32

Creating a new dataframe, where we do the column tweaking

``` r
all_data[,`:=`(hdlngth_2=hdlngth**2,skullw_2=skullw**2,totlngth_2=totlngth**2,
               taill_2=taill**2,footlgth_2=footlgth**2,earconch_2=earconch**2,eye_2=eye**2,
               chest_2=chest**2,belly_2=belly**2)]
all_data[,sex_m:=0]
all_data[sex=='m',sex_m:=1]

all_data[,Pop_Vic:=0]
all_data[Pop=='Vic',Pop_Vic:=1]

all_data[,site_1:=0]
all_data[site==1,site_1:=1]

all_data[,site_2:=0]
all_data[site==2,site_2:=1]

all_data[,site_3:=0]
all_data[site==3,site_3:=1]

all_data[,site_4:=0]
all_data[site==4,site_4:=1]

all_data[,site_5:=0]
all_data[site==5,site_5:=1]

all_data[,site_6:=0]
all_data[site==6,site_6:=1]

all_data[,site_7:=0]
all_data[site==7,site_7:=1]

all_data=all_data[,`:=`(case=NULL,site=NULL,Pop=NULL,sex=NULL)]
head(all_data)
```

    ##    age hdlngth skullw totlngth taill footlgth earconch  eye chest belly
    ## 1:   8    94.1   60.4     89.0  36.0     74.5     54.5 15.2  28.0    36
    ## 2:   6    92.5   57.6     91.5  36.5     72.5     51.2 16.0  28.5    33
    ## 3:   6    94.0   60.0     95.5  39.0     75.4     51.9 15.5  30.0    34
    ## 4:   6    93.2   57.1     92.0  38.0     76.1     52.2 15.2  28.0    34
    ## 5:   2    91.5   56.3     85.5  36.0     71.0     53.2 15.1  28.5    33
    ## 6:   1    93.1   54.8     90.5  35.5     73.2     53.6 14.2  30.0    32
    ##    hdlngth_2 skullw_2 totlngth_2 taill_2 footlgth_2 earconch_2  eye_2 chest_2
    ## 1:   8854.81  3648.16    7921.00 1296.00    5550.25    2970.25 231.04  784.00
    ## 2:   8556.25  3317.76    8372.25 1332.25    5256.25    2621.44 256.00  812.25
    ## 3:   8836.00  3600.00    9120.25 1521.00    5685.16    2693.61 240.25  900.00
    ## 4:   8686.24  3260.41    8464.00 1444.00    5791.21    2724.84 231.04  784.00
    ## 5:   8372.25  3169.69    7310.25 1296.00    5041.00    2830.24 228.01  812.25
    ## 6:   8667.61  3003.04    8190.25 1260.25    5358.24    2872.96 201.64  900.00
    ##    belly_2 sex_m Pop_Vic site_1 site_2 site_3 site_4 site_5 site_6 site_7
    ## 1:    1296     1       1      1      0      0      0      0      0      0
    ## 2:    1089     0       1      1      0      0      0      0      0      0
    ## 3:    1156     0       1      1      0      0      0      0      0      0
    ## 4:    1156     0       1      1      0      0      0      0      0      0
    ## 5:    1089     0       1      1      0      0      0      0      0      0
    ## 6:    1024     0       1      1      0      0      0      0      0      0

``` r
model=lm(age~.,data=all_data)
summary(model)
```

    ## 
    ## Call:
    ## lm(formula = age ~ ., data = all_data)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -3.0214 -1.0603 -0.0664  1.0333  5.0211 
    ## 
    ## Coefficients: (2 not defined because of singularities)
    ##                Estimate  Std. Error t value Pr(>|t|)  
    ## (Intercept) -225.987619  120.998511  -1.868   0.0657 .
    ## hdlngth        3.937819    2.410795   1.633   0.1066  
    ## skullw         2.947073    1.820839   1.619   0.1097  
    ## totlngth      -0.301456    2.114633  -0.143   0.8870  
    ## taill          1.588866    2.856078   0.556   0.5797  
    ## footlgth      -2.662433    1.697651  -1.568   0.1210  
    ## earconch       1.377767    1.741758   0.791   0.4314  
    ## eye           -2.486286    4.146877  -0.600   0.5506  
    ## chest          1.055357    2.536260   0.416   0.6785  
    ## belly         -0.009627    1.426310  -0.007   0.9946  
    ## hdlngth_2     -0.020875    0.012936  -1.614   0.1108  
    ## skullw_2      -0.023673    0.015020  -1.576   0.1192  
    ## totlngth_2     0.001429    0.012046   0.119   0.9059  
    ## taill_2       -0.020047    0.038286  -0.524   0.6021  
    ## footlgth_2     0.018644    0.012288   1.517   0.1334  
    ## earconch_2    -0.014607    0.017851  -0.818   0.4158  
    ## eye_2          0.084292    0.135752   0.621   0.5365  
    ## chest_2       -0.016955    0.046702  -0.363   0.7176  
    ## belly_2        0.001428    0.021483   0.066   0.9472  
    ## sex_m          0.236491    0.432248   0.547   0.5859  
    ## Pop_Vic        1.504084    1.542537   0.975   0.3327  
    ## site_1        -0.014699    0.874555  -0.017   0.9866  
    ## site_2               NA          NA      NA       NA  
    ## site_3         0.155229    0.912027   0.170   0.8653  
    ## site_4        -0.674990    1.069253  -0.631   0.5298  
    ## site_5         0.977197    0.833386   1.173   0.2447  
    ## site_6         0.567375    0.925141   0.613   0.5415  
    ## site_7               NA          NA      NA       NA  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.725 on 75 degrees of freedom
    ## Multiple R-squared:  0.3918, Adjusted R-squared:  0.1891 
    ## F-statistic: 1.933 on 25 and 75 DF,  p-value: 0.01538
