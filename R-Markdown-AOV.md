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
head(all_data)
```

    ##    case site Pop sex age hdlngth skullw totlngth taill footlgth earconch  eye
    ## 1:    1    1 Vic   m   8    94.1   60.4     89.0  36.0     74.5     54.5 15.2
    ## 2:    2    1 Vic   f   6    92.5   57.6     91.5  36.5     72.5     51.2 16.0
    ## 3:    3    1 Vic   f   6    94.0   60.0     95.5  39.0     75.4     51.9 15.5
    ## 4:    4    1 Vic   f   6    93.2   57.1     92.0  38.0     76.1     52.2 15.2
    ## 5:    5    1 Vic   f   2    91.5   56.3     85.5  36.0     71.0     53.2 15.1
    ## 6:    6    1 Vic   f   1    93.1   54.8     90.5  35.5     73.2     53.6 14.2
    ##    chest belly hdlngth_2 skullw_2 totlngth_2 taill_2 footlgth_2 earconch_2
    ## 1:  28.0    36   8854.81  3648.16    7921.00 1296.00    5550.25    2970.25
    ## 2:  28.5    33   8556.25  3317.76    8372.25 1332.25    5256.25    2621.44
    ## 3:  30.0    34   8836.00  3600.00    9120.25 1521.00    5685.16    2693.61
    ## 4:  28.0    34   8686.24  3260.41    8464.00 1444.00    5791.21    2724.84
    ## 5:  28.5    33   8372.25  3169.69    7310.25 1296.00    5041.00    2830.24
    ## 6:  30.0    32   8667.61  3003.04    8190.25 1260.25    5358.24    2872.96
    ##     eye_2 chest_2 belly_2
    ## 1: 231.04  784.00    1296
    ## 2: 256.00  812.25    1089
    ## 3: 240.25  900.00    1156
    ## 4: 231.04  784.00    1156
    ## 5: 228.01  812.25    1089
    ## 6: 201.64  900.00    1024

Running a 1 Way ANOVA

``` r
temp_df=all_data[,c('sex','age')]

model=aov(age~sex,data=temp_df)
```

``` r
summary(model)
```

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## sex          1    1.2   1.226   0.332  0.566
    ## Residuals   99  365.6   3.693

Running a 2 Way ANOVA

``` r
temp_df=all_data[,c('sex','age','Pop')]
#convert case to a str.
temp_df[,Pop:=as.factor(Pop)]

model=aov(age~.,data=temp_df)
```

``` r
summary(model)
```

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## sex          1    1.2   1.226   0.330  0.567
    ## Pop          1    1.8   1.799   0.485  0.488
    ## Residuals   98  363.8   3.712
