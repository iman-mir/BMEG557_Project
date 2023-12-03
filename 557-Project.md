557 Project Analysis
================

``` r
library(tibble)
library(BSDA)
```

    ## Loading required package: lattice

    ## 
    ## Attaching package: 'BSDA'

    ## The following object is masked from 'package:datasets':
    ## 
    ##     Orange

## Outcome \#1: Number (Percentage) of Participants Reporting a ≥ 50% Reduction in Headache Days Code (Fisher Test)

``` r
# For Topiramate vs Placebo
outcome1.top <- matrix(c(58, 72, 26,40), nrow = 2, byrow = TRUE)

rownames(outcome1.top) <- c("Topiramate", "Placebo")
colnames(outcome1.top) <- c("No Reduction in Headaches", "Reduction in Headaches")

outcome1.top <- as.data.frame(outcome1.top)

fisher_result_top <- fisher.test(outcome1.top)
print(fisher_result_top)
```

    ## 
    ##  Fisher's Exact Test for Count Data
    ## 
    ## data:  outcome1.top
    ## p-value = 0.5425
    ## alternative hypothesis: true odds ratio is not equal to 1
    ## 95 percent confidence interval:
    ##  0.6510692 2.3775266
    ## sample estimates:
    ## odds ratio 
    ##   1.237961

``` r
# For Amitriptyline vs Placebo 
outcome1.ami <- matrix(c(63, 69, 26, 40), nrow = 2, byrow = TRUE)

rownames(outcome1.ami) <- c("Amitriptyline", "Placebo")
colnames(outcome1.ami) <- c("No Reduction in Headaches", " Reduction in Headaches")

outcome1.ami <- as.data.frame(outcome1.ami)

fisher_result_ami <- fisher.test(outcome1.ami)
print(fisher_result_ami)
```

    ## 
    ##  Fisher's Exact Test for Count Data
    ## 
    ## data:  outcome1.ami
    ## p-value = 0.2914
    ## alternative hypothesis: true odds ratio is not equal to 1
    ## 95 percent confidence interval:
    ##  0.7398783 2.6867585
    ## sample estimates:
    ## odds ratio 
    ##   1.402265

``` r
# For Topiramate vs Amitriptyline

outcome1.together <- matrix(c(58, 72, 63, 69), nrow = 2, byrow = TRUE)

rownames(outcome1.together) <- c("Topiramate", "Amitriptyline")
colnames(outcome1.together) <- c("No Reduction in Headaches", " Reduction in Headaches")

outcome1.together <- as.data.frame(outcome1.together)

fisher_result_together <- fisher.test(outcome1.together)
print(fisher_result_together)
```

    ## 
    ##  Fisher's Exact Test for Count Data
    ## 
    ## data:  outcome1.together
    ## p-value = 0.6225
    ## alternative hypothesis: true odds ratio is not equal to 1
    ## 95 percent confidence interval:
    ##  0.527068 1.476530
    ## sample estimates:
    ## odds ratio 
    ##  0.8826991

## Outcome \#2: Changes in Absolute Headache Disability Score on PEDIMAS Code (two sample ttest)

``` r
# Data for Topiramate
mean_Topiramate<- -26.8
sd_Topiramate <- 27.5
n_Topiramate <- 130

# Data for Placebo
mean_Placebo <- -22.6
sd_Placebo <- 29.42
n_Placebo <- 66

# Data for Amitriptyline
mean_Amitriptyline<- -22.5
sd_Amitriptyline <- 26.37
n_Amitriptyline <- 132

#For Topiramate vs Placebo
tsum.test(
  mean_Topiramate,
  s.x = sd_Topiramate,
  n.x = n_Topiramate,
  mean.y = mean_Placebo,
  s.y = sd_Placebo,
  n.y = n_Placebo,
  alternative = "two.sided",
  mu = 0,
  var.equal = FALSE,
  conf.level = 0.95
)
```

    ## 
    ##  Welch Modified Two-Sample t-Test
    ## 
    ## data:  Summarized x and y
    ## t = -0.96529, df = 123.24, p-value = 0.3363
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -12.81244   4.41244
    ## sample estimates:
    ## mean of x mean of y 
    ##     -26.8     -22.6

``` r
# For Amitriptyline vs Placebo 
tsum.test(
  mean_Amitriptyline,
  s.x = sd_Amitriptyline,
  n.x = n_Amitriptyline,
  mean.y = mean_Placebo,
  s.y = sd_Placebo,
  n.y = n_Placebo,
  alternative = "two.sided",
  mu = 0,
  var.equal = FALSE,
  conf.level = 0.95
)
```

    ## 
    ##  Welch Modified Two-Sample t-Test
    ## 
    ## data:  Summarized x and y
    ## t = 0.023324, df = 118.24, p-value = 0.9814
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -8.39013  8.59013
    ## sample estimates:
    ## mean of x mean of y 
    ##     -22.5     -22.6

``` r
# For Topiramate vs Amitriptyline 
tsum.test(
  mean_Topiramate,
  s.x = sd_Topiramate,
  n.x = n_Topiramate,
  mean.y = mean_Amitriptyline,
  s.y = sd_Amitriptyline,
  n.y = n_Amitriptyline,
  alternative = "two.sided",
  mu = 0,
  var.equal = FALSE,
  conf.level = 0.95
)
```

    ## 
    ##  Welch Modified Two-Sample t-Test
    ## 
    ## data:  Summarized x and y
    ## t = -1.2915, df = 259.15, p-value = 0.1977
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -10.856243   2.256243
    ## sample estimates:
    ## mean of x mean of y 
    ##     -26.8     -22.5

## Outcome \#3: Change in number of headache days Code (two sample ttest)

``` r
# Data for Topiramate
mean_Topiramate<- -6.7
sd_Topiramate <- 4.99
n_Topiramate <- 101

# Data for Placebo
mean_Placebo <- -5.9
sd_Placebo <- 6.96
n_Placebo <- 59

# Data for Amitriptyline
mean_Amitriptyline<- -6.7
sd_Amitriptyline <- 6.20
n_Amitriptyline <- 104

#For Topiramate vs Placebo
tsum.test(
  mean_Topiramate,
  s.x = sd_Topiramate,
  n.x = n_Topiramate,
  mean.y = mean_Placebo,
  s.y = sd_Placebo,
  n.y = n_Placebo,
  alternative = "two.sided",
  mu = 0,
  var.equal = FALSE,
  conf.level = 0.95
)
```

    ## 
    ##  Welch Modified Two-Sample t-Test
    ## 
    ## data:  Summarized x and y
    ## t = -0.77427, df = 93.188, p-value = 0.4407
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -2.85175  1.25175
    ## sample estimates:
    ## mean of x mean of y 
    ##      -6.7      -5.9

``` r
# For Amitriptyline vs Placebo 
tsum.test(
  mean_Amitriptyline,
  s.x = sd_Amitriptyline,
  n.x = n_Amitriptyline,
  mean.y = mean_Placebo,
  s.y = sd_Placebo,
  n.y = n_Placebo,
  alternative = "two.sided",
  mu = 0,
  var.equal = FALSE,
  conf.level = 0.95
)
```

    ## 
    ##  Welch Modified Two-Sample t-Test
    ## 
    ## data:  Summarized x and y
    ## t = -0.73316, df = 109.48, p-value = 0.465
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -2.962564  1.362564
    ## sample estimates:
    ## mean of x mean of y 
    ##      -6.7      -5.9

``` r
# For Topiramate vs Amitriptyline 
tsum.test(
  mean_Topiramate,
  s.x = sd_Topiramate,
  n.x = n_Topiramate,
  mean.y = mean_Amitriptyline,
  s.y = sd_Amitriptyline,
  n.y = n_Amitriptyline,
  alternative = "two.sided",
  mu = 0,
  var.equal = FALSE,
  conf.level = 0.95
)
```

    ## 
    ##  Welch Modified Two-Sample t-Test
    ## 
    ## data:  Summarized x and y
    ## t = 0, df = 196.28, p-value = 1
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -1.548024  1.548024
    ## sample estimates:
    ## mean of x mean of y 
    ##      -6.7      -6.7

## Outcome \#4: Tolerability as Indicated by the Percentage of Participants that Completed the 24 weeks Treatment Phase Code (Fisher Test)

``` r
# For Topiramate vs Placebo 
outcome4.top <- matrix(c(28, 102, 7, 59), nrow = 2, byrow = TRUE)

rownames(outcome4.top) <- c("Topiramate", "Placebo")
colnames(outcome4.top) <- c("Not Tolerable", " Tolerable")

outcome4.top <- as.data.frame(outcome4.top)

fisher_result_top <- fisher.test(outcome4.top)
print(fisher_result_top)
```

    ## 
    ##  Fisher's Exact Test for Count Data
    ## 
    ## data:  outcome4.top
    ## p-value = 0.07536
    ## alternative hypothesis: true odds ratio is not equal to 1
    ## 95 percent confidence interval:
    ##  0.9105247 6.6462295
    ## sample estimates:
    ## odds ratio 
    ##   2.304743

``` r
# For Amitriptyline vs Placebo 
outcome4.ami <- matrix(c(26, 106, 7, 59), nrow = 2, byrow = TRUE)

rownames(outcome4.ami) <- c("Amitriptyline", "Placebo")
colnames(outcome4.ami) <- c("Not Tolerable", " Tolerable")

outcome4.ami <- as.data.frame(outcome4.ami)

fisher_result_ami <- fisher.test(outcome4.ami)
print(fisher_result_ami)
```

    ## 
    ##  Fisher's Exact Test for Count Data
    ## 
    ## data:  outcome4.ami
    ## p-value = 0.1557
    ## alternative hypothesis: true odds ratio is not equal to 1
    ## 95 percent confidence interval:
    ##  0.8078188 5.9718589
    ## sample estimates:
    ## odds ratio 
    ##   2.060455

``` r
# For Topiramate vs Amitriptyline 

outcome4.together <- matrix(c(28, 102, 26, 106), nrow = 2, byrow = TRUE)

rownames(outcome4.together) <- c("Topiramate", "Amitriptyline")
colnames(outcome4.together) <- c(" Not tolerable", "Tolerable")

outcome4.together <- as.data.frame(outcome4.together)

fisher_result_together <- fisher.test(outcome4.together)
print(fisher_result_together)
```

    ## 
    ##  Fisher's Exact Test for Count Data
    ## 
    ## data:  outcome4.together
    ## p-value = 0.7611
    ## alternative hypothesis: true odds ratio is not equal to 1
    ## 95 percent confidence interval:
    ##  0.5881012 2.1338681
    ## sample estimates:
    ## odds ratio 
    ##   1.118673

## Outcome 5: Occurence of Treatment Emergent Serious Adverse Effects Code (Fisher Test)

``` r
# For Topiramate vs Placebo 
outcome5.top <- matrix(c(141, 4, 70, 2 ), nrow = 2, byrow = TRUE)

rownames(outcome5.top) <- c("Topiramate", "Placebo")
colnames(outcome5.top) <- c("No Serious Adverse Effect", "Serious Adverse Effect")

outcome5.top <- as.data.frame(outcome5.top)

fisher_result_top <- fisher.test(outcome5.top)
print(fisher_result_top)
```

    ## 
    ##  Fisher's Exact Test for Count Data
    ## 
    ## data:  outcome5.top
    ## p-value = 1
    ## alternative hypothesis: true odds ratio is not equal to 1
    ## 95 percent confidence interval:
    ##  0.08905939 7.22121204
    ## sample estimates:
    ## odds ratio 
    ##   1.007111

``` r
# For Amitriptyline vs Placebo 

outcome5.ami <- matrix(c(138, 6, 70, 2), nrow = 2, byrow = TRUE)

rownames(outcome5.ami) <- c("Amitriptyline", "Placebo")
colnames(outcome5.ami) <- c("No Serious Adverse Effect", " Serious Adverse Effect")

outcome5.ami <- as.data.frame(outcome5.ami)

fisher_result_ami <- fisher.test(outcome5.ami)
print(fisher_result_ami)
```

    ## 
    ##  Fisher's Exact Test for Count Data
    ## 
    ## data:  outcome5.ami
    ## p-value = 0.7217
    ## alternative hypothesis: true odds ratio is not equal to 1
    ## 95 percent confidence interval:
    ##  0.06340831 3.80473721
    ## sample estimates:
    ## odds ratio 
    ##  0.6583297

``` r
# For Topiramate vs Amitriptyline 

outcome5.together <- matrix(c(141, 4, 138, 6), nrow = 2, byrow = TRUE)

rownames(outcome5.together) <- c("Topiramate", "Amitriptyline")
colnames(outcome5.together) <- c("No Serious Adverse Effect", " Serious Adverse Effect")

outcome5.together <- as.data.frame(outcome5.together)

fisher_result_together <- fisher.test(outcome5.together)
print(fisher_result_together)
```

    ## 
    ##  Fisher's Exact Test for Count Data
    ## 
    ## data:  outcome5.together
    ## p-value = 0.5409
    ## alternative hypothesis: true odds ratio is not equal to 1
    ## 95 percent confidence interval:
    ##  0.354125 7.539726
    ## sample estimates:
    ## odds ratio 
    ##   1.530362
