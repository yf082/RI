############################################### .
#                                               .
#               Simulated Data                  .
#                                               .
############################################### .

rm(list = ls())

packages <- c("tidyverse")
lapply(packages, function(x) {library(x, character.only = T)})

Nsim <- 1000
set.seed(6666)

## 协变量
Covariates <- MASS::mvrnorm(
    n = Nsim,
    mu = c(1, 2, 3, 4),
    Sigma = matrix(
        c(
            2, 0, 0, 0,
            0, 3, 0, 0,
            0, 0, 2.5, 0,
            0, 0, 0, 4
        ),
        nrow = 4, byrow = T
    )
)

# Part I 连续结局，连续暴露 ----

## 暴露
Exposures <- MASS::mvrnorm(
    n = Nsim,
    mu = c(1.5, 2.5),
    Sigma = matrix(
        c(
            0.5, 0.01,
            0.01, 5.4
        ),
        nrow = 2, byrow = T
    )
)
Exposures <- cbind(Exposures, Exposures[,1]*Exposures[,2])

## 结局
Design_matrin <- cbind(1, Exposures, Covariates)
Coefficents <- c(-0.2, 0.15, 0.13, 0.05, 0.18, -0.3, -0.24, 0.6)
Outcome <- Design_matrin %*% Coefficents + runif(Nsim)
dt_cycx <- data.frame(cbind(Outcome, Exposures, Covariates))
names(dt_cycx) <- c("y", paste0("x", c(1, 2, 12)), paste0("c", 1:4))

## 核查数据生成情况
coef(lm(y ~ x1 + x2 + x1:x2 + c1 + c2 + c3 + c4, data = dt_cycx))

# Part II 二分类结局，连续暴露 ----

## 暴露
Exposures <- MASS::mvrnorm(
    n = Nsim,
    mu = c(1.5, 2.5),
    Sigma = matrix(
        c(
            5, 0.01,
            0.01, 14
        ),
        nrow = 2, byrow = T
    )
)
Exposures <- cbind(Exposures, Exposures[,1]*Exposures[,2])

## 结局
Design_matrin <- cbind(1, Exposures, Covariates)
Coefficents <- c(-0.2, 0.15, 0.13, 0.05, 0.18, -0.3, -0.24, 0.6)
Outcome <- Design_matrin %*% Coefficents
Probability <- plogis(Outcome)
Binary_outcome <- rbinom(n = Nsim, size = 1, prob = Probability)
dt_bycx <- data.frame(cbind(Binary_outcome, Exposures, Covariates))
names(dt_bycx) <- c("y", paste0("x", c(1, 2, 12)), paste0("c", 1:4))

## 核查数据生成情况
coef(glm(y ~ x1 + x2 + x1:x2 + c1 + c2 + c3 + c4, data = dt_bycx, family = binomial()))

# Part III 连续结局，二分类暴露 ----

## 暴露
x1_binary <- rbinom(Nsim, 1, plogis(1.5 - 1))  # 均值为~1.5
x2_binary <- rbinom(Nsim, 1, plogis(2.5 - 1))  # 均值为~2.5
Exposures <- cbind(x1_binary, x2_binary, x1_binary*x2_binary)

## 结局
Design_matrin <- cbind(1, Exposures, Covariates)
Coefficents <- c(-0.2, 0.15, 0.13, 0.05, 0.18, -0.3, -0.24, 0.6)
Outcome <- Design_matrin %*% Coefficents + runif(Nsim)
dt_cybx <- data.frame(cbind(Outcome, Exposures, Covariates))
names(dt_cybx) <- c("y", paste0("x", c(1,2,12)), paste0("c",1:4))

## 核查
coef(lm(y ~ x1 + x2 + x1:x2 + c1 + c2 + c3 + c4, data = dt_cybx))

# Part IV 二分类结局，二分类暴露 ----

## 暴露
x1_binary <- rbinom(Nsim, 1, plogis(1.5 - 1))
x2_binary <- rbinom(Nsim, 1, plogis(2.5 - 1))
Exposures <- cbind(x1_binary, x2_binary, x1_binary*x2_binary)

## 结局
Design_matrin <- cbind(1, Exposures, Covariates)
Coefficents <- c(-0.2, 0.15, 0.13, 0.05, 0.18, -0.3, -0.24, 0.6)
Outcome <- Design_matrin %*% Coefficents
Probability <- plogis(Outcome)
Binary_outcome <- rbinom(Nsim, 1, Probability)
dt_bybx <- data.frame(cbind(Binary_outcome, Exposures, Covariates))
names(dt_bybx) <- c("y", paste0("x",c(1,2,12)), paste0("c",1:4))

## 核查
coef(glm(y ~ x1 + x2 + x1:x2 + c1 + c2 + c3 + c4, data = dt_bybx, family = binomial()))

# Part V 连续结局，三分类暴露 ----

## 暴露
x1_tertile <- cut(rnorm(Nsim, mean=1.5, sd=sqrt(0.5)), 
                 breaks = quantile(rnorm(1e6,1.5,sqrt(0.5)), probs=0:3/3),
                 labels = 0:2) |> as.numeric()
x2_tertile <- cut(rnorm(Nsim, mean=2.5, sd=sqrt(5.4)), 
                 breaks = quantile(rnorm(1e6,2.5,sqrt(5.4)), probs=0:3/3),
                 labels = 0:2) |> as.numeric()
Exposures <- cbind(x1_tertile, x2_tertile, x1_tertile*x2_tertile)

## 结局生成
Design_matrin <- cbind(1, Exposures, Covariates)
Coefficents <- c(-0.2, 0.15, 0.13, 0.05, 0.18, -0.3, -0.24, 0.6)
Outcome <- Design_matrin %*% Coefficents + runif(Nsim)
dt_cytx <- data.frame(cbind(Outcome, Exposures, Covariates))
names(dt_cytx) <- c("y", paste0("x",c(1,2,12)), paste0("c",1:4))

## 核查
coef(lm(y ~ ordered(x1) + ordered(x2) + ordered(x1):ordered(x2) + c1 + c2 + c3 + c4, data = dt_cytx))

# Part VI 二分类结局，三分类暴露 ----

## 暴露
x1_tertile <- cut(rnorm(Nsim, mean=1.5, sd=sqrt(5)), 
                 breaks = quantile(rnorm(1e6,1.5,sqrt(5)), probs=0:3/3),
                 labels = 0:2) |> as.numeric()
x2_tertile <- cut(rnorm(Nsim, mean=2.5, sd=sqrt(14)), 
                 breaks = quantile(rnorm(1e6,2.5,sqrt(14)), probs=0:3/3),
                 labels = 0:2) |> as.numeric()
Exposures <- cbind(x1_tertile, x2_tertile, x1_tertile*x2_tertile)

## 结局
Design_matrin <- cbind(1, Exposures, Covariates)
Coefficents <- c(-0.2, 0.15, 0.13, 0.05, 0.18, -0.3, -0.24, 0.6)
Outcome <- Design_matrin %*% Coefficents
Probability <- plogis(Outcome)
Binary_outcome <- rbinom(Nsim, 1, Probability)
dt_bytx <- data.frame(cbind(Binary_outcome, Exposures, Covariates))
names(dt_bytx) <- c("y", paste0("x",c(1,2,12)), paste0("c",1:4))

## 核查
coef(glm(y ~ ordered(x1) + ordered(x2) + ordered(x1):ordered(x2) + c1 + c2 + c3 + c4, data = dt_bytx, family = binomial()))

# Part VII 结果保存 ----
ls()[stringr::str_detect(objects(), "dt")]
save(dt_bybx,dt_bycx,dt_bytx,dt_cybx,dt_cycx,dt_cytx,  file = "Simdata.rdata")
