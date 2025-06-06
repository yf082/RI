###############################################.
#                                              .
#           Realized Interaction               .
#                                              .
###############################################.

rm(list = ls())

packages <- c("tidyverse", "data.table")
lapply(packages, function(x) {library(x, character.only = T)})
source("ri_base.R")
source("boot_ci.R")
source("sub_ri.R")
source("pop_ri.R")

load("Simdata.rdata")

Out = "y"
Exp = c("x1", "x2")
Cov = c("c1", "c2", "c3", "c4")
data = dt_bytx

#
ri_base(Out = "y",
        Exp = c("x1", "x2"),
        Cov = c("c1", "c2", "c3", "c4"),
        data = dt_bytx)

sub_ri(Out = "y",
       Exp = c("x1", "x2"),
       Cov = c("c1", "c2", "c3", "c4"),
       data = dt_bytx)

pop_ri(Out = "y",
       Exp = c("x1", "x2"),
       Cov = c("c1", "c2", "c3", "c4"),
       data = dt_bytx, 
       n.boot = 10)

