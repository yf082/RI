pop_ri = function(Out, Exp, Cov, data, n.boot = 100) {
  
  library(data.table);library(tidyverse)
  
  ## Convert into data.table
  if (!is.data.table(data)) {
    data = as.data.table(data[, c(Out, Exp, Cov)])
  }
  setDT(data)
  
  
  bbb = sub_ri(Out = Out, Exp =Exp, Cov = Cov, data = data)
  
  ccc = boot_ci_comb(bbb, n.boot = 50)
  
  result = list(fin_res = ccc,
                sub_res = bbb)
  
  class(result) = "ri"
  print.ri = function(x, ...) {
    cat(paste0(rep("=", 30), collapse = "="), "\n")
    cat('Over population    RI:', 
        format(round(ccc$pop_est[1,1], 3), nsmall = 3), "(",
        format(round(ccc$pop_est[1,1] - 1.96*ccc$pop_est[1,2], 3), nsmall = 3), ",",
        format(round(ccc$pop_est[1,1] + 1.96*ccc$pop_est[1,2], 3), nsmall = 3), ")", "\n")
    
    cat('Over population RRERI:', 
        format(round(ccc$pop_est[2,1], 3), nsmall = 3), "(",
        format(round(ccc$pop_est[2,1] - 1.96*ccc$pop_est[2,2], 3), nsmall = 3), ",",
        format(round(ccc$pop_est[2,1] + 1.96*ccc$pop_est[2,2], 3), nsmall = 3), ")", "\n")
    
    cat('Over population   RSI:', 
        format(round(ccc$pop_est[3,1], 3), nsmall = 3), "(",
        format(round(ccc$pop_est[3,1] - 1.96*ccc$pop_est[3,2], 3), nsmall = 3), ",",
        format(round(ccc$pop_est[3,1] + 1.96*ccc$pop_est[3,2], 3), nsmall = 3), ")", "\n")
    cat(paste0(rep("=", 30), collapse = "="), "\n")
  }
  ## Dynamically define print.ri
  assign("print.ri", print.ri, envir = globalenv())
  
  return(result)
}