sub_ri = function(Out, Exp, Cov, data, n.boot = 100) {
  
  library(data.table);library(tidyverse)
  
  ## Convert into data.table
  if (!is.data.table(data)) {
    data = as.data.table(data[, c(Out, Exp, Cov)])
  }
  setDT(data)
  
  ## Get exposures levels
  Exp_1 = Exp[1]
  Exp_2 = Exp[2]
  levels_1 = rownames(table(data[, ..Exp]))
  levels_2 = colnames(table(data[, ..Exp]))
  length_level_1 = length(levels_1)
  length_level_2 = length(levels_2)
  ref_level_1 = levels_1[1]
  ref_level_2 = levels_2[1]
  
  ## Estimate subgroup realized interaction
  z = 1
  result = list()
  pop_num = pop_nam = c()
  for (i in 2:length_level_1) {
    for (j in 2:length_level_2) {
      
      ## Split dataset
      data_ = copy(data[data[[Exp_1]] %in% c(ref_level_1,levels_1[i]) & data[[Exp_2]] %in% c(ref_level_2,levels_2[j])])
      
      ## Point estimation
      result[[z]] = ri_base(Out = Out, Exp =Exp, Cov = Cov, data = data_)
      pop_num[z] = nrow(data[data[[Exp_1]] == levels_1[i] & data[[Exp_2]] == levels_2[j]])
      pop_nam[z] = paste0(levels_1[i], levels_2[j])
      z = z + 1
    }
  }
  
  ## Population proportion
  pop_pro = pop_num / sum(pop_num)
  
  ## Subgroup realized interaction
  sub_est = sapply(result, function(x) {x$res})
  
  ## Overall population realized interaction
  ove_est = sub_est %*% pop_pro
  
  ## Results arrangement
  res = data.frame(cbind(sub_est, ove_est))
  names(res) = c(pop_nam, "overall")
  
  return(list(estmate = res,
              pop_nam = pop_nam,
              pop_pro = pop_pro,
              result = result))
  
}
