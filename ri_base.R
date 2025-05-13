ri_base = function(Out, Exp, Cov, data) {
  
  library(data.table);library(tidyverse)

  if (!is.data.table(data)) {
    data = as.data.table(data[, c(Out, Exp, Cov)])
  }
  setDT(data)
  
  ## Model in overall population
  formula_ = paste0(
    Out, "~",
    paste0(
      c(
        paste0(Exp, collapse = "+"),
        paste0(Exp[1], ":", Exp[2]),
        paste0(Cov, collapse = "+")
      ),
      collapse = "+"
    )
  )
  Mod = glm(formula_, data = data, family = binomial())
  
  ## Conterfactual exposure levels
  
  ### Get exposures levels
  Exp_1 = Exp[1]
  Exp_2 = Exp[2]
  levels_1 = rownames(table(data[, ..Exp]))
  levels_2 = colnames(table(data[, ..Exp]))
  
  ### Conterfactual exposure levels
  data_11 = copy(data[data[[Exp_1]] == levels_1[2] & data[[Exp_2]] == levels_2[2]])
  data_10 = copy(data[data[[Exp_1]] == levels_1[2] & data[[Exp_2]] == levels_2[2]])[, (Exp_2) := 0]
  data_01 = copy(data[data[[Exp_1]] == levels_1[2] & data[[Exp_2]] == levels_2[2]])[, (Exp_1) := 0]
  data_00 = copy(data[data[[Exp_1]] == levels_1[2] & data[[Exp_2]] == levels_2[2]])[, c(Exp_1, Exp_2) := list(0, 0)]
  
  ## Conterfactual outcomes
  p_11 = predict(Mod, data_11, type = "response")
  p_10 = predict(Mod, data_10, type = "response")
  p_01 = predict(Mod, data_01, type = "response")
  p_00 = predict(Mod, data_00, type = "response")
  
  ## Realized interaction
  RI = mean(p_11) - mean(p_01) - mean(p_10) + mean(p_00)
  RRERI = (mean(p_11) - mean(p_01) - mean(p_10) + mean(p_00)) / mean(p_00)
  RSI = (mean(p_11) - mean(p_00)) / (mean(p_01) - mean(p_00) + mean(p_10) - mean(p_00)) 
  return(list(res = c(RI = RI, RRERI = RRERI, RSI = RSI), OEC = list(Out = Out, Exp = Exp, Cov = Cov), data = data))
}
