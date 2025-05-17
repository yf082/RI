boot_ci = function(res, n.boot = n.boot) {
  
  point.est = res$res
  data = res$data
  
  ## nonparametric bootstrap
  n = nrow(data)
  boot.est = replicate(n.boot, {
    id.boot = sample(1:n, n, replace = TRUE)
    ri_base(Out = res$OEC$Out,
            Exp = res$OEC$Exp,
            Cov = res$OEC$Cov,
            data = data[id.boot,],
            ...)$res
  })
  
  boot.se = apply(boot.est, 1, sd)
  
  Res= data.frame(eff = c("RI", "RRERI", "RSI"),
                  est = point.est,
                  se = boot.se)
  row.names(Res) = 1:nrow(Res)
  
  return(Res)
}


boot_ci_comb = function(res, n.boot = n.boot) {
  
  sub_result = res$result
  pop_pro = res$pop_pro
  pop_nam = res$pop_nam
  
  result = list()
  boot_est = list()
  boot_se = list()
  
  for (i in 1:length(sub_result)) {
    
    res = sub_result[[i]]
    
    data = res$data
    
    ## nonparametric bootstrap
    n = nrow(data)
    boot.est = replicate(n.boot, {
      id.boot = sample(1:n, n, replace = TRUE)
      ri_base(Out = res$OEC$Out,
              Exp = res$OEC$Exp,
              Cov = res$OEC$Cov,
              data = data[id.boot,])$res
    })
    
    result[[i]] = res$res
    boot_se[[i]] = apply(boot.est, 1, sd)
    boot_est[[i]] = boot.est
  }
  
  result = data.frame(group = pop_nam, 
                      rlist::list.rbind(result)) %>% 
    pivot_longer(cols = c("RI", "RRERI", "RSI"), 
                 names_to = "eff", 
                 values_to = "est")
  boot_se = data.frame(group = pop_nam, 
                       rlist::list.rbind(boot_se)) %>% 
    pivot_longer(cols = c("RI", "RRERI", "RSI"), 
                 names_to = "eff", 
                 values_to = "se")
  sub_est = bind_cols(result, se = boot_se$se)
  
  pop_est = matrix(0, nrow = 3, ncol = n.boot)
  for (i in 1:length(pop_pro)) {
    pop_med = boot_est[[i]]* pop_pro[i]
    pop_est = pop_est + pop_med
  }
  
  pop_est = data.frame(est = res$estmate$overall,
                       se = apply(pop_est, 1, sd))
  return(list(sub_est = sub_est, pop_est = pop_est))
}