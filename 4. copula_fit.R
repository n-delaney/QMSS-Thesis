library(dplyr)
library(readr)
library(plm)
library(tidyr)
library(xtable)
library(VineCopula)
library(parallel)
library(ggplot2)
library(purrr)


# Read in data

copula_key <- read_csv('copula_key.csv')
grads_list <- readRDS('./formatted_NLSY_samples/grads_all_vars.rds')


# Fit samples
get_samples <- function(df){
  age_transitions = list()
  for(j in c(23:53)){
    ages_df <- df %>% filter(age == paste(j - 1) | age == paste(j) | 
                               age == paste(j + 1)) %>% 
      filter(is.na(ern_cdf) == F)
    ids <- (ages_df %>% group_by(id) %>% summarise(id_count = n()) %>% 
              filter(id_count == 3))$id
    if(length(ids) > 0){
      out <- data.frame()
      for(i in c(1:length(ids))){
        e1_i <- (ages_df %>% filter(id == ids[i] & age == j - 1))$ern_cdf
        e2_i <- (ages_df %>% filter(id == ids[i] & age == j))$ern_cdf
        e3_i <- (ages_df %>% filter(id == ids[i] & age == j + 1))$ern_cdf
        out[i, c('id', paste0('rank_', j - 1), paste0('rank_', j),  paste0('rank_', j + 1))] <- c(i, e1_i, e2_i, e3_i)
      }
      age_transitions[[paste0('age_', j)]] <- out
    }
    else{ # individuals observed at 2 year intervals after 2011
      for(k in c(31:51)){
        ages_df <- df %>% filter(age == paste(k - 2) | age == paste(k) |
                                   age == paste(k + 2)) %>%
          filter(is.na(ern_cdf) == F)
        ids <- (ages_df %>% group_by(id) %>% summarise(id_count = n()) %>%
                  filter(id_count == 3))$id
        out <- data.frame()
        for(i in c(1:length(ids))){
          e1_i <- (ages_df %>% filter(id == ids[i] & age == k - 2))$ern_cdf
          e2_i <- (ages_df %>% filter(id == ids[i] & age == k))$ern_cdf
          e3_i <- (ages_df %>% filter(id == ids[i] & age == k + 2))$ern_cdf
          out[i, c('id', paste0('rank_', k - 2), paste0('rank_', k),  paste0('rank_', k + 2))] <-  c(i, e1_i, e2_i, e3_i)
        }
        age_transitions[[paste0('age_', k)]] <- out
      }
    }
  }
  return(age_transitions)
}

age_samples_f <- get_samples(grads_list$grads_f)
age_samples_m <- get_samples(grads_list$grads_m)

raw_age_samples <- list(age_samples_f, age_samples_m)
names(raw_age_samples) <- c('age_samples_f', 'age_samples_m')
saveRDS(raw_age_samples, './copula_age_samples/raw_age_samples.rds')

raw_age_samples <- readRDS('./copula_age_samples/raw_age_samples.rds')

format_age_samples <- function(mylist){
  out <- data.frame()
  for(i in c(1:length(mylist))){
    out[i, c('age', 'observations')] <- c(i + 22, nrow(mylist[[i]]))
  } 
  return(out)
}

ages_f <- format_age_samples(raw_age_samples$age_samples_f) 
colnames(ages_f)[2] <- 'female'
ages_m <- format_age_samples(raw_age_samples$age_samples_m) 
colnames(ages_m)[2] <- 'male'

ages <- cbind(ages_f, ages_m[2]) %>% select(age, female, male) %>% 
  mutate(age = as.integer(age), female = as.integer(female), male = as.integer(male))
rownames(ages) <- NULL

unique(c97$birth_year)
unique(c79$birth_year)

c97 %>% filter(is.na(earnings) == F) %>% group_by(id) %>% summarise(count = n()) %>% arrange(desc(count))
c79 %>% filter(is.na(earnings) == F & age >= 35) %>% group_by(id) %>% summarise(count = n()) %>% arrange(desc(count))

fit_vine_cop <- function(df_list){
  map(df_list, function(df){
    m <- as.matrix(df[, c(2, 3, 4)]) # turn observed rank data into matrix
    
    vine_structure <- matrix(c(1, 0, 0, 3, 2, 0, 2, 3, 3), 3, 3, byrow = T)
    
    cop_estimate <- RVineCopSelect(m, familyset = 2, selectioncrit = 'AIC', 
                                   rotations = F, # making life easier by not considering right now
                                   se = T,
                                   Matrix = vine_structure, 
                                   method = 'mle', # alternatives 'itau' and 'mle' - 'itau' limited families, but maybe better?
                                   cores = 8)
    
    # create vine copula object with format sepecified by RVineCopSelect
    cop <- RVineMatrix(Matrix = vine_structure, 
                       family = cop_estimate$family, 
                       par = cop_estimate$par, 
                       par2 = cop_estimate$par2)
    
  })
}

copula_fits_f <- fit_vine_cop(age_samples_f)
copula_fits_m <- fit_vine_cop(age_samples_m)
copula_fits <- list(copula_fits_f, copula_fits_m)
names(copula_fits) <- c('female', 'male')

fit_seq_vine <- function(df_list, obj_list){
  map2(df_list, obj_list, function(df, obj){
    
    m <- as.matrix(df[, c(2, 3, 4)]) # turn observed rank data into matrix
    
    vine_structure <- matrix(c(1, 0, 0, 3, 2, 0, 2, 3, 3), 3, 3, byrow = T)
    
    cop_estimate <- RVineSeqEst(data = m,
                                RVM = obj,
                                method = 'itau',
                                se = T,
                                max.df = 15)
  })
}

seq_vine_f <- fit_seq_vine(age_samples_f, copula_fits_f)
seq_vine_m <- fit_seq_vine(age_samples_m, copula_fits_m)


get_params <- function(cop_list){
  k <- 1
  params <- data.frame()
  for(a in c(23:51)){
    if(a %in% c(23:30)){
      rho <- cop_list[[paste0('age_', a)]]$par[3, 1]
      se_rho <- cop_list[[paste0('age_', a)]]$se[3, 1]
      deg_f <- cop_list[[paste0('age_', a)]]$par2[3, 1]
      se_deg_f <- cop_list[[paste0('age_', a)]]$se2[3, 1]
      tau <- cop_list[[paste0('age_', a)]]$tau[3, 1]
      age <- a
      trans <- '1,2'
      params[k, c('rho', 'se_rho', 'deg_f', 'se_deg_f', 'tau', 'age', 'trans')] <- c(rho, se_rho, deg_f, se_deg_f, tau, age, trans)
      k <- k + 1
      
      rho <- cop_list[[paste0('age_', a)]]$par[3, 2]
      se_rho <- cop_list[[paste0('age_', a)]]$se[3, 2]
      deg_f <- cop_list[[paste0('age_', a)]]$par2[3, 2]
      se_deg_f <- cop_list[[paste0('age_', a)]]$se2[3, 2]
      tau <- cop_list[[paste0('age_', a)]]$tau[3, 2]
      age <- a + 1
      trans <- '2,3'
      params[k, c('rho', 'se_rho', 'deg_f', 'se_deg_f', 'tau', 'age', 'trans')] <- c(rho, se_rho, deg_f, se_deg_f, tau, age, trans)
      k <- k + 1
    }
    else if(a %in% c(31:51)){
      rho <- cop_list[[paste0('age_', a)]]$par[3, 1]
      se_rho <- cop_list[[paste0('age_', a)]]$se[3, 1]
      deg_f <- cop_list[[paste0('age_', a)]]$par2[3, 1]
      se_deg_f <- cop_list[[paste0('age_', a)]]$se2[3, 1]
      tau <- cop_list[[paste0('age_', a)]]$tau[3, 1]
      age <- a
      trans <- '1,2'
      params[k, c('rho', 'se_rho', 'deg_f', 'se_deg_f', 'tau', 'age', 'trans')] <- c(rho, se_rho, deg_f, se_deg_f, tau, age, trans)
      k <- k + 1
      
      rho <- cop_list[[paste0('age_', a)]]$par[3, 2]
      se_rho <- cop_list[[paste0('age_', a)]]$se[3, 2]
      deg_f <- cop_list[[paste0('age_', a)]]$par2[3, 2]
      se_deg_f <- cop_list[[paste0('age_', a)]]$se2[3, 2]
      tau <- cop_list[[paste0('age_', a)]]$tau[3, 2]
      age <- a + 2
      trans <- '2,3'
      params[k, c('rho', 'se_rho', 'deg_f', 'se_deg_f', 'tau', 'age', 'trans')] <- c(rho, se_rho, deg_f, se_deg_f, tau, age, trans)
      k <- k + 1
    }
  }
  params <- params %>% mutate(rho = as.numeric(rho), 
                              se_rho = as.numeric(se_rho), 
                              deg_f = as.numeric(deg_f), 
                              se_deg_f = as.numeric(se_deg_f), 
                              tau = as.numeric(tau), 
                              age = as.numeric(age))
  return(params)
}

get_params(seq_vine_f)


params_f <- get_params(seq_vine_f)
params_m <- get_params(seq_vine_m)
params_mf <- rbind(params_f %>% mutate(sex = 'female'),
                   params_m %>% mutate(sex = 'male'))

params <- list(params_f, params_m, params_mf)
names(params) <- c('female', 'male', 'all')

saveRDS(params, './copula_outputs/fitted_copula_params.rds')

# Plot rho and degrees of freedom (pooling across ages and transitions 1,2 and 2,3)

ggplot(data = params_mf) + 
  geom_line(aes(x = age, y = rho, color = 'raw_values')) + 
  geom_line(aes(x = age, y = rho + se_rho, color = 'raw_values', linetype = 'conf_int'), alpha = 0.6) +
  geom_line(aes(x = age, y = rho - se_rho, color = 'raw_values', linetype = 'conf_int'), alpha = 0.6) +
  facet_wrap(. ~ sex, labeller = as_labeller(c('female' = 'Female', 'male' = 'Male'))) + 
  geom_smooth(aes(x = age, y = rho, color = 'smoothed_values'), se = F) +
  scale_color_manual(name = NULL, 
                     values = c(raw_values = t_colors[2], smoothed_values = t_colors[3]), 
                     labels = c('raw values', 'smooth values')) +
  scale_linetype_manual(name = NULL, values = c(conf_int = 'dotdash'), labels = c('confidence interval')) +
  labs(title = 'Fitted Values of Rho (\u03c1)', 
       x = 'Age', y = 'Rho (\u03c1)') +
  thesis_theme + 
  theme(panel.grid.major.y = element_line(color = 'darkgrey',
                                          linetype = 'dotted', size = 0.3),
        panel.spacing.x = unit(1.25, 'lines'), 
        strip.background = element_rect(fill = 'gray95'))
ggsave(filename = paste0('./Plots/fitted_rho_', Sys.Date(), '.png'), height = 4, width = 6, units = 'in')

ggplot(data = params_mf) + 
  geom_line(aes(x = age, y = deg_f, color = 'raw_values')) + 
  geom_line(aes(x = age, y = deg_f + se_deg_f, color = 'raw_values', linetype = 'conf_int'), alpha = 0.6) +
  geom_line(aes(x = age, y = deg_f - se_deg_f, color = 'raw_values', linetype = 'conf_int'), alpha = 0.6) +
  facet_wrap(. ~ sex, labeller = as_labeller(c('female' = 'Female', 'male' = 'Male'))) + 
  geom_smooth(aes(x = age, y = deg_f, color = 'smoothed_values'), se = F) +
  scale_color_manual(name = NULL, 
                     values = c(raw_values = t_colors[2], smoothed_values = t_colors[3]), 
                     labels = c('raw values', 'smooth values')) +
  scale_linetype_manual(name = NULL, values = c(conf_int = 'dotdash'), labels = c('confidence interval')) +
  labs(title = 'Fitted Degrees of Freedom (\u03bd)', 
       x = 'Age', y = 'Degrees of Freedom (\u03bd)') +
  thesis_theme + 
  theme(panel.grid.major.y = element_line(color = 'darkgrey',
                                          linetype = 'dotted', size = 0.3),
        panel.spacing.x = unit(1.25, 'lines'), 
        strip.background = element_rect(fill = 'gray95'))
ggsave(filename = paste0('./Plots/fitted_deg_f_', Sys.Date(), '.png'), height = 4, width = 6, units = 'in')

