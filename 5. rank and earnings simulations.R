library(dplyr)
library(VineCopula)
library(purrr)
library(ggplot2)
library(reshape2)
library(scales)
library(spatstat)
set.seed(12345)


raw_params <- readRDS('./copula_outputs/fitted_copula_params.rds')

#####################################################################
###       Turn Raw parameters into smoothed parameters            ###
#####################################################################


smooth_params <- list()
for(i in c('female', 'male')){
  d <- raw_params[[i]] 
  out <- data.frame(c(23:51))
  names(out) <- 'age'
  smooth_rho <- loess(rho ~ age, data = d, span = 0.7)
  smooth_deg_f <- loess(deg_f ~ age, data = d, span = 0.5)
  smooth_tau <- loess(tau ~ age, data = d, span = 0.5)
  out <- out %>% mutate(rho = predict(smooth_rho, age), 
                        deg_f = predict(smooth_deg_f, age), 
                        tau = predict(smooth_tau, age),
                        sex = i)
  smooth_params[[i]] <- out
}

# Replace degrees of freedom < 2 with 2 for BiCopCondSim()
# This is result of smoothing, minimum raw value is 2 anyway 
smooth_params$female[smooth_params$female$deg_f < 2, 'deg_f'] <- 2.0000001
smooth_params$male[smooth_params$male$deg_f < 2, 'deg_f'] <- 2.0000001

saveRDS(smooth_params, 'copula_outputs/smoothed_params.rds')

for(i in c('female', 'male')){
  p_rho <- ggplot() + 
    geom_line(data = smooth_params[[i]], aes(x = age, y = rho), color = t_colors[1], size = 0.7) + 
    geom_line(data = raw_params[[i]], aes(x = age, y = rho), color = t_colors[5], linetype = 'dashed') +
    labs(title = paste0('Rho (', i, ')'), 
         subtitle = 'Smoothed with loess span of 0.7', 
         x = 'Age', y = 'Rho (\u03c1)') + 
    thesis_theme
  print(p_rho)
  
  p_deg_f <- ggplot() + 
    geom_line(data = smooth_params[[i]], aes(x = age, y = deg_f), color = t_colors[1], size = 0.7) + 
    geom_line(data = raw_params[[i]], aes(x = age, y = deg_f), color = t_colors[5], linetype = 'dashed') +
    labs(title = paste0('Degrees of Freedom (', i, ')'), 
         subtitle = 'Smoothed with loess span of 0.5', 
         x = 'Age', y = 'Degrees of Freedom (\u03bd)') + 
    thesis_theme
  print(p_deg_f)
  
  p_tau <- ggplot() + 
    geom_line(data = smooth_params[[i]], aes(x = age, y = tau), color = t_colors[1], size = 0.7) + 
    geom_line(data = raw_params[[i]], aes(x = age, y = tau), color = t_colors[5], linetype = 'dashed') +
    labs(title = paste0('Kendall\'s Tau (', i, ')'), 
         subtitle = 'Smoothed with loess span of 0.5', 
         x = 'Age', y = 'Tau (\u03c4)') + # unicode value for tau
    thesis_theme
  print(p_tau)
} 


####################################################################################
###   Get rank simulations using draws from Bivariate earnings transitions       ###
####################################################################################

sims <- list()
for(s in c('female', 'male')){
  p_id <- paste0(substr(s, 1, 1), '_', c(1:10000)) # add unique identifier to each pseudo-person
  ranks <- data.frame(p_id)
  ranks$r_22 <- VineCopula::pobs(seq(0.0001, 1, 0.0001)) # use pobs to make sure true univariate
  for(i in c(23:70)){
    if(i <= 51){
      ranks[, paste0('r_', i)] <- BiCopCondSim(10000,
                                               cond.val = ranks[, paste0('r_', i - 1)], 
                                               cond.var = 1, # indicates that this is C_{2|1}
                                               family = 2, # Student-t
                                               par = (smooth_params[[s]] %>% filter(age == i))$rho,
                                               par2 = (smooth_params[[s]] %>% filter(age == i))$deg_f, 
                                               obj = NULL)
    }
    else if(i > 51){
      ranks[, paste0('r_', i)] <- BiCopCondSim(10000,
                                               cond.val = ranks[, paste0('r_', i - 1)], 
                                               cond.var = 1, # indicates that this is C_{2|1}
                                               family = 2, # Student-t
                                               par = (smooth_params[[s]] %>% filter(age == 51))$rho,
                                               par2 = (smooth_params[[s]] %>% filter(age == 51))$deg_f, 
                                               obj = NULL)
    }
  }
  out <- ranks %>% # reshape to long format
    melt(id.vars = 'p_id', variable.name = 'age_trans', value.name = 'rank') %>% 
    mutate(age = substr(age_trans, 3, 4), # pull age out of rank name
           age = as.integer(age), 
           sex = s) %>% 
    select(-age_trans)
  sims[[s]] <- out
}


####################################################################################
###         Scale to NLSY earnings to evaluate simulations                       ###
####################################################################################

nlsy_grads <- readRDS('./formatted_NLSY_samples/grads_all_vars.rds')
names(nlsy_grads) <- c('female', 'male')
nlsy_df <- rbind(nlsy_grads$female, nlsy_grads$male)

sims_inc_nlsy <- list() # scale simulated ranks to nlsy earnings
for(s in c('female', 'male')){
  out <- data.frame(p_id = factor(), rank = numeric(),
                    age = integer(), sex = character(), 
                    real_ern = numeric())
  for(i in 22:70){
    sub <- sims[[s]][sims[[s]]$age == i, ] %>% 
      # scale to real earnings observed in nlsy data using quantile function 
      # probs is rank - should give earnings associated with decimal of 
      # real earnings empirical CDF
      mutate(real_ern = quantile(nlsy_grads[[s]][nlsy_grads[[s]]$age == i, ]$real_ern, 
                                 probs = rank,
                                 na.rm = T), 
             sex = s)
    # print(sub)
    out <- rbind(out, sub)
  }
  sims_inc_nlsy[[s]] <- out
}


d_sims <- rbind(sims_inc_nlsy[['female']], sims_inc_nlsy[['male']]) %>% 
  mutate(source = 'simulated') %>% 
  mutate(ern_quintile = cut(rank, breaks = seq(0, 1, by = 0.20), 
                            labels = c('1', '2', '3', '4', '5'))) %>% 
  select(p_id, age, sex, real_ern, source, ern_quintile)
colnames(d_sims) <- c('id', 'age', 'sex', 'real_ern', 'source', 'ern_quintile')


c_all <- rbind(nlsy_grads$female, nlsy_grads$male)
c_all <- c_all %>% select(id, age, sex, real_ern, ern_quintile) %>% 
  mutate(source = 'observed') %>% filter(is.na(real_ern) == F)

d <- rbind(d_sims, c_all) # combine observed and simualted data into single dataframe

ggplot() +
  geom_smooth(data = d %>% filter(source == 'observed'), 
              mapping = aes(x = age, y = real_ern, group = ern_quintile, 
                            linetype = ern_quintile, color = 'obs'), se = F, size = 0.8) + 
  geom_smooth(data = d %>% filter(source == 'simulated'), 
              mapping = aes(x = age, y = real_ern, group = ern_quintile, 
                            linetype = ern_quintile, color = 'sim'), se = F, size = 0.8) +
  scale_x_continuous(breaks = pretty_breaks()) + 
  scale_y_continuous(breaks = pretty_breaks()) +
  scale_color_manual(name = 'Source', 
                     values = c('obs' = t_colors[2], 'sim' = t_colors[3]), 
                     labels = c('observed', 'simulated')) +
  scale_linetype_manual(name = 'Earnings\nQuintile',
                        values = c('longdash', 'dotdash', 'dotted', 'dashed', 'twodash')
  ) +
  facet_wrap(sex ~ .) +
  labs(title = 'Observed NLSY Earnings and Simulated Equivalents',
       x = 'Age', y = 'Earnings (constant 2018 USD)') +
  guides(linetype = guide_legend(override.aes=list(colour = 'gray55'))) +
  thesis_theme + 
  theme(panel.grid.major.y = element_line(color = 'darkgrey', 
                                          linetype = 'dotted', size = 0.3), 
        panel.spacing.x = unit(1.25, 'lines'), 
        strip.background = element_rect(fill = 'gray95'))

ggsave(filename = paste0('./Plots/nlsy_vs_sim_earnings_', Sys.Date(), '.png'), width = 6, height = 4, units = 'in')


####################################################################################
###   Process CPS data and use to scale earnings ranks                           ###
####################################################################################

cps <- read.csv('cps_11-12-19.csv') 
colnames(cps) <- tolower(colnames(cps))

cps <- cps %>% select(year, sex, age, race, cpsid, incwage, educ, educ99, earnwt, asecwt, wtfinl) %>% 
  filter(is.na(incwage) == F & incwage != 9999999) %>% # remove missing values
  mutate(ba = ifelse(educ99 >= 15, 1, 0), 
         sex = ifelse(sex == 1, 'male', 'female')) %>% 
  filter(ba == 1 & age >= 22 & year != 2019)

# import CPI information to deflate earnings 
library(readr)
cpi <- read_csv('./CPI_world_bank/CPI_world_bank.csv', skip = 4)
cpi <- (cpi %>% filter(`Country Name` == 'United States'))[, c(5:ncol(cpi))] %>% 
  melt(variable.name = 'year', value.name = 'CPI_2010') %>% 
  mutate(year = as.character(year)) %>% 
  arrange(desc(CPI_2010)) %>% 
  mutate(CPI_2018 = CPI_2010  * (100 / 115.1573), 
         CPI_18 = CPI_2018 / 100)

cps_sparse <- data.frame(year = numeric(), sex = character(), 
                         age = numeric(), real_ern = numeric(), 
                         asecwt = numeric())

for(i in unique(cps$year)){
  deflator <- (cpi %>% filter(year == i))$CPI_18
  sub <- cps %>% filter(year == i) %>%  
    mutate(real_ern = incwage / deflator) %>% 
    select(year, sex, age, real_ern, asecwt)
  cps_sparse <- rbind(cps_sparse, sub)
}

# add earnings quintiles 
scale_earnings <- function(df){
  out <- data.frame()
  for(i in c(22:79)){
    sub <- df %>% filter(age == i)
    if(nrow(sub %>% filter(is.na(real_ern) == F)) >= 25){
      get_rank <- ewcdf(sub$real_ern, weights = sub$asecwt, # use asec sample weights
                        normalise = T)
      # should be max. 2.2 %-ile difference between ewcdf and ecdf ranks
      get_rank_uwt <- ecdf(sub$real_ern)
      out <- rbind(out, sub %>% 
                     mutate(rank = get_rank(real_ern),
                            rank_uwt = get_rank_uwt(real_ern), # see difference with unweighted cdf
                            ern_quint_uwt = cut(rank_uwt, 
                                                breaks = seq(0, 1, by = 0.2),  
                                                labels = c('1', '2', '3', '4', '5')),
                            ern_quintile = cut(rank, 
                                               breaks = seq(0, 1, by = 0.2),  
                                               labels = c('1', '2', '3', '4', '5'))))
    }
  }
  return(out)
}

cps_sparse <- scale_earnings(cps_sparse)

######################################################################
###         Compare NLSY and CPS earnings data                     ###
######################################################################

d <- rbind(cps_sparse %>% select(age, real_ern, ern_quintile, sex) %>% mutate(data_source = 'asec'), 
           nlsy_df %>% filter(is.na(real_ern) == F) %>%
             select(age, real_ern, ern_quintile, sex) %>% mutate(data_source = 'nlsy'))

ggplot(d) + 
  geom_smooth(aes(x = age, y = real_ern, group = ern_quintile, color = ern_quintile), 
              method = 'gam', formula = y ~ s(x, bs = 'cs'), se = F, size = 0.8) +
  facet_grid(sex ~ data_source, labeller = as_labeller(c('female' = 'Female', 'male' = 'Male', 
                                                         'nlsy' = 'NLSY', 'asec' = 'CPS'))) + 
  labs(title = 'Comparing Earnings Data',
       subtitle = 'National Longitudinal Survey of Youth vs Current Population Survey', 
       caption = 'Note: NLSY earnings data extends to age 53 only',
       x = 'Age', y = 'Earnings') + 
  scale_color_manual(values = t_colors, name = 'Earnings\nQuintile', 
                     breaks = c(1, 2, 3, 4, 5), 
                     labels = c('1st', '2nd', '3rd', '4th', '5th')) +
  scale_y_continuous(label=comma, breaks = pretty_breaks()) + 
  guides(linetype = guide_legend(reverse = TRUE)) +
  thesis_theme + 
  theme(panel.grid.major.y = element_line(color = 'darkgrey', 
                                          linetype = 'dotted', size = 0.3), 
        panel.spacing.x = unit(1.25, 'lines'), 
        strip.background = element_rect(fill = 'gray95'))

ggsave(filename = paste0('./Plots/CPS_NLSY_earnings_comparison_', Sys.Date(), '.png'), height = 4, width = 7)


sims_inc_asec <- list() # scale simulated ranks to nlsy earnings
for(s in c('female', 'male')){
  out <- data.frame(p_id = factor(), rank = numeric(),
                    age = integer(), sex = character(), 
                    real_ern = numeric())
  for(i in 22:70){
    sub <- sims[[s]][sims[[s]]$age == i, ] %>% 
      # scale to real earnings observed in asec data using quantile function 
      # probs is rank - should give earnings associated with decimal of 
      # real earnings empirical CDF
      mutate(real_ern = quantile((cps_sparse %>% filter(age == i & sex == s))$real_ern, 
                                 probs = rank,
                                 na.rm = T), 
             sex = s, 
             ern_quintile = cut(rank, breaks = seq(0, 1, 0.2), 
                                labels = c('1', '2', '3', '4', '5')))
    # print(sub)
    out <- rbind(out, sub)
  }
  sims_inc_asec[[s]] <- out
}

all_sims <- list(sims, sims_inc_nlsy, sims_inc_asec)
names(all_sims) <- c('ranks_only', 'nsly', 'asec')

saveRDS(all_sims, './copula_outputs/rank_and_income_simulations.rds')

######################################################################
###      Evaluate Simulated CPS (ASEC) earnings fit                ###
######################################################################


d <- rbind(cps_sparse %>% select(age, real_ern, ern_quintile, sex) %>% mutate(data_source = 'asec'), 
           sims_inc_asec[['female']] %>% select(age, real_ern, ern_quintile, sex) %>% mutate(data_source = 'sims'), 
           sims_inc_asec[['male']] %>% select(age, real_ern, ern_quintile, sex) %>% mutate(data_source = 'sims'))

ggplot(mapping = aes(x = age, y = real_ern, group = ern_quintile, linetype = ern_quintile)) + 
  geom_smooth(data = d %>% filter(data_source == 'asec'), mapping = aes(color = 'cps'), se = F) + 
  geom_smooth(data = d %>% filter(data_source == 'sims'), mapping = aes(color = 'simulated'), se = F) + 
  scale_y_continuous(label = comma, breaks = pretty_breaks()) + 
  scale_x_continuous(breaks = pretty_breaks()) +
  scale_linetype_manual(name = 'Earnings\nQuintile', 
                        values =  c('dashed', 'dotted', 'dotdash', 'longdash', 'twodash'), 
                        labels = c('1st', '2nd', '3rd', '4th', '5th'),
                        guide = guide_legend(reverse = TRUE)) +
  scale_color_manual(name = 'Earnings\nSource', 
                     values = c(cps = t_colors[3], simulated = t_colors[5]), 
                     labels = c('Observed CPS data', 'Simulated Data')) + 
  facet_wrap(. ~ sex, labeller = as_labeller(c('female' = 'Female', 'male' = 'Male'))) + 
  labs(title = 'Simulated and Observed Earnings', 
       subtitle = 'Earnings rank simulations scaled to Current Population Survey Data',
       x = 'Age', y = 'Real Earnings (2019 dollars)') +
  thesis_theme + 
  theme(panel.grid.major.y = element_line(color = 'darkgrey', 
                                          linetype = 'dotted', size = 0.3), 
        panel.spacing.x = unit(1.25, 'lines'), 
        strip.background = element_rect(fill = 'gray95'))

ggsave(filename = paste0('./Plots/simulated_vs_obs_cps_', Sys.Date(), '.png'))



