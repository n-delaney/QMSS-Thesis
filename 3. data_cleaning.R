library(dplyr)
library(readr)
library(reshape2)
library(spatstat)

#################################################################################
###     IMPORT AND CLEAN 1997 COHORT                                          ###
#################################################################################

codes_97 <- read.csv('./NLSY97/codes_97.csv')
codes_97$year <- as.character(codes_97$year)

data_97$id <- paste0('c97_', data_97$R0000100)

demog <- data_97 %>% 
  select(R0536300, # sex
         R0536402, # birth year
         Z9083900, # education - degree attainment
         R1482600, # race
         id) %>% 
  mutate(sex = R0536300, 
         sex = replace(sex, sex == 2, 0), 
         sex = factor(sex, levels = c(0, 1), labels = c('female', 'male')),
         race = factor(R1482600, levels = c(1, 2, 3, 4), 
                       labels = c('black', 'hispanic', 
                                  'mixed-race', 'non-black, non-hispanic')),
         birth_year = R0536402, 
         degree = Z9083900,
         ba = ifelse(degree >= 4, 1, 0),
         degree = factor(degree, levels = c(0:7), 
                         labels = c('None', 'GED', 'HS diploma', 
                                    'AA', 'BA', 'MA', 'PhD', 'Prof'))) %>%
  select(sex, birth_year, degree, ba, id, race)

reformat_nlsy97 <- function(search_string, var_name, question = c(TRUE, FALSE)){
  if(question == F){
    vars <- codes_97[grep(search_string, codes_97$var, ignore.case = T), ]
    sub <- data_97 %>% select(vars$ref, id)
    colnames(sub)[c(1:ncol(sub) - 1)] <- vars$year
    out <- sub %>% melt(id.vars = 'id', variable.name = 'year', value.name = var_name)
    out$year <- as.character(out$year)
  }
  else if(question == T){
    vars <- codes_97[grep(search_string, codes_97$question, ignore.case = T), ]
    sub <- data_97 %>% select(vars$ref, id)
    colnames(sub)[c(1:ncol(sub) - 1)] <- vars$year
    out <- sub %>% melt(id.vars = 'id', variable.name = 'year', value.name = var_name)
    out$year <- as.character(out$year)
  }
  return(out)
}

earnings_df <- reformat_nlsy97('total income from wages and salary in past year', 
                               'earnings', question = FALSE)

emp_stat_df <- reformat_nlsy97('CV_ESR_COLLAPSED', 'emp_stat', question = TRUE) %>%
  mutate(emp_stat = factor(emp_stat, levels = c(1:4), # change integers to factor labels
                           labels = c('employed', 'unemployed',
                                      'not in labor force',
                                      'active armed forces')))

s_weights_df <- reformat_nlsy97('SAMPLING_WEIGHT_CC', 
                                'sample_wt', question = TRUE)

c97 <- full_join(demog, earnings_df, by = 'id') %>% 
  full_join(emp_stat_df, by = c('id', 'year')) %>%
  full_join(s_weights_df, by = c('id', 'year')) %>%
  mutate(age = as.numeric(year) - birth_year) %>% 
  filter(age >= 22) %>% 
  mutate(cohort = '97')

cpi <- read_csv('./CPI_world_bank/CPI_world_bank.csv', skip = 4)
cpi <- cpi %>% filter(`Country Name` == 'United States')
nlsy97_years <- unique(c97$year)
cpi <- cpi[, c(nlsy97_years, 2016, 2017, 2018)] %>% 
  melt(variable.name = 'year', value.name = 'CPI_2010') %>% 
  mutate(year = as.character(year))

cpi <- cpi %>% arrange(desc(CPI_2010)) %>% 
  mutate(CPI_2018 = CPI_2010 * (100 / 115.1573), 
         CPI_18 = CPI_2018 / 100)


real_c97 <- data.frame()

for(y in nlsy97_years){
  sub <- c97 %>% filter(year == y)
  deflator <- (cpi %>% filter(year == y))$CPI_18
  sub$real_ern <- sub$earnings / deflator
  real_c97 <- rbind(real_c97, sub)
}

c97 <- real_c97 %>% 
  select(id, # select so that order is standardized with c79
         year, sex, age, birth_year, degree, ba,
         race, earnings, real_ern, emp_stat, 
         sample_wt, cohort) 

write.csv(c97, 'c97.csv')


#################################################################################
###     IMPORT AND CLEAN 1979 COHORT                                          ###
#################################################################################

codes_79 <- read.csv('./NLSY79/codebook_79.csv') %>% select(-X)
codes_79 <- codes_79 %>% mutate(var = as.character(var), 
                                year = as.character(year), 
                                question = as.character(question), 
                                ref = as.character(ref))

data_79$id <- paste0('c79_', data_79$R0000100)

demog <- data_79 %>% 
  select(R0214800, # sex
         R0000500, # birth year
         T1215600 , # education - degree attainment
         R0214700, # race
         id) %>% 
  mutate(sex = R0214800,
         sex = replace(sex, sex == 2, 0),
         sex = factor(sex, levels = c(0, 1), labels = c('female', 'male')),
         race = R0214700,
         race = factor(R0214700, levels = c(1, 2, 3),
                       labels = c('hispanic', 'black', 'non-black, non-hispanic')),
         birth_year = 1900 + R0000500,
         degree = T1215600,
         ba = ifelse(degree >= 3 & degree != 8, 1, 0), 
         degree = factor(degree, levels = c(0:8),
                         labels = c('None', 'HS or GED', 'AA',
                                    'BA', 'BS',
                                    'Masters', 'PhD', 'Prof', 'Other'))) %>% 
  select(sex, birth_year, degree, ba, id, race)

reformat_nlsy79 <- function(search_string, var_name, question = c(TRUE, FALSE)){
  if(question == F){
    vars <- codes_79[grep(search_string, codes_79$var, ignore.case = T), ]
    sub <- data_79 %>% select(vars$ref, id)
    colnames(sub)[c(1:ncol(sub) - 1)] <- vars$year
    out <- sub %>% melt(id.vars = 'id', variable.name = 'year', value.name = var_name)
    out$year <- as.character(out$year)
  }
  else if(question == T){
    vars <- codes_79[grep(search_string, codes_79$question, ignore.case = T), ]
    sub <- data_79 %>% select(vars$ref, id)
    colnames(sub)[c(1:ncol(sub) - 1)] <- vars$year
    out <- sub %>% melt(id.vars = 'id', variable.name = 'year', value.name = var_name)
    out$year <- as.character(out$year)
  }
  return(out)
}

earnings_df <- data_79 %>% select((codes_79 %>% filter(question == 'Q13-5_TRUNC' | question == 'Q13-5_TRUNC_REVISED'))$ref, id)
colnames(earnings_df)[c(1:ncol(earnings_df) - 1)] <- (codes_79 %>% filter(question == 'Q13-5_TRUNC' | question == 'Q13-5_TRUNC_REVISED'))$year
earnings_df <- earnings_df %>% melt(id.vars = 'id', variable.name = 'year', value.name = 'earnings') %>% mutate(year = as.character(year))

emp_stat_df <- reformat_nlsy79('ESR_COL', 'emp_stat', question = T) %>% 
  mutate(emp_stat = factor(emp_stat, levels = c(1:4), 
                           labels = c('employed', 
                                      'unemployed', 
                                      'out of LF', 
                                      'in active forces')))

s_weights_df <- reformat_nlsy79('C_SAMPWEIGHT', 'sample_wt', question = T)

c79 <- full_join(demog, earnings_df, by = 'id') %>% 
  full_join(emp_stat_df, by = c('id', 'year')) %>%
  full_join(s_weights_df, by = c('id', 'year')) %>%
  mutate(age = as.numeric(year) - birth_year) %>% 
  filter(age >= 22) %>% 
  mutate(cohort = '79')

nlsy79_years <- unique(c79$year)

cpi <- read_csv('./CPI_world_bank/CPI_world_bank.csv', skip = 4)

cpi <- (cpi %>% filter(`Country Name` == 'United States'))[, c(5:ncol(cpi))] %>% 
  melt(variable.name = 'year', value.name = 'CPI_2010') %>% 
  mutate(year = as.character(year)) %>% 
  arrange(desc(CPI_2010)) %>% 
  mutate(CPI_2018 = CPI_2010  * (100 / 115.1573), 
         CPI_18 = CPI_2018 / 100)

real_c79 <- data.frame()

for(y in nlsy79_years){
  sub = data.frame()
  sub <- c79 %>% filter(year == y)
  deflator <- (cpi %>% filter(year == y))$CPI_18
  sub$real_ern <- sub$earnings / deflator
  real_c79 <- rbind(real_c79, sub)
}

c79 <- real_c79 %>% select(id, # select so that order is standardized with c97
                           year, sex, age, birth_year, degree, ba,
                           race, earnings, real_ern, emp_stat, 
                           sample_wt, cohort)

write.csv(c79, 'c79.csv')


#################################################################################
###     MERGE AND ASSIGN WEIGHTTED CDF                                        ###
#################################################################################


library(dplyr)
library(readr)
library(plm)
library(tidyr)
library(xtable)
library(VineCopula)
library(parallel)
library(purrr)


c79_sub <- c79 %>% filter(age >= 31) # include '79 cohort for ages 31+ only
c_all <- rbind(c79_sub, c97)

c_all %>% filter(is.na(earnings) == F) %>% group_by(age) %>% summarise(n())

grads_f <- c_all %>% filter(sex == 'female' & ba == 1)
grads_m <- c_all %>% filter(sex == 'male' & ba == 1)

scale_earnings <- function(df){
  out <- data.frame()
  for(i in c(22:65)){
    sub <- df %>% filter(age == i)
    if(nrow(sub %>% filter(is.na(real_ern) == F)) >= 5){
      weighted_cdf <- ewcdf(sub$real_ern, sub$sample_wt, normalise = TRUE)
      out <- rbind(out, sub %>%
                     mutate(ern_cdf = weighted_cdf(real_ern),
                            ern_quintile = cut(ern_cdf, 
                                               breaks = seq(0, 1, by = 0.2), 
                                               labels = c('1', '2', '3', '4', '5'))))
    }
  }
  return(out)
}

grads_f <- scale_earnings(grads_f)
grads_m <- scale_earnings(grads_m)

grads_list <- list(grads_f, grads_m)
names(grads_list) <- c('grads_f', 'grads_m')

saveRDS(grads_list, 'grads_all_vars.rds')



