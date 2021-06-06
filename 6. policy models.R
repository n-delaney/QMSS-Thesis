
library(dplyr)
library(reshape2)
library(ggplot2)
library(scales)
library(rvest)
library(stringr)
library(purrr)

url <- read_html('https://www.nerdwallet.com/blog/taxes/federal-income-tax-brackets/')

taxes_18 <- url %>%
  html_node(xpath = '//*[@id="tablepress-4176"]') %>% html_table() %>%
  mutate(tax_rate = as.numeric(gsub('\\%', '', `Tax rate`)) / 100,
         bracket = gsub('[\\,\\$]', '', `Taxable income bracket`),
         owed = gsub('[\\,\\$]', '', `Tax owed`),
         lower = as.numeric(regmatches(bracket, regexpr('([0-9]+)', bracket, perl=TRUE))),
         upper = as.numeric(gsub('\\$', '', str_extract(bracket, '\\s(\\d+)'))),
         base = as.numeric(str_extract(owed, '\\d+\\.\\d+')))

write.csv(taxes_18, paste0('./data/2018_tax_brackets_', Sys.Date(), '.csv', row.names = FALSE))

taxes_18 <- taxes_18 %>% select(tax_rate, base, lower, upper)
taxes_18

# Import relevant data 

```{r}
borrowers <- rbind(readRDS('./copula_outputs/rank_and_income_simulations.rds')$asec$female, 
                   readRDS('./copula_outputs/rank_and_income_simulations.rds')$asec$male)

# csv with income tax brackets
taxes_18 <- read.csv('./data/2018_tax_brackets_2019-12-10.csv')

# write function to get value of federal taxes for given earnings 
get_taxes <- function(x){
  if(x < taxes_18[1, ]$upper){
    out <- taxes_18[1, ]$tax_rate * x
  }
  else if(x >= taxes_18[2, ]$lower & x < taxes_18[2, ]$upper){
    out <- taxes_18[2, ]$base + (taxes_18[2, ]$tax_rate * x)
  }
  else if(x >= taxes_18[3, ]$lower & x < taxes_18[3, ]$upper){
    out <- taxes_18[3, ]$base + (taxes_18[3, ]$tax_rate * x)
  }
  else if(x >= taxes_18[4, ]$lower & x < taxes_18[4, ]$upper){
    out <- taxes_18[4, ]$base + (taxes_18[4, ]$tax_rate * x)
  }
  else if(x >= taxes_18[5, ]$lower & x < taxes_18[5, ]$upper){
    out <- taxes_18[5, ]$base + (taxes_18[5, ]$tax_rate * x)
  }
  else if(x >= taxes_18[6, ]$lower & x < taxes_18[6, ]$upper){
    out <- taxes_18[6, ]$base + (taxes_18[6, ]$tax_rate * x)
  }
  else if(x >= taxes_18[7, ]$lower){
    out <- taxes_18[7, ]$base + (taxes_18[7, ]$tax_rate * x)
  }
  return(out)
}

# calculate taxes for each individual for each year
borrowers <- borrowers %>% mutate(taxes = get_taxes(real_ern),
                                  post_tax = real_ern - taxes, 
                                  year = 1995 + age, 
                                  p_id = as.character(p_id))  
```

# Write function to simulate policy scenarios based on parameter inputs

function(threshold, int_rate, repay_rate, repay_period, cap_times_borrowed){
  max_age <- 23 + (repay_period - 1)
  b <- borrowers %>% filter(age >= 23 & age <= max_age) 
  # b <- b %>% filter(p_id %in% short_ids)
  
  ids <- unique(b$p_id)
  
  if(threshold == 'base'){
    threshold <- 12140 * 1.5 # 150% of 2018 poverty line for individuals
  }
  if(int_rate > 0){
    # Max. annual loan amount over 4 year education = 45000 plus interest
    loan_amt <-  (9500 * (1 + int_rate) ^ 4) + (10500 * (1 + int_rate) ^ 3) +
      (12500 * (1 + int_rate) ^ 2) + (12500 * (1 + int_rate) ^ 1)
    # for compounding inerest monthly
    int_month <-  1 + (int_rate / 12)
  }
  else{
    loan_amt <- 9500 + 10500 + 12500 + 12500
    int_month <-  1 + (int_rate / 12)
  }
  b <- b %>% mutate(annual_payment = ifelse(post_tax > threshold,
                                            (post_tax - threshold) * repay_rate, 0),
                    monthly_payment = annual_payment / 12)
  # expand out yearly frequency data to monthly frequency data
  month_year <- data.frame(month = numeric(), year = numeric())
  s_years <- c(2018:(2018 + max_age)) # pseudo-years of repayment period
  
  for(y in s_years){
    for(m in c(1:12)){ # 12 months per year
      month_year <- rbind(month_year, c(m, y))
    }
  }
  
  colnames(month_year) <- c('month', 'year')
  s <- month_year %>% left_join(b, by = 'year') %>%
    mutate(loan_out = NA)
  
  out <- data.frame(month = numeric(), year = numeric(), p_id = character(), rank = numeric(), 
                    age = numeric(), sex = character(), real_ern = numeric(), ern_quintile = factor(), 
                    taxes = numeric(), post_tax = numeric(), annual_payment = numeric(), 
                    monthly_payment = numeric(), loan_out = numeric())
  
  if(is.na(cap_times_borrowed) == TRUE){
    for(i in seq_along(ids)){
      if(i %% 100 == 0){
        print(paste('Running id #:', i, 'of 20,000'))
      }
      if(is.na(ids[i]) == F){
        sub <- s %>% filter(p_id == ids[i])
        for(i in c(1:nrow(sub))){
          if(i == 1){
            sub$loan_out[i] <- (loan_amt * int_month) - sub$monthly_payment[i]
            sub$total_payment[i] <- sub$monthly_payment[i]
          }
          else if(i > 1){
            sub$loan_out[i] <- (sub$loan_out[i - 1] * int_month) - sub$monthly_payment[i]
            sub$total_payment[i] <- sub$total_payment[i - 1] + sub$monthly_payment[i]
          }
        }
      }
      out <- rbind(out, sub)
    }
  }
  else if(is.na(cap_times_borrowed) == FALSE){
    stop_payment <- loan_amt - (2 * loan_amt)
    for(i in seq_along(ids)){
      if(i %% 100 == 0){
        print(paste('Running id #:', i, 'of 20,000'))
      }
      if(is.na(ids[i]) == F){
        sub <- s %>% filter(p_id == ids[i])
        for(i in c(1:nrow(sub))){
          if(i == 1){
            sub$loan_out[i] <- (loan_amt * int_month) - sub$monthly_payment[i]
            sub$total_payment[i] <- sub$monthly_payment[i]
          }
          else{
            if(sub$loan_out[i - 1] > stop_payment){
              sub$loan_out[i] <- (sub$loan_out[i - 1] * int_month) - sub$monthly_payment[i]
              sub$total_payment[i] <- sub$total_payment[i - 1] + sub$monthly_payment[i]
            }
            else{
              sub$annual_payment[i] <- 0
              sub$monthly_payment[i] <- 0
              sub$loan_out[i] <- sub$loan_out[i - 1] 
              sub$total_payment[i] <- sub$total_payment[i - 1] + sub$monthly_payment[i]
            }
          }
        }
      }
      out <- rbind(out, sub)
    }
  }
  return(out)
}

# Run policy scenarios and save outputs

scenarios <- list()

s_base <- policy_scenario(threshold = 'base', int_rate = 0.045, repay_rate = 0.1, repay_period = 20, cap_times_borrowed = NA)
print('s_base complete')
scenarios[['s_base']] <- s_base
saveRDS(scenarios, file = paste0('scenarios_', Sys.Date(), '.rds'))

s2 <- policy_scenario(threshold = 'base', int_rate = 0, repay_rate = 0.1, repay_period = 20, cap_times_borrowed = NA)
print('s2 complete')
scenarios[['s2']] <- s2
saveRDS(scenarios, file = paste0('scenarios_', Sys.Date(), '.rds'))

s3 <- policy_scenario(threshold = 30000, int_rate = 0, repay_rate = 0.1, repay_period = 20, cap_times_borrowed = NA)
print('s3 complete')
scenarios[['s3']] <- s3
saveRDS(scenarios, file = paste0('scenarios_', Sys.Date(), '.rds'))

s4 <- policy_scenario(threshold = 25000, int_rate = 0, repay_rate = 0.1, repay_period = 25, cap_times_borrowed = NA)
print('s4 complete')
scenarios[['s4']] <- s4
saveRDS(scenarios, file = paste0('scenarios_', Sys.Date(), '.rds'))

s5 <- policy_scenario(threshold = 25000, int_rate = 0, repay_rate = 0.03, repay_period = 43, cap_times_borrowed = NA)
print('s5 complete')
scenarios[['s5']] <- s5
saveRDS(scenarios, file = paste0('scenarios_', Sys.Date(), '.rds'))

s6 <- policy_scenario(threshold = 30000, int_rate = 0, repay_rate = 0.05, repay_period = 25, cap_times_borrowed = NA)
print('s6 complete')
scenarios[['s6']] <- s6
saveRDS(scenarios, file = paste0('scenarios_', Sys.Date(), '.rds'))

s7 <- policy_scenario(threshold = 30000, int_rate = 0, repay_rate = 0.10, repay_period = 25, cap_times_borrowed = 2.5)
print('s7 complete')
scenarios[['s7']] <- s7
saveRDS(scenarios, file = paste0('scenarios_', Sys.Date(), '.rds'))











