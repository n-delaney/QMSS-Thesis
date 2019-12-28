
library(dplyr)
library(reshape2)
library(ggplot2)
library(scales)
library(rvest)
library(stringr)
library(purrr)

scenarios <- readRDS('scenarios_2019-12-16.rds')


# Show difference between loan balance and repayments


# get outstanding loan balance for s_base
df1 <- scenarios$s_base %>% filter(month == 12) %>% group_by(year) %>% 
  summarise(total_outstanding = sum(loan_out)) %>% 
  mutate(year = year - 2017, 
         total_outstanding = total_outstanding / 1000000000)

df2 <- scenarios$s_base %>% group_by(year) %>% summarise(tot_paid_year = sum(monthly_payment)) %>% 
  mutate(tot_paid_year = tot_paid_year / 1000000000, 
         cumulative_paid = NA, 
         year = year - 2017)

for(i in c(1:20)){
  if(i == 1){
    df2$cumulative_paid[i] <- df2$tot_paid_year[i]
  }
  else{
    df2$cumulative_paid[i] <- df2$cumulative_paid[i - 1] + df2$tot_paid_year[i]
  }
}

ggplot() +
  geom_line(data = df1, mapping = aes(x = year, y = total_outstanding, 
                                      color = 'Pooled loan balance outstanding', linetype = 'Pooled loan balance outstanding')) +
  geom_line(data = df2, mapping = aes(x = year, y = cumulative_paid, 
                                      color = 'Cumulative revenue collected', linetype = 'Cumulative revenue collected')) +
  scale_y_continuous(breaks = seq(0, 2, 0.5)) +
  scale_x_continuous(breaks = seq(0, 25, 5)) +
  scale_color_manual(name = NULL, 
                     values = c('Pooled loan balance outstanding' = t_colors[2], 'Cumulative revenue collected' = t_colors[3]),
                     guide = 'legend') +
  scale_linetype_manual(name = NULL, 
                        values = c('Pooled loan balance outstanding' = 'dotted', 'Cumulative revenue collected' = 'solid'), 
                        guide = 'legend') +
  labs(x = 'Years of repayment', y = 'Billions of dollars (constant 2018 USD)') +
  coord_fixed(ratio = 10) +
  thesis_theme +
  theme(legend.position = 'bottom', 
        panel.grid.major.y = element_line(color = 'darkgrey', 
                                          linetype = 'dotted', size = 0.3)) 


ggsave(filename = paste0('./Plots/revenue_vs_outsanding_', Sys.Date(), '.png'))



# Model cash flows

options(warn=-1)
plot_cash_flows <- function(df_list){
  # d <- data.frame(year = numeric(), receipts = numeric(), 
  #                 s_name = character(), 
  #                 cum_rev = numeric()) # need a measure of cumulative revenue
  for(i in c(1:length(df_list))){
    df <- df_list[[i]] %>% group_by(year) %>% summarise(receipts = sum(monthly_payment) / 1000000) %>% 
      mutate(year = year - 2017) %>% 
      mutate(s_name = names(df_list[i]))
    # df$cum_rev = NA
    # loop through to get cummulative revenue
    for(j in c(1:nrow(df))){
      if(j == 1){
        df$cum_rev[j] <- df$receipts[j]
      }
      else{
        df$cum_rev[j] <- df$cum_rev[j - 1] + df$receipts[j]
      }
    }
    if(i == 1){
      d <- df
    }
    else{
      d <- rbind(d, df)
    }
  }
  d$loan_amount <- 900
  
  ggplot(d) +
    geom_line(aes(x = year, y = cum_rev, linetype = s_name), color = t_colors[2]) +
    geom_line(aes(x = year, y = loan_amount, color = 'my_color'), linetype = 'longdash', size = 0.8, alpha = 0.6) +
    scale_y_continuous(labels = dollar) +
    scale_x_continuous(breaks = seq(0, 50, 5)) +
    scale_color_manual(name = NULL, values = c('my_color' = t_colors[5]), labels = 'disbursed loan amount', guide = 'legend') +
    scale_linetype_manual(name = 'Revenue Collected', values = c('dashed', 'dotdash', 'dotted'), 
                          labels = c('REPAYE', 'graduate tax', 'capped repayment')) + 
    labs(y = 'Millions of constant 2018 USD',
         x = 'Years of Repayment') +
    thesis_theme
}

plot_cash_flows(scenarios[c('s_base', 's5', 's7')])
ggsave(filename = paste0('./Plots/revenues_three_senarios_', Sys.Date(), '.png'))




# Find IRR


# write function to find NPV with a given discount rate
get_cash_flows <- function(df, disc_rate){
  options(scipen = 999)
  cash_out <- -1 * 20000 *(9500 + 10500 + 12500 + 12500) 
  
  d <- df %>% group_by(month, year) %>% summarise(payment = sum(monthly_payment)) %>% 
    arrange(year) %>% mutate(month_index = NA)
  
  # create an index value to count number of months passed and use as exponent FV
  i <- 1
  for(m in c(1:nrow(d))){
    d$month_index[m] <- i
    i <- i + 1
  }
  d <- d %>% mutate(present_value = payment / (1 + (disc_rate / 12)) ^ month_index)
  
  sum_pv <- sum(d$present_value)
  npv <- cash_out + sum_pv
  
  # print(sum(d$payment))
  return(npv)
}

# find IRRs - get NPV as close to zero as possible
get_cash_flows(scenarios$s2, disc_rate = 0.0475864) # IRR = 4.75 %
get_cash_flows(scenarios$s5, disc_rate = 0.006276202) # IRR = 0.627 %
get_cash_flows(scenarios$s7, disc_rate = 0.01765641) # IRR = 1.76 %


# Fraction Borrowers in Non-Payment by Age


s5_np_by_age <- scenarios$s5 %>% filter(month == 12) %>% 
  mutate(non_repay = ifelse(annual_payment == 0, 1, 0)) %>% 
  group_by(age, sex) %>% 
  summarise(count_by_age = sum(non_repay), fraction = count_by_age / 20000) %>% mutate(s_name = 's5')

s_base_np_by_age <- scenarios$s_base %>% filter(month == 12) %>% 
  mutate(non_repay = ifelse(annual_payment == 0, 1, 0)) %>% 
  group_by(age, sex) %>% 
  summarise(count_by_age = sum(non_repay), fraction = count_by_age / 20000) %>% mutate(s_name = 's_base')

s7_np_by_age <- scenarios$s7 %>% filter(month == 12) %>% 
  mutate(non_repay = ifelse(annual_payment == 0, 1, 0)) %>% 
  group_by(age, sex) %>% 
  summarise(count_by_age = sum(non_repay), fraction = count_by_age / 20000) %>% mutate(s_name = 's7')

df <- rbind(s5_np_by_age, s_base_np_by_age, s7_np_by_age)

ggplot(df) + 
  geom_bar(aes(x = age, y = fraction, fill = sex), stat = 'identity', position = 'dodge') + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), breaks = pretty_breaks()) + 
  scale_x_continuous(breaks = seq(0, 70, 2)) + 
  scale_fill_manual(values = t_colors[c(3, 2)]) + 
  guides(fill = guide_legend(title = 'Sex of\nBorrower')) + 
  facet_grid(s_name ~ ., 
             labeller = as_labeller(c('s_base' = 'Modified REPAYE', 's5' = 'Graduate Tax', 's7' = 'Capped Repayment'))) +
  labs(
    # title = 'Percent of Borrowers in Non-Repayment by Age',
    #    subtitle = '(Repayment Threshold of $30,000)',
    x = 'Age', y = 'Percent of same-sex cohort non-repayment') +
  thesis_theme +
  theme(strip.background = element_rect(fill = 'gray95'), 
        panel.grid.major.y = element_line(color = 'darkgrey', 
                                          linetype = 'dotted', size = 0.3), 
        panel.spacing = unit(1.25, 'lines'))

# ggsave(filename = paste0(Sys.Date(),'non_repaye_age.png'))
# ggsave(paste0('/Users/noradelaney/Desktop/', Sys.Date(), 'non_repaye_age.png'))



s7_np_by_age %>% filter(age >= 25 & age < 40) %>% filter(sex == 'female') %>% arrange(desc(fraction))



# Proportion paid down by decile of lifetime earnings



dfs <- scenarios[c('s_base', 's5', 's7')]

# loop through and tally up payment proportions by sex and lifetime earnings decile
for(i in c(1:3)){
  df <- dfs[[i]] %>% filter(month == 1) %>% # just get one annual payment amount
    group_by(p_id, sex) %>% summarise(life_pay = sum(annual_payment)) %>% 
    group_by(sex) %>% mutate(decile = cut(life_pay, include.lowest = T, 
                                          breaks = quantile(life_pay, probs = seq(0, 1, 0.1)),
                                          labels = seq(1, 10, 1), ordered_result = T), 
                             decile = as.numeric(decile))
  
  
  decile_df <- df %>% group_by(sex, decile) %>% summarise(sum_payments = sum(life_pay))
  
  # calculate decile totals as proportion of total payments
  total_payments <- sum(df$life_pay)
  decile_df <- decile_df %>% mutate(prop = sum_payments / total_payments, 
                                    s_name = names(dfs[i]))
  if(i == 1){
    decile_payments <- decile_df
  }
  else{
    decile_payments <- rbind(decile_payments, decile_df)
  }
}



ggplot(decile_payments) + 
  geom_bar(aes(x = decile, y = prop, fill = sex), stat = 'identity', position = 'dodge') + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1), breaks = pretty_breaks()) + 
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  scale_fill_manual(values = t_colors[c(3, 2)]) +
  guides(fill = guide_legend(title = 'Sex of\nBorrower')) +
  facet_grid(s_name ~ .,
             labeller = as_labeller(c('s_base' = 'Modified REPAYE', 's5' = 'Graduate Tax', 's7' = 'Capped Repayment'))) +
  labs(
    # title = 'Percent of Borrowers in Non-Repayment by Age',
    #    subtitle = '(Repayment Threshold of $30,000)',
    x = 'Decile of lifetime earnings', y = 'Percent of loan balance paid') +
  thesis_theme +
  theme(strip.background = element_rect(fill = 'gray95'), 
        panel.grid.major.y = element_line(color = 'darkgrey', 
                                          linetype = 'dotted', size = 0.3), 
        panel.spacing = unit(1.25, 'lines'))



# Plot percent of loan paid 

# loop through and tally up payment proportions by sex and lifetime earnings decile
for(i in c(1:3)){
  df <- dfs[[i]] %>% filter(month == 1) %>% # just get one annual payment amount
    group_by(p_id, sex) %>% summarise(life_pay = sum(annual_payment)) %>% 
    group_by(sex) %>% mutate(decile = cut(life_pay, include.lowest = T, 
                                          breaks = quantile(life_pay, probs = seq(0, 1, 0.1)),
                                          labels = seq(1, 10, 1), ordered_result = T), 
                             decile = as.numeric(decile))
  
  decile_df <- df %>% group_by(decile, sex) %>% summarise(sum_payments = sum(life_pay)) %>% 
    mutate(prop = sum_payments / (1000 * 45000), # scale to sum of loans taken out by that decile
           s_name = names(dfs[i])) 
  
  # make into single dataframe
  if(i == 1){
    decile_loan_scale <- decile_df
  }
  else{
    decile_loan_scale <- rbind(decile_loan_scale, decile_df)
  }
}

ggplot(decile_loan_scale) + 
  geom_bar(aes(x = decile, y = prop, fill = sex), stat = 'identity', position = 'dodge') + 
  scale_y_continuous(breaks = seq(1, 7, 1)) +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  scale_fill_manual(values = t_colors[c(3, 2)]) +
  guides(fill = guide_legend(title = 'Sex of\nBorrower')) +
  facet_grid(s_name ~ .,
             labeller = as_labeller(c('s_base' = 'Modified REPAYE', 's5' = 'Graduate Tax', 's7' = 'Capped Repayment'))) +
  labs(
    # title = 'Percent of Borrowers in Non-Repayment by Age',
    #    subtitle = '(Repayment Threshold of $30,000)',
    x = 'Decile of lifetime earnings', y = 'Multiple of loan balance paid') +
  thesis_theme +
  theme(strip.background = element_rect(fill = 'gray95'), 
        panel.grid.major.y = element_line(color = 'darkgrey', 
                                          linetype = 'dotted', size = 0.3), 
        panel.spacing = unit(1.25, 'lines'))

ggsave(filename = paste0(Sys.Date(),'_sex_decile_mult_repayment.png'))


# Same plot as above, but without disaggregating by gender

options(warn = 0)

# loop through and tally up payment proportions by sex and lifetime earnings decile
for(i in c(1:3)){
  df <- dfs[[i]] %>% filter(month == 1) %>% # just get one annual payment amount
    group_by(p_id) %>% summarise(life_pay = sum(annual_payment)) %>% 
    mutate(decile = cut(life_pay, include.lowest = T, 
                        breaks = quantile(life_pay, probs = seq(0, 1, 0.1)),
                        labels = seq(1, 10, 1), ordered_result = T), 
           decile = as.numeric(decile))
  decile_df <- df %>% group_by(decile) %>% summarise(sum_payments = sum(life_pay)) %>%
    mutate(prop = sum_payments / (2000 * 45000), # scale to sum of loans taken out by that decile
           s_name = names(dfs[i]))
  
  # make into single dataframe
  if(i == 1){
    decile_loan_scale <- decile_df
  }
  else{
    decile_loan_scale <- rbind(decile_loan_scale, decile_df)
  }
}

ggplot(decile_loan_scale) + 
  geom_bar(aes(x = decile, y = prop), stat = 'identity', position = 'dodge', fill = t_colors[2]) + 
  scale_y_continuous(breaks = seq(1, 7, 1)) +
  scale_x_continuous(breaks = seq(0, 10, 1)) +
  facet_grid(s_name ~ .,
             labeller = as_labeller(c('s_base' = 'Modified REPAYE', 's5' = 'Graduate Tax', 's7' = 'Capped Repayment'))) +
  labs(x = 'Decile of lifetime earnings', y = 'Multiple of loan balance paid') +
  thesis_theme +
  theme(strip.background = element_rect(fill = 'gray95'), 
        panel.grid.major.y = element_line(color = 'darkgrey', 
                                          linetype = 'dotted', size = 0.3), 
        panel.spacing = unit(1.25, 'lines'))

ggsave(filename = paste0(Sys.Date(),'_decile_mult_repayment.png'))
ggsave(paste0('/Users/noradelaney/Desktop/', Sys.Date(), '_decile_mult_repayment.png'))


options(warn = 0)
scenarios$s7 %>% filter(total_payment >= (45000 * 2)) %>% filter(month == 12 & age == max(scenarios$s7$age)) %>% nrow()
scenarios$s7 %>% filter(total_payment >= (45000 * 2)) %>% filter(month == 12 & age == max(scenarios$s7$age)) %>% group_by(sex) %>% summarise(n())

s7_lifetime_pay <- scenarios$s7 %>% filter(month == 12 & age == max(scenarios$s7$age))
median(s7_lifetime_pay$total_payment)










