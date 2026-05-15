#Bedre mulighet for norsk språk
Sys.setlocale(locale='no_NO')


# install.packages(c('readxl','dplyr','lubridate','janitor','ggplot2','fixest'))
library(readxl)
library(tidyverse)
library(tidyr)
library(dplyr)
library(lubridate)
library(janitor)
library(ggplot2)
library(modelsummary)
library(fixest)
library(MSwM)
library(gt)
library(writexl)
library(sjPlot)

MIDAS <- read_excel('C:/Users/Trygve-Tobias/OneDrive/Johansens master/MIDAScom.xlsx')

CRSP <- read_excel('C:/Users/Trygve-Tobias/OneDrive/Johansens master/CRSPcom.xlsx')

CRSP <- CRSP %>%
  rename(Date = `Names Date`,
         Ticker = `Ticker Symbol`)

MIDAS <- MIDAS %>%
  rename(Ticker = `ticker`)


#Fjerner NASDAQ number of trades for 8 av 10 selskaper gir ikke denne infoen(NA)
CRSP <- CRSP %>%
  select(-`NASDAQ Number of Trades`)

df_all <- left_join(CRSP, MIDAS,
                    by = c('Ticker', 'Date'))


df_all <- df_all %>%
  mutate(
    # 1. Quoted spread
    Quoted_spread = Ask - Bid,
    # 2. Relative spread (standard definisjon)
    Relative_spread = (Ask - Bid) / ((Ask + Bid) / 2),
    # 3. Amihud illiquidity (absolutt retur per volum)
    Amihud = abs(Returns) / Volume,
    absRet = abs(Returns)) #Lagt til for Markov-test

#Nytt datasett uten rådata
df_clean <- df_all %>%
  select(-`Sum of trade volume for trades that are not against hidden orders ('000)`,
         -`Sum of order volume for all add order messages ('000)`,
         -`Count of trades against hidden orders from exchanges that report trades against hidden orders`,
         -`Count of trades from exchanges that report trades against hidden orders.`,
         -`Sum of trade volume for trades against hidden orders from exchanges that report trades against hidden orders ('000)`,
         -`Sum of trade volume from exchanges that report trades against hidden orders ('000)`,
         -`Count of all cancel messages, either full or partial, for all exchanges.`,
         -`Count of all trade messages for trades that are not against hidden orders.`,
         -`Count of odd lot trade messages for all exchanges.`,
         -`Count of trades from exchanges that report individual trades.`,
         -`Sum of odd lot trade volume for all exchanges ('000)`,
         -`Sum of trade volume from exchanges that report individual trades ('000)`,
         -`Names Ending Date`, -`Record Date`, -`Shares Observation End Date`)


#Markov Swiching modell for å sjekke om våre valgte perioder er bra.#########################

regime_daily <- df_clean %>%
  group_by(Date) %>%
  summarise(
    mkt_absret   = mean(absRet, na.rm = TRUE),
    med_spread   = median(Relative_spread, na.rm = TRUE),
    med_amihud   = median(Amihud, na.rm = TRUE),
    n_stocks     = n(),
    .groups = 'drop') %>%
  arrange(Date)

#Forsøk 2 fortsettelse MS-modell
regime_daily2 <- regime_daily %>%
  mutate(
    log_mkt_absret = log(mkt_absret + 1e-8),
    lag_log_mkt_absret = lag(log_mkt_absret)) %>%
  filter(is.finite(log_mkt_absret),
         is.finite(lag_log_mkt_absret))


#Enkel modell med konstantledd
mod_absret2 <- lm(log_mkt_absret ~ lag_log_mkt_absret, data = regime_daily2)
summary(mod_absret2)

# 2-regime Markov Switching
ms_absret2 <- msmFit(mod_absret2, k = 2, p = 0, sw = c(TRUE, TRUE, TRUE))

summary(ms_absret2)

#Smoothere sannsynligheter(ChatGPT- anbefaling)
smo <- as.data.frame(ms_absret2@Fit@smoProb)

# smo har én rad mer, fjern første rad
if (nrow(smo) == nrow(regime_daily2) + 1) {
  smo <- smo[-1, ]}

# Gi sannsynlighetskolonnene navn
colnames(smo) <- c('Prob_Regime1', 'Prob_Regime2')

# Legg sannsynlighetene inn i datasettet
regime_daily2 <- bind_cols(regime_daily2, smo)

#Regime gjennomsnitt. (Regimet med høyest gjennomsnittlig mkt_absret kan regnes som stressregime)
regime_summary <- regime_daily2 %>%
  summarise(
    mean_absret_r1 = mean(mkt_absret[Prob_Regime1 > 0.5], na.rm = TRUE),
    mean_absret_r2 = mean(mkt_absret[Prob_Regime2 > 0.5], na.rm = TRUE),
    mean_logabsret_r1 = mean(log_mkt_absret[Prob_Regime1 > 0.5], na.rm = TRUE),
    mean_logabsret_r2 = mean(log_mkt_absret[Prob_Regime2 > 0.5], na.rm = TRUE))

stress_regime <- ifelse(regime_summary$mean_absret_r1 > regime_summary$mean_absret_r2, 1, 2)

stress_regime <- ifelse(
  regime_summary$mean_absret_r1 > regime_summary$mean_absret_r2,
  1, 2)

if (stress_regime == 1) 
{regime_daily2$Prob_Stress <- regime_daily2$Prob_Regime1} else 
    {regime_daily2$Prob_Stress <- regime_daily2$Prob_Regime2}


#Sjekker våre forhåndsvalgte perioder
regime_daily2 <- regime_daily2 %>%
  mutate(
    chosen_period = case_when(
      Date >= as.Date('2020-02-20') & Date <= as.Date('2020-05-15') ~ 'Stress_COVID',
      Date >= as.Date('2022-04-01') & Date <= as.Date('2022-06-30') ~ 'Stress_2022',
      Date >= as.Date('2019-06-01') & Date <= as.Date('2019-08-31') ~ 'Calm_2019',
      Date >= as.Date('2021-06-01') & Date <= as.Date('2021-08-31') ~ 'Calm_2021',
      TRUE ~ NA_character_))

#Hvor stressete periodene faktisk er
period_check2 <- regime_daily2 %>%
  filter(!is.na(chosen_period)) %>%
  group_by(chosen_period) %>%
  summarise(
    days = n(),
    mean_prob_stress   = mean(Prob_Stress, na.rm = TRUE),
    median_prob_stress = median(Prob_Stress, na.rm = TRUE),
    min_prob_stress    = min(Prob_Stress, na.rm = TRUE),
    max_prob_stress    = max(Prob_Stress, na.rm = TRUE),
    .groups = 'drop')

period_check2

#Fin GT-table til resultat-delen for 'backing' av regimer
gt_period <- period_check2 %>%
  gt(rowname_col = 'chosen_period') %>%
  tab_header(
    title = 'Validation of Selected Market Regimes',
    subtitle = 'Markov Switching model (2 regimes)') %>%
  fmt_number(
    columns = c(mean_prob_stress, median_prob_stress, min_prob_stress, max_prob_stress),
    decimals = 3) %>%
  cols_label(
    days = 'Trading days',
    mean_prob_stress = 'Mean',
    median_prob_stress = 'Median',
    min_prob_stress = 'Min',
    max_prob_stress = 'Max') %>%
  cols_align(align = 'center', -chosen_period) %>%
  opt_table_outline()

gt_period


#Visuell fremstilling
ggplot(regime_daily2, aes(x = Date, y = Prob_Stress)) +
  annotate('rect',
           xmin = as.Date('2019-06-01'), xmax = as.Date('2019-08-31'),
           ymin = -Inf, ymax = Inf, fill='green', alpha = 0.15) +
  annotate('rect',
           xmin = as.Date('2021-06-01'), xmax = as.Date('2021-08-31'),
           ymin = -Inf, ymax = Inf, fill='green', alpha = 0.15) +
  annotate('rect',
           xmin = as.Date('2020-02-20'), xmax = as.Date('2020-05-15'),
           ymin = -Inf, ymax = Inf, fill='red', alpha = 0.15) +
  annotate('rect',
           xmin = as.Date('2022-04-01'), xmax = as.Date('2022-06-30'),
           ymin = -Inf, ymax = Inf, fill='red', alpha = 0.15) +
  geom_line() +
  geom_hline(yintercept = 0.5, linetype = 'dashed') +
  labs(
    title = 'Smoothed probability of stress regime',
    x = 'Date',
    y = 'Probability of stress regime'
  ) +
  theme_bw()

#Forsøk 2 slutt ###############################################################################

# 1.4.22 - 30.6.22 - 62  ____ + COVID: 20.2.20 - 15.5.20

df_stress22 <- df_clean %>%
  filter(Date >= as.Date('2022-04-01'),
         Date <= as.Date('2022-06-30'))

df_stress_covid <- df_clean %>%
  filter(Date >= as.Date('2020-02-20'),
         Date <= as.Date('2020-05-15'))

#Stressperiodene slått sammen
df_stress <- bind_rows(df_stress22, df_stress_covid) %>%
  mutate(Stress = 1)

#Rolig periode
# 1.6.21 - 31.8.21 - 64 Handelsdager ___ + før covid: 1.6.19-31.8.19

df_calm21 <- df_clean %>%
  filter(Date >= as.Date('2021-06-01'),
         Date <= as.Date('2021-08-31'))

df_calm_19 <- df_clean %>%
  filter(Date >= as.Date('2019-06-01'),
         Date <= as.Date('2019-08-31'))

#Rolig-periodene slått sammen
df_calm <- bind_rows(df_calm21, df_calm_19) %>%
  mutate(Stress = 0)



#Til Fredrik for å se i excel##################
#install.packages('writexl')
write_xlsx(df_stress, 'df_stress.xlsx')
write_xlsx(df_calm, 'df_calm.xlsx')
getwd()
##############################################

# GT-tabell for deskriptiv statistikk for hver aksje i ulike regimer og tot. gj.snitt
##Relative spread & Amihud

#Likviditet
#
# - Stress = 1
# - Rolig = 0
df_periods <- bind_rows(df_stress,df_calm)

desk_stock <- df_periods %>%
  group_by(Stress, Ticker) %>%
  summarise(
    relspread_mean = mean(Relative_spread, na.rm = TRUE),
    relspread_sd   = sd(Relative_spread, na.rm = TRUE),
    Quoted_spread_mean = mean(Quoted_spread, na.rm = TRUE),
    Quoted_spread_sd   = sd(Quoted_spread, na.rm = TRUE),
    ami_mean = mean(Amihud, na.rm = TRUE),
    ami_sd   = sd(Amihud, na.rm = TRUE),
    .groups = 'drop')


desk_total <- df_periods %>%
  group_by(Stress) %>%
  summarise(
    relspread_mean = mean(Relative_spread, na.rm = TRUE),
    relspread_sd   = sd(Relative_spread, na.rm = TRUE),
    Quoted_spread_mean = mean(Quoted_spread, na.rm = TRUE),
    Quoted_spread_sd   = sd(Quoted_spread, na.rm = TRUE),
    ami_mean = mean(Amihud, na.rm = TRUE),
    ami_sd   = sd(Amihud, na.rm = TRUE),
    .groups = 'drop')

desk_total <- desk_total %>%
  mutate(Ticker='TOTAL') %>%
  select(Stress, Ticker, everything())

desk <- bind_rows(
  desk_stock %>%
    select(Stress, Ticker, everything()),
  desk_total) %>%
  mutate(Period = factor(Stress, levels = c(0, 1), labels = c('Calm', 'Stress')),
         is_total = ifelse(Ticker == 'TOTAL', 1,0)) %>%
  arrange(is_total, Ticker, Period)%>%
  select(-is_total, -Stress) %>%
  relocate(Period, .after = Ticker)   # <- flytt Period rett etter Ticker

#GT-table
gt_desk <- desk %>%
  gt(rowname_col = 'Ticker') %>%
  tab_header(
    title = 'Descriptive Statistics for liquidity',
    subtitle = 'Mean and standard deviation per period') %>%
  #for å få finere tall med fem desimaler
  fmt_number(
      columns = -c(ami_mean, ami_sd), 
      decimals = 5) %>%
  #så små tall at må bruke vitenskapelig notasjon
  fmt_scientific(
    columns = c(ami_mean, ami_sd)) %>%
  cols_label(relspread_mean = 'Relative Spread mean',relspread_sd = 'Relative Spread sd',
    Quoted_spread_mean = 'Quoted Spread mean', Quoted_spread_sd = 'Quoted Spread sd',
    ami_mean = 'Amihud mean', ami_sd = 'Amihud sd') %>%
  #    cancels_mean = 'Cancels mean',  cancels_min = 'Cancels min',  cancels_max = 'Cancels max',  cancels_sd = 'Cancels sd'
  opt_table_outline() %>%
  tab_style(
    style = cell_borders(sides = 'right', color = 'grey80', weight = px(2)),
    locations = cells_body(columns = everything()) )

gt_desk


#Deskriptiv statistikk for hver aksje i ulike regimer og tot. gj.snitt
##Cancels, Decile for turnover,	Decile for volatility,	Decile for price,	100*LitVol/OrderVol,	100*Hidden/TradesForHidden,	100*HiddenVol/TradeVolForHidden,	100*OddLots/TradesForOddLots,	100*OddLotVol/TradeVolForOddLots

#Algorime -------------- 2=algo

vars_extra <- c(
  'Decile for turnover',
  'Decile for volatility',
  '100*LitVol/OrderVol',
  '100*Hidden/TradesForHidden',
  '100*OddLots/TradesForOddLots',
  'Cancels/LitTrades')

desk2_stock <- df_periods %>%
  group_by(Stress, Ticker) %>%
  summarise(across(all_of(vars_extra),
                   list(
                     mean = ~mean(.x, na.rm = TRUE),
                     sd   = ~sd(.x,  na.rm = TRUE)),
                   .names = '{.col}_{.fn}'),.groups = 'drop')

desk2_total <- df_periods %>%
  group_by(Stress) %>%
  summarise(
    across(
      all_of(vars_extra),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        sd   = ~sd(.x,  na.rm = TRUE)),
      .names = '{.col}_{.fn}'), .groups = 'drop') %>%
  mutate(Ticker = 'TOTAL') %>%
  select(Stress, Ticker, everything())

desk2 <- bind_rows(
  desk2_stock %>% select(Stress, Ticker, everything()),
  desk2_total) %>%
  mutate(
    Period = factor(Stress, levels = c(0, 1), labels = c('Calm', 'Stress')),
    is_total = ifelse(Ticker == 'TOTAL', 1, 0)) %>%
  arrange(is_total, Ticker, Stress) %>%
  select(-is_total, -Stress)%>%
  relocate(Period, .after = Ticker)   # <- flytt Period rett etter Ticker

gt_desk2 <- desk2 %>%
  gt(rowname_col = 'Ticker') %>%
  fmt_number(decimals=3)%>%
  tab_header(
    title = 'Descriptive Statistics for Algorithmic Trading',
    subtitle = 'Mean and standard deviation for each period') %>%
  cols_label('Decile for turnover_mean' = 'Turnover mean',
             'Decile for turnover_sd' = 'Turnover SD',
             'Decile for volatility_mean' = 'Volatility mean',
             'Decile for volatility_sd' = 'Volatility SD',
             '100*LitVol/OrderVol_mean' = 'Order to trade mean',
             '100*LitVol/OrderVol_sd' = 'Order to trade SD',
             '100*Hidden/TradesForHidden_mean' = 'Hidden trades mean',
             '100*Hidden/TradesForHidden_sd' = 'Hidden trades SD',
             '100*OddLots/TradesForOddLots_mean' = 'Odd lots mean',
             '100*OddLots/TradesForOddLots_sd' = 'Odd lots SD',
             'Cancels/LitTrades_mean' = 'Cancels mean',
             'Cancels/LitTrades_sd' = 'Cancels SD') %>%
  opt_table_outline() %>%
  tab_style(style = cell_borders(sides = 'right', color = 'grey80', weight = px(2)),
    locations = cells_body(columns = everything()))

gt_desk2

gtsave(gt_desk2, 'gt_tablealgoen.html')
gtsave(gt_desk, 'gt_tableliquen.html')

#Deler opp gt_desk2 til to tabeller for lettere leselighet.

desk2a <- desk2 %>%
  select(Ticker, Period,
         'Decile for turnover_mean', 'Decile for turnover_sd',
         'Decile for volatility_mean', 'Decile for volatility_sd',
         '100*LitVol/OrderVol_mean', '100*LitVol/OrderVol_sd')

desk2b <- desk2 %>%
  select(Ticker, Period,
         '100*Hidden/TradesForHidden_mean', '100*Hidden/TradesForHidden_sd',
         '100*OddLots/TradesForOddLots_mean', '100*OddLots/TradesForOddLots_sd',
         'Cancels/LitTrades_mean', 'Cancels/LitTrades_sd')

gt_desk2a <- desk2a %>%
  gt(rowname_col = 'Ticker') %>%
  fmt_number(decimals = 3) %>%
  tab_header(title = 'Descriptive Statistics for Trading Activity',
             subtitle = 'Mean and standard deviation for each period') %>%
  cols_label('Decile for turnover_mean' = 'Turnover mean',
             'Decile for turnover_sd' = 'Turnover SD',
             'Decile for volatility_mean' = 'Volatility mean',
             'Decile for volatility_sd' = 'Volatility SD',
             '100*LitVol/OrderVol_mean' = 'Order to trade mean',
             '100*LitVol/OrderVol_sd' = 'Order to trade SD') %>%
  opt_table_outline() %>%
  tab_style(style = cell_borders(sides = 'right', color = 'grey80', weight = px(2)),
            locations = cells_body(columns = everything()))
gt_desk2a



gt_desk2b <- desk2b %>%
  gt(rowname_col = 'Ticker') %>%
  fmt_number(decimals = 3) %>%
  tab_header(title = 'Descriptive Statistics for Trading Activity',
             subtitle = 'Mean and standard deviation for each period') %>%
  cols_label('100*Hidden/TradesForHidden_mean' = 'Hidden trades mean',
             '100*Hidden/TradesForHidden_sd' = 'Hidden trades SD',
             '100*OddLots/TradesForOddLots_mean' = 'Odd lots mean',
             '100*OddLots/TradesForOddLots_sd' = 'Odd lots SD',
             'Cancels/LitTrades_mean' = 'Cancels mean',
             'Cancels/LitTrades_sd' = 'Cancels SD') %>%
  opt_table_outline() %>%
  tab_style(style = cell_borders(sides = 'right', color = 'grey80', weight = px(2)),
            locations = cells_body(columns = everything()))
gt_desk2b

gtsave(gt_desk2a, 'gt_algo2a.html')
gtsave(gt_desk2b, 'gt_algo2b.html')


#Hypoteser

#Proxyer
df_hyp <- df_periods %>%
  mutate(Algo_C =as.numeric(scale('Cancels/LitTrades')),                 # proxy 1 (kanselleringsrate)
         Algo_O = as.numeric(scale('100*OddLots/TradesForOddLots')), #proxy 2
         logAmihud = log(Amihud + 1e-12),
         logRelSpread = log(Relative_spread + 1e-12),
         logQuoSpread = log(Quoted_spread + 1e-12),
         VolDec = `Decile for volatility`,
         TurnDec = `Decile for turnover`,
         PriceDec = `Decile for price`,
         logVol = log(Volume+1),
         absRet = abs(Returns))    # proxy 2 (odd-lot-rate)

df_hyp <- na.omit(df_hyp)


# Sjekk at disse finnes (for å unngå silent feil)
stopifnot(all(c('Ticker','Date','Stress','Algo_C','Algo_O',
                'logQuoSpread','logRelSpread','logAmihud') %in% names(df_hyp)))


# Lag en oversiktlig tabell med alle variablene som er tatt med i analysen,
#FJERN PCTL og variabler som ikke brukes
library(vtable)

sumstat <- df_hyp %>%
  rename(HiddenTrades= `100*Hidden/TradesForHidden`) %>%
  select(logRelSpread,logAmihud,HiddenTrades, Algo_C, Algo_O, 
         logVol, absRet, VolDec, TurnDec, PriceDec)

sum_table <- st(sumstat)


#Testing av hypoteser
#feols = fixed effect OLS

#Låser standarder______________________________________________________________________________________
# Standard kontrollsett (samme overalt)
ctrls <- c('logVol', 'absRet', 'VolDec', 'TurnDec', 'PriceDec')

# Helper: bygg formeltekst automatisk
rhs_base  <- paste(c('Algo_C*Stress', ctrls), collapse = ' + ')
rhs_calm  <- paste(c('Algo_C', ctrls), collapse = ' + ')

# Fixed effects og klustring
fe_part <- 'Ticker + Date'
clust   <- ~ Ticker


#for å slippe repeterende kode___________________________________________

# Marginaleffekt i stress + SE(b1+b3)
stress_marginal <- function(model){
  b <- coef(model)
  V <- vcov(model)
  
  beta <- b['Algo_C'] + b['Algo_C:Stress']
  se   <- sqrt(V['Algo_C','Algo_C'] +
                 V['Algo_C:Stress','Algo_C:Stress'] +
                 2*V['Algo_C','Algo_C:Stress'])
  c(beta_stress = beta, se_stress = se, t = beta/se)}

# FJERN! FUNKER IKKE. test_beta3 <- function(model){
  # Tester H0: Algo_C:Stress = 0
  #wald(model, 'Algo_C:Stress = 0')}


#H2 i rolig regime: test β1 < 0
test_beta1_sign <- function(model){
  b <- coef(model)['Algo_C']
  se <- sqrt(vcov(model)['Algo_C','Algo_C'])
  t  <- b / se
  # En-sidig p for H2: beta < 0
  p_one_sided <- pnorm(t, lower.tail = TRUE)
  
  c(beta = b, se = se, t = t, p_one_sided = p_one_sided)}
#_____________________________________________________________________________

# H1 (hoved): Regimeavhengig effekt på spread

m_H1_relspread <- feols(
  as.formula(paste0('logRelSpread ~ ', rhs_base, ' | ', fe_part)),
  cluster = clust,
  data = df_hyp)

summary(m_H1_relspread)

tab_h1 <- as.data.frame(etable(m_H1_relspread,
  se = 'cluster',
  fitstat = ~ n + r2 + ar2 + wr2 + F, 
  digits = 3,
  tex =FALSE,
  dict = c(
    logRelSpread = 'Log(Relative spread)',
    logVol = 'Log(Volume)',
    Algo_C = 'Cancellations (std.)',
    Stress = 'Stress period',
    `Algo_C:Stress` = 'Cancellations × Stress')))
write_xlsx(tab_h1, 'H1_table.xlsx')


# Stress-dummyen er konstant innen dato og absorberes av dato-faste effekter. 
# Vi identifiserer derfor regimeforskjeller gjennom interaksjonsleddet mellom 
# algoritmisk handelsintensitet og stress, og rapporterer både effekten i rolige perioder (β₁) og i stressperioder (β₁+β₃).

# H1-test: regimeavhengighet (β3 != 0)
ct <- summary(m_H1_relspread)$coeftable
ct['Algo_C:Stress', c('Estimate','Std. Error','t value','Pr(>|t|)')]

# Rapporter effekt i stress: β1+β3

stress_marginal(m_H1_relspread)

# H2 Effekt i rolig regime (β1)_______________________________
tab2<-as.data.frame(test_beta1_sign(m_H1_relspread))


#H3: Price impact i stress (Amihud) – test__________________________

m_H3_amihud <- feols(
  as.formula(paste0('logAmihud ~ ', rhs_base, ' | ', fe_part)),
  cluster = clust,
  data = df_hyp)

summary(m_H3_amihud)

tab_h3 <- as.data.frame(etable(m_H3_amihud,
                               se = 'cluster',
                               fitstat = ~ n + r2 + ar2 + wr2 + F, 
                               digits = 3,
                               tex =FALSE,
                               dict = c(
                                 logAmihud = 'Log(Amihud Illiquidity)',
                                 logVol = 'Log(Volume)',
                                 Algo_C = 'Cancellations (std.)',
                                 Stress = 'Stress period',
                                 `Algo_C:Stress` = 'Cancellations × Stress')))

write_xlsx(tab_h3, 'H3_table.xlsx')

# Regimeendring i sammenheng (β3 != 0) - valgfritt
#test_beta3(m_H3_amihud) funker ikke

# H3 hovedtest: stress-effekt = β1+β3 (ønsket positiv)
stress_marginal(m_H3_amihud)

#__________________________________________________________________

#Mekanisme M1 Hiddenshare
m_M1_hidden_clean <- feols(
  as.formula(paste0('`100*Hidden/TradesForHidden` ~ Algo_C + ',
                    paste(ctrls, collapse=' + '),
                    ' | ', fe_part)),
  cluster = clust,
  data = df_hyp)

summary(m_M1_hidden_clean)
test_beta1_sign(m_M1_hidden_clean)  # forventet > 0

tab_m1 <- as.data.frame(etable(m_M1_hidden_clean,
                               se = 'cluster',
                               fitstat = ~ n + r2 + ar2 + wr2 + F, 
                               digits = 3,
                               tex =FALSE,
                               dict = c(
                                 `100*Hidden/TradesForHidden`= 'Hidden trades',
                                 logVol = 'Log(Volume)',
                                 Algo_C = 'Cancellations (std.)',
                                 Stress = 'Stress period',
                                 `Algo_C:Stress` = 'Cancellations × Stress')))

write_xlsx(tab_m1, 'm1_table.xlsx')

#Med regime
m_M1_hidden_regime <- feols(
  as.formula(paste0('`100*Hidden/TradesForHidden` ~ ', rhs_base, ' | ', fe_part)),
  cluster = clust,
  data = df_hyp)

summary(m_M1_hidden_regime)

tab_m2 <- as.data.frame(etable(m_M1_hidden_regime,
                               se = 'cluster',
                               fitstat = ~ n + r2 + ar2 + wr2 + F, 
                               digits = 3,
                               tex =FALSE,
                               dict = c(
                                 `100*Hidden/TradesForHidden`= 'Hidden trades',
                                 logVol = 'Log(Volume)',
                                 Algo_C = 'Cancellations (std.)',
                                 Stress = 'Stress period',
                                 `Algo_C:Stress` = 'Cancellations × Stress')))

write_xlsx(tab_m2, 'm2_table.xlsx')

# Mekanisme sterkere i stress (b3 > 0)

# Nivåeffekt i stress: b1+b3
stress_marginal(m_M1_hidden_regime)


#Robusthet (Algo_O)___________________________________________________________

# Robusthet: bytt Algo_C → Algo_O
rhs_base_O <- paste(c('Algo_O*Stress', ctrls), collapse = ' + ')
rhs_calm_O <- paste(c('Algo_O', ctrls), collapse = ' + ')

stress_marginal_O <- function(model){
  b <- coef(model)
  V <- vcov(model)
  
  beta <- b['Algo_O'] + b['Algo_O:Stress']
  se   <- sqrt(V['Algo_O','Algo_O'] +
                 V['Algo_O:Stress','Algo_O:Stress'] +
                 2*V['Algo_O','Algo_O:Stress'])
  
  c(beta_stress = beta, se_stress = se, t = beta/se)}

test_beta1_sign_O <- function(model){
  b <- coef(model)['Algo_O']
  se <- sqrt(vcov(model)['Algo_O','Algo_O'])
  t  <- b / se
  p_one_sided <- pnorm(t, lower.tail = TRUE)  # H_A: beta < 0
  c(beta = b, se = se, t = t, p_one_sided = p_one_sided)}

#H1/H2
m_H1_relspread_O <- feols(
  as.formula(paste0('logRelSpread ~ ', rhs_base_O, ' | ', fe_part)),
  cluster = clust,
  data = df_hyp)

summary(m_H1_relspread_O)

tabo_h1 <- as.data.frame(etable(m_H1_relspread_O,
                               se = 'cluster',
                               fitstat = ~ n + r2 + ar2 + wr2 + F, 
                               digits = 3,
                               tex =FALSE,
                               dict = c(
                                 logRelSpread = 'Log(Relative spread)',
                                 logVol = 'Log(Volume)',
                                 Algo_O = 'Odd lots (std.)',
                                 Stress = 'Stress period',
                                 `Algo_O:Stress` = 'Odd lots × Stress')))
write_xlsx(tabo_h1, 'H1o_table.xlsx')

# β3 (regime) i O-modell
ctO <- summary(m_H1_relspread_O)$coeftable
ctO['Algo_O:Stress', c('Estimate','Std. Error','t value','Pr(>|t|)')]

# Effekt i stress: β1+β3
stress_marginal_O(m_H1_relspread_O)

# Effekt i rolig: β1 (ensidig p for <0)
test_beta1_sign_O(m_H1_relspread_O)


#H3

m_H3_amihud_O <- feols(
  as.formula(paste0('logAmihud ~ ', rhs_base_O, ' | ', fe_part)),
  cluster = clust,
  data = df_hyp)

summary(m_H3_amihud_O)

tabo_h3 <- as.data.frame(etable(m_H3_amihud_O,
                               se = 'cluster',
                               fitstat = ~ n + r2 + ar2 + wr2 + F, 
                               digits = 3,
                               tex =FALSE,
                               dict = c(
                                 logAmihud = 'Log(Amihud Illiquidity)',
                                 logVol = 'Log(Volume)',
                                 Algo_O = 'Odd lots (std.)',
                                 Stress = 'Stress period',
                                 `Algo_O:Stress` = 'Odd lots × Stress')))

write_xlsx(tabo_h3, 'H3o_table.xlsx')

# Effekt i stress: β1+β3
stress_marginal_O(m_H3_amihud_O)

m_M1_hidden_regime_O <- feols(
  as.formula(paste0('`100*Hidden/TradesForHidden` ~ ', rhs_base_O, ' | ', fe_part)),
  cluster = clust,
  data = df_hyp)

summary(m_M1_hidden_regime_O)

# Effekt i stress: β1+β3
stress_marginal_O(m_M1_hidden_regime_O)

tab_mo2 <- as.data.frame(etable(m_M1_hidden_regime_O,
                               se = 'cluster',
                               fitstat = ~ n + r2 + ar2 + wr2 + F, 
                               digits = 3,
                               tex =FALSE,
                               dict = c(
                                 `100*Hidden/TradesForHidden`= 'Hidden trades',
                                 logVol = 'Log(Volume)',
                                 Algo_C = 'Cancellations (std.)',
                                 Stress = 'Stress period',
                                 `Algo_C:Stress` = 'Cancellations × Stress')))

write_xlsx(tab_mo2, 'm02_table.xlsx')


#Tabell med reslutater samlet
tab_all <- as.data.frame(etable(m_H1_relspread,       m_H1_relspread_O,
  m_H3_amihud,          m_H3_amihud_O,
  m_M1_hidden_regime,   m_M1_hidden_regime_O,
  fitstat = ~ n + r2 + ar2 + wr2 + F,
  dict = c(
    'Algo_C' = 'Algo (C)',
    'Algo_O' = 'Algo (O)',
    'Algo_C:Stress' = 'Algo (C) × Stress',
    'Algo_O:Stress' = 'Algo (O) × Stress',
    'logVol' = 'log(Volum)',
    'absRet' = '|Ret|',
    'VolDec' = 'VolDec',
    'TurnDec' = 'TurnDec',
    'PriceDec' = 'PriceDec')))

write_xlsx(tab_all, 'all_table.xlsx')



#Bedre tabell? Brukt excel til nå for lettest


# Koeffisient-figur 

# Funksjon som henter ut effekt i rolige perioder og total effekt i stressperioder
extract_effects <- function(model, proxy_var, proxy_label, outcome_label) {
  b <- coef(model)
  V <- vcov(model)
  
  inter_name <- paste0(proxy_var, ':Stress')
  
  # Effekt i rolige perioder
  beta_calm <- b[proxy_var]
  se_calm   <- sqrt(V[proxy_var, proxy_var])
  
  # Effekt i stressperioder = beta_algo + beta_interaksjon
  beta_stress <- b[proxy_var] + b[inter_name]
  se_stress   <- sqrt(
    V[proxy_var, proxy_var] +
      V[inter_name, inter_name] +
      2 * V[proxy_var, inter_name])
  
  tibble(
    Outcome = outcome_label,
    Proxy   = proxy_label,
    Regime  = c('Calm', 'Stress'),
    Estimate = c(beta_calm, beta_stress),
    SE       = c(se_calm, se_stress)) %>%
    mutate(CI_low  = Estimate - 1.96 * SE,
           CI_high = Estimate + 1.96 * SE)}

# Bygg samlet figurdata fra modellene deres
coef_plot_data <- bind_rows(
  extract_effects(m_H1_relspread,       'Algo_C', 'Algo_C', 'logRelSpread'),
  extract_effects(m_H1_relspread_O,     'Algo_O', 'Algo_O', 'logRelSpread'),
  extract_effects(m_H3_amihud,          'Algo_C', 'Algo_C', 'logAmihud'),
  extract_effects(m_H3_amihud_O,        'Algo_O', 'Algo_O', 'logAmihud'),
  extract_effects(m_M1_hidden_regime,   'Algo_C', 'Algo_C', 'HiddenTrades'),
  extract_effects(m_M1_hidden_regime_O, 'Algo_O', 'Algo_O', 'HiddenTrades'))

# Ryddigere etiketter til figuren
coef_plot_data <- coef_plot_data %>%
  mutate(Outcome = factor(Outcome,
                          levels = c('logRelSpread', 'logAmihud', 'HiddenTrades'),
                          labels = c('Log(Relative spread)', 'Log(Amihud)', 'Hidden trades')),
         Proxy = factor(Proxy,
                        levels = c('Algo_C', 'Algo_O'),
                        labels = c('Cancellations (Algo_C)', 'Odd lots (Algo_O)')))

ggplot(coef_plot_data,
       aes(x = Estimate, y = Proxy, xmin = CI_low, xmax = CI_high, shape = Regime)) +
  geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_errorbarh(height = 0.18,
                 position = position_dodge(width = 0.5)) +
  geom_point(size = 2.8,
             position = position_dodge(width = 0.5)) +
  facet_wrap(~ Outcome, scales = 'free_x') + #For ulik skala (best for hver) for å se ting tydeligere
  labs(
    title = 'Estimated effects of algorithmic trading on liquidity',
    subtitle = 'Calm-period effect and total stress-period effect with 95% confidence intervals',
    x = 'Coefficient estimate',
    y = NULL,
    shape = 'Market regime') +
  theme_bw() +
  theme(legend.position = 'bottom')



#Tabell for å enklere se, til metode.
model_overview <- tibble(
  Modell = c('1', '2', '3', '4', '5', '6'),
  `Avhengig variabel` = c('log(Relative spread)',
                          'log(Relative spread)',
                          'log(Amihud)',
                          'log(Amihud)',
                          'Hidden trades',
                          'Hidden trades'),
  `Proxy for algoritmisk handel` = c('Algo_C',
                                     'Algo_O',
                                     'Algo_C',
                                     'Algo_O',
                                     'Algo_C',
                                     'Algo_O'),
  Formål = c('Kanselleringsrate og relativ spread',
             'Robusthetstest med odd-lot-proxy',
             'Kanselleringsrate og prispåvirkning',
             'Robusthetstest med odd-lot-proxy',
             'Kanselleringsrate og skjult likviditet',
             'Robusthetstest med odd-lot-proxy'))

gt_model_overview <- model_overview %>%
  gt() %>%
  tab_header(title = 'Oversikt over estimerte modeller',
             subtitle = 'Likviditetsmål og proxyer for algoritmisk handelsaktivitet') %>%
  cols_align(align = 'center',
             columns = c(Modell, `Proxy for algoritmisk handel`)) %>%
  cols_width(Modell ~ px(70),
             `Avhengig variabel` ~ px(220),
             `Proxy for algoritmisk handel` ~ px(220),
             Formål ~ px(320)) %>%
  opt_table_outline()

gt_model_overview

