### Load Librarires ###
library(pdftools)
library(tidyverse)
library('openxlsx')


### Set Working Directory ###
# setwd('C:/Users/Jordan/Documents/Reason/Pension/Idaho PERSI/Winklevoss')


### Load Data ###

pdf <- "https://www.soa.org/globalassets/assets/files/research/exp-study/rp00_mortalitytables.pdf"



### Page 43, Male Table 1 ###

p43 <- pdf_text(pdf)[[43]]

age <- p43[8:47, "text"]

emp <- p43[48:87, "text"]

ha <- rep(NA, 40)

ch <- p43[92:131, "text"]

na20 <- rep(NA, 20)
na20 <- as.data.frame(na20)
names(na20) <- "text"
dis <- rbind(na20, p43[134:153, "text"])

t43 <- cbind(age, emp, ha, ch, dis)
names(t43) <- c("Age", "Employees", "Healthy Annuitant", "Combined Healthy", "Disabled Retiree")


### Page 44, Male Table 2 ###

p44 <- pdf_data(pdf)[[44]]

age1 <- p44[8:50, "text"]

emp1 <- p44[51:80, "text"]

na13 <- rep(NA, 13)
na13 <- as.data.frame(na13)
names(na13) <- "text"

emp1 <- rbind(emp1, na13)

ha1 <- p44[83:116, "text"]

na9 <- rep(NA, 9)
na9 <- as.data.frame(na9)
names(na9) <- "text"
ha1 <- rbind(na9, ha1)

ch1 <- p44[119:161, "text"]

dis1 <- p44[164:206, "text"]

t44 <- cbind(age1, emp1, ha1, ch1, dis1)
names(t44) <- c("Age", "Employees", "Healthy Annuitant", "Combined Healthy", "Disabled Retiree")


### Page 45, Male Table 3 ###

p45 <- pdf_data(pdf)[[45]]

age2 <- p45[7:43, "text"]

emp2 <- as.data.frame(rep(NA, 37))
names(emp2) <- "text"

ha2 <- p45[47:83, "text"]

ch2 <- p45[86:122, "text"]

dis2 <- p45[125:161, "text"]

t45 <- cbind(age2, emp2, ha2, ch2, dis2)
names(t45) <- c("Age", "Employees", "Healthy Annuitant", "Combined Healthy", "Disabled Retiree")


### Merging Male Tables ###

male_rp_2000 <- rbind(t43, t44, t45)


### Page 46, Female Table 1 ###

p46 <- pdf_data(pdf)[[46]]

age <- p46[8:47, "text"]

emp <- p46[48:87, "text"]

ha <- rep(NA, 40)

ch <- p46[92:131, "text"]

na20 <- rep(NA, 20)
na20 <- as.data.frame(na20)
names(na20) <- "text"
dis <- rbind(na20, p46[134:153, "text"])

t46 <- cbind(age, emp, ha, ch, dis)
names(t46) <- c("Age", "Employees", "Healthy Annuitant", "Combined Healthy", "Disabled Retiree")


### Page 47, Female Table 2 ###

p47 <- pdf_data(pdf)[[47]]

age1 <- p47[8:49, "text"]

emp1 <- p47[50:79, "text"]
na13 <- rep(NA, 12)
na13 <- as.data.frame(na13)
names(na13) <- "text"
emp1 <- rbind(emp1, na13)

ha1 <- p47[82:114, "text"]

na9 <- rep(NA, 9)
na9 <- as.data.frame(na9)
names(na9) <- "text"
ha1 <- rbind(na9, ha1)

ch1 <- p47[117:158, "text"]

dis1 <- p47[161:202, "text"]

t47 <- cbind(age1, emp1, ha1, ch1, dis1)
names(t47) <- c("Age", "Employees", "Healthy Annuitant", "Combined Healthy", "Disabled Retiree")


### Page 48, Male Table 3 ###

p48 <- pdf_data(pdf)[[48]]

age2 <- p48[7:44, "text"]

emp2 <- as.data.frame(rep(NA, 38))
names(emp2) <- "text"

ha2 <- p48[48:85, "text"]

ch2 <- p48[88:125, "text"]

dis2 <- p48[128:165, "text"]

t48 <- cbind(age2, emp2, ha2, ch2, dis2)
names(t48) <- c("Age", "Employees", "Healthy Annuitant", "Combined Healthy", "Disabled Retiree")


### Merging Female Tables ###

female_rp_2000 <- rbind(t46, t47, t48)


### Switching Column Values to Numeric ###

male_rp_2000 <- male_rp_2000 %>% 
  mutate(Age = as.numeric(Age)) %>%
  mutate(Employees = as.numeric(Employees)) %>%
  mutate(`Healthy Annuitant` = as.numeric(`Healthy Annuitant`)) %>%
  mutate(`Combined Healthy` = as.numeric(`Combined Healthy`)) %>%
  mutate(`Disabled Retiree` = as.numeric(`Disabled Retiree`))


female_rp_2000 <- female_rp_2000 %>% 
  mutate(Age = as.numeric(Age)) %>%
  mutate(Employees = as.numeric(Employees)) %>%
  mutate(`Healthy Annuitant` = as.numeric(`Healthy Annuitant`)) %>%
  mutate(`Combined Healthy` = as.numeric(`Combined Healthy`)) %>%
  mutate(`Disabled Retiree` = as.numeric(`Disabled Retiree`))


### Writign XLSX Files ###

write.xlsx(male_rp_2000, file = "male_mortality_tables.xlsx", sheetName="Male")
write.xlsx(female_rp_2000, file = "female_mortality_tables.xlsx", sheetName="Female")


### Linear Interpolation ###

##### Service Retirement #####

age_t <- c(55, 60, 65, 70)
age_t <- approx(age_t, n = 16)$y

fye_m <- c(0.22, 0.26, 0.33, 0.18)
fye_m <- approx(fye_m, n = 16)$y

ta_m <- c(0.1, 0.17, 0.5, 0.2)
ta_m <- approx(ta_m, n = 16)$y

fye_f <- c(0.26, 0.26, 0.37, 0.18)
fye_f <- approx(fye_f, n = 16)$y


ta_f <- c(0.18, 0.18, 0.52, 0.21)
ta_f <- approx(ta_f, n = 16)$y



serv_ret <- cbind(age_t, fye_m, ta_m, fye_f, ta_f)
serv_ret <- as.data.frame(serv_ret)
names(serv_ret) <- c("Age", "Male - First Year Eligible", "Male - There after",
                     "Female - First Year Eligible", "Female - There after")


write.xlsx(serv_ret, file = "service_retirement.xlsx", sheetName = "Service Retirement")



##### Early Retirement #####

age_e <- c(55, 60)
age_e <- approx(age_e, n = 6)$y

ret_m <- c(0.03, 0.05)
ret_m <- approx(ret_m, n = 6)$y

ret_f <- c(0.03, 0.06)
ret_f <- approx(ret_f, n = 6)$y

early_ret <- cbind(age_e, ret_m, ret_f)
early_ret <- as.data.frame(early_ret)
names(early_ret) <- c("Age", "General Employees - Male", "General Employees - Female")

write.xlsx(early_ret, file = "early_retirement.xlsx", sheetName = "Early Retirement")




##### Other Terminations #####

yos <- c(5, 10, 15, 20, 25, 30)
yos <- approx(yos, n = 26)$y

ter_m <- c(0.088, 0.055, 0.035, 0.024, 0.017, 0.015)
ter_m <- approx(ter_m, n = 26)$y

ter_f <- c(0.103, 0.064, 0.04, 0.029, 0.025, 0.025)
ter_f <- approx(ter_f, n = 26)$y

oth_ter <- cbind(yos, ter_m, ter_f)
oth_ter <- as.data.frame(oth_ter)
names(oth_ter) <- c("Years of Service", "General Employees - Male", "General Employees - Female")

write.xlsx(oth_ter, file = "other_terminations_retirement.xlsx", sheetName = "Other Terminations")



##### Disability Retirement #####

age_dr <- c(25, 35, 45, 55)
age_dr <- approx(age_dr, n = 31)$y

dr_m <- c(0.0001, 0.0003, 0.0011, 0.0032)
dr_m <- approx(dr_m, n = 31)$y

dr_f <- c(0.0001, 0.0001, 0.0010, 0.0028)
dr_f <- approx(dr_f, n = 31)$y

dis_ret <- cbind(age_dr, dr_m, dr_f)
dis_ret <- as.data.frame(dis_ret)
names(dis_ret) <- c("Age", "General Employees - Male", "General Employees - Female")

write.xlsx(dis_ret, file = "disability_retirement.xlsx", sheetName = "Disability Retirement")



##### Future Salaries #####

yos <- c(5, 10, 15, 20)
yos <- approx(yos, n = 16)$y

fs_m <- c(0.0603, 0.0510, 0.0463, 0.0437)
fs_m <- approx(fs_m, n = 16)$y

fs_f <- c(0.0645, 0.0546, 0.0468, 0.0442)
fs_f <- approx(fs_f, n = 16)$y

fut_sal <- cbind(yos, fs_m, fs_f)
fut_sal <- as.data.frame(fut_sal)
names(fut_sal) <- c("Years of Service", "General Employees - Male", "General Employees - Female")

write.xlsx(dis_ret, file = "future_salaries.xlsx", sheetName = "Future Salaries")


##### Vesting #####

age_v <- c(25, 35, 45, 55)

vest_m <- c(0.52, 0.71, 0.76, NA)

vest_f <- c(0.61, 0.7, 0.73, NA)

vest <- cbind(age_v, vest_m, vest_f)



##################################################
########## WINKLEVOSS PENSION FUNCTIONS ##########
##################################################

dec_tables <- read_excel('Idaho_Decrement_071620.xlsx')

dec_tables <- as.data.frame(dec_tables)



survival_prob <- function(start_age = 20, end_age = 65) {
  i <- start_age
  j <- end_age
  prod(1 - dec_tables[i:j, 2])
}

survival_prob(29, 50)




########## FORMULAS ##########


double_decrement <- function(q_1_r, q_2_r) {
  q_1_r * (1 - (1/2) * q_2_r)
}

double_decrement(q_1_r = 0.1, q_2_r = 0.1)


three_decrement <- function(q_1_r, q_2_r, q_3_r) {
  q_1_r * (1 - ((1/2) * (q_2_r + q_3_r)) + ((1/3) * (q_2_r * q_3_r)))
}

three_decrement(q_1_r = 0.1, q_2_r = 0.1, q_3_r = 0.1)


four_decrement <- function(q_1_r, q_2_r, q_3_r, q_4_r) {
  q_1_r * (1 - ((1/2) * (q_2_r + q_3_r + q_4_r)) +
             ((1/3) * (q_2_r * q_3_r + q_2_r*q_4_r + q_3_r*q_4_r)) -
             ((1/4) * (q_2_r * q_3_r * q_4_r)))
}



male_dec_table <- dec_tables %>%
  select(1:2, 4, 6:7, 10)


male_d_dec <- male_dec_table %>%
  mutate(d_dec = double_decrement(`Mortality - Male`, `DR - Male`))


male_t_dec <- male_d_dec %>%
  mutate(t_dec = three_decrement(`Mortality - Male`, `DR - Male`, `SR TA - Male`))


male_f_dec <- male_t_dec %>%
  mutate(f_dec = four_decrement(`Mortality - Male`, `DR - Male`, `SR TA - Male`, `ER - Male`))

View(male_f_dec)
























