# New Mexico ERB funding model
# This is provided as-is, without an expressed or implied warranty
# Version 1.0
# Copyright The Terry Group 2020


#### REASON edits noted with ** ####
### model function that allows to project AAL, MVA, etc. based on function inputs (act. assumptions)

###########  FUNCTION STARTS  #############

NM_final_model_f = function(dis_r_20 = 0.0725,
                            actual_inflation_20 = 0.025,
                            consistent_inflation = 1,
                            asset_r_21 = 0.0725,
                            consistent_asset_r_21 = 1,
                            scenario = 2, #(scenarios #2:5 for 
                            #1.recession/
                            #2.2 recessions/
                            #3. severe recession/
                            #4. 2 severe recessions
                            COLA_scenario = "Expected COLA",
                            ER_contri_policy = "Statutory Rate",
                            yearbase_input_file = "year_base_var.csv",
                            agebase_input_file = "age_base_var.csv",
                            output_select = 0){
  
  library(tidyverse)  # version 1.3.0
  library(purrr)    # version 0.3.4
  
  year_base_var = read.csv(file = "/Users/anilniraula/Downloads/year_base_var.csv", 
                           header = TRUE, fileEncoding = "UTF-8-BOM") #**Modified file location to read from local csv copies
  #View(year_base_var)
  age_base_var = read.csv(file = "/Users/anilniraula/Downloads/ERB R model/R model copy/Model_Prototype_08182020/age_base_var.csv",
                          header = TRUE, fileEncoding = "UTF-8-BOM") #**Modified file location to read from local csv copies
  
  #View(year_base_var)
  #View(age_base_var)
  
  dis_r = 0.0725           # Discount Rate
  #dis_r_20 = 0.0725       # Discount Rate Valuation 2020      
  infla = 0.025           # Assumed Inflation (for 2020 real dollar cont)
  #COLA_scenario = "Expected COLA"
  
  
  ## 3.2 Plan design
  
  
  N_C_T1 = 0.1368     # Normal Cost - Tier 1
  N_C_T2 = 0.1368     # Normal Cost - Tier 2
  N_C_T3 = 0.1368     # Normal Cost - Tier 3
  N_C_NH = 0.105      # Normal Cost - New Hire
  EE_contri_r_over24 = 0.107        # Employee Contribution Rate (salary > 24000)
  EE_contri_r_less24 = 0.079        # Employee Contribution Rate (Salary < 24000)
  
  ## 3.3 Assumptions
  
  asset_r_20 = 0.0725    # Actual return on assets (FY 2020)
  #asset_r_21 = 0.0725    # Actual return on assets (FY 2021+)
  prop_payroll_over24 = 0.995   # Percentage of payroll over 24000
  payroll_growth = 0.03     # payroll growth
  COLA = 0.019            # COLA assumption
  prop_retiree_over25 = 0.4   # % Retirees with over 25 years at Retirement
  Ad_exp = 0.0034       # Administrative Expenses (as % of payroll)
  first_COLA_age = 66 # First COLA
  actual_inflation_19 = 0.0228
  
  ## 3.4 Funding Policy
  
  
  # ER_contri_policy = "ADC"
  stat_ER_contri_r = 0.1415   # Statutory Employer Contribution Rate
  ER_contri_ERB = 0.0325    #Employer contribution to ERB on ARP Payroll
  n_year_adc = 30   #Number of Years - ADC
  amort_base_incre_r = 0.03   # Amortization Base Increase Rate
  threshold_base_reset = 99.99    # Reset Bases to Zero Funding Threshold
  threshold_contri_holi_funded_r = 1.2  # Contribution Holiday Funded Ratio Threshold (100% or more)
  AVA_Corridor_lower = 0   # AVA Corridor - lower bound (% of MVA)
  AVA_Corridor_upper = 9.99   # AVA Corridor - upper bound (% of MVA)
  
  
  ## 3.5 Liability Sensitivties
  
  
  lia_sens = data.frame(a = c(0, 13544.691114, 10228.778073),
                        b = c(0, 13544.691114, 7577.302491),
                        c = c(0, 13544.691114, 5384.280133))
  lia_sens[1, ] = lia_sens[2, ] + lia_sens[3, ]
  colnames(lia_sens) = c("6.25%", "7.25%", "8.25%")
  rownames(lia_sens) = c("Total pension liability",
                         "Fiduciary net position",
                         "Net pension liability")
  lia_sens_to_dis_r = ((lia_sens[1, 1] / lia_sens[1, 3]) ^ 0.5 - 1) *
    100  #Liability sensitivity to discount rate
  convexity = ((lia_sens[1, 1] * lia_sens[1, 3]) / 
                 (lia_sens[1, 2] ^ 2) - 1) * 100 
  N_C_sens = lia_sens_to_dis_r * 2  # Normal Cost sensitivity 
  
  
  ## 3.6 Payroll
  
  
  # Tier 1, 2, 3 payroll for 2018 and 2019
  payroll_1819 = data.frame(
    Tier_1 = c(1537.117105, 1488.566508, 0.04, 0.04, 2019),
    Tier_2 = c(292.08833, 288.171815, 0.03, 0.03, 2019),
    Tier_3 = c(780.042198, 930.944758, 0.02, 0.02, 2019))
  rownames(payroll_1819) = c("2018 payroll", "2019 payroll",
                             "parameter 1", "parameter 2",
                             "Anchor Yr")
  ARP_pay_18 = 6.037541 / 0.03
  ARP_pay_19 = 6.15144 / 0.03
  
  
  ## 3.7 Benefit Part
  
  
  # age_group_portion = data.frame(age_group = c(60, 65, 70, 75, 80, 85, 90, 95),
  #                                portion = c(387.95800991588, 299.221815299858,
  #                                            210.611464951392, 128.631027125928,
  #                                            78.5766011975189, 41.2295624383448,
  #                                            15.0039725832479, 3.41253348782934))
  joint_survive_factor = 1.0468
  
  ## 3.8 matrix initial values
  
  # 2020 payroll total
  payroll_total_2020 = 2839.919652
  # original discount rate in 2018
  dis_origin_18 = 0.0725
  # 2018 new discount rate
  dis_new_18 = 0.0725
  # 2018 and 2019 liability with original discount rate
  liability_origin_1819 = c(20457.996102, 21287.572757)
  # total normal cost for Tier 1, 2, 3 in 2018
  N_C_T123_origin_18 = 374.095202
  # 2017's funded ratio of AVA
  FYE_2y_19 = 0.629
  # 2018 AVA
  AVA_18 = 12996.62532
  # 2018 and 2019 MVA
  MVA_1819 = c(12970.300855, 13544.691114)
  # 2018, 2019 asset return
  ROA_MVA_1819 = c(0.072, 0.081)
  # 2019 administration expense
  admin_exp_19 = -9.325712
  # 2019 Employee contribution
  EE_contri_19 = 300.652249 + 2.789843
  # 2019 Employer contribution to ERB on ARP payroll
  EE_ARP_19 = 5.972272
  # 2019 benefit payments
  benefit_pay_19 = -(1122.274311 + 42.370676)
  # 2019 net cash flow
  net_CF_19 = -454.653839
  # 2019 1-year deferred value, 2-year deferred value and 3-year deferred value
  D_1_2_3_19 = c(-1.039379, 183.021847, -104.754168)
  
  # 4. construct data frame for each part
  
  
  ## 4.1 Payroll and Liability
  
  pension_lia = matrix(0, 34, 15)
  n = nrow(pension_lia)
  colnames(pension_lia) = c("year",
                            "payroll_total",
                            "payroll_T1",
                            "payroll_T2",
                            "payroll_T3",
                            "payroll_T4",
                            "payroll_ARP",
                            "origin_DR",
                            "new_DR",
                            "liability_origin",
                            "N_C_T123_origin",
                            "N_C_T4_origin",
                            "liability_new",
                            "N_C_T123_new",
                            "N_C_T4_new")
  # year
  pension_lia[, "year"] = 2018: 2051
  
  
  ## 4.2 Asset
  
  # build matrix for pension part
  pension_asset = matrix(0, n, 7)
  colnames(pension_asset) = c("AVA", "MVA", "ROA_MVA", "UAL-AVA",
                              "funded_r_AVA", "funded_r_MVA",
                              "funding_Period")
  
  
  ## 4.3 other
  
  # matrix for cash flow part
  cash_flow = matrix(0, n, 8)
  colnames(cash_flow) = c("benefit_pay", "admin_exp", "EE_contri",
                          "ER_N_C_admin", "ER_amort", "ER_ARP",
                          "sol_contri", "total_contri")
  
  # matrix for total employer contribution part
  Fixed_data = matrix(0, n, 3)
  colnames(Fixed_data) = c("total_ER", "infl_adj", "AVA_FR")
  
  # matrix for development of actuarial valued asset
  develop_AVA = matrix(0, n, 9)
  colnames(develop_AVA) = c("net_CF", "exp_inv_income", "exp_MVA",
                            "G_L", "D_current", "D_1", "D_2", "D_3",
                            "total_D")
  
  # matrix for amortization base, payment and annual sum-up total
  amort_policy = matrix(0, n, 2)
  colnames(amort_policy) = c("net_amort_pay", "initial_year")
  amort_policy[2:(n - 2), "initial_year"] = n_year_adc
  amort_base = matrix(0, n, (n_year_adc + 1))
  amort_pay = amort_base
  
  
  ## 4.4 benefit
  
  # benefit matrix
  mortality_table = age_base_var[, 1: 2]
  benefit_table = matrix(0, n, nrow(mortality_table))
  colnames(benefit_table) = 60: 120
  benefit_table[2,] = age_base_var$benefit
  
  # cola matrix
  cola_table = matrix(0, n, 10)
  colnames(cola_table) = c(
    "COLA_assumption", "FYE-2y", "actual_inflation", "full_COLA",
    "actual_COLA", "Expected", "PV_1.9", "COLA_gain", "percent_intact", "COLA")
  
  
  # 5. User-defined function
  
  cum_pv_f = function(DR, cola){
    if (cola == 0) {
      aa = sum(mortality_table$PV_0[(DR + 1): nrow(mortality_table)])
      bb = (1 + dis_r) ^ DR / mortality_table$Survival_rate[DR + 1]
      return(aa * bb * joint_survive_factor)
    } else if (cola == 0.019) {
      aa = sum(mortality_table$PV_19[(DR + 1): nrow(mortality_table)])
      bb = (1 + dis_r) ^ DR / mortality_table$Survival_rate[DR + 1]
      cc = (1 + 0.019) ^ 
        max((mortality_table$DR_per[DR + 1] - 
               first_COLA_age + 1 + mortality_table$Age[1]), 0)
      return(aa * bb / cc * joint_survive_factor)
    }
  }
  
  cola_pv_f = function(DR, cola){
    if (cola == 0) {
      mortality_table$Survival_rate[DR + 1] / 
        (1 + dis_r) ^ (mortality_table$DR_per[DR + 1] + 0.5)
    } else if (cola == 0.019){
      aa = mortality_table$Survival_rate[DR + 1]
      bb = (1 + dis_r) ^ (mortality_table$DR_per[DR + 1] + 0.5)
      cc = (1 + COLA) ^ 
        max((mortality_table$DR_per[DR + 1] - 
               first_COLA_age + 1 + mortality_table$Age[1]), 0)
      return(aa / bb * cc)
    }
  }
  
  
  ## 5.2 Present value of annuity due
  
  
  pv_due_f = function(rate, nper, pmt) {
    a = pmt * (1 - (1 + rate) ^ (-nper)) / rate * (1 + rate)
    return(a)
  }
  
  
  ## 5.3 Full COLA function
  
  
  # Corresponding to formula in excel EE column
  full_cola_f = function(a) {
    ifelse(a < 0.02, a, min(max(a * 0.5, 0.02), 0.04))
  }
  
  
  ## 5.4 Funding period functions
  
  
  
  # creating function to solve the investment period
  # based on the property of natural log, ln(x) can't take non-positive
  # let invest-period = log(a), where a is 1 minus combination of 
  # pv(present value), r(interest rate), p(periodic installment)
  # a must > 0
  # b is the row number
  # this function can compute investment period for different row in matrix
  # with row number known, the function will locate other important variables like
  # "pension_lia[b, "new_DR"]" new discount rate in liability matrix
  # "pension_asset[b, "UAL-AVA"]" unfunded liabilty
  period_solve_log = function(b) {
    r = (1 + pension_lia[b, "new_DR"]) / 
      (1 + payroll_growth) - 1
    pv = pension_asset[b, "UAL-AVA"] * 
      (1 + pension_lia[b, "new_DR"]) ^ 0.5
    p = sum(cash_flow[(b + 1), 5: 7])
    not_1 = pv * r / p / (1 + r)
    if (not_1 >= 1) {
      return(NA)
    } else {
      return(-log(1 - not_1) / log(1 + r))
    }
  }
  
  # defined a function that handles negative value from 
  # "period_solve_log()" above
  # when a = 1, it means the statement "unfunded liability is <= 0" is TRUE.
  # This function is corresponding to formula in excel column Y
  # b is corresponding to the function input above.
  
  funding_per_log = function(a, b) {
    ifelse(a == 1, 0, max(period_solve_log(b), 0))
  }
  
  
  # 6. Matrices filling
  
  ## 6.1 payroll and liability
  
  
  # payroll of 2018 and 2019 for Tier 1, 2, 3, 4 and total
  pension_lia[1:2, "payroll_T1"] = payroll_1819$Tier_1[1:2]
  pension_lia[1:2, "payroll_T2"] = payroll_1819$Tier_2[1:2]
  pension_lia[1:2, "payroll_T3"] = payroll_1819$Tier_3[1:2]
  pension_lia[1:2, "payroll_T4"] = 0
  pension_lia[1:2, "payroll_total"] = rowSums(pension_lia[1:2, 3:5])
  pension_lia[3, "payroll_total"] = payroll_total_2020 
  # ARP payroll
  pension_lia[1:3, "payroll_ARP"] = c(0, ARP_pay_18, ARP_pay_19)
  # original discount rate
  pension_lia[1: (n - 1), "origin_DR"] = dis_r
  pension_lia[1, "origin_DR"] = dis_origin_18 
  # new discount rate
  pension_lia[1: (n - 1), "new_DR"] = dis_r_20
  pension_lia[1, "new_DR"] = dis_new_18 
  pension_lia[2, "new_DR"] = pension_lia[2, "origin_DR"]
  
  # Original discount rate: liability and normal cost
  # liability with original discount rate
  pension_lia[1:2, "liability_origin"] = liability_origin_1819 
  # Normal cost with original discount rate
  pension_lia[1:2, "N_C_T123_origin"] = 
    c(N_C_T123_origin_18 , 0.1368 * pension_lia[3, "payroll_total"])
  pension_lia[1:2, "N_C_T4_origin"] = 0
  
  # New discount rate: liability and normal cost
  ## difference between original and new discount rate
  diff_dis_r = 
    pension_lia[1: 2, "origin_DR"] - 
    pension_lia[1: 2, "new_DR"]
  ## liability corresponding to new discount rate
  pension_lia[1: 2, "liability_new"] = 
    pension_lia[1: 2, "liability_origin"] *
    ((1 + lia_sens_to_dis_r / 100) ^ (diff_dis_r * 100) *
       (1 + convexity / 100) ^ ((diff_dis_r * 100) ^ 2 / 2))
  ## Normal cost Tier 1, 2, 3
  pension_lia[1: 2, "N_C_T123_new"] = 
    pension_lia[1: 2, "N_C_T123_origin"] * 
    ((1 + N_C_sens / 100) ^ (diff_dis_r * 100))
  ## Normal cost Tier 4
  pension_lia[1: 2, "N_C_T4_new"] = 
    pension_lia[1: 2, "N_C_T4_origin"] * 
    ((1 + N_C_sens / 100) ^ (diff_dis_r * 100))
  
  
  ## 6.2 benefit, cola table and mortality table
  
  
  # Insert initial benefit
  benefit_table[3: n, 1] = year_base_var$initial_benefit[3: n]
  
  
  ### 6.2.1 computation in mortality table
  
  
  # discount period
  mortality_table$DR_per = 0: (nrow(mortality_table) - 1)
  ## the column named DR_per was originally not in "mortality_table". The code above is used to add new column into the data frame.
  
  # Present value with cola = 0
  mortality_table$PV_0 = 
    sapply(0: (nrow(mortality_table) - 1),
           cola_pv_f, cola = 0, simplify = "array")
  # present value with cola = 1.9%
  mortality_table$PV_19 = 
    sapply(0: (nrow(mortality_table) - 1),
           cola_pv_f, cola = COLA, simplify = "array")
  # cumulative present value with 0% discount rate
  mortality_table$cumPV_0 = 
    sapply(0: (nrow(mortality_table) - 1),
           cum_pv_f, cola = 0, simplify = "array")
  # cumulative present value with 1.9% discount rate
  mortality_table$cumPV_19 = 
    sapply(0: (nrow(mortality_table) - 1), 
           cum_pv_f, cola = 0.019, simplify = "array")
  
  
  ### 6.2.2 `cola_table` input
  
  
  # cola assumption, (excel column EB)
  cola_table[3: (n - 1), "COLA_assumption"] = COLA
  # 2017's funded ratio of AVA
  cola_table[2, "FYE-2y"] = FYE_2y_19
  # actual inflation
  cola_table[2, "actual_inflation"] = actual_inflation_19
  if (consistent_inflation == 1) {
    cola_table[3: (n - 1), "actual_inflation"] = actual_inflation_20
  } else {
    cola_table[3: (n - 1), "actual_inflation"] = 
      year_base_var$actual_inflation[3: (n - 1)]
  }
  
  # full COLA, (excel column EE)
  cola_table[3: (n - 1), "full_COLA"] = 
    sapply(cola_table[2: (n - 2), "actual_inflation"], 
           full_cola_f, simplify = "array")
  
  
  ## 6.3 Asset
  
  
  # 2018 AVA
  pension_asset[1, "AVA"] = AVA_18 
  # 2018 and 2019 MVA
  pension_asset[1: 2, "MVA"] = MVA_1819 
  # 2018, 2019, 2020 and 2021 beyond asset return
  pension_asset[1: 3, "ROA_MVA"] = c(ROA_MVA_1819, asset_r_20)
  if (consistent_asset_r_21 == 1){
    pension_asset[4: (n - 1), "ROA_MVA"] = asset_r_21
  } else {
    pension_asset[4: (n - 1), "ROA_MVA"] = 
      year_base_var[4: (n - 1), (scenario+1)]
  }
  
  View(year_base_var[4: (n - 1), (scenario+1)])
  
  ## 6.4 Cash Flow
  
  
  # 2019 administration expense
  cash_flow[2, "admin_exp"] = admin_exp_19 
  # 2019 Employee contribution
  cash_flow[2, "EE_contri"] = EE_contri_19 
  # 2019 Employer contribution to ERB on ARP payroll
  cash_flow[2, "ER_ARP"] = EE_ARP_19 
  # 2019 Solvency contribution
  cash_flow[2, "sol_contri"] = max(
    -(pension_asset[1, "MVA"] * (1 + pension_asset[2, "ROA_MVA"]) + 
        (1 + pension_asset[2, "ROA_MVA"]) ^ 0.5 * 
        sum(cash_flow[2, (1: 6)])) / 
      (1 + pension_asset[2, "ROA_MVA"]) ^ 0.5,
    0
  )
  # 2019 benefit payments
  cash_flow[2, "benefit_pay"] = benefit_pay_19
  
  
  ## 6.5 Development of the Actuarial Value of Assets
  
  
  # 2019 net cash flow
  develop_AVA[2, "net_CF"] = net_CF_19
  # 2019 1-year deferred value, 2-year deferred value and 3-year deferred value
  develop_AVA[2, c("D_1", "D_2", "D_3")] = D_1_2_3_19 
  
  
  # 7. model
  
  ## 7.1 payroll part
  
  
  # i is index number of a loop. Also, it is the row number for each of matrices built above. It will be assigned with one number for each loop. The number comes from vector (3: n) in order.
  for (i in 3: n) {
    if (i > 3) {
      # total payroll and ARP is known for 2020. The code will execute when
      # i reaches 4 or bigger.
      # payroll total part
      pension_lia[i, "payroll_total"] = 
        pension_lia[(i - 1), "payroll_total"] * (1 + payroll_growth)
      # ARP
      pension_lia[i, "payroll_ARP"] = 
        pension_lia[(i - 1), "payroll_ARP"] / 
        pension_lia[(i - 1), "payroll_total"] * 
        pension_lia[i, "payroll_total"]
    }
    # tier 1, 2, 3
    for(j in 1: 3) {
      # payroll formula for tier 1, 2, 3 has same structure
      # so the j will be used for identifying matrix column
      # and data frame's column (payroll_1819, refer to line 103)
      bbb = max(0, 
                (pension_lia[i, "year"] - payroll_1819["Anchor Yr", j]))
      ccc = 1 - payroll_1819["parameter 1", j]
      ddd = payroll_1819["parameter 1", j] * 
        payroll_1819["parameter 2", j]
      pension_lia[i, (j + 2)] = 
        max((pension_lia[(i - 1), (j + 2)] * (ccc - ddd * bbb)), 0)
    }
  }
  # tier 4
  ## column for tier 4 payroll is computed outside of the loop
  ## since each year's value doesn't directly depend on previous year
  ## instead, it only depends on total payroll and payroll tier 1,2,3 
  ## on the same year
  pension_lia[3: n, "payroll_T4"] = 
    pension_lia[3: n, "payroll_total"] - 
    rowSums(pension_lia[3: n, 3: 5])
  
  
  ## 7.2 Linear operation
  
  ### 7.2.1 Accrued Liability part
  
  
  # normal cost for Tier 4 -- original discount rate
  pension_lia[3: (n - 1), "N_C_T4_origin"] = 
    pension_lia[4: n, "payroll_T4"] * N_C_NH
  
  # original discount rate: Normal Cost of Tier 1,2,3
  pension_lia[3 : (n - 1), "N_C_T123_origin"] = 
    pension_lia[4 : n, "payroll_T1"] * N_C_T1 + 
    pension_lia[4 : n, "payroll_T2"] * N_C_T2 + 
    pension_lia[4 : n, "payroll_T3"] * N_C_T3
  
  
  ### 7.2.2 Cash Flow
  
  
  # Administration Expenses
  cash_flow[3: (n - 1), "admin_exp"] = 
    -pension_lia[3: (n - 1), "payroll_total"] * Ad_exp
  # Employee Contribution
  cash_flow[3: (n - 1), "EE_contri"] = 
    pension_lia[3: (n - 1), "payroll_total"] * 
    EE_contri_r_over24 * prop_payroll_over24 +
    EE_contri_r_less24 * (1 - prop_payroll_over24) * 
    pension_lia[3: (n - 1), "payroll_total"]
  # Employer normal cost and administration expense
  cash_flow[2, "ER_N_C_admin"] =  
    pension_lia[1, "N_C_T123_new"] + 
    pension_lia[1, "N_C_T4_new"] - 
    cash_flow[2, "EE_contri"]
  ## 2019 Employer amortization (this one requires 2019 employer normal
  ## cost and administration)
  cash_flow[2, "ER_amort"] = 400.576784 - cash_flow[2, "ER_N_C_admin"]
  # Employer contribution to ERB on ARP payroll
  cash_flow[3: (n - 1), "ER_ARP"] = 
    pension_lia[3: (n - 1), "payroll_ARP"] * ER_contri_ERB
  
  
  ### 7.2.3 2019 in Development of the Actuarial Value of Assets
  
  # Expected investment income of 2019
  develop_AVA[2, "exp_inv_income"] = 
    pension_lia[1, "new_DR"] * pension_asset[1, "MVA"] + 
    develop_AVA[2, "net_CF"] * pension_lia[1, "new_DR"] / 2
  # Expected Market Value of asset in 2019
  develop_AVA[2, "exp_MVA"] = 
    pension_asset[1, "MVA"] + develop_AVA[2, "net_CF"] + 
    develop_AVA[2, "exp_inv_income"]
  # 2019 Gain/ loss
  develop_AVA[2, "G_L"] = 
    pension_asset[2, "MVA"] - develop_AVA[2, "exp_MVA"]
  # 2019 current year deferred
  develop_AVA[2, "D_current"] = develop_AVA[2, "G_L"] * 0.8
  # 2019 total deferred
  develop_AVA[2, "total_D"] = sum(develop_AVA[2, (5: 8)])
  
  
  ### 7.2.4 2018 and 2019 for asset
  
  # 2019 AVA
  pension_asset[2, "AVA"] = min(
    max(pension_asset[2, "MVA"] * AVA_Corridor_lower, 
        (pension_asset[2, "MVA"] - develop_AVA[2, "total_D"])), 
    AVA_Corridor_upper * pension_asset[2, "MVA"]
  )
  # 2018 and 2019 funded ratio for AVA
  pension_asset[1: 2, "funded_r_AVA"] = 
    pension_asset[1: 2, "AVA"] / pension_lia[1: 2, "liability_new"]
  
  
  ## 7.3 arithmetic sequence operation
  
  
  next_year_survival = 
    mortality_table$Survival_rate[2: nrow(mortality_table)]/ 
    mortality_table$Survival_rate[1: (nrow(mortality_table) - 1)]
  offset_num = first_COLA_age - 2 - mortality_table$Age[1]
  for (i in 3: (n - 1)) {
    # In Each loop, we will compute unknowns within same year. 
    # The loop starts from row 3 (year 2020) 
    # At the end of each loop, index "i" will take next value from 
    # vector (3: (n - 1)) (3, 4, 5,..., n - 1) 
    
    # First, we must get COLA
    ## get AVA funded ratio from 2 years back
    cola_table[i, "FYE-2y"] = pension_asset[(i - 2), "funded_r_AVA"]
    ## based on formula from excel column EF, obtain actual COLA
    cola_table[i, "actual_COLA"] = max(
      ifelse(cola_table[i, "FYE-2y"] >= 1,
             cola_table[i, "full_COLA"],
             ifelse(cola_table[i, "FYE-2y"] > 0.9,
                    cola_table[i, "full_COLA"] * 
                      (0.95 * prop_retiree_over25 + 
                         0.9 * (1 - prop_retiree_over25)),
                    cola_table[i, "full_COLA"] * 
                      (0.9 * prop_retiree_over25 + 
                         0.8 * (1 - prop_retiree_over25)))), 0)
    ## Finally, based on COLA scenario input, obtain COLA
    cola_table[i, "COLA"] = 
      ifelse(COLA_scenario == "Expected COLA",
             ifelse(pension_lia[(i - 1), "year"] == 2019, 
                    cola_table[i, "actual_COLA"],
                    cola_table[i, "COLA_assumption"]),
             cola_table[i, "actual_COLA"])
    ## With COLA known, we can calculate entire row for benefit matrix
    benefit_table[i, 2: nrow(mortality_table)] = 
      benefit_table[(i - 1), 1: (nrow(mortality_table) - 1)] * 
      (1 + ifelse(
        mortality_table[2: nrow(mortality_table), "Age"] < first_COLA_age,
        0, 
        cola_table[i, "COLA"])
      ) * next_year_survival
    ## Then, we can sum up the benefit payment and insert it under the column
    ## "benefit_pay" in the cash flow matrix
    cash_flow[i, "benefit_pay"] = -sum(benefit_table[i, ])
    ## Next, we want to solve the present value with 1.9% (excel column EG)
    if (i != 3){
      cola_table[i, "Expected"] = 
        benefit_table[i, 1] * mortality_table$cumPV_19[1] + 
        ifelse(first_COLA_age < 62,
               0,
               sum(benefit_table[(i - 1), 1: (offset_num + 1)] *
                     next_year_survival[1 : (1 + offset_num)] * 
                     mortality_table$cumPV_19[2: (2 + offset_num)])) + 
        sum(benefit_table[(i - 1), (offset_num + 2): (ncol(benefit_table) - 1)] *
              next_year_survival[(2 + offset_num): length(next_year_survival)] * 
              mortality_table$cumPV_19[(3 + offset_num): nrow(mortality_table)]) *
        (1 + cola_table[i, "COLA_assumption"])
      
      cola_table[i, "PV_1.9"] = sum(benefit_table[i, ] *
                                      mortality_table$cumPV_19)
      ## The COLA gain/loss can directly affect liability
      cola_table[i, "COLA_gain"] = 
        cola_table[i, "PV_1.9"] - cola_table[i, "Expected"]
    }
    
    
    # liability baseline
    # with benefit payment and COLA gain/loss known, we can solve liability
    pension_lia[i, "liability_origin"] = 
      pension_lia[(i - 1), "liability_origin"] * 
      (1 + pension_lia[(i - 1), "origin_DR"]) + 
      (pension_lia[(i - 1), "N_C_T123_origin"] + 
         pension_lia[(i - 1), "N_C_T4_origin"] + 
         cash_flow[i, "benefit_pay"]) * 
      (1 + pension_lia[(i - 1), "origin_DR"]) ^ 0.5 +
      cola_table[i, "COLA_gain"]
    
    # New Discount Rate:
    ## liability
    diff_dis_r = 
      pension_lia[i, "origin_DR"] - 
      pension_lia[i, "new_DR"]
    ## liability change due to discount rate change
    pension_lia[i, "liability_new"] = 
      pension_lia[i, "liability_origin"] *
      ((1 + lia_sens_to_dis_r / 100) ^ (diff_dis_r * 100) *
         (1 + convexity / 100) ^ ((diff_dis_r * 100) ^ 2 / 2))
    ## Normal cost Tier 1, 2, 3
    pension_lia[i, "N_C_T123_new"] = 
      pension_lia[i, "N_C_T123_origin"] * 
      ((1 + N_C_sens / 100) ^ (diff_dis_r * 100))
    ## Normal cost Tier 4
    pension_lia[i, "N_C_T4_new"] = 
      pension_lia[i, "N_C_T4_origin"] * 
      ((1 + N_C_sens / 100) ^ (diff_dis_r * 100))
    
    # Employer normal cost and administration expense
    cash_flow[i, "ER_N_C_admin"] =  
      pension_lia[(i - 1), "N_C_T123_new"] + 
      pension_lia[(i - 1), "N_C_T4_new"] - 
      cash_flow[i, "EE_contri"]
    
    # amortization
    ## outstanding base
    ## The base has two component for year 2021 and beyond
    ### 1. gain or loss from asset return
    ### 2. amortized remaining base from last year
    ## for year 2020, the amortization has only one component: unfunded liability
    ## thus, when i != 3, (year is 2021 or beyond), we need to calculate remaining
    ## base from last year's amortized base
    
    ## the following code corresponding to formula embedded in 
    ## excel column BI to CK
    if (i != 3) {
      for (k in (2: (i - 2))) {
        amort_base[(i - 1), k] = round(ifelse(
          (pension_asset[(i - 1), "funded_r_AVA"] >= 
             threshold_base_reset),
          0,
          amort_base[(i - 2), (k - 1)] * 
            (1 + pension_lia[(i - 2), "new_DR"]) - 
            amort_pay[(i - 2), (k - 1)] * 
            (1 + pension_lia[(i - 2), "new_DR"]) ^ 0.5
        ), 2)
      }
    }
    
    ## The following code corresponding to formula embedded in 
    ## excel column BH
    amort_base[(i - 1), 1] = ifelse(
      (pension_asset[(i - 1), "funded_r_AVA"] >= 
         threshold_base_reset),
      0,
      pension_lia[(i - 1), "liability_new"] - 
        pension_asset[(i - 1), "AVA"] - 
        sum(amort_base[(i - 1), (2: ncol(amort_base))])
    )
    
    ## amortization payment
    ## The following code is corresponding to formula embedded in excel
    ## column CN to DQ
    ## Also, it will use one of user-defined function called "pv_due_f()"
    ## please refer to 5.2
    for (k in (1: (i - 2))) {
      amort_pay[(i - 1), k] = ifelse(
        (pension_asset[(i - 1), "funded_r_AVA"] >=
           threshold_base_reset),
        0,
        amort_base[(i - 1), k] / 
          (((pv_due_f(rate = ((1 + pension_lia[(i - 1), "new_DR"]) / 
                                (1 + amort_base_incre_r) - 1),
                      nper = max((amort_policy[(i - 1), "initial_year"] - 
                                    k + 1), 1),
                      pmt = 1))) /
             ((1 + pension_lia[(i - 1), "new_DR"]) ^ 0.5))
      )
    }
    
    # net amortization payment
    amort_policy[(i - 1), "net_amort_pay"] = 
      sum(amort_pay[(i - 1), (1: (i - 2))])
    
    # employer amortization in Cash Flow
    cash_flow[i, "ER_amort"] = ifelse(
      (ER_contri_policy == "Statutory Rate" | pension_lia[i, "year"] == 2020),
      stat_ER_contri_r * pension_lia[i, "payroll_total"] - 
        cash_flow[i, "ER_N_C_admin"],
      max(amort_policy[(i - 1), "net_amort_pay"] - cash_flow[i, "ER_ARP"], 
          -cash_flow[i, "ER_N_C_admin"] - cash_flow[i, "ER_ARP"])
    )
    
    # Solvency Contribution
    cash_flow[i, "sol_contri"] = max(
      -(pension_asset[(i - 1), "MVA"] * 
          (1 + pension_asset[i, "ROA_MVA"]) + 
          (1 + pension_asset[i, "ROA_MVA"]) ^ 0.5 * 
          sum(cash_flow[i, (1: 6)])) / 
        (1 + pension_asset[i, "ROA_MVA"]) ^ 0.5,
      0
    )
    
    # MVA
    pension_asset[i, "MVA"] = 
      pension_asset[(i - 1), "MVA"] * 
      (1 + pension_asset[i, "ROA_MVA"]) + 
      sum(cash_flow[i, (1: 7)]) * 
      (1 + pension_asset[i, "ROA_MVA"]) ^ 0.5
    
    # Development of the Actuarial Value of Assets
    ## Net CF
    develop_AVA[i, "net_CF"] = 
      sum(cash_flow[i, 1: 7])
    ## EXP Investment income
    develop_AVA[i, "exp_inv_income"] = 
      pension_lia[(i - 1), "new_DR"] * pension_asset[(i - 1), "MVA"] + 
      develop_AVA[i, "net_CF"] * pension_lia[(i - 1), "new_DR"] / 2
    ## Expected MVA
    develop_AVA[i, "exp_MVA"] = 
      pension_asset[(i - 1), "MVA"] + develop_AVA[i, "net_CF"] + 
      develop_AVA[i, "exp_inv_income"]
    ## Gain/Loss
    develop_AVA[i, "G_L"] = 
      pension_asset[i, "MVA"] - develop_AVA[i, "exp_MVA"]
    ## Current Year Deferred
    develop_AVA[i, "D_current"] = develop_AVA[i, "G_L"] * 0.8
    ## Year-1 Deferred
    develop_AVA[i, "D_1"] = 
      develop_AVA[(i - 1), "D_current"] * 0.6 / 0.8
    ## Year-2 Deferred
    develop_AVA[i, "D_2"] = 
      develop_AVA[(i - 1), "D_1"] * 0.4 / 0.6
    ## Year-3 Deferred
    develop_AVA[i, "D_3"] = 
      develop_AVA[(i - 1), "D_2"] * 0.2 / 0.4
    ## Total Deferred
    develop_AVA[i, "total_D"] = 
      sum(develop_AVA[i, (5: 8)])
    
    # AVA 
    pension_asset[i, "AVA"] = min(
      max(pension_asset[i, "MVA"] * AVA_Corridor_lower,
          (pension_asset[i, "MVA"] - develop_AVA[i, "total_D"])),
      AVA_Corridor_upper * pension_asset[i, "MVA"]
    )
    
    # Funded ratio -- AVA
    pension_asset[i, "funded_r_AVA"] = 
      pension_asset[i, "AVA"] / pension_lia[i, "liability_new"]
  }
  
  
  ## 7.4 more Linear operation
  
  # UAL-AVA (2018 to 2050)
  pension_asset[1: (n - 1), "UAL-AVA"] = 
    pension_lia[1: (n - 1), "liability_new"] - 
    pension_asset[1: (n - 1), "AVA"]
  
  #Funded Ratio - MVA  (2018 to 2050)
  pension_asset[1: (n - 1), "funded_r_MVA"] = 
    pension_asset[1: (n - 1), "MVA"] / 
    pension_lia[1: (n - 1), "liability_new"]
  
  # total contribution in cash flow
  cash_flow[2: (n - 1), "total_contri"] = rowSums(cash_flow[2: (n - 1), 3: 7])
  
  
  
  # funding period
  
  # map each set of input variables to the function
  
  ## for more information about user-defined function "funding_per_log()"
  ## please refer to section 5.4
  
  ## r_i is the row index input for one of functions in section 5.4
  r_i = 1: (n - 2)
  
  ## transfer the truthfulness of the statement "unfunded liability <= 0"
  ## into numeric variable. 
  ## R usually treat TRUE as 1 and FLASE as 0. 
  T_F_list = as.list(as.numeric(pension_asset[1: (n - 2), "UAL-AVA"] <= 0))
  pension_asset[1: (n - 2), "funding_Period"] = 
    map2_dbl(T_F_list, r_i, funding_per_log)
  
  
  cola_table[3: (n - 1), "percent_intact"] = 
    cola_table[3: (n - 1), "PV_1.9"] / pension_lia[3: (n - 1), "liability_origin"]
  
  ## 7.5 Fixed Data point(total employer contribution)
  
  
  Fixed_data[2 : (n - 1), "total_ER"] = rowSums(cash_flow[2 : (n - 1), 4: 7])
  Fixed_data[2 : (n - 1), "infl_adj"] = 
    Fixed_data[2 : (n - 1), "total_ER"] / (1 + infla) ^ 
    sapply((pension_lia[2 : (n - 1), "year"] - pension_lia[3, "year"]), max, 0)
  Fixed_data[2: (n - 1), "AVA_FR"] = pension_asset[2: (n - 1), "funded_r_AVA"]
  
  if (output_select == 0){
    return(as.data.frame(cbind(pension_lia, pension_asset, cash_flow, 
                               develop_AVA, cola_table)))
  } else{
    return(as.data.frame(Fixed_data))
  }
  
}
###########  FUNCTION ENDS  #############
##**Example
model <- NM_final_model_f(dis_r_20 = 0.05,
                          actual_inflation_20 = 0.025,
                          consistent_inflation = 1,
                          asset_r_21 = 0.05,
                          consistent_asset_r_21 = 0,
                          scenario = 3,
                          COLA_scenario = "Expected COLA",
                          ER_contri_policy = "Statutory Rate",
                          yearbase_input_file = "year_base_var.csv",
                          agebase_input_file = "age_base_var.csv",
                          output_select = 0)
View(model$MVA)
plot(model$AVA, type = "line")
plot(model$MVA, type = "line")

#write.csv(model, file = "/Users/anilniraula/Downloads/ERB.model.R.csv")
#columns <- colnames(model)
#write.csv(columns , file = "/Users/anilniraula/Downloads/ERB.columns.R.csv")
##########
