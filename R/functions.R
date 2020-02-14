library(msm)

# Population generation
generate_pop_actives <-
  function(entry_age,
           ret_age,
           pop_maturity_type,
           pop_size,
           mean_age_active,
           sd_age_active) {
    # generating empty vector of size of age range
    pop1 <- numeric(ret_age - entry_age + 1)
    
    # Conditional statements to generate population based on user input for population
    if (pop_maturity_type == 'custom')
    {
      pop1 <-
        round(
          rtnorm(
            pop_size,
            mean_age_active,
            sd_age_active,
            lower = entry_age - 1,
            upper = ret_age + 1
          )
        )
    }
    else if (pop_maturity_type == 'Over mature')
    {
      pop1 <-
        round(rtnorm(
          pop_size,
          ret_age,
          sd_age_active,
          lower = entry_age - 1,
          upper = ret_age + 1
        ))
    }
    else if (pop_maturity_type == 'Under mature')
    {
      pop1 <-
        round(rtnorm(
          pop_size,
          entry_age,
          sd_age_active,
          lower = entry_age - 1,
          upper = ret_age + 1
        ))
    }
    else if (pop_maturity_type == 'Uniform') {
      repcount <- round(pop_size / (ret_age - entry_age))
      for (i in entry_age:ret_age)
        pop1 <- c(pop1, rep(i, repcount))
      pop1 <- pop1[pop1 != 0]
    }
    
    # truncating population exceeding the age range
    pop1 <- pop1[pop1 != entry_age - 1]
    pop1 <- pop1[pop1 != ret_age + 1]
    
    actives <- tibble(age = pop1, population = 'active')
    return(actives)
  }

# Generate retirees population
generate_pop_retirees <-
  function(entry_age,
           ret_age,
           mort_tab,
           population_actives) {
    # Getting active population frequencies for each age group
    pop_freq <- table(population_actives)
    
    max_age <- 100
    
    # generating retiree population frquencies for each age after retirement to max_age using mortality table
    pop2 <-
      ceiling(pop_freq[length(pop_freq)] * cumprod(1 - get_mortality_table(entry_age, ret_age, mort_tab)[(ret_age - entry_age + 1):(max_age - entry_age + 1)]))
    
    # empty vector
    pop3 <- vector()
    # age just after retirement
    j <- ret_age + 1
    # translating the retiree frquency to population for age distribution plot
    for (i in pop2) {
      pop3 <- c(pop3, rep(j, times = i))
      j = j + 1
      if (j == max_age + 1)
        break
      
    }
    ### truncating any active members in the population of retiress ###
    pop3 <- pop3[2:length(pop3)]
    retirees <- tibble(age = pop3, population = 'retiree')
    ### return population vector to server.R ###
    return(retirees)
  }

# Mortality table

# Mortality.Rdata consists of the mortality tables in the form of a data frame
load(here('data', 'MortalityASRS.RData'))
load(here('data', 'Mortality.RData'))

# Termination.Rdata consists of the termination rates in the form of a data frame
load(here('data', 'Termination.RData'))

get_mortality_table <- function(entry_age, ret_age, mort_tab) {
  annuitant_mort <-
    (mort_tab + 1) # skips a column to access annuitants in the data frame
  # minimum age in the mortality tables
  min_mort_age <- 18
  # maximum age in the mortality tables
  max_mort_age <- 100
  max_age <- max_mort_age
  # generating a vector of mortality rates with active members and annuitants based on user
  # input for mortality table
  mortality_tab <-
    c(mort_tables[[mort_tab]][(entry_age - min_mort_age + 1):(ret_age - min_mort_age + 1)],
      mort_tables[[annuitant_mort]][(ret_age - min_mort_age + 2):(max_age - min_mort_age + 1)])
  # return the new vector without any NAs to calling code
  return(na.omit(mortality_tab))
}
