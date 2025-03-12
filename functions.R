# Loading necessary packages 
library(broom)
library(papaja) 

#Create a function to make dynamic inline text easier 
#For logistic results 
format_logit_results <- function(model, predictor, label) {
  # Extract model output
  logit_results <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
  
  # Subset the desired predictor
  predictor_row <- logit_results[logit_results$term == predictor, ]
  
  # Ensure predictor exists in the model output
  if (nrow(predictor_row) == 0) {
    return("Predictor not found in model.")
  }
  
  # Extract values
  OR <- round(predictor_row$estimate, 2)
  CI_lower <- round(predictor_row$conf.low, 2)
  CI_upper <- round(predictor_row$conf.high, 2)
  p_val <- apa_p(predictor_row$p.value)  # APA-style p-value
  
  # Format output with italicized p
  return(sprintf("The odds of psychiatric drug exposure for individuals with **%s** are **%.2f** times that of the reference group, with a **95%% CI** of (%.2f, %.2f) and *p* = %s.", 
                 label, OR, CI_lower, CI_upper, p_val))
}

#For chisquare test results 
get_chisq_p <- function(income_level, data) {
  p_val <- data[data$`Income Level` == income_level, "Chi-Square / Fisher's p-value"]
  return(paste0("*p* = ", p_val))
}