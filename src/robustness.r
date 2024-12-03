df1 <- read.csv("output/all_scenarios_ratings_results.csv")

scenarios <- unique(df1$scenario_)
scenarios[4] <- "National"

slm_estimate <- c()
slm_t_value <- c()
robust_estimate <- c()
robust_t_value <- c()
slm_model_fits <- c()
robust_model_fits <- c()

df2 <- read.csv("rawdata/per-capita-ghg-emissions.csv")
df3 <- read.csv("rawdata/total-ghg-emissions.csv")
df4 <- read.csv("rawdata/carbon-intensity-electricity.csv")
df2 <- inner_join(df2, df3, by=c("Entity", "Year", "Code"))
df2 <- inner_join(df2, df4, by=c("Entity", "Year", "Code"))
df2 <- df2 %>% dplyr::filter(
    Year > 2020
) %>% group_by(
    Entity
    ) %>% dplyr::summarize(
    log_emission_pc = log(median(Per.capita.greenhouse.gas.emissions.in.CO..equivalents)),
    log_emissions_total = log(median(Annual.greenhouse.gas.emissions.in.CO..equivalents)),
    log_emissions_intensity = log(median(Carbon.intensity.of.electricity...gCO2.kWh))
    )


for (i in 1:length(scenarios)){
    temp <- df1 %>% 
        dplyr::filter(
        str_detect(scenario_, scenarios[i]) &
        str_detect(year, "30")
        ) %>% group_by(country) %>% summarise(
            rating_delta = median(rating_delta),
            actual = median(actual),
            est = mean(est)
        )
    df_estimate <- inner_join(temp, df2, by = c("country" = "Entity"))
    dt_model <- lm(rating_delta ~ scale(log_emissions_total), data = df_estimate)
    dt_robust_model <- coeftest(dt_model, vcov = vcovHC(dt_model, type = "HC0"))
    dt_model <- summary(dt_model)
    slm_estimate[i] <- dt_model$coefficients[2]
    slm_t_value[i] <- dt_model$coefficients[6]
    robust_estimate[i] <- dt_robust_model[2]
    robust_t_value[i] <- dt_robust_model[6]
    slm_model_fits[i] <- dt_model$adj
    robust_model_fits[i] <- dt_model$adj
}

results <- data.frame(
    scenario = scenarios,
    slm_estimate = slm_estimate,
    slm_t_value = slm_t_value,
    robust_estimate = robust_estimate,
    robust_t_value = robust_t_value,
    slm_model_fits = slm_model_fits,
    robust_model_fits = robust_model_fits
)
# results


pretty_n <- function(n){
    return (format(round(as.numeric(n), 2), nsmall = 2))
}

sink("tables/table4.tex", append=FALSE, split=FALSE)
cat("
\\begin{table}[tb!]
\\footnotesize
\\center
\\caption{Regression of Simulated Downgrades on Emissions}
\\label{tab:tab4}
\\begin{tabularx}{\\textwidth}{X X X X X}
\\hline
Scenario & $\\beta$ & t-value & t-value (robust) & $R^{2}$ \\\\
\\hline
Below 2$^\\circ$C  & ",pretty_n(results[1,2])," & ",pretty_n(results[1,3])," & ",pretty_n(results[1,5])," & ",pretty_n(results[1,6]),"\\\\
Delayed Transition  & ",pretty_n(results[2,2])," & ",pretty_n(results[2,3])," & ",pretty_n(results[2,5])," & ",pretty_n(results[2,6]),"\\\\
Fragmented World  & ",pretty_n(results[3,2])," & ",pretty_n(results[3,3])," & ",pretty_n(results[3,5])," & ",pretty_n(results[3,6]),"\\\\
NDCs & ",pretty_n(results[4,2])," & ",pretty_n(results[4,3])," & ",pretty_n(results[4,5])," & ",pretty_n(results[4,6]),"\\\\
Net Zero 2050 & ",pretty_n(results[5,2])," & ",pretty_n(results[5,3])," & ",pretty_n(results[5,5])," & ",pretty_n(results[5,6]),"\\\\
\\hline
\\multicolumn{5}{p{\\textwidth}}{\\begin{footnotesize}This table shows the results of our regression of simulated downgrades on standardized log total emissions ($\\Delta Rating_{i} = \\beta lnEmissions_{i} + \\mu_{i}$) for 2020. We do this for each of the five scenarios. Column 2 reveals the $\\beta$ estimate for the regression, Columns 3 and 4 show the t-value and robust t-value respectively, and Column 5 shows the model fit.
\\end{footnotesize}
}
\\end{tabularx}
\\end{table}
")
sink()
