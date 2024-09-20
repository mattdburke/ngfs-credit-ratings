df1 <- read.csv("output/all_scenarios_ratings_results.csv")

# Table
a <- df1 %>% dplyr::filter(
    year==10
) %>% dplyr::group_by(scenario_) %>%
summarize(
    rating_delta_10 = mean(rating_delta),
    cd_delta_10 = mean(cd_delta),
    pd_delta_10 = mean(pd_delta)
)
c <- df1 %>% dplyr::filter(
    year==30
) %>% dplyr::group_by(scenario_) %>%
summarize(
    rating_delta_30 = mean(rating_delta),
    cd_delta_30 = mean(cd_delta),
    pd_delta_30 = mean(pd_delta)
)

# T test for statement in footer.
for (i in unique(df1$scenario_)){
    x <- df1 %>% dplyr::filter(year==30 & scenario_==i)
    print (t.test(x$rating_delta))
}

get_dat <- function(row, column, frame){
    return (format(round(as.numeric(frame[row, column]), 2), nsmall = 2))
}

sink("tables/benchmark_result_2050.tex", append=FALSE, split=FALSE)

cat("
\\begin{table}[tb!]
\\footnotesize
\\center
\\caption{Sovereign Risk by Scenario}
\\label{tab:SRBS}
\\begin{tabularx}{\\textwidth}{X X X X}
\\hline
Scenario & Rating change (notches) & Cost of debt premium (\\%) & Change to PD (\\%)\\\\
\\hline
\\multicolumn{4}{p{\\textwidth}}{Panel A: 2030}\\\\
\\hline
Below 2\\textdegree C & ", get_dat(1,2,a)," & ", get_dat(1,3,a)," & ", get_dat(1,4,a),"\\\\
Delayed Transition & ", get_dat(2,2,a)," & ", get_dat(2,3,a)," & ", get_dat(2,4,a),"\\\\
Fragmented World & ", get_dat(3,2,a)," & ", get_dat(3,3,a)," & ", get_dat(3,4,a),"\\\\
NDCs & ", get_dat(4,2,a)," & ", get_dat(4,3,a)," & ", get_dat(4,4,a),"\\\\
Net Zero 2050 & ", get_dat(5,2,a)," & ", get_dat(5,3,a)," & ", get_dat(5,4,a),"\\\\
\\hline
\\multicolumn{4}{p{\\textwidth}}{Panel B: 2050}\\\\
\\hline
Below 2\\textdegree C & ", get_dat(1,2,c)," & ", get_dat(1,3,c)," & ", get_dat(1,4,c),"\\\\
Delayed Transition & ", get_dat(2,2,c)," & ", get_dat(2,3,c)," & ", get_dat(2,4,c),"\\\\
Fragmented World & ", get_dat(3,2,c)," & ", get_dat(3,3,c)," & ", get_dat(3,4,c),"\\\\
NDCs & ", get_dat(4,2,c)," & ", get_dat(4,3,c)," & ", get_dat(4,4,c),"\\\\
Net Zero 2050 & ", get_dat(5,2,c)," & ", get_dat(5,3,c)," & ", get_dat(5,4,c),"\\\\
\\hline
\\multicolumn{4}{p{\\textwidth}}{\\begin{footnotesize}This table shows the results of our benchmark simulations. Column 1 highlights the scenario being simulated. Column 2 shows the global average change to the credit rating. Column 3 is the global average change in the premium to cost of debt and Column 4 is the global average change in the probability of default. Each of these grouped downgrades are significant at the 1\\% level in a t-test.
\\end{footnotesize}
}
\\end{tabularx}
\\end{table}
")

sink()








countries <- c("Canada", "France", "Germany", "Italy", "Japan", "United Kingdom", "United States", "India", "China")

scenarios <- c("Net Zero 2050", "Fragmented World", "Delayed transition")

a <- df1 %>% dplyr::filter(
    country %in% countries &
    scenario_ %in% scenarios &
    year==30
) %>% group_by(country, scenario_) %>%
mutate(
    rating_delta = mean(rating_delta),
    cd_delta = mean(cd_delta),
    pd_delta = mean(pd_delta)
) %>% ungroup() %>% dplyr::select(
    country, rating_delta, cd_delta, pd_delta, scenario_
) %>% distinct()



sink("tables/benchmark_result_country.tex", append=FALSE, split=FALSE)

cat("
\\begin{table}[tb!]
\\footnotesize
\\center
\\caption{Sovereign Risk for the G7 plus India and China (2050)}
\\label{tab:SRBC}
\\begin{tabularx}{\\textwidth}{X X X X}
\\hline
Scenario & Rating change (notches) & Cost of debt premium (\\%) & Change to PD (\\%)\\\\
\\hline
\\multicolumn{4}{p{\\textwidth}}{Panel A: Net Zero 2050}\\\\
\\hline
Canada & ", get_dat(19,2,a)," & ", get_dat(19,3,a)," & ", get_dat(19,4,a),"\\\\
China & ", get_dat(20,2,a)," & ", get_dat(20,3,a)," & ", get_dat(20,4,a),"\\\\
France & ", get_dat(21,2,a)," & ", get_dat(21,3,a)," & ", get_dat(21,4,a),"\\\\
Germany & ", get_dat(22,2,a)," & ", get_dat(22,3,a)," & ", get_dat(22,4,a),"\\\\
India & ", get_dat(23,2,a)," & ", get_dat(23,3,a)," & ", get_dat(23,4,a),"\\\\
Italy & ", get_dat(24,2,a)," & ", get_dat(24,3,a)," & ", get_dat(24,4,a),"\\\\
Japan & ", get_dat(25,2,a)," & ", get_dat(25,3,a)," & ", get_dat(25,4,a),"\\\\
United Kingdom & ", get_dat(26,2,a)," & ", get_dat(26,3,a)," & ", get_dat(26,4,a),"\\\\
United States & ", get_dat(27,2,a)," & ", get_dat(27,3,a)," & ", get_dat(27,4,a),"\\\\
\\hline
\\multicolumn{4}{p{\\textwidth}}{Panel B: Delayed Transition}\\\\
\\hline
Canada & ", get_dat(1,2,a)," & ", get_dat(1,3,a)," & ", get_dat(1,4,a),"\\\\
China & ", get_dat(2,2,a)," & ", get_dat(2,3,a)," & ", get_dat(2,4,a),"\\\\
France & ", get_dat(3,2,a)," & ", get_dat(3,3,a)," & ", get_dat(3,4,a),"\\\\
Germany & ", get_dat(4,2,a)," & ", get_dat(4,3,a)," & ", get_dat(4,4,a),"\\\\
India & ", get_dat(5,2,a)," & ", get_dat(5,3,a)," & ", get_dat(5,4,a),"\\\\
Italy & ", get_dat(6,2,a)," & ", get_dat(6,3,a)," & ", get_dat(6,4,a),"\\\\
Japan & ", get_dat(7,2,a)," & ", get_dat(7,3,a)," & ", get_dat(7,4,a),"\\\\
United Kingdom & ", get_dat(8,2,a)," & ", get_dat(8,3,a)," & ", get_dat(8,4,a),"\\\\
United States & ", get_dat(9,2,a)," & ", get_dat(9,3,a)," & ", get_dat(9,4,a),"\\\\
\\hline
\\multicolumn{4}{p{\\textwidth}}{Panel C: Fragmented World}\\\\
\\hline
Canada & ", get_dat(10,2,a)," & ", get_dat(10,3,a)," & ", get_dat(10,4,a),"\\\\
China & ", get_dat(11,2,a)," & ", get_dat(11,3,a)," & ", get_dat(11,4,a),"\\\\
France & ", get_dat(12,2,a)," & ", get_dat(12,3,a)," & ", get_dat(12,4,a),"\\\\
Germany & ", get_dat(13,2,a)," & ", get_dat(13,3,a)," & ", get_dat(13,4,a),"\\\\
India & ", get_dat(14,2,a)," & ", get_dat(14,3,a)," & ", get_dat(14,4,a),"\\\\
Italy & ", get_dat(15,2,a)," & ", get_dat(15,3,a)," & ", get_dat(15,4,a),"\\\\
Japan & ", get_dat(16,2,a)," & ", get_dat(16,3,a)," & ", get_dat(16,4,a),"\\\\
United Kingdom & ", get_dat(17,2,a)," & ", get_dat(17,3,a)," & ", get_dat(17,4,a),"\\\\
United States & ", get_dat(18,2,a)," & ", get_dat(18,3,a)," & ", get_dat(18,4,a),"\\\\
\\hline
\\multicolumn{4}{p{\\textwidth}}{\\begin{footnotesize}This table shows the results of our benchmark simulations. In this table we show across three panels the country-specific results for the G7 + India and China. These results are simulated for 2050. Column 1 highlights the scenario being simulated. Column 2 shows the global average change to the credit rating. Column 3 is the global average change in the premium to cost of debt and Column 4 is the global average change in the probability of default.
\\end{footnotesize}
}
\\end{tabularx}
\\end{table}
")

sink()



df1 <- read.csv("cleandata/baseline_data_clean.csv")

pretty_n <- function(n){
    return (format(round(as.numeric(n), 2), nsmall = 2))
}

sink("tables/descriptives.tex", append=FALSE, split=FALSE)

cat("
\\begin{table}[tb!]
\\footnotesize
\\center
\\caption{Descriptive Statistics}
\\label{tab:D}
\\begin{tabularx}{\\textwidth}{p{7cm} X X X X}
\\hline
Variable & Mean & Standard deviation & Minimun & Maximum\\\\
\\hline
Rating & ", pretty_n(mean(df1$scale20))," & ", pretty_n(sd(df1$scale20))," & ", pretty_n(min(df1$scale20))," & ", pretty_n(max(df1$scale20)),"\\\\
ln GDP per capita & ", pretty_n(mean(df1$ln_S_GDPpercapitaUS))," & ", pretty_n(sd(df1$ln_S_GDPpercapitaUS))," & ", pretty_n(min(df1$ln_S_GDPpercapitaUS))," & ", pretty_n(max(df1$ln_S_GDPpercapitaUS)),"\\\\
GDP growth & ", pretty_n(mean(df1$S_RealGDPgrowth))," & ", pretty_n(sd(df1$S_RealGDPgrowth))," & ", pretty_n(min(df1$S_RealGDPgrowth))," & ", pretty_n(max(df1$S_RealGDPgrowth)),"\\\\
Net General Government Debt / GDP & ", pretty_n(mean(df1$S_NetGGdebtGDP))," & ", pretty_n(sd(df1$S_NetGGdebtGDP))," & ", pretty_n(min(df1$S_NetGGdebtGDP))," & ", pretty_n(max(df1$S_NetGGdebtGDP)),"\\\\
General Government Balance / GDP & ", pretty_n(mean(df1$S_GGbalanceGDP))," & ", pretty_n(sd(df1$S_GGbalanceGDP))," & ", pretty_n(min(df1$S_GGbalanceGDP))," & ", pretty_n(max(df1$S_GGbalanceGDP)),"\\\\
Narrow Net External debt / CARs & ", pretty_n(mean(df1$S_NarrownetextdebtCARs))," & ", pretty_n(sd(df1$S_NarrownetextdebtCARs))," & ", pretty_n(min(df1$S_NarrownetextdebtCARs))," & ", pretty_n(max(df1$S_NarrownetextdebtCARs)),"\\\\
Current Account Balance / GDP & ", pretty_n(mean(df1$S_CurrentaccountbalanceGDP))," & ", pretty_n(sd(df1$S_CurrentaccountbalanceGDP))," & ", pretty_n(min(df1$S_CurrentaccountbalanceGDP))," & ", pretty_n(max(df1$S_CurrentaccountbalanceGDP)),"\\\\
\\hline
\\multicolumn{5}{p{\\textwidth}}{\\begin{footnotesize}This table shows the descriptive statistics for our model training data. This ranges from 2015 to 2020, for 123 different countries and a total number of observation equalling 723. The sovereign credit rating variable is on the 20-notch scale, where AAA=20. 
\\end{footnotesize}
}
\\end{tabularx}
\\end{table}
")

sink()













# Table 1
df2 <- read.csv("output/all_scenarios_gdp_results.csv")

get_filtered_gdp_frame <- function(model, physical_risk, year){
    dataframe <- df2 %>% dplyr::filter(
        grepl(model, scenario) &
        grepl(physical_risk, scenario)
    ) %>% dplyr::select(
        c(contains(year), scenario, Region)
    ) %>% mutate(
        scenario = sub("_.*", "", scenario)
    )
    colnames(dataframe)<-c("GDPloss", "scenario", "Region")   
    return (dataframe) 
}

#         X2050 scenario Region
# 1 -0.06208460 Below 2C    AGO
# 2 -0.03533077 Below 2C    ALB
# 3 -0.09318745 Below 2C    ARE
# 4  0.01868413 Below 2C    ARG
# 5 -0.02340845 Below 2C    ARM
# 6 -0.03367096 Below 2C    AUS

get_filtered_gdp_frame("MESSAGE", "high", "50")

get_stat <- function(frame){
    frame %>% mutate(
        GDPloss = GDPloss*-100
    ) %>% select(
        -c(Region)
    ) %>% group_by(
        scenario
        ) %>% summarise_all(
            list(
            mean,
            sd,
            max,
            min
        ))
}


x <- get_stat(get_filtered_gdp_frame("MESSAGE", "high", "50"))
x <- rbind(x, get_stat(get_filtered_gdp_frame("GCAM", "high", "50")))
x <- rbind(x, get_stat(get_filtered_gdp_frame("REMIND", "high", "50")))

colnames(x) <- c("Scenario", "Mean", "St.Dev", "Min", "Max")
x$Mean <- x$Mean
x$Min <- x$Min
x$Max <- x$Max
x$Max <- x$St.Dev
x <- x %>% mutate_at(
    vars(Mean, St.Dev, Min, Max), 
    funs(format(round(., 2), nsmall=2)))

insertDF <- as.data.frame(matrix(c("MESSAGEixGLOBIOM 11MR12", "", "", "", ""),ncol=5))
x <-insertRows(x, 1 , new = insertDF)
insertDF <- as.data.frame(matrix(c("GCAM 60 NGFS", "", "", "", ""),ncol=5))
x<-insertRows(x, 8 , new = insertDF)
insertDF <- as.data.frame(matrix(c("REMINDMAgPIE 3246", "", "", "", ""),ncol=5))
x<-insertRows(x, 16 , new = insertDF)
as.data.frame(x)

write.csv(x, "tables/table1.csv", row.names=FALSE, quote = FALSE)
x <- print(xtable(x, include.rownames=FALSE))
tmp <- tempfile()
options(xtable.comment=FALSE)  ## removes the nasty comments
capture.output(, file=tmp)
rmarkdown::render(tmp, 
    output_format="pdf_document", 
    output_file="C:/Users/mattb/OneDrive/GitHub/ngfs-credit-ratings/tables/table1.pdf")
unlink(tmp)
bitmap <- pdftools::pdf_render_page("C:/Users/mattb/OneDrive/GitHub/ngfs-credit-ratings/tables/table1.pdf", dpi=300)
png::writePNG(bitmap, "C:/Users/mattb/OneDrive/GitHub/ngfs-credit-ratings/tables/table1.png")


format(round(min(df1[[variable]]), 2), nsmall = 2)





as.data.frame(x %>% group_by(Region) %>% summarize(
    sd_gdp = sd(X2050)
) %>% arrange(-sd_gdp))




















df1 <- read.csv("output/all_scenarios_ratings_results.csv")

G7 <- c(
    "Italy",
    "Australia",
    "Canada",
    "Saudi Arabia",
    "Russia",
    "South Africa",
    "Turkey",
    "Argentina",
    "Brazil",
    "Indonesia",
    "South Korea",
    "France",
    "Spain",
    "Germany",
    "United Kingdom",
    "United States",
    "Japan",
    "China",
    "India",
    "Mexico"
)

df1_summary <- df1 %>% dplyr::filter(
    country %in% G7
    ) %>% mutate(
        model = sub("^[^_]+_([^_]+)_.*", "\\1", scenario),
        scenario = sub("_.*", "", scenario)
        ) %>% group_by(
            scenario, country
        ) %>% summarize(
            actual = mean(actual),
            rating_delta = mean(rating_delta),
            cd_delta = mean(cd_delta),
            pd_delta = mean(pd_delta), .groups = 'drop'
        ) 



sink("tables/test.tex", append=FALSE, split=FALSE)
cat("
\\documentclass[convert]{standalone} 
\\begin{document}    
\\begin{table}[tb]
    \\caption{Summary statistics}
    \\label{tab:summary}
    \\footnotesize
    \\begin{tabularx}{\\textwidth}{X X X X X X}
    \\hline
    \\multicolumn{6}{p{\\textwidth}}{Descriptive statistics}\\\\
    \\hline
    1 & 2 & 3 & 4 & 5 & 6\\
    \\hline
    \\multicolumn{6}{p{\\textwidth}}{\\begin{footnotesize}
    Description here
    \\end{footnotesize}}
    \\end{tabularx}
\\end{table}
\\end{document}
")

sink()
    
##############

















































df1 <- read.csv("output/all_scenarios_gdp_results_imf.csv")
df1 <- df1[complete.cases(df1), ]
# Table
a <- df1  %>% dplyr::group_by(Scenario) %>%
summarize(
    mean_GDP = mean(X2050)
)


c <- df1 %>% dplyr::filter(
    year==30
) %>% dplyr::group_by(scenario_) %>%
summarize(
    rating_delta_30 = mean(rating_delta),
    cd_delta_30 = mean(cd_delta),
    pd_delta_30 = mean(pd_delta)
)

# T test for statement in footer.
for (i in unique(df1$scenario_)){
    x <- df1 %>% dplyr::filter(year==30 & scenario_==i)
    print (t.test(x$rating_delta))
}

get_dat <- function(row, column, frame){
    return (format(round(as.numeric(frame[row, column]), 2), nsmall = 2))
}

sink("tables/benchmark_result_2050.tex", append=FALSE, split=FALSE)

cat("
\\begin{table}[tb!]
\\footnotesize
\\center
\\caption{Sovereign Risk by Scenario}
\\label{tab:SRBS}
\\begin{tabularx}{\\textwidth}{X X X X}
\\hline
Scenario & Rating change (notches) & Cost of debt premium (\\%) & Change to PD (\\%)\\\\
\\hline
\\multicolumn{4}{p{\\textwidth}}{Panel A: 2030}\\\\
\\hline
Below 2\\textdegree C & ", get_dat(1,2,a)," & ", get_dat(1,3,a)," & ", get_dat(1,4,a),"\\\\
Delayed Transition & ", get_dat(2,2,a)," & ", get_dat(2,3,a)," & ", get_dat(2,4,a),"\\\\
Fragmented World & ", get_dat(3,2,a)," & ", get_dat(3,3,a)," & ", get_dat(3,4,a),"\\\\
NDCs & ", get_dat(4,2,a)," & ", get_dat(4,3,a)," & ", get_dat(4,4,a),"\\\\
Net Zero 2050 & ", get_dat(5,2,a)," & ", get_dat(5,3,a)," & ", get_dat(5,4,a),"\\\\
\\hline
\\multicolumn{4}{p{\\textwidth}}{Panel B: 2050}\\\\
\\hline
Below 2\\textdegree C & ", get_dat(1,2,c)," & ", get_dat(1,3,c)," & ", get_dat(1,4,c),"\\\\
Delayed Transition & ", get_dat(2,2,c)," & ", get_dat(2,3,c)," & ", get_dat(2,4,c),"\\\\
Fragmented World & ", get_dat(3,2,c)," & ", get_dat(3,3,c)," & ", get_dat(3,4,c),"\\\\
NDCs & ", get_dat(4,2,c)," & ", get_dat(4,3,c)," & ", get_dat(4,4,c),"\\\\
Net Zero 2050 & ", get_dat(5,2,c)," & ", get_dat(5,3,c)," & ", get_dat(5,4,c),"\\\\
\\hline
\\multicolumn{4}{p{\\textwidth}}{\\begin{footnotesize}This table shows the results of our benchmark simulations. Column 1 highlights the scenario being simulated. Column 2 shows the global average change to the credit rating. Column 3 is the global average change in the premium to cost of debt and Column 4 is the global average change in the probability of default. Each of these grouped downgrades are significant at the 1\\% level in a t-test.
\\end{footnotesize}
}
\\end{tabularx}
\\end{table}
")

sink()


