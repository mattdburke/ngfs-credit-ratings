# Table 1
df1 <- read.csv("cleandata/baseline_data_clean.csv")
pretty_n <- function(n){
    return (format(round(as.numeric(n), 2), nsmall = 2))
}
sink("tables/table1.tex", append=FALSE, split=FALSE)
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


df2 <- read.csv("output/all_scenarios_gdp_results.csv")
df2 <- df2[complete.cases(df2), ]
df2 <- df2 %>% group_by(
    Scenario
) %>% summarise(
    round(mean(X2050)*100, 2)
)
write.csv(df2, "tables/table2.csv", row.names=FALSE)


df1 <- read.csv("output/all_scenarios_ratings_results.csv")
df1 <- df1 %>% dplyr::group_by(scenario_) %>%
summarize(
    rating_delta_30 = round(mean(rating_delta),2),
    cd_delta_30 = round(mean(cd_delta),2),
    pd_delta_30 = round(mean(pd_delta),2)
)
write.csv(df1, "tables/table3.csv", row.names=FALSE)


# Not in final paper
baseline <- read.csv("cleandata/baseline_data_clean.csv", header=TRUE)
set.seed(5)
sample = sample.split(baseline$CountryName, SplitRatio = .7)
train = subset(baseline, sample == TRUE)
test  = subset(baseline, sample == FALSE)
set.seed(77)
model.forest <- ranger(scale20 ~
	ln_S_GDPpercapitaUS +
	S_RealGDPgrowth +
	S_NetGGdebtGDP +
	S_GGbalanceGDP +
	S_NarrownetextdebtCARs +
	S_CurrentaccountbalanceGDP,
	data=train,
	num.trees=2000,
	importance='permutation',
	write.forest = TRUE,
	keep.inbag=TRUE)
pred <- round(predict(model.forest, test, type="se")$predictions)
actual <- test$scale20
acc <- actual - pred
a <- table(acc)
a1 <- a[names(a)==0]/length(acc)
a2 <- (a[names(a)==-1]+a[names(a)==0]+a[names(a)==1])/length(acc)
a3 <- (a[names(a)==-2]+a[names(a)==-1]+a[names(a)==0]+a[names(a)==1]+a[names(a)==2])/length(acc)
a4 <- (a[names(a)==-3]+a[names(a)==-2]+a[names(a)==-1]+a[names(a)==0]+a[names(a)==1]+a[names(a)==2]+a[names(a)==3])/length(acc)
acc_table <- cbind(c(a1, a2, a3, a4))