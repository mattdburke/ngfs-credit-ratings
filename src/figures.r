c1 <- wes_palette("Zissou1", 5, type="discrete")[1]
c2 <- wes_palette("Zissou1", 5, type="discrete")[2]
c3 <- wes_palette("Zissou1", 5, type="discrete")[3]
c4 <- wes_palette("Zissou1", 5, type="discrete")[4]
c5 <- wes_palette("Zissou1", 5, type="discrete")[5]

FONT = 21
ALPHA=1

produce_adjusted_ratings <- function(model, df){
	pred <- predict(model, df, type="se")
	est <- pred$predictions
	se <- pred$se
	actual <- df$scale20
	T <- pred$predictions / se
	P = exp(-0.717*T -0.416*(T^2))
	n = length(df$CountryName)
	DF = n - 3
	crit = tinv(.05, DF)
	est_lower = est + crit*se
	est_upper = est - crit*se
	country <- df$CountryName
	ISO2 <- df$ISO2
	m1 <- cbind(country, ISO2, actual, est, est_lower, est_upper)
	m1 <- do.call(rbind, Map(data.frame, country=country,
		ISO2 = ISO2,
		actual=actual,
		est=est,
		est_lower=est_lower,
		est_upper=est_upper
		))
	m1.sum <- as.data.frame(m1 %>% group_by(country) %>%
		summarise(ISO2 = first(ISO2),
				actual = mean(actual),
				est = mean(est), 
				est_lower = mean(est_lower),
				est_upper = mean(est_upper))) %>% ungroup() %>%
        mutate(
            cd_actual = calculate_spreads(actual),
            pd_actual = implement_PD_equation(actual),
            cd_est = calculate_spreads(est),
            pd_est = implement_PD_equation(est),
            cd_est_95 = calculate_spreads(est_lower),
            pd_est_95 = implement_PD_equation(est_lower),
            rating_delta = est - actual,
            cd_delta = cd_est - cd_actual,
            pd_delta = pd_est - pd_actual,
            rating_delta_95 = est_lower - actual,
            cd_delta_95 = cd_est_95 - cd_actual,
            pd_delta_95 = pd_est_95 - pd_actual
        )
    m1.sum <- m1.sum[!(m1.sum$ISO2 %in% "AR"),]
	return (m1.sum)
}



# Figure 1
baseline <- read.csv("cleandata/baseline_data_clean.csv", header=TRUE)
set.seed(5)
sample = sample.split(baseline$CountryName, SplitRatio = .8)
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
pred <- predict(model.forest, test, type="se")
est <- pred$predictions
se <- pred$se
actual <- test$scale20
T <- pred$predictions / se
P = exp(-0.717*T -0.416*(T^2))
n = length(test$CountryName)
DF = n - 3
crit = tinv(.05, DF)
est_lower = est + crit*se
est_upper = est - crit*se
country <- test$CountryName
ISO2 <- test$ISO2
m1 <- cbind(country, ISO2, actual, est, est_lower, est_upper)
m1 <- do.call(rbind, Map(data.frame, country=country,
	ISO2=ISO2,
	actual=actual,
	est=est,
	est_lower=est_lower,
	est_upper=est_upper
	))
m1$est_l <- m1$est - m1$est_lower
m1$est_u <- m1$est_upper - m1$est
m1.sum <- as.data.frame(m1 %>% group_by(country) %>%
	summarise(ISO2 = ISO2,
			actual = mean(actual),
			est = mean(est),
			est_l = mean(est_l),
			est_u = mean(est_u),
			est_lower=mean(est_lower),
			est_upper=mean(est_upper)))
m1.sum$notch <- round(abs(m1.sum$est - m1.sum$actual))
write.csv(m1.sum, "output/country_level_accuracy.csv")
countries <- unique(read.csv("output/all_scenarios_ratings_results.csv")$country)
fig <- m1.sum %>% 
    dplyr::filter(country %in% countries) %>%
    rowwise() %>% 
    mutate( mymean = mean(c(actual, est) )) %>% 
    arrange(mymean) %>% 
    mutate(country=factor(country, country))
ggplot(fig, aes(country)) +
    geom_segment(aes(x=country, xend=country, y=5, yend=est_upper), color="grey", lineend="round") +
    geom_errorbar(aes(ymin = est_lower, ymax = est_upper), width = 0.3) +
    geom_point( aes(x=country, y=actual, color=factor(2)), size=3 ) +
    geom_point( aes(x=country, y=est, color=factor(3)), size=3 ) +
    coord_flip()+
    theme_classic() +
    theme(
        legend.position = "inside",         
        legend.position.inside = c(0.85,0.2),
        axis.text.y = element_text(color="black"),
        axis.text.x = element_text(color="#989898")
    ) +
    scale_color_manual(
        name = "Rating",
        values=c(c4, c1),
        labels = c("Actual","Simulated")
    ) +
    theme(text=element_text(size=FONT)) +
    xlab("") +
    ylab("Credit Rating (20-point scale)")
ggsave("plots/figure1.png", dpi=300, width=12, height=10, units="in")


# Figure 2
PD_data <- read.csv("rawdata/10_year_default_rate.csv", header = FALSE)
ggplot(PD_data, aes(x=V2, y=V3))+
geom_point(size=2, shape=23) +
theme_classic() +
theme(legend.position="none")+
stat_smooth(method="lm", se=FALSE, formula=y ~ poly(x,5), aes(colour="Quintic Regression", linetype="Non-Linear"))+
labs(
    y = "Probability of Default (%)", 
    caption = "Source: Compiled based on data from Standard & Poor's",
    x="20 point numerical rating (20=AAA)")
ggsave("plots/figure2.png",dpi=300)




# Figure 3
df2 <- read.csv("output/all_scenarios_gdp_results.csv")
df2 <- dplyr::select(
    df2,
    c(
        Model,
        Scenario,
        Region,
        X2050
    )
)
df2 <- df2[complete.cases(df2),]
fig <- df2 %>% dplyr::mutate(
        country = countrycode(
            Region,
            origin = "iso3c",
            destination = "country.name"
        )
    ) %>%
    dplyr::mutate(
        country = if_else(country == "Hong Kong SAR China", "Hong Kong", country)
    ) %>% group_by(
        Scenario,
        country
    ) %>% summarize(
        gdp = mean(X2050) * 100,
        .groups = "drop"
    )
fig$Scenario <- factor(fig$Scenario, 
    levels=c(
        'Net Zero 2050', 
        'Below 2', 
        'Nationally Determined Contributions (NDCs)', 
        'Delayed transition', 
        'Fragmented World'))
fig$country = with(fig, reorder(country, gdp, mean))
    fig %>% 
        ggplot(aes(x=gdp,y=country)) +
        geom_line(aes(group=country), color="#E7E7E7", linewidth=3.5) + 
        geom_point(aes(color=Scenario), size=3) +
        theme_classic() +
        theme(
            legend.position = "inside", 
            legend.position.inside = c(0.74,0.11),
            axis.text.y = element_text(color="black"),
            axis.text.x = element_text(color="#989898")
        ) +
        scale_color_manual(
            name = "Scenario",
            labels = c("Net Zero 2050","Below 2", "NDCs", "Delayed Transition", "Fragmented World"),
            values=c(c1, c2, c3, c4, c5),
        ) +
        ylab("") +
        theme(text=element_text(size=FONT)) +
        geom_vline(xintercept = 0, linetype="dashed") +
        xlab("GDP compared to baseline (%)")
ggsave("plots/figure3.png", dpi=300, width=12, height=10, units="in")


# Figure 4
df1 <- read.csv("output/all_scenarios_ratings_results.csv")
temp <- df1 %>% dplyr::filter(
    year==30
    ) %>% group_by(country, scenario_) %>%
    mutate(
        cd_delta = median(cd_delta),
        pd_delta = median(pd_delta),
        rating_delta = median(rating_delta)
    ) %>% dplyr::select(
        country,
        scenario_,
        pd_delta,
        cd_delta,
        rating_delta
    )
a <- ggplot(temp, aes(rating_delta, fill = scenario_)) +
    geom_boxplot() + 
    scale_fill_manual(
        breaks = c("Net Zero 2050", "Below 2", "Nationally Determined Contributions (NDCs)", "Delayed transition", "Fragmented World"),
        values = c(c1, c2, c3,c4,c5)
        ) + 
        xlim(-8,1.5) +
        labs(x = "",
        y = "", fill = "Scenario") +
        theme_classic() +
        theme(text=element_text(size=FONT))
b <- ggplot(temp, aes(rating_delta, fill = scenario_)) +
    geom_density() + 
    scale_fill_manual(
        breaks = c("Net Zero 2050", "Below 2", "Nationally Determined Contributions (NDCs)", "Delayed transition", "Fragmented World"),
        values = c(c1, c2, c3,c4,c5)
        ) + 
        labs(x = "Sovereign credit downgrade (20-notch scale)",
        y = "", fill = "Scenario") +
        theme_classic() +
        theme(text=element_text(size=FONT))
ggarrange(
    a,
    b,
    ncol=1,
    nrow=2,
    common.legend=TRUE,
    legend="right"
)
ggsave("plots/figure4.png", dpi=300, width=12, height=8, units="in")





# Figure 5
df1 <- read.csv("output/all_scenarios_ratings_results.csv")
df1 <- dplyr::filter(
    df1,
    year==30
)

# x_1 is a frame to get the current rating into the right format to be appended to a long dataframe later.
x_1 <- df1 %>%
    select(
        country,
        est = actual,
    ) %>% 
    mutate(
        scenario_ = "Actual"
    ) %>% distinct()
    
# x_2 gives the whiskers for the prediction in the plot
x_2 <-  produce_adjusted_ratings(model.forest, baseline)
x_2 <- x_2 %>% select(
    country, est_lower, est_upper
)

fig <- df1 %>% 
    select(
        country,
        est,
        model,
        scenario_
    ) %>% group_by(
        country, scenario_
    ) %>% summarise(
        est = median(est),
        .groups = "drop"
    ) %>% select(
        country,est,scenario_
    )

fig <- rbind(fig,x_1)
fig <- inner_join(
    fig,
    x_2,
    by=c("country")
)

df_fw_means <- fig %>%
    group_by(country) %>%
    summarize(mean_fw = mean(est))          
fig$scenario_ <- factor(fig$scenario_, 
    levels=c(
        'Actual',
        'Net Zero 2050', 
        'Below 2', 
        'Nationally Determined Contributions (NDCs)', 
        'Delayed transition', 
        'Fragmented World'))
    fig <- fig %>%
    mutate(country = factor(country, levels = df_fw_means$country[order(df_fw_means$mean_fw)]))
    fig %>% 
        ggplot(aes(x=est,y=country)) +
        geom_line(aes(group=country), color="#E7E7E7", linewidth=3.5) + 
        geom_point(aes(color=scenario_), size=3) +
        geom_errorbar(aes(xmin = est_lower, xmax = est_upper), width = 0.3) +
        theme_classic() +
        theme(
            legend.position = "inside", 
            legend.position.inside = c(0.85,0.2),
            axis.text.y = element_text(color="black"),
            axis.text.x = element_text(color="#989898"),
            ) +
        scale_color_manual(
            name = "Scenario",
            labels = c("Actual", "Net Zero 2050","Below 2", "NDCs", "Delayed Transition", "Fragmented World"),
            values=c("grey", c1, c2, c3, c4, c5)) +
        ylab("") +
        theme(text=element_text(size=FONT)) +
        xlab("Credit Rating (20-point scale)")
ggsave("plots/figure5.png", dpi=300, width=12, height=10, units="in")
