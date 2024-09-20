df1 <- read.csv("output/all_scenarios_ratings_results.csv")
df2 <- read.csv("output/all_scenarios_gdp_results_imf.csv")

c1 <- wes_palette("Zissou1", 5, type="discrete")[1]
c2 <- wes_palette("Zissou1", 5, type="discrete")[2]
c3 <- wes_palette("Zissou1", 5, type="discrete")[3]
c4 <- wes_palette("Zissou1", 5, type="discrete")[4]
c5 <- wes_palette("Zissou1", 5, type="discrete")[5]

FONT = 21
ALPHA=1

df1 <- dplyr::filter(
    df1,
    year==30
)

df2 <- dplyr::select(
    df2,
    c(
        Model,
        Scenario,
        Region,
        X2050
    )
)



# df2_lollipop <- df2 %>% pivot_longer(
#     cols = starts_with("X"),
#     names_to = "year",
#     values_to = "gdp"
# )
df2 <- df2[complete.cases(df2),]

# Step 1: Remove the leading 'X'
# df2_lollipop$year <- sub("^X", "", df2_lollipop$year)

# Step 2: Convert to numeric (if you want it as a number)
# df2_lollipop$year <- as.numeric(df2_lollipop$year)

# df2_lollipop <- df2_lollipop %>% select(
#     !c(
#         NA.
#     )
# )



#### Accuracy plot
produce_adjusted_ratings <- function(model, df){
	pred <- predict(model, df, type="se")
	est <- pred$predictions
	se <- pred$se
	# actual <- produce_baseline_estimate(model, baseline)
	actual <- df$scale20
	# actual <- act
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
df1 <- read.csv("output/all_scenarios_ratings_results.csv")
countries <- unique(df1$country)
df1 <- produce_adjusted_ratings(model.forest, baseline)
fig <- df1 %>% 
    dplyr::filter(country %in% countries) %>%
    rowwise() %>% 
    mutate( mymean = mean(c(actual, est) )) %>% 
    arrange(mymean) %>% 
    mutate(country=factor(country, country))
ggplot(fig, aes(country)) +
    geom_segment(aes(x=country, xend=country, y=5, yend=est_upper), width = 0.3, color="grey", lineend="round") +
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
ggsave("plots/accuracy_lollipop.png", dpi=300, width=12, height=10, units="in")


# Histogram and boxplot
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
ggsave("plots/av_rating_downgrade.png", dpi=300, width=12, height=8, units="in")



# Big GDP lollipop
fig <- df2 %>% 
    dplyr::mutate(
        country = countrycode(
            Region,
            origin = "iso3c",
            destination = "country.name"
        )
    ) %>%
    dplyr::mutate(
        country = if_else(country == "Hong Kong SAR China", "Hong Kong", country)
    # ) %>%
    # dplyr::filter(
    #     (Scenario == "Fragmented World" | Scenario == "Net Zero 2050") &
    #     (year == 2050)
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
            legend.position.inside = c(0.89,0.3),
            axis.text.y = element_text(color="black"),
            axis.text.x = element_text(color="#989898")
        ) +
        scale_color_manual(
            name = "Scenario",
            # values=c(c5, c1),
            labels = c("Net Zero 2050","Below 2", "NDCs", "Delayed Transition", "Fragmented World"),
            values=c(c1, c2, c3, c4, c5),
            # labels = unique(fig$Scenario)
        ) +
        ylab("") +
        theme(text=element_text(size=FONT)) +
        geom_vline(xintercept = 0, linetype="dashed") +
        xlab("GDP compared to baseline (%)")
ggsave("plots/big_gdp_lollipop.png", dpi=300, width=12, height=10, units="in")

# rating lollipop
df1 <- read.csv("output/all_scenarios_ratings_results.csv")
df1 <- dplyr::filter(
    df1,
    year==30
)
df3 <- df1 %>%
    select(
        country,
        est = actual,
    ) %>% 
    mutate(
        scenario_ = "Actual"
    ) %>% distinct()
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
    ) 
fig <- fig %>% select(
    country, est, scenario_
)
x <-  produce_adjusted_ratings(model.forest, baseline)
x <- x %>% select(
    country, est_lower, est_upper
)
fig <- rbind(fig,df3)
fig <- inner_join(
    fig,
    x,
    by=c("country")
)

#### test

y <- fig %>% dplyr::filter(
    scenario_ == "Actual"
)
y$test <- ifelse(
    (y$est > y$est_lower) & (y$est < y$est_upper),
    1,
    0
)

x <-  produce_adjusted_ratings(model.forest, baseline)
x$test <- ifelse((x$actual > x$est_lower) & (x$actual < x$est_upper), 1, 0) 

x[x$country=="United Kingdom",]
y[y$country=="United Kingdom",]

####

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

fig <- fig %>% dplyr::filter(
    scenario_ %in% c("Actual", "Fragmented World", "Net Zero 2050")
)

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
            # labels = c("Actual", "Net Zero 2050","Below 2", "NDCs", "Delayed Transition", "Fragmented World"),
            # values=c("grey", c1, c2, c3, c4, c5)) +
            labels = c("Actual", "Net Zero 2050","Fragmented World"),
            values=c("grey", c1, c5)) +
        ylab("") +
        theme(text=element_text(size=FONT)) +
        xlab("Credit Rating (20-point scale)")
ggsave("plots/big_rating_lollipop.png", dpi=300, width=12, height=10, units="in")













# rating lollipop
df1 <- read.csv("output/all_scenarios_ratings_results.csv")
df1 <- dplyr::filter(
    df1,
    year==30
)
df3 <- df1 %>%
    select(
        country,
        est = actual,
    ) %>% 
    mutate(
        scenario_ = "Actual"
    ) %>% distinct()
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
    ) 
fig <- fig %>% select(
    country, est, scenario_
)
x <-  produce_adjusted_ratings(model.forest, baseline)
x <- x %>% select(
    country, est_lower, est_upper
)
fig <- rbind(fig,df3)
fig <- inner_join(
    fig,
    x,
    by=c("country")
)

#### test

y <- fig %>% dplyr::filter(
    scenario_ == "Actual"
)
y$test <- ifelse(
    (y$est > y$est_lower) & (y$est < y$est_upper),
    1,
    0
)

x <-  produce_adjusted_ratings(model.forest, baseline)
x$test <- ifelse((x$actual > x$est_lower) & (x$actual < x$est_upper), 1, 0) 

x[x$country=="United Kingdom",]
y[y$country=="United Kingdom",]

####

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

# fig <- fig %>% dplyr::filter(
#     scenario_ %in% c("Actual", "Fragmented World", "Net Zero 2050")
# )

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
            # labels = c("Actual", "Net Zero 2050","Fragmented World"),
            # values=c("grey", c1, c5)) +
        ylab("") +
        theme(text=element_text(size=FONT)) +
        xlab("Credit Rating (20-point scale)")
ggsave("plots/big_rating_lollipop_appendix.png", dpi=300, width=12, height=10, units="in")
