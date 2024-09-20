df1 <- read.csv("rawdata/NiGEM_data.csv")

####################
### GET MODEL NAME
extract_bracket_text <- function(text) {
  # Regular expression to match text within square brackets
  pattern <- "\\[(.*?)\\]"
  
  # Extract the matched text
  matches <- regmatches(text, regexpr(pattern, text))
  
  # Remove the square brackets from the result
  extracted_text <- gsub("\\[|\\]", "", matches)
  
  return(extracted_text)
}
df1$Model <- sapply(df1$Model, extract_bracket_text)


remove_after_backslash <- function(text) {
  
  if (substr(text, 1, 1) == "B") {
  
  # Regular expression to match backslash and any text after it
  pattern <- "\\s.*"
  
  # Replace the matched text with an empty string
  cleaned_text <- sub(pattern, " 2", iconv(text, from = "ISO-8859-1", to = "UTF-8"))
  
  return(cleaned_text)
} else {
    return (text)
}}
remove_after_backslash("Below 2\xb0C")

df1$Scenario <- sapply(df1$Scenario, remove_after_backslash)






##############
extract_country_name <- function(text) {
  # Split the string at the pipe character
  parts <- strsplit(text, "\\|")[[1]]
  # Return the part after the pipe character
  return(parts[2])
}

# Apply the function to the dataframe column
df1$parsed_country <- sapply(df1$Region, extract_country_name)
df1$Region <- countrycode(
    df1$parsed_country,
    origin="country.name",
    destination="iso3c"
)
#####################

df1 <- df1[df1$Variable=="Gross Domestic Product (GDP)(combined)",]
df1 <- df1[df1$Unit=="% difference,  2017 PPP ; US$ Bn",]


# df1 <- read.csv("rawdata/GDP_Losses_%26_Benefits.csv")

df1 <- df1 %>%
# dplyr::filter(
#     Indicator=="Potential National Income Loss From Climate Risks"
# ) %>%
dplyr::select(-c(
    # Region,
    Variable,
    Unit,
    Unit,
    parsed_country)) 

unique_models <- unique(df1$Model)
unique_scenarios <- unique(df1$Scenario)
# unique_variable <- unique(df1$Variable)

for (i in unique_models){
    for (j in unique_scenarios){
        # for (k in unique_variable){

            temp_frame <- df1 %>% dplyr::filter(
                Model==i &
                Scenario==j #&
                # Variable==k 
            ) %>% dplyr::select(
                -c(
                # Variable,
                Model,
                Scenario
                )
            ) %>% mutate(across(X2023:X2050, ~ ./ 100))
            # colnames(temp_frame) <- c("Region", paste0("X", c(2023:2050)))
            temp_frame <- dplyr::select(
                temp_frame,
                Region,
                X2045,
                X2046,
                X2047,
                X2048,
                X2049,
                X2050
            )
            if (nrow(temp_frame)>0){
            write.csv(
                temp_frame,
                paste0(
                    "scenario_data/GDPloss_",
                    i,
                    "_",
                    j,
                    ".csv"
                ), row.names = FALSE)
            }
        }
    }


df1 <- df1 %>% dplyr::mutate(
    X2045 = X2045/100,
    X2046 = X2046/100,
    X2047 = X2047/100,
    X2048 = X2048/100,
    X2049 = X2049/100,
    X2050 = X2050/100
) %>% dplyr::select(
    Model,
    Scenario,
    Region,
    X2045,
    X2046,
    X2047,
    X2048,
    X2049,
    X2050
)

# df1 <- df1 %>% mutate(across(
#                 X2023:X2050,~. /100
#             ))
#             colnames(df1) <- c(
#                 "Model", 
#                 "Scenario",
#                 "Region",
#                 # "X2050")
#                 paste0("X", c(2023:2050)))
write.csv(df1, "output/all_scenarios_gdp_results_imf.csv", row.names=FALSE)



