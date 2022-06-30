downloadDem <- function(data_source, fh_data = "FIH_hist", dd_data = "col_sheet", remove_file = TRUE) {
  # Requires xlsx, foreign
  # data_source <- c("FH", "VDem", "PolityV")
  # fh_data <- c("FIH_hist","FIH_recent")

  # Freedom House
  if(tolower(data_source) == "fh") {
    if(fh_data == "FIH_hist") {
      download.file(url = "https://freedomhouse.org/sites/default/files/2021-02/Country_and_Territory_Ratings_and_Statuses_FIW1973-2021.xlsx", destfile = "fh.xlsx", method = "curl")

      temp <- read.xlsx(file = "fh.xlsx", sheetIndex = 2, startRow = 3)

      colnames(temp) <- c("country", apply(expand.grid(c("PR","CL", "Status"), c(1972:1981,1983:2020)), 1, paste, collapse="."))
    }

    if(fh_data == "FIH_recent") {
      download.file(url = "https://freedomhouse.org/sites/default/files/2021-02/All_data_FIW_2013-2021.xlsx", destfile = "fh.xlsx", method = "curl")

      temp <- read.xlsx(file = "fh.xlsx", sheetIndex = 2, startRow = 2)
    }

    if(fh_data == "FIH_aggregate") {
      download.file(url = "https://freedomhouse.org/sites/default/files/2021-02/Aggregate_Category_and_Subcategory_Scores_FIW_2003-2021.xlsx", destfile = "fh.xlsx", method = "curl")

      temp <- read.xlsx(file = "fh.xlsx", sheetIndex = 2)
    }

    if(remove_file == TRUE) {
      file.remove("fh.xlsx")
    }
  }

  # V-Dem
  if(tolower(data_source) == "vdem") {
    download.file(url = "https://v-dem.net/media/datasets/Country_Year_V-Dem_Full_others_R_v11.1.zip", destfile = "vdem.zip", method = "curl")
    unzip("vdem.zip")

    temp <- readRDS("Country_Year_V-Dem_Full+others_R_v11.1/V-Dem-CY-Full+Others-v11.1.rds")

    if(remove_file == TRUE) {
      unlink("Country_Year_V-Dem_Full+others_R_v11.1/", recursive = TRUE)
      file.remove("vdem.zip")
    }
  }

  # PolityV
  if(tolower(data_source) == "polityv") {
    download.file(url = "http://www.systemicpeace.org/inscr/p5v2018.sav", destfile = "polityV.sav", method = "curl")

    temp <- read.spss("polityV.sav", to.data.frame = TRUE)

    if(remove_file == TRUE) {
      file.remove("polityV.sav")
    }
  }

  # UDS
  if(tolower(data_source) == "uds") {
    download.file(url = "http://www.unified-democracy-scores.net/files/20140312/z/uds_summary.csv.gz", destfile = "uds_summary.csv.gz")

    temp <- read.table("uds_summary.csv.gz", header = T, sep = ",")

    if(remove_file == TRUE) {
      file.remove("uds_summary.csv.gz")
    }
  }

  # DD
  if(tolower(data_source) == "dd") {

    download.file(url = "http://www.christianbjoernskov.com/wp-content/uploads/2020/09/Bj%C3%B8rnskov-Rode-integrated-dataset-v3.2.xlsx", destfile = "dd.xlsx", method = "curl")

    if(dd_data == "reg_char") {
      temp <- read_excel(path = "dd.xlsx", sheet = 1)
    } else {
      temp <- read_excel(path = "dd.xlsx", sheet = 3)
    }

    if(remove_file == TRUE) {
      file.remove("dd.xlsx")
    }
  }

  return(temp)

}


intlOrgDfCreate <- function(orgList) {
  dfFinal <- data.frame()
  for(i in 1:length(orgList)){
    temp <- as.data.frame(orgList[[i]], col.names = c("iso3c", "ascession", "withdrawal"))
    dfFinal <- rbind(dfFinal, temp)
  }
  return(dfFinal)
}

intlOrgGraphDf <- function(orgListName) {
  orgList <- orgListName$values

  IntlOrg_df <- intlOrgDfCreate(orgList)

  if(deparse(substitute(orgListName)) == "EU") {
    IntlOrg_df <- IntlOrg_df %>%
      mutate(wave =
               ifelse(
                 ascession < 1973, 1,
                 ifelse(ascession %in% c(1973:1986), 2, 3)
               )
      )
  }

  vdem <- demMA::downloadDem(data_source = "vdem") %>%
    select(country_name, country_id, country_text_id, year, v2x_polyarchy) %>%
    mutate(iso3c = countrycode(sourcevar = country_id, origin = "vdem", destination = "iso3c", warn = F)) %>%
    filter(year>=min(IntlOrg_df$ascession), iso3c %in% IntlOrg_df$iso3c) %>%
    select(country_name,iso3c,year,v2x_polyarchy)

  dfFinal<- data.frame()
  for(i in 1:length(IntlOrg_df$iso3c)) {
    year_start <- IntlOrg_df[i,"ascession"]
    if(is.na(IntlOrg_df[i,"withdrawal"])) {
      years <- c(year_start:2020)
    } else{
      years <- c(year_start:IntlOrg_df[i,"withdrawal"])
    }
    if("wave" %in% colnames(IntlOrg_df)) {
      tempdf <- data.frame(iso3c = rep(IntlOrg_df$iso3c[i], length(years)), year = years, wave = rep(IntlOrg_df$wave[i], length(years)))
    } else {
      tempdf <- data.frame(iso3c = rep(IntlOrg_df$iso3c[i], length(years)), year = years)
    }
    dfFinal <- rbind(dfFinal,tempdf)
  }

  dfFinal <- dfFinal %>%
    left_join(vdem)

  return(dfFinal)
}

print_progress <- function(c_state, max) {
  print(paste(round((c_state/max)*100, digits = 2), "%"))
}

SVM_index <- function(svm_dataframe, id_vars, support_vars, svm_data_vars, priming_selec, save_path = "") {

  var_SVM <- c(id_vars,support_vars, svm_data_vars)

  priming_df <- svm_dataframe %>%
    filter(iso_year %in% priming_selec$iso_year) %>%
    select(-all_of(c(id_vars,support_vars)))

  result_full_df <- svm_dataframe %>% select(all_of(id_vars))

  for(i in 1:2000) {
    sample <- sample.split(1:nrow(priming_df), SplitRatio = runif(1, min=0.2, max=0.5))

    train <- subset(priming_df, sample==TRUE)
    test <- subset(priming_df, sample==FALSE)

    train_x <- train %>%
      select(-v2x_polyarchy)

    train_y <- train[,"v2x_polyarchy"]

    model1 <- svm(x = train_x, y = train_y, scale = TRUE, kernel = "radial", cost = 1, na.action = na.omit, epsilon = 0.025)

    res1 <- predict(model1, svm_df %>% select(-all_of(c(id_vars,support_vars, "v2x_polyarchy"))))

    result_full_df <- cbind(result_full_df, res1)

    colnames(result_full_df)[ncol(result_full_df)] <- paste("iter",i,sep = "_")

    print_progress(i, 2000)
  }

  result_output_df <- svm_dataframe %>% select(all_of(c(id_vars, "v2x_polyarchy")))

  temp_output_df <- data.frame()

  for(j in 1:nrow(result_full_df)) {
    res_vec <- as.numeric(result_full_df[j,7:ncol(result_full_df)])

    temp_obs_df <- data.frame(med = median(res_vec), sd = sd(res_vec), t(quantile(res_vec, probs = seq(from = 0, to = 1, by = 0.01))))

    colnames(temp_obs_df)[3:103] <- paste(seq(from = 0, to = 100, by = 1), "p", sep = "")

    temp_output_df <- rbind(temp_output_df, temp_obs_df)

    print_progress(j, nrow(result_full_df))
  }

  result_output_df <- cbind(result_output_df, temp_output_df)

  result_SVM <- list(
    Full_Result = result_full_df,
    Output = result_output_df
  )

  if (save_path != "") {
    saveRDS(result_SVM, save_path)
  }

  return(result_SVM)
}
