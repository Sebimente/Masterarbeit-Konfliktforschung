#                         Library 
install.packages("ggdag")
install.packages("lavaan",dependencies = TRUE)
install.packages("InvariantCausalPrediction")
install.packages("nonlinearICP",dependencies = TRUE)
install.packages("CompareCausalNetworks")
install.packages("backShift")
install.packages("pcalg")
install.packages("RBGL")
install.packages("graph",dependencies = TRUE)
install.packages("semPlot")
install.packages("randomForest",dependencies = TRUE)
install.packages(urlPackage, repos=NULL, type="source")## geht nicht 'NAMESPACE' file ist benötigt
install.packages("devtools")
install.packages("cartography")
install.packages("rnaturalearthdata")
install.packages("rnaturalearth")
install.packages("tidyverse")
install.packages("psy")
install.packages("nFactors")
install.packages("ggpubr")
install.packages("stargazer")
install.packages("lmtest")
install.packages("rcompanion")
install.packages("pROC")
install.packages("caret")
install.packages("logistf")
install.packages("safeBinaryRegression")
install.packages("rstanarm")
install.packages("statisticalModeling")
devtools::install_github("Laksafoss/ICPSurv")
library(tidyverse)
library(tidyr)
library(dplyr)
library(readr)
library(ggplot2)
library(data.table)
library(readxl)
library(scales)
library(rnaturalearthdata)
library(rnaturalearth)
library(sf)
library(cartography)
library(ggdag)
library(dagitty)
library(lavaan)
library(InvariantCausalPrediction)
library(CompareCausalNetworks)
library(semPlot)
library(nonlinearICP)
library(randomForest)
library(psych)
library(psy)
library(nFactors)
library(missMDA)
library(VIM)
library(FactoMineR)
library(naniar)
library(ggpubr)
library(car)
library(stargazer)
library(lmtest)
library(rcompanion)
library(pROC)
library(sandwich)
library(caret)
library(logistf)
library(devtools)
library(ICPSurv)
library(arm)
library(rstanarm)
#library(statisticalModeling)
library(corrplot)
#library(safeBinaryRegression) ### keine logistische Regression funktioniert wenn das Package aktiviert ist
###################################### Datensätz einlesen
wdi_2611 <- read_csv("assets/wdi_2611.csv")
DemocracyMatrix_v4 <- read_excel("assets/DemocracyMatrix_v4.xlsx")
ucdp_dpc <- read_excel("assets/ucdp_dpc.xlsx")
Human_Development_Index_HDI_ <- read_excel("assets/Human Development Index (HDI).xlsx", 
                                           sheet = "Human Development Index (HD (2)")
Gender_Inequality_Index <- read_excel("assets/Gender_Inequality_Index.xlsx")
Education_index <- read_excel("assets/Education index.xlsx")
Income_Index <- read_excel("assets/Income Indec GNI.xlsx")
Investment_Index <- read_excel("assets/Investment Index.xlsx")
Governance_Indicators <- read_csv("assets/Governance Indicators.csv")
Inequality_adjHDI <- read_excel("assets/Inequality_adjHDI.xlsx")
Gender_development_Index <- read_excel("assets/Gender_development_Index.xlsx")
Inequality_Coef <- read_excel("assets/Inequality_Coef.xlsx")
In_Ex <- read_excel("assets/In_Ex.xlsx")
Demographie <- read_excel("assets/Demographie.xlsx")
Meta26_11_Excel <- read_excel("assets/Meta26_11_Excel.xlsx")
Female_in_par <- read_excel("assets/Female_in_par.xlsx")
Export1803 <- read_csv("assets/Export1803.csv")
Export2203 <- read_csv("assets/Export2203.csv")
Export2403 <- read_csv("assets/Export2403.csv")
Export2403_2 <- read_csv("assets/Export2403_2.csv")
Export2603 <- read_csv("assets/Export2603.csv")

Labor_force_participation_rate_for_ages_15_24_male_ <- read_csv("assets/Labor force participation rate for ages 15-24, male (%).csv")

Estimated_gross_national_income_per_capita_male_2017_PPP_ <- read_delim("assets/Estimated gross national income per capita, male (2017 PPP $).csv", 
                                                                        delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  dplyr::select_if(is.character)%>%
  dplyr::select(-one_of("Column28"))

Estimated_gross_national_income_per_capita_female_2017_PPP_ <- read_delim("assets/Estimated gross national income per capita, female (2017 PPP $).csv", 
                                                                          delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  dplyr::select_if(is.character)%>%
  dplyr::select(-one_of("Column28"))

Adolescent_birth_rate_births_per_1_000_women_ages_15_19_ <- read_delim("assets/Adolescent birth rate (births per 1,000 women ages 15-19).csv", 
                                                                       delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  dplyr::select(-one_of("Column1","Column14","Column16","Column18","Column20","Column24","Column26","Column28", "Column30"))%>%
  dplyr::select_if(is.character)


Expected_years_of_schooling_years_ <- read_delim("assets/Expected years of schooling (years).csv", 
                                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  dplyr::select(-one_of("_60","_58","_56","_54","_52","_50","_48","_46","_44","_42","_40","_38","_36","_34","_32","_30","_28","_26","_24","_22","_20","_18","_16","_14","_12","_10","_8","_6","_4","_2"))

Expected_years_of_schooling_female_years_ <- read_delim("assets/Expected years of schooling, female (years).csv", 
                                                        delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  dplyr::select_if(is.character)%>%
  dplyr::select(-one_of("Column30"))

Expected_years_of_schooling_male_years_ <- read_delim("assets/Expected years of schooling, male (years).csv", 
                                                      delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  dplyr::select_if(is.character)%>%
  dplyr::select(-one_of("Column30"))

Labour_force_participation_rate_ages_15_and_older_female <- read_delim("assets/Labour force participation rate (% ages 15 and older), female.csv", 
                                                                       delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  dplyr::select_if(is.character)%>%
  dplyr::select(-one_of("Column30"))

Labour_force_participation_rate_ages_15_and_older_male <- read_delim("assets/Labour force participation rate (% ages 15 and older), male.csv", 
                                                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  dplyr::select_if(is.character)%>%
  dplyr::select(-one_of("Column30"))

Labour_force_participation_rate_ages_15_and_older_ <- read_delim("assets/Labour force participation rate (% ages 15 and older).csv", 
                                                                 delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  dplyr::select_if(is.character)%>%
  dplyr::select(-one_of("Column30"))

Life_expectancy_at_birth_years_ <- read_delim("assets/Life expectancy at birth (years).csv", 
                                              delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  dplyr::select_if(is.character)%>%
  dplyr::select(-one_of("_60"))

Life_expectancy_at_birth_female_years_ <- read_delim("assets/Life expectancy at birth, female (years).csv", 
                                                     delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  dplyr::select_if(is.character)

Life_expectancy_at_birth_male_years_ <- read_delim("assets/Life expectancy at birth, male (years).csv", 
                                                   delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  dplyr::select_if(is.character)

Maternal_mortality_ratio_deaths_per_100_000_live_births_ <- read_delim("assets/Maternal mortality ratio (deaths per 100,000 live births).csv", 
                                                                       delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  dplyr::select_if(is.character)%>%
  dplyr::select(-one_of("Column26"))

Mean_years_of_schooling_years_ <- read_delim("assets/Mean years of schooling (years).csv", 
                                             delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  dplyr::select_if(is.character)%>%
  dplyr::select(-one_of("Column62"))

Mean_years_of_schooling_female_years_ <- read_delim("assets/Mean years of schooling, female (years).csv", 
                                                    delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  dplyr::select_if(is.character)%>%
  dplyr::select(-one_of("Column30"))

Mean_years_of_schooling_male_years_ <- read_delim("assets/Mean years of schooling, male (years).csv", 
                                                  delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  dplyr::select_if(is.character)%>%
  dplyr::select(-one_of("Column30"))

Population_with_at_least_some_secondary_education_ages_25_and_older_ <- read_delim("assets/Population with at least some secondary education (% ages 25 and older).csv", 
                                                                                   delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  dplyr::select_if(is.character)%>%
  dplyr::select(-one_of("Column30"))

Population_with_at_least_some_secondary_education_female_ages_25_and_older_ <- read_delim("assets/Population with at least some secondary education, female (% ages 25 and older).csv", 
                                                                                          delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  dplyr::select_if(is.character)%>%
  dplyr::select(-one_of("Column28","Column30"))

Population_with_at_least_some_secondary_education_male_ages_25_and_older_ <- read_delim("assets/Population with at least some secondary education, male (% ages 25 and older).csv", 
                                                                                        delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  dplyr::select_if(is.character)%>%
  dplyr::select(-one_of("Column28","Column30"))

Total_unemployment_rate_female_to_male_ratio_ <- read_delim("assets/Total unemployment rate (female to male ratio).csv", 
                                                            delim = ";", escape_double = FALSE, trim_ws = TRUE)%>%
  dplyr::select_if(is.character)

###################################### erste Spalten als Spaltenname. Einige Datensätze müssen formatiert werden

Adolescent_birth_rate_births_per_1_000_women_ages_15_19_[1,]<-Adolescent_birth_rate_births_per_1_000_women_ages_15_19_[194,]
Adolescent_birth_rate_births_per_1_000_women_ages_15_19_<-Adolescent_birth_rate_births_per_1_000_women_ages_15_19_[-194,]

colnames(Adolescent_birth_rate_births_per_1_000_women_ages_15_19_)<- Adolescent_birth_rate_births_per_1_000_women_ages_15_19_[1,]
Adolescent_birth_rate_births_per_1_000_women_ages_15_19_<- Adolescent_birth_rate_births_per_1_000_women_ages_15_19_[-1,]


colnames(Estimated_gross_national_income_per_capita_male_2017_PPP_)<- Estimated_gross_national_income_per_capita_male_2017_PPP_[1,]
Estimated_gross_national_income_per_capita_male_2017_PPP_<- Estimated_gross_national_income_per_capita_male_2017_PPP_[-1,]

colnames(Estimated_gross_national_income_per_capita_female_2017_PPP_)<- Estimated_gross_national_income_per_capita_female_2017_PPP_[1,]
Estimated_gross_national_income_per_capita_female_2017_PPP_<- Estimated_gross_national_income_per_capita_female_2017_PPP_[-1,]

colnames(Expected_years_of_schooling_female_years_)<- Expected_years_of_schooling_female_years_[1,]
Expected_years_of_schooling_female_years_<- Expected_years_of_schooling_female_years_[-1,]

colnames(Expected_years_of_schooling_male_years_)<- Expected_years_of_schooling_male_years_[1,]
Expected_years_of_schooling_male_years_<- Expected_years_of_schooling_male_years_[-1,]


colnames(Expected_years_of_schooling_years_)<- Expected_years_of_schooling_years_[1,]
Expected_years_of_schooling_years_<- Expected_years_of_schooling_years_[-1,]
col.num2<- c("1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019")
Expected_years_of_schooling_years_[col.num2] <- sapply(Expected_years_of_schooling_years_[col.num2],as.character)

colnames(Labour_force_participation_rate_ages_15_and_older_female)<- Labour_force_participation_rate_ages_15_and_older_female[1,]
Labour_force_participation_rate_ages_15_and_older_female<- Labour_force_participation_rate_ages_15_and_older_female[-1,]

colnames(Labour_force_participation_rate_ages_15_and_older_male)<- Labour_force_participation_rate_ages_15_and_older_male[1,]
Labour_force_participation_rate_ages_15_and_older_male<- Labour_force_participation_rate_ages_15_and_older_male[-1,]

colnames(Life_expectancy_at_birth_years_)<- Life_expectancy_at_birth_years_[1,]
Life_expectancy_at_birth_years_<- Life_expectancy_at_birth_years_[-1,]

colnames(Labour_force_participation_rate_ages_15_and_older_)<- Labour_force_participation_rate_ages_15_and_older_[1,]
Labour_force_participation_rate_ages_15_and_older_<- Labour_force_participation_rate_ages_15_and_older_[-1,]

colnames(Life_expectancy_at_birth_female_years_)<- Life_expectancy_at_birth_female_years_[1,]
Life_expectancy_at_birth_female_years_<- Life_expectancy_at_birth_female_years_[-1,]

colnames(Life_expectancy_at_birth_male_years_)<- Life_expectancy_at_birth_male_years_[1,]
Life_expectancy_at_birth_male_years_<- Life_expectancy_at_birth_male_years_[-1,]

colnames(Maternal_mortality_ratio_deaths_per_100_000_live_births_)<- Maternal_mortality_ratio_deaths_per_100_000_live_births_[1,]
Maternal_mortality_ratio_deaths_per_100_000_live_births_<- Maternal_mortality_ratio_deaths_per_100_000_live_births_[-1,]

colnames(Mean_years_of_schooling_years_)<- Mean_years_of_schooling_years_[1,]
Mean_years_of_schooling_years_<- Mean_years_of_schooling_years_[-1,]

colnames(Mean_years_of_schooling_female_years_)<- Mean_years_of_schooling_female_years_[1,]
Mean_years_of_schooling_female_years_<- Mean_years_of_schooling_female_years_[-1,]

colnames(Mean_years_of_schooling_male_years_)<- Mean_years_of_schooling_male_years_[1,]
Mean_years_of_schooling_male_years_<- Mean_years_of_schooling_male_years_[-1,]

colnames(Population_with_at_least_some_secondary_education_ages_25_and_older_)<- Population_with_at_least_some_secondary_education_ages_25_and_older_[1,]
Population_with_at_least_some_secondary_education_ages_25_and_older_<- Population_with_at_least_some_secondary_education_ages_25_and_older_[-1,]

colnames(Population_with_at_least_some_secondary_education_female_ages_25_and_older_)<- Population_with_at_least_some_secondary_education_female_ages_25_and_older_[1,]
Population_with_at_least_some_secondary_education_female_ages_25_and_older_<- Population_with_at_least_some_secondary_education_female_ages_25_and_older_[-1,]

colnames(Population_with_at_least_some_secondary_education_male_ages_25_and_older_)<- Population_with_at_least_some_secondary_education_male_ages_25_and_older_[1,]
Population_with_at_least_some_secondary_education_male_ages_25_and_older_<- Population_with_at_least_some_secondary_education_male_ages_25_and_older_[-1,]

colnames(Total_unemployment_rate_female_to_male_ratio_)<- Total_unemployment_rate_female_to_male_ratio_[1,]
Total_unemployment_rate_female_to_male_ratio_<- Total_unemployment_rate_female_to_male_ratio_[-1,]


colnames(In_Ex)<- In_Ex[1,]
In_Ex<- In_Ex[-1,]

colnames(Inequality_adjHDI)<- Inequality_adjHDI[1,]
Inequality_adjHDI<- Inequality_adjHDI[-1,]

colnames(Gender_development_Index)<- Gender_development_Index[1,]
Gender_development_Index<- Gender_development_Index[-1,]

colnames(Inequality_Coef)<- Inequality_Coef[1,]
Inequality_Coef<- Inequality_Coef[-1,]

colnames(Investment_Index)<- Investment_Index[1,]
Investment_Index<- Investment_Index[-1,]

colnames(Human_Development_Index_HDI_) <- Human_Development_Index_HDI_[1,]
Human_Development_Index_HDI_<- Human_Development_Index_HDI_[-1,]

colnames(Gender_Inequality_Index)<- Gender_Inequality_Index[1,]
Gender_Equality_Index<- Gender_Inequality_Index[-1,]

colnames(Education_index)<- Education_index[1,]
Education_index<- Education_index[-1,]

colnames(Income_Index)<- Income_Index[1,]
Income_Index<- Income_Index[-1,]

colnames(Meta26_11_Excel)<- Meta26_11_Excel[1,]
Meta26_11_Excel<- Meta26_11_Excel[-1,]

colnames(Female_in_par)<- Female_in_par[1,]
Female_in_par<- Female_in_par[-1,]

###################################### ein paar Namensänderungen. Jahre werden je nach Quelle unterschiedlich benannt
setnames(wdi_2611, old =c("1980 [YR1980]","1981 [YR1981]","1982 [YR1982]","1983 [YR1983]", "1984 [YR1984]", "1985 [YR1985]", "1986 [YR1986]", "1987 [YR1987]","1988 [YR1988]","1989 [YR1989]","1990 [YR1990]","1991 [YR1991]","1992 [YR1992]","1993 [YR1993]", "1994 [YR1994]", "1995 [YR1995]", "1996 [YR1996]", "1997 [YR1997]","1998 [YR1998]","1999 [YR1999]","2000 [YR2000]","2001 [YR2001]","2002 [YR2002]","2003 [YR2003]","2004 [YR2004]","2005 [YR2005]","2006 [YR2006]","2007 [YR2007]","2008 [YR2008]","2009 [YR2009]","2010 [YR2010]","2011 [YR2011]","2012 [YR2012]","2013 [YR2013]","2014 [YR2014]","2015 [YR2015]","2016 [YR2016]","2017 [YR2017]","2018 [YR2018]","2019 [YR2019]","2020 [YR2020]"), new = c("1980", "1981","1982", "1983","1984", "1985","1986", "1987","1988", "1989","1990", "1991","1992", "1993","1994", "1995","1996", "1997","1998", "1999","2000", "2001","2002", "2003","2004", "2005","2006", "2007","2008", "2009","2010", "2011","2012", "2013","2014", "2015","2016", "2017","2018", "2019","2020"))
setnames(DemocracyMatrix_v4, old = "Country", new = "Country Name")
setnames(Governance_Indicators, old = c("1996 [YR1996]","1998 [YR1998]","2000 [YR2000]","2002 [YR2002]","2003 [YR2003]","2004 [YR2004]","2005 [YR2005]","2006 [YR2006]","2007 [YR2007]","2008 [YR2008]","2009 [YR2009]","2010 [YR2010]","2011 [YR2011]","2012 [YR2012]","2013 [YR2013]","2014 [YR2014]","2015 [YR2015]","2016 [YR2016]","2017 [YR2017]","2018 [YR2018]","2019 [YR2019]","2020 [YR2020]"), new = c("1996", "1998","2000","2002", "2003","2004", "2005","2006", "2007","2008", "2009","2010", "2011","2012", "2013","2014", "2015","2016", "2017","2018", "2019","2020"))
setnames(Demographie, old =c("1980 [YR1980]","1981 [YR1981]","1982 [YR1982]","1983 [YR1983]", "1984 [YR1984]", "1985 [YR1985]", "1986 [YR1986]", "1987 [YR1987]","1988 [YR1988]","1989 [YR1989]","1990 [YR1990]","1991 [YR1991]","1992 [YR1992]","1993 [YR1993]", "1994 [YR1994]", "1995 [YR1995]", "1996 [YR1996]", "1997 [YR1997]","1998 [YR1998]","1999 [YR1999]","2000 [YR2000]","2001 [YR2001]","2002 [YR2002]","2003 [YR2003]","2004 [YR2004]","2005 [YR2005]","2006 [YR2006]","2007 [YR2007]","2008 [YR2008]","2009 [YR2009]","2010 [YR2010]","2011 [YR2011]","2012 [YR2012]","2013 [YR2013]","2014 [YR2014]","2015 [YR2015]","2016 [YR2016]","2017 [YR2017]","2018 [YR2018]","2019 [YR2019]","2020 [YR2020]"), new = c("1980", "1981","1982", "1983","1984", "1985","1986", "1987","1988", "1989","1990", "1991","1992", "1993","1994", "1995","1996", "1997","1998", "1999","2000", "2001","2002", "2003","2004", "2005","2006", "2007","2008", "2009","2010", "2011","2012", "2013","2014", "2015","2016", "2017","2018", "2019","2020"))
setnames(Female_in_par, old =c("1980 [YR1980]","1981 [YR1981]","1982 [YR1982]","1983 [YR1983]", "1984 [YR1984]", "1985 [YR1985]", "1986 [YR1986]", "1987 [YR1987]","1988 [YR1988]","1989 [YR1989]","1990 [YR1990]","1991 [YR1991]","1992 [YR1992]","1993 [YR1993]", "1994 [YR1994]", "1995 [YR1995]", "1996 [YR1996]", "1997 [YR1997]","1998 [YR1998]","1999 [YR1999]","2000 [YR2000]","2001 [YR2001]","2002 [YR2002]","2003 [YR2003]","2004 [YR2004]","2005 [YR2005]","2006 [YR2006]","2007 [YR2007]","2008 [YR2008]","2009 [YR2009]","2010 [YR2010]","2011 [YR2011]","2012 [YR2012]","2013 [YR2013]","2014 [YR2014]","2015 [YR2015]","2016 [YR2016]","2017 [YR2017]","2018 [YR2018]","2019 [YR2019]","2020 [YR2020]"), new = c("1980", "1981","1982", "1983","1984", "1985","1986", "1987","1988", "1989","1990", "1991","1992", "1993","1994", "1995","1996", "1997","1998", "1999","2000", "2001","2002", "2003","2004", "2005","2006", "2007","2008", "2009","2010", "2011","2012", "2013","2014", "2015","2016", "2017","2018", "2019","2020"))
setnames(Labor_force_participation_rate_for_ages_15_24_male_, old =c("1980 [YR1980]","1981 [YR1981]","1982 [YR1982]","1983 [YR1983]", "1984 [YR1984]", "1985 [YR1985]", "1986 [YR1986]", "1987 [YR1987]","1988 [YR1988]","1989 [YR1989]","1990 [YR1990]","1991 [YR1991]","1992 [YR1992]","1993 [YR1993]", "1994 [YR1994]", "1995 [YR1995]", "1996 [YR1996]", "1997 [YR1997]","1998 [YR1998]","1999 [YR1999]","2000 [YR2000]","2001 [YR2001]","2002 [YR2002]","2003 [YR2003]","2004 [YR2004]","2005 [YR2005]","2006 [YR2006]","2007 [YR2007]","2008 [YR2008]","2009 [YR2009]","2010 [YR2010]","2011 [YR2011]","2012 [YR2012]","2013 [YR2013]","2014 [YR2014]","2015 [YR2015]","2016 [YR2016]","2017 [YR2017]","2018 [YR2018]","2019 [YR2019]","2020 [YR2020]"), new = c("1980", "1981","1982", "1983","1984", "1985","1986", "1987","1988", "1989","1990", "1991","1992", "1993","1994", "1995","1996", "1997","1998", "1999","2000", "2001","2002", "2003","2004", "2005","2006", "2007","2008", "2009","2010", "2011","2012", "2013","2014", "2015","2016", "2017","2018", "2019","2020"))
setnames(Export1803, old =c("1980 [YR1980]","1981 [YR1981]","1982 [YR1982]","1983 [YR1983]", "1984 [YR1984]", "1985 [YR1985]", "1986 [YR1986]", "1987 [YR1987]","1988 [YR1988]","1989 [YR1989]","1990 [YR1990]","1991 [YR1991]","1992 [YR1992]","1993 [YR1993]", "1994 [YR1994]", "1995 [YR1995]", "1996 [YR1996]", "1997 [YR1997]","1998 [YR1998]","1999 [YR1999]","2000 [YR2000]","2001 [YR2001]","2002 [YR2002]","2003 [YR2003]","2004 [YR2004]","2005 [YR2005]","2006 [YR2006]","2007 [YR2007]","2008 [YR2008]","2009 [YR2009]","2010 [YR2010]","2011 [YR2011]","2012 [YR2012]","2013 [YR2013]","2014 [YR2014]","2015 [YR2015]","2016 [YR2016]","2017 [YR2017]","2018 [YR2018]","2019 [YR2019]","2020 [YR2020]"), new = c("1980", "1981","1982", "1983","1984", "1985","1986", "1987","1988", "1989","1990", "1991","1992", "1993","1994", "1995","1996", "1997","1998", "1999","2000", "2001","2002", "2003","2004", "2005","2006", "2007","2008", "2009","2010", "2011","2012", "2013","2014", "2015","2016", "2017","2018", "2019","2020"))
setnames(Export2203, old =c("1980 [YR1980]","1981 [YR1981]","1982 [YR1982]","1983 [YR1983]", "1984 [YR1984]", "1985 [YR1985]", "1986 [YR1986]", "1987 [YR1987]","1988 [YR1988]","1989 [YR1989]","1990 [YR1990]","1991 [YR1991]","1992 [YR1992]","1993 [YR1993]", "1994 [YR1994]", "1995 [YR1995]", "1996 [YR1996]", "1997 [YR1997]","1998 [YR1998]","1999 [YR1999]","2000 [YR2000]","2001 [YR2001]","2002 [YR2002]","2003 [YR2003]","2004 [YR2004]","2005 [YR2005]","2006 [YR2006]","2007 [YR2007]","2008 [YR2008]","2009 [YR2009]","2010 [YR2010]","2011 [YR2011]","2012 [YR2012]","2013 [YR2013]","2014 [YR2014]","2015 [YR2015]","2016 [YR2016]","2017 [YR2017]","2018 [YR2018]","2019 [YR2019]","2020 [YR2020]"), new = c("1980", "1981","1982", "1983","1984", "1985","1986", "1987","1988", "1989","1990", "1991","1992", "1993","1994", "1995","1996", "1997","1998", "1999","2000", "2001","2002", "2003","2004", "2005","2006", "2007","2008", "2009","2010", "2011","2012", "2013","2014", "2015","2016", "2017","2018", "2019","2020"))
setnames(Export2603, old =c("1980 [YR1980]","1981 [YR1981]","1982 [YR1982]","1983 [YR1983]", "1984 [YR1984]", "1985 [YR1985]", "1986 [YR1986]", "1987 [YR1987]","1988 [YR1988]","1989 [YR1989]","1990 [YR1990]","1991 [YR1991]","1992 [YR1992]","1993 [YR1993]", "1994 [YR1994]", "1995 [YR1995]", "1996 [YR1996]", "1997 [YR1997]","1998 [YR1998]","1999 [YR1999]","2000 [YR2000]","2001 [YR2001]","2002 [YR2002]","2003 [YR2003]","2004 [YR2004]","2005 [YR2005]","2006 [YR2006]","2007 [YR2007]","2008 [YR2008]","2009 [YR2009]","2010 [YR2010]","2011 [YR2011]","2012 [YR2012]","2013 [YR2013]","2014 [YR2014]","2015 [YR2015]","2016 [YR2016]","2017 [YR2017]","2018 [YR2018]","2019 [YR2019]","2020 [YR2020]"), new = c("1980", "1981","1982", "1983","1984", "1985","1986", "1987","1988", "1989","1990", "1991","1992", "1993","1994", "1995","1996", "1997","1998", "1999","2000", "2001","2002", "2003","2004", "2005","2006", "2007","2008", "2009","2010", "2011","2012", "2013","2014", "2015","2016", "2017","2018", "2019","2020"))


Export2403[ , 5:24]<- list(NULL)
setnames(Export2403, old =c("1980 [YR1980]","1981 [YR1981]","1982 [YR1982]","1983 [YR1983]", "1984 [YR1984]", "1985 [YR1985]", "1986 [YR1986]", "1987 [YR1987]","1988 [YR1988]","1989 [YR1989]","1990 [YR1990]","1991 [YR1991]","1992 [YR1992]","1993 [YR1993]", "1994 [YR1994]", "1995 [YR1995]", "1996 [YR1996]", "1997 [YR1997]","1998 [YR1998]","1999 [YR1999]","2000 [YR2000]","2001 [YR2001]","2002 [YR2002]","2003 [YR2003]","2004 [YR2004]","2005 [YR2005]","2006 [YR2006]","2007 [YR2007]","2008 [YR2008]","2009 [YR2009]","2010 [YR2010]","2011 [YR2011]","2012 [YR2012]","2013 [YR2013]","2014 [YR2014]","2015 [YR2015]","2016 [YR2016]","2017 [YR2017]","2018 [YR2018]","2019 [YR2019]","2020 [YR2020]"), new = c("1980", "1981","1982", "1983","1984", "1985","1986", "1987","1988", "1989","1990", "1991","1992", "1993","1994", "1995","1996", "1997","1998", "1999","2000", "2001","2002", "2003","2004", "2005","2006", "2007","2008", "2009","2010", "2011","2012", "2013","2014", "2015","2016", "2017","2018", "2019","2020"))
setnames(Export2403_2, old =c("1980 [YR1980]","1981 [YR1981]","1982 [YR1982]","1983 [YR1983]", "1984 [YR1984]", "1985 [YR1985]", "1986 [YR1986]", "1987 [YR1987]","1988 [YR1988]","1989 [YR1989]","1990 [YR1990]","1991 [YR1991]","1992 [YR1992]","1993 [YR1993]", "1994 [YR1994]", "1995 [YR1995]", "1996 [YR1996]", "1997 [YR1997]","1998 [YR1998]","1999 [YR1999]","2000 [YR2000]","2001 [YR2001]","2002 [YR2002]","2003 [YR2003]","2004 [YR2004]","2005 [YR2005]","2006 [YR2006]","2007 [YR2007]","2008 [YR2008]","2009 [YR2009]","2010 [YR2010]","2011 [YR2011]","2012 [YR2012]","2013 [YR2013]","2014 [YR2014]","2015 [YR2015]","2016 [YR2016]","2017 [YR2017]","2018 [YR2018]","2019 [YR2019]","2020 [YR2020]"), new = c("1980", "1981","1982", "1983","1984", "1985","1986", "1987","1988", "1989","1990", "1991","1992", "1993","1994", "1995","1996", "1997","1998", "1999","2000", "2001","2002", "2003","2004", "2005","2006", "2007","2008", "2009","2010", "2011","2012", "2013","2014", "2015","2016", "2017","2018", "2019","2020"))

###################################### Die Datensätze in die richtige Form bringen. Die Variablen sollen einzele Spalten haben. Die Jahre als einzelne Spalte formatieren. 
Exp2603_piv<- Export2603%>%
  dplyr::select(-one_of("Series Name"))%>%
  pivot_longer(
    cols = `1980`:`2020`,
    names_to = "Year",
    names_transform = list(Year= as.integer),
    values_drop_na = T
  )%>%
  pivot_wider(names_from = "Series Code", values_from = "value")

Exp2403_2_piv<- Export2403_2%>%
  dplyr::select(-one_of("Series Name"))%>%
  pivot_longer(
    cols = `1980`:`2020`,
    names_to = "Year",
    names_transform = list(Year= as.integer),
    values_drop_na = T
  )%>%
  pivot_wider(names_from = "Series Code", values_from = "value")

Exp2403_piv<- Export2403%>%
  dplyr::select(-one_of("Series Name"))%>%
  pivot_longer(
    cols = `1980`:`2020`,
    names_to = "Year",
    names_transform = list(Year= as.integer),
    values_drop_na = T
  )%>%
  pivot_wider(names_from = "Series Code", values_from = "value")

Exp2203_piv<- Export2203%>%
  dplyr::select(-one_of("Series Name"))%>%
  pivot_longer(
    cols = `1980`:`2020`,
    names_to = "Year",
    names_transform = list(Year= as.integer),
    values_drop_na = T
  )%>%
  pivot_wider(names_from = "Series Code", values_from = "value")

Exp1803_piv<- Export1803%>%
  dplyr::select(-one_of("Series Name"))%>%
  pivot_longer(
    cols = `1980`:`2020`,
    names_to = "Year",
    names_transform = list(Year= as.integer),
    values_drop_na = T
  )%>%
  pivot_wider(names_from = "Series Code", values_from = "value")

Wdi_pivot <- wdi_2611 %>%
  dplyr::select(-one_of("Series Name"))%>%
  pivot_longer(
    cols = `1980`:`2020`,
    names_to = "Year",
    names_transform = list(Year= as.integer),
    values_drop_na = TRUE
  )%>%
  pivot_wider(names_from = "Series Code", values_from = "value")

lfp_male_1524_pivot <- Labor_force_participation_rate_for_ages_15_24_male_ %>%
  dplyr::select(-one_of("Series Name"))%>%
  pivot_longer(
    cols = `1980`:`2020`,
    names_to = "Year",
    names_transform = list(Year= as.integer),
    values_drop_na = TRUE
  )%>%
  pivot_wider(names_from = c("Series Code"), values_from = "value")



HDI_pivot <- Human_Development_Index_HDI_ %>%
  pivot_longer(
    cols = `1990`:`2019`,
    names_to = "Year",
    names_transform = list(Year= as.integer),
    values_to = "HDI",
    values_drop_na = TRUE
  )

Gender_pivot <- Gender_Inequality_Index %>%
  pivot_longer(
    cols = `1995`:`2019`,
    names_to = "Year",
    names_transform = list(Year= as.integer),
    values_to = "GII",
    values_drop_na = TRUE
  )

Education_pivot <- Education_index %>%
  pivot_longer(
    cols = `1990`:`2019`,
    names_to = "Year",
    names_transform = list(Year= as.integer),
    values_to = "EDI",
    values_drop_na = TRUE
  )

Income_pivot <- Income_Index %>%
  pivot_longer(
    cols = `1990`:`2019`,
    names_to = "Year",
    names_transform = list(Year= as.integer),
    values_to = "GNI",
    values_drop_na = TRUE
  )
Invest_pivot <- Investment_Index %>%
  pivot_longer(
    cols = `1990`:`2011-2019`,
    names_to = "Year",
    names_transform = list(Year= as.integer),
    values_to = "Investment",
    values_drop_na = TRUE
  )

Governance_pivot <- Governance_Indicators %>%
  dplyr::select(-one_of("Series Name"))%>%
  pivot_longer(
    cols = `1996`:`2020`,
    names_to = "Year",
    names_transform = list(Year= as.integer),
    values_drop_na = TRUE
  )%>%
  pivot_wider(names_from = c("Series Code"), values_from = "value")

Inequ_adjHDI_pivot <- Inequality_adjHDI%>%
  pivot_longer(
    cols=`2010`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "adjHDI",
  )

GDI_pivot<- Gender_development_Index%>%
  pivot_longer(
    cols = `1995`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "GDI",
  )

Ineq_coef_pivot <- Inequality_Coef%>%
  pivot_longer(
    cols = `2010`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "Ineq_Coef",
  )

demographie_pivot <- Demographie %>%
  dplyr::select(-one_of("Series Name"))%>%
  pivot_longer(
    cols = `1980`:`2020`,
    names_to = "Year",
    names_transform = list(Year= as.integer),
    values_drop_na = TRUE
  )%>%
  pivot_wider(names_from = c("Series Code"), values_from = "value")

Im_Ex_pivot <- In_Ex%>%
  pivot_longer(
    cols = `1990`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "Im_Ex",
  )

F_i_p_pivot <- Female_in_par %>%
  dplyr::select(-one_of("Series Name"))%>%
  pivot_longer(
    cols = `1980`:`2020`,
    names_to = "Year",
    names_transform = list(Year= as.integer),
    values_drop_na = TRUE
  )%>%
  pivot_wider(names_from = c("Series Code"), values_from = "value")

t_unemp_mf_pivot <- Total_unemployment_rate_female_to_male_ratio_%>%
  pivot_longer(
    cols = `1991`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "Unemployment_M_F",
  )

Pop_secedu_m_25 <- Population_with_at_least_some_secondary_education_male_ages_25_and_older_%>%
  pivot_longer(
    cols = `1990`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "Population_secondary_education_male_25+",
  )

Pop_secedu_f_25 <- Population_with_at_least_some_secondary_education_female_ages_25_and_older_%>%
  pivot_longer(
    cols = `1990`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "Population_secondary_education_female_25+",
  )

Pop_secedu_25 <- Population_with_at_least_some_secondary_education_ages_25_and_older_%>%
  pivot_longer(
    cols = `1990`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "Population_secondary_education_25+",
  )

mean_years_schooling_male <- Mean_years_of_schooling_male_years_%>%
  pivot_longer(
    cols = `1990`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "mean_years_schooling_male",
  )

mean_years_schooling_female <- Mean_years_of_schooling_female_years_%>%
  pivot_longer(
    cols = `1990`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "mean_years_schooling_female",
  )

mean_years_schooling <- Mean_years_of_schooling_years_%>%
  pivot_longer(
    cols = `1990`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "mean_years_schooling",
  )

Maternal_mortality_ratio <- Maternal_mortality_ratio_deaths_per_100_000_live_births_%>%
  pivot_longer(
    cols = `1990`:`2017`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "Maternal_mortality_ratio",
  )

life_exp_male <- Life_expectancy_at_birth_male_years_%>%
  pivot_longer(
    cols = `1990`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "life_exp_male",
  )

life_exp_female <- Life_expectancy_at_birth_female_years_%>%
  pivot_longer(
    cols = `1990`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "life_exp_female",
  )

life_exp <- Life_expectancy_at_birth_years_%>%
  pivot_longer(
    cols = `1991`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "life_exp",
  )

Laborforce_male_15 <- Labour_force_participation_rate_ages_15_and_older_male%>%
  pivot_longer(
    cols = `1990`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "Laborforce_male_15+",
  )

Laborforce_female_15 <- Labour_force_participation_rate_ages_15_and_older_female%>%
  pivot_longer(
    cols = `1990`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "Laborforce_female_15+",
  )


exp_years_school <- Expected_years_of_schooling_years_%>%
  pivot_longer(
    cols = `1990`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "exp_years_school",
  )
exp_years_school_male <- Expected_years_of_schooling_male_years_%>%
  pivot_longer(
    cols = `1990`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "exp_years_school_male",
  )

exp_years_school_female <- Expected_years_of_schooling_female_years_%>%
  pivot_longer(
    cols = `1990`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "exp_years_school_female",
  )

GNI_per_capita_female <- Estimated_gross_national_income_per_capita_female_2017_PPP_%>%
  pivot_longer(
    cols = `1995`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "GNI_per_capita_female",
  )

GNI_per_capita_male <- Estimated_gross_national_income_per_capita_male_2017_PPP_%>%
  pivot_longer(
    cols = `1995`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "GNI_per_capita_male",
  )

Adolescent_birth_rate <- Adolescent_birth_rate_births_per_1_000_women_ages_15_19_%>%
  pivot_longer(
    cols = `1990`:`2019`,
    names_to = "Year",
    names_transform = list(Year =as.integer),
    values_drop_na= TRUE,
    values_to = "Adolescent_birth_rate",
  )
### Die Namen der von Country zu Country Code ändern. Können dann besser Zusammengefügt werden

setnames(Gender_pivot, old= "Country", new = "Country Name")
setnames(HDI_pivot, old= "Country", new = "Country Name")
setnames(Income_pivot, old= "Country", new = "Country Name")
setnames(Education_pivot, old= "Country", new = "Country Name")
setnames(Invest_pivot, old= "Country", new = "Country Name")
setnames(Ineq_coef_pivot, old= "Country", new = "Country Name")
setnames(GDI_pivot, old= "Country", new = "Country Name")
setnames(Inequ_adjHDI_pivot, old= "Country", new = "Country Name")
setnames(Im_Ex_pivot, old = "Country", new = "CountryCode")

#### Country Codes und weitere Attribute aus dem cartography Packet
countries_sf <- ne_countries(returnclass = "sf")
## Vorbereitung einer Karte
countries_sf %>%
  ggplot() +
  geom_sf()


###################################### Zusammenfügen der Datensätze in einen großen Datensatz, in dem jeder Konflikt eine Zeile hat


wdi_sf <- left_join(Wdi_pivot,countries_sf, by= c("Country Code" = "iso_a3")) ## erste WDI-Daten mit ökonomischen und räumlichen Regionen verknüpfen.
wdi_Gov<- left_join(wdi_sf,Governance_pivot, by= c("Year", "Country Code")) ## Governance Indikatoren hinzufügen
Wdi_Konflikt <- left_join(wdi_Gov, ucdp_dpc, by = c("Year", "Country Code")) ## Konfliktdatensatz hinzufügen
wdi_Konf_num <- Wdi_Konflikt 
wdi_Konf_num$Konflikt<- Wdi_Konflikt$Konflikt%>%
  replace(is.na(Wdi_Konflikt$Konflikt),0)%>%
  as.logical(Wdi_Konflikt$Konflikt) #### True und False für das Auftreten von Konflikten. Alle Missing Values sind Länder ohen Konflikte

Join_HDI <- left_join(wdi_Konf_num,HDI_pivot, by = c("Year", "Country Code")) ## HDI Hinzufügen
Demokratie <- left_join(Join_HDI, DemocracyMatrix_v4, by = c("Country Code","Year")) ## Demokratie Matrix hinzufügen
HDI_GII_Konf<- left_join(Demokratie,Gender_pivot, by = c("Year", "Country Code")) ## GII hinzufügen
HDI_GII_EDI<- left_join(HDI_GII_Konf,Education_pivot, by = c( "Year", "Country Code")) ## EDI hinzufügen
HDI_GII_EDI_GNI<- left_join(HDI_GII_EDI,Income_pivot, by = c( "Year", "Country Code")) ## Einkommens Index hinzufügen
HDI_GII_EDI_GNI_Dem_Invest_Gov <- left_join(HDI_GII_EDI_GNI,Invest_pivot, by = c("Year","Country Code")) ## Investment Index hinzufügen
HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI <-left_join(HDI_GII_EDI_GNI_Dem_Invest_Gov, GDI_pivot, by= c("Year", "Country Code")) ## GDI hinzufügen
HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI <- left_join(HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI, Inequ_adjHDI_pivot, by= c("Year", "Country Code")) ## HDI auf Ungleichheiten angepasst
HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef <- left_join(HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI, Ineq_coef_pivot, by = c("Year","Country Code")) ## Ungleichheits Koeffizient
HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex <- left_join(HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef, Im_Ex_pivot,by = c("Year","Country Code")) ## Import Export Verhältnis
HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie <- left_join(HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex,demographie_pivot, by = c("Year","Country Code")) ## Demographie daten hizufügen
HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip <- left_join(HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie, F_i_p_pivot, by= c("Year","Country Code")) ## Frauen im Parlament
## eine andere und schnellere Art Indicatoren in einen Datensatz zu fusionieren
df_list<- list(mean_years_schooling,
               Adolescent_birth_rate,
               GNI_per_capita_male,
               GNI_per_capita_female,
               exp_years_school_female,
               exp_years_school_male,
               exp_years_school,
               Laborforce_female_15,
               Laborforce_male_15,
               life_exp,
               life_exp_female,
               life_exp_male,
               Maternal_mortality_ratio,
               mean_years_schooling_female,
               mean_years_schooling_male,
               Pop_secedu_25,
               Pop_secedu_f_25,
               Pop_secedu_m_25,
               t_unemp_mf_pivot)
Join_Index_Indicator <- df_list%>%
  reduce(full_join,by=c("Year", "Country","Country Code"))
Join_Index_Indicator_lfp <- left_join(Join_Index_Indicator,lfp_male_1524_pivot, by = c("Year","Country Code")) ### eine schnellere Methode um mehrere Datensätze zusammenzufügen. Dieser Datensatz wird im nächsten Schritt zu dem allgemeinen Datensatz zusammengeführt

HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi <-left_join(HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip, Join_Index_Indicator_lfp, by= c("Year","Country Code")) ## Indikatoren spezifiziert auf männlich und weiblich 
HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803 <- left_join(HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi, Exp1803_piv, by= c("Year","Country Code")) ## Bevölkerungsdichte, Bevölkerungswachstum und weitere WDI Daten hinzugefügt
HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803_2203<-left_join(HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803, Exp2203_piv, by=c("Year","Country Code")) ## Lebenserwartung, Müttersterblichkeitsrate, Jugendschwangerschaft
HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803_2203_2403<- left_join(HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803_2203,Exp2403_piv, by = c("Year","Country Code")) ## Export von Treibstoff 
HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803_2203_2403_2<- left_join(HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803_2203_2403,Exp2403_2_piv,by = c("Year","Country Code")) ## GNI pro Kopf
HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803_2203_2403_2_2603<-left_join(HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803_2203_2403_2, Exp2603_piv,by = c("Year","Country Code")) ## Geburten- und Todesrate

## erste Auswahl der Variablen. Muss noch weiter eingegrenzt werden. Aber diese Variablen können potenziell genutzt werden.

Data_geo <- HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803_2203_2403_2_2603%>%
  dplyr::select(one_of(c("type_of_conflict","SP.DYN.CBRT.IN","SP.DYN.CDRT.IN","SE.SEC.CUAT.LO.ZS","NY.GNP.PCAP.PP.CD","TX.VAL.FUEL.ZS.UN","SH.STA.MMRT","SP.ADO.TFRT","SP.DYN.LE00.IN","NE.EXP.GNFS.ZS","SE.XPD.TOTL.GD.ZS","TX.VAL.MMTL.ZS.UN","SL.UEM.1524.ZS","NY.GNP.PCAP.CD","SM.POP.TOTL.ZS","EN.POP.DNST","SP.POP.GROW","SP.POP.TOTL","SL.TLF.ACTI.1524.MA.ZS", "SL.TLF.ACTI.1524.FE.ZS",
                  "Unemployment_M_F",
                  "Population_secondary_education_male_25+",
                  "Population_secondary_education_female_25+",
                  "Population_secondary_education_25+",
                  "mean_years_schooling_male",
                  "mean_years_schooling_female",
                  "mean_years_schooling",
                  "Maternal_mortality_ratio",
                  "life_exp_male",
                  "life_exp_female",
                  "life_exp","Laborforce_male_15+",
                  "Laborforce_female_15+",
                  "Adolescent_birth_rate",
                  "GNI_per_capita_male",
                  "GNI_per_capita_female",
                  "exp_years_school_female",
                  "exp_years_school_male",
                  "exp_years_school",
                  "SG.GEN.PARL.ZS",
                  "Konflikt",
                  "type_of_conflict",
                  "SP.POP.2529.FE.5Y",
                  "SP.POP.2529.MA.5Y",
                  "SP.POP.TOTL.FE.ZS",
                  "SP.POP.TOTL.MA.ZS", 
                  "SP.POP.2024.MA.5Y", 
                  "SP.POP.2024.FE.5Y",
                  "Im_Ex","Ineq_Coef",
                  "adjHDI",
                  "GDI",
                  "Country Name.x",
                  "Country Code", 
                  "Year","classification_core",
                  "freedom_dim_index_core",
                  "rule_settlement_freedom_core",
                  "rights_freedom_core",
                  "communication_freedom_core",
                  "intermediate_freedom_core",
                  "decision_freedom_core",
                  "classification_core",
                  "freedom_dim_index_core",
                  "equality_dim_index_core", 
                  "control_dim_index_core", 
                  "total_index_core",
                  "SI.POV.GINI",
                  "SL.TLF.ACTI.1524.FE.ZS",
                  "SL.TLF.CACT.FE.ZS",
                  "SL.TLF.TOTL.FE.ZS",
                  "SL.TLF.CACT.FM.ZS",
                  "NY.GDP.MKTP.KN",
                  "NY.GDP.MKTP.KD.ZG",
                  "NY.GDP.PCAP.CN",
                  "NY.GDP.PCAP.KD.ZG",
                  "IQ.CPA.PROP.XQ",
                  "IQ.CPA.GNDR.XQ",
                  "SE.ADT.1524.LT.FM.ZS",
                  "SE.ENR.PRIM.FM.ZS", 
                  "SE.ENR.PRSC.FM.ZS", 
                  "SE.ENR.SECO.FM.ZS", 
                  "SE.ENR.TERT.FM.ZS",
                  "CC.EST", "PV.EST",
                  "GE.EST", "RQ.EST", 
                  "RL.EST",
                  "VA.EST","Country Code", 
                  "type", 
                  "economy",
                  "geometry", "continent", 
                  "region_un", "subregion",
                  "region_wb", "bd_best", 
                  "bd_low", "bd_high", 
                  "HDI", "GII","EDI","GNI","Investment"))) ## Mit dem Datensatz wird weitergearbeitet. Hier sind alle Konflikte einzelt angegeben. Jeder Konflikt ist einem Land und einem Jahr zugeordnet und den dazugehörigen Ausprägungen der Variablen. (enthält Missing Values)



#### Viele Variablen sind in "character" gespeichert. Müssen in numerische Werte umgewandelt werden.

col.num.all <-c("SP.DYN.CBRT.IN","SP.DYN.CDRT.IN","SE.SEC.CUAT.LO.ZS","NY.GNP.PCAP.PP.CD","TX.VAL.FUEL.ZS.UN","SH.STA.MMRT","SP.ADO.TFRT","SP.DYN.LE00.IN","NE.EXP.GNFS.ZS","SE.XPD.TOTL.GD.ZS","TX.VAL.MMTL.ZS.UN","SL.UEM.1524.ZS","NY.GNP.PCAP.CD","SM.POP.TOTL.ZS","EN.POP.DNST","SP.POP.GROW","SP.POP.TOTL","SL.TLF.ACTI.1524.MA.ZS",
                "Unemployment_M_F",
                "Population_secondary_education_male_25+",
                "Population_secondary_education_female_25+",
                "Population_secondary_education_25+",
                "mean_years_schooling_male",
                "mean_years_schooling_female",
                "mean_years_schooling",
                "Maternal_mortality_ratio",
                "life_exp_male",
                "life_exp_female",
                "life_exp","Laborforce_male_15+",
                "Laborforce_female_15+",
                "Adolescent_birth_rate",
                "GNI_per_capita_male",
                "GNI_per_capita_female",
                "exp_years_school_female",
                "exp_years_school_male",
                "exp_years_school","SG.GEN.PARL.ZS","Im_Ex","equality_dim_index_core", "control_dim_index_core", "total_index_core","SP.POP.2529.FE.5Y","SP.POP.2529.MA.5Y","SP.POP.TOTL.FE.ZS","SP.POP.TOTL.MA.ZS", "SP.POP.2024.MA.5Y", "SP.POP.2024.FE.5Y","Ineq_Coef","adjHDI","GDI","Year","freedom_dim_index_core","rule_settlement_freedom_core","rights_freedom_core","communication_freedom_core","intermediate_freedom_core","SI.POV.GINI","SL.TLF.ACTI.1524.FE.ZS","SL.TLF.CACT.FE.ZS","SL.TLF.TOTL.FE.ZS","SL.TLF.CACT.FM.ZS","NY.GDP.MKTP.KN","NY.GDP.MKTP.KD.ZG", "NY.GDP.PCAP.CN","NY.GDP.PCAP.KD.ZG","IQ.CPA.PROP.XQ","IQ.CPA.GNDR.XQ","SE.ADT.1524.LT.FM.ZS","SE.ENR.PRIM.FM.ZS", "SE.ENR.PRSC.FM.ZS", "SE.ENR.SECO.FM.ZS", "SE.ENR.TERT.FM.ZS","CC.EST", "PV.EST","GE.EST", "RQ.EST", "RL.EST","VA.EST", "bd_best", "bd_low", "bd_high", "HDI", "GII","EDI","GNI","Investment")
Data_geo[col.num.all] <- sapply(Data_geo[col.num.all],as.numeric)

###################################### Ein Datensatz, in dem nur ein Konflikt pro Jahr im Land vorkommt. Ist für eine statistische Auswertung der Daten nötig. Die Frage ist ob ein Konflikt in einem Land stattfindet. Wenn jeder Konflikt berücksichtigt wird, werden die Variablen-Werte für das Jahr mehrfach betrachtet. Die Berechnungen werden verzerrt

### Gruppeirung der LÃnder und Jahre plus die Aufsummierung der Todesopfer in den Jahren. 
Data_bd_best<-Data_geo%>%
  group_by(`Country Code`,`Year`)%>%
  summarise(Sum_bd_best=sum(bd_best))

Data_bd_high<-Data_geo%>%
  group_by(`Country Code`,`Year`)%>%
  summarise(Sum_bd_high=sum(bd_high))

test_type<- Data_geo
Data_bd_low<-Data_geo%>%
  group_by(`Country Code`,`Year`)%>%
  summarise(Sum_bd_low = sum (bd_low))

  

Death<- inner_join(Data_bd_best,Data_bd_high, by=c("Country Code","Year"))

Death_join<-inner_join(Death,Data_bd_low,by=c("Country Code", "Year")) ## die verschiedenen geschätzen Werte zusammenfügen


#### Diesen Datensatz mit den summierten Todesopfern mit den restlichen Daten zusammenfügen. Gleiches Prinzip, wie bei dem Datensatz mit allen Konflikten. 

Death_wdi <- left_join(wdi_Gov,Death_join, by= c("Country Code","Year"))

Stat_Join_HDI <- left_join(Death_wdi,HDI_pivot, by = c("Year", "Country Code"))
Stat_Demokratie <- left_join(Stat_Join_HDI, DemocracyMatrix_v4, by = c("Country Code","Year"))
Stat_HDI_GII_Konf<- left_join(Stat_Demokratie,Gender_pivot, by = c("Year", "Country Code"))
Stat_HDI_GII_EDI<- left_join(Stat_HDI_GII_Konf,Education_pivot, by = c( "Year", "Country Code"))
Stat_HDI_GII_EDI_GNI<- left_join(Stat_HDI_GII_EDI,Income_pivot, by = c( "Year", "Country Code"))
Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov <- left_join(Stat_HDI_GII_EDI_GNI,Invest_pivot, by = c("Year","Country Code"))
Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI <-left_join(Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov, GDI_pivot, by= c("Year", "Country Code"))
Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI <- left_join(Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI, Inequ_adjHDI_pivot, by= c("Year", "Country Code"))
Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef <- left_join(Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI, Ineq_coef_pivot, by = c("Year","Country Code"))
Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex <- left_join(Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef, Im_Ex_pivot,by = c("Year","Country Code"))
Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie <- left_join(Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex,demographie_pivot, by = c("Year","Country Code"))
Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip <- left_join(Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie, F_i_p_pivot, by= c("Year","Country Code"))
Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi <-left_join(Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip, Join_Index_Indicator_lfp, by= c("Year","Country Code"))
Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803 <- left_join(Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi, Exp1803_piv, by= c("Year","Country Code"))
Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803_2203<-left_join(Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803, Exp2203_piv, by=c("Year","Country Code"))
Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803_2203_2403<- left_join(Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803_2203,Exp2403_piv, by = c("Year","Country Code"))
Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803_2203_2403_2<- left_join(Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803_2203_2403, Exp2403_2_piv, by = c("Year","Country Code"))
Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803_2203_2403_2_2603 <- left_join(Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803_2203_2403_2, Exp2603_piv,by = c("Year","Country Code") )

Stat_Data_geo <- Stat_HDI_GII_EDI_GNI_Dem_Invest_Gov_GDI_adjHDI_Ineq_Coef_Im_Ex_Demographie_fip_IndexIndi_1803_2203_2403_2_2603%>%
  dplyr::select(one_of(c("classification_core","SP.DYN.CBRT.IN","SP.DYN.CDRT.IN","SE.SEC.CUAT.LO.ZS","SP.URB.TOTL.IN.ZS","NY.GNP.PCAP.PP.CD","TX.VAL.FUEL.ZS.UN","SH.STA.MMRT","SP.ADO.TFRT","SP.DYN.LE00.IN","NE.EXP.GNFS.ZS","SE.XPD.TOTL.GD.ZS","TX.VAL.MMTL.ZS.UN","SL.UEM.1524.ZS","NY.GNP.PCAP.CD","SM.POP.TOTL.ZS","EN.POP.DNST","SP.POP.GROW","SP.POP.TOTL","SL.TLF.ACTI.1524.MA.ZS",
                  "Unemployment_M_F",
                  "Population_secondary_education_male_25+",
                  "Population_secondary_education_female_25+",
                  "Population_secondary_education_25+",
                  "mean_years_schooling_male",
                  "mean_years_schooling_female",
                  "mean_years_schooling",
                  "Maternal_mortality_ratio",
                  "life_exp_male",
                  "life_exp_female",
                  "life_exp","Laborforce_male_15+",
                  "Laborforce_female_15+",
                  "Adolescent_birth_rate",
                  "GNI_per_capita_male",
                  "GNI_per_capita_female",
                  "exp_years_school_female",
                  "exp_years_school_male",
                  "exp_years_school","SG.GEN.PARL.ZS","SP.POP.2529.FE.5Y","SP.POP.2529.MA.5Y","SP.POP.TOTL.FE.ZS","SP.POP.TOTL.MA.ZS", "SP.POP.2024.MA.5Y", "SP.POP.2024.FE.5Y","Im_Ex","Ineq_Coef","adjHDI","GDI","Country Name.x","Country Code", "Year","classification_core","freedom_dim_index_core","equality_dim_index_core", "control_dim_index_core", "total_index_core","SI.POV.GINI","SL.TLF.ACTI.1524.FE.ZS","SL.TLF.CACT.FE.ZS","SL.TLF.TOTL.FE.ZS","SL.TLF.CACT.FM.ZS","NY.GDP.MKTP.KN","NY.GDP.MKTP.KD.ZG", "NY.GDP.PCAP.CN","NY.GDP.PCAP.KD.ZG","IQ.CPA.PROP.XQ","IQ.CPA.GNDR.XQ","SE.ADT.1524.LT.FM.ZS","SE.ENR.PRIM.FM.ZS", "SE.ENR.PRSC.FM.ZS", "SE.ENR.SECO.FM.ZS", "SE.ENR.TERT.FM.ZS","CC.EST", "PV.EST","GE.EST", "RQ.EST", "RL.EST","VA.EST","Country Code", "type", "economy", "continent", "region_un", "subregion","region_wb","geometry", "Sum_bd_best", "Sum_bd_low", "Sum_bd_high", "HDI", "GII","EDI","GNI","Investment")))



stat.col.num<-c("SP.DYN.CBRT.IN","SP.DYN.CDRT.IN","SE.SEC.CUAT.LO.ZS","SP.URB.TOTL.IN.ZS","NY.GNP.PCAP.PP.CD","TX.VAL.FUEL.ZS.UN","SH.STA.MMRT","SP.ADO.TFRT","SP.DYN.LE00.IN","NE.EXP.GNFS.ZS","SE.XPD.TOTL.GD.ZS","TX.VAL.MMTL.ZS.UN","SL.UEM.1524.ZS","NY.GNP.PCAP.CD","SM.POP.TOTL.ZS","EN.POP.DNST","SP.POP.GROW","SP.POP.TOTL","SL.TLF.ACTI.1524.MA.ZS",
                "Unemployment_M_F",
                "Population_secondary_education_male_25+",
                "Population_secondary_education_female_25+",
                "Population_secondary_education_25+",
                "mean_years_schooling_male",
                "mean_years_schooling_female",
                "mean_years_schooling",
                "Maternal_mortality_ratio",
                "life_exp_male",
                "life_exp_female",
                "life_exp","Laborforce_male_15+",
                "Laborforce_female_15+",
                "Adolescent_birth_rate",
                "GNI_per_capita_male",
                "GNI_per_capita_female",
                "exp_years_school_female",
                "exp_years_school_male",
                "exp_years_school","SG.GEN.PARL.ZS","equality_dim_index_core", "control_dim_index_core", "total_index_core","SP.POP.2529.FE.5Y","SP.POP.2529.MA.5Y","SP.POP.TOTL.FE.ZS","SP.POP.TOTL.MA.ZS", "SP.POP.2024.MA.5Y", "SP.POP.2024.FE.5Y","Im_Ex","Ineq_Coef","adjHDI","GDI","Year","freedom_dim_index_core","SI.POV.GINI","SL.TLF.ACTI.1524.FE.ZS","SL.TLF.CACT.FE.ZS","SL.TLF.TOTL.FE.ZS","SL.TLF.CACT.FM.ZS","NY.GDP.MKTP.KN","NY.GDP.MKTP.KD.ZG", "NY.GDP.PCAP.CN","NY.GDP.PCAP.KD.ZG","IQ.CPA.PROP.XQ","IQ.CPA.GNDR.XQ","SE.ADT.1524.LT.FM.ZS","SE.ENR.PRIM.FM.ZS", "SE.ENR.PRSC.FM.ZS", "SE.ENR.SECO.FM.ZS", "SE.ENR.TERT.FM.ZS","CC.EST", "PV.EST","GE.EST", "RQ.EST", "RL.EST","VA.EST", "Sum_bd_best", "Sum_bd_low", "Sum_bd_high", "HDI", "GII","EDI","GNI","Investment")
Stat_Data_geo[stat.col.num]<-sapply(Stat_Data_geo[stat.col.num],as.numeric) 

Stat_Data_geo <-Stat_Data_geo%>%
  transform(Konflikt=Sum_bd_best> 24)%>%
  replace_na(list(Konflikt = FALSE)) ## Länder ohne Konfliktote werden als Länder ohne Konflikte definiert. 

###################################### Konflikte auf regionaler, zeitlicher und demokratischer Ebene untersuchen
### Wie viele Konflikte in den Datensätzen?
Data_geo%>%
  count(Konflikt)## 1657 Konflikte, 10443 ohne Konflikte

Stat_Data_geo%>%
  count(Konflikt) ## 954 Konflikte, 10433 ohne Konflikte 

Data_cor_sel%>%
  count(Konflikt) ## 156 Konflikte, 894 ohne Konflikte für die logit Regression

DataICP_sel_na%>%
  count(Konflikt) ## 156 Konflikte, 894 ohne Konflikte für die ICP

## Aufteilung nach Regionen, Ökonomie und Politik

Data_geo%>%
  filter(Konflikt==T)%>%
  count(region_wb)
Data_geo%>%
  filter(Konflikt==T)%>%
  count(economy)
Data_geo%>%
  filter(Konflikt==T)%>%
  count(classification_core)  

###################################### Korrelationen berechnen 
## Vorher wurde auf Miltikollinearität getestet
Data_cor <- Stat_Data_geo%>%
  dplyr::select(one_of("Konflikt","SP.DYN.LE00.IN","NY.GNP.PCAP.PP.CD","equality_dim_index_core","control_dim_index_core","freedom_dim_index_core","SH.STA.MMRT","mean_years_schooling","SP.DYN.CBRT.IN","SP.DYN.CDRT.IN","Population_secondary_education_25.","SP.POP.GROW","EN.POP.DNST","SP.URB.TOTL.IN.ZS","SP.ADO.TFRT","Im_Ex","SP.POP.2024.MA.5Y","SL.TLF.TOTL.FE.ZS","SG.GEN.PARL.ZS","SL.UEM.1524.ZS","TX.VAL.MMTL.ZS.UN","SE.XPD.TOTL.GD.ZS","TX.VAL.FUEL.ZS.UN","NY.GDP.PCAP.CN","total_index_core"))%>%
  drop_na()

subset_cor <- subset(Data_cor, select = c(NY.GNP.PCAP.PP.CD, SH.STA.MMRT, SP.DYN.LE00.IN,equality_dim_index_core,control_dim_index_core,freedom_dim_index_core,mean_years_schooling, SP.DYN.CBRT.IN,SP.DYN.CDRT.IN, Population_secondary_education_25.,  SP.POP.GROW,EN.POP.DNST,SP.URB.TOTL.IN.ZS,SP.ADO.TFRT,Im_Ex,SP.POP.2024.MA.5Y,SL.TLF.TOTL.FE.ZS,SG.GEN.PARL.ZS,SL.UEM.1524.ZS,TX.VAL.MMTL.ZS.UN,TX.VAL.FUEL.ZS.UN,SE.XPD.TOTL.GD.ZS,NY.GDP.PCAP.CN,total_index_core))
cor_tab <- cor(subset_cor, method = "spearman")
cor_tab ## korrelation über 0.8  ist nicht gut-> Variablen aussortieren um Korrealtionen unter den Variablen zu verhindern

### oder mit corrplot
Data_cor_mat <-Data_cor%>%
  dplyr::select(-one_of("Konflikt","total_index_core"))
colnames(Data_cor_mat)<- c("Lebenserwartung", "GNI pro Kopf","Gleichheits-Index","Kontroll-Index","Freiheits-Index","Muttersterblichkeitsrate","Schuljahre(im Mittel)","Geburtenrate","Sterberate","Bev. ü. 25j. mit Grundausbildung","Bev. Wachstum","Bev. Dichte","urbane Bev.","Schwangerschaften bei Jugendlichen","Import/Export","Youth Bulge","Beteiligun F. am Arbeitsmarkt","F. im Parlament","Jugendarbeitslosigkeit","Export: Metalle und Erze","Staatsausgaben Bildung","Exporte: Treibstoffe","GDP pro Kopf")

Cor_mat<-cor(Data_cor_mat)

corrplot(Cor_mat, method = "ellipse",order = "AOE", tl.col = "grey20")


## Einen neuen Datensatz erstellen mit allen Variablen. 

Data_cor_test <- Stat_Data_geo%>%
  dplyr::select(one_of("region_wb","total_index_core","Konflikt","SP.DYN.CBRT.IN","SP.DYN.CDRT.IN","SP.DYN.LE00.IN","Population_secondary_education_25.","NY.GNP.PCAP.PP.CD","SP.POP.GROW","EN.POP.DNST","SH.STA.MMRT","SP.URB.TOTL.IN.ZS","SP.ADO.TFRT","Im_Ex","SP.POP.2024.MA.5Y","SL.TLF.TOTL.FE.ZS","SG.GEN.PARL.ZS","SL.UEM.1524.ZS","TX.VAL.MMTL.ZS.UN","SE.XPD.TOTL.GD.ZS","TX.VAL.FUEL.ZS.UN","NY.GDP.PCAP.CN","mean_years_schooling","equality_dim_index_core","control_dim_index_core","freedom_dim_index_core"))%>% 
  drop_na()
Data_cor_test$Konflikt<-as.numeric(as.factor(Data_cor_test$Konflikt))
  

# Korrelationen der Variablen mit Konflikten Ja/Nein
cor.test(Data_cor_test$freedom_dim_index_core, Data_cor_test$Konflikt)
cor.test(Data_cor_test$control_dim_index_core, Data_cor_test$Konflikt)
cor.test(Data_cor_test$equality_dim_index_core, Data_cor_test$Konflikt)
cor.test(Data_cor_test$exp_years_school, Data_cor_test$Konflikt)
cor.test(Data_cor_test$Im_Ex, Data_cor_test$Konflikt)
cor.test(Data_cor_test$NY.GDP.PCAP.CN, Data_cor_test$Konflikt)#GDP
cor.test(Data_cor_test$SE.XPD.TOTL.GD.ZS, Data_cor_test$Konflikt)# Ausgaben für Bildung am GDP
cor.test(Data_cor_test$TX.VAL.MMTL.ZS.UN, Data_cor_test$Konflikt)# Ores and metals exports (% of merchandise exports)
cor.test(Data_cor_test$SL.UEM.1524.ZS, Data_cor_test$Konflikt)#Unemployment, youth total (% of total labor force ages 15-24 
cor.test(Data_cor_test$EN.POP.DNST, Data_cor_test$Konflikt)# Bevölkerungs Dichte
cor.test(Data_cor_test$SP.POP.GROW, Data_cor_test$Konflikt)# Bevölkerungswachstum
cor.test(Data_cor_test$SP.POP.TOTL, Data_cor_test$Konflikt)# Totale Bev.
cor.test(Data_cor_test$SG.GEN.PARL.ZS, Data_cor_test$Konflikt)# Frauen im Parlament
cor.test(Data_cor_test$Population_secondary_education_25., Data_cor_test$Konflikt)
cor.test(Data_cor_test$SL.TLF.TOTL.FE.ZS, Data_cor_test$Konflikt)
cor.test(Data_cor_test$SP.POP.2024.MA.5Y, Data_cor_test$Konflikt)
cor.test(Data_cor_test$SH.STA.MMRT, Data_cor_test$Konflikt)
cor.test(Data_cor_test$SP.ADO.TFRT, Data_cor_test$Konflikt)
cor.test(Data_cor_test$SP.DYN.LE00.IN, Data_cor_test$Konflikt)
cor.test(Data_cor_test$NY.GNP.PCAP.CD, Data_cor_test$Konflikt)
cor.test(Data_cor_test$mean_years_schooling, Data_cor_test$Konflikt)
cor.test(Data_cor_test$TX.VAL.FUEL.ZS.UN, Data_cor_test$Konflikt)
cor.test(Data_cor_test$SP.URB.TOTL.IN.ZS, Data_cor_test$Konflikt)
cor.test(Data_cor_test$SP.DYN.CBRT.IN, Data_cor_test$Konflikt)
cor.test(Data_cor_test$SP.DYN.CDRT.IN, Data_cor_test$Konflikt)
cor.test(Data_cor_test$total_index_core, Data_cor_test$Konflikt)


###################################### logit Regression berechnen

Data_cor_sel <- Stat_Data_geo%>%
  dplyr::select(one_of("Konflikt","Year","Country.Code","SP.DYN.CDRT.IN","Population_secondary_education_25.","SP.POP.GROW","EN.POP.DNST","SP.URB.TOTL.IN.ZS","SP.ADO.TFRT","Im_Ex","SP.POP.2024.MA.5Y","SL.TLF.TOTL.FE.ZS","SG.GEN.PARL.ZS","SL.UEM.1524.ZS","TX.VAL.MMTL.ZS.UN","SE.XPD.TOTL.GD.ZS","TX.VAL.FUEL.ZS.UN","NY.GDP.PCAP.CN","total_index_core","economy","region_wb","classification_core"))%>%
  drop_na()

summary(Data_cor_sel)


# Konflikt in numerischen Faktor umwandeln und Variablen logarythmieren wegen hoher Spannweite der Werte
Data_cor_sel$Konflikt<- as.factor(as.numeric(Data_cor_sel$Konflikt))
Data_cor_sel$Im_Ex<- log(Data_cor_sel$Im_Ex)
Data_cor_sel$NY.GDP.PCAP.CN<- log(Data_cor_sel$NY.GDP.PCAP.CN)
Data_cor_sel$EN.POP.DNST<- log(Data_cor_sel$EN.POP.DNST)
Data_cor_sel$SP.ADO.TFRT<- log(Data_cor_sel$SP.ADO.TFRT)

# Formula erstellen für die Regerssion. 

Formula_logit<- Konflikt~SP.DYN.CDRT.IN+Population_secondary_education_25.+SP.POP.GROW+EN.POP.DNST+SP.URB.TOTL.IN.ZS+SP.ADO.TFRT+Im_Ex+SP.POP.2024.MA.5Y+SL.TLF.TOTL.FE.ZS+SG.GEN.PARL.ZS+SL.UEM.1524.ZS+TX.VAL.MMTL.ZS.UN+TX.VAL.FUEL.ZS.UN+SE.XPD.TOTL.GD.ZS+NY.GDP.PCAP.CN+total_index_core
#Formula_logit2<- partic !="Konflikt"~SP.DYN.CDRT.IN+Population_secondary_education_25.+SP.POP.GROW+EN.POP.DNST+SP.URB.TOTL.IN.ZS+SP.ADO.TFRT+Im_Ex+SP.POP.2024.MA.5Y+SL.TLF.TOTL.FE.ZS+SG.GEN.PARL.ZS+SL.UEM.1524.ZS+TX.VAL.MMTL.ZS.UN+TX.VAL.FUEL.ZS.UN+SE.XPD.TOTL.GD.ZS+NY.GDP.PCAP.CN+total_index_core

# logit Regression mit Koeffizienten ausgeben

Data_logit_reg <- glm(Formula_logit, data = Data_cor_sel, family = binomial("logit"))
summary(Data_logit_reg)
print(Data_logit_reg$coefficients)




## modellgrüßemaß -> Subsets erstellen und gucken, wie viel Werte die Regression vorhersagt.  
Subset_logit_reg1 <- subset(Data_cor_sel, split="TRUE")
Subset_logit_reg2 <- subset(Data_cor_sel, split="FALSE")

res <- predict(Data_logit_reg,Subset_logit_reg1,type="response")
res
res <- predict(Data_logit_reg,Subset_logit_reg2,type="response")
res

confmatrix <- table(actual_value = Subset_logit_reg1$Konflikt, Predicted_value= res>0.5)
confmatrix ### Konfusionsmatrix ein Nullmodell gegen das ausgerechnete Modell laufen lassen. Ein Cut off von 0.5. Zeigt wie viel Werte erklärt werden
(confmatrix[[1,1]]+confmatrix[[2,2]])/sum(confmatrix) # das logit Modell sagt 87% der Werte richtig vorraus. 87 % der Vorhersagen treten tatsächlich so ein



#### Cross Validation

Data_logit_reg_cv <- train(Formula_logit, Data_cor_sel, family = binomial("logit"), trControl = trainControl(
  method = "cv", number = 10, verboseIter = TRUE)) ## bis 93 Accuracy

###################################### auf Güte testen

## Omnibus test
glm_chi<- Data_logit_reg$null.deviance- Data_logit_reg$deviance
chiglm <- Data_logit_reg$df.null - Data_logit_reg$df.residual
chiglmsqd <- 1-pchisq(glm_chi,chiglm)## direkt Null. Weil das Modell nur 5 Ziffern nach dem Komma angibt. sehr gut


## Odds Ratio
exp(cbind(OR= coef(Data_logit_reg), confint(Data_logit_reg))) ## Erklärt die Richtung und stärke des Zusammenhangs

## Gütemaß
n<- length(Data_logit_reg$residuals)
R2cs <- 1-exp((Data_logit_reg$deviance - Data_logit_reg$null.deviance)/n)
R2n <- R2cs/(1-exp(-(Data_logit_reg$null.deviance/n))) ## 0.44 pseudo R2, Nagelkerker  
nagelkerke(Data_logit_reg)

## pROC Curve
par(pty="s")
roc(Data_cor_sel$Konflikt, Data_logit_reg$fitted.values, plot = T) ## Area under the Cure 0.836. guter Wert!. Beschreibt die Falsch positiven und falsch negativen Vorhersage. Rate der Falsch-positiven und richtig-positiven wird gegenüber gestellt

## Modell Dagnostik
pred.prop <- predict(Data_logit_reg, Data_cor_sel, type = "response")
pred.Logit <- log(pred.prop/(1- pred.prop))
hist(pred.Logit)
plot(Data_cor_sel$EN.POP.DNST, pred.Logit) #Liniarität der Daten

plot(Data_logit_reg, which = 4, id.n = 3) ## Cooks distance: Obersation 656 und 654 und 920 überprüfen. Einlussreiche beobachtung

coeftest(Data_logit_reg, vcov= vcovHC(Data_logit_reg, type = "HC1")) ## Heteroskedastizitätsrobuster Standardfehler -> Prüft auf Invarianzen entlang der Gerade -> verändert ie signifikanzen der Varaiblen. Aber nicht so ausschlaggebend

## Avarage marginal effekts

## logliklihood abfragen logLik
logLik(Data_logit_reg) ### was bedeutet -97.?? -> 

### auf Multikollinearität testen
vif(Data_logit_reg) # größer als 10 zeigt Multikollinearität
1/vif(Data_logit_reg) #  kleiner als 0.1 zeigt Multikollinearität

##Pseudo R²
nagelkerke(Data_logit_reg)


###################################### Invaraint Causal Prediction mit versteckten Variablen
# gleicher Datensatz wie Data_cor_sel für die Regression, nur mit Environment Variablen für die ICP
Data_ICP_sel <- Stat_Data_geo%>%
  dplyr::select(one_of("SP.DYN.CDRT.IN" ,"Population_secondary_education_25.","SP.POP.GROW","EN.POP.DNST","SP.URB.TOTL.IN.ZS","SP.ADO.TFRT","Im_Ex","SP.POP.2024.MA.5Y","SL.TLF.TOTL.FE.ZS","SG.GEN.PARL.ZS","SL.UEM.1524.ZS","TX.VAL.MMTL.ZS.UN","SE.XPD.TOTL.GD.ZS","TX.VAL.FUEL.ZS.UN","NY.GDP.PCAP.CN","total_index_core","Konflikt","region_wb","Year","Country.Code","economy"))

DataICP_sel_na <- Data_ICP_sel%>%
  drop_na()

DataICP_sel_na$Konflikt<-as.numeric(as.factor(DataICP_sel_na$Konflikt))
DataICP_sel_na$NY.GDP.PCAP.CN<- log(DataICP_sel_na$NY.GDP.PCAP.CN)
DataICP_sel_na$EN.POP.DNST<- log(DataICP_sel_na$EN.POP.DNST)
DataICP_sel_na$SP.ADO.TFRT<- log(DataICP_sel_na$SP.ADO.TFRT)
DataICP_sel_na$Im_Ex<- log(DataICP_sel_na$Im_Ex)

# Matrix aus den Prediktor Variablen
X_sel<-DataICP_sel_na%>%
  dplyr::select(-one_of("Country.Code","Year","Konflikt","region_wb","economy"))%>%
  as.matrix()
# Konflikte als Zielvariable
Y_sel<- DataICP_sel_na%>%
  pull(Konflikt)
# Regionen der World Bank als Interventionen  
ExpInd_sel_reg <- DataICP_sel_na%>%
  pull(region_wb)
ExpInd_sel_reg<- as.numeric(as.factor(ExpInd_sel_reg))


# hidden ICP mit der Region_wb
hidICP_sel<-hiddenICP(X_sel,Y_sel,ExpInd_sel_reg)
print(hidICP_sel)
print(hidICP_sel$betahat)
print(hidICP_sel$maximinCoefficients)

str(DataICP_sel_na)


#### normale ICP World Bank Regionen
Data_ICP_sel_nr <- Stat_Data_geo%>%
  dplyr::select(one_of("SP.DYN.CDRT.IN" ,"Population_secondary_education_25.","SP.POP.GROW","EN.POP.DNST","SP.URB.TOTL.IN.ZS","SP.ADO.TFRT","Im_Ex","SP.POP.2024.MA.5Y","SL.TLF.TOTL.FE.ZS","SG.GEN.PARL.ZS","SL.UEM.1524.ZS","TX.VAL.MMTL.ZS.UN","SE.XPD.TOTL.GD.ZS","TX.VAL.FUEL.ZS.UN","NY.GDP.PCAP.CN","total_index_core","Konflikt","region_wb","Year","Country.Code","economy"))

DataICP_sel_na_nr <- Data_ICP_sel_nr%>%
  drop_na()

DataICP_sel_na_nr$Konflikt<-as.numeric(as.factor(DataICP_sel_na_nr$Konflikt))
DataICP_sel_na_nr$NY.GDP.PCAP.CN<- log(DataICP_sel_na_nr$NY.GDP.PCAP.CN)
DataICP_sel_na_nr$EN.POP.DNST<- log(DataICP_sel_na_nr$EN.POP.DNST)
DataICP_sel_na_nr$SP.ADO.TFRT<- log(DataICP_sel_na_nr$SP.ADO.TFRT)

# Matrix aus den Prediktor Variablen
X_sel_nr<-DataICP_sel_na_nr%>%
  dplyr::select(-one_of("Country.Code","Year","Konflikt","region_wb","economy"))%>%
  as.matrix()
# Konflikte als Zielvariable
Y_sel_nr<- DataICP_sel_na_nr%>%
  pull(Konflikt)
# Regionen der World Bank als Interventionen  
ExpInd_sel_reg_nr <- DataICP_sel_na_nr%>%
  pull(region_wb)
ExpInd_sel_reg_nr<- as.numeric(as.factor(ExpInd_sel_reg_nr))

ICP_reg_wb <-InvariantCausalPrediction::ICP(X_sel_nr,Y_sel_nr,ExpInd_sel_reg_nr, showAcceptedSets = T) ### Unter gleichen Umständen werden die Bev Dichte, Urbane Bev sehr stark signifikant und der Anteil der Frauen am Arbeirsmarkt sowie die Treibstoff Exporte signifikant 
summary(ICP_reg_wb)
plot(ICP_reg_wb)

# *** 1% complete: tested 2 of 255 sets of variables 
# accepted set of variables 4,5,9,14
# accepted set of variables 4,5,9,15
# accepted set of variables 4,5,14,15
# accepted set of variables 4,5,7,9,14
# accepted set of variables 4,5,9,11,14
# accepted set of variables 4,5,9,12,14
# accepted set of variables 4,5,7,14,15
# accepted set of variables 4,5,9,14,15
# accepted set of variables 4,5,11,14,15
# accepted set of variables 4,5,12,14,15
# accepted set of variables 4,5,9,11,12,14
# accepted set of variables 4,5,7,9,14,15
# accepted set of variables 4,5,7,11,14,15
# accepted set of variables 4,5,9,11,14,15
# accepted set of variables 4,5,9,12,14,15
# accepted set of variables 4,5,11,12,14,15
# accepted set of variables 4,5,7,9,11,14,15
# accepted set of variables 4,5,7,9,12,14,15
# accepted set of variables 4,5,9,11,12,14,15     das sind die von der normalen ICP akzeptierten Sets



#### Im Folgenden wird das Ergebnis der ICP mit Hilfe von logistischen Regressionen in den World Bank Regionen untersucht. Jede logistische Regression wird auf ihre Güte überprüft.

# Logit in Sub-Sahara Afrika
Data_cor_Subasahara <- Stat_Data_geo%>%
  filter(region_wb == "Sub-Saharan Africa")%>%
  dplyr::select(one_of("Konflikt","Year","SP.DYN.CDRT.IN","Population_secondary_education_25.","SP.POP.GROW","EN.POP.DNST","SP.URB.TOTL.IN.ZS","SP.ADO.TFRT","Im_Ex","SP.POP.2024.MA.5Y","SL.TLF.TOTL.FE.ZS","SG.GEN.PARL.ZS","SL.UEM.1524.ZS","TX.VAL.MMTL.ZS.UN","SE.XPD.TOTL.GD.ZS","TX.VAL.FUEL.ZS.UN","NY.GDP.PCAP.CN","total_index_core","economy","region_wb"))%>%
  drop_na()

# Konflikt in numerischen Faktor umwandeln und Variablen logarythmieren wegen hoher Spannweite der Werte
Data_cor_Subasahara$Konflikt<- as.factor(as.numeric(Data_cor_Subasahara$Konflikt))
Data_cor_Subasahara$Im_Ex<- log(Data_cor_Subasahara$Im_Ex)
Data_cor_Subasahara$NY.GDP.PCAP.CN<- log(Data_cor_Subasahara$NY.GDP.PCAP.CN)
Data_cor_Subasahara$EN.POP.DNST<- log(Data_cor_Subasahara$EN.POP.DNST)
Data_cor_Subasahara$SP.ADO.TFRT<- log(Data_cor_Subasahara$SP.ADO.TFRT)

# logit Regression mit Koeffizienten ausgeben


Data_logit_subsahra <- glm(Formula_logit, data = Data_cor_Subasahara, family = binomial("logit"))
summary(Data_logit_subsahra)

#### Güte der Logit Subsahara Afrika. geht nicht


# Chi quadrat test
logitchisq_subsahara <- chisq.test(table(actual_value = Subset_subsahara1$Konflikt))
print(logitchisq_subsahara)


#### Cross Validation

Data_subsahara_cv <- train(Formula_logit, Data_logit_subsahra, family = binomial("logit"), trControl = trainControl(
  method = "cv", number = 10, verboseIter = TRUE)) 
nagelkerke(Data_logit_subsahra)


## Odds Ratio
exp(cbind(OR= coef(Data_logit_subsahra), confint(Data_logit_subsahra))) ## Erklärt die Richtung und stärke des Zusammenhangs

## Gütemaß
n<- length(Data_logit_subsahra$residuals)
R2cs <- 1-exp((Data_logit_subsahra$deviance - Data_logit_reg$null.deviance)/n)
R2n <- R2cs/(1-exp(-(Data_logit_subsahra$null.deviance/n))) ## 0.44 pseudo R2, Nagelkerker  
rcompanion::nagelkerke(Data_logit_subsahra)


# Mittlerer Osten und Nord Afrika

Data_cor_mideastnordafr <- Stat_Data_geo%>%
  filter(region_wb == "Middle East & North Africa")%>%
  dplyr::select(one_of("Konflikt","Year","SP.DYN.CDRT.IN","Population_secondary_education_25.","SP.POP.GROW","EN.POP.DNST","SP.URB.TOTL.IN.ZS","SP.ADO.TFRT","Im_Ex","SP.POP.2024.MA.5Y","SL.TLF.TOTL.FE.ZS","SG.GEN.PARL.ZS","SL.UEM.1524.ZS","TX.VAL.MMTL.ZS.UN","SE.XPD.TOTL.GD.ZS","TX.VAL.FUEL.ZS.UN","NY.GDP.PCAP.CN","total_index_core","economy","region_wb"))%>%
  drop_na()

# Konflikt in numerischen Faktor umwandeln und Variablen logarythmieren wegen hoher Spannweite der Werte
Data_cor_mideastnordafr$Konflikt<- as.factor(as.numeric(Data_cor_mideastnordafr$Konflikt))
Data_cor_mideastnordafr$Im_Ex<- log(Data_cor_mideastnordafr$Im_Ex)
Data_cor_mideastnordafr$NY.GDP.PCAP.CN<- log(Data_cor_mideastnordafr$NY.GDP.PCAP.CN)
Data_cor_mideastnordafr$EN.POP.DNST<- log(Data_cor_mideastnordafr$EN.POP.DNST)
Data_cor_mideastnordafr$SP.ADO.TFRT<- log(Data_cor_mideastnordafr$SP.ADO.TFRT)

# logit Regression mit Koeffizienten ausgeben

Data_logit_mideastnordafr <- glm(Formula_logit, data = Data_cor_mideastnordafr, family = binomial("logit"))
summary(Data_logit_mideastnordafr)


nagelkerke(Data_logit_mideastnordafr)
Data_mideastnordafr_cv <- train(Formula_logit, Data_cor_mideastnordafr, family = binomial("logit"), trControl = trainControl(
  method = "cv", number = 10, verboseIter = TRUE)) 

exp(cbind(OR= coef(Data_logit_mideastnordafr), confint(Data_logit_mideastnordafr))) ## Erklärt die Richtung und stärke des Zusammenhangs

# Europa und Zentralsien

Data_cor_eurozenasi <- Stat_Data_geo%>%
  filter(region_wb == "Europe & Central Asia")%>%
  dplyr::select(one_of("Konflikt","Year","SP.DYN.CDRT.IN","Population_secondary_education_25.","SP.POP.GROW","EN.POP.DNST","SP.URB.TOTL.IN.ZS","SP.ADO.TFRT","Im_Ex","SP.POP.2024.MA.5Y","SL.TLF.TOTL.FE.ZS","SG.GEN.PARL.ZS","SL.UEM.1524.ZS","TX.VAL.MMTL.ZS.UN","SE.XPD.TOTL.GD.ZS","TX.VAL.FUEL.ZS.UN","NY.GDP.PCAP.CN","total_index_core","economy","region_wb"))%>%
  drop_na()

# Konflikt in numerischen Faktor umwandeln und Variablen logarythnmieren wegen hoher Spannweite der Werte
Data_cor_eurozenasi$Konflikt<- as.factor(as.numeric(Data_cor_eurozenasi$Konflikt))
Data_cor_eurozenasi$Im_Ex<- log(Data_cor_eurozenasi$Im_Ex)
Data_cor_eurozenasi$NY.GDP.PCAP.CN<- log(Data_cor_eurozenasi$NY.GDP.PCAP.CN)
Data_cor_eurozenasi$EN.POP.DNST<- log(Data_cor_eurozenasi$EN.POP.DNST)
Data_cor_eurozenasi$SP.ADO.TFRT<- log(Data_cor_eurozenasi$SP.ADO.TFRT)

# logit Regression mit Koeffizienten ausgeben

Data_logit_eurozenasi <- glm(Formula_logit, data = Data_cor_eurozenasi, family = binomial("logit"))
summary(Data_logit_eurozenasi)
#### Cross Validation

Data_eurozenas_cv <- train(Formula_logit, Data_cor_eurozenasi, family = binomial("logit"), trControl = trainControl(
  method = "cv", number = 10, verboseIter = TRUE)) 
nagelkerke(Data_logit_eurozenasi)

exp(cbind(OR= coef(Data_logit_eurozenasi), confint(Data_logit_eurozenasi))) ## Erklärt die Richtung und stärke des Zusammenhangs


# Nordamerika
Data_cor_nordame <- Stat_Data_geo%>%
  filter(region_wb == "North America")%>%
  dplyr::select(one_of("Konflikt","Year","SP.DYN.CDRT.IN","Population_secondary_education_25.","SP.POP.GROW","EN.POP.DNST","SP.URB.TOTL.IN.ZS","SP.ADO.TFRT","Im_Ex","SP.POP.2024.MA.5Y","SL.TLF.TOTL.FE.ZS","SG.GEN.PARL.ZS","SL.UEM.1524.ZS","TX.VAL.MMTL.ZS.UN","SE.XPD.TOTL.GD.ZS","TX.VAL.FUEL.ZS.UN","NY.GDP.PCAP.CN","total_index_core","economy","region_wb"))%>%
  drop_na()

# Konflikt in numerischen Faktor umwandeln und Variablen logarythmieren wegen hoher Spannweite der Werte
Data_cor_nordame$Konflikt<- as.factor(as.numeric(Data_cor_nordame$Konflikt))
Data_cor_nordame$Im_Ex<- log(Data_cor_nordame$Im_Ex)
Data_cor_nordame$NY.GDP.PCAP.CN<- log(Data_cor_nordame$NY.GDP.PCAP.CN)
Data_cor_nordame$EN.POP.DNST<- log(Data_cor_nordame$EN.POP.DNST)
Data_cor_nordame$SP.ADO.TFRT<- log(Data_cor_nordame$SP.ADO.TFRT)

# logit Regression mit Koeffizienten ausgeben

Data_logit_nordame <- glm(Formula_logit, data = Data_cor_nordame, family = binomial("logit"))
summary(Data_logit_nordame)

Data_nordam_cv <- train(Formula_logit, Data_cor_nordame, family = binomial("logit"), trControl = trainControl(
  method = "cv", number = 10, verboseIter = TRUE)) 
nagelkerke(Data_logit_nordame)

# Lateinamerika und Karibik
Data_cor_latamekar <- Stat_Data_geo%>%
  filter(region_wb == "Latin America & Caribbean")%>%
  dplyr::select(one_of("Konflikt","Year","SP.DYN.CDRT.IN","Population_secondary_education_25.","SP.POP.GROW","EN.POP.DNST","SP.URB.TOTL.IN.ZS","SP.ADO.TFRT","Im_Ex","SP.POP.2024.MA.5Y","SL.TLF.TOTL.FE.ZS","SG.GEN.PARL.ZS","SL.UEM.1524.ZS","TX.VAL.MMTL.ZS.UN","SE.XPD.TOTL.GD.ZS","TX.VAL.FUEL.ZS.UN","NY.GDP.PCAP.CN","total_index_core","economy","region_wb"))%>%
  drop_na()

# Konflikt in numerischen Faktor umwandeln und Variablen logarythmieren wegen hoher Spannweite der Werte
Data_cor_latamekar$Konflikt<- as.factor(as.numeric(Data_cor_latamekar$Konflikt))
Data_cor_latamekar$Im_Ex<- log(Data_cor_latamekar$Im_Ex)
Data_cor_latamekar$NY.GDP.PCAP.CN<- log(Data_cor_latamekar$NY.GDP.PCAP.CN)
Data_cor_latamekar$EN.POP.DNST<- log(Data_cor_latamekar$EN.POP.DNST)
Data_cor_latamekar$SP.ADO.TFRT<- log(Data_cor_latamekar$SP.ADO.TFRT)

# logit Regression mit Koeffizienten ausgeben

Data_logit_latamekar <- glm(Formula_logit, data = Data_cor_latamekar, family = binomial("logit"))
summary(Data_logit_latamekar)
nagelkerke(Data_logit_latamekar)

test_latamekar<-logistf(Formula_logit, data = Data_cor_latamekar, pl=FALSE, family = binomial("logit"))
summary(test_latamekar)# signfikanter ML

#Cross Validation
Data_latkab_cv <- train(Formula_logit, Data_cor_latamekar, family = binomial("logit"), trControl = trainControl(
  method = "cv", number = 10, verboseIter = TRUE))

#Pseudo R²
nagelkerke(Data_logit_latamekar)

# Süd Asien

Data_cor_southasia <- Stat_Data_geo%>%
  filter(region_wb == "South Asia")%>%
  dplyr::select(one_of("Konflikt","Year","SP.DYN.CDRT.IN","Population_secondary_education_25.","SP.POP.GROW","EN.POP.DNST","SP.URB.TOTL.IN.ZS","SP.ADO.TFRT","Im_Ex","SP.POP.2024.MA.5Y","SL.TLF.TOTL.FE.ZS","SG.GEN.PARL.ZS","SL.UEM.1524.ZS","TX.VAL.MMTL.ZS.UN","SE.XPD.TOTL.GD.ZS","TX.VAL.FUEL.ZS.UN","NY.GDP.PCAP.CN","total_index_core","economy","region_wb"))%>%
  drop_na()

# Konflikt in numerischen Faktor umwandeln und Variablen logarythmieren wegen hoher Spannweite der Werte
Data_cor_southasia$Konflikt<- as.factor(as.numeric(Data_cor_southasia$Konflikt))
Data_cor_southasia$Im_Ex<- log(Data_cor_southasia$Im_Ex)
Data_cor_southasia$NY.GDP.PCAP.CN<- log(Data_cor_southasia$NY.GDP.PCAP.CN)
Data_cor_southasia$EN.POP.DNST<- log(Data_cor_southasia$EN.POP.DNST)
Data_cor_southasia$SP.ADO.TFRT<- log(Data_cor_southasia$SP.ADO.TFRT)

# logit Regression mit Koeffizienten ausgeben

Data_logit_southasia <- glm(Formula_logit, data = Data_cor_southasia, family = binomial("logit"))
summary(Data_logit_southasia)

test_southasia<-logistf(Formula_logit, data = Data_cor_southasia, pl=FALSE, family = binomial("logit"))
summary(test_southasia)# nicht signifikant

#Cross Validation
Data_südas_cv <- train(Formula_logit, Data_cor_southasia, family = binomial("logit"), trControl = trainControl(
  method = "cv", number = 10, verboseIter = TRUE))

#Pseudo R²
nagelkerke(Data_logit_southasia)

# Ost-asien und Pazifik

Data_cor_osasiepaz <- Stat_Data_geo%>%
  filter(region_wb == "East Asia & Pacific")%>%
  dplyr::select(one_of("Konflikt","Year","SP.DYN.CDRT.IN","Population_secondary_education_25.","SP.POP.GROW","EN.POP.DNST","SP.URB.TOTL.IN.ZS","SP.ADO.TFRT","Im_Ex","SP.POP.2024.MA.5Y","SL.TLF.TOTL.FE.ZS","SG.GEN.PARL.ZS","SL.UEM.1524.ZS","TX.VAL.MMTL.ZS.UN","SE.XPD.TOTL.GD.ZS","TX.VAL.FUEL.ZS.UN","NY.GDP.PCAP.CN","total_index_core","economy","region_wb"))%>%
  drop_na()

# Konflikt in numerischen Faktor umwandeln und Variablen logarythmieren wegen hoher Spannweite der Werte
Data_cor_osasiepaz$Konflikt<- as.factor(as.numeric(Data_cor_osasiepaz$Konflikt))
Data_cor_osasiepaz$Im_Ex<- log(Data_cor_osasiepaz$Im_Ex)
Data_cor_osasiepaz$NY.GDP.PCAP.CN<- log(Data_cor_osasiepaz$NY.GDP.PCAP.CN)
Data_cor_osasiepaz$EN.POP.DNST<- log(Data_cor_osasiepaz$EN.POP.DNST)
Data_cor_osasiepaz$SP.ADO.TFRT<- log(Data_cor_osasiepaz$SP.ADO.TFRT)

# logit Regression mit Koeffizienten ausgeben

Data_logit_osasiepaz <- glm(Formula_logit, data = Data_cor_osasiepaz, family = binomial("logit"))
summary(Data_logit_osasiepaz)

# Pseudo R²
nagelkerke(Data_logit_osasiepaz)

#Cross Validation
Data_osaspaz_cv <- train(Formula_logit, Data_cor_osasiepaz, family = binomial("logit"), trControl = trainControl(
  method = "cv", number = 10, verboseIter = TRUE))


########## Im Folgenden sind werden die geografischen Regionen der United Nations als Interventionsvariable geprüft Und logistische Regressionen pro Region durchgeführt 
Data_cor_logit_un <- Stat_Data_geo%>%
  dplyr::select(one_of("Konflikt","Year","SP.DYN.CDRT.IN","Population_secondary_education_25.","SP.POP.GROW","EN.POP.DNST","SP.URB.TOTL.IN.ZS","SP.ADO.TFRT","Im_Ex","SP.POP.2024.MA.5Y","SL.TLF.TOTL.FE.ZS","SG.GEN.PARL.ZS","SL.UEM.1524.ZS","TX.VAL.MMTL.ZS.UN","SE.XPD.TOTL.GD.ZS","TX.VAL.FUEL.ZS.UN","NY.GDP.PCAP.CN","total_index_core","economy","region_un"))%>%
  drop_na()

Data_cor_logit_un$Konflikt<- as.factor(as.numeric(Data_cor_logit_un$Konflikt))
Data_cor_logit_un$Im_Ex<- log(Data_cor_logit_un$Im_Ex)
Data_cor_logit_un$NY.GDP.PCAP.CN<- log(Data_cor_logit_un$NY.GDP.PCAP.CN)
Data_cor_logit_un$EN.POP.DNST<- log(Data_cor_logit_un$EN.POP.DNST)
Data_cor_logit_un$SP.ADO.TFRT<- log(Data_cor_logit_un$SP.ADO.TFRT)

## Asia n= 234
Data_cor_logit_asia <-Data_cor_logit_un%>%
  filter(region_un == "Asia")
Data_logit_asia <- glm(Formula_logit, data = Data_cor_logit_asia, family = binomial("logit"))
summary(Data_logit_asia)
Data_logit_asia2<-logistf(Formula_logit, Data_cor_logit_asia)
summary(Data_logit_asia2)

## Africa n= 307

Data_cor_logit_africa <-Data_cor_logit_un%>%
  filter(region_un == "Africa")
Data_logit_africa <- glm(Formula_logit, data = Data_cor_logit_africa, family = binomial("logit"))
summary(Data_logit_africa)

### Europe n = 307; komisch keine signifikanzen
Data_cor_logit_europe <-Data_cor_logit_un%>%
  filter(region_un == "Europe")
Data_logit_europe <- glm(Formula_logit, data = Data_cor_logit_europe, family = binomial("logit"))
summary(Data_logit_europe)
Data_logit_europe2<-logistf(Formula_logit, Data_cor_logit_europe )
summary(Data_logit_europe2)

### Americas n = 181; alles stark signifikant
Data_cor_logit_americas <-Data_cor_logit_un%>%
  filter(region_un == "Americas")
Data_logit_americas <- glm(Formula_logit, data = Data_cor_logit_americas, family = binomial("logit"))
summary(Data_logit_americas)
Data_logit_americas2<-logistf(Formula_logit, Data_cor_logit_americas )
summary(Data_logit_americas2)

### Ozeania n = 21; nichts signifikant, aber nich tkomischn, wegen zu kleinem n
Data_cor_logit_ozeania <-Data_cor_logit_un%>%
  filter(region_un == "Oceania")
Data_logit_ozeania <- glm(Formula_logit, data = Data_cor_logit_ozeania, family = binomial("logit"))
summary(Data_logit_ozeania)
Data_logit_ozeania2<-logistf(Formula_logit, Data_cor_logit_ozeania)
summary(Data_logit_ozeania2)


### Mit den UN-Regionen versuchen (logitnach Regionen, logit und ICP)

##### icp Un Regionen
Data_ICP_sel_un <- Stat_Data_geo%>%
  dplyr::select(one_of("SP.DYN.CDRT.IN" ,"Population_secondary_education_25.","SP.POP.GROW","EN.POP.DNST","SP.URB.TOTL.IN.ZS","SP.ADO.TFRT","Im_Ex","SP.POP.2024.MA.5Y","SL.TLF.TOTL.FE.ZS","SG.GEN.PARL.ZS","SL.UEM.1524.ZS","TX.VAL.MMTL.ZS.UN","SE.XPD.TOTL.GD.ZS","TX.VAL.FUEL.ZS.UN","NY.GDP.PCAP.CN","total_index_core","Konflikt","region_wb","Year","Country.Code","economy","region_un"))

DataICP_sel_na_un <- Data_ICP_sel_un%>%
  drop_na()

DataICP_sel_na_un$Konflikt<-as.numeric(as.factor(DataICP_sel_na_un$Konflikt))
DataICP_sel_na_un$NY.GDP.PCAP.CN<- log(DataICP_sel_na_un$NY.GDP.PCAP.CN)
DataICP_sel_na_un$EN.POP.DNST<- log(DataICP_sel_na_un$EN.POP.DNST)
DataICP_sel_na_un$SP.ADO.TFRT<- log(DataICP_sel_na_un$SP.ADO.TFRT)

# Matrix aus den Prediktor Variablen
X_sel_un<-DataICP_sel_na_un%>%
  dplyr::select(-one_of("Country.Code","Year","Konflikt","region_wb","economy","region_un"))%>%
  as.matrix()
# Konflikte als Zielvariable
Y_sel_un<- DataICP_sel_na_un%>%
  pull(Konflikt)
# Regionen der World Bank als Interventionen  
ExpInd_sel_reg_un <- DataICP_sel_na_un%>%
  pull(region_un)
ExpInd_sel_reg_un<- as.numeric(as.factor(ExpInd_sel_reg_un))

ICP_reg_un <-InvariantCausalPrediction::ICP(X_sel_un,Y_sel_un,ExpInd_sel_reg_un) ### Unter gleichen Umständen werden die Bev Dichte, Urbane Bev sehr stark signifikant und der Anteil der Frauen am Arbeirsmarkt sowie die Treibstoff Exporte signifikant 

summary(ICP_reg_un)
plot(ICP_reg_un) #-> Exporte von Treibstoffen, Anteil der urbanen Bevölkerung und die Bevölkerungsdichte zeigen einen signifikanten Zusammenhang

#### Eine weitere Interventionsvariable wird geprüft. Aufgrund der wenigen Fallzahl in ein paar Regionen, werden die Regionen wiefolgt aufgeteilt:
# Südasien, Mittlerer Osten und Nordafrika, Sub-Sahara Afrika sind Environment 2; Europa und Zentralasien, Lateinamerika und Karibik, Ostasien und Pazifik und Nordamerika sind Environment 2
# So können mehr Fälle pro Environment genutzt werden. Eine geringere Anzhal an Environments kann bessere Ergebnisse liefern

# Aufteilung der Regionen und logarithmieren
data_cor_merge <- DataICP_sel_na%>%
  mutate(region_new= region_wb)
data_cor_merge$region_new <- as.numeric(as.factor(data_cor_merge$region_new))

data_cor_merge$region_new<- replace(data_cor_merge$region_new,data_cor_merge$region_new<4,1)
data_cor_merge$region_new<- replace(data_cor_merge$region_new,data_cor_merge$region_new>3,2)

data_cor_merge$Konflikt<-as.numeric(as.factor(data_cor_merge$Konflikt))
data_cor_merge$NY.GDP.PCAP.CN<- log(data_cor_merge$NY.GDP.PCAP.CN)
data_cor_merge$EN.POP.DNST<- log(data_cor_merge$EN.POP.DNST)
data_cor_merge$SP.ADO.TFRT<- log(data_cor_merge$SP.ADO.TFRT)
#data_cor_merge$Im_Ex<- log(data_cor_merge$Im_Ex)

# Durchführung der ICP

# Matrix aus den Prediktor Variablen
X_sel_new<-data_cor_merge%>%
  dplyr::select(-one_of("Country.Code","Year","Konflikt","region_wb","economy","region_new"))%>%
  as.matrix()
# Konflikte als Zielvariable
Y_sel_new<- data_cor_merge%>%
  pull(Konflikt)
# Regionen der World Bank als Interventionen  
ExpInd_sel_new <- data_cor_merge%>%
  pull(region_new)

#ExpInd_sel_new<- as.numeric(as.factor(ExpInd_sel_new))



ICP2ev<-InvariantCausalPrediction::ICP(X=X_sel_new,Y=Y_sel_new,ExpInd= ExpInd_sel_new)
summary(ICP2ev) #-> nur der Anteil der urbanen Bevölkerung hat einen signifikanten kausalen Zusammenhang mit dem Auftreten von Konflikten


### Für die beiden Environments werden logistische Regressionen durchgeführt
#Environment 1
data_cor_merge1 <- Data_cor_sel%>%
  mutate(region_new= region_wb)
data_cor_merge1$region_new <- as.numeric(as.factor(data_cor_merge1$region_new))

data_cor_merge1$region_new<- replace(data_cor_merge1$region_new,data_cor_merge1$region_new<4,1)
data_cor_merge1$region_new<- replace(data_cor_merge1$region_new,data_cor_merge1$region_new>3,2)

data_cor_logit1 <- data_cor_merge1%>%
  filter(region_new== 1)

Data_logit_new1 <- glm(Formula_logit, data_cor_logit1, family = binomial("logit"))
summary(Data_logit_new1)

# Environment 2
data_cor_merge2 <- Data_cor_sel%>%
  mutate(region_new= region_wb)
data_cor_merge2$region_new <- as.numeric(as.factor(data_cor_merge2$region_new))

data_cor_merge2$region_new<- replace(data_cor_merge2$region_new,data_cor_merge2$region_new<4,1)
data_cor_merge2$region_new<- replace(data_cor_merge2$region_new,data_cor_merge2$region_new>3,2)

data_cor_logit2 <- data_cor_merge2%>%
  filter(region_new== 2)

Data_logit_new2 <- glm(Formula_logit, data_cor_logit2, family = binomial("logit"))
summary(Data_logit_new2)


### Im folgenden wird die Verteilung der fehlenden Werte über die geografischen und ökonomischen Regionen analysiert und dargestellt. -> Daten für den Anhang


Data_cor_sel_mitNA <- Stat_Data_geo%>%
  dplyr::select(one_of("Konflikt","Year","SP.DYN.CDRT.IN","Population_secondary_education_25.","SP.POP.GROW","EN.POP.DNST","SP.URB.TOTL.IN.ZS","SP.ADO.TFRT","Im_Ex","SP.POP.2024.MA.5Y","SL.TLF.TOTL.FE.ZS","SG.GEN.PARL.ZS","SL.UEM.1524.ZS","TX.VAL.MMTL.ZS.UN","SE.XPD.TOTL.GD.ZS","TX.VAL.FUEL.ZS.UN","NY.GDP.PCAP.CN","total_index_core","region_wb","economy"))
colnames(Data_cor_sel_mitNA)<- c("Konflikt","Jahr","Todsrate","Bev. ü. 25J mit Grundausbildung","Bev. Wachstum","Bev. Dichte","urbane Bev.","Schwangerschaften bei Jugendlichen","Import/Export","Youth Bulge","F. am Arbeitsmarkt","F.im Parlament","Jugendarbeitslosigkeit","Export: Erze u.Metalle","staatliche Bildungsausgaben","Export: Treibstoffe","GDP pro Kopf","Demokratieindex","Region (World Bank)","ökonomische Regionen (World Bank)")

gg_miss_var(Data_cor_sel_mitNA, show_pct = T)+
  labs(y= "Prädiktorvariablen",x = "fehlende Werte in %")


Data_cor_Sel_laender<- Stat_Data_geo%>%
  dplyr::select(one_of("Konflikt","Year","SP.DYN.CDRT.IN","Population_secondary_education_25.","SP.POP.GROW","EN.POP.DNST","SP.URB.TOTL.IN.ZS","SP.ADO.TFRT","Im_Ex","SP.POP.2024.MA.5Y","SL.TLF.TOTL.FE.ZS","SG.GEN.PARL.ZS","SL.UEM.1524.ZS","TX.VAL.MMTL.ZS.UN","SE.XPD.TOTL.GD.ZS","TX.VAL.FUEL.ZS.UN","NY.GDP.PCAP.CN","total_index_core","region_wb","economy","classification_core"))


NA_logit_region <- Data_cor_Sel_laender%>%
  group_by(region_wb)%>%
  miss_var_summary()%>%
  pivot_wider(names_from =region_wb ,values_from = c("pct_miss","n_miss"))

NA_logit_year <- Data_cor_Sel_laender%>%
  group_by(Year)%>%
  miss_var_summary()%>%
  pivot_wider(names_from = Year ,values_from = c("n_miss","pct_miss"))

NA_logit_eco <- Data_cor_Sel_laender%>%
  group_by(economy)%>%
  miss_var_summary()%>%
  pivot_wider(names_from = economy ,values_from = c("n_miss","pct_miss"))

Na_logit_demo <-Data_cor_Sel_laender%>%
  group_by(classification_core)%>%
  miss_var_summary()%>%
  pivot_wider(names_from = classification_core ,values_from = c("n_miss","pct_miss"))


NA_logit <- Data_cor_Sel_laender%>%
  miss_var_summary


############# Abbildungen und Plots

#### ## Absolute Häufigkeitsverteilung aller geografischen und ökonomischen Regionen im Datensatz ohne fehlende Werte


Data_logit_sel_reg<-Data_cor_sel%>%
  ggplot(aes(region_wb))+
  geom_bar(aes(fill=Konflikt))+
  scale_fill_viridis_d("Konflikte")+
  theme_minimal()+
  theme(text = element_text(size = 12),
        axis.text.x = element_text(angle = 30, hjust=1))

Data_logit_sel_eco<-Data_cor_sel%>%
  ggplot(aes(economy))+
  geom_bar(aes(fill=Konflikt))+
  scale_fill_viridis_d("Konflikte")+
  theme_minimal()+
  theme(text = element_text(size = 12),
        axis.text.x = element_text(angle = 30, hjust=1))

## Absolute Häufigkeitsverteilung aller geografischen und ökonomischen Regionen im Datensatz mit fehlenden Werten

Verteilungabsolut_reg<-Stat_Data_geo%>%
  filter(!is.na(region_wb))%>%
  ggplot(aes(region_wb))+
  geom_bar(aes(fill=Konflikt))+
  scale_fill_viridis_d("Konflikte")+
  theme_minimal()+
  theme(text = element_text(size = 12),
        axis.text.x = element_text(angle = 30, hjust=1))

Verteilungabsolut_eco<-Stat_Data_geo%>%
  filter(!is.na(economy))%>%
  ggplot(aes(economy))+
  geom_bar(aes(fill=Konflikt))+
  scale_fill_viridis_d("Konflikte", labels = c("True", "False"))+
  theme_minimal()+
  theme(text = element_text(size = 12),
        axis.text.x = element_text(angle = 30, hjust=1))

#Abbildung 15
gdppc_stat <- Data_cor_sel%>%
  ggplot(aes(Konflikt,NY.GDP.PCAP.CN))+
  geom_boxplot()+
  scale_x_discrete(labels= c("False","True"))+
  labs(y= "GDP pro Kopf (log)")+
  #ggtitle("Boxplot des Indikators GDP pro Kopf,\nVerteilt nach dem Auftreten von Konflikten", subtitle = "Datensatz für die statistische Berechnung")+
  theme_minimal()

## Abbildung 14
gdppc_vio <- Data_cor_sel%>%
  ggplot(aes(Konflikt,NY.GDP.PCAP.CN))+
  geom_violin()+
  scale_x_discrete(labels= c("False","True"))+
  labs(y= "GDP pro Kopf (log)")+
  #ggtitle("Boxplot des Indikators GDP pro Kopf,\nVerteilt nach dem Auftreten von Konflikten", subtitle = "Datensatz für die statistische Berechnung")+
  theme_minimal()

# Boxplot mit dem GDP pro Kopf im Datensatz mit fehlenden Daten
gdppc_Land_Jahr <- Stat_Data_geo%>%
  ggplot(aes(Konflikt,NY.GDP.PCAP.CN))+
  geom_boxplot()+
  scale_x_discrete(labels= c("False","True"))+
  labs(y= "GDP pro Kopf (log)")+
  #ggtitle("Boxplot des Indikators GDP pro Kopf,\nVerteilt nach dem Auftreten von Konflikten", subtitle = "Datensatz mit allen Land/Jahres Kombinationen")+
  theme_minimal()

# Der Trend des Youth Bulge nimmt ab, auch in dem für die Statistik genutzen Datensatz
Tren_cor <- Data_cor_sel%>%
  group_by(Year,region_wb)%>%
  summarise(across(SP.DYN.CDRT.IN:total_index_core, mean))

Tren_cor%>%
  filter(Year<2019)%>%
  ggplot(aes(Year,SP.POP.2024.MA.5Y, color=region_wb))+
  geom_line()

Tren_cor_world <- Data_cor_sel%>%
  group_by(Year)%>%
  summarise(across(SP.DYN.CDRT.IN:total_index_core, mean))### negativer Trend über die Jahre

Tren_cor_world%>%
  filter(Year<2019)%>%
  ggplot(aes(Year,SP.POP.2024.MA.5Y))+
  geom_line()+
  labs(x= "Jahre", y= "Anteil der männlichen Bevölkerung zwischen 20-24 Jahren")+
  ggtitle("Zeitliche Verteilung des Youth Bulges im gloablen Durchschnitt", subtitle = "Datensatz für die statistische Berechnung")+
  theme_minimal()

# Boxplot Youth Bulge im Datensatz ohne fehlende Werte
YouthBulges_box<- Data_cor_sel%>%
  ggplot(aes(Konflikt, SP.POP.2024.MA.5Y))+
  geom_boxplot() 

# Boxplot Youth Bulge im Datensatz mit fehlende Werte
YouthBulges_box_alle<- Stat_Data_geo%>%
  ggplot(aes(Konflikt, SP.POP.2024.MA.5Y))+
  geom_boxplot()

# geographische Regionen und die Verteilung von Ländern mit und ohne Konflikt in dem Datensatz mit fehlenden Werten
Verteilungabsolut_reg<-Stat_Data_geo%>%
  filter(!is.na(region_wb))%>%
  ggplot(aes(region_wb))+
  geom_bar(aes(fill=Konflikt),width = 0.4)+
  scale_fill_viridis_d("Konflikte",labels= c("Nein","Ja"))+
  labs(x="Regionen World Bank", y= "Anzahl der Konflikte")+
  ggtitle("Anzahl der Länder mit mindestens einem Konflikt \naufgeteilt nach geographischen Regionen", subtitle = "vollständiger Datensatz")+
  theme_minimal()+
  theme(text = element_text(size = 12),
        axis.text.x = element_text(angle = 30, hjust=1))

# geographische Regionen und die Verteilung von Ländern mit und ohne Konflikt in dem Datensatz ohne fehlende Werte
Data_logit_sel_reg<-Data_cor_sel%>%
  ggplot(aes(region_wb,fill=Konflikt))+
  geom_bar(width = 0.4)+
  scale_fill_viridis_d("Konflikte",labels= c("Nein","Ja"))+
  labs(x="Regionen World Bank", y= "Anzahl der Konflikte")+
  ggtitle("Anzahl der Länder mit mindestens einem Konflikt \naufgeteilt nach geographischen Regionen", subtitle = "statistischer Datensatz")+
  theme_minimal()+
  theme(text = element_text(size = 12),
        axis.text.x = element_text(angle = 30, hjust=1))


#ökonomische Regionen und die Verteilung von Ländern mit und ohne Konflikte im Datensatz mit fehlenden Werten

Verteilungabsolut_eco<-Stat_Data_geo%>%
  filter(!is.na(economy))%>%
  ggplot(aes(economy))+
  geom_bar(aes(fill=Konflikt),width = 0.4)+
  scale_fill_viridis_d("Konflikte",labels= c("Nein","Ja"))+
  labs(x="Regionen World Bank", y= "Anzahl der Konflikte")+
  ggtitle("Anzahl der Länder mit mindestens einem Konflikt \naufgeteilt nach ökonomische Regionen", subtitle = "globaler Datensatz")+
  theme_minimal()+
  theme(text = element_text(size = 12),
        axis.text.x = element_text(angle = 30, hjust=1))
#ökonomische Regionen und die Verteilung von Ländern mit und ohne Konflikte im Datensatz ohne fehlende Werte
Data_logit_sel_eco<-Data_cor_sel%>%
  ggplot(aes(economy,fill=Konflikt))+
  geom_bar(width = 0.4)+
  scale_fill_viridis_d("Konflikte",labels= c("Nein","Ja"))+
  labs(x="Regionen World Bank", y= "Anzahl der Konflikte")+
  ggtitle("Anzahl der Länder mit mindestens einem Konflikt \naufgeteilt nach ökonomische Regionen", subtitle = "statistischer Datensatz")+
  theme_minimal()+
  theme(text = element_text(size = 12),
        axis.text.x = element_text(angle = 30, hjust=1))


#### Demokratische Systeme und das Verhältnis zwischen Ländern mit und ohne Konflikte im reduzierten Datensatz mit fehlenden Werten

Verteilungabsolut_dem<-Stat_Data_geo%>%
  filter(!is.na(classification_core))%>%
  ggplot(aes(classification_core))+
  geom_bar(aes(fill=Konflikt),width = 0.4)+
  scale_fill_viridis_d("Konflikte",labels= c("Nein","Ja"))+
  labs(x="politische Systeme (Universität Würzburg)", y= "Anzahl der Konflikte")+
  ggtitle("Anzahl der Länder mit mindestens einem Konflikt \naufgeteilt nach dem politischen System", subtitle = "globale Datensatz")+
  theme_minimal()+
  theme(text = element_text(size = 12),
        axis.text.x = element_text(angle = 30, hjust=1))

## Demokratische Systeme und das Verhältnis zwischen Ländern mit und ohne Konflikte im reduzierten Datensatz ohne fehlende Werte
 Data_logit_sel_dem<-Data_cor_sel%>%
   ggplot(aes(classification_core,fill=Konflikt))+
   geom_bar(width = 0.4)+
   scale_fill_viridis_d("Konflikte",labels= c("Nein","Ja"))+
   labs(x="politische Systeme (Universität Würzburg)", y= "Anzahl der Konflikte")+
   ggtitle("Anzahl der Länder mit mindestens einem Konflikt \naufgeteilt nach dem politischen System", subtitle = "statistischer Datensatz")+
   theme_minimal()+
   theme(text = element_text(size = 12),
         axis.text.x = element_text(angle = 30, hjust=1))


# Abbildung 5
 Konf_regio <-Data_geo%>%
   filter(Konflikt==TRUE)%>%
   ggplot(aes(region_wb))+
   geom_bar(fill= "grey50")+
   labs(x= "Regionen (World Bank)", y= "Anzahl der Konflikte")+
   #ggtitle("Verteilung der Konflikte nach Regionen(1989-2020)")+
   theme_minimal()+
   theme(text = element_text(size = 10),
         axis.text.x = element_text(angle = 30, hjust=1))
 
 ##  Bar-Diagramm, welche ökonomischen Regionen am meisten Konflikte haben
 #Abbildung 7
 Konf_year_eco <- Data_geo%>%
   filter(Konflikt==TRUE)%>%
   ggplot(aes(economy))+
   geom_bar(fill= "grey50")+ 
   labs(x= "Ökonomische Regionen (World Bank)", y= "Anzahl der Konflikte")+
   #ggtitle("Verteilung der Konflikte nach ökonomischen Regionen(1989-2020)")+
   theme_minimal()+
   theme(text = element_text(size = 10),
         axis.text.x = element_text(angle = 30, hjust=1)) 

  ### bar Diagramm, welche politischen Systeme haben am meisten Konflikte
 ## NAs sind als Character in der Variable der demokratischen Klassifizierung gespeichert und müssen in richtige Missing Values transformiert werden
 
 na_strings <- c("NA")
 Data_geo<-Data_geo%>% 
   replace_with_na_at(.vars=c("classification_core"), condition = ~.x %in% na_strings) 
 
 
 ## Welche politischen Systeme haben die meisten Konflikte. Und wie sind die über räumliche Rgionen aufgeteilt
 #Abbildung 6
 demokratie_Konf <-Data_geo%>%
   filter(Konflikt==TRUE)%>%
   replace_with_na_at(.vars=c("classification_core"), condition = ~.x %in% na_strings)%>%
   ggplot(aes(classification_core))+
   geom_bar(fill = "grey50")+
   #scale_fill_viridis_d("Regionen (World Bank)",na.value="grey50")+
   labs(x="politische Systeme (Uni Würzburg)",y= "Anzahl der Konflikte")+
   #ggtitle("Konflikte und Staatssystem (1989-2020)")+
   theme_minimal()+
   theme(text = element_text(size = 10),
         axis.text.x = element_text(angle = 30, hjust=1))  
## Konflikte über die Jahre Verteilt und der Anteil der Region pro Jahr

# Abbildung 8
   Konf_year2 <- Data_geo%>%
   filter(Konflikt==T)%>%
   ggplot()+
   geom_line(aes(Year, color= region_wb),stat = 'count', size = 1)+
   geom_line(aes(Year, color = "Alle Konflikte"), stat = 'count', size = 1)+
   scale_color_viridis_d("Region (World Bank)")+
   labs(x = "Jahre",y = "Anzahl der Konflikte")+
   theme_minimal()
 

# Abbildung 9
Data_cor_sel%>%
   filter(Konflikt== 1)%>%
   ggplot(aes(region_wb))+
   geom_bar(fill= "grey50")+
   labs(x= "Regionen (World Bank)", y= "Anzahl der Konflikte")+
   #ggtitle("Verteilung der Konflikte nach Regionen(1989-2020)")+
   theme_minimal()+
   theme(text = element_text(size = 10),
         axis.text.x = element_text(angle = 30, hjust=1))
 
# Verteilung der Häufigkeiten von Konflikten in den geografischen Regionen 
 Data_cor_sel%>%
   filter(Konflikt==1)%>%
   count(region_wb)
# Abbildung 10
 Data_cor_sel%>%
   filter(Konflikt== 1)%>%
   ggplot(aes(economy))+
   geom_bar(fill= "grey50")+
   labs(x= "ökonomische Regionen (World Bank)", y= "Anzahl der Konflikte")+
   #ggtitle("Verteilung der Konflikte nach Regionen(1989-2020)")+
   theme_minimal()+
   theme(text = element_text(size = 10),
         axis.text.x = element_text(angle = 30, hjust=1))
# Verteilung der Häufigkeiten von Konflikten in den ökonomischen Regionen
 Data_cor_sel%>%
   filter(Konflikt==1)%>%
   count(economy)
 
# Abbildung 11
 Data_cor_sel%>%
   filter(Konflikt== 1)%>%
   ggplot(aes(classification_core))+
   geom_bar(fill= "grey50", size = 100)+
   labs(x= "politische Systeme  (Universität Würzburg)", y= "Anzahl der Konflikte")+
   #ggtitle("Verteilung der Konflikte nach Regionen(1989-2020)")+
   theme_minimal()

# Verteilung der Häufigkeiten von Konflikten in den politischen Systemen

 Data_cor_sel%>%
   filter(Konflikt==1)%>%
   count(classification_core)
#Abbildung 12
 Data_cor_sel%>%
   filter(Konflikt==1)%>%
   ggplot()+
   geom_line(aes(Year, color= region_wb),stat = 'count', size = 1)+
   geom_line(aes(Year, color = "Alle Konflikte"), stat = 'count', size=1)+
   scale_color_viridis_d("Region (World Bank)")+
   labs(x = "Jahre",y = "Anzahl der Konflikte")+
   theme_minimal()
# Veretilung der Häufigkeit in den Jahren
 Data_cor_sel%>%
   filter(Konflikt==1)%>%
   count(Year)
 
 ## Überprüdung der Konflikte in der Region Nordamerika und den Konflikttyp 4
 Northamerika_konf <-Data_geo%>%
  filter(region_wb== "North America")%>%
  filter(bd_best >24)

Konf_type_4<- Data_geo%>%
  filter(type_of_conflict==4)

## Abbildung 18 zeigt die fehlenden Werte bei den harten Autokratien in den Prädiktorvariablen
Stat_Data_geo_hartautocracy<-Stat_Data_geo%>%
  filter(classification_core=="Hard Autocracy")%>%
  dplyr::select(one_of(c("Year","Country.Code","total_index_core","SP.URB.TOTL.IN.ZS","SP.DYN.CDRT.IN","Population_secondary_education_25.","SP.POP.GROW","EN.POP.DNST","SP.ADO.TFRT","Im_Ex","SP.POP.2024.MA.5Y","SL.TLF.TOTL.FE.ZS","SG.GEN.PARL.ZS","SL.UEM.1524.ZS","TX.VAL.MMTL.ZS.UN","SE.XPD.TOTL.GD.ZS","TX.VAL.FUEL.ZS.UN","NY.GDP.PCAP.CN")))

colnames(Stat_Data_geo_hartautocracy)<- c("Jahr","Country Code","Demokratieindex","Anteil der urbanen Bev.","Sterberate","Bev. ü. 25J. mit Grundausbildung","Bev. Wachstum","Bev. Dichte","Schwangerschaften bei Jugendlichen","Import/Export","Youth Bulge","Beteiligung von F. am Arbeitsmarkt", "Anteil von F. im Parlament","Jugendarbeitslosigkeit","Export: Metalle und Erze","staatliche Bildungdausgaben","Exporte: Treibstoffe","GDP pro Kopf")
gg_miss_var(Stat_Data_geo_hartautocracy, show_pct = T)+
  labs(y= "fehlende Werte in den harten Autokratien in % zu allen Werten", x = "Variablenname")

#Abbildung 19
FiPar_demo <- Stat_Data_geo%>%
  ggplot(aes(x= total_index_core,y=SG.GEN.PARL.ZS))+
  geom_point()+
  geom_smooth()+
  labs(x="Demokratieindex",y= "Anteil von Frauen im Parlament (in %)")+
  theme_minimal()

#Anteil der 25-Jährigen mit einer Grundausbildung mit dem Deomkratiindex in einem Punktdiagram
b25j_demo <- Stat_Data_geo%>%
  ggplot(aes(x= total_index_core,y=Population_secondary_education_25.))+
  geom_point()+
  geom_smooth()+
  labs(x="Demokratieindex",y= "Anteil der ü. 25-Jährigen mit einer Grundausbildung")+
  theme_minimal()

# Datensatz mit allen Mangelhaften Demokratien
mangelDemo <-Stat_Data_geo%>%
  filter(classification_core =="Deficient Democracy")

## Weißrussland als Beispiel für eine genauere Untersuchung
Belarus <-Stat_Data_geo%>%
  filter(Country.Name.x== "Belarus")

# Funktionierende Demokratien mit Konflikten
funktioneirende_demokratien <- Data_geo%>%
  filter(Konflikt == T)%>%
  filter(classification_core=="Working Democracy")

# Datensatz, um China genauer zu untersuchen
China <- Stat_Data_geo%>%
  filter(Country.Name.x== "China")

# GDp pro Kopf und Länder mit Konflikten um reduzierten Datensatz ohne fehlende Werte
GDP_proKopf_konf<- Data_cor_sel%>%
  filter(Konflikt==1)

#Abbildung 16
Data_cor_sel%>%
  ggplot(aes(Konflikt,TX.VAL.FUEL.ZS.UN))+
  geom_violin()+
  labs(x="Konflikte False(0)/True(1)",y="Export: Treibstoffe \n(in % gemessen am gesamten Warenexport)")+
  theme_minimal()

#Abbildung 16 als Boxplot
Data_cor_sel%>%
    ggplot(aes(Konflikt,TX.VAL.FUEL.ZS.UN))+
    geom_boxplot()+
    labs(x="Konflikte False(0)/True(1)",y="Export: Treibstoffe \n(in % gemessen am gesamten Warenexport)")+
  theme_minimal()
#Abbildung 17
Stat_Data_geo%>%
  ggplot(aes(Konflikt,TX.VAL.FUEL.ZS.UN))+
  geom_violin()+
  labs(x="Konflikte",y="Export: Treibstoffe \n(in % gemessen am gesamten Warenexport)")+
  theme_minimal()

#Länder mit Konflikten im reduzierten Datensatz mit fehlenden Werten mit der Variable Export: Treibstoffen
Fuel<-Stat_Data_geo%>%
  dplyr::select(one_of(c("Konflikt","Year","Country.Name.x","TX.VAL.FUEL.ZS.UN")))%>%
  filter(Konflikt== T)

sum(is.na(Fuel$TX.VAL.FUEL.ZS.UN)) #-> 387 fehlende Werte

# Länder ohne Konflikte im reduzierten Datensatz ohne fehlende Werte
Fuel_Neu<- Data_cor_sel%>%
  dplyr::select(one_of(c("Konflikt","Year","Country.Code","TX.VAL.FUEL.ZS.UN")))%>%
  filter(Konflikt == 0)

#Abbildung 20
Data_cor_sel%>%
  ggplot(aes(Konflikt, EN.POP.DNST))+
  geom_violin()+
  labs(x= "Konflikt Ja(1)/Nein(0)",y="Bev. Dichte (Personen pro km², logarithmiert)")+
  theme_minimal()

#Abbildung 21
Stat_Data_geo$EN.POP.DNST<-log(Stat_Data_geo$EN.POP.DNST)
Stat_Data_geo%>%
  ggplot(aes(Konflikt, EN.POP.DNST))+
  geom_violin()+
  labs(x= "Konflikt",y="Bev. Dichte (Personen pro km², logarithmiert)")+
  theme_minimal()

# Datensätze mit der Bevölkerungsdichte und Anteil der urbanen Bevölkerung golaber Datensatz und dem statistischen Datensatz
Bevdichte<-Stat_Data_geo%>%
  dplyr::select("Konflikt","Year","Country.Name.x","EN.POP.DNST")

Bevdichte_neu<-Data_cor_sel%>%
  dplyr::select("Konflikt","Year","Country.Code","EN.POP.DNST")

urban<- Data_cor_sel%>%
  dplyr::select("Konflikt","Year","Country.Code","SP.URB.TOTL.IN.ZS")

urban_Neu<- Stat_Data_geo%>%
  dplyr::select("Konflikt","Year","Country.Name.x","SP.URB.TOTL.IN.ZS")

# Abbildung 22
Data_cor_sel%>%
  ggplot(aes(Konflikt,SP.URB.TOTL.IN.ZS))+
  geom_violin()+
  labs(x="Konflikt Nein(0)/ Ja (1)", y = "Anteil der urbanen Bevölkerung \n(% der totalen Bevölkerung)")+
  theme_minimal()

# Abbildung 23
Stat_Data_geo%>%
  ggplot(aes(Konflikt,SP.URB.TOTL.IN.ZS))+
  geom_violin()+
  labs(x="Konflikt", y = "Anteil der urbanen Bevölkerung \n(% der totalen Bevölkerung)")+
  theme_minimal()

# Abbildung 24
Data_cor_sel%>%
  ggplot(aes(Konflikt, SL.TLF.TOTL.FE.ZS))+
  geom_violin()+
  labs(x="Konflikt Nein(0)/ Ja (1)", y="weibliche Arbeitskraft\n(% gemessen an der gesamten Arbeitskraft)")+
  theme_minimal()

# Abbildung 25
Stat_Data_geo%>%
  ggplot(aes(Konflikt, SL.TLF.TOTL.FE.ZS))+
  geom_violin()+
  labs(x="Konflikt", y="weibliche Arbeitskraft\n(% gemessen an der gesamten Arbeitskraft)")+
  theme_minimal()

# Fehlende Werte bei dem Anteil von Frauen am Arbeitsmarkt im reduzierten Datensatz mit fehlenden Werten-> 3995
frauarbeit <- Stat_Data_geo%>%
  dplyr::select("Konflikt","Year","Country.Name.x","SL.TLF.TOTL.FE.ZS")
sum(is.na(frauarbeit$SL.TLF.TOTL.FE.ZS))  

# violinplot des Youth Bulges in Europa und Zentralasien
Violineuropa <-Data_cor_sel%>%
  filter(region_wb== "Europe & Central Asia")%>%
  ggplot(aes(Konflikt,SP.POP.2024.MA.5Y))+
    geom_violin()+
  labs(x="KonfliktJa (1)/Nein(0)", y="Anteil von jungen Männern an der Bevölkerung \n(20-24 Jahre, in %)")+
  theme_minimal()

# Youth Bulge in Sub-Sahara Afrika im Violinplot
Violinsubsahara <-Data_cor_sel%>%
  filter(region_wb== "Sub-Saharan Africa")%>%
  ggplot(aes(Konflikt,SP.POP.2024.MA.5Y))+
  geom_violin()+
  labs(x="KonfliktJa (1)/Nein(0)", y="Anteil von jungen Männern an der Bevölkerung \n(20-24 Jahre, in %)")+
  theme_minimal()

# Abbildung 26
Data_cor_sel%>%
  filter(region_wb == c("Sub-Saharan Africa","Europe & Central Asia","Middle East & North Africa"))%>%
  ggplot(aes(fill=Konflikt,y=SP.POP.2024.MA.5Y,x=region_wb))+
  geom_violin()+
  scale_fill_viridis_d("Konflikte \nJa(1)/ Nein(0)")+
  labs(x="Region (World Bank)", y="Anteil von jungen Männern an der Bevölkerung \n(20-24 Jahre, in %)")+
  theme_minimal()

## Abbildung 27
Data_cor_sel%>%
  filter(region_wb == c("Sub-Saharan Africa","Europe & Central Asia","Middle East & North Africa"))%>%
  ggplot(aes(fill=Konflikt,y=SG.GEN.PARL.ZS,x=region_wb))+
  geom_violin()+
  scale_fill_viridis_d("Konflikte \nJa(1)/ Nein(0)")+
  labs(x="Region (World Bank)", y="Anteil von Frauen im Parlament (in %)")+
  theme_minimal()

# Länder in Europa und Zentralasien ohne Konflikte
konfeuro <-Data_cor_sel%>%
  filter(region_wb == "Europe & Central Asia")%>%
  filter(Konflikt == 0)

# Urbane bevölkerung auf Regionen mit Violinplots
 Data_cor_sel%>%
   filter(region_wb == c("Sub-Saharan Africa","Europe & Central Asia","Middle East & North Africa"))%>%
   ggplot(aes(fill=Konflikt,y=SP.URB.TOTL.IN.ZS,x=region_wb))+
   geom_violin()+
   scale_fill_viridis_d("Konflikte \nJa(1)/ Nein(0)", option = "magma")+
   labs(x="Region (World Bank)", y="Anteil der urbanen Bevölkerung (in %)")+
   theme_minimal()

# Exporte Metalle und Erze auf Regionen in Violinplots
 Data_cor_sel%>%
   filter(region_wb == c("Sub-Saharan Africa","Europe & Central Asia","Middle East & North Africa"))%>%
   ggplot(aes(fill=Konflikt,y=TX.VAL.MMTL.ZS.UN,x=region_wb))+
   geom_violin()+
   scale_fill_viridis_d("Konflikte \nJa(1)/ Nein(0)")+
   labs(x="Region (World Bank)", y="Exporte: Metalle und Erze \n(in % gemessen am gesamten Warenexport)")+
   theme_minimal()


#Karte GII Abbildung 28

data_map <- inner_join(countries_sf,Data_geo, by = c("iso_a3"="Country Code"))
head(data_map)
GII_map <- data_map %>%
  filter(Year == 2019)%>%
  ggplot(aes(fill= GII))+
  geom_sf(col="black", size = 0.1)+
  coord_sf(crs = "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs")+
  scale_fill_viridis_c(direction = -1)+
  theme_minimal()
GII_map

Demokratieaufregio <- Stat_Data_geo%>%
  filter(region_wb==c("Sub-Saharan Africa","Europe & Central Asia","Middle East & North Africa"))%>%
  ggplot(aes(fill= Konflikt, y= total_index_core, x = region_wb))+
  geom_violin()+
  scale_fill_viridis_d("Konflikte")+
  labs(x="Region (World Bank)", y="Demokratieindex")+
  theme_minimal()
Demokratieaufregio
