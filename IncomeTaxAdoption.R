########################################################
# Data Prep
#######################################################


####### GENERAL PREP

# Start Fresh
rm(list=ls())
cat("\014")
setwd("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption")

# Load/Transform the data
load("/Users/hectorbahamonde/RU/Dissertation/Data/dissertation.Rdata") # Load data

## Keep sample countries and variables
data = subset(dissertation, country == "Chile" | country ==  "Colombia" | country ==  "Ecuador" | country ==  "Guatemala" | country ==  "Nicaragua" | country ==  "Peru" | country ==  "Venezuela" | country == "Mexico" | country == "Argentina")
data = subset(data, select = c(country, year, democ, autoc, polity, polity2, urbpop, totpop, constmanufact, constagricult, exports, ppp, propagrmanu, realgdp, incometax, madisongdp, madisonpop, boix_democracy, madisonpercapgdp, customtax))
data = subset(data, constmanufact != "NA" & constagricult != "NA")

## Construct a the outcome variable (0,1) for when the income tax was imposed.
incometax.d.chile = data.frame(ifelse(data$year>=1924 & data$country == "Chile",1,0)) # Chile,  1924 (Mamalakis [1976, p. 20]
incometax.d.colombia  = data.frame(ifelse(data$year>=1935 & data$country == "Colombia",1,0)) # Colombia, Ley 78 Figueroa2008a, p. 9.
incometax.d.ecuador  = data.frame(ifelse(data$year>=1945 & data$country == "Ecuador",1,0)) # Ecuador Aguilera2013 p. 135
incometax.d.guatemala  = data.frame(ifelse(data$year>=1963 & data$country == "Guatemala",1,0)) # Guatemala: Decreto 1559, De2007 p 165
incometax.d.nicaragua  = data.frame(ifelse(data$year>=1974 & data$country == "Nicaragua",1,0)) # Ley No. 662 de 5 de Noviembre de 1974 (http://legislacion.asamblea.gob.ni/Normaweb.nsf/($All)/024063C3B373125E062570A10057EE73?OpenDocument)
incometax.d.peru  = data.frame(ifelse(data$year>=1934 & data$country == "Peru",1,0)) # Peru, Ley 7904 de 1934
incometax.d.venezuela  = data.frame(ifelse(data$year>=1943 & data$country == "Venezuela",1,0)) # Venezuela, Ley de Impuesto sobre la Renta, Publicada en la Gaceta Oficial número 20.851 del 17 de julio de 1.942, pero entra en vigencia el ano siguiente
incometax.d.mexico  = data.frame(ifelse(data$year>=1925 & data$country == "Mexico",1,0)) # Mexico, \citet[8]{UndaGutierrez2017}
incometax.d.argentina  = data.frame(ifelse(data$year>=1933 & data$country == "Argentina",1,0)) # Argentina, http://servicios.infoleg.gob.ar/infolegInternet/verNorma.do?id=185729

# Other countries
## Uruguay: 1959 (https://www.colibri.udelar.edu.uy/bitstream/123456789/4155/5/dt-05-06.pdf, p )
## Rep Dom
## Paraguay: 2012 (http://www.ultimahora.com/sepa-mas-el-impuesto-la-renta-personal-n548616.html)
## Panama : 1956? (https://panama.eregulations.org/media/codigo%20fiscal.pdf)
## Honduras: 1963 DECRETO LEY NO. 25 http://www.fideseguros.com/wp-content/uploads/2015/06/Anexo-Honduras-ley-impuesto-sobre-la-renta_15408_.pdf //  http://bufeterosa.com/biblioteca/leyimpuestorenta.pdf // http://www.poderjudicial.gob.hn/CEDIJ/Documents/Ley%20Impuesto%20sobre%20la%20Renta%20actualizada%20a%20febrero%20de%202012.pdf
## Haiti
## El Salvador: 1915 (BADIO2013), but modernized in 1991.
## Costa Rica: http://slideplayer.es/slide/142777/
## Brasil
## Bolivia 
##





## Sum across all the rows.
incometax.d = incometax.d.chile +  incometax.d.colombia +  incometax.d.ecuador +  incometax.d.guatemala +  incometax.d.nicaragua +  incometax.d.peru +  incometax.d.venezuela + incometax.d.mexico + incometax.d.argentina
colnames(incometax.d) = "incometax"


## generate the dataset
aux1.d = subset(data, select = c(country, year))
incometax.d = data.frame(aux1.d, incometax.d)

## merge with the dataset
data = merge(incometax.d, data, by=c("country", "year"))
colnames(data)[3] = "incometax.d"

## WHEN DEMOCRACY STARTED.
democracy.d.chile = data.frame(ifelse(data$year>= 1909 & data$country == "Chile",1,0)) 
democracy.d.colombia  = data.frame(ifelse(data$year>= 1937 & data$country == "Colombia",1,0)) 
democracy.d.ecuador  = data.frame(ifelse(data$year>= 1948 & data$country == "Ecuador",1,0)) 
democracy.d.guatemala  = data.frame(ifelse(data$year>= 1945 & data$country == "Guatemala",1,0)) 
democracy.d.nicaragua  = data.frame(ifelse(data$year>= 1984 & data$country == "Nicaragua",1,0)) 
democracy.d.peru  = data.frame(ifelse(data$year>= 1956 & data$country == "Peru",1,0)) 
democracy.d.venezuela  = data.frame(ifelse(data$year>= 1959 & data$country == "Venezuela",1,0))
democracy.d.mexico  = data.frame(ifelse(data$year>= 2000 & data$country == "Mexico",1,0))
democracy.d.argentina  = data.frame(ifelse(data$year>= 1912 & data$country == "Argentina",1,0))


### use this molde para ver cuando empezo la democracia
# data.frame(
#         test = as.numeric(data$boix_democracy[data$country=="Argentina"]), 
#         test2 = as.numeric(data$year[data$country=="Argentina"])
#         )




## Sum across all the rows.
democracy.d = 
        democracy.d.chile +  democracy.d.colombia +  democracy.d.ecuador +  
        democracy.d.guatemala +  democracy.d.nicaragua +  democracy.d.peru +  
        democracy.d.venezuela + democracy.d.mexico + democracy.d.argentina
colnames(democracy.d) = "democracy"


## generate the dataset
aux2.d = subset(data, select = c(country, year))
democracy.d = data.frame(aux2.d, democracy.d)

## merge with the dataset
data = merge(democracy.d, data, by=c("country", "year"))
colnames(data)[3] = "democracy.d"

## generate year2 variable, which is the "end" time.
data$year2 = data$year+1


### Cumulative effects of spatial dependence
spatial.cum = c(
        ifelse(data$year>=1924,1,0) + # Chile,  1924 (Mamalakis [1976, p. 20]
                ifelse(data$year>=1935,1,0) + # Colombia, Ley 78 Figueroa2008a, p. 9.               
                ifelse(data$year>=1945,1,0) + # Ecuador Aguilera2013 p. 135                
                ifelse(data$year>=1963,1,0) + # Guatemala: Decreto 1559, De2007 p 165                
                ifelse(data$year>=1974,1,0) + # Nicaragua: Ley No. 662 de 5 de Noviembre de 1974 
                ifelse(data$year>=1934,1,0) + # Peru, Ley 7904 de 1934   
                ifelse(data$year>=1925,1,0) + # Mexico
                ifelse(data$year>=1933,1,0) + # Arg
                ifelse(data$year>=1943,1,0) # Venezuela, Ley de Impuesto sobre la Renta 
)


data$spatial.cum = spatial.cum

## Saving Data
save(data, file = "/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/incometax_data.RData") # in paper's folder

## save data in STATA format for Paper 2, where I use STATA
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(foreign)

write.dta(data, "/Users/hectorbahamonde/RU/Dissertation/Papers/NegativeLink/data.dta")

##################################################
##             CENSUS DATA
##################################################



# HERE

rm(list=ls())
cat("\014")

# Census Data

# Load the census data
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(foreign)

## Load Census data 
census.d <- read.dta("/Users/hectorbahamonde/RU/research/Inequality_State_Capacities/Data/Census_Data_Hanson/Census_data_until_2015.dta")


## replaces NAs with 0s
census.d$census_full[is.na(census.d$census_full)] <- 0

## generate cumulative census variable by country
census.d = census.d[with(census.d, order(country, year)),]
census.d$cum.census <- as.numeric(ave(census.d$census_full, census.d$country, FUN=cumsum))

## keep just important variables
census.d <- census.d[, c("country", "year", "census_full", "cum.census")]





if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(data.table) 
census.d = merge(data.frame(setDT(census.d[ which(census.d$cum.census > 0),])[, .(country_num_of_census = uniqueN(cum.census)), by = country]),
                 census.d,
                 by=c("country"),
                 all.x=T
)
### generate variable with number of years by country
census.d = merge(
        data.frame(setDT(census.d)[, .(country_tot_years = uniqueN(year)), by = country]),
        census.d,
        by=c("country"),
        all.x=T
)

# average census per year
census.d$ave_census = census.d$country_num_of_census/census.d$country_tot_years



# subset
census.d = subset(census.d, country == "Chile" | country ==  "Colombia" | country ==  "Ecuador" | country ==  "Guatemala" | country ==  "Nicaragua" | country ==  "Peru" | country ==  "Venezuela" | country == "Mexico" | country == "Argentina")


census.d = data.frame(
        Country = census.d$country,
        Year = census.d$year,
        Census = census.d$cum.census
)

View(data.frame(aggregate(census.d$Census, by = list(census.d$Country), max)))




if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2)
ggplot(na.omit(census.d), aes(Year, Census, colour = Country)) + #facet_wrap(~Variable,  ncol = 3, scales = "free") +
        geom_line() + 
        xlab("Year") +
        ylab("Cumulative State Capacities") +
        theme_bw() +
        labs(title="") +
        theme(
                axis.text.y = element_text(size=7), 
                axis.text.x = element_text(size=7), 
                axis.title.y = element_text(size=7), 
                axis.title.x = element_text(size=7), 
                legend.text=element_text(size=7), 
                legend.title=element_text(size=7),
                plot.title = element_text(size=7),
                legend.position="bottom")


##################################################
##              DATA PREP
# Cox Porportional Hazard Models
##################################################

# Start Fresh
cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption")


# Load Data
load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/incometax_data.RData") # Load data

# Declare Data for this model in particular
cox = data

# Exclude remanent "deaths" rows
## This function below creates a cumulative sum of the dummy var for income taxation
cox=within(cox, {
        incometax.d <- ave(incometax.d, country, FUN = cumsum)
}
)

## This below eliminates incometax.s <= 1
cox = subset(cox, incometax.d <= 1)

## Now, it renames the "incometax.d" column for "incometax.s"
colnames(cox)[4] = "incometax.s"


## Manual Log Transformation of Covariates
cox$ln.constmanufact = log(cox$constmanufact)
cox$ln.constagricult = log(cox$constagricult)
cox$ln.totpop = log(cox$totpop)

# Order and Transform the data
cox$ln.constmanufact = log(cox$constmanufact)
cox$ln.constagricult = log(cox$constagricult)
cox$ln.totpop = log(cox$totpop)

# base model
# normal cox model // take the log of these covariates
cox$L_constmanufact = log(cox$constmanufact)
cox$L_constagricult = log(cox$constagricult)
cox$L_totpop = log(cox$totpop)

## Saving Data
save(cox, file = "/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/cox.RData") # in paper's folder

##################################################
##              DATA PREP
# Cox Porportional Hazard Models LAGGED MODELS
##################################################

# Start Fresh
cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption")


## LAGGED DATA SET "cox.L"

# Load Data
load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/cox.RData") # Load data

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(data.table, DataCombine)

cox.L <- setorder(cox, country, year)
cox.L <- subset(cox.L, select = - incometax.s)


constmanufact.L = slide(cox.L, Var = "constmanufact", GroupVar = "country", slideBy = 1)$constmanufact1
constagricult.L = slide(cox.L, Var = "constagricult", GroupVar = "country", slideBy = 1)$constagricult1

cox.L$constmanufact.L = constmanufact.L
cox.L$constagricult.L = constagricult.L

## Excluding NAs
cox.L = cox.L[!is.na(cox.L$constmanufact.L),]
cox.L = cox.L[!is.na(cox.L$constagricult.L),]

# The lag was eliminating the incometax.s=1's, so I do "year+1" to "update" the years
cox.L$year = cox.L$year+1

# sum one more to year2
cox.L$year2 = cox.L$year2+1


# Then I constructed the l.incometax.s.COUNTRY var again...
L.incometax.s.chile = data.frame(ifelse(cox.L$year==1924 & cox.L$country == "Chile",1,0)) # Chile,  1924 (Mamalakis [1976, p. 20] # Ley 3996 (from Ley Chile)
L.incometax.s.colombia  = data.frame(ifelse(cox.L$year==1935 & cox.L$country == "Colombia",1,0)) # Colombia, Ley 78 Figueroa2008a, p. 9.
L.incometax.s.ecuador  = data.frame(ifelse(cox.L$year==1945 & cox.L$country == "Ecuador",1,0)) # Ecuador Aguilera2013 p. 135
L.incometax.s.guatemala  = data.frame(ifelse(cox.L$year==1963 & cox.L$country == "Guatemala",1,0)) # Guatemala: Decreto 1559, De2007 p 165
L.incometax.s.nicaragua  = data.frame(ifelse(cox.L$year==1974 & cox.L$country == "Nicaragua",1,0)) # Ley No. 662 de 5 de Noviembre de 1974 (http://legislacion.asamblea.gob.ni/Normaweb.nsf/($All)/024063C3B373125E062570A10057EE73?OpenDocument)
L.incometax.s.peru  = data.frame(ifelse(cox.L$year==1934 & cox.L$country == "Peru",1,0)) # Peru, Ley 7904 de 1934
L.incometax.s.venezuela  = data.frame(ifelse(cox.L$year==1943 & cox.L$country == "Venezuela",1,0)) # Venezuela, Ley de Impuesto sobre la Renta, Publicada en la Gaceta Oficial número 20.851 del 17 de julio de 1.942, pero entra en vigencia el ano siguiente
L.incometax.s.mexico  = data.frame(ifelse(cox.L$year==1925 & cox.L$country == "Mexico",1,0)) # 
L.incometax.s.argentina  = data.frame(ifelse(cox.L$year==1933 & cox.L$country == "Argentina",1,0)) 



## Summed across all the rows.
L.incometax.s = L.incometax.s.chile +  L.incometax.s.colombia +  
        L.incometax.s.ecuador +  L.incometax.s.guatemala +  
        L.incometax.s.nicaragua +  L.incometax.s.peru +  
        L.incometax.s.venezuela  + L.incometax.s.mexico + L.incometax.s.argentina
colnames(L.incometax.s) = "incometax"


## generated the dataset
L.aux1.d = subset(cox.L, select = c(country, year))
L.incometax.s = data.frame(L.aux1.d, L.incometax.s)

## merge with the dataset
L.cox = merge(L.incometax.s, cox.L, by=c("country", "year"))
colnames(L.cox)[3] = "incometax.s"


# Transforming data
a= log(L.cox$constmanufact.L)
## ??




## Saving Data
save(L.cox, file = "/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/L_cox.RData") # in paper's folder


##################################################
##              DATA PREP
# Generalized Estimating Equation for Logit Regression GEE
##################################################


# GEE logit allows for dependence within clusters, such as in longitudinal
# GEE models measure population-averaged effects as opposed to cluster-specific effects


# Start Fresh
cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption")


# Load Data
load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/incometax_data.RData") # Load data

# Declare Data for this model in particular
logitgee  = data


# Order and Transform the data
logitgee <- logitgee[order(logitgee$country), ]
logitgee$ln.constmanufact = log(logitgee$constmanufact)
logitgee$ln.constagricult = log(logitgee$constagricult)
logitgee$ln.totpop = log(logitgee$totpop)
logitgee$ln.customtax = log(logitgee$customtax)


## Saving Data
save(logitgee, file = "/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/logitgee.RData") # in paper's folder


##################################################
##              DATA PREP
# Multiple Events Framework: Andersen-Gill
##################################################


### From the clinical sciences, Cox Prop Haz Models have been estimating the recurrence of, say, infections.
### One important assumption, is that once a patient suffers from one, having a 2nd one, becomes more likely.
### That's exactly the type of similarity I'm looking for.
#### Andersen-Gill Models: (Therneau2000, 185-ss.): "multiple failure-time data": when a subject experiences one of the events, it still remains at risk for events of different types
##### This model is ideally suited to the situation of mutual independence of
##### the observations within a subject. This assumption is equivalent to each individual
##### counting process possessing independent increments, i.e., the num- bers of events 
##### in nonoverlapping time intervals are independent, given the covariates.

###### From Stata info: In these studies, failure times are correlated within cluster (subject or group), 
###### violating the independence of failure times assumption required in traditional survival analysis.
####### Further reference : http://www.stata.com/support/faqs/statistics/multiple-failure-time-data/



cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption")


# Load Data
load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/incometax_data.RData") # Load data

ag.data = data

# Data Transformation
ag.data$ln.constmanufact = log(ag.data$constmanufact)
ag.data$ln.constagricult = log(ag.data$constagricult)
ag.data$ln.totpop = log(ag.data$totpop)

# Create a starting point variable for DEMOCRACY.
ag.data.chile = data.frame(ifelse(ag.data$year== 1909 & ag.data$country == "Chile",1,0)) 
ag.data.colombia  = data.frame(ifelse(ag.data$year== 1937 & ag.data$country == "Colombia",1,0)) 
ag.data.ecuador  = data.frame(ifelse(ag.data$year== 1948 & ag.data$country == "Ecuador",1,0)) 
ag.data.guatemala  = data.frame(ifelse(ag.data$year== 1945 & ag.data$country == "Guatemala",1,0)) 
ag.data.nicaragua  = data.frame(ifelse(ag.data$year== 1984 & ag.data$country == "Nicaragua",1,0)) 
ag.data.peru  = data.frame(ifelse(ag.data$year== 1956 & ag.data$country == "Peru",1,0)) 
ag.data.venezuela  = data.frame(ifelse(ag.data$year== 1959 & ag.data$country == "Venezuela",1,0))
ag.data.mexico  = data.frame(ifelse(ag.data$year>= 2000 & ag.data$country == "Mexico",1,0))
ag.data.argentina  = data.frame(ifelse(ag.data$year>= 1912 & ag.data$country == "Argentina",1,0))



## Sum across all the rows.
ag.data.d = ag.data.chile +  ag.data.colombia +  ag.data.ecuador +  
        ag.data.guatemala +  ag.data.nicaragua +  ag.data.peru +  
        ag.data.venezuela + ag.data.mexico + ag.data.argentina
colnames(ag.data.d) = "democracy"

## generate the dataset
aux3.d = subset(ag.data, select = c(country, year))
democracy.s = data.frame(aux3.d, ag.data.d)

## merge with the dataset
ag.data = merge(democracy.s, ag.data, by=c("country", "year"))
colnames(ag.data)[3] = "democracy.s"

# Create a starting point variable for INCOMETAX
incometax.s.chile = data.frame(ifelse(ag.data$year==1924 & ag.data$country == "Chile",1,0)) # Chile,  1924 (Mamalakis [1976, p. 20]
incometax.s.colombia  = data.frame(ifelse(ag.data$year==1935 & ag.data$country == "Colombia",1,0)) # Colombia, Ley 78 Figueroa2008a, p. 9.
incometax.s.ecuador  = data.frame(ifelse(ag.data$year==1945 & ag.data$country == "Ecuador",1,0)) # Ecuador Aguilera2013 p. 135
incometax.s.guatemala  = data.frame(ifelse(ag.data$year==1963 & ag.data$country == "Guatemala",1,0)) # Guatemala: Decreto 1559, De2007 p 165
incometax.s.nicaragua  = data.frame(ifelse(ag.data$year==1974 & ag.data$country == "Nicaragua",1,0)) # Ley No. 662 de 5 de Noviembre de 1974 (http://legislacion.asamblea.gob.ni/Normaweb.nsf/($All)/024063C3B373125E062570A10057EE73?OpenDocument)
incometax.s.peru  = data.frame(ifelse(ag.data$year==1934 & ag.data$country == "Peru",1,0)) # Peru, Ley 7904 de 1934
incometax.s.venezuela  = data.frame(ifelse(ag.data$year==1943 & ag.data$country == "Venezuela",1,0)) # Venezuela, Ley de Impuesto sobre la Renta, Publicada en la Gaceta Oficial número 20.851 del 17 de julio de 1.942, pero entra en vigencia el ano siguiente
incometax.s.mexico  = data.frame(ifelse(ag.data$year==1925 & ag.data$country == "Mexico",1,0)) # 
incometax.s.argentina  = data.frame(ifelse(ag.data$year==1933 & ag.data$country == "Argentina",1,0)) 




## Sum across all the rows.
incometax.s = incometax.s.chile +  incometax.s.colombia +  
        incometax.s.ecuador +  incometax.s.guatemala +  
        incometax.s.nicaragua +  incometax.s.peru +  
        incometax.s.venezuela + incometax.s.mexico + incometax.s.argentina
colnames(incometax.s) = "incometax"


## generate the dataset
aux4.s = subset(data, select = c(country, year))
incometax.s = data.frame(aux4.s, incometax.s)

## merge with the dataset
ag.data = merge(incometax.s, ag.data, by=c("country", "year"))
colnames(ag.data)[3] = "incometax.s"

# Add up the two columns
## Because there are no (exact) "ties" between DEMOCRACY and TAXATION, a simple
## cross summation between the two columns will suffice.
### Check this if the sample of countries is modified
ag.data$dem.tax = ag.data$incometax.s+ag.data$democracy.s


# Now, generate a variable to exclude the periods that follow BOTH events (i.e. DEM and INCOMETAX)
##
ag.data=within(ag.data, {
        dem.cumsum <- ave(democracy.s, country, FUN = cumsum)
}
)
ag.data=within(ag.data, {
        tax.cumsum <- ave(incometax.s, country, FUN = cumsum)
}
)

ag.data<-ag.data[!(ag.data$dem.cumsum==1 & ag.data$tax.cumsum==1 & ag.data$incometax.s==0 & ag.data$democracy.s==0),]

## Saving Data
save(ag.data, file = "/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/ag_data.RData") # in paper's folder

##################################################
##              DATA PREP
# Multiple Events Framework: Democracy and Income Taxation Data
## BY SEPARATE
##################################################

## data prep
cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption")


# Load Data
load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/incometax_data.RData") # Load data

### Rename Dataset
tax.dem.long = data

## trim dataset 
# Create a starting point variable for DEMOCRACY.
tax.dem.long.chile = data.frame(ifelse(tax.dem.long$year== 1909 & tax.dem.long$country == "Chile",1,0)) 
tax.dem.long.colombia  = data.frame(ifelse(tax.dem.long$year== 1937 & tax.dem.long$country == "Colombia",1,0)) 
tax.dem.long.ecuador  = data.frame(ifelse(tax.dem.long$year== 1948 & tax.dem.long$country == "Ecuador",1,0)) 
tax.dem.long.guatemala  = data.frame(ifelse(tax.dem.long$year== 1945 & tax.dem.long$country == "Guatemala",1,0)) 
tax.dem.long.nicaragua  = data.frame(ifelse(tax.dem.long$year== 1984 & tax.dem.long$country == "Nicaragua",1,0)) 
tax.dem.long.peru  = data.frame(ifelse(tax.dem.long$year== 1956 & tax.dem.long$country == "Peru",1,0)) 
tax.dem.long.venezuela  = data.frame(ifelse(tax.dem.long$year== 1959 & tax.dem.long$country == "Venezuela",1,0))
tax.dem.long.mexico  = data.frame(ifelse(tax.dem.long$year==2000 & tax.dem.long$country == "Mexico",1,0)) 
tax.dem.long.argentina  = data.frame(ifelse(tax.dem.long$year== 1912 & tax.dem.long$country == "Argentina",1,0))


## Sum across all the rows.
tax.dem.long.d = tax.dem.long.chile +  tax.dem.long.colombia +  
        tax.dem.long.ecuador +  tax.dem.long.guatemala +  
        tax.dem.long.nicaragua +  tax.dem.long.peru +  
        tax.dem.long.venezuela + tax.dem.long.mexico + tax.dem.long.argentina
colnames(tax.dem.long.d) = "democracy"

## generate the dataset
aux3.d = subset(tax.dem.long, select = c(country, year))
democracy.s = data.frame(aux3.d, tax.dem.long.d)

## merge with the dataset
tax.dem.long = merge(democracy.s, tax.dem.long, by=c("country", "year"))
colnames(tax.dem.long)[3] = "democracy.s"

# Create a starting point variable for INCOMETAX
incometax.s.chile = data.frame(ifelse(tax.dem.long$year==1924 & tax.dem.long$country == "Chile",1,0)) # Chile,  1924 (Mamalakis [1976, p. 20]
incometax.s.colombia  = data.frame(ifelse(tax.dem.long$year==1935 & tax.dem.long$country == "Colombia",1,0)) # Colombia, Ley 78 Figueroa2008a, p. 9.
incometax.s.ecuador  = data.frame(ifelse(tax.dem.long$year==1945 & tax.dem.long$country == "Ecuador",1,0)) # Ecuador Aguilera2013 p. 135
incometax.s.guatemala  = data.frame(ifelse(tax.dem.long$year==1963 & tax.dem.long$country == "Guatemala",1,0)) # Guatemala: Decreto 1559, De2007 p 165
incometax.s.nicaragua  = data.frame(ifelse(tax.dem.long$year==1974 & tax.dem.long$country == "Nicaragua",1,0)) # Ley No. 662 de 5 de Noviembre de 1974 (http://legislacion.asamblea.gob.ni/Normaweb.nsf/($All)/024063C3B373125E062570A10057EE73?OpenDocument)
incometax.s.peru  = data.frame(ifelse(tax.dem.long$year==1934 & tax.dem.long$country == "Peru",1,0)) # Peru, Ley 7904 de 1934
incometax.s.venezuela  = data.frame(ifelse(tax.dem.long$year==1943 & tax.dem.long$country == "Venezuela",1,0)) # Venezuela, Ley de Impuesto sobre la Renta, Publicada en la Gaceta Oficial número 20.851 del 17 de julio de 1.942, pero entra en vigencia el ano siguiente
incometax.s.mexico  = data.frame(ifelse(tax.dem.long$year==1925 & tax.dem.long$country == "Mexico",1,0)) 
incometax.s.argentina  = data.frame(ifelse(tax.dem.long$year==1933 & tax.dem.long$country == "Argentina",1,0)) 

## Sum across all the rows.
incometax.s = incometax.s.chile +  incometax.s.colombia +  
        incometax.s.ecuador +  incometax.s.guatemala +  
        incometax.s.nicaragua +  incometax.s.peru +  
        incometax.s.venezuela + incometax.s.mexico + incometax.s.argentina
colnames(incometax.s) = "incometax"


## generate the dataset
aux4.s = subset(data, select = c(country, year))
incometax.s = data.frame(aux4.s, incometax.s)

## merge with the dataset
tax.dem.long = merge(incometax.s, tax.dem.long, by=c("country", "year"))
colnames(tax.dem.long)[3] = "incometax.s"

# Add up the two columns
## Because there are no (exact) "ties" between DEMOCRACY and TAXATION, a simple
## cross summation between the two columns will suffice.
### Check this if the sample of countries is modified
tax.dem.long$dem.tax = tax.dem.long$incometax.s+tax.dem.long$democracy.s


# Now, generate a variable to exclude the periods that follow BOTH events (i.e. DEM and INCOMETAX)
##
tax.dem.long=within(tax.dem.long, {
        dem.cumsum <- ave(democracy.s, country, FUN = cumsum)
}
)
tax.dem.long=within(tax.dem.long, {
        tax.cumsum <- ave(incometax.s, country, FUN = cumsum)
}
)

tax.dem.long<-tax.dem.long[!(tax.dem.long$dem.cumsum==1 & tax.dem.long$tax.cumsum==1 & tax.dem.long$incometax.s==0 & tax.dem.long$democracy.s==0),]


### drop Boix's missings
tax.dem.long = tax.dem.long[!is.na(tax.dem.long$boix_democracy),]

## gen dem.trans
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(data.table)

tax.dem.long.dt = data.table(tax.dem.long)
tax.dem.long.dt = tax.dem.long.dt[, dem.trans := ifelse(duplicated(boix_democracy) & boix_democracy == 1, NA_integer_, boix_democracy), by = rleid(boix_democracy)][]
tax.dem.long = data.frame(tax.dem.long.dt)

## gen tax.trans
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(data.table)

tax.dem.long.dt = data.table(tax.dem.long)
tax.dem.long.dt = tax.dem.long.dt[, tax.trans := ifelse(duplicated(incometax.d) & incometax.d == 1, NA_integer_, incometax.d), by = rleid(incometax.d)][]
tax.dem.long = data.frame(tax.dem.long.dt)

## generate a cumsum of income tax.
tax.dem.long$incometax.d.cumsum <- ave(tax.dem.long$incometax.d, tax.dem.long$country, FUN=cumsum)

## generate a cumsum of dem 
tax.dem.long$democracy.d.cumsum <- ave(tax.dem.long$democracy.d, tax.dem.long$country, FUN=cumsum)

### gen quadratic terms
tax.dem.long$constmanufact.sq = tax.dem.long$constmanufact^2 
tax.dem.long$constagricult.sq = tax.dem.long$constagricult^2
tax.dem.long$democracy.d.cumsum.sq = tax.dem.long$democracy.d.cumsum^2
tax.dem.long$incometax.d.cumsum.sq = tax.dem.long$incometax.d.cumsum^2

## Saving Data
save(tax.dem.long, file = "/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/tax_dem_long.RData") # in paper's folder


##################################################
##              DATA PREP
#                               Lagged CLOGIT
##################################################

cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption")


# Load Data
load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/incometax_data.RData") # Load data

L.clogit = data

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(data.table, DataCombine)

L.clogit <- setorder(L.clogit, country, year)
L.clogit <- subset(L.clogit, select = - incometax.d)


constmanufact.L = slide(L.clogit, Var = "constmanufact", GroupVar = "country", slideBy = 1)$constmanufact1
constagricult.L = slide(L.clogit, Var = "constagricult", GroupVar = "country", slideBy = 1)$constagricult1

L.clogit$constmanufact.L = constmanufact.L
L.clogit$constagricult.L = constagricult.L

## Excluding NAs
L.clogit = L.clogit[!is.na(L.clogit$constmanufact.L),]
L.clogit = L.clogit[!is.na(L.clogit$constagricult.L),]

# The lag was eliminating the incometax.s=1's, so I do "year+1" to "update" the years
L.clogit$year = L.clogit$year+1

# Then I constructed the l.incometax.d.COUNTRY var again...
L.incometax.d.chile = data.frame(ifelse(L.clogit$year>=1924 & L.clogit$country == "Chile",1,0)) # Chile,  1924 (Mamalakis [1976, p. 20]
L.incometax.d.colombia  = data.frame(ifelse(L.clogit$year>=1935 & L.clogit$country == "Colombia",1,0)) # Colombia, Ley 78 Figueroa2008a, p. 9.
L.incometax.d.ecuador  = data.frame(ifelse(L.clogit$year>=1945 & L.clogit$country == "Ecuador",1,0)) # Ecuador Aguilera2013 p. 135
L.incometax.d.guatemala  = data.frame(ifelse(L.clogit$year>=1963 & L.clogit$country == "Guatemala",1,0)) # Guatemala: Decreto 1559, De2007 p 165
L.incometax.d.nicaragua  = data.frame(ifelse(L.clogit$year>=1974 & L.clogit$country == "Nicaragua",1,0)) # Ley No. 662 de 5 de Noviembre de 1974 (http://legislacion.asamblea.gob.ni/Normaweb.nsf/($All)/024063C3B373125E062570A10057EE73?OpenDocument)
L.incometax.d.peru  = data.frame(ifelse(L.clogit$year>=1934 & L.clogit$country == "Peru",1,0)) # Peru, Ley 7904 de 1934
L.incometax.d.venezuela  = data.frame(ifelse(L.clogit$year>=1943 & L.clogit$country == "Venezuela",1,0)) # Venezuela, Ley de Impuesto sobre la Renta, Publicada en la Gaceta Oficial número 20.851 del 17 de julio de 1.942, pero entra en vigencia el ano siguiente
L.incometax.d.mexico  = data.frame(ifelse(L.clogit$year>=1925 & L.clogit$country == "Mexico",1,0)) 
L.incometax.d.argentina  = data.frame(ifelse(L.clogit$year>=1933 & L.clogit$country == "Argentina",1,0)) 

## Summed across all the rows.
L.incometax.d = L.incometax.d.chile +  L.incometax.d.colombia +  
        L.incometax.d.ecuador +  L.incometax.d.guatemala +  
        L.incometax.d.nicaragua +  L.incometax.d.peru +  
        L.incometax.d.venezuela + L.incometax.d.mexico + L.incometax.d.argentina
colnames(L.incometax.d) = "incometax.d"

# attach "L.incometax.d" to the "L.clogit" dataset
## generate the dataset
aux5 = subset(L.clogit, select = c(country, year))
incometax.d = data.frame(aux5, L.incometax.d)

## merge with the dataset
L.clogit = merge(incometax.d, L.clogit, by=c("country", "year"))

## Saving Data
save(L.clogit, file = "/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/l_clogit.RData") # in paper's folder

### TESTING LAG
#### test2= data.frame(L.clogit$country, L.clogit$year, L.clogit$constmanufact.L, L.clogit$constmanufact, L.clogit$constagricult.L, L.clogit$constagricult)
##### IT WORKS

##################################################
##              PLOTTING DEPENDENT VARIABLE
## [survival:plot]
##################################################





## ---- survival:plot ----
# load dataset
load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/cox.RData") # Cox

# generate Contestation var.
cox$Proximity <- as.numeric(cox$constmanufact + (cox$constmanufact*.5) >= cox$constagricult)


if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(plyr)

cox$Industrialization <- mapvalues(cox$Proximity, from = c("0", "1"), to = c("Slow", "Rapid"))


# plot ggsurv function
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2)


ggsurv <- function(s, CI = 'def', plot.cens = T, surv.col = 'gg.def',
                   cens.col = 'red', lty.est = 1, lty.ci = 2,
                   cens.shape = 3, back.white = F, xlab = 'Time',
                   ylab = 'Survival', main = ''){
        library(ggplot2)
        strata <- ifelse(is.null(s$strata) ==T, 1, length(s$strata))
        stopifnot(length(surv.col) == 1 | length(surv.col) == strata)
        stopifnot(length(lty.est) == 1 | length(lty.est) == strata)
        
        ggsurv.s <- function(s, CI = 'def', plot.cens = T, surv.col = 'gg.def',
                             cens.col = 'red', lty.est = 1, lty.ci = 2,
                             cens.shape = 3, back.white = F, xlab = 'Time',
                             ylab = 'Survival', main = ''){
                
                dat <- data.frame(time = c(0, s$time),
                                  surv = c(1, s$surv),
                                  up = c(1, s$upper),
                                  low = c(1, s$lower),
                                  cens = c(0, s$n.censor))
                dat.cens <- subset(dat, cens != 0)
                
                col <- ifelse(surv.col == 'gg.def', 'black', surv.col)
                
                pl <- ggplot(dat, aes(x = time, y = surv)) +
                        xlab(xlab) + ylab(ylab) + ggtitle(main) +
                        geom_step(col = col, lty = lty.est)
                
                pl <- if(CI == T | CI == 'def') {
                        pl + geom_step(aes(y = up), color = col, lty = lty.ci) +
                                geom_step(aes(y = low), color = col, lty = lty.ci)
                } else (pl)
                
                pl <- if(plot.cens == T & length(dat.cens) > 0){
                        pl + geom_point(data = dat.cens, aes(y = surv), shape = cens.shape,
                                        col = cens.col)
                } else if (plot.cens == T & length(dat.cens) == 0){
                        stop ('There are no censored observations')
                } else(pl)
                
                pl <- if(back.white == T) {pl + theme_bw()
                } else (pl)
                pl
        }
        
        ggsurv.m <- function(s, CI = 'def', plot.cens = T, surv.col = 'gg.def',
                             cens.col = 'red', lty.est = 1, lty.ci = 2,
                             cens.shape = 3, back.white = F, xlab = 'Time',
                             ylab = 'Survival', main = '') {
                n <- s$strata
                
                groups <- factor(unlist(strsplit(names
                                                 (s$strata), '='))[seq(2, 2*strata, by = 2)])
                gr.name <-  unlist(strsplit(names(s$strata), '='))[1]
                gr.df <- vector('list', strata)
                ind <- vector('list', strata)
                n.ind <- c(0,n); n.ind <- cumsum(n.ind)
                for(i in 1:strata) ind[[i]] <- (n.ind[i]+1):n.ind[i+1]
                
                for(i in 1:strata){
                        gr.df[[i]] <- data.frame(
                                time = c(0, s$time[ ind[[i]] ]),
                                surv = c(1, s$surv[ ind[[i]] ]),
                                up = c(1, s$upper[ ind[[i]] ]),
                                low = c(1, s$lower[ ind[[i]] ]),
                                cens = c(0, s$n.censor[ ind[[i]] ]),
                                group = rep(groups[i], n[i] + 1))
                }
                
                dat <- do.call(rbind, gr.df)
                dat.cens <- subset(dat, cens != 0)
                
                pl <- ggplot(dat, aes(x = time, y = surv, group = group)) +
                        xlab(xlab) + ylab(ylab) + ggtitle(main) +
                        geom_step(aes(col = group, lty = group))
                
                col <- if(length(surv.col == 1)){
                        scale_colour_manual(name = gr.name, values = rep(surv.col, strata))
                } else{
                        scale_colour_manual(name = gr.name, values = surv.col)
                }
                
                pl <- if(surv.col[1] != 'gg.def'){
                        pl + col
                } else {pl + scale_colour_discrete(name = gr.name)}
                
                line <- if(length(lty.est) == 1){
                        scale_linetype_manual(name = gr.name, values = rep(lty.est, strata))
                } else {scale_linetype_manual(name = gr.name, values = lty.est)}
                
                pl <- pl + line
                
                pl <- if(CI == T) {
                        if(length(surv.col) > 1 && length(lty.est) > 1){
                                stop('Either surv.col or lty.est should be of length 1 in order
                                     to plot 95% CI with multiple strata')
                        }else if((length(surv.col) > 1 | surv.col == 'gg.def')[1]){
                                pl + geom_step(aes(y = up, color = group), lty = lty.ci) +
                                        geom_step(aes(y = low, color = group), lty = lty.ci)
                        } else{pl +  geom_step(aes(y = up, lty = group), col = surv.col) +
                                        geom_step(aes(y = low,lty = group), col = surv.col)}
                } else {pl}
                
                
                pl <- if(plot.cens == T & length(dat.cens) > 0){
                        pl + geom_point(data = dat.cens, aes(y = surv), shape = cens.shape,
                                        col = cens.col)
                } else if (plot.cens == T & length(dat.cens) == 0){
                        stop ('There are no censored observations')
                } else(pl)
                
                pl <- if(back.white == T) {pl + theme_bw()
                } else (pl)
                pl
        }
        pl <- if(strata == 1) {ggsurv.s(s, CI , plot.cens, surv.col ,
                                        cens.col, lty.est, lty.ci,
                                        cens.shape, back.white, xlab,
                                        ylab, main)
        } else {ggsurv.m(s, CI, plot.cens, surv.col ,
                         cens.col, lty.est, lty.ci,
                         cens.shape, back.white, xlab,
                         ylab, main)}
        pl
}

# generate survival object

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(survival)

surv.object = Surv(cox$year, cox$year2, cox$incometax.s, origin=1900)

# plot
ggsurv(survfit(surv.object~Industrialization, cox, conf.type="none")) + 
        theme_bw() + 
        xlab("Year") +
        guides(fill = guide_legend(title = "LEFT")) + 
        theme(axis.text.y = element_text(size=9), 
              axis.text.x = element_text(size=9), 
              axis.title.y = element_text(size=9), 
              axis.title.x = element_text(size=9), 
              legend.text=element_text(size=9),
              legend.title=element_text(size=9),
              legend.position = "bottom",
              legend.key = element_rect(colour = NA, fill = NA, size = 0.5)
        )
## ----








##################################################
##              MODELS 1
## [results:1]
##################################################
rm(list=ls())
cat("\014")


##################################################
### THIS CHUNK GOES BEFORE THE MODELS





## ---- texreg-extractor-geeglm ----

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(texreg,methods)

extract.geepack <- function(model) {
        s <- summary(model)
        names <- rownames(s$coef)
        co <- s$coef[, 1]
        se <- s$coef[, 2]
        pval <- s$coef[, 4]
        
        n <- nrow(model.frame(model))
        nclust <- length(s$geese$clusz)
        
        gof = c(n, nclust)
        gof.names = c("Num. obs.", "Num. clust.")
        
        tr <- createTexreg(
                coef.names = names,
                coef = co,
                se = se,
                pvalues = pval,
                gof.names = gof.names,
                gof = gof,
                gof.decimal = rep(FALSE, length(gof))
        )
        return(tr)
}

setMethod("extract", signature = className("geeglm", "geepack"),
          definition = extract.geepack)
## ---- 



##################################################






## ---- results:1:data ----
# Load Datasets
load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/incometax_data.RData") # Load data
load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/cox.RData") # Cox
load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/logitgee.RData") # Logit GEE
#load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/l_clogit.RData") # Lagged CONSTANT AGR MANUFACT for clogit  (fixed effects)



if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(survival)


# this is the model I use for simulation
cox2 <- coxph(Surv(year, year2, incometax.s) ~ 
                      L_constmanufact +
                      L_constagricult +
                      totpop +
                      cluster(country),
              data = cox)

## logit GEE
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(geepack)

logitgee.1 = geeglm(incometax.d ~ log(constmanufact) + log(constagricult) + log(totpop), 
                    family = binomial, 
                    id = country, 
                    corstr = "independence",
                    std.err = "san.se",
                    data = logitgee)
logitgee.1 = extract(logitgee.1)


# conditional logit
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(survival)

clogit.1 = clogit(
        incometax.d ~  
                log(constmanufact) + 
                log(constagricult) + 
                log(totpop) +
                strata(country), 
        method= "efron", data = data)
## ----



## ---- results:1:table ----
# screenreg / texreg
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(texreg)

texreg(
        list(cox2, logitgee.1, clogit.1), # it needs to be texreg for knitr
        caption = "Sectoral Origins of Income Taxation: Income Tax Law and Industrial Development",
        custom.coef.names = c(
                "Manufacture Output$_{t-1}$",
                "Agricultural Output$_{t-1}$",
                "Total Population",
        
               #"(Intercept)",
        
                "Manufacture Output (ln)",
                "Agricultural Output (ln)",        
                "Total Population (ln)"),
        custom.model.names = c(
                "(1) Cox (1 lag)",# Base Model
                "(2) Logit GEE", # GEE
                "(3) Conditional Logit (FE)" # Fixed Effects model
                ),
        label = "results:1",
        custom.note = "%stars. Robust standard errors, models 1 and 2. Country fixed effects in 3. Intercept omitted.",
        fontsize = "small",
        center = TRUE,
        use.packages = FALSE,
        dcolumn = TRUE,
        booktabs = TRUE,
        omit.coef = "(Intercept)",
        #longtable = TRUE,
        digits = 3,
        table = TRUE,
        stars = c(0.01, 0.05, 0.1),
        #sideways = TRUE,
        no.margin = TRUE, 
        float.pos = "!htbp"
        )
## ---- 


## Appendix
## ---- results:2:data ----

# Load Datasets
load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/L_cox.RData") # Lagged Data 
load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/cox.RData") # Cox


# LAGGED MODEL
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(survival)
cox.L = coxph(Surv(L.cox$year, L.cox$year2, L.cox$incometax.s, origin=1901) ~ 
                      log(constmanufact.L) + 
                      log(constagricult.L) + 
                      log(totpop) +
                      cluster(country), data=L.cox)


# spatial dependence model
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(survival)

spatial.m = coxph(Surv(cox$year, cox$year2, cox$incometax.s, origin=1900) 
                  ~ log(constmanufact) + 
                          log(constagricult) + 
                          log(totpop) +
                          cluster(spatial.cum), 
                  data=cox)

## ----


## ---- results:2:table ----
# screenreg / texreg
if (!require("pacman")) install.packages("pacman"); library(pacman)

p_load(texreg)
texreg(
        list(cox.L,spatial.m), # it needs to be texreg for knitr
        caption = "Sectoral Origins of Income Taxation: Alternative Explanations",
        custom.coef.names = c(
                "Manufacture Output$_{t-1}$ (ln)",
                "Agricultural Output$_{t-1}$ (ln)",
        #        
                "Total Population (ln)",
        #
                "Manufacture Output (ln)",
                "Agricultural Output (ln)"),
        custom.model.names = c(
                "(1) Cox (1 lag, ln)", # Lagged Cox Model 
                "(2) Spatial Dependence"
        ),
        label = "results:2",
        custom.note = "%stars. Robust standard errors in all models",
        fontsize = "small",
        center = TRUE,
        use.packages = FALSE,
        dcolumn = TRUE,
        booktabs = TRUE,
        omit.coef = "(Intercept)",
        #longtable = TRUE,
        digits = 3,
        stars = c(0.01, 0.05, 0.1),
        #sideways = TRUE,
        no.margin = TRUE, 
        float.pos = "!htbp"
        )
## ---- 






##################################################
##              POST ESTIMATION
##################################################



################
# COX PROP
## [fig:simpleplots]
## [fig:coxassump]
################



# Preliminary Plots of the WORKING model
par(mfrow=c(2,1)) 
termplot(cox2, term=1, se=TRUE)
termplot(cox2, term=2, se=TRUE)





## ---- coxassump ----
## Testing Prop. Assumption
test.assumption = cox.zph(cox2, transform = 'log')
print(test.assumption)
par(mar=c(1,1,1,1))
par(mfrow=c(2,1)) 
plot(test.assumption[1]) # covariate 1
plot(test.assumption[2]) # covariate 2
## ----






### If the proportional hazards assumption is true, 
### beta(t) will be a horizontal line. 
### The printout gives a test for slope=0.
### A p-value less than 0.05 indicates a violation of the proportionality assumption.
### The last row contains the global test for all the interactions tested at once. 
### if the predictors satisfy the proportional hazard assumption then the shapes of the curves should be basically the same, and the separation between the curves should remain proportional across analysis time. 
### the proportional hazard assumption is precisely that the coefficient does not change over time


#### COX2 IT IS NOT VIOLATED.
#### COX1 IT IS VIOLATED (NOT USING THIS ONE ANYMORE)
#### TIME DYNAMICS SHOULD BE INCORATED for COX1 i.e. log-time interactions
## Testing Prop. Assumption THIS PART OF THE CODE DOESNT HAVE TOO MUCH BACK UP PART FROM THE CODERS OF THESE FUNCIONS

#### LOGS
cox1.tt2 = coxph(Surv(year, incometax.s) ~ tt(constmanufact) + tt(constagricult) + cluster(country), 
                 data=cox, x=T, tt = c(function(x, t, ...) x * log(t+20), function(x, t, ...) x * log(t-20))
)

test.assumption = cox.zph(cox1.tt2, transform = 'log')
print(test.assumption)
plot(test.assumption[1]) # covariate 1
plot(test.assumption[2]) # covariate 2


#### SPLINES
##### this one here seems to work better compared to logs. Box-St. recommends SPLINES, so I will use them.
cox1.splines = coxph(Surv(year, incometax.s) ~ pspline(constmanufact, df=5) + pspline(constagricult, df=5) + cluster(country), 
                     data=cox)

termplot(cox1.splines, term=1, se=TRUE)
termplot(cox1.splines, term=2, se=TRUE)


########################################################
#### Simulation: Relative Hazard
#### [simulation:1] [simulation:2]
########################################################







## ---- simulation ----
########################  

## industrial sector
# simulate qi's

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(simPH)

# quantities
nsim = 5000 # original: 1000
qi = "Hazard Rate" # original: Hazard Rate
ci = 0.95


set.seed(602)
sim.m.ind <- coxsimLinear(cox2, 
                          b = "L_constmanufact", 
                          qi = qi, 
                          ci = ci,
                          #spin = T,
                          extremesDrop = T,
                          nsim = nsim,
                          Xj = c(min(cox$L_constmanufact), max(cox$L_constmanufact))
)


### agricultural sector
# simulate qi's
set.seed(602)
sim.m.agr <- coxsimLinear(cox2, 
                          b = "L_constagricult", 
                          qi = qi, 
                          ci = ci,
                          #spin = T,
                          extremesDrop = T,
                          nsim = nsim,
                          Xj = c(min(cox$L_constagricult), max(cox$L_constagricult))
)

## function to combine the two plots


if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2,gridExtra,grid)

grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
        
        plots <- list(...)
        position <- match.arg(position)
        g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
        legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
        lheight <- sum(legend$height)
        lwidth <- sum(legend$width)
        gl <- lapply(plots, function(x) x + theme(legend.position="none"))
        gl <- c(gl, ncol = ncol, nrow = nrow)
        
        combined <- switch(position,
                           "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                                  legend,
                                                  ncol = 1,
                                                  heights = unit.c(unit(1, "npc") - lheight, lheight)),
                           "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                                 legend,
                                                 ncol = 2,
                                                 widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
        grid.newpage()
        grid.draw(combined)
        
}

simtitle <- paste(
        paste0("{\\bf ",qi, " of Implementing the Income Tax Law", "}", "."),
        "\\\\\\hspace{\\textwidth}", 
        paste("{\\bf Note}:", "Using estimations of Model 1 in \\autoref{results:1} (\\autoref{results:2}), figure shows", format(nsim,big.mark=",",scientific=FALSE), "simulations with different sectoral growth speeds. `Slow' is the minimum value, while `rapid' is the maximum value for each sectoral output."), 
        paste("The figure also shows the", paste(ci*100, "\\% confidence intervals.", sep = "")), 
        "\n")

## ----





## ---- simulation:plots ----
# install.packages("devtools")
# library(devtools)
# devtools::install_github('christophergandrud/simPH')
# library(simPH)

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2,simPH)


## IMPORTANT! A relative hazard for a unit at zero is always one, as it is a ratio of the hazards with itself. Gandrud2015 p. 10

# Plot
options(scipen=10000)
sim.p.ind = simGG(sim.m.ind, type = 'lines',# type = 'points' // 'lines'
                  xlab = "Year", 
                  ylab = "Hazard Rate", 
                  ribbons = F, alpha = 0.25) + 
        theme_bw() + 
        theme(axis.text.y = element_text(size=8), 
              axis.text.x = element_text(size=8), 
              axis.title.y = element_text(size=8), 
              axis.title.x = element_text(size=8),
              title = element_text(size=9)
        ) +
        labs(title = "Industrial Output") +
        scale_color_manual(labels = c("Slow", "Rapid"), values = c("red", "blue")) +
        guides(color=guide_legend("Sectoral Output"))


# Plot
options(scipen=10000)
sim.p.agr = simGG(sim.m.agr, type = 'lines',# type = 'points' // 'lines'
                  xlab = "Year", 
                  ylab = "Hazard Rate", 
                  ribbons = F, alpha = 0.25) + 
        theme_bw() + 
        theme(axis.text.y = element_text(size=8), 
              axis.text.x = element_text(size=8), 
              axis.title.y = element_text(size=8), 
              axis.title.x = element_text(size=8),
              title = element_text(size=9)
        ) +
        labs(title = "Agriculture Output") +
        scale_color_manual(labels = c("Slow", "Rapid"), values = c("red", "blue")) +
        guides(color=guide_legend("Sectoral Output"))

grid_arrange_shared_legend(sim.p.ind, sim.p.agr, ncol = 2, nrow = 1)
## ----




# It says "All Xl set to 0" b/c it sets the reference category to 0.
# Hazard ratios are COMPARISONS, so the coefficients mean the effects of the Betas, relative to 0.

# Just in case I want to plot these using GGPLOT2, the QI are
## sim3.m$sims["QI"]
## sim3.a$sims["QI"]

########################################################
#### predicted survivor functions
########################################################

# main model
cox3.p = coxph(Surv(year, incometax.s) ~ constmanufact + constagricult + totpop, 
               data=cox
)

# predictions with std. errors
cox3.p.d= data.frame(predict(cox3.p, type="expected", se.fit=TRUE))
cox.predicted= cbind(cox3.p.d, cox)


quantile(cox$constmanufact, .15)

# test
summary(survfit(cox3.p), constmanufact=quantile(cox$constmanufact, .15))
summary(survfit(cox3.p), constmanufact=quantile(cox$constmanufact, .75))

str(summary(survfit(cox3.p), time = 2))

summary(survfit(cox3.p), time=20)$surv


########################################################
#### Simulation: Logit GEE
########################################################

if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(Zelig)

# It's working but it's not a too informative simulation
x.out <- setx(logitgee.1)
set.seed(602)
s.out <- sim(logitgee.1, x = x.out)
plot(s.out)

# TEST

# 1 X's IT IS STARTING MAKING SENSE...NOW I NEED THE PLOTS
## WORKING HERE
logitgee.1.x.L <- setx(logitgee.1, ln.constmanufact = summary(logitgee$ln.constmanufact)[2])
logitgee.1.x.H <- setx(logitgee.1, ln.constmanufact = summary(logitgee$ln.constmanufact)[5])
set.seed(602)
logitgee.1.s <- sim(logitgee.1, x = logitgee.1.x.L, x1= logitgee.1.x.H, num=2500)







########################################################
#### DEMOCRACY DURATION PLOT
########################################################







# ---- democracy:plot ----
cat("\014")
rm(list=ls())
setwd("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption")


# Load Data
load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/incometax_data.RData") # Load data

### Rename Dataset
tax.dem.long = data

### drop Boix's missings
tax.dem.long = tax.dem.long[!is.na(tax.dem.long$boix_democracy),]


## Democracy Plot

### before recode, save country names
levels = c(levels(tax.dem.long$country)[4],
           levels(tax.dem.long$country)[5],
           levels(tax.dem.long$country)[8],
           levels(tax.dem.long$country)[10],
           levels(tax.dem.long$country)[14], 
           levels(tax.dem.long$country)[17],
           levels(tax.dem.long$country)[20]
)

### recode country labels - the plot cant plot cat vars.
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(car)

tax.dem.long$country = recode(as.numeric(tax.dem.long$country), 
                              "4 = 2 ; 
                              5 = 4 ;
                              8 = 6;
                              10 = 8;
                              14 = 10;
                              17 = 12;
                              20 = 14")

tax.dem.long$country <- ordered(tax.dem.long$country,
                                levels = c(2,4,6,8,10,12,14),
                                labels = c(levels))


### label Boix's variable on democracy.
tax.dem.long$boix_democracy <- ordered(tax.dem.long$boix_democracy,
                                       levels = c(0,1),
                                       labels = c("Non-Democracy", "Democracy"))

### plot
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot)

ggplot(tax.dem.long, 
       aes(xmin = year, 
           xmax = year + 1, 
           ymin = country , 
           ymax = as.numeric(country) + .4,
           fill = as.factor(boix_democracy))) + 
        geom_rect(alpha=0.7) +
        #xlim(1895, 2010) + 
        theme_bw() + 
        theme(
                axis.text.y = element_text(size=12), 
                axis.text.x = element_text(size=12), 
                axis.title.y = element_text(size=10), 
                axis.title.x = element_text(size=10), 
                legend.text=element_text(size=15), 
                legend.title=element_text(size=0),
                legend.position = "bottom"
        ) +
        scale_fill_discrete(guide = guide_legend(title = NULL))
# ----




########################################################
#### TAX IMPOSITION PLOT
########################################################










# ---- incometax ----

# Load data
load("/Users/hectorbahamonde/RU/Dissertation/Data/dissertation.Rdata") 

# load libraries
if (!require("pacman")) install.packages("pacman"); library(pacman)
p_load(ggplot2,gridExtra)



# To force GGplots to share same legend.
grid_arrange_shared_legend <- function(...) {
        require(ggplot2)
        require(gridExtra)
        plots <- list(...)
        g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
        legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
        lheight <- sum(legend$height)
        grid.arrange(
                do.call(arrangeGrob, lapply(plots, function(x)
                        x + theme(legend.position="none"))),
                legend,
                ncol = 1,
                heights = grid::unit.c(unit(1, "npc") - lheight, lheight))
}

#### multiplot
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
        library(grid)
        
        # Make a list from the ... arguments and plotlist
        plots <- c(list(...), plotlist)
        
        numPlots = length(plots)
        
        # If layout is NULL, then use 'cols' to determine layout
        if (is.null(layout)) {
                # Make the panel
                # ncol: Number of columns of plots
                # nrow: Number of rows needed, calculated from # of cols
                layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                                 ncol = cols, nrow = ceiling(numPlots/cols))
        }
        
        if (numPlots==1) {
                print(plots[[1]])
                
        } else {
                # Set up the page
                grid.newpage()
                pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
                
                # Make each plot, in the correct location
                for (i in 1:numPlots) {
                        # Get the i,j matrix positions of the regions that contain this subplot
                        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                        
                        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                        layout.pos.col = matchidx$col))
                }
        }
}

#### plots

##### Chile
chile.p = ggplot() + 
        geom_line(data=subset(dissertation, country=="Chile"), aes(x=year, y=log(constagricult), colour="Agricultural Output"), fill=NA, size=1) +
        geom_line(data=subset(dissertation, country=="Chile"), aes(x=year, y=log(constmanufact), colour="Industrial Output"), fill=NA, size=1) + 
        xlab("Year") +
        ylab("GDP Output (ln)") +
        labs(colour = "Legend") +
        scale_x_continuous(limits=c(1890,2010)) + 
        geom_vline(data=subset(dissertation, country=="Chile"), aes(xintercept = 1924, colour= "Income Tax Law"), linetype = "longdash") + # Income Tax Law  
        theme_bw() + 
        theme(axis.text.y = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=10), axis.title.x = element_text(size=10), legend.text=element_text(size=15), legend.title=element_text(size=0),  legend.position = "bottom")  + 
        labs(title="Chile") 

##### Peru
peru.p = ggplot() + 
        geom_line(data=subset(dissertation, country=="Peru"), aes(x=year, y=log(constagricult), colour="Agricultural Output"), fill=NA, size=1) +
        geom_line(data=subset(dissertation, country=="Peru"), aes(x=year, y=log(constmanufact), colour="Industrial Output"), fill=NA, size=1) + 
        xlab("Year") +
        ylab("GDP Output (ln)") +
        labs(colour = "Legend") +
        scale_x_continuous(limits=c(1890,2010)) + 
        geom_vline(data=subset(dissertation, country=="Peru"), aes(xintercept = 1934, colour= "Income Tax Law"), linetype = "longdash") + # Income Tax Law
        theme_bw() + 
        theme(axis.text.y = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=10), axis.title.x = element_text(size=10), legend.text=element_text(size=15), legend.title=element_text(size=0),  legend.position = "bottom")  + 
        labs(title="Peru") 

##### Colombia
colombia.p = ggplot() + 
        geom_line(data=subset(dissertation, country=="Colombia"), aes(x=year, y=log(constagricult), colour="Agricultural Output"), fill=NA, size=1) +
        geom_line(data=subset(dissertation, country=="Colombia"), aes(x=year, y=log(constmanufact), colour="Industrial Output"), fill=NA, size=1) + 
        xlab("Year") +
        ylab("GDP Output (ln)") +
        labs(colour = "Legend") +
        scale_x_continuous(limits=c(1890,2010)) + 
        geom_vline(data=subset(dissertation, country=="Colombia"), aes(xintercept = 1935, colour= "Income Tax Law"), linetype = "longdash") + # Income Tax Law
        theme_bw() + 
        theme(axis.text.y = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=10), axis.title.x = element_text(size=10), legend.text=element_text(size=15), legend.title=element_text(size=0),  legend.position = "bottom")  + 
        labs(title="Colombia") 

##### Ecuador
ecuador.p= ggplot() + 
        geom_line(data=subset(dissertation, country=="Ecuador"), aes(x=year, y=log(constagricult), colour="Agricultural Output"), fill=NA, size=1) +
        geom_line(data=subset(dissertation, country=="Ecuador"), aes(x=year, y=log(constmanufact), colour="Industrial Output"), fill=NA, size=1) + 
        xlab("Year") +
        ylab("GDP Output (ln)") +
        labs(colour = "Legend") +
        scale_x_continuous(limits=c(1890,2010)) + 
        geom_vline(data=subset(dissertation, country=="Ecuador"), aes(xintercept = 1945, colour= "Income Tax Law"), linetype = "longdash") + # Income Tax Law
        theme_bw() + 
        theme(axis.text.y = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=10), axis.title.x = element_text(size=10), legend.text=element_text(size=15), legend.title=element_text(size=0),  legend.position = "bottom")  + 
        labs(title="Ecuador") 

##### Venezuela
venezuela.p= ggplot() + 
        geom_line(data=subset(dissertation, country=="Venezuela"), aes(x=year, y=log(constagricult), colour="Agricultural Output"), fill=NA, size=1) +
        geom_line(data=subset(dissertation, country=="Venezuela"), aes(x=year, y=log(constmanufact), colour="Industrial Output"), fill=NA, size=1) + 
        xlab("Year") +
        ylab("GDP Output (ln)") +
        labs(colour = "Legend") +
        scale_x_continuous(limits=c(1890,2010)) + 
        geom_vline(data=subset(dissertation, country=="Venezuela"), aes(xintercept = 1943, colour= "Income Tax Law"), linetype = "longdash") + # Income Tax Law
        theme_bw() + 
        theme(axis.text.y = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=10), axis.title.x = element_text(size=10), legend.text=element_text(size=15), legend.title=element_text(size=0),  legend.position = "bottom")  + 
        labs(title="Venezuela") 


##### Nicaragua
nicaragua.p= ggplot() + 
        geom_line(data=subset(dissertation, country=="Nicaragua"), aes(x=year, y=log(constagricult), colour="Agricultural Output"), fill=NA, size=1) +
        geom_line(data=subset(dissertation, country=="Nicaragua"), aes(x=year, y=log(constmanufact), colour="Industrial Output"), fill=NA, size=1) + 
        xlab("Year") +
        ylab("GDP Output (ln)") +
        labs(colour = "Legend") +
        scale_x_continuous(limits=c(1890,2010)) + 
        geom_vline(data=subset(dissertation, country=="Nicaragua"), aes(xintercept = 1974, colour= "Income Tax Law"), linetype = "longdash") + # Income Tax Law
        theme_bw() + 
        theme(axis.text.y = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=10), axis.title.x = element_text(size=10), legend.text=element_text(size=15), legend.title=element_text(size=0),  legend.position = "bottom")  + 
        labs(title="Nicaragua")  


##### Guatemala
guatemala.p= ggplot() + 
        geom_line(data=subset(dissertation, country=="Guatemala"), aes(x=year, y=log(constagricult), colour="Agricultural Output"), fill=NA, size=1) +
        geom_line(data=subset(dissertation, country=="Guatemala"), aes(x=year, y=log(constmanufact), colour="Industrial Output"), fill=NA, size=1) + 
        xlab("Year") +
        ylab("GDP Output (ln)") +
        labs(colour = "Income Tax (ln)") +
        scale_x_continuous(limits=c(1890,2010)) + 
        geom_vline(data=subset(dissertation, country=="Guatemala"), aes(xintercept = 1963, colour= "Income Tax Law"), linetype = "longdash") + # Income Tax Law
        theme_bw() + 
        theme(axis.text.y = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=10), axis.title.x = element_text(size=10), legend.text=element_text(size=15), legend.title=element_text(size=0),  legend.position = "bottom")  + 
        labs(title="Guatemala")  


##### Argentina
argentina.p= ggplot() + 
        geom_line(data=subset(dissertation, country=="Argentina"), aes(x=year, y=log(constagricult), colour="Agricultural Output"), fill=NA, size=1) +
        geom_line(data=subset(dissertation, country=="Argentina"), aes(x=year, y=log(constmanufact), colour="Industrial Output"), fill=NA, size=1) + 
        xlab("Year") +
        ylab("GDP Output (ln)") +
        labs(colour = "Income Tax (ln)") +
        scale_x_continuous(limits=c(1890,2010)) + 
        geom_vline(data=subset(dissertation, country=="Argentina"), aes(xintercept = 1933, colour= "Income Tax Law"), linetype = "longdash") + # Income Tax Law
        theme_bw() + 
        theme(axis.text.y = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=10), axis.title.x = element_text(size=10), legend.text=element_text(size=15), legend.title=element_text(size=0),  legend.position = "bottom")  + 
        labs(title="Argentina") 



##### Mexico
mexico.p= ggplot() + 
        geom_line(data=subset(dissertation, country=="Mexico"), aes(x=year, y=log(constagricult), colour="Agricultural Output"), fill=NA, size=1) +
        geom_line(data=subset(dissertation, country=="Mexico"), aes(x=year, y=log(constmanufact), colour="Industrial Output"), fill=NA, size=1) + 
        xlab("Year") +
        ylab("GDP Output (ln)") +
        labs(colour = "Income Tax (ln)") +
        scale_x_continuous(limits=c(1890,2010)) + 
        geom_vline(data=subset(dissertation, country=="Mexico"), aes(xintercept = 1925, colour= "Income Tax Law"), linetype = "longdash") + # Income Tax Law
        theme_bw() + 
        theme(axis.text.y = element_text(size=12), axis.text.x = element_text(size=12), axis.title.y = element_text(size=10), axis.title.x = element_text(size=10), legend.text=element_text(size=15), legend.title=element_text(size=0),  legend.position = "bottom")  + 
        labs(title="Mexico") 

##### All
grid_arrange_shared_legend(
        chile.p, 
        ecuador.p, 
        nicaragua.p, 
        venezuela.p, 
        peru.p, 
        colombia.p, 
        guatemala.p, 
        argentina.p, 
        mexico.p,
        ncol = 3, nrow = 3)

outputstitle <- paste(
        "{\\bf Industrial and Agricultural Outputs, and The Passage of the Income Tax Law}.",
        "\\\\\\hspace{\\textwidth}", 
        "{\\bf Note}: Figure shows historical sectoral outputs, and year of the passage of the income tax law. Following convention, the figure shows logged values.",
        "\\\\\\hspace{\\textwidth}", 
        paste("{\\bf Source}: \\href{http://moxlad-staging.herokuapp.com/home/en?}{MOxLAD}, and other sources compiled by the author (see \\autoref{sample:data:income:tax:tab})."),
        "\n")
# ----



















######################################################################
#### HERE / TEST/ TESTING / PENDING

## WLW (Zorn's Day 6, competing events)
rm(list=ls())
cat("\014")
setwd("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption")

# load data
load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/ag_data.RData")

# dur.dep data organization
dur.dep = ag.data 

dur.dep<-dur.dep[order(dur.dep$country,dur.dep$year),]

dur.dep$one<-rep(1,times=nrow(dur.dep))

library(plyr)
dur.dep<- ddply(dur.dep,"country",mutate,eventno=cumsum(dem.tax)+1,altstart=cumsum(one)-1,altstop=cumsum(one))

# wlw model
## data prep

wlw = dur.dep
wlw<-wlw[rep(1:nrow(wlw),each=max(wlw$eventno)),]

wlw<-ddply(wlw,c("country","year"),mutate, eventrisk=cumsum(one))

wlw$dem.tax<-ifelse(wlw$eventno==wlw$eventrisk & wlw$dem.tax==1,1,0)


## model
library(survival)
wlw.S<-Surv(wlw$altstart, wlw$altstop, wlw$dem.tax)

coxph(wlw.S ~ constmanufact + 
              constagricult + 
              totpop +
              strata(eventno) + cluster(country), data=wlw, method="efron")





## time varying parameters model
dur.dep.t.v.S <- Surv(dur.dep$year,dur.dep$year2,dur.dep$dem.tax)

dur.dep$manXevent<-dur.dep$constmanufact*dur.dep$eventno
dur.dep$agrXevent<-dur.dep$constagricult*dur.dep$eventno

options(scipen = 999) # bias against scientific notation


coxph(dur.dep.t.v.S~ 
              log(manXevent) +
              log(agrXevent) + 
              log(exports) + 
              strata(eventno) + 
              cluster(country),
      data=dur.dep,
      method="efron")


# elapsed time
dur.dep.elap.t.S<-Surv(dur.dep$altstart, dur.dep$altstop, dur.dep$dem.tax)

coxph(dur.dep.elap.t.S ~ 
              constmanufact +
              constagricult + 
              strata(eventno) + 
              cluster(country), 
      data=dur.dep,
      method="efron")


# gap time
library(survival)

dur.dep.t.v.S <- Surv(dur.dep$year,dur.dep$year2,dur.dep$dem.tax)

coxph(dur.dep.t.v.S~ 
              constmanufact +
              constagricult + 
              strata(eventno) + 
              cluster(country),
      data=dur.dep,
      method="efron")

######################################################################

# Time-varying Effects
load("/Users/hectorbahamonde/RU/Dissertation/Papers/IncomeTaxAdoption/cox.RData") # Cox

library(simPH)
# Create time interactions
cox.tvc <- tvc(cox, b = c('constmanufact', 'constagricult'), tvar = 'year2', tfun = 'log')


# Run Cox PH Model // time transformed log-time.
library(survival) # install.packages("survival") 
cox2 <- coxph(Surv(year, year2, incometax.s) ~ 
                      constmanufact +
                      constagricult +
                      constmanufact_log +
                      constagricult_log +
                      cluster(country),
              data = cox.tvc)

# constagricult simulation plot
set.seed(602)
sim3.a = coxsimtvc(obj = cox2, 
                   b = "constagricult", 
                   btvc = "constagricult_log",
                   qi = "First Difference",
                   tfun = "log", 
                   from = min(cox.tvc$year), 
                   to = max(cox.tvc$year),
                   spin = F,
                   Xl = NULL,
                   Xj = 1, 
                   by = 2, ci = 0.95, nsim = 100)


# constmanufact simulation plot
set.seed(602)
sim3.m = coxsimtvc(obj = cox2, 
                   b = "constmanufact", 
                   btvc = "constmanufact_log",
                   qi = "First Difference",
                   tfun = "log", 
                   from = min(cox.tvc$year), 
                   to = max(cox.tvc$year),
                   spin = F,
                   Xl = NULL,
                   Xj = 1, 
                   by = 2, ci = 0.95, nsim = 100)


library(simPH)
library(ggplot2)
sim.1 = simGG(sim3.m, type = 'points',# type = 'points' // 'lines'
              #xlab = "Industrial\nOutput", 
              ylab = "", 
              ribbons = T, alpha = 0.2) + 
        theme_bw() + 
        theme(axis.text.y = element_text(size=8), 
              axis.text.x = element_text(size=8), 
              axis.title.y = element_text(size=8), 
              axis.title.x = element_text(size=8)
        )


sim.2 = simGG(sim3.a, type = 'points',# type = 'points' // 'lines'
              #xlab = "Agricultural\nOutput", 
              ylab = "", 
              ribbons = T, alpha = 0.5) + 
        theme_bw() + 
        theme(axis.text.y = element_text(size=8), 
              axis.text.x = element_text(size=8), 
              axis.title.y = element_text(size=8), 
              axis.title.x = element_text(size=8)
        )


# type = 'points'
# type = 'lines'

library(cowplot) # install.packages("cowplot")
plot_grid(sim.1, sim.2, ncol = 2, labels = "auto", label_size = 7)


