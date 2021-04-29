

chooseCRANmirror(ind=90) #set the CRAN repository for the session
packages_vector <- c("dplyr","ggplot2","tidyverse","MatchIt","optmatch","RItools","table1","tableone","readxl",
                     "np","stargazer","Matching","rgenoud","sandwich","boot","survival","lmtest")
#install.packages(packages_vector)
lapply(packages_vector, require, character.only = TRUE)



matching_final <- read_excel("~/Desktop/Data/PSM/Final_Final_Final_PSM.xlsx")
view(matching_final)

country_data <- function(x){
  matching_final %>% filter(`Country Name` == x) %>% select(!c(`Country Name`)) %>%t() %>% as.data.frame() -> country
  factors_in_psm <- c()
  #for(j in country[1,1:7]){factors_in_psm<- append(factors_in_psm,j)}
  colnames(country) <- PSM_columns
  country <- country[2:nrow(country),]
  country <- sapply(country,as.numeric)
  return(country)
}

PSM_columns <- c("ST_Int","LT_Int","Inflation","GDP","CAB","FDI","FCI","IT")
matching_final$`Country Name`  %>% unique() -> country_names
country_names <- country_names[2:length(country_names)]
PSM <- country_data("Australia")
for (i in country_names){
  country <- country_data(i)
  PSM <- as.data.frame(rbind(PSM,country))
}
View(PSM)
str(PSM)
PSM$`IT` <- as.factor(PSM$`IT`)

# Calculating propensity scores on my own using logit or probit in order to see its fit : 
vignette("np",package  = "np")
model.index <- npindex(IT ~ ST_Int + LT_Int + Inflation + GDP + FDI, method = "kleinspady",gradients =TRUE, data = PSM)
summary(model.index)
plot(model.index)

model.np<- npconmode(IT ~ ST_Int + LT_Int + Inflation + GDP + FDI, family = binomial, data = PSM)


logi_fit <- glm(IT ~ ST_Int + LT_Int + Inflation + GDP + FDI, family = binomial, data = PSM)
summary(logi_fit)

logi_fit$coefficients
write.csv(PSM,file = "PSM_prior.csv")


PSM_df <- na.omit(PSM)
PSM_with_scores <- cbind(PSM_df, data.frame(pr_score = predict(logi_fit, type = "response")))
#write.csv(PS_all,file = "PS_all.csv")

# Before matching plots
PSM_with_scores %>%
  ggplot(aes(x = pr_score)) + 
  geom_histogram(color = "blue", binwidth = 0.01) + 
  facet_wrap(~IT) + 
  xlab("Probability of adopting Inflation targeting")
 
View(PS_all)
PS_98<- PS_all[c(7:22,29:44,51:66,73:88,95:110,117:132,139:154,161:176,183:198,205:220,227:242,249:264,271:286,293:308,315:330,337:352),]
str(PS_98)
FCI_col <- as.data.frame(FCI_scaled$FCI_Australia)
colnames(FCI_col) <- c("FCI")
for( j in FCI_scaled[,c(2:12,14,13,15,16)]){
  j_d <- as.data.frame(j)
  colnames(j_d) <- c("FCI")
  FCI_col %>% rbind(j_d) -> FCI_col
}
View(FCI_col)
View(PS_98)

PS_98 %>% cbind(FCI_col) -> matching_trial
View(matching_trial)
write.csv(matching_trial,"matching_trial.csv")


#matching_df <- PSM[c(7:22,29:44,51:66,73:88,95:110,117:132,139:154,161:176,183:198,205:220,227:242,249:264,271:286,293:308,315:330,337:352),] %>% cbind(FCI_col)
#matching_df$IT <- as.factor(matching_df$IT)
#View(matching_df)


#Checking Initial Imbalance : 

pre_match<- matchit(IT ~ ST_Int + LT_Int + Inflation + GDP + CAB + FDI, distance = "glm",method = NULL, data = PSM_df)
summary(pre_match)

#Note the std mean differences and whatever the hell variance ratio and ecdf imply

#Conducting the matching using nearest neighbour

nearestnb_match<- matchit(IT ~ ST_Int + LT_Int + Inflation + GDP + CAB + FDI, distance = "glm",method = "nearest", data = PSM_df)

summary(nearestnb_match,un= FALSE)
plot(nearestnb_match,type = "jitter", interactive = FALSE)
plot(nearestnb_match,type = "qq",interactive = FALSE)


# Matching using full matching
full_match<- matchit(IT ~ ST_Int + LT_Int + Inflation + GDP + CAB + FDI,distance = "mahalanobis",method = "full", data = PSM_df)
plot(summary(full_match, un = FALSE))
plot(full_match,type = "jitter", interactive = FALSE)
plot(full_match,type = "qq",interactive = FALSE)

plot(summary(full_match)) 

?matchit

#Estimating Treatment Effects for the above two PSMs
NN_data<- match.data(nearestnb_match)

ATE_NN <- lm(FCI ~ IT + ST_Int + LT_Int + Inflation + GDP + CAB + FDI,weights = weights, data = NN_data)
coeftest(ATE_NN,vcov. = vcovCL,cluster = ~subclass)     

full_data <- match.data(full_match)
ATE_full <- lm(FCI ~ IT + ST_Int + LT_Int + Inflation + GDP + CAB + FDI,weights = weights, data = full_data)
coeftest(ATE_full,vcov. = vcovCL,cluster = ~subclass)

# Non Propensity score matching

PSM_df <- drop_na(PSM)
view(PSM_df)

#IMPORTANT
# Test results on full matching - Final - MDM measure, ATE
Tr <- as.logical(PSM_df$IT)
imp_match <- Match(PSM_df$FCI,Tr,PSM_df[,1:6])

full_match<- matchit(IT ~ ST_Int + LT_Int + Inflation + GDP + CAB + FDI,distance = "mahalanobis",method = "full", data = PSM_df,estimand = "ATE")
plot(full_match)
dev.new(width=5,height=4,noRStudioGD = TRUE)
dev.off()
plot(summary(full_match)) #for balance and reporting
stargazer(full_att_summary$sum.matched)
plot(full_match,type = "ecdf",interactive = TRUE)
plot(full_match, type = "qq",interactive = TRUE)

#without covariate adjustment
md <- match.data(full_match)
view(md)
md %>% filter(subclass == 134)


fit1 <- lm(FCI ~ IT, data = md,weights = weights)
summary(fit1)
full_match_t_test <- coeftest(fit1,vcov. = vcovCL,cluster = ~subclass)
full_match_t_test

#With covariate adjustment 

md_cen <- md

covs_to_center <- c("ST_Int","LT_Int","Inflation","GDP","CAB","FDI")
#covs_to_center <- c("Inflation","FDI")
md_cen[covs_to_center] <- scale(md_cen[covs_to_center],scale = FALSE)

fit2 <- lm(FCI ~ IT * (Inflation + FDI + ST_Int + LT_Int + GDP + CAB), data = md_cen,weights = weights)
x<-coeftest(fit2,vcov. = vcovCL, cluster = ~subclass)
stargazer(a)
a<-confint(x)[1:2,]


# Matching procedure includes calculating propensity scores
 PSM_df<- na.omit(PSM)
  
  
trial_match<- matchit(IT ~ ST_Int + LT_Int + Inflation + GDP + CAB + FDI, distance = "mahalanobis",method = "full", data = PSM_df)
trial_1 <- match.data(trial_match)
View(trial_1)

unmatched(trial_1)

#Examining covariate balance using visual inspection
fn_bal <- function(dta, variable) {
  dta$variable <- dta[, variable]
  dta$IT <- as.factor(dta$IT)
  support <- c(min(dta$variable), max(dta$variable))
  ggplot(dta, aes(x = distance, y = variable, color = IT)) +
    geom_point(alpha = 0.2, size = 1.3) +
    geom_smooth(method = "loess", se = F) +
    xlab("Propensity score") +
    ylab(variable) +
    theme_bw() +
    ylim(support)
}

library(gridExtra)
grid.arrange(
  fn_bal(trial_1, "ST_Int"),
  fn_bal(trial_1, "LT_Int"),
  fn_bal(trial_1, "Inflation"),
  fn_bal(trial_1, "GDP"),
  fn_bal(trial_1, "CAB"),
  fn_bal(trial_1, "FDI"),
  nrow = 3, widths = c(1, 0.8)  )

# difference in means :
trial_1[1:7] %>% group_by(IT) %>% summarise_all(mean)

# Another map of checking balance before looking at outcomes:

plot(trial_match, type = "h")
plot(trial_match, type = "qq")

plot(summary(trial_match)) # for abs standardised mean differences

summary(trial_match)

#Estimating Treatment effects 

with(trial_1,t.test(FCI ~ IT))

lm_treat <- lm(FCI ~ IT + ST_Int + LT_Int + Inflation + GDP + CAB + FDI, data = trial_1)
summary(lm_treat)

xvars <- c("ST_Int","LT_Int","Inflation","GDP","CAB","FDI")
table_trial <- CreateTableOne(vars = xvars,strata = "IT",data=trial_match)
print(table_trial)
summary(trial_match)

str(trial_match)

#outcome analysis

len <- max(as.numeric(trial_1$subclass))
y_trt = c()
y_con = c()
for(k in 1:len){
  y_trt <- append(y_trt,c(trial_1$FCI[trial_1$IT == 1 & trial_1$subclass == k]))
  y_con <- append(y_con,c(trial_1$FCI[trial_1$IT == 0 & trial_1$subclass == k]))
}


#plots

years<-rep(c(1998:2013),times = 23)
PSM_years<- cbind(PSM,years)

PSM_years %>%filter(IT ==0) %>% group_by(years) %>% summarise(mean(Inflation)) -> NonIT
NonIT %>% cbind(temp = "NonIT") -> NonIT
PSM_years %>%filter(IT ==1) %>% group_by(years) %>% summarise(mean(Inflation)) -> ITer
ITer%>% cbind(temp = "IT") -> ITer

inflation_graph <- rbind(NonIT,ITer)
inflation_graph$temp <- as.factor(inflation_graph$temp) 
view(FCI_graph)
ggplot(data = inflation_graph, mapping = aes(x = `years`, y= `mean(Inflation)`,color = temp)) + geom_line()

IT_countries <- c("Australia(1993M4),Canada(1991M2),Czech Republic(1997M12),Iceland(2001M3),South Korea(2001M1),Norway(2001M3),Slovakia(2005M1),Switzerland(2000M1 rose 2007),Sweden(1993M1),United Kingdom(1992M10)")
NonIT_countries <- c("Austria,Belgium,Denmark,Finland,France,Germany,Greece,Ireland,Italy,Japan,Singapore,Spain,United States")
Country_table<-data.frame("Inflation Targeters" = IT_countries,"Non Inflation Targeters" = NonIT_countries)

stargazer(Country_table)
