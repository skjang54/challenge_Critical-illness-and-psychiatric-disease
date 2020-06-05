install.packages("tidyr")
library("dplyr")
library("tidyr")
library("bigrquery")
options(gargle_oob_default=TRUE)
options(httr_oauth_cache=FALSE)

project_id <- "korea-dfthon-2019"
options(httr_oauth_cache=FALSE)

# Wrapper for running BigQuery queries.
run_query <- function(query){
  tb <- bq_project_query(project_id, query)
  return(bq_table_download(tb))
}

## ===============  pre-icu
df <- run_query('
                SELECT person_id, 
                prePsyDate,
                date_diff( firstTrache, prePsyDate,DAY) AS duration_case, -- firstTrache - prePsyDate
                date_diff( firstAdmit, prePsyDate,DAY) AS duration_control, -- firstAdmit - prePsyDate
                case when firstTrache is not null then 1
                else 0 end as Trache,
                firstAdmit, year_of_birth, firstTrache, -- for age 
                CASE WHEN gender_concept_id = 8532 then 1
                WHEN gender_concept_id = 8507 then 0
                ELSE NULL END
                as female,
                case when diabetes is not null then 1
                else  0 end as dm,
                Hyper as htn,
                case when Alz is not null then 1
                else  0 end as alz
                FROM `korea-datathon-2019.team13.dataset_02`
                ')%>% as.data.frame()
# 243,501 rows

## calculate age 
# data stucture


firstAdmit_yr=(df$firstAdmit)%>% as.character() %>% gsub(pattern="-..-..$", replacement = "")
firstTrache_yr=(df$firstTrache)%>% as.character() %>% gsub(pattern="-..-..$", replacement = "")
startyr = ifelse(df$Trache==1,firstTrache_yr, firstAdmit_yr)%>% as.integer()
df$age= startyr-df$year_of_birth

hist(df$age, col="blue")

finaldat = df[(df$age>18)&(df$age<80)&!is.na(df$age),!colnames(df)%in%c("firstTrache","firstAdmit","year_of_birth")]
finaldat$Trache%>%unique
finaldat%>%str
finaldat%>%NROW
table(finaldat$Trache)
finaldat$duration = ifelse(finaldat$Trache==1,finaldat$duration_case, finaldat$duration_control)


tmp=finaldat$duration[!is.na(finaldat$prePsyDate)]
tmp[tmp>0]%>% hist 
tmp[tmp>0]%>% summary()


## ===================== post-icu
postdf <- run_query('SELECT person_id,  postPsyDate, prePsyDate,-- lastTrache, lastAdmit --postPsyDate, for criteria
                    date_diff( lastTrache, postPsyDate,DAY) AS duration_case, -- lastTrache - postPsyDate
                    date_diff( lastAdmit, postPsyDate,DAY) AS duration_control, -- lastAdmit - postPsyDate
                    date_diff( lastTrache, prePsyDate,DAY)<0 AS cr_case, 
                    date_diff( lastAdmit, prePsyDate,DAY)<0 AS cr_control, 
                    case when lastTrache is not null then 1
                    else 0 end as Trache,
                    lastAdmit, year_of_birth, lastTrache, -- for age 
                    CASE WHEN gender_concept_id = 8532 then 1
                    WHEN gender_concept_id = 8507 then 0
                    ELSE NULL END
                    as female,
                    case when diabetes is not null then 1
                    else  0 end as dm,
                    Hyper as htn,
                    case when Alz is not null then 1 
                    else  0 end as alz
                    FROM `korea-datathon-2019.team13.dataset_02`
')%>% as.data.frame()

postdf = postdf[(postdf$Trache&postdf$cr_case)|(!postdf$Trache&postdf$cr_control)|is.na(postdf$postPs),]

lastAdmit_yr=(postdf$lastAdmit)%>% as.character() %>% gsub(pattern="-..-..$", replacement = "")
lastTrache_yr=(postdf$lastTrache)%>% as.character() %>% gsub(pattern="-..-..$", replacement = "")
startyr = ifelse(postdf$Trache==1,lastTrache_yr, lastAdmit_yr)%>% as.integer()
postdf$age= startyr-postdf$year_of_birth


hist(postdf$age, col="blue")
hist(finaldat_post$age, col="blue")

finaldat_post = postdf[(postdf$age>18)&(postdf$age<80)&!is.na(postdf$age),!colnames(postdf)%in%c("lastTrache","lastAdmit","year_of_birth")]

finaldat_post$Trache %>%unique
finaldat_post%>%str
finaldat_post%>%NROW
table(finaldat_post$Trache)
finaldat_post$duration = ifelse(finaldat_post$Trache==1,finaldat_post$duration_case, finaldat_post$duration_control)


tmp=finaldat_post$duration[!is.na(finaldat_post$postPsyDate)]
(-tmp[tmp<0])%>% hist 
(-tmp[tmp<0])%>% summary()
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.0   196.2   961.5  1671.0  2569.0  8476.0 
# median : 2.6 yr
# 3rd q :7yr
finaldat_post$duration=-finaldat_post$duration
finaldat_post$PostPsyOutcome=as.integer(!is.na(finaldat_post$prePsyDate))

# > summary(finaldat_post$duration)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#     1.0   196.2   961.5  1671.0  2569.0  8476.0   56008 

##################
# Cox model in R #
##################

install.packages(c("survival","survminer"))

library("survival")
library("survminer")

#The function coxph() can be used to compute the Cox proportional
#hazards regression model in R.
#coxph(formula, data, method)

#Univariate Cox regression
res.cox<-coxph(Surv(duration,PostPsyOutcome)~Trache,data=finaldat_post)
res.cox
summary(res.cox)


#Univaruate coxph function for multiple covariates
covariates <-colnames(finaldat_post)[8:13]
univ_formulas <- sapply(covariates,
                        function(x) as.formula(paste('Surv(duration,PostPsyOutcome)~', x)))

univ_models <- lapply( univ_formulas, function(x){coxph(x, data = finaldat_post)})
# Extract data 
univ_results <- lapply(univ_models,
                       function(x){ 
                         x <- summary(x)
                         p.value<-signif(x$wald["pvalue"], digits=2)
                         wald.test<-signif(x$wald["test"], digits=2)
                         beta<-signif(x$coef[1], digits=2);#coeficient beta
                         HR <-signif(x$coef[2], digits=2);#exp(beta)
                         HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
                         HR.confint.upper <- signif(x$conf.int[,"upper .95"],2)
                         HR <- paste0(HR, " (", 
                                      HR.confint.lower, "-", HR.confint.upper, ")")
                         res<-c(beta, HR, wald.test, p.value)
                         names(res)<-c("beta", "HR (95% CI for HR)", "wald.test", 
                                       "p.value")
                         return(res)
                         #return(exp(cbind(coef(x),confint(x))))
                       })
res <- t(as.data.frame(univ_results, check.names = FALSE))
as.data.frame(res)


# beta HR (95% CI for HR) wald.test p.value
# Trache   0.41      1.5 (1.2-1.9)        11 0.00077
# female  -0.21   0.81 (0.67-0.99)       4.2   0.041
# dm     -0.043    0.96 (0.71-1.3)      0.08    0.78
# htn     -0.12    0.88 (0.71-1.1)       1.2    0.26
# alz     0.019       1 (0.51-2.1)         0    0.96
# age    0.0054            1 (1-1)       2.2    0.14


#Multivariate Cox regression analysis

res.cox<-coxph(Surv(duration,PostPsyOutcome)~Trache+female+dm+htn+alz+age,data=finaldat_post)
summary(res.cox)

###
dt=finaldat_post[finaldat_post$duration<30,]

cox_m<-function(x){
  dt=finaldat_post[(finaldat_post$duration>(x-1)*30)&(finaldat_post$duration<x*30),]
res.cox<-coxph(Surv(duration,PostPsyOutcome)~Trache,data=dt)
return(res.cox)
}

cox_m(1)
cox_m(2)
cox_m(3)
cox_m(4)



multicox_m<-function(x){
  dt=finaldat_post[(finaldat_post$duration>(x-1)*30)&(finaldat_post$duration<x*30),]
  res.cox<-coxph(Surv(duration,PostPsyOutcome)~Trache+female+dm+htn+alz+age,data=dt)
  return(res.cox)
}


multicox_m(1)
multicox_m(2)
multicox_m(3)
multicox_m(4)
multicox_m(15)

###
subset1= finaldat_post[which(finaldat_post$duration<median(finaldat_post$duration,na.rm = T)),]
subset2= finaldat_post[which(finaldat_post$duration<summary(finaldat_post$duration)[5]),] #3rdQ
subset3= finaldat_post[which(finaldat_post$duration<summary(finaldat_post$duration)[6]),] #Max
subset1%>% NROW #208
summary(finaldat_post$duration)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#     1.0   196.2   961.5  1671.0  2569.0  8476.0   56008 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#       1      48     400    1003    1391    7061   56008 
res.cox_sub1<-coxph(Surv(duration,PostPsyOutcome)~Trache+female+dm+htn+alz+age,data=subset1);summary(res.cox_sub1)
res.cox_sub2<-coxph(Surv(duration,PostPsyOutcome)~Trache+female+dm+htn+alz+age,data=subset2);summary(res.cox_sub2)
res.cox_sub3<-coxph(Surv(duration,PostPsyOutcome)~Trache+female+dm+htn+alz+age,data=subset3);summary(res.cox_sub3)
res.cox_sub4<-coxph(Surv(duration,PostPsyOutcome)~Trache+female+dm+htn+alz+age,data=subset4);summary(res.cox_sub4)

# 
subset1= finaldat_post[which(finaldat_post$duration<summary(finaldat_post$duration)[2]),] # 0 to 1stQ
subset2= finaldat_post[which(finaldat_post$duration<summary(finaldat_post$duration)[3]&finaldat_post$duration>summary(finaldat_post$duration)[2]),] # 1stQ to Med
subset3= finaldat_post[which(finaldat_post$duration<summary(finaldat_post$duration)[5]&finaldat_post$duration>summary(finaldat_post$duration)[3]),] #Med to 3rdQ 
subset4= finaldat_post[which(finaldat_post$duration<summary(finaldat_post$duration)[6]&finaldat_post$duration>summary(finaldat_post$duration)[5]),] #3rdQ to Max 
res.cox_sub1<-coxph(Surv(duration,PostPsyOutcome)~Trache+female+dm+htn+alz+age,data=subset1);summary(res.cox_sub1)
res.cox_sub2<-coxph(Surv(duration,PostPsyOutcome)~Trache+female+dm+htn+alz+age,data=subset2);summary(res.cox_sub2)
res.cox_sub3<-coxph(Surv(duration,PostPsyOutcome)~Trache+female+dm+htn+alz+age,data=subset3);summary(res.cox_sub3)
res.cox_sub4<-coxph(Surv(duration,PostPsyOutcome)~Trache+female+dm+htn+alz+age,data=subset4);summary(res.cox_sub4)



#Visualizing the estimated distribution of survival times


#main_whole combined graph from res.cox (multivariate Cox regression)
ggsurvplot(survfit(res.cox), color = "#2E9FDF",data=finaldat_post,
           ggtheme = theme_minimal())


#alternative1_focusing on sex from res.cox (multivariate Cox regression)
#If you want to assess the impact of the sex on the esimated survival probability.
#You should build the new data from the original, making other covariates than the sex constant (average level or lowest level)
data(lung)
# Create the new data  
sex_df <- with(lung,
               data.frame(sex = c(1, 2), 
                          age = rep(mean(age, na.rm = TRUE), 2),
                          ph.ecog = c(1, 1)
               )
)
sex_df

# Survival curves
fit <- survfit(res.cox, newdata = sex_df)
ggsurvplot(fit, conf.int = TRUE, legend.labs=c("Sex=1", "Sex=2"),
           ggtheme = theme_minimal(),pval=TRUE,data=sex_df)
require(survival)
cancer$sex <- factor(ifelse(cancer$sex == 1, "male", "female"))

mod.1 <- coxph(Surv(time, status) ~ age + wt.loss, data=cancer)
plot(mod.1)
plot(mod.1, typical=function(x) quantile(x, c(.25, .75)))



# ============================================

# 1.2 Difference-in-means

df_cov <- c('age', 'male', 'dm', 'htn', 'alz')
finaldat %>%
  group_by(Trache) %>%
  select(one_of(df_cov)) %>%
  summarise_all(funs(mean(., na.rm = T)))
lapply(df_cov, function(v) {
  t.test(finaldat[, v] ~ finaldat[, 'Trache'])
})
with(finaldat, t.test(age ~ Trache))  #(repeat for each covariate)


# 2 Propensity score estimation
for(i in 2:NCOL(finaldat)-2){
  finaldat[,i]<-as.factor(finaldat[,i])
}
finaldat%>%str
m_ps <- glm(Trache ~ age+male+dm+htn+alz,
            family = binomial(), df = finaldat)
summary(m_ps)
prs_df <- df.frame(pr_score = predict(m_ps, type = "response"),
                   Trache = m_ps$model$Trache)
head(prs_df)


# 3 Executing a matching algorithm

df_nomiss <- df %>%  # MatchIt does not allow missing values
  select(c5r2mtsc_std, Trache, one_of(df_cov)) %>%
  na.omit()

mod_match <- matchit(Trache ~ race_white + w3income + p5hmage + p5numpla + w3momed_hsb,
                     method = "nearest", df = df_nomiss)
dta_m <- match.df(mod_match)
dim(dta_m)

