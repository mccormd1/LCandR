## LC_investmentpicker.r - hopefully screens LC data and finds data with lowest default rate.
#unfortunately this has no algorithm optimization b/c idk where the data to train would be.



getwd()
dir()
setwd("Downloads/")
my_data<-read_csv('primaryMarketNotes-browseNotes_1-RETAIL.csv')

summary(my_data)
names(my_data)
##important columns are: int_rate, installment, term, purpose, annual_inc, delinq_2yrs, home_ownership, emp_length, revol_bal

#filter: term=36, purpose=c("Credit card refinancing", "Debt consolidation"), home_ownership=c("MORTGAGE","OWN") 
#emp_length=c("3 years","4 years","5 years","6 years","7 years","8 years","9 years","10+ years")
#delinq_2yrs=0

#algorithm, want big positive numbers: annual_inc-(120*installment), want small num revol_bal/annual_inc

#rank: int_rate - final ordering, highest to lowest

my_data %>%
  select(id,int_rate,installment,term,purpose,annual_inc,delinq_2yrs,home_ownership,emp_length,revol_bal,dti)->rel_data

my_data %>%
  select(id,home_ownership) %>%
  mutate(ho=as.numeric(as.factor(home_ownership))) ->mytest
  class(mytest$homeownership)
  
rel_data %>%
  filter(term==36, purpose %in% c("Credit card refinancing", "Debt consolidation"), home_ownership %in% c("MORTGAGE","OWN"),emp_length %in% c("3 years","4 years","5 years","6 years","7 years","8 years","9 years","10+ years"),delinq_2yrs==0) %>%
  mutate(payperc=installment/(annual_inc/12)) %>%
  filter(dti<=25,payperc<=.10) %>%
  arrange(desc(int_rate)) -> ranked_data
  head(ranked_data,10)
  