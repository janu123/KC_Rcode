## Averages
numSummary(fulldata[,c("number_of_employees", 
                                     "number_of_years_in_the_business", "owners_age")], statistics=c("mean"))
## Averages of the procurement and selling value, quantity
materials<-materials %>% mutate (procure =(SP*Volume)/82,
                  procure1=Volume/82)
numSummary(data2[,c("procure","procure1")],statistics=c("mean"))

## Percentages

Table <- with(fulldata, table(do_you_have_mobile_phone))
print(round(100*Table/sum(Table), 2))
Table1<-with(fulldata, table(owners_educational_qualifications))
print(round(100*Table1/sum(Table1), 2))

