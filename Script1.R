
## Part B
## Averages
numSummary(fulldata[,c("number_of_employees", 
                                     "number_of_years_in_the_business", "owners_age")], statistics=c("mean"))
## Averages of the procurement and selling value, quantity
na.omit(fulldata)
n=nrow(fulldata)
n
materials<-materials %>% mutate (procure =(SP*Volume)/n,
                  procure1=Volume/n)
numSummary(data2[,c("procure","procure1")],statistics=c("mean"))

## Percentages

Table <- with(fulldata, table(do_you_have_mobile_phone))
print(round(100*Table/sum(Table), 2))
Table1<-with(fulldata, table(owners_educational_qualifications))
print(round(100*Table1/sum(Table1), 2))

## Part D
## Materials Profile 
attach(materials)
materials$temp<- CP*Volume
Total_amount_procured=materials %>% group_by(Material) %>% 
  summarise(Totalamount=sum(temp, na.rm=TRUE))%>% mutate(Perc=(Totalamount/sum(Totalamount))*100)
Total_Volume=materials %>% group_by(Material) %>% 
  summarise(Totalvol=sum(Volume, na.rm=TRUE)) %>% mutate(Perc=(Totalvol/sum(Totalvol))*100)

## Part E 
## Processing
### Equipment to process material
Glass <- with(fulldata, 
               table(do_you_use_any_equipment_to_process_the_material_glass))
print(round(100*Glass/sum(Glass), 2))
Metal<-with(fulldata, 
            table(do_you_use_any_equipment_to_process_the_material_metal))
print(round(100*Metal/sum(Glass), 2))
Paper<-with(fulldata, 
            table(do_you_use_any_equipment_to_process_the_material_paper))
print(round(100*Paper/sum(Paper), 2))
plastic <- with(fulldata, 
              table(do_you_use_any_equipment_to_process_the_material_plastic))
print(round(100*plastic/sum(plastic), 2))

### Using power or Electricity
Power <- with(fulldata, table(do_you_consume_power_for_processing))
print(round(100*Power/sum(Power), 2))
Water <- with(fulldata, table(do_you_consume_water_for_processing))
print(round(100*Water/sum(Water), 2))

### Storage
Secondary<- with(fulldata, table(do_you_have_secondary_storage_units))
print(round(100*Secondary/sum(Secondary), 2))
Increase <- with(fulldata, 
              table(have_you_increased_storage_space_in_the_last_5.10_years))
print(round(100*Increase/sum(Increase), 2))

### Health and Safety
Glass<- with(fulldata, 
              table(are_there_any_risks_involved_dealing_._working_with_this_material_glass))
print(round(100*Glass/sum(Glass), 2))
Metal<-with(fulldata, 
            table(are_there_any_risks_involved_dealing_._working_with_this_material_metal))
print(round(100*Metal/sum(Metal), 2))
Paper<-with(fulldata, 
     table(are_there_any_risks_involved_dealing_._working_with_this_material_paper))
print(round(100*Paper/sum(Paper), 2))
plastic<-with(fulldata, 
              table(are_there_any_risks_involved_dealing_._working_with_this_material_plastic))
print(round(100*plastic/sum(plastic), 2))
Insurance<-with(fulldata, table(do_you_have_an_insurance_cover))
print(round(100*Insurance/sum(Insurance), 2))

## Doctor visits-mean
numSummary(fulldata[, "how_many_times_have_you_been_to_the_doctor_in_the_last_3_months"], 
           statistics=c("mean"))

## which material is percieved as most risky
temp <- fulldata[,c(38,47,56,65)]
temp
names(temp) <- c("glass", "paper", "metal", "plastic")
temp <- temp %>% gather()
temp
temp1<-filter(temp, value=="Yes")
p <- ggplot(temp1, aes(value))+stat_count()
p
p + facet_grid(.~key)
