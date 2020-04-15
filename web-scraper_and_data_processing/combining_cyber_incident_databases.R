library(stringr)

# 1.1 Processing the first dataset (cyber operations tracker, council of foreign relations) so that it only includes cyber_incidents relevant to China and Southeast Asia

cyber_incidents = read.csv('cyber-operations-incidents.csv')

which_china = which(cyber_incidents$Sponsor == "China")

china_incidents = cyber_incidents[which_china,]

se_e_asia = c("Taiwan", "Japan", "Singapore", "Southeast", "Philippines", "Indonesia", "Vietnam", "Malaysia","Thailand", "Cambodia")

china_incidents$relevant = FALSE

for(i in 1:length(se_e_asia))
{
  index_of_asia = str_detect(china_incidents$Victims, se_e_asia[i])
  china_incidents$relevant[index_of_asia] = TRUE
  
  index_of_asia_2 = str_detect(china_incidents$Description, se_e_asia[i])
  china_incidents$relevant[index_of_asia_2] = TRUE
}

dataset_1 = china_incidents[which(china_incidents$relevant == TRUE),]



# 2.1 Processing the second dataset (Valeriano and Maness), which takes a bit longer 

cyber_incidents_alt = read.csv("cyber-operations-incidents-2.csv")

which_non_china = which(cyber_incidents_alt$StateA != "China")
which_india = which(cyber_incidents_alt$StateB == "India")

non_china = append(which_non_china, which_india)
china_incidents_alt = cyber_incidents_alt[-non_china,]


## 2.2 Also have to reformat the dates into standard YYYY-DD-MM 

china_incidents_alt$date = NA

for(i in 1:nrow(china_incidents_alt))
{
  date = china_incidents_alt$interactionstartdate[i]
  print(i)
  print(date)
  index_dash = as.data.frame(str_locate_all(date, "/"))
  
  year = str_sub(date, index_dash$start[2] + 1, str_length(date))
  
  day = str_sub(date, index_dash$start[1] + 1, index_dash$start[2]-1)
  if(str_length(day) == 1){
    day = paste("0", day, sep = "")
  }
  
  month = str_sub(date, 1, index_dash$start[1] - 1)
  if(str_length(month) == 1){
    month = paste("0", month, sep = "")
  }
  
  new_date = paste(year, month, day, sep = "-")
  print(new_date)
  china_incidents_alt$date[i] = new_date
}

save(china_incidents_alt, file = "china_incidents_alt.RData")


# 2.3 Also going recode some rows in this database (according to the codebook) to provide more relevant information

recode = function(col_name, code, replacement)
{
  cols = as.data.frame(colnames(china_incidents_alt))
  
  col_num = which(colnames$`colnames(china_incidents_alt)` == col_name)
  
  index = which(china_incidents_alt[,col_num] == code)
  
  china_incidents_alt[index, col_num] = replacement
  
  return(china_incidents_alt)
}

type_1 = "Private sector"
type_2 = "Government"
type_3 = "Military"

china_incidents_alt = recode("targettype", "1", type_1)
china_incidents_alt = recode("targettype", "2", type_2)
china_incidents_alt = recode("targettype", "3", type_3)

obj_1 = "Disruption"
obj_2 = "Espionage, short-term"
obj_3 = "Espionage, long-term"
obj_4 = "Degradation"

china_incidents_alt = recode("cyber_objective", "1", obj_1)
china_incidents_alt = recode("cyber_objective", "2", obj_2)
china_incidents_alt = recode("cyber_objective", "3", obj_3)


# 2.5 Remove an incident in which Vietnam was the aggressor (since this dataset focues on China) before I forget

china_incidents_alt = china_incidents_alt[which(china_incidents_alt$initiator== "710"),]


# 2.6 Also going to split up sources in this dataset into three separate columns as in Dataset 1
dataset_2 = china_incidents_alt

sources = dataset_2$Sources 
dataset_2$source_1 = NA
dataset_2$source_2 = NA
dataset_2$source_3 = NA

for(i in 1:nrow(dataset_2))
{
  source = as.character(sources[i])
  
  index_http = as.data.frame(str_locate_all(sources[i], "http"))
  
  source_list = list()
  
  for(j in 1:nrow(index_http))
  {
    if(j < nrow(index_http))
    {
      source_list[j] = str_sub(source, index_http$start[j], index_http$start[j+1] - 2)
    }
    
    if(j == nrow(index_http))
    {
      source_list[j] = str_sub(source, index_http$start[j], str_length(source))
    }
  }
  
  if(length(source_list) == 1)
  {
    dataset_2$source_1[i] = as.character(source_list[1])
  }
  
  if(length(source_list) == 3)
  {
    dataset_2$source_1[i] = source_list[1]
    dataset_2$source_2[i] = source_list[2]
    dataset_2$source_3[i] = source_list[3]
  }
  
  if(length(source_list) == 2)
  {
    dataset_2$source_1[i] = as.character(source_list[1])
    dataset_2$source_2[i] = as.character(source_list[2])
  }
  
  if(length(source_list) == 3)
  {
    dataset_2$source_1[i] = source_list[1]
    dataset_2$source_2[i] = source_list[2]
    dataset_2$source_3[i] = source_list[3]
  }
}

save(dataset_1, file = "dataset_1.Rdata")
save(dataset_2, file = "dataset_2.Rdata")



# 3.1 Ok, finally, combine the two datasests together - losing some information from Dataset 2, unfortunately. But so it goes

# Dataset 1 / Dataset 2 equivalent
# - Date / Date
# - Title + Affiliation (only dataset A) / Name
# - Description / Political.Objective
# - Victims / StateB
# - Category / Targettype
# - Type / Cyber_objective
# - Source 1 2 3 / Sourcess

dataset_1$combined_title = NA

for(i in 1:nrow(dataset_1))
{
  
  if(dataset_1$Affiliations[i] == "")
  {
    dataset_1$combined_title[i] = as.character(dataset_1$Title[i])
  }
  
  if(dataset_1$Affiliations[i] != "")
  {
    dataset_1$combined_title[i] = paste(as.character(dataset_1$Title[i]), as.character(dataset_1$Affiliations[i]), sep = " - ")
  }
}


dates = append(as.character(dataset_1$Date), dataset_2$date)
titles = append(as.character(dataset_1$combined_title), as.character(dataset_2$Name))
descriptions = append(as.character(dataset_1$Description), as.character(dataset_2$Political.Objective))
targets = append(as.character(dataset_1$Victims), as.character(dataset_2$StateB))
target_categories = append(as.character(dataset_1$Category), as.character(dataset_2$targettype))
objectives = append(as.character(dataset_1$Type), as.character(dataset_2$cyber_objective))
source_1 = append(as.character(dataset_1$Sources_1), as.character(dataset_2$source_1))
source_2 = append(as.character(dataset_1$Sources_1), as.character(dataset_2$source_2))
source_3 = append(as.character(dataset_1$Sources_1), as.character(dataset_2$source_3))

cyber_incident_datasets_combined = data.frame("date" = dates, 
                              "title" = titles, 
                              "description" = descriptions, 
                              "target" = targets, 
                              "target_category" = target_categories, 
                              "objective" = objectives, 
                              "source_1" = source_1, 
                              "source_2" = source_2, 
                              "source_3" = source_3)

save(cyber_incident_datasets_combined, file = "cyber_incident_datasets_combined.RData")