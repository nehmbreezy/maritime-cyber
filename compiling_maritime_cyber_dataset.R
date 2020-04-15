load('combined_dataset.RData')
load('maritime_disputes_FT.RData')

library(stringr)

# Combining both datasets into a single new dataset.
  
# Date
# Title / Headline
# Description
# Source_1 / Link

# Type: News, Cyber Incident

# News: Category
# News: Article Text [Omit this in the public dataset: it's copyrighted]

# Cyber Incident: Target
# Cyber Incident: Target_Category
# Cyber Incident: Objective
# Cyber Incident: Source 2, 3

maritime_disputes_FT$truncated_date = str_sub(as.character(maritime_disputes_FT$Timestamp), 0, 10)
maritime_disputes_FT$type = "NEWS"
maritime_disputes_FT$Article_Text = NA
maritime_disputes_FT$target = NA
maritime_disputes_FT$target_category = NA
maritime_disputes_FT$objective = NA
maritime_disputes_FT$source_2 = NA
maritime_disputes_FT$source_3 = NA

combined_dataset$type = "CYBER INCIDENT"
combined_dataset$category = NA
combined_dataset$article_text = NA

maritime_cyber = data.frame("Date" = append(as.character(maritime_disputes_FT$truncated_date),
                                            as.character(combined_dataset$date)),
                            "Title" = append(as.character(maritime_disputes_FT$Headline), 
                                             as.character(combined_dataset$title)),
                            "Description" = append(as.character(maritime_disputes_FT$Description),
                                                   as.character(combined_dataset$description)),
                            "Type" = append(as.character(maritime_disputes_FT$type),
                                            as.character(combined_dataset$type)),
                            "Source" = append(as.character(maritime_disputes_FT$Link),
                                              as.character(combined_dataset$source_1)),
                            "News_Category" = append(as.character(maritime_disputes_FT$Category),
                                                     as.character(combined_dataset$category)),
                            "News_Article" = append(as.character(maritime_disputes_FT$Article_Text),
                                                    as.character(combined_dataset$article_text)),
                            "Cyber_Target" = append(as.character(maritime_disputes_FT$target),
                                                    as.character(combined_dataset$target)),
                            "Cyber_Target_Category" = append(as.character(maritime_disputes_FT$target_category),
                                                             as.character(combined_dataset$target_category)),
                            "Cyber_Objective" = append(as.character(maritime_disputes_FT$objective),
                                                       as.character(combined_dataset$objective)),
                            "Cyber_Source_2" = append(as.character(maritime_disputes_FT$source_2),
                                                      as.character(combined_dataset$source_2)),
                            "Cyber_Source_3" = append(as.character(maritime_disputes_FT$source_3),
                                                      as.character(combined_dataset$source_3)))


save(maritime_cyber, file = "maritime_cyber.RData")