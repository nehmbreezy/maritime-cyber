
library(rvest)
library(stringr)
SLEEPTIME = 12

base_url = "https://www.ft.com/indepth/asia-maritime-tensions?page="
pages = as.character(1:21)

url_list = paste(base_url, pages, sep = "")


# Let's first create a scraper pulls the description and title and date and link for every article

scrape_thread_for_articles = function(url)
{
  # Using rvest to retrieve the entire page
  thread = read_html(url)
  closeAllConnections()
  Sys.sleep(SLEEPTIME)
  print("Thread rvested. Resting.")
  
  # scrape all rows containing article links
  o_grids = thread %>% html_nodes(".o-grid-row")
  
  # extract relevant headings
  dates = o_grids %>% html_nodes(".o-date") %>% html_text() %>% as.data.frame()
  
  timestamps = o_grids %>% html_nodes(".o-date") %>% html_attr("datetime") %>% as.data.frame()
  
  headings = o_grids %>% html_nodes(".o-teaser__heading") %>% html_text() %>% as.data.frame()
  
  category = o_grids %>% html_nodes(".o-teaser__meta") %>% html_text() %>% as.data.frame()
  
  descriptions = o_grids %>% html_nodes(".o-teaser__standfirst") %>% html_text() %>% as.data.frame()
  
  # some articles have no descriptions
  if(nrow(descriptions)<nrow(dates))
  {
    descriptions = o_grids %>% html_nodes(".o-teaser__content") %>% html_text() %>% as.data.frame()
  }
  
  link_tails = o_grids %>% html_nodes(".o-teaser__heading") %>% html_nodes("a") %>% html_attr('href')
  links = paste("https://www.ft.com", link_tails, sep = "") %>% as.data.frame()
  
  all_articles = data.frame()
  
  all_articles = cbind(dates, timestamps, headings, category, descriptions, links)
  
  return(all_articles)
}


# Great, now to actually run it

articles_df = data.frame()

for(i in 1:length(url_list))
{
  print(paste("Now scraping thread", i))
  
  temp_df = data.frame()
  
  temp_df = scrape_thread_for_articles(url_list[i])
  
  articles_df = rbind(articles_df, temp_df)
}

names(articles_df) = c("Date", "Timestamp", "Headline", "Category", "Description", "Link")


# Save and complete

maritime_disputes_FT = articles_df
save(maritime_disputes_FT, file = "maritime_disputes_FT.RData")