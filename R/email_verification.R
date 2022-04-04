#! /usr/bin/env Rscript

#Sys.getenv()

library(lubridate)

#-- Create Markdown issue
md_file <- readLines("R/daily_results.Rmd")
writeLines(md_file, "issue.Rmd")
rmarkdown::render(input = "issue.Rmd")

msg_text <- readLines("issue.md") %>% paste(collapse = "\n")

# Add header text
issue <- paste0('---\ntitle: Daily AQI report \n',
                'labels: daily_email',
                '\n---\n\n', 
                msg_text)

# Save issue to markdown file
cat(issue, file = "issue.md") 


# Update Github model performance table for AQI Watch website

# Filter verification to last 7 days
verify <- all_verify %>% filter(forecast_date > (Sys.Date() - 8), forecast_date < Sys.Date())

# Filter to one forecast per day for each site
verify <- verify %>%
          group_by(forecast_date, site_catid) %>%
          mutate(forecast_day = ifelse(forecast_day == 0, 99, forecast_day)) %>%
          arrange(forecast_day) %>%
          slice(1) %>%
          mutate(forecast_day = ifelse(forecast_day == 99, 0, forecast_day))

# Round - set signif digits
verify <- verify %>% mutate_at(vars(matches("mod_")), round, 3)

# Save
write_csv(verify, "data/model_performance.csv")

##
