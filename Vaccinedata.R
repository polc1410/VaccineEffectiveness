#
require(tidyverse)
require(flextable)
require(ggthemes)
require(ggsci)
# create a function to calculate halflife from two timepoints

halfLife <- function( d1, d2, t1, t2) {
    # d1 = data point at t1
    # d2 = data point at t1
    # t1 = first time point (numeric or date)
    # t2 = second time point (numeric or date)
    tdif = as.numeric(t2 - t1)
    decay =  (log(d2/d1)) / (tdif)
    halfLife = log(2)/-decay  #log = natural log
    return(halfLife)
}


# Build the daily data for: 0-69yrs, 70-79yrs and 80yrs+



caseData %>%
    filter(areaName == "England")  %>%
    filter(date > as.Date("2020-10-25")) %>%
    group_by(date) %>%
    summarise(`all` = sum(`newCasesBySpecimenDate-0_59`+`newCasesBySpecimenDate-60+`),
              `0_69`=sum(`newCasesBySpecimenDate-0_59`+`newCasesBySpecimenDate-60_64` +`newCasesBySpecimenDate-65_69`),                  `70_79`=sum(`newCasesBySpecimenDate-70_74` +`newCasesBySpecimenDate-75_79`),
              `80+` = sum(`newCasesBySpecimenDate-80_84`+`newCasesBySpecimenDate-85_89`+`newCasesBySpecimenDate-90+`)) %>%
    mutate(across(!contains("date"), list(roll = ~zoo::rollmean(., k=7, fill=NA, align = "right")))) -> rollData

rollData %>%
    pivot_longer(cols=contains("roll")&!contains("all"), names_to="Age", values_to = "Rolling_average") -> graphData


p <- ggplot()+
    scale_color_jco() +
    geom_line(data=graphData, aes(x=date, y=Rolling_average, group=Age, colour=Age ), size=1)+
    labs(y="Number of new daily cases (Log Scale)") +
    scale_y_log10(limits=c(100,60000))+
    theme_classic() +
    labs(title ="Rolling average new cases over previous 7 days", 
         subtitle="in England, with projected time (days) for new infections to half", 
         caption = "Data from data.gov.uk") +
    theme(legend.position="bottom")+
    theme(axis.title.x = element_blank())+
    xlim(c(as.Date("2020-11-01"), as.Date("2021-03-07")))
p

# calculate half lifes
rollData %>%
    mutate (day = weekdays(date)) %>%
    filter(day == "Friday") %>%
    select(contains("date"), contains("roll")) %>%
    mutate (across(!contains("date"), list( halflife = ~halfLife(., lead(.), date, lead(`date`))))) ->halfLives


halfLives %>%
    mutate(next_date = lead(date),
           across(ends_with("roll"), list(next_roll = ~lead(.))))%>%
    pivot_longer(cols = ends_with("roll")&!contains("all")&!contains("next"), 
                 names_to="Age", values_to = "Rolling_average" ) %>%
    arrange(Age, date) %>%
    mutate (next_Rolling_average = lead(Rolling_average)) ->halfLivesLines

halfLives %>%
    pivot_longer(cols = ends_with("halflife")&!contains("all"), 
                 names_to="Age", values_to = "Rolling_average" ) %>%
    mutate(Age = str_replace(Age, "_halflife", "")) %>%
    left_join(halfLivesLines, by = c("Age"="Age", "date"="date"))  %>% 
    select(date, Age, contains("Rolling")) %>%
    filter(Rolling_average.x >0 & Rolling_average.x < 300)->halfLivesLabels     

# Add some points to the plot

p <- p +
    geom_point(data=halfLivesLines, aes(x=date, y=Rolling_average, colour=Age))+
   geom_segment(data=halfLivesLines, 
                aes(x=date, 
                    xend=next_date, 
                    y=Rolling_average, 
                    yend = next_Rolling_average, 
                    colour=Age), linetype = "dashed") +
    geom_text(data=halfLivesLabels[halfLivesLabels$Age!="80+_roll",], aes(x=date+3, y=next_Rolling_average, label=paste(round(Rolling_average.x,0), "days"), colour=Age), size=2.8, angle=45, check_overlap = T, hjust=1, show.legend = FALSE ) +
    geom_text(data=halfLivesLabels[halfLivesLabels$Age=="80+_roll",], aes(x=date+3, y=Rolling_average.y, label=paste(round(Rolling_average.x,0), "days"), colour=Age), size=2.8, angle=45, check_overlap = T, hjust=0, show.legend = FALSE)

p

halfLives %>%
    select(date, contains("half")) -> tableContent

colnames(tableContent) <- str_replace(
    colnames(tableContent), "_roll_halflife", "")
    
tableContent %>%
    mutate( date = format(date, "%d %b %Y"))%>%
flextable()%>%
    #autofit()%>%
    add_header_row(values = c("","Age Groups"), colwidths = c(1,4)) %>%
    theme_booktabs()%>%
    fit_to_width(3.5)%>%
    align(align = "center", part = "header")%>%
    set_caption("Time taken (in days) for current weekly average case rate to fall by half, based on the decline in cases over the next 7 day period.")

ggsave("Rplot.png", plot=p)









