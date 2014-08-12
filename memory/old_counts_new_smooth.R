## code to aggregate Queen Sirikit data into biweeks
## Nicholas Reich
## August 2014

library(dplyr)
library(reshape2)
library(parallel)

## Get data.
counts <- tbl_df(read.csv("/Volumes/projects/Projects/Dengue/QSData/QSNICH_data_1973_2014.csv"))

## aggregate, dropping missing serotype
grouped_counts <- counts %>% 
        group_by(S1YR, S1MO, ISOTYP) %>% 
        summarize( cases = n() ) %>% 
        filter( ISOTYP>0 ) %>% 
        mutate( date = S1YR + (S1MO-1) / 12 ,
                year = S1YR,
                month = S1MO,
                serotype = ISOTYP) %>%
        ungroup() %>%
        select(date, year, month, serotype, cases)

# qplot(date, ymax=cases, ymin=0, geom="linerange", data=grouped_counts, facets=serotype~.)

## make complete data structure
empty_data <- tbl_df(expand.grid(month=1:12, year=1973:2012, serotype=1:4))
full_data <- left_join(empty_data, grouped_counts) %>%
        mutate(date = year + (month-1)/12) %>%
        filter(date>=1973.5) 
full_data[is.na(full_data$cases), "cases"] <- 0
        
# qplot(date, ymax=cases, ymin=0, geom="linerange", data=full_data, facets=serotype~.)

## make cumulative counts
full_data <- full_data %>% 
        arrange(date) %>%
        group_by(serotype) %>%
        mutate(cum_cases = cumsum(cases)) %>%
        ungroup()
 

## make spline functions for interpolation
split_counts <- vector(mode="list", length=4)
for(i in 1:4){
        split_counts[[i]] <- filter(full_data, serotype==i)
}

spline_funs <- mclapply(
        X=split_counts,
        FUN=function(counts) {
                sf <- splinefun(x=counts$date, y=counts$cum_cases, 
                                method="hyman")
                return(sf)
        }
)

## run interpolation
years <- 1974:2012
biweeks <- 1:26

base <- tbl_df(expand.grid(date_sick_biweek = biweeks, 
                           year = years, 
                           serotype=1:4))
base <- base %>%
        mutate(date = year + (date_sick_biweek-1)/26,
               smooth_cum_counts=NA)

for(i in 1:4){
        f <- spline_funs[[i]]
        idx <- which(base$serotype==i)
        base[idx, "smooth_cum_counts"] <- f(x=base[idx, "date"])
}


biweek_count_data <- base %>% 
        arrange(date) %>%
        group_by(serotype) %>%
        mutate(cases = round(c(0, diff(smooth_cum_counts)))) %>%
        ungroup()

#qplot(date, ymax=cases, ymin=0, geom="linerange", data=biweek_count_data, facets=serotype~.)

saveRDS(biweek_count_data, file="new_QS_case_data_1973_2012.rds")


## make wide data
ready_data <- tbl_df(dcast(biweek_count_data, 
                           date + year + date_sick_biweek ~ serotype, 
                           value.var="cases"))



runCrossProtectMemoryAnalysis(data=ready_data[ready_data$date>1990, 4:7], 
                              k=200, 
                              max_lambda=26*6, 
                              plot=TRUE, verbose=FALSE)
