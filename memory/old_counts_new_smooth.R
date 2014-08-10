## code to aggregate Queen Sirikit data into biweeks
## Nicholas Reich
## August 2014

library(dplyr)

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


split_counts <- split(x=old_counts, f=old_counts[['province']])

split_counts <- mclapply(
	X=split_counts,
	FUN=function(counts) {
		counts_anchor <- data.frame(
			year = min_year, month = 0, 
			province_name = counts[['province_name']][1],
			province = counts[['province']][1], 
			population = counts[['population']][1],
			area_km_2 = counts[['area_km_2']][1],
			count = 0
		)
		counts <- rbind(counts_anchor,counts)
		counts <- counts[order(counts[['year']], counts[['month']]),]
		months_are_sequential <- all(diff(counts[['month']]) %in% c(1,-11,NA))
		years_are_sequential <- all(diff(counts[['year']]) %in% c(1,0,NA))
		if (!months_are_sequential) 
			return('Check that all months are available.')
		if (!years_are_sequential) 
			return('Check that all years are available.')
		use_these <- counts[['count']]
		use_these[is.na(use_these)] <- 0
		counts[['cumulative_count']] <- cumsum(use_these)
		return(counts)
	}
)

spline_funs <- mclapply(
	X=split_counts,
	FUN=function(counts) {
		times <- counts[['year']] + counts[['month']]/12
		sf <- splinefun(x=times, y=counts[['cumulative_count']], method="hyman")
		return(sf)
	}
)

min_year <- min(old_counts[['year']])
max_year <- max(old_counts[['year']])
years <- min_year:max_year
biweeks <- 1:26

base <- expand.grid(date_sick_biweek = biweeks, year = years)
base_anchor <- data.frame(
	date_sick_biweek = 0,
	year = min_year
)
base <- rbind(base_anchor, base)
base[['times']] <- base[['year']] + base[['date_sick_biweek']]/26


interpolated_times <- mclapply(
	X=names(spline_funs),
	FUN=function(nom, base) {
		f <- spline_funs[[nom]]
		base[['province']] <- as.numeric(nom)
		base[['smooth_cumulative_counts']] <- f(x=base[['times']])
		base[['smooth_counts']] <-
			c(NA,diff(base[['smooth_cumulative_counts']]))
		return(base[2:nrow(base),])
	},
	base = base
)

smooth_counts <- do.call(what=rbind, args=interpolated_times)


## Write standardized columns version:
count_table <- 'biweely_dengue_counts_1968_to_2005'
dbSendQuery(link$conn,paste0('DROP TABLE IF EXISTS public.', count_table, ';'))
#dbSendQuery(link$conn,paste0('DROP TABLE IF EXISTS standardized_data.', count_table, ';'))
dbWriteTable(conn=link$conn, name=count_table,
             value=smooth_counts, row.names=FALSE, overwrite=TRUE, append=FALSE)
#dbSendQuery(link$conn,paste0('ALTER TABLE ', count_table, ' SET SCHEMA standardized_data;'))



#for ( i in unique(smooth_counts[['province']]) ) {
#	print(i)
#	pl <- ggplot(
#		data=smooth_counts[smooth_counts[['province']] == i,], 
#		aes(x=times, y=smooth_counts, colour=factor(province))
#	) + geom_line() 
#	print(pl) 
#	readline()
#}

