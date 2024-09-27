
#Question A

file_root = "https://www.ndbc.noaa.gov/view_text_file.php?filename=44013h"
years = 1985:2023
tail = ".txt.gz&dir=data/historical/stdmet/"
bd_list = list()

#loop through the years and skip lines based on data

for(year in years){
  path<-paste0(file_root,year,tail)
  header=scan(path,what= 'character',nlines=1)
  skip_lines = ifelse(year >= 2007, 2, 1)
  buoy<-fread(path,header=FALSE,skip=skip_lines)
  num_col = ncol(buoy)
  if (length(header) > num_col){
    header = header[1:num_col]
  } else if (length(header) < num_col) {
    header = c(header, paste0("V",(length(header) + 1):num_col))
  }
  
  #create header
  colnames(buoy) = header
  
  if ('YY' %in% colnames(buoy)) {
    buoy$YY = ifelse(buoy$YY < 100, ifelse(buoy$YY > 20, 1900 + buoy$YY, 2000 + buoy$YY), buoy$YY)
  }
  
  if ('YY' %in% colnames(buoy) & "MM" %in% colnames(buoy) & 'DD' %in% colnames(buoy) & 'hh' %in% colnames(buoy) & 'mm' %in% colnames(buoy)) {
    buoy$Date = ymd_hms(paste(buoy$YY, buoy$MM, buoy$DD, buoy$hh, buoy$mm))
  }
  
  bd_list[[as.character(year)]] = buoy
}

bd_list = rbindlist(bd_list, fill = TRUE)

#Create Date Column and get rid of excess columns
bd_list = bd_list %>%
  mutate(Year = coalesce(as.numeric(YY), as.numeric(YYYY), as.numeric(`#YY`))) %>%
  select(-YY, -YYYY, -`#YY`) %>%
  select(Year, everything())

bd_list = bd_list %>%
  mutate(Wind_Direction = coalesce(WD, WDIR)) %>%
  select(-WD, -WDIR)

bd_list = bd_list %>%
  mutate(Pressure = coalesce(BAR, PRES)) %>%
  select(-BAR, -PRES)

bd_list$Date <- ymd_h(paste(bd_list$Year, bd_list$MM, bd_list$DD, bd_list$hh, sep = "-"))
bd_list <- bd_list[, -c(1:4)]
bd_list <- bd_list[, c(15, 1:14)]
