#Sewer map series at maintenance holes

# Load results ----

# execute  code/sewer.map.series/sewer.daily.infected.dw.mapseries.R

mholes_tbl = read_sf("data/manholes.snt.height.shp")
blocks_tbl = read_sf("data/13m.loc.snt.2020.mz800.wgs84.shp")
pipes_tbl = read_sf("data/pipes.snt.shp")
households_tbl = read_sf("data/households.snt.wgs84.shp")

# Matching ID manholes ---- 

# col names (results and shape file ids)
mholes_tbl = rename(mholes_tbl, manhole.id = manhole_id)

#Filtering blocks of interest
blocks_tbl_f = blocks_tbl|>
  filter(wwtp.conex == 'y' & TIPOMZA =='Típica')


#PEAK COVID detection at April 1 ------------------------------

#Targeting mean infected stool events between 9 and 10#-------------------------------------------------------------
stool.infected <- manholes.snt.pol|>
  filter(  event.typ =="poo"&
             date.time <= as.POSIXct("2022-04-01 10:30:00", tz='UTC') &
             date.time >= as.POSIXct("2022-04-01 08:55:00", tz='UTC'))|>
  filter(virus == "Infected"|virus=="Recovered")|>
  mutate(inf.stool=1)

#Aggregate by manholes at 6 min. Mean events

stool.infected$date.time <- align.time(
  as.POSIXct(stool.infected$date.time), 6 * 60) # temporal res in min=6

stool.infected.manholes <- aggregate(
  inf.stool ~format(as.POSIXct(stool.infected$date.time), "%Y-%m-%d %H:%M")+
    stool.infected$manhole.id,
  data= stool.infected,
  FUN = sum)%>% as_tibble()

colnames(stool.infected.manholes) <-c(
  "date.time","manhole.id","stool.infected")

stool.infected.manholes$date.time <- parse_date_time(
  stool.infected.manholes$date.time, orders = c("%Y-%m-%d %H:%M"))

stool.infected.manholes
stool.infected.manholes$stool.infected|>unique()

# Join shapes and time-series resolutions
mholes06m_tbl.stool.recovering <- inner_join(
  mholes_tbl,stool.infected.manholes, by = "manhole.id")


#NEW

#Aggregate by manholes at 60 min. Mean events
stool.infected <- manholes.snt.pol|>
  filter(  event.typ =="poo"&#run ==2&
             date.time <= as.POSIXct("2022-04-01 15:00:00", tz='UTC') &
             date.time >= as.POSIXct("2022-04-01 07:00:00", tz='UTC'))|>
  filter(virus == "Infected"|virus=="Recovered")|>
  mutate(inf.stool=1)

stool.infected$date.time <- align.time(
  as.POSIXct(stool.infected$date.time), 60 * 60) # temporal res in min=6

stool.infected.manholes <- aggregate(
  inf.stool ~format(as.POSIXct(stool.infected$date.time), "%Y-%m-%d %H:%M")+
    stool.infected$manhole.id,
  data= stool.infected,
  FUN = sum)%>% as_tibble()

colnames(stool.infected.manholes) <-c(
  "date.time","manhole.id","stool.infected")

stool.infected.manholes$date.time <- parse_date_time(
  stool.infected.manholes$date.time, orders = c("%Y-%m-%d %H:%M"))

stool.infected.manholes
stool.infected.manholes$stool.infected|>unique()

# Join shapes and time-series resolutions
mholes60m_tbl.stool.recovering <- inner_join(
  mholes_tbl,stool.infected.manholes, by = "manhole.id")


#Full outbreak of daily infected wastewater  ------------------------------

#Aggregate by manholes at 24 horus (1440 min.) Mean events
stool.infected <- manholes.snt.pol|>
  filter(  event.typ =="poo"#run ==2&
           )|>
  filter(vir.prev.inpoo == "disease.in.poo")|>
  mutate(inf.stool=1)

stool.infected$date.time <- align.time(
  as.POSIXct(stool.infected$date.time), 1440 * 60) # temporal res in min=6

stool.infected.manholes <- aggregate(
  inf.stool ~format(as.POSIXct(stool.infected$date.time), "%Y-%m-%d %H:%M")+
    stool.infected$manhole.id,
  data= stool.infected,
  FUN = sum)%>% as_tibble()

colnames(stool.infected.manholes) <-c(
  "date.time","manhole.id","stool.infected")

stool.infected.manholes$date.time <- parse_date_time(
  stool.infected.manholes$date.time, orders = c("%Y-%m-%d %H:%M"))

stool.infected.manholes
stool.infected.manholes$stool.infected|>unique()

# Join shapes and time-series resolutions
mholes24h_tbl.stool.recovering <- inner_join(
  mholes_tbl,stool.infected.manholes, by = "manhole.id")

#clean environment

remove(stool.infected,
       stool.infected.manholes, 
       blocks.snt.pol,
       manholes.snt.pol,
       mholes_tbl,
       stool.infected,
       stool.infected.manholes)




