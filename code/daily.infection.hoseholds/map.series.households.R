# Map series of households daily infections

# Import ABM outputs ----

households.infec<- read_csv(
  'results/infectionsathome.csv',
  col_names = c(
    "date.time",
    "id",
    "daily.infect",
    "seed",
    "run",
    "exp"))

households.infec$date.time <- parse_date_time(
  households.infec$date.time, orders = c("%Y-%m-%d"))

# Verify mathing households IDs
households.infec
households_tbl

# Join shapes and time-series resolutions ----
hholds24h_tbl <- inner_join(households_tbl,households.infec, by = "id")

hholds24h_tbl

# Faceted maps plotting DW spatial variability----

#Targeting mean infected stool events between days March 21 and Ap 4#-------------------------------------------------------------
hholds24h_tbl <- hholds24h_tbl|>
  filter(
    date.time <= as.POSIXct("2022-04-05", tz='UTC') &
      date.time >= as.POSIXct("2022-03-21", tz='UTC'))

hholds24h_tbl$date.time |> unique()

#Aggregate by household at 24 hours. Mean events

hholds.infected <- aggregate(
  daily.infect ~format(as.POSIXct(hholds24h_tbl$date.time), "%Y-%m-%d")+
    hholds24h_tbl$id,
  data= hholds24h_tbl,
  FUN = sum)%>% as_tibble()

colnames(hholds.infected) <-c(
  "date.time","id","daily.infect")

hholds.infected

hholds.infected$date.time <- parse_date_time(
  hholds.infected$date.time, orders = c("%Y-%m-%d"))

hholds.infected
hholds.infected$daily.infect|>unique()

# Join shapes and time-series resolutions
hholds.infected.daily <- inner_join(
  households_tbl,hholds.infected, by = "id")

hholds.infected.daily
hholds.infected.daily$date.time |> unique()

hholds.infected.daily$daily.infect |> unique()

#Clean memory

remove(hholds.infected,hholds24h_tbl,households.infec )

