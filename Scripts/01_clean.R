## MSBA 500 Group Project | Airbnb Los Angeles Listings
##  Data Cleaning (simplified version, base R only)
## Run from the project root:  Rscript Scripts/01_clean.R
##
## Input : Data/raw/los_angeles_airbnb_listings.csv.gz (Inside Airbnb, scrape 2026-09-14, never edited)
## Output: Data/clean/airbnb_analytic.csv        final analytic file
##         Data/clean/data_dictionary_draft.csv   starter dictionary, fill in descriptions
##         Outputs/cleaning_log.csv               rows and columns after each step
##         Outputs/raw_profile.csv and eda_*.csv  missing values and EDA tables
## Target (Y) is price (nightly, USD). We model log(price) later.

##Part 0 Settings (change here and re-run, every setting is saved in the cleaning log)
PRICE_MIN<-20            #review against the raw price quantiles printed below
PRICE_MAX<-1000          #about 5% of priced listings are above $1,000
MISSING_PCT<-10          #drop variables missing more than this percent
TOP_NEIGHBORHOODS<-15   #keep the 15 biggest neighborhoods, group the rest as Other
TOP_PROPERTY_TYPES<-8    #keep the 8 most common property types, group the rest as Other
lax_lat<-33.9416         #LAX airport, used for the distance variable
lax_lon<--118.4085
set.seed(2026)           #seed for the random fill of missing values

raw_file<-"Data/raw/los_angeles_airbnb_listings.csv.gz"
clean_dir<-"Data/clean"
out_dir<-"Outputs"
dir.create(clean_dir,recursive=TRUE,showWarnings=FALSE)
dir.create(out_dir,recursive=TRUE,showWarnings=FALSE)

##Part 1: Import and inspect the raw data
if(!file.exists(raw_file)) stop("Missing ",raw_file," (Inside Airbnb Los Angeles listings.csv.gz)")
raw<-read.csv(raw_file,na.strings=c("","NA")) #blank cells are read as NA
dim(raw)
#Built on one fixed snapshot
if(nrow(raw)!=43781 | ncol(raw)!=90) stop("Raw file is not the 2026-09-14 snapshot (43,781 x 90)")

#Missing values by variable
raw_profile<-data.frame(variable=names(raw),
                        type=as.vector(sapply(raw,function(x) class(x)[1])),
                        n_missing=as.vector(colSums(is.na(raw))),
                        pct_missing=round(as.vector(colMeans(is.na(raw)))*100,2),
                        n_distinct=as.vector(sapply(raw,function(x) length(unique(x)))))
write.csv(raw_profile,file.path(out_dir,"raw_profile.csv"),row.names=FALSE)
raw_profile[raw_profile$pct_missing>MISSING_PCT,c("variable","pct_missing")]

#Price is text like "$1,234.00", turn it into a number to look at it
price_raw<-as.numeric(gsub("[$,]","",raw$price))
round(quantile(price_raw,c(0,0.01,0.05,0.5,0.95,0.99,1),na.rm=TRUE))
sum(price_raw>=1000,na.rm=TRUE) #listings >= $1,000
table(raw$source,is.na(price_raw),useNA="ifany",dnn=c("source","price missing")) #missing prices are old scrapes
table(raw$room_type)

#Raw price by room type
raw_by_room<-data.frame(room_type=names(table(raw$room_type)),
                        n=as.vector(table(raw$room_type)),
                        n_price=as.vector(tapply(!is.na(price_raw),raw$room_type,sum)),
                        median=as.vector(tapply(price_raw,raw$room_type,median,na.rm=TRUE)),
                        mean=round(as.vector(tapply(price_raw,raw$room_type,mean,na.rm=TRUE))),
                        max=as.vector(tapply(price_raw,raw$room_type,max,na.rm=TRUE)))
raw_by_room
write.csv(raw_by_room,file.path(out_dir,"eda_raw_price_by_room_type.csv"),row.names=FALSE)

##Part 2 Cleaning (one row is added to the cleaning log after each step)
add_log<-function(log,data,step,note){
  rbind(log,data.frame(step=step,rows=nrow(data),cols=ncol(data),note=note))
}
cleaning_log<-data.frame(step=character(),rows=integer(),cols=integer(),note=character())
df<-raw
cleaning_log<-add_log(cleaning_log,df,"0 raw file","as downloaded")

#Record every setting in the log
settings<-data.frame(name=c("PRICE_MIN","PRICE_MAX","MISSING_PCT","TOP_NEIGHBORHOODS","TOP_PROPERTY_TYPES",
                            "LAX","SEED","RAW_FILE"),
                     value=c(PRICE_MIN,PRICE_MAX,MISSING_PCT,TOP_NEIGHBORHOODS,TOP_PROPERTY_TYPES,
                             paste0("lat ",lax_lat,", lon ",lax_lon),2026,basename(raw_file)))
cleaning_log<-add_log(cleaning_log,df,paste("setting",settings$name),settings$value)

#Step 1 keep only the columns we need. Leave out urls, text descriptions, host fields that are empty
#availability/occupancy/revenue/review fields
keep<-c("id","price","room_type","property_type","accommodates","bedrooms","beds","bathrooms_text",
        "amenities","license","minimum_nights","neighbourhood_cleansed","latitude","longitude",
        "host_is_superhost","host_identity_verified","hosts_time_as_host_years",
        "calculated_host_listings_count")
df<-raw[,keep]
df$host_is_superhost<-df$host_is_superhost=="t"         
df$host_identity_verified<-df$host_identity_verified=="t"
cleaning_log<-add_log(cleaning_log,df,"1 select columns",
                      paste(length(keep),"of",ncol(raw),"kept; host t/f flags converted to TRUE/FALSE"))

#Step 2 one row per listing id
df<-df[!duplicated(df$id),]
cleaning_log<-add_log(cleaning_log,df,"2 one row per listing id","")

#Step 3 price is the target, so listings with no price are removed
before<-nrow(df)
df$price<-as.numeric(gsub("[$,]","",df$price))
df<-df[!is.na(df$price),]
cleaning_log<-add_log(cleaning_log,df,"3 missing price",paste("removed",before-nrow(df),"listings with no price"))

#Step 4 Remove extreme outliers 
before<-nrow(df)
df<-df[df$price>=PRICE_MIN & df$price<=PRICE_MAX,]
cleaning_log<-add_log(cleaning_log,df,"4 price range",
                      paste0("kept $",PRICE_MIN,"-$",PRICE_MAX,"; removed ",before-nrow(df)))

#Step 5 drop variables with too much missing data (price and license are handled separately)
too_missing<-raw_profile$variable[raw_profile$pct_missing>MISSING_PCT]
too_missing<-too_missing[too_missing %in% names(df) & !(too_missing %in% c("price","license"))]
df<-df[,!(names(df) %in% too_missing)]
cleaning_log<-add_log(cleaning_log,df,"5 drop variables over missing threshold",
                      paste0(">",MISSING_PCT,"% missing: ",
                             ifelse(length(too_missing)==0,"none",paste(too_missing,collapse=", "))))

#Step 6 bathrooms come as text ("1.5 baths", "1 shared bath", "Half-bath")
bath_text<-tolower(df$bathrooms_text)
df$shared_bath<-grepl("shared",bath_text)
df$bathrooms<-as.numeric(gsub("[^0-9.]","",bath_text)) #keep only the number
df$bathrooms[grepl("half",bath_text)]<-0.5
cleaning_log<-add_log(cleaning_log,df,"6 parse bathrooms","bathrooms_text -> bathrooms (number), shared_bath (flag)")

#Step 7 derived variables (the data dictionary lists the antecedent variables)
#amenities is a list like ["Wifi", "Kitchen"], count the items
df$n_amenities<-as.integer(ifelse(df$amenities=="[]",0,
                                  lengths(regmatches(df$amenities,gregexpr('",\\s*"',df$amenities)))+1))
df$has_license<-!is.na(df$license)
#distance in miles from the listing to LAX (haversine formula)
haversine_mi<-function(lat1,lon1,lat2,lon2){
  rad<-pi/180
  a<-sin((lat2-lat1)*rad/2)^2+cos(lat1*rad)*cos(lat2*rad)*sin((lon2-lon1)*rad/2)^2
  2*3958.8*asin(sqrt(a))
}
df$dist_lax_mi<-haversine_mi(df$latitude,df$longitude,lax_lat,lax_lon)
df<-df[,!(names(df) %in% c("amenities","license","bathrooms_text"))]
cleaning_log<-add_log(cleaning_log,df,"7 derived variables","n_amenities, has_license, dist_lax_mi (miles from LAX)")

#Step 8 remaining missing values: fill the few gaps in size variables with a random draw from the
#observed values, then delete any other rows with missing values
fill_cols<-c("bedrooms","beds","bathrooms")
n_filled<-sapply(df[,fill_cols],function(x) sum(is.na(x)))
for(col in fill_cols){
  gaps<-which(is.na(df[[col]]))
  observed<-df[[col]][!is.na(df[[col]])]
  if(length(gaps)>0) df[[col]][gaps]<-sample(observed,length(gaps),replace=TRUE)
}
before<-nrow(df)
df<-na.omit(df)
cleaning_log<-add_log(cleaning_log,df,"8 missing values",
                      paste0("hot-deck ",paste(fill_cols,n_filled,sep="=",collapse=", "),
                             "; listwise deleted ",before-nrow(df)," rows"))

#Step 9 group rare categories (top N + Other) and set the categorical variables as factors
top_neigh<-names(sort(table(df$neighbourhood_cleansed),decreasing=TRUE))[1:TOP_NEIGHBORHOODS]
top_prop<-names(sort(table(df$property_type),decreasing=TRUE))[1:TOP_PROPERTY_TYPES]
df$neighborhood_grp<-as.factor(ifelse(df$neighbourhood_cleansed %in% top_neigh,df$neighbourhood_cleansed,"Other"))
df$property_type_grp<-as.factor(ifelse(df$property_type %in% top_prop,df$property_type,"Other"))
df$room_type<-as.factor(df$room_type)
df<-df[,!(names(df) %in% c("neighbourhood_cleansed","property_type"))]
cleaning_log<-add_log(cleaning_log,df,"9 transformations",
                      paste0("neighborhood_grp (top ",TOP_NEIGHBORHOODS," + Other), property_type_grp (top ",
                             TOP_PROPERTY_TYPES," + Other), room_type as factor"))

##Part 3 EDA on the cleaned data (tables saved to Outputs, charts go in 02_explore.R)
head(df)
str(df)
summary(df)

num_vars<-c("price","accommodates","bedrooms","beds","minimum_nights","hosts_time_as_host_years",
            "calculated_host_listings_count","bathrooms","n_amenities","dist_lax_mi")
eda_numeric<-data.frame(variable=num_vars,
                        mean=as.vector(sapply(df[,num_vars],mean)),
                        sd=as.vector(sapply(df[,num_vars],sd)),
                        min=as.vector(sapply(df[,num_vars],min)),
                        median=as.vector(sapply(df[,num_vars],median)),
                        max=as.vector(sapply(df[,num_vars],max)))
eda_numeric
write.csv(eda_numeric,file.path(out_dir,"eda_numeric_summary.csv"),row.names=FALSE)

#correlation of each numeric variable with log(price)
eda_cor<-data.frame(variable=num_vars[-1],
                    cor_with_log_price=as.vector(sapply(df[,num_vars[-1]],function(x) cor(x,log(df$price)))))
eda_cor<-eda_cor[order(-abs(eda_cor$cor_with_log_price)),]
eda_cor
write.csv(eda_cor,file.path(out_dir,"eda_correlation_with_log_price.csv"),row.names=FALSE)

#price by room type and by neighborhood
eda_room<-data.frame(room_type=levels(df$room_type),
                     n=as.vector(table(df$room_type)),
                     median_price=as.vector(tapply(df$price,df$room_type,median)),
                     mean_price=round(as.vector(tapply(df$price,df$room_type,mean))))
eda_room
write.csv(eda_room,file.path(out_dir,"eda_price_by_room_type.csv"),row.names=FALSE)

eda_area<-data.frame(neighborhood_grp=levels(df$neighborhood_grp),
                     n=as.vector(table(df$neighborhood_grp)),
                     median_price=as.vector(tapply(df$price,df$neighborhood_grp,median)))
eda_area<-eda_area[order(-eda_area$median_price),]
eda_area
write.csv(eda_area,file.path(out_dir,"eda_price_by_neighborhood.csv"),row.names=FALSE)

#skewness of price vs log(price), the reason we model log(price)
skew<-function(x) mean((x-mean(x))^3)/sd(x)^3
round(skew(df$price),2)
round(skew(log(df$price)),2)

##Part 4 Save the cleaned data, the cleaning log and the data dictionary
cleaning_log$rows_removed<-c(NA,-diff(cleaning_log$rows))
write.csv(cleaning_log,file.path(out_dir,"cleaning_log.csv"),row.names=FALSE)
write.csv(df,file.path(clean_dir,"airbnb_analytic.csv"),row.names=FALSE)

#starter data dictionary, fill in the description column by hand
antecedents<-c(bathrooms="bathrooms_text",shared_bath="bathrooms_text",n_amenities="amenities",
               has_license="license",dist_lax_mi="latitude, longitude",
               neighborhood_grp="neighbourhood_cleansed",property_type_grp="property_type")
dict<-data.frame(variable=names(df),
                 role=ifelse(names(df)=="price","Y (target)",
                      ifelse(names(df)=="id","ID (not a predictor)",
                      ifelse(names(df) %in% c("latitude","longitude"),"maps only (use dist_lax_mi)","X (predictor)"))),
                 type=as.vector(sapply(df,function(x) class(x)[1])),
                 n_distinct=as.vector(sapply(df,function(x) length(unique(x)))),
                 values=as.vector(sapply(df,function(x){
                   if(is.numeric(x)) paste0(round(min(x),2)," to ",round(max(x),2))
                   else if(length(unique(x))<=8) paste(sort(unique(as.character(x))),collapse=" | ")
                   else paste(length(unique(x)),"levels")})),
                 antecedent=as.vector(ifelse(is.na(antecedents[names(df)]),"",antecedents[names(df)])),
                 description="")
write.csv(dict,file.path(clean_dir,"data_dictionary_draft.csv"),row.names=FALSE)

cleaning_log
dim(df)
