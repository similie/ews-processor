##############################################
# Developed and manteined by Decatab Pte Ltd #
# for addtional informaton please contact    #
# info@decatab.com                           #
##############################################

# start from the rocker image
FROM rocker/r-ver

# install the linux libraries needed 
RUN apt-get update -qq && apt-get install -y \
  libssl-dev \
  libcurl4-gnutls-dev \
  libpq-dev \
  make \  
  zlib1g-dev

# install caret xgboost etc...
RUN ["install2.r", "caret", "data.table", "DBI", "ggplot2", "lattice", "lubridate", "missRanger", "padr", "RPostgreSQL", "xgboost", "zoo"]

# copy all the other R files.
COPY src /src
WORKDIR /src

# open port 80 to traffic
EXPOSE 80

# when the container starts, start the main.R script
ENTRYPOINT ["Rscript", "MLews.R"]

