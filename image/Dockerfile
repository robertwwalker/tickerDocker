# get shiny server plus tidyverse packages image
# FROM rocker/tidyverse:latest
# system libraries of general use
FROM openanalytics/r-shiny
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libxml2-dev \
    libudunits2-dev \
    libcurl4-openssl-dev \
    libsasl2-dev \
    libv8-dev
 
# install R packages required 
# (change it depending on the packages you need)
RUN R -e "install.packages(c('tidyverse','knitr',plotly','hrbrthemes','ggthemes','lubridate','magrittr'), repos='http://cran.rstudio.com/')"
# Copy configuration files into the Docker image
# COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf
# COPY /app/index.Rmd /srv/shiny-server/
# RUN rm /srv/shiny-server/index.html

# make directory and copy Rmarkdown flexdashboard file in it
RUN mkdir -p /bin
COPY app/app.R    /bin/app.R


# make all app files readable (solves issue when dev in Windows, but building in Ubuntu)
RUN chmod -R 755 /bin

# expose port on Docker container
EXPOSE 3838
# Copy further configuration files into the Docker image
# COPY shiny-server.sh /usr/bin/shiny-server.sh
# RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]
# CMD ["/usr/bin/shiny-server.sh"]
# run flexdashboard as localhost and on exposed port in Docker container
CMD ["R", "-e", "rmarkdown::run('/bin/app.R', shiny_args = list(port = 3838))"]