FROM rocker/r-ver:4.3.1

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev && \
    rm -rf /var/lib/apt/lists/*

# Install plumber + ranger normally
RUN R -e "install.packages(c('plumber','ranger'), repos='https://cloud.r-project.org/')"

# FORCE Rscript to use the site-library where plumber actually installs
ENV R_LIBS_SITE=/usr/local/lib/R/site-library

WORKDIR /app
COPY . /app

EXPOSE 8000

CMD ["R", "-e", "pr <- plumber::pr('plumber.R'); pr$run(host='0.0.0.0', port=8000)"]

ENTRYPOINT ["Rscript", "api.R"]
