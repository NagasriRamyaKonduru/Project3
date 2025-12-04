# ------------------------------------------------------------------------------
# Base image: lightweight R environment
# ------------------------------------------------------------------------------
FROM rocker/r-ver:4.3.1

# ------------------------------------------------------------------------------
# Install Linux system dependencies needed by R packages
# ------------------------------------------------------------------------------
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# ------------------------------------------------------------------------------
# Install required R packages
# ------------------------------------------------------------------------------
RUN R -e "install.packages(c('tidyverse', 'tidymodels', 'janitor', 'plumber', 'ggplot2', 'ranger', 'yardstick'), repos='https://cloud.r-project.org/')"

# ------------------------------------------------------------------------------
# Set working directory in the container
# ------------------------------------------------------------------------------
WORKDIR /app

# ------------------------------------------------------------------------------
# Copy your project files into the container
# NOTE: This copies EVERYTHING including your API, data, and Modeling files
# ------------------------------------------------------------------------------
COPY . /app

# ------------------------------------------------------------------------------
# Expose port 8000 so the API is reachable
# ------------------------------------------------------------------------------
EXPOSE 8000

# ------------------------------------------------------------------------------
# Run the plumber API when the container starts
# Replace BestFitModel.R with your actual API filename if different
# ------------------------------------------------------------------------------
CMD ["R", "-e", "pr <- plumber::plumb('BestFitModel.R'); pr$run(host='0.0.0.0', port=8000)"]
