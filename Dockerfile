FROM  rstudio/r-base:4.2.1-focal

RUN apt-get update

ENV PATH="/root/miniconda3/bin:${PATH}"
ARG PATH="/root/miniconda3/bin:${PATH}"
RUN apt-get update

RUN apt-get install -y wget && rm -rf /var/lib/apt/lists/*
  
RUN wget \
https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh \
&& mkdir /root/.conda \
&& bash Miniconda3-latest-Linux-x86_64.sh -b \
&& rm -f Miniconda3-latest-Linux-x86_64.sh

RUN apt-get update && apt-get install --no-install-recommends -y git-all python3.8-venv python3-pip \
libxml2-dev libssl-dev libpng-dev libv8-dev jq libcurl4-openssl-dev libsasl2-dev libopenblas-dev odbc-postgresql \
unixodbc unixodbc-dev

RUN echo "[postgresql]\nDriver = /usr/lib/x86_64-linux-gnu/odbc/psqlodbcw.so" >> /etc/odbcinst.ini


ENV RENV_VERSION v1.0.7
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

COPY renv.lock /renv.lock

RUN R -e 'renv::restore()'
RUN R -e 'reticulate::py_install(packages = c("fake_useragent","requests", "uuid"), pip=T)'

COPY www/ /app/www
COPY functions.py /app/
COPY app.R /app/

EXPOSE 8080
WORKDIR /app

CMD ["Rscript", "app.R"] 


