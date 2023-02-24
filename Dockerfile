#FROM rocker/shiny:3.6.0
FROM rocker/shiny:3.4.2

RUN rm -rf /srv/shiny-server/*

WORKDIR /srv/shiny-server/

COPY ./Practica1 ./Practica1

RUN R -e "install.packages('car')"
RUN R -e "install.packages('e1071')"
RUN R -e "install.packages('DT')"
RUN R -e "install.packages('corrplot')"
RUN R -e "install.packages('tools')"


