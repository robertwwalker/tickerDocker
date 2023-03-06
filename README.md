# Shiny app in a docker container


I started with a fork of a repo accompanies an article [here](https://blog.sellorm.com/2021/04/25/shiny-app-in-docker/).


# 'image' directory

This contains the entirety of the image files.

- Dockerfile
- `shiny-server.conf`
- `shiny-server.sh`
- the *app* directory
  + `app.R` the single app file.


The *app* directory contains only those things which are needed for the app to run.
In our case that's the `app.R` file.


## Dockerfile and project layout

The Dockerfile here has everything that we need to build the Docker container.  There is [a blog post on the entirety of this exercise here.](https://robertwwalker.github.io/posts/Docker-Shiny/)


## `shiny-server.conf`

The configuration files for the shiny server.

## `shiny-server.sh`

The logs are configured here.
