```sh
docker build -f ./Dockerfile src
```


```sh
heroku container:push web -a guarded-refuge-10281 --context-path src
heroku container:release web -a guarded-refuge-10281
```