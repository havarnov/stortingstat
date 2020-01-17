FROM microsoft/dotnet:2.2-sdk-alpine as build

WORKDIR /app
COPY . ./
WORKDIR /app/stortingsskam.app
RUN dotnet restore
RUN dotnet publish -c Release -o /app/out

FROM microsoft/dotnet:2.2-aspnetcore-runtime-alpine as deploy

WORKDIR /app
COPY --from=build /app/out .
CMD ASPNETCORE_URLS=http://*:$PORT dotnet stortingsskam.app.dll