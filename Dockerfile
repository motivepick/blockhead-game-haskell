ARG BASE_IMAGE="amd64/haskell:latest"
FROM ${BASE_IMAGE}

WORKDIR /app

COPY stack.yaml stack.yaml
COPY blockhead-game.cabal blockhead-game.cabal

COPY . .

# See https://github.com/docker/for-win/issues/1340
RUN sed -i 's/\r$//' dictionary.txt

RUN stack setup
RUN stack build --install-ghc

EXPOSE 8080

CMD ["stack", "exec", "blockhead-game-exe"]
