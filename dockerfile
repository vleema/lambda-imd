FROM haskell:9.12.2-slim-bookworm AS builder
WORKDIR /usr/local/app
COPY app ./app
COPY lib ./lib
COPY test ./test
COPY *.cabal LICENSE CHANGELOG.md ./
RUN cabal update -j && \
    cabal install --installdir=/build/bin --install-method=copy -j --ghc-options="-O2"

FROM debian:bookworm-slim
ENV TZ=America/Fortaleza
WORKDIR /usr/local/app
RUN apt-get update && apt-get install -y --no-install-recommends netbase ca-certificates && rm -rf /var/lib/apt/lists/*
COPY --from=builder /build/bin/lambda-imd /usr/local/bin/app
COPY .env ./
RUN useradd -m app
RUN chown -R app:app /usr/local/app
USER app
EXPOSE 8080
CMD ["/usr/local/bin/app"]
