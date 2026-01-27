FROM haskell:9.12.2-slim-bookworm AS builder
WORKDIR /usr/local/app
COPY app ./app
COPY lib ./lib
COPY test ./test
COPY *.cabal LICENSE CHANGELOG.md .env ./
RUN cabal update -j && \
    cabal install --installdir=/build/bin --install-method=copy -j

FROM debian:bookworm-slim
WORKDIR /usr/local/app
COPY --from=builder /build/bin/lambda-imd /usr/local/bin/app
RUN useradd -m app
USER app
EXPOSE 3000
CMD ["/usr/local/bin/app"]
