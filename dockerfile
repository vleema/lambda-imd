FROM haskell:9.12.2-slim-bookworm AS builder
WORKDIR /usr/local/app
COPY app ./app
COPY lib ./lib
COPY test ./test
COPY *.cabal LICENSE CHANGELOG.md ./
RUN cabal update -j && \
    cabal install --installdir=/build/bin --install-method=copy -j

FROM debian:bookworm-slim
WORKDIR /usr/local/app
COPY --from=builder /build/bin/lambda-imd /usr/local/bin/app
COPY .env ./
RUN useradd -m app
RUN chown -R app:app /usr/local/app
USER app
EXPOSE 8080
CMD ["/usr/local/bin/app"]
