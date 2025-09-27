# Use an official Haskell image as a base image
FROM haskell:latest

# Install necessary tools: cabal, git, curl (para testes API), e mais
RUN apt-get update && apt-get install -y \
    build-essential \
    curl \
    git \
    hlint \  # Linter Haskell (opcional, para análise de código)
    && rm -rf /var/lib/apt/lists/*

# Install cabal-install (gerenciador de pacotes)
RUN curl -sSL https://get-ghcup.haskell.org | sh -s -- -j -- set cabal && \
    source ~/.ghcup/env && \
    cabal update

# Set up the working directory
WORKDIR /workspace

# Expor porta para o servidor (opcional, mas bom para docs)
EXPOSE 3000
