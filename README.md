[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/7NMOLXjY)


# Scotty — Registro de jogos apitados


**Autor:** SEU NOME — Curso: ...


## Objetivo
Serviço web mínimo em Haskell usando Scotty para registrar jogos nos quais o usuário atuou como árbitro.


## Endpoints principais
- `GET /campeonatos` — lista campeonatos
- `POST /campeonatos` — cria um campeonato (`{ "nome": "Copa 2025" }`)
- `GET /jogos` — lista todos os jogos
- `POST /jogos` — registra um jogo (JSON com os campos do `Jogo`)
- `GET /jogos/:campeonato` — lista jogos de um campeonato


## Como executar (Codespaces)
1. Abra o Codespace a partir do repositório.
2. No terminal do Codespace: `cabal update` e `cabal v2-build`.
3. Rodar servidor: `cabal v2-run scotty-jogos-exe`.
4. Rodar testes: `cabal v2-test`.
5. Exponha a porta 3000 no painel "Ports" do Codespaces para testar no navegador ou via curl/Postman.


## Exemplos (curl)
```bash
# criar campeonato
curl -X POST -H "Content-Type: application/json" -d '{"nome":"Copa 2025"}' http://localhost:3000/campeonatos


# listar campeonatos
curl http://localhost:3000/campeonatos


# registrar jogo
curl -X POST -H "Content-Type: application/json" -d '{"campeonato":"Copa 2025","dataJogo":"2025-09-22","numQuadra":2,"numMesa":1,"salario":150.0}' http://localhost:3000/jogos


# buscar jogos por campeonato
curl http://localhost:3000/jogos/Copa%202025
