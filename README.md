# Scotty — Registro de jogos apitados


**Autor:** Juran Quesada Tavares — Curso: Sistema de Informação


## Objetivo
Serviço web mínimo em Haskell usando Scotty para registrar jogos nos quais o árbitro atuou com crud e filtragem de jogos por meses conforme data de pagamento.

## Processo de desenvolvimento:
Me baseei no exemplo de SQLite fornecido.<br>
Defini a estrutura básica: Modelo Campeonato + CRUD + Frontend simples (fiz um esboço em um jframe de como queria e pedi pra ia copilot fazer)<br>
![imagem front](frontimage.png)
Implementei a função wordsWhen quando busquei pra fazer a separação da data, no inicio funcionava porém foi mais complexo de entender e ao analizar lembrei da função splitOn que era bem simples e tinha a mesma função.
```
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s'' where (w, s'') = break p s'
```

Tive dificuldades para fazer o html aparecer na página principal, mas ao pesquisar vi que tinha que fazer a requisição HTTP do get pro caminho raiz e enviar o arquivo index.html  
```
get "/" $ file "index.html"
```



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
