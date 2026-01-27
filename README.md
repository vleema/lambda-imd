# lambda-imd

Esse projeto visa notificar a publicação de novos editais em <https://www.imd.ufrn.br/portal/editais>. O projeto é uma reescrita em Haskell do projeto [observador-editais-imd](https://github.com/IanGabriel12/observador-editais-imd) que não é mais mantido.

Existe uma versão disponível em <https://observador-imd.mariads.org> que pode ser usada livremente.

## Desenvolvimento

Tenha certeza que pelo menos isso aqui do kit de desenvolvimento do haskell esteja instalado:

- ghc 9.12.2
- cabal 3.14.2.0

Configure as seguintes variáveis de ambiente no arquivo `.env` ou em um outro lugar de preferência, tu vai precisar configurar as credenciais para teu server SMTP favorito e a url em que tua aplicação ficará exposta (pode ser `http://localhost:3000` se for simplesmente rodar local).

```bash
URL=http://localhost:3000
SMTP_HOST=
SMTP_PORT=
SMTP_USER=
SMTP_PASS=
```

Depois disso, basta executar `cabal run` e o servidor ficará exposto na porta `8080`. A página principal da aplicação fica em `/`.
