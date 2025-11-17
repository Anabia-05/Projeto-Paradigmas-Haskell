# Projeto-Paradigmas-Haskell

## Integrantes 
- Ana Beatriz Romero 
- Guilherme Cavalcanti de Sá Barreto (849537)
- João Victor Rocha Fernandes (850069)
- João Constantino Pontes Barreto (849518)
- Pedro Henrique Afonso

## Tecnologias utilizadas:

![Haskell](https://img.shields.io/badge/Haskell-5e5086?style=for-the-badge&logo=haskell&logoColor=white)
![GHC](https://img.shields.io/badge/GHC-8.10+-yellow?style=for-the-badge)
![Cabal](https://img.shields.io/badge/Cabal-3.0+-brightgreen?style=for-the-badge)
![Stack](https://img.shields.io/badge/Stack-0.1+-orange?style=for-the-badge&logo=stackshare&logoColor=white)

## Instruções para Executar o codigo:

### 1. Instalar o Stack

Se você ainda não possui o Stack instalado:

```bash
curl -sSL https://get.haskellstack.org/ | sh
```

Ou baixe diretamente pela página oficial:

https://docs.haskellstack.org/en/stable/

### 2. Clonar este repositório

```bash
git clone https://github.com/Projeto-Paradigmas-Haskell
cd Projeto-Paradigmas-Haskell
```


### 3. Construir o projeto

O comando abaixo baixa o GHC adequado, instala dependências e compila:

```bash
stack build
```

### 4. Executar o projeto

Se o projeto possui um único executável, basta rodar:

```bash
stack run
```

### 5. Executar no GHCi (modo interativo)
```bash
stack ghci
```

Dentro do GHCi:

```Haskell
main
```
