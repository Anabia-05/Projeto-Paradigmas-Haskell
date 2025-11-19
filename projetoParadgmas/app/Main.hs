module Main (main) where

import  Conversao(mostrarMenuConversao, executarOpcaoConversao)
import  Validacao(mostrarMenuValidacao, executarOpcaoValidacao)
import  Estatistica(mostrarMenuEstatistica, executarOpcaoEstatistica)
import System.IO (hFlush, stdout)

-- Menu Principal
mostrarMenu :: IO ()
mostrarMenu = do
    putStrLn "\n--- MENU DE OPERAÇÃO ---"
    putStrLn "1. Transformação Monetária"
    putStrLn "2. Estatísticas"
    putStrLn "3. Verificar E-mail ou CPF"
    putStrLn "4. Sair"
    putStrLn "-------------------------"
    putStr "Escolha uma opção (1-4): "
    hFlush stdout

-- I/O: Executar Opção
executarOpcao :: Int -> IO ()
executarOpcao 4 = putStrLn "Saindo do programa. Até mais!"
executarOpcao opcao =
    case opcao of
        1 -> do
            loopConversao
        2 -> do
            loopEstatistica
        3 -> do
            loopValidacao  
        _ -> putStrLn "Opção inválida. Por favor, escolha 1, 2, 3 ou 4."

-- Loop para Conversão Monetária
loopConversao :: IO ()
loopConversao = do
    mostrarMenuConversao
    opcaoStr <- getLine

    -- Tratamento de entrada vazia e leitura
    let opcao = if null opcaoStr then 0 else read opcaoStr :: Int

    if opcao == 5
        then putStrLn "Voltando ao Menu Principal..."
        else do
            executarOpcaoConversao opcao
            loopConversao

-- Loop para Validação (CPF/E-mail)
loopValidacao :: IO ()
loopValidacao = do
    mostrarMenuValidacao
    opcaoStr <- getLine

    -- Tratamento de entrada vazia e leitura
    let opcao = if null opcaoStr then 0 else read opcaoStr :: Int

    if opcao == 3
        then putStrLn "Voltando ao Menu Principal..."
        else do
            executarOpcaoValidacao opcao
            loopValidacao

loopEstatistica :: IO()
loopEstatistica = do
    mostrarMenuEstatistica
    opcaoStr <- getLine

    let opcao = if null opcaoStr then 0 else read opcaoStr :: Int

    if opcao == 2
        then putStrLn "Voltando ao Menu Principal..."
    else do
        executarOpcaoEstatistica opcao
        loopEstatistica

-- I/O: Loop Principal
loop :: IO ()
loop = do
    mostrarMenu
    opcaoStr <- getLine

    -- Tratamento de entrada vazia e leitura
    let opcao = if null opcaoStr then 0 else read opcaoStr :: Int

    if opcao == 4
        then executarOpcao 4
        else do
            executarOpcao opcao
            loop

main :: IO ()
main = loop