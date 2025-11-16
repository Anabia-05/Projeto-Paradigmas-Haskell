module Main (main) where

import Lib (mostrarMenuConversao, executarOpcaoConversao)
import System.IO (hFlush, stdout) -- Comando flush para garantir a organixzação do I/O no terminal

-- Menu Principal
mostrarMenu :: IO ()
mostrarMenu = do
    putStrLn "\n--- MENU DE OPERAÇÃO ---"
    putStrLn "1. Transformação Monetária"
    putStrLn "2. Estatísticas"
    putStrLn "3. Verificar e-mail ou cpf"
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
        2 -> putStrLn "Opção 2 (Estatísticas) selecionada. Implementação pendente."
        3 -> putStrLn "Opção 3 (Verificar e-mail/cpf) selecionada. Implementação pendente."
        _ -> putStrLn "Opção inválida. Por favor, escolha 1, 2, 3 ou 4."


loopConversao :: IO ()
loopConversao = do
    mostrarMenuConversao
    opcaoStr <- getLine
    
    let opcao = if null opcaoStr then 0 else read opcaoStr :: Int
    
    if opcao == 5 
        then putStrLn "Voltando ao Menu Principal..."
        else do
            executarOpcaoConversao opcao
            loopConversao

-- I/O: Loop Principal
loop :: IO ()
loop = do
    mostrarMenu
    opcaoStr <- getLine
    
    let opcao = if null opcaoStr then 0 else read opcaoStr :: Int
    
    if opcao == 4 
        then executarOpcao 4
        else do
            executarOpcao opcao
            loop

main :: IO ()
main = loop