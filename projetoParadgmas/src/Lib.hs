module Lib
    ( dolarParaReal, realParaDolar, euroParaReal, realParaEuro
    , mostrarMenuConversao, executarOpcaoConversao
    ) where

import System.IO (hFlush, stdout) 

taxaDolarReal :: Double
taxaDolarReal = 5.31  

taxaEuroReal :: Double
taxaEuroReal = 6.16

dolarParaReal :: Double -> Double
dolarParaReal dolar = dolar * taxaDolarReal

realParaDolar :: Double -> Double
realParaDolar real = real / taxaDolarReal

euroParaReal :: Double -> Double
euroParaReal euro = euro * taxaEuroReal

realParaEuro :: Double -> Double
realParaEuro real = real / taxaEuroReal

-- I/O: Menu de Conversão
mostrarMenuConversao :: IO ()
mostrarMenuConversao = do
    putStrLn "\n--- MENU DE CONVERSÃO ---"
    putStrLn "1. Dólar para Real"
    putStrLn "2. Real para Dólar"
    putStrLn "3. Euro para Real"
    putStrLn "4. Real para Euro"
    putStrLn "5. Voltar ao Menu Principal"
    putStrLn "-------------------------"
    putStr "Escolha uma opção (1-5): "
    hFlush stdout -- Garante que o prompt seja exibido imediatamente

-- I/O: Executar Opção de Conversão
executarOpcaoConversao :: Int -> IO ()
executarOpcaoConversao 5 = putStrLn "Voltando..."
executarOpcaoConversao opcao = do
    putStr "Digite a quantia a ser convertida: "
    hFlush stdout
    
    let quantia = read input :: Double

    case opcao of
        1 -> do
            let resultado = dolarParaReal quantia
            putStrLn $ show quantia ++ " USD = R$ " ++ show resultado
        2 -> do
            let resultado = realParaDolar quantia
            putStrLn $ "R$ " ++ show quantia ++ " = " ++ show resultado ++ " USD"
        3 -> do
            let resultado = euroParaReal quantia
            putStrLn $ show quantia ++ " EUR = R$ " ++ show resultado
        4 -> do
            let resultado = realParaEuro quantia
            putStrLn $ "R$ " ++ show quantia ++ " = " ++ show resultado ++ " EUR"
        _ -> putStrLn "Opção inválida. Por favor, escolha 1, 2, 3, 4 ou 5."