module Estatistica
    ( mostrarMenuEstatistica, executarOpcaoEstatistica
    ) where

import System.IO (hFlush, stdout)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Control.Monad (replicateM)


lengthAsDouble :: [a] -> Double
lengthAsDouble [] = 0.0
lengthAsDouble (_:xs) = 1.0 + lengthAsDouble xs

insert :: Double -> [Double] -> [Double]
insert e [] = [e]
insert e (a:x) | e < a = e:(a:x)
               | otherwise = a : insert e x

sortList :: [Double] -> [Double]
sortList[] = []
sortList(a:x) = insert a (sortList x)

sumList :: [Double] -> Double
sumList[] = 0
sumList(a:x) = a + sumList x

mean :: [Double] -> Double
mean[] = 0
mean x = sumList x / lengthAsDouble x

median :: [Double] -> Double
median x | null x = 0
         | odd len = sorted !! mid
         | even len = evenMedian
                    where sorted = sortList x
                          len = length x
                          mid = len `div` 2
                          evenMedian = (sorted !! mid + sorted !! (mid-1)) / 2

executarOpcaoEstatistica :: Int -> IO()
executarOpcaoEstatistica opcao = do
    if opcao == 1 then do
        -- Input
        putStrLn "Digite os valores (separe eles utilizando espaços)"
        hFlush stdout
        userInput <- getLine
        let numbers = map read (words userInput) :: [Double]

        -- Functions
        let sortedDados = sortList numbers
        let media = mean sortedDados
        let mediana = median sortedDados
        let max = last sortedDados
        let min = head sortedDados

        -- Output
        putStrLn "\n-------------------------"
        putStrLn $ "Media: " ++ show media
        putStrLn $ "Mediana: " ++ show mediana
        putStrLn $ "Max: " ++ show max
        putStrLn $ "Min: " ++ show min
        putStrLn "-------------------------"
        else do
            putStrLn "\nOpção inválida. Por favor, escolha 1 ou 2."

mostrarMenuEstatistica :: IO()
mostrarMenuEstatistica = do
    putStrLn "\n--- MENU DE ESTATISTICA ---"
    putStrLn "1. Calcular Dados Estaticos"
    putStrLn "2. Voltar ao Menu Principal"
    putStrLn "-------------------------"
    putStrLn "Escolha uma opção (1-2): "
    hFlush stdout