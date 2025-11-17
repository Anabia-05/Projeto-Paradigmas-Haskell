module Lib
    ( dolarParaReal, realParaDolar, euroParaReal, realParaEuro
    , mostrarMenuConversao, executarOpcaoConversao
    , mostrarMenuValidacao, executarOpcaoValidacao
    , mostrarMenuEstatistica, executarOpcaoEstatistica
    ) where

import System.IO (hFlush, stdout)
import Data.Char (isSpace, digitToInt, isDigit)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Control.Monad (replicateM)

-- --- Conversão Monetária ---

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
    hFlush stdout

-- I/O: Executar Opção de Conversão
executarOpcaoConversao :: Int -> IO ()
executarOpcaoConversao 5 = putStrLn "Voltando..."
executarOpcaoConversao opcao = do
    putStr "Digite a quantia a ser convertida: "
    hFlush stdout

    inputStr <- getLine
    let quantia = read inputStr :: Double -- ASSUME inputStr is valid Double for simplicity

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

-- --- Validação (CPF/E-mail) ---

-- Validação de E-mail (Corrigida na linha 78)
validaEmail :: String -> Bool
validaEmail email =
    -- 1. Safety check & must contain '@'
    (length email >= 3) && '@' `elem` email &&

    -- 1. '@' não pode ser o primeiro nem o último caractere (CORREÇÃO APLICADA)
    (not $ "@" `isPrefixOf` email) &&
    (not $ "@" `isSuffixOf` email) &&

    -- 2. Não deve conter espaços
    not (any isSpace email) &&

    -- 3. Deve terminar em um domínio simples
    let parts = splitOn '@' email in
    case parts of
        [local, domain] -> 
            '.' `elem` domain && 
            let domainParts = splitOn '.' domain in
            case reverse domainParts of
                (tld:rest) -> length tld >= 2 && not (null rest)
                _ -> False
        _ -> False

-- Função auxiliar para dividir string (Inalterada)
splitOn :: Char -> String -> [String]
splitOn delim str = case dropWhile (== delim) str of
    "" -> []
    s -> w : splitOn delim (drop 1 s')
        where (w, s') = break (== delim) s

-- Função auxiliar para calcular DV (Inalterada)
calculaDigito :: Int -> [Int] -> Int
calculaDigito fatorInicial digitos =
    let
        fatores = reverse [2..fatorInicial]
        soma = sum $ zipWith (*) digitos fatores
        resto = soma `mod` 11
    in
        if resto < 2 then 0 else 11 - resto

-- Validação principal do CPF (Corrigida na linha 128)
validaCPF :: String -> Bool
validaCPF cpf =
    -- 1. Verifica se tem 11 dígitos e se são todos números
    if length cpf /= 11 || not (all isDigit cpf)
        then False
    
    -- 2. Verifica se são todos dígitos iguais (CORREÇÃO APLICADA)
    else if all (== head cpf) cpf
        then False
        
    else
        let
            todosDigitos = map digitToInt cpf
            digitos9 = take 9 todosDigitos
            digitos10 = take 10 todosDigitos
            dvReais = drop 9 todosDigitos
            dvCalculado1 = calculaDigito 10 digitos9
            dvCalculado2 = calculaDigito 11 digitos10
        in
            [dvCalculado1, dvCalculado2] == dvReais


-- I/O: Menu de Validação (mantido)
mostrarMenuValidacao :: IO ()
mostrarMenuValidacao = do
    putStrLn "\n--- MENU DE VALIDAÇÃO ---"
    putStrLn "1. Validar CPF"
    putStrLn "2. Validar E-mail"
    putStrLn "3. Voltar ao Menu Principal"
    putStrLn "-------------------------"
    putStr "Escolha uma opção (1-3): "
    hFlush stdout

-- I/O: Executar Opção de Validação (mantido)
executarOpcaoValidacao :: Int -> IO ()
executarOpcaoValidacao 3 = putStrLn "Voltando..."
executarOpcaoValidacao opcao = do
    case opcao of
        1 -> do
            putStr "Digite o CPF (apenas números): "
            hFlush stdout
            inputStr <- getLine
            if validaCPF inputStr
                then putStrLn "CPF é válido (Verificação completa)."
                else putStrLn "CPF é inválido (Formato, tamanho, ou dígito verificador incorreto)."
        2 -> do
            putStr "Digite o E-mail: "
            hFlush stdout
            inputStr <- getLine
            if validaEmail inputStr
                then putStrLn "E-mail é válido."
                else putStrLn "E-mail é inválido (Verifique '@', espaços e domínio)."
        _ -> putStrLn "Opção inválida. Por favor, escolha 1, 2 ou 3."


-- Estatica

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
        let min = head sortedDados -- CORREÇÃO: O primeiro elemento da lista crescente é o MÍNIMO
        let max = last sortedDados -- CORREÇÃO: O último elemento da lista crescente é o MÁXIMO

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