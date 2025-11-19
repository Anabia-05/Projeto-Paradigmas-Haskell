module Validacao
    ( mostrarMenuValidacao, executarOpcaoValidacao
    ) where

import System.IO (hFlush, stdout)
import Data.Char (isSpace, digitToInt, isDigit)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf)
import Control.Monad (replicateM)

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

