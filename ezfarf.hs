{-
    ezfarf
    traduttore italiano-farfallino.
    Sono state applicate le regole standard del farfallino.
    Per ogni vocale in una parola, questa viene sostituita dalla costruzione vocale + "f" + vocale.
-}

import Control.Monad (when)
import System.Exit (exitSuccess)

-- main program
main :: IO ()
main = do  
    putStrLn "---------------ezfarf---------------"
    putStrLn "Scegli cosa vuoi fare: [1: Ita -> Farf / 2: Farf -> Ita / 3: Esci]"
    scelta <- getLine
    check scelta
    main
    
    
-- ccheck sulla scelta in input, redirige sulle funzioni di traduzione o uscita
check :: [Char] -> IO()
check scelta = case scelta of
    "1" -> farf
    "2" -> ita
    "3" -> esci
    _ -> when (scelta /= "") $ putStrLn "Selezione invalida."
    
-- caso di esecuzione per tradurre in farfallino
farf :: IO ()
farf = do
    putStrLn "Inserisci frase da tradurre in farfallino:"
    xs <- getLine
    putStrLn $ "\nLa frase tradotta equivale a: " ++ toFarf xs ++ "\n"
        where toFarf :: [Char] -> [Char]
              toFarf [] = []
              toFarf (x:xs) | x `elem` "AEIOUaeiou" = [x] ++ "f" ++ [x] ++ toFarf xs
                            | otherwise = x : toFarf xs


-- caso di esecuzione per tradurre in italiano
ita :: IO ()
ita = do
    putStrLn "Inserisci frase da tradurre in italiano:"
    xs <- getLine
    putStrLn $ "\nLa frase tradotta equivale a: " ++ toIta xs ++ "\n"
        where toIta :: [Char] -> [Char]
              toIta [] = []
              toIta [x] = [x]
              toIta (x:xs) | x `elem` "AEIOUaeiou" && head xs `elem` "Ff" && xs !! 1 == x = x : toIta (drop 2 xs)
                           | otherwise = x : toIta xs


-- caso di esecuzione per uscita dal programma
esci :: IO ()
esci = do
    putStrLn "------------Arrivederci!------------"
    exitSuccess




