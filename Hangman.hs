module Hangman where

import System.Posix.Terminal 
import System.Posix.IO (stdInput)
import System.IO
import Data.List
import Data.Char
 
-- This function is used to hide the input typed into the console.
getWord :: IO String 
getWord = do
    att <- getTerminalAttributes stdInput
    setTerminalAttributes stdInput (withoutMode att EnableEcho) Immediately
    word <- getLine
    setTerminalAttributes stdInput att Immediately
    return word

-- show the word to the players
showWord :: String -> String -> String
showWord word guesses = intersperse ' ' (map (\c -> if c `notElem` guesses then '_' else c) word)

-- function is used to determine if the player(s) have correctly guessed the word
updateWord :: String -> Char -> String
updateWord word guess = map (\c -> if c == guess then '+' else c) word

game :: String -> String -> String -> Int -> IO ()
game word guesses updatedWord lifes = do
    putStrLn $ showWord word guesses ++ "       ," ++ show lifes ++ " attempts left."
    putStrLn "\n> Make a guess!"
    bigGuess <- getChar
    putStrLn ""
    let guess = toLower bigGuess in 
     if guess `elem` word then 
        let newWord = updateWord updatedWord guess in
        if all (=='+') newWord then
             putStrLn $ showWord word (guess:guesses) ++ "\n> Congratulations! You won!"
        else game word (guess:guesses) newWord lifes
     else 
        case lifes of
            0 -> putStrLn $ "> You lost!" ++ " - The word was \"" ++ word ++ "\""
            _ -> game word (guess:guesses) updatedWord (lifes - 1)
    
main :: IO ()
main = do
    putStrLn "---- Welcome to HANGMAN! ----\n> Player 1, please enter the word to be guessed:"
    hFlush stdout
    input <- getWord   
    if not (null input) then let word = map toLower input in game word "" word ((length word `div` 2) + 1) -- Player 2 has (length of word / 2) + 1 tries to guess the word
    else putStrLn "> You have to enter a word with at least 1 character." >> main   




