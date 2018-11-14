{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Control.Applicative
import Control.Monad
import Data.Char
import Text.RawString.QQ
import Text.Trifecta


---------------
---- Types ----
---------------

type PatternName = String
data TimeSignature = TimeSignature Int Int deriving Show
data Note = Rest | X | O | Accent deriving Show
data Track = Track Sound [Note] deriving Show

data Pattern = Pattern PatternName TimeSignature Track [Track] deriving Show

data Sound
    = AC
    | BD
    | SD
    | LT
    | MT
    | HT
    | CH
    | OH
    | CY
    | RS
    | CP
    | CB
    | TM
    deriving Eq

---------------------
---- Typeclasses ----
---------------------

instance Show Sound where
    show Lib.AC = show "Accent"
    show Lib.BD = show "Bass Drum"
    show Lib.SD = show "Snare Drum"
    show Lib.LT = show "Low Tom"
    show Lib.MT = show "Medium Tom"
    show Lib.HT = show "High Tom"
    show Lib.CH = show "Closed Hi-Hat"
    show Lib.OH = show "Open Hi-Hat"
    show Lib.CY = show "Cymbal"
    show Lib.RS = show "Rim Shot"
    show Lib.CP = show "Claps"
    show Lib.CB = show "Cowbell"
    show Lib.TM = show "Tambourine"

-----------------
---- Helpers ----
-----------------

readSound :: String -> Sound
readSound xs = case xs of
    "AC" -> Lib.AC
    "BD" -> Lib.BD
    "SD" -> Lib.SD
    "LT" -> Lib.LT
    "MT" -> Lib.MT
    "HT" -> Lib.HT
    "CH" -> Lib.CH
    "OH" -> Lib.OH
    "CY" -> Lib.CY
    "RS" -> Lib.RS
    "CP" -> Lib.CP
    "CB" -> Lib.CB
    "TM" -> Lib.TM

readNote :: Char -> Note
readNote x = case x of
    ' ' -> Rest
    'x' -> X
    'o' -> O
    '*' -> Accent

sigToEighths :: TimeSignature -> Int
sigToEighths (TimeSignature num 8) = num
sigToEighths (TimeSignature num denom)
    | 8 `mod` denom /= 0 = 0
    | denom < 8 = sigToEighths $ TimeSignature (num * 2) (denom * 2)
    | denom > 8 = sigToEighths $ TimeSignature (num `div` 2) (denom `div` 2)

stringToAccents :: String -> [Note]
stringToAccents xs = fst $ foldr f ([], 0) xs
    where f a (b, i) = ((readNote a):b, i+1)

-----------------
---- Parsers ----
-----------------

parsePatternName :: Parser PatternName
parsePatternName = token . some $ satisfy isAlphaNum

parseSignature :: Parser TimeSignature
parseSignature = token $ do
    num <- digit
    char '/'
    denom <- digit
    return $ TimeSignature (read [num] :: Int) (read [denom] :: Int)

parseSound :: Parser Sound
parseSound = token $ do
    sound <- fmap readSound $ sequence [letter, letter]
    return $ sound

parseNote :: Parser Note
parseNote = do
    note <- readNote <$> oneOf "xo "
    return note
    
parseTrack :: Int -> Parser Track
parseTrack i = token $ do
    sound <- parseSound
    string ": "
    notes <- parseNote `manyTill` char '\n'
    let pad = i - length notes
    return $ Track sound (notes ++ replicate pad Rest)

parseBeatCount :: Int -> Parser [Note]
parseBeatCount i = token $ do
    string "#   "
    sequence $ replicate i digit
    char '\n'
    accents <- parseAccents i
    return accents
    
parseAccents :: Int -> Parser [Note]
parseAccents i = do
    string "    "
    accentString <- oneOf "* " `manyTill` char '\n'
    let accents = stringToAccents accentString
    let pad = i - length accents
    return $ accents ++ replicate pad Rest

parsePattern :: Parser Pattern
parsePattern = token $ do
    spaces
    pName <- parsePatternName
    pSig <- parseSignature
    char '-' `manyTill` char '\n'
    let eighths = 2 * sigToEighths pSig
    accents <- parseBeatCount eighths
    tracks <- some $ parseTrack eighths
    let accentTrack = Track Lib.AC accents
    return $ Pattern pName pSig accentTrack tracks
    
-------------------
---- Test Data ----
-------------------

testPattern = [r|
AfroCub1 4/4
--------------------
#   1234567890123456
      *  *
CH: o ooo o o o o o
RS:    o  o     o
BD: o       o o   o
|]
