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
type Beat = String
data TimeSignature = TimeSignature Int Int deriving (Eq, Show)
data Track = Track Sound Beat deriving (Eq, Show)

data Pattern = Pattern PatternName TimeSignature [Track] deriving (Eq, Show)

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
    sound <- sequence [letter, letter]
    return $ readSound sound

parseTrack :: Parser Track
parseTrack = token $ do
    sound <- parseSound
    string ": "
    beat <- oneOf "xo " `manyTill` char '\n'
    return $ Track sound beat

parseBeatCount :: Parser ()
parseBeatCount = token $ do
    char '#'
    spaces
    digit `manyTill` char '\n'
    return ()

parsePattern :: Parser Pattern
parsePattern = do
    spaces
    pName <- parsePatternName
    pSign <- parseSignature
    char '-' `manyTill` char '\n'
    parseBeatCount
    tracks <- some parseTrack
    return $ Pattern pName pSign tracks
    

testPattern = [r|
AfroCub1 4/4
--------------------
#   1234567890123456
CH: o ooo o o o o o
RS:    o  o     o
BD: o       o o   o
|]

someFunc :: IO ()
someFunc = putStrLn "someFunc"
