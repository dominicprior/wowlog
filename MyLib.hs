{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module MyLib where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Error
import Data.Functor.Identity
import Text.RawString.QQ
import Control.Monad

p :: Stream s Data.Functor.Identity.Identity t =>
     Parsec s () a -> s -> Either ParseError a
p x s = parse x "" s

date :: Parser String
date = many1 $ Text.Parsec.Char.digit <|> oneOf "/ :."

restOfLine = many (noneOf "\n") >> newline

row :: String -> Parser String
row str = date >> string str >> restOfLine >> return "row"

w :: Parser [String]
w = row "COMBAT_LOG_VERSION,9,ADVANCED_LOG_ENABLED,1" >>
  many (try (row "ZONE_CHANGE") <|>
        try (row "MAP_CHANGE")  <|>
        try (row "SPELL_AURA")  <|>
        swingDamage
        )

word :: Parser String
word = char ',' >> many1 (letter <|> digit <|> oneOf "-_\" .")

swingDamage :: Parser String
swingDamage = do
  d <- date
  string "SWING_DAMAGE"
  src  <- replicateM 4 word
  dest <- replicateM 4 word
  srcOrDest <- word
  guard $ srcOrDest == head src
  string ",0000000000000000"
  currHp <- char ',' >> many digit
  string ",100"
  replicateM 3 word
  char ','
  absorb <- string "1" <|> string "-1"
  currPower <- word
  word   -- max power
  string ",0"
  replicateM 4 word   -- posX, posY, map id, facing
  level <- word
  amount <- word
  overkill <- word
  school <- word
  string ",1,0,0,0"  -- ? resisted, blocked, absorbed, glancing ?
  crit <- word   -- 1 or nil
  string ",nil,nil"  -- crushing, isOffHand
  newline
  return $ amount



str = [r|2/19 21:34:06.467  COMBAT_LOG_VERSION,9,ADVANCED_LOG_ENABLED,1,bla
2/19 21:34:06.467  ZONE_CHANGE,0,"UNKNOWN AREA",0
2/19 21:34:10.432  SPELL_AURA_APPLIED,bla
2/19 21:34:11.023  MAP_CHANGE,bla
2/19 21:34:34.693  SWING_DAMAGE,Player-4456-01E73915,"Sidespin-NethergardeKeep",0x511,0x0,Creature-0-5571-0-262-1186-0000913776,"Elder Black Bear",0x10a48,0x0,Player-4456-01E73915,0000000000000000,100,100,367,0,2268,1,110,1000,0,-5799.95,-3087.99,1432,2.2373,28,149,81,-1,1,0,0,0,1,nil,nil
2/19 21:34:36.392  SWING_DAMAGE,Player-4456-01E73915,"Sidespin-NethergardeKeep",0x511,0x0,Creature-0-5571-0-262-1186-0000913776,"Elder Black Bear",0x10a48,0x0,Player-4456-01E73915,0000000000000000,100,100,367,0,2268,1,166,1000,0,-5799.95,-3087.99,1432,2.2373,28,64,68,-1,1,0,0,0,nil,nil,nil
|]

k :: Either Text.Parsec.Error.ParseError [String]
k = p w str
