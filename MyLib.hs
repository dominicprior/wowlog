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
import Data.List
import Data.Map

type SwingMap = Map [String] Int

u = undefined

p :: Stream s Data.Functor.Identity.Identity t =>
     Parsec s () a -> s -> Either ParseError a
p x s = parse x "" s

parseDate :: Parsec String SwingMap String
parseDate = many1 $ Text.Parsec.Char.digit <|> oneOf "/ :."

restOfLine = many (noneOf "\n") >> newline

boringRow :: String -> Parsec String SwingMap String
boringRow str = string str >> restOfLine >> return "row"

ww :: Parsec String SwingMap SwingMap
ww = do
  w
  getState

w :: Parsec String SwingMap [String]
w = parseDate >> boringRow "COMBAT_LOG_VERSION,9,ADVANCED_LOG_ENABLED,1"
        >> many row <* eof

row :: Parsec String SwingMap String
row = do
  date <- parseDate
  boringRow "ZONE_CHANGE" <|>
    boringRow "MAP_CHANGE"  <|>
    (char 'S' >> row_S date)

row_S :: String -> Parsec String SwingMap String
row_S date = do
  boringRow "PELL_AURA"  <|>
    (string "WING_DAMAGE" >> row_SWING_DAMAGE date)

row_SWING_DAMAGE :: String -> Parsec String SwingMap String
row_SWING_DAMAGE date = do
  (string "_LANDED" >> swingDamage date True)
                   <|> swingDamage date False

word :: Parsec String SwingMap String
word = char ',' >> many1 (letter <|> digit <|> oneOf "-_\" .")

parserFailIf :: Bool -> String -> ParsecT s u m ()
parserFailIf cond str =
  if cond
  then parserFail str
  else return ()

updateSwingMap :: [String] -> SwingMap -> Bool -> Parsec String SwingMap ()
updateSwingMap key m landed =
  if member key m
  then if landed
       then modifyState $ Data.Map.delete key
       else parserFail "duplicate"
  else if landed
       then parserFail "not found"
       else modifyState $ Data.Map.insert key 1

swingDamage :: String -> Bool -> Parsec String SwingMap String
swingDamage date landed = do
  src  <- replicateM 4 word
  dest <- replicateM 4 word
  let key = date : src
  state <- getState
  updateSwingMap key state landed
  srcOrDest <- word
  let target = if landed then dest else src
  parserFailIf (srcOrDest /= head target) $ srcOrDest ++ " /= " ++ head src
  string ",0000000000000000"
  unitInfo <- replicateM 5 word  -- curr hp, max hp, attack power, spell power, armor
  char ','
  absorb <- string "1" <|> string "-1"  -- what is absorb?
  moreUnitInfo <- replicateM 2 word  -- curr power, max power
  let allUnitInfo = unitInfo ++ moreUnitInfo
  string ",0"
  replicateM 4 word   -- posX, posY, map id, facing
  level <- word
  -- amount, overkill, school, resisted, blocked, absorbed, crit, glancing, crushing, isOffHand
  dmgInfo <- replicateM 10 word
  newline
  return $ intercalate ", " dmgInfo



str = [r|2/19 21:34:06.467  COMBAT_LOG_VERSION,9,ADVANCED_LOG_ENABLED,1,bla
2/19 21:34:06.467  ZONE_CHANGE,0,"UNKNOWN AREA",0
2/19 21:34:10.432  SPELL_AURA_APPLIED,bla
2/19 21:34:11.023  MAP_CHANGE,bla
2/19 21:34:34.693  SWING_DAMAGE,Player-4456-01E73915,"Sidespin-NethergardeKeep",0x511,0x0,Creature-0-5571-0-262-1186-0000913776,"Elder Black Bear",0x10a48,0x0,Player-4456-01E73915,0000000000000000,100,100,367,0,2268,1,110,1000,0,-5799.95,-3087.99,1432,2.2373,28,149,81,-1,1,0,0,0,1,nil,nil
2/19 21:34:36.392  SWING_DAMAGE,Player-4456-01E73915,"Sidespin-NethergardeKeep",0x511,0x0,Creature-0-5571-0-262-1186-0000913776,"Elder Black Bear",0x10a48,0x0,Player-4456-01E73915,0000000000000000,100,100,367,0,2268,1,166,1000,0,-5799.95,-3087.99,1432,2.2373,28,64,68,-1,1,0,0,0,nil,nil,nil
2/19 21:34:34.693  SWING_DAMAGE_LANDED,Player-4456-01E73915,"Sidespin-NethergardeKeep",0x511,0x0,Creature-0-5571-0-262-1186-0000913776,"Elder Black Bear",0x10a48,0x0,Creature-0-5571-0-262-1186-0000913776,0000000000000000,44,100,0,0,0,-1,0,0,0,-5801.52,-3086.12,1432,5.2693,11,149,81,-1,1,0,0,0,1,nil,nil
|]

k :: Either Text.Parsec.Error.ParseError [String]
-- k = p w str
k = runParser w Data.Map.empty "" str

kk = runParser ww Data.Map.empty "" str
