module Main where

import qualified Data.ByteString as B
import Data.Foldable
import qualified ParsersBench.Json.Attoparsec as A
import qualified ParsersBench.Json.Megaparsec as M
import qualified ParsersBench.Json.MegaparsecSimple as MS
import qualified ParsersBench.Json.MegaparsecSimpleOpt as MSO
import Test.Hspec

main :: IO ()
main = hspec $ do
  for_ jsonFiles $ \file -> context file $ do
    bs <- runIO $ B.readFile $ "data/" <> file
    for_
      [ (M.parseJson, "Megaparsec"),
        (MS.parseJson, "Megaparsec simple"),
        (MSO.parseJson, "Megaparsec simple choice-optimized")
      ]
      $ \(j, s) ->
        it
          (s <> " coinsides with Attoparsec")
          $ j bs `shouldBe` A.parseJson bs

csvFiles :: [FilePath]
csvFiles =
  [ "csv-5.csv",
    "csv-10.csv",
    "csv-20.csv",
    "csv-40.csv"
  ]

logFiles :: [FilePath]
logFiles =
  [ "log-5.log",
    "log-10.log",
    "log-20.log",
    "log-40.log"
  ]

jsonFiles :: [FilePath]
jsonFiles =
  [ "json-5.json",
    "json-10.json",
    "json-20.json",
    "json-40.json"
  ]
