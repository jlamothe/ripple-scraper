module Main where

import System.IO
import Data.List
import qualified Text.CSV as CSV

main = interact $ process . lines

process :: [String] -> String
process xs = "\"Ripple Address\",\"Contact\"\n" ++ subProcess xs

subProcess :: [String] -> String
subProcess [] = "\n"
subProcess (id : ripple : "Edit" : "Send" : xs) =
  CSV.printCSV [makeRecord id ripple] ++ "\n" ++ subProcess xs
subProcess _ = "\"[invalid data]\"\n"

makeRecord :: String -> String -> CSV.Record
makeRecord idString rippleString =
  [rippleAddress rippleString, idString]

rippleAddress :: String -> CSV.Field
rippleAddress str =
  if "ripple: r" `isPrefixOf` str
  then drop 8 str
  else "[invalid address]"

-- jl
