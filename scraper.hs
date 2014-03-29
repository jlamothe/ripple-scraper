-- Ripple Contact Scraper

-- Copyright (C) 2014 Jonathan Lamothe <jonathan@jlamothe.net>

-- This program is free software: you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see
-- <http://www.gnu.org/licenses/>.

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
