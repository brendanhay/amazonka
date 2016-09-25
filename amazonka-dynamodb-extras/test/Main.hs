-- |
-- Module      : Main
-- Copyright   : (c) 2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Main where

import Test.DocTest (doctest)

main :: IO ()
main = doctest
    ["-isrc"
    , "-XOverloadedStrings"
    , "src/Amazonka/DynamoDB/Expression/Compile.hs"
    , "src/Amazonka/DynamoDB/Expression/Condition.hs"
    , "src/Amazonka/DynamoDB/Expression/Update.hs"
    , "src/Amazonka/DynamoDB/Expression/Projection.hs"
    ]
