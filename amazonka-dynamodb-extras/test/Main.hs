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
    [ "-isrc"
    , "Network.AWS.DynamoDB.Expression"
    ]
