{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Main (main) where

import           Test.AWS.S3
import           Test.AWS.S3.Internal
import           Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "S3"
    [ testGroup "tests"    tests
    , testGroup "fixtures" fixtures
    ]
