{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Main (main) where

import Test.Amazonka.GamesParks
import Test.Amazonka.GamesParks.Internal
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "GamesParks"
      [ testGroup "tests" tests,
        testGroup "fixtures" fixtures
      ]
