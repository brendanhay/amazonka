{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Main (main) where

import Test.Amazonka.RobOMaker
import Test.Amazonka.RobOMaker.Internal
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "RobOMaker"
      [ testGroup "tests" tests,
        testGroup "fixtures" fixtures
      ]
