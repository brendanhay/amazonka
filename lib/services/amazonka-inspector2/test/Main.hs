{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Main (main) where

import Test.Amazonka.Inspector2
import Test.Amazonka.Inspector2.Internal
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "Inspector2"
      [ testGroup "tests" tests,
        testGroup "fixtures" fixtures
      ]
