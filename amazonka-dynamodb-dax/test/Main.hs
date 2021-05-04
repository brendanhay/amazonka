{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Main (main) where

import Test.AWS.DAX
import Test.AWS.DAX.Internal
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "DAX"
      [ testGroup "tests" tests,
        testGroup "fixtures" fixtures
      ]
