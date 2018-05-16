-- |
-- Module      : Test.AWS.Assert
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Assert where

import           Control.Monad
import           Test.AWS.Diff
import           Test.AWS.Orphans ()
import           Test.Tasty.HUnit

assertDiff :: (Eq a, Show a) => String -> a -> Either String a -> Assertion
assertDiff _ _ (Left  m) = assertFailure m
assertDiff n e (Right a) = unless (e == a) (msg >>= assertFailure)
  where
    msg = do
        d <- diff e a
        return $! "[Expected]:\n"      ++ show e
               ++ "\n[" ++ n ++ "]:\n" ++ show a
               ++ "\n"
               ++ d
