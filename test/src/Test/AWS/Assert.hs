-- |
-- Module      : Test.AWS.Assert
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Assert where

import           Control.Monad
import           Test.AWS.Orphans
import           Test.Tasty
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Text.PrettyPrint
import           Text.PrettyPrint.GenericPretty

assertEqualPP :: (Eq a, Out a) => String -> a -> Either String a -> Assertion
assertEqualPP _ _ (Left  m) = assertFailure m
assertEqualPP n e (Right a) = unless (e == a) (assertFailure msg)
  where
    msg = "[Expected]:\n"    ++ prettyStyle s e
     ++ "\n[" ++ n ++ "]:\n" ++ prettyStyle s a

    s = Style
        { mode           = PageMode
        , lineLength     = 80
        , ribbonsPerLine = 1.5
        }
