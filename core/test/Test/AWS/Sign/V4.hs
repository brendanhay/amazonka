{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- |
-- Module      : Test.AWS.Sign.V$
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Sign.V4 (tests) where

import qualified Data.ByteString.Char8    as BS8
import qualified Data.Foldable            as Fold
import           Data.List                (sort)
import           Data.Monoid
import           Data.String
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import           Network.AWS.Prelude
import           Network.AWS.Sign.V4
import           Test.AWS.Arbitrary       ()
import qualified Test.AWS.Sign.V4.Chunked as Chunked
import           Test.QuickCheck.Property
import           Test.Tasty
import           Test.Tasty.QuickCheck

-- Write some V4 signing properties:
--  test canonical query
--  test query
--  test canonical headers
--  test signed headers
--  test time format
--  test host, x-amz-date, content-sha256 headers exist
--  test empty path
--  test empty query

tests :: TestTree
tests = testGroup "v4"
            [ testGroup "chunked"
                [ Chunked.tests
                ]
            ]
