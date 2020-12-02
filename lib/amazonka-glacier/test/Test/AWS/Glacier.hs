{-# LANGUAGE OverloadedStrings #-}

-- Module      : Test.AWS.Glacier
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Test.AWS.Glacier
    ( tests
    , fixtures
    ) where

import           Network.AWS.Glacier
import           Test.AWS.Gen.Glacier
import           Test.Tasty

tests :: [TestTree]
tests = []

fixtures :: [TestTree]
fixtures = []
