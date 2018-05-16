{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Test.AWS.ApplicationAutoScaling
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Test.AWS.ApplicationAutoScaling
    ( tests
    , fixtures
    ) where

import           Network.AWS.ApplicationAutoScaling
import           Test.AWS.Gen.ApplicationAutoScaling
import           Test.Tasty

tests :: [TestTree]
tests = []

fixtures :: [TestTree]
fixtures = []
