-- Module      : Test.AWS.Data
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Test.AWS.Data (tests) where

import qualified Test.AWS.Data.List    as List
import qualified Test.AWS.Data.Map     as Map
import qualified Test.AWS.Data.Numeric as Numeric
import qualified Test.AWS.Data.Time    as Time
import           Test.Tasty

tests :: TestTree
tests = testGroup "data types"
    [ List.tests
    , Map.tests
    , Numeric.tests
    , Time.tests
    ]
