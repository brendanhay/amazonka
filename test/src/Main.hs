-- Module      : Main
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Main (main) where

import           Data.Proxy
import qualified Test.AWS.Fixture as Fixture
import qualified Test.AWS.SQS     as SQS
import           Test.Tasty

-- NOTE:
-- Eventually I'll render separate test projects, to make testing more
-- granular with a single amazonka/test amazonka-test library for common shit.
--
-- For now though, it's simpler while actually adding the tests to just
-- generate everything in one project and go through the failing cases
-- one by one.

main :: IO ()
main = defaultMain $ testGroup "amazonka"
    [ testGroup "tests"
        [ SQS.tests
        ]

    , testGroup "fixtures"
        [ SQS.fixtures
        ]
    ]
