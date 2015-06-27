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

import qualified Test.AWS.Data.Numeric      as Numeric
import qualified Test.AWS.Data.Time         as Time
import qualified Test.AWS.Protocol.EC2      as EC2
import qualified Test.AWS.Protocol.JSON     as JSON
import qualified Test.AWS.Protocol.Query    as Query
import qualified Test.AWS.Protocol.RestJSON as RestJSON
import qualified Test.AWS.Protocol.RestXML  as RestXML
import           Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "amazonka"
    [ testGroup "primitives"
        [ Numeric.tests
        , Time.tests
        ]

    , testGroup "protocols"
        [ EC2.tests
        , JSON.tests
        , Query.tests
        , RestJSON.tests
        , RestXML.tests
        ]
    ]
