{-# LANGUAGE OverloadedStrings #-}

-- Module      : Test.AWS.RDS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Test.AWS.RDS
    ( tests
    , fixtures
    ) where

import           Control.Lens
import           Network.AWS.RDS
import           Test.AWS.Gen.RDS
import           Test.Tasty

tests :: [TestTree]
tests = []

fixtures :: [TestTree]
fixtures =
    [ testGroup "response"
        [ testDescribeDBInstancesResponse $ describeDBInstancesResponse 200
            & desDBInstances .~
                [ dbInstance
                    & diBackupRetentionPeriod      ?~ 7
                    & diMultiAZ                    ?~ False
                    & diDBInstanceStatus           ?~ "available"
                    & diDBInstanceIdentifier       ?~ "mysqlexampledb"
                    & diPreferredBackupWindow      ?~ "10:07-10:37"
                    & diPreferredMaintenanceWindow ?~ "sun:06:13-sun:06:43"
                    & diAvailabilityZone           ?~ "us-west-2b"
--                    & diLatestRestorableTime       ?~ $(mkTime "2014-04-21T17:15:00Z")
                    & diEngine                     ?~ "mysql"
                    & diLicenseModel               ?~ "general-public-license"
                ]
        ]
    ]

-- diff :: (Eq a, Show a) => a -> a -> Assertion
-- diff e a = unless (e == a) (diff e a >> assertFailure "Non-equal.")
--  where
--    m = "diff: "
    -- msg = "expected: " ++ show e
    --  ++ "\n but got: " ++ show a
