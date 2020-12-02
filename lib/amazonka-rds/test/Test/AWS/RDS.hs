{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Test.AWS.RDS
-- Copyright   : (c) 2013-2018 Brendan Hay
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

import           Data.Time
import           Network.AWS.Lens    ((&), (.~), (?~))
import           Network.AWS.Prelude
import           Network.AWS.RDS
import           Test.AWS.Gen.RDS
import           Test.AWS.Prelude

tests :: [TestTree]
tests = []

fixtures :: [TestTree]
fixtures =
    [ testGroup "response"
        [ responseDescribeDBInstances $ describeDBInstancesResponse 200
            & ddbirsDBInstances .~
                [ dbInstance
                    & diAllocatedStorage           ?~ 5
                    & diAutoMinorVersionUpgrade    ?~ True
                    & diAvailabilityZone           ?~ "us-west-2b"
                    & diBackupRetentionPeriod      ?~ 7
                    & diDBInstanceClass            ?~ "db.t1.micro"
                    & diDBInstanceIdentifier       ?~ "mysqlexampledb"
                    & diDBInstanceStatus           ?~ "available"
                    & diDBName                     ?~ "mysampledb"
                    & diEngine                     ?~ "mysql"
                    & diEngineVersion              ?~ "5.6.13"
                    & diInstanceCreateTime         ?~ $(mkTime "2014-01-29T22:58:24.231Z")
                    & diLatestRestorableTime       ?~ $(mkTime "2014-04-21T17:15:00Z")
                    & diLicenseModel               ?~ "general-public-license"
                    & diMasterUsername             ?~ "myawsuser"
                    & diMultiAZ                    ?~ False
                    & diPreferredBackupWindow      ?~ "10:07-10:37"
                    & diPreferredMaintenanceWindow ?~ "sun:06:13-sun:06:43"
                    & diPubliclyAccessible         ?~ True
                    & diEndpoint                   ?~
                        (endpoint
                            & eAddress ?~ "mysqlexampledb.c6c1rntzufv0.us-west-2.rds.amazonaws.com"
                            & ePort    ?~ 3306)
                    & diDBSecurityGroups .~
                        [ dbSecurityGroupMembership
                            & dsgmStatus              ?~ "active"
                            & dsgmDBSecurityGroupName ?~ "my-db-secgroup"
                        ]
                    & diDBParameterGroups .~
                        [ dbParameterGroupStatus
                            & dpgsDBParameterGroupName ?~ "default.mysql5.6"
                            & dpgsParameterApplyStatus ?~ "in-sync"
                        ]
                    & diOptionGroupMemberships .~
                        [ optionGroupMembership
                            & ogmStatus          ?~ "in-sync"
                            & ogmOptionGroupName ?~ "default:mysql-5-6"
                        ]
                ]
        ]
    ]
