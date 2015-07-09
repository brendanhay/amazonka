{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

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
import           Data.Time
import           Network.AWS.Prelude
import           Network.AWS.RDS
import           Test.AWS.Gen.RDS
import           Test.AWS.Prelude
import           Test.Tasty

tests :: [TestTree]
tests = []

fixtures :: [TestTree]
fixtures =
    [ testGroup "response"
        [ testDescribeDBInstancesResponse $ describeDBInstancesResponse 200
            & desDBInstances .~
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
                            & endAddress ?~ "mysqlexampledb.c6c1rntzufv0.us-west-2.rds.amazonaws.com"
                            & endPort    ?~ 3306)
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

-- _diDBSecurityGroups = Just [DBSecurityGroupMembership' {_dsgmStatus = Just "active", _dsgmDBSecurityGroupName = Just "my-db-secgroup"}],
-- _diOptionGroupMemberships = Just [OptionGroupMembership' {_ogmStatus = Just "in-sync",_ogmOptionGroupName = Just "default:mysql-5-6"}],
-- _diEndpoint = Just Endpoint' {_endAddress = Just "mysqlexampledb.c6c1rntzufv0.us-west-2.rds.amazonaws.com", _endPort = Just 3306},
-- _diDBParameterGroups = Just [DBParameterGroupStatus' {_dpgsDBParameterGroupName = Just "default.mysql5.6", _dpgsParameterApplyStatus = Just "in-sync"}],
-- },

-- DBInstance' {_diDBSecurityGroups = Just [DBSecurityGroupMembership' {_dsgmStatus = Just "active",
-- _dsgmDBSecurityGroupName = Just "default"}],
-- _diEngineVersion = Just "5.6.13",
-- _diAutoMinorVersionUpgrade = Just True,
-- _diMasterUsername = Just "myawsuser",
-- _diPubliclyAccessible = Just True,
-- _diInstanceCreateTime = Just Time "2014-03-28 20:14:17.296 UTC",
-- _diEngine = Just "mysql",
-- _diLatestRestorableTime = Just Time "2014-04-21 17:15:00 UTC",
-- _diDBInstanceClass = Just "db.t1.micro",
-- _diLicenseModel = Just "general-public-license",
-- _diPreferredMaintenanceWindow = Just "sun:06:13-sun:06:43",
-- _diDBInstanceIdentifier = Just "mysqlexampledb-restore",
-- _diPreferredBackupWindow = Just "10:07-10:37",
-- _diAvailabilityZone = Just "us-west-2b",
-- _diBackupRetentionPeriod = Just 7,
-- _diMultiAZ = Just False,
-- _diOptionGroupMemberships = Just [OptionGroupMembership' {_ogmStatus = Just "in-sync",
-- _ogmOptionGroupName = Just "default:mysql-5-6"}],
-- _diAllocatedStorage = Just 5,
-- _diEndpoint = Just Endpoint' {_endAddress = Just "mysqlexampledb-restore.c6c2mntzugv0.us-west-2.rds.amazonaws.com",
-- _endPort = Just 3306},
-- _diDBParameterGroups = Just [DBParameterGroupStatus' {_dpgsDBParameterGroupName = Just "default.mysql5.6",
-- _dpgsParameterApplyStatus = Just "in-sync"}],
-- _diDBInstanceStatus = Just "available",
-- _diDBName = Just "mysampledb"}]
