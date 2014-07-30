{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon Relational Database Service (Amazon RDS) is a web service that makes
-- it easy to set up, operate, and scale a relational database in the cloud.
-- It provides cost-efficient and resizable capacity while managing
-- time-consuming database administration tasks, freeing you up to focus on
-- your applications and business.
module Network.AWS.RDS.V2013_09_09
    ( module Network.AWS.RDS.V2013_09_09.AddSourceIdentifierToSubscription
    , module Network.AWS.RDS.V2013_09_09.AddTagsToResource
    , module Network.AWS.RDS.V2013_09_09.AuthorizeDBSecurityGroupIngress
    , module Network.AWS.RDS.V2013_09_09.CopyDBSnapshot
    , module Network.AWS.RDS.V2013_09_09.CreateDBInstance
    , module Network.AWS.RDS.V2013_09_09.CreateDBInstanceReadReplica
    , module Network.AWS.RDS.V2013_09_09.CreateDBParameterGroup
    , module Network.AWS.RDS.V2013_09_09.CreateDBSecurityGroup
    , module Network.AWS.RDS.V2013_09_09.CreateDBSnapshot
    , module Network.AWS.RDS.V2013_09_09.CreateDBSubnetGroup
    , module Network.AWS.RDS.V2013_09_09.CreateEventSubscription
    , module Network.AWS.RDS.V2013_09_09.CreateOptionGroup
    , module Network.AWS.RDS.V2013_09_09.DeleteDBInstance
    , module Network.AWS.RDS.V2013_09_09.DeleteDBParameterGroup
    , module Network.AWS.RDS.V2013_09_09.DeleteDBSecurityGroup
    , module Network.AWS.RDS.V2013_09_09.DeleteDBSnapshot
    , module Network.AWS.RDS.V2013_09_09.DeleteDBSubnetGroup
    , module Network.AWS.RDS.V2013_09_09.DeleteEventSubscription
    , module Network.AWS.RDS.V2013_09_09.DeleteOptionGroup
    , module Network.AWS.RDS.V2013_09_09.DescribeDBEngineVersions
    , module Network.AWS.RDS.V2013_09_09.DescribeDBInstances
    , module Network.AWS.RDS.V2013_09_09.DescribeDBLogFiles
    , module Network.AWS.RDS.V2013_09_09.DescribeDBParameterGroups
    , module Network.AWS.RDS.V2013_09_09.DescribeDBParameters
    , module Network.AWS.RDS.V2013_09_09.DescribeDBSecurityGroups
    , module Network.AWS.RDS.V2013_09_09.DescribeDBSnapshots
    , module Network.AWS.RDS.V2013_09_09.DescribeDBSubnetGroups
    , module Network.AWS.RDS.V2013_09_09.DescribeEngineDefaultParameters
    , module Network.AWS.RDS.V2013_09_09.DescribeEventCategories
    , module Network.AWS.RDS.V2013_09_09.DescribeEventSubscriptions
    , module Network.AWS.RDS.V2013_09_09.DescribeEvents
    , module Network.AWS.RDS.V2013_09_09.DescribeOptionGroupOptions
    , module Network.AWS.RDS.V2013_09_09.DescribeOptionGroups
    , module Network.AWS.RDS.V2013_09_09.DescribeOrderableDBInstanceOptions
    , module Network.AWS.RDS.V2013_09_09.DescribeReservedDBInstances
    , module Network.AWS.RDS.V2013_09_09.DescribeReservedDBInstancesOfferings
    , module Network.AWS.RDS.V2013_09_09.DownloadDBLogFilePortion
    , module Network.AWS.RDS.V2013_09_09.Lenses
    , module Network.AWS.RDS.V2013_09_09.ListTagsForResource
    , module Network.AWS.RDS.V2013_09_09.ModifyDBInstance
    , module Network.AWS.RDS.V2013_09_09.ModifyDBParameterGroup
    , module Network.AWS.RDS.V2013_09_09.ModifyDBSubnetGroup
    , module Network.AWS.RDS.V2013_09_09.ModifyEventSubscription
    , module Network.AWS.RDS.V2013_09_09.ModifyOptionGroup
    , module Network.AWS.RDS.V2013_09_09.PromoteReadReplica
    , module Network.AWS.RDS.V2013_09_09.PurchaseReservedDBInstancesOffering
    , module Network.AWS.RDS.V2013_09_09.RebootDBInstance
    , module Network.AWS.RDS.V2013_09_09.RemoveSourceIdentifierFromSubscription
    , module Network.AWS.RDS.V2013_09_09.RemoveTagsFromResource
    , module Network.AWS.RDS.V2013_09_09.ResetDBParameterGroup
    , module Network.AWS.RDS.V2013_09_09.RestoreDBInstanceFromDBSnapshot
    , module Network.AWS.RDS.V2013_09_09.RestoreDBInstanceToPointInTime
    , module Network.AWS.RDS.V2013_09_09.RevokeDBSecurityGroupIngress
    , module Network.AWS.RDS.V2013_09_09.Types
    ) where

import Network.AWS.RDS.V2013_09_09.AddSourceIdentifierToSubscription
import Network.AWS.RDS.V2013_09_09.AddTagsToResource
import Network.AWS.RDS.V2013_09_09.AuthorizeDBSecurityGroupIngress
import Network.AWS.RDS.V2013_09_09.CopyDBSnapshot
import Network.AWS.RDS.V2013_09_09.CreateDBInstance
import Network.AWS.RDS.V2013_09_09.CreateDBInstanceReadReplica
import Network.AWS.RDS.V2013_09_09.CreateDBParameterGroup
import Network.AWS.RDS.V2013_09_09.CreateDBSecurityGroup
import Network.AWS.RDS.V2013_09_09.CreateDBSnapshot
import Network.AWS.RDS.V2013_09_09.CreateDBSubnetGroup
import Network.AWS.RDS.V2013_09_09.CreateEventSubscription
import Network.AWS.RDS.V2013_09_09.CreateOptionGroup
import Network.AWS.RDS.V2013_09_09.DeleteDBInstance
import Network.AWS.RDS.V2013_09_09.DeleteDBParameterGroup
import Network.AWS.RDS.V2013_09_09.DeleteDBSecurityGroup
import Network.AWS.RDS.V2013_09_09.DeleteDBSnapshot
import Network.AWS.RDS.V2013_09_09.DeleteDBSubnetGroup
import Network.AWS.RDS.V2013_09_09.DeleteEventSubscription
import Network.AWS.RDS.V2013_09_09.DeleteOptionGroup
import Network.AWS.RDS.V2013_09_09.DescribeDBEngineVersions
import Network.AWS.RDS.V2013_09_09.DescribeDBInstances
import Network.AWS.RDS.V2013_09_09.DescribeDBLogFiles
import Network.AWS.RDS.V2013_09_09.DescribeDBParameterGroups
import Network.AWS.RDS.V2013_09_09.DescribeDBParameters
import Network.AWS.RDS.V2013_09_09.DescribeDBSecurityGroups
import Network.AWS.RDS.V2013_09_09.DescribeDBSnapshots
import Network.AWS.RDS.V2013_09_09.DescribeDBSubnetGroups
import Network.AWS.RDS.V2013_09_09.DescribeEngineDefaultParameters
import Network.AWS.RDS.V2013_09_09.DescribeEventCategories
import Network.AWS.RDS.V2013_09_09.DescribeEventSubscriptions
import Network.AWS.RDS.V2013_09_09.DescribeEvents
import Network.AWS.RDS.V2013_09_09.DescribeOptionGroupOptions
import Network.AWS.RDS.V2013_09_09.DescribeOptionGroups
import Network.AWS.RDS.V2013_09_09.DescribeOrderableDBInstanceOptions
import Network.AWS.RDS.V2013_09_09.DescribeReservedDBInstances
import Network.AWS.RDS.V2013_09_09.DescribeReservedDBInstancesOfferings
import Network.AWS.RDS.V2013_09_09.DownloadDBLogFilePortion
import Network.AWS.RDS.V2013_09_09.Lenses
import Network.AWS.RDS.V2013_09_09.ListTagsForResource
import Network.AWS.RDS.V2013_09_09.ModifyDBInstance
import Network.AWS.RDS.V2013_09_09.ModifyDBParameterGroup
import Network.AWS.RDS.V2013_09_09.ModifyDBSubnetGroup
import Network.AWS.RDS.V2013_09_09.ModifyEventSubscription
import Network.AWS.RDS.V2013_09_09.ModifyOptionGroup
import Network.AWS.RDS.V2013_09_09.PromoteReadReplica
import Network.AWS.RDS.V2013_09_09.PurchaseReservedDBInstancesOffering
import Network.AWS.RDS.V2013_09_09.RebootDBInstance
import Network.AWS.RDS.V2013_09_09.RemoveSourceIdentifierFromSubscription
import Network.AWS.RDS.V2013_09_09.RemoveTagsFromResource
import Network.AWS.RDS.V2013_09_09.ResetDBParameterGroup
import Network.AWS.RDS.V2013_09_09.RestoreDBInstanceFromDBSnapshot
import Network.AWS.RDS.V2013_09_09.RestoreDBInstanceToPointInTime
import Network.AWS.RDS.V2013_09_09.RevokeDBSecurityGroupIngress
import Network.AWS.RDS.V2013_09_09.Types
