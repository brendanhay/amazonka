-- Module      : Network.AWS.RDS
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Amazon Relational Database Service (Amazon RDS) is a web service that makes
-- it easy to set up, operate, and scale a relational database in the cloud. It
-- provides cost-efficient and resizable capacity while managing time-consuming
-- database administration tasks, freeing you up to focus on your applications
-- and business.
module Network.AWS.RDS
    ( module Network.AWS.RDS.AddSourceIdentifierToSubscription
    , module Network.AWS.RDS.AddTagsToResource
    , module Network.AWS.RDS.ApplyPendingMaintenanceAction
    , module Network.AWS.RDS.AuthorizeDBSecurityGroupIngress
    , module Network.AWS.RDS.CopyDBParameterGroup
    , module Network.AWS.RDS.CopyDBSnapshot
    , module Network.AWS.RDS.CopyOptionGroup
    , module Network.AWS.RDS.CreateDBInstance
    , module Network.AWS.RDS.CreateDBInstanceReadReplica
    , module Network.AWS.RDS.CreateDBParameterGroup
    , module Network.AWS.RDS.CreateDBSecurityGroup
    , module Network.AWS.RDS.CreateDBSnapshot
    , module Network.AWS.RDS.CreateDBSubnetGroup
    , module Network.AWS.RDS.CreateEventSubscription
    , module Network.AWS.RDS.CreateOptionGroup
    , module Network.AWS.RDS.DeleteDBInstance
    , module Network.AWS.RDS.DeleteDBParameterGroup
    , module Network.AWS.RDS.DeleteDBSecurityGroup
    , module Network.AWS.RDS.DeleteDBSnapshot
    , module Network.AWS.RDS.DeleteDBSubnetGroup
    , module Network.AWS.RDS.DeleteEventSubscription
    , module Network.AWS.RDS.DeleteOptionGroup
    , module Network.AWS.RDS.DescribeAccountAttributes
    , module Network.AWS.RDS.DescribeCertificates
    , module Network.AWS.RDS.DescribeDBEngineVersions
    , module Network.AWS.RDS.DescribeDBInstances
    , module Network.AWS.RDS.DescribeDBLogFiles
    , module Network.AWS.RDS.DescribeDBParameterGroups
    , module Network.AWS.RDS.DescribeDBParameters
    , module Network.AWS.RDS.DescribeDBSecurityGroups
    , module Network.AWS.RDS.DescribeDBSnapshots
    , module Network.AWS.RDS.DescribeDBSubnetGroups
    , module Network.AWS.RDS.DescribeEngineDefaultParameters
    , module Network.AWS.RDS.DescribeEventCategories
    , module Network.AWS.RDS.DescribeEventSubscriptions
    , module Network.AWS.RDS.DescribeEvents
    , module Network.AWS.RDS.DescribeOptionGroupOptions
    , module Network.AWS.RDS.DescribeOptionGroups
    , module Network.AWS.RDS.DescribeOrderableDBInstanceOptions
    , module Network.AWS.RDS.DescribePendingMaintenanceActions
    , module Network.AWS.RDS.DescribeReservedDBInstances
    , module Network.AWS.RDS.DescribeReservedDBInstancesOfferings
    , module Network.AWS.RDS.DownloadDBLogFilePortion
    , module Network.AWS.RDS.ListTagsForResource
    , module Network.AWS.RDS.ModifyDBInstance
    , module Network.AWS.RDS.ModifyDBParameterGroup
    , module Network.AWS.RDS.ModifyDBSubnetGroup
    , module Network.AWS.RDS.ModifyEventSubscription
    , module Network.AWS.RDS.ModifyOptionGroup
    , module Network.AWS.RDS.PromoteReadReplica
    , module Network.AWS.RDS.PurchaseReservedDBInstancesOffering
    , module Network.AWS.RDS.RebootDBInstance
    , module Network.AWS.RDS.RemoveSourceIdentifierFromSubscription
    , module Network.AWS.RDS.RemoveTagsFromResource
    , module Network.AWS.RDS.ResetDBParameterGroup
    , module Network.AWS.RDS.RestoreDBInstanceFromDBSnapshot
    , module Network.AWS.RDS.RestoreDBInstanceToPointInTime
    , module Network.AWS.RDS.RevokeDBSecurityGroupIngress
    , module Network.AWS.RDS.Types
    , module Network.AWS.RDS.Waiters
    ) where

import Network.AWS.RDS.AddSourceIdentifierToSubscription
import Network.AWS.RDS.AddTagsToResource
import Network.AWS.RDS.ApplyPendingMaintenanceAction
import Network.AWS.RDS.AuthorizeDBSecurityGroupIngress
import Network.AWS.RDS.CopyDBParameterGroup
import Network.AWS.RDS.CopyDBSnapshot
import Network.AWS.RDS.CopyOptionGroup
import Network.AWS.RDS.CreateDBInstance
import Network.AWS.RDS.CreateDBInstanceReadReplica
import Network.AWS.RDS.CreateDBParameterGroup
import Network.AWS.RDS.CreateDBSecurityGroup
import Network.AWS.RDS.CreateDBSnapshot
import Network.AWS.RDS.CreateDBSubnetGroup
import Network.AWS.RDS.CreateEventSubscription
import Network.AWS.RDS.CreateOptionGroup
import Network.AWS.RDS.DeleteDBInstance
import Network.AWS.RDS.DeleteDBParameterGroup
import Network.AWS.RDS.DeleteDBSecurityGroup
import Network.AWS.RDS.DeleteDBSnapshot
import Network.AWS.RDS.DeleteDBSubnetGroup
import Network.AWS.RDS.DeleteEventSubscription
import Network.AWS.RDS.DeleteOptionGroup
import Network.AWS.RDS.DescribeAccountAttributes
import Network.AWS.RDS.DescribeCertificates
import Network.AWS.RDS.DescribeDBEngineVersions
import Network.AWS.RDS.DescribeDBInstances
import Network.AWS.RDS.DescribeDBLogFiles
import Network.AWS.RDS.DescribeDBParameterGroups
import Network.AWS.RDS.DescribeDBParameters
import Network.AWS.RDS.DescribeDBSecurityGroups
import Network.AWS.RDS.DescribeDBSnapshots
import Network.AWS.RDS.DescribeDBSubnetGroups
import Network.AWS.RDS.DescribeEngineDefaultParameters
import Network.AWS.RDS.DescribeEventCategories
import Network.AWS.RDS.DescribeEventSubscriptions
import Network.AWS.RDS.DescribeEvents
import Network.AWS.RDS.DescribeOptionGroupOptions
import Network.AWS.RDS.DescribeOptionGroups
import Network.AWS.RDS.DescribeOrderableDBInstanceOptions
import Network.AWS.RDS.DescribePendingMaintenanceActions
import Network.AWS.RDS.DescribeReservedDBInstances
import Network.AWS.RDS.DescribeReservedDBInstancesOfferings
import Network.AWS.RDS.DownloadDBLogFilePortion
import Network.AWS.RDS.ListTagsForResource
import Network.AWS.RDS.ModifyDBInstance
import Network.AWS.RDS.ModifyDBParameterGroup
import Network.AWS.RDS.ModifyDBSubnetGroup
import Network.AWS.RDS.ModifyEventSubscription
import Network.AWS.RDS.ModifyOptionGroup
import Network.AWS.RDS.PromoteReadReplica
import Network.AWS.RDS.PurchaseReservedDBInstancesOffering
import Network.AWS.RDS.RebootDBInstance
import Network.AWS.RDS.RemoveSourceIdentifierFromSubscription
import Network.AWS.RDS.RemoveTagsFromResource
import Network.AWS.RDS.ResetDBParameterGroup
import Network.AWS.RDS.RestoreDBInstanceFromDBSnapshot
import Network.AWS.RDS.RestoreDBInstanceToPointInTime
import Network.AWS.RDS.RevokeDBSecurityGroupIngress
import Network.AWS.RDS.Types
import Network.AWS.RDS.Waiters
