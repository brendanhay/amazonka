-- Module      : Network.AWS.RDS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Amazon Relational Database Service
--
-- Amazon Relational Database Service (Amazon RDS) is a web service that
-- makes it easier to set up, operate, and scale a relational database in
-- the cloud. It provides cost-efficient, resizable capacity for an
-- industry-standard relational database and manages common database
-- administration tasks, freeing up developers to focus on what makes their
-- applications and businesses unique.
--
-- Amazon RDS gives you access to the capabilities of a MySQL, PostgreSQL,
-- Microsoft SQL Server, Oracle, or Aurora database server. This means the
-- code, applications, and tools you already use today with your existing
-- databases work with Amazon RDS without modification. Amazon RDS
-- automatically backs up your database and maintains the database software
-- that powers your DB instance. Amazon RDS is flexible: you can scale your
-- database instance\'s compute resources and storage capacity to meet your
-- application\'s demand. As with all Amazon Web Services, there are no
-- up-front investments, and you pay only for the resources you use.
--
-- This is an interface reference for Amazon RDS. It contains documentation
-- for a programming or command line interface you can use to manage Amazon
-- RDS. Note that Amazon RDS is asynchronous, which means that some
-- interfaces may require techniques such as polling or callback functions
-- to determine when a command has been applied. In this reference, the
-- parameter descriptions indicate whether a command is applied
-- immediately, on the next instance reboot, or during the maintenance
-- window. For a summary of the Amazon RDS interfaces, go to
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Welcome.html#Welcome.Interfaces Available RDS Interfaces>.
module Network.AWS.RDS
    ( module Export
    ) where

import           Network.AWS.RDS.AddSourceIdentifierToSubscription      as Export
import           Network.AWS.RDS.AddTagsToResource                      as Export
import           Network.AWS.RDS.ApplyPendingMaintenanceAction          as Export
import           Network.AWS.RDS.AuthorizeDBSecurityGroupIngress        as Export
import           Network.AWS.RDS.CopyDBParameterGroup                   as Export
import           Network.AWS.RDS.CopyDBSnapshot                         as Export
import           Network.AWS.RDS.CopyOptionGroup                        as Export
import           Network.AWS.RDS.CreateDBInstance                       as Export
import           Network.AWS.RDS.CreateDBInstanceReadReplica            as Export
import           Network.AWS.RDS.CreateDBParameterGroup                 as Export
import           Network.AWS.RDS.CreateDBSecurityGroup                  as Export
import           Network.AWS.RDS.CreateDBSnapshot                       as Export
import           Network.AWS.RDS.CreateDBSubnetGroup                    as Export
import           Network.AWS.RDS.CreateEventSubscription                as Export
import           Network.AWS.RDS.CreateOptionGroup                      as Export
import           Network.AWS.RDS.DeleteDBInstance                       as Export
import           Network.AWS.RDS.DeleteDBParameterGroup                 as Export
import           Network.AWS.RDS.DeleteDBSecurityGroup                  as Export
import           Network.AWS.RDS.DeleteDBSnapshot                       as Export
import           Network.AWS.RDS.DeleteDBSubnetGroup                    as Export
import           Network.AWS.RDS.DeleteEventSubscription                as Export
import           Network.AWS.RDS.DeleteOptionGroup                      as Export
import           Network.AWS.RDS.DescribeAccountAttributes              as Export
import           Network.AWS.RDS.DescribeCertificates                   as Export
import           Network.AWS.RDS.DescribeDBEngineVersions               as Export
import           Network.AWS.RDS.DescribeDBInstances                    as Export
import           Network.AWS.RDS.DescribeDBLogFiles                     as Export
import           Network.AWS.RDS.DescribeDBParameterGroups              as Export
import           Network.AWS.RDS.DescribeDBParameters                   as Export
import           Network.AWS.RDS.DescribeDBSecurityGroups               as Export
import           Network.AWS.RDS.DescribeDBSnapshots                    as Export
import           Network.AWS.RDS.DescribeDBSubnetGroups                 as Export
import           Network.AWS.RDS.DescribeEngineDefaultParameters        as Export
import           Network.AWS.RDS.DescribeEventCategories                as Export
import           Network.AWS.RDS.DescribeEvents                         as Export
import           Network.AWS.RDS.DescribeEventSubscriptions             as Export
import           Network.AWS.RDS.DescribeOptionGroupOptions             as Export
import           Network.AWS.RDS.DescribeOptionGroups                   as Export
import           Network.AWS.RDS.DescribeOrderableDBInstanceOptions     as Export
import           Network.AWS.RDS.DescribePendingMaintenanceActions      as Export
import           Network.AWS.RDS.DescribeReservedDBInstances            as Export
import           Network.AWS.RDS.DescribeReservedDBInstancesOfferings   as Export
import           Network.AWS.RDS.DownloadDBLogFilePortion               as Export
import           Network.AWS.RDS.ListTagsForResource                    as Export
import           Network.AWS.RDS.ModifyDBInstance                       as Export
import           Network.AWS.RDS.ModifyDBParameterGroup                 as Export
import           Network.AWS.RDS.ModifyDBSubnetGroup                    as Export
import           Network.AWS.RDS.ModifyEventSubscription                as Export
import           Network.AWS.RDS.ModifyOptionGroup                      as Export
import           Network.AWS.RDS.PromoteReadReplica                     as Export
import           Network.AWS.RDS.PurchaseReservedDBInstancesOffering    as Export
import           Network.AWS.RDS.RebootDBInstance                       as Export
import           Network.AWS.RDS.RemoveSourceIdentifierFromSubscription as Export
import           Network.AWS.RDS.RemoveTagsFromResource                 as Export
import           Network.AWS.RDS.ResetDBParameterGroup                  as Export
import           Network.AWS.RDS.RestoreDBInstanceFromDBSnapshot        as Export
import           Network.AWS.RDS.RestoreDBInstanceToPointInTime         as Export
import           Network.AWS.RDS.RevokeDBSecurityGroupIngress           as Export
import           Network.AWS.RDS.Types                                  as Export
import           Network.AWS.RDS.Waiters                                as Export
