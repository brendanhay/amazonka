{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- Module      : Network.AWS.RDS.V2013_09_09.Monadic
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
--
-- The 'State' operator variants from "Control.Lens.Setter" such as '.='
-- can be used to modify any additional request parameters before sending.
module Network.AWS.RDS.V2013_09_09.Monadic
    (
    -- * DownloadDBLogFilePortion
    -- $DownloadDBLogFilePortion
      downloadDBLogFilePortion
    , downloadDBLogFilePortionCatch

    -- * AddSourceIdentifierToSubscription
    -- $AddSourceIdentifierToSubscription
    , addSourceIdentifierToSubscription
    , addSourceIdentifierToSubscriptionCatch

    -- * AddTagsToResource
    -- $AddTagsToResource
    , addTagsToResource
    , addTagsToResourceCatch

    -- * AuthorizeDBSecurityGroupIngress
    -- $AuthorizeDBSecurityGroupIngress
    , authorizeDBSecurityGroupIngress
    , authorizeDBSecurityGroupIngressCatch

    -- * CopyDBSnapshot
    -- $CopyDBSnapshot
    , copyDBSnapshot
    , copyDBSnapshotCatch

    -- * CreateDBInstance
    -- $CreateDBInstance
    , createDBInstance
    , createDBInstanceCatch

    -- * CreateDBInstanceReadReplica
    -- $CreateDBInstanceReadReplica
    , createDBInstanceReadReplica
    , createDBInstanceReadReplicaCatch

    -- * CreateDBParameterGroup
    -- $CreateDBParameterGroup
    , createDBParameterGroup
    , createDBParameterGroupCatch

    -- * CreateDBSecurityGroup
    -- $CreateDBSecurityGroup
    , createDBSecurityGroup
    , createDBSecurityGroupCatch

    -- * CreateDBSnapshot
    -- $CreateDBSnapshot
    , createDBSnapshot
    , createDBSnapshotCatch

    -- * CreateDBSubnetGroup
    -- $CreateDBSubnetGroup
    , createDBSubnetGroup
    , createDBSubnetGroupCatch

    -- * CreateEventSubscription
    -- $CreateEventSubscription
    , createEventSubscription
    , createEventSubscriptionCatch

    -- * CreateOptionGroup
    -- $CreateOptionGroup
    , createOptionGroup
    , createOptionGroupCatch

    -- * DeleteDBInstance
    -- $DeleteDBInstance
    , deleteDBInstance
    , deleteDBInstanceCatch

    -- * DeleteDBParameterGroup
    -- $DeleteDBParameterGroup
    , deleteDBParameterGroup
    , deleteDBParameterGroupCatch

    -- * DeleteDBSecurityGroup
    -- $DeleteDBSecurityGroup
    , deleteDBSecurityGroup
    , deleteDBSecurityGroupCatch

    -- * DeleteDBSnapshot
    -- $DeleteDBSnapshot
    , deleteDBSnapshot
    , deleteDBSnapshotCatch

    -- * DeleteDBSubnetGroup
    -- $DeleteDBSubnetGroup
    , deleteDBSubnetGroup
    , deleteDBSubnetGroupCatch

    -- * DeleteEventSubscription
    -- $DeleteEventSubscription
    , deleteEventSubscription
    , deleteEventSubscriptionCatch

    -- * DeleteOptionGroup
    -- $DeleteOptionGroup
    , deleteOptionGroup
    , deleteOptionGroupCatch

    -- * DescribeDBEngineVersions
    -- $DescribeDBEngineVersions
    , describeDBEngineVersions
    , describeDBEngineVersionsCatch

    -- * DescribeDBInstances
    -- $DescribeDBInstances
    , describeDBInstances
    , describeDBInstancesCatch

    -- * DescribeDBLogFiles
    -- $DescribeDBLogFiles
    , describeDBLogFiles
    , describeDBLogFilesCatch

    -- * DescribeDBParameterGroups
    -- $DescribeDBParameterGroups
    , describeDBParameterGroups
    , describeDBParameterGroupsCatch

    -- * DescribeDBParameters
    -- $DescribeDBParameters
    , describeDBParameters
    , describeDBParametersCatch

    -- * DescribeDBSecurityGroups
    -- $DescribeDBSecurityGroups
    , describeDBSecurityGroups
    , describeDBSecurityGroupsCatch

    -- * DescribeDBSnapshots
    -- $DescribeDBSnapshots
    , describeDBSnapshots
    , describeDBSnapshotsCatch

    -- * DescribeDBSubnetGroups
    -- $DescribeDBSubnetGroups
    , describeDBSubnetGroups
    , describeDBSubnetGroupsCatch

    -- * DescribeEngineDefaultParameters
    -- $DescribeEngineDefaultParameters
    , describeEngineDefaultParameters
    , describeEngineDefaultParametersCatch

    -- * DescribeEventCategories
    -- $DescribeEventCategories
    , describeEventCategories
    , describeEventCategoriesCatch

    -- * DescribeEventSubscriptions
    -- $DescribeEventSubscriptions
    , describeEventSubscriptions
    , describeEventSubscriptionsCatch

    -- * DescribeEvents
    -- $DescribeEvents
    , describeEvents
    , describeEventsCatch

    -- * DescribeOptionGroupOptions
    -- $DescribeOptionGroupOptions
    , describeOptionGroupOptions
    , describeOptionGroupOptionsCatch

    -- * DescribeOptionGroups
    -- $DescribeOptionGroups
    , describeOptionGroups
    , describeOptionGroupsCatch

    -- * DescribeOrderableDBInstanceOptions
    -- $DescribeOrderableDBInstanceOptions
    , describeOrderableDBInstanceOptions
    , describeOrderableDBInstanceOptionsCatch

    -- * DescribeReservedDBInstances
    -- $DescribeReservedDBInstances
    , describeReservedDBInstances
    , describeReservedDBInstancesCatch

    -- * DescribeReservedDBInstancesOfferings
    -- $DescribeReservedDBInstancesOfferings
    , describeReservedDBInstancesOfferings
    , describeReservedDBInstancesOfferingsCatch

    -- * ListTagsForResource
    -- $ListTagsForResource
    , listTagsForResource
    , listTagsForResourceCatch

    -- * ModifyDBInstance
    -- $ModifyDBInstance
    , modifyDBInstance
    , modifyDBInstanceCatch

    -- * ModifyDBParameterGroup
    -- $ModifyDBParameterGroup
    , modifyDBParameterGroup
    , modifyDBParameterGroupCatch

    -- * ModifyDBSubnetGroup
    -- $ModifyDBSubnetGroup
    , modifyDBSubnetGroup
    , modifyDBSubnetGroupCatch

    -- * ModifyEventSubscription
    -- $ModifyEventSubscription
    , modifyEventSubscription
    , modifyEventSubscriptionCatch

    -- * ModifyOptionGroup
    -- $ModifyOptionGroup
    , modifyOptionGroup
    , modifyOptionGroupCatch

    -- * PromoteReadReplica
    -- $PromoteReadReplica
    , promoteReadReplica
    , promoteReadReplicaCatch

    -- * PurchaseReservedDBInstancesOffering
    -- $PurchaseReservedDBInstancesOffering
    , purchaseReservedDBInstancesOffering
    , purchaseReservedDBInstancesOfferingCatch

    -- * RebootDBInstance
    -- $RebootDBInstance
    , rebootDBInstance
    , rebootDBInstanceCatch

    -- * RemoveSourceIdentifierFromSubscription
    -- $RemoveSourceIdentifierFromSubscription
    , removeSourceIdentifierFromSubscription
    , removeSourceIdentifierFromSubscriptionCatch

    -- * RemoveTagsFromResource
    -- $RemoveTagsFromResource
    , removeTagsFromResource
    , removeTagsFromResourceCatch

    -- * ResetDBParameterGroup
    -- $ResetDBParameterGroup
    , resetDBParameterGroup
    , resetDBParameterGroupCatch

    -- * RestoreDBInstanceFromDBSnapshot
    -- $RestoreDBInstanceFromDBSnapshot
    , restoreDBInstanceFromDBSnapshot
    , restoreDBInstanceFromDBSnapshotCatch

    -- * RestoreDBInstanceToPointInTime
    -- $RestoreDBInstanceToPointInTime
    , restoreDBInstanceToPointInTime
    , restoreDBInstanceToPointInTimeCatch

    -- * RevokeDBSecurityGroupIngress
    -- $RevokeDBSecurityGroupIngress
    , revokeDBSecurityGroupIngress
    , revokeDBSecurityGroupIngressCatch

    -- * Re-exported
    , module AWS
    , module Network.AWS.RDS.V2013_09_09
    , (.=)
    , (?=)
    , (<>=)
    , (%=)
    ) where

import Control.Monad.Trans.AWS as AWS
import Network.AWS.Prelude
import Network.AWS.RDS.V2013_09_09

type ServiceErr = RDSError


-- $DownloadDBLogFilePortion
-- Downloads the last line of the specified log file.
-- https://rds.amazonaws.com/ ?DBInstanceIdentifier=rra-mysql &MaxRecords=100
-- &Version=2013-05-15 &Action=DescribeDBLogFiles &SignatureVersion=4
-- &SignatureMethod=HmacSHA256 &Timestamp=20130327T173621Z
-- &X-Amz-Algorithm=AWS4-HMAC-SHA256 &X-Amz-Date=20130327T173621Z
-- &X-Amz-SignedHeaders=Host &X-Amz-Expires=20130327T173621Z
-- &X-Amz-Credential= &X-Amz-Signature= 1364403600000
-- error/mysql-error-running.log 0 1364338800000
-- error/mysql-error-running.log.0 0 1364342400000
-- error/mysql-error-running.log.1 0 1364371200000
-- error/mysql-error-running.log.9 0 1364405700000 error/mysql-error.log 0
-- d70fb3b3-9704-11e2-a0db-871552e0ef19.
--
-- See: 'Network.AWS.RDS.V2013_09_09.DownloadDBLogFilePortion'

downloadDBLogFilePortion :: ( MonadCatch m
                            , MonadResource m
                            , MonadError AWS.Error m
                            , MonadReader Env (ResumableSource m)
                            )
    => Text -- ^ 'ddblfpDBInstanceIdentifier'
    -> Text -- ^ 'ddblfpLogFileName'
    -> State DownloadDBLogFilePortion a
    -> ResumableSource m DownloadDBLogFilePortionResponse
downloadDBLogFilePortion p1 p2 s =
    paginate $ (mkDownloadDBLogFilePortion p1 p2) &~ s

downloadDBLogFilePortionCatch :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadReader Env (ResumableSource m)
                                 )
    => Text -- ^ 'ddblfpDBInstanceIdentifier'
    -> Text -- ^ 'ddblfpLogFileName'
    -> State DownloadDBLogFilePortion a
    -> ResumableSource m (Either ServiceErr DownloadDBLogFilePortionResponse)
downloadDBLogFilePortionCatch p1 p2 s =
    paginateCatch $ (mkDownloadDBLogFilePortion p1 p2) &~ s

-- $AddSourceIdentifierToSubscription
-- Adds a source identifier to an existing RDS event notification
-- subscription. https://rds.us-east-1.amazonaws.com/
-- ?Action=AddSourceIdentifierToSubscription
-- ?SubscriptionName=EventSubscription01 &SourceIdentifier=dbinstance01
-- &Version=2013-01-10 &SignatureVersion=4 &SignatureMethod=HmacSHA256
-- &Timestamp=20130128T011410Z &AWSAccessKeyId= &Signature= true 012345678901
-- db-instance modifying dbinstance01 2013-01-28 00:29:23.736 creation
-- deletion EventSubscription01
-- arn:aws:sns:us-east-1:012345678901:EventSubscription01
-- 05d0fd8a-68e8-11e2-8e4d-31f087d822e1.
--
-- See: 'Network.AWS.RDS.V2013_09_09.AddSourceIdentifierToSubscription'

addSourceIdentifierToSubscription :: ( MonadCatch m
                                     , MonadResource m
                                     , MonadError AWS.Error m
                                     , MonadReader Env m
                                     )
    => Text -- ^ 'asitsSubscriptionName'
    -> Text -- ^ 'asitsSourceIdentifier'
    -> State AddSourceIdentifierToSubscription a
    -> m AddSourceIdentifierToSubscriptionResponse
addSourceIdentifierToSubscription p1 p2 s =
    send $ (mkAddSourceIdentifierToSubscription p1 p2) &~ s

addSourceIdentifierToSubscriptionCatch :: ( MonadCatch m
                                          , MonadResource m
                                          , MonadReader Env m
                                          )
    => Text -- ^ 'asitsSubscriptionName'
    -> Text -- ^ 'asitsSourceIdentifier'
    -> State AddSourceIdentifierToSubscription a
    -> m (Either ServiceErr AddSourceIdentifierToSubscriptionResponse)
addSourceIdentifierToSubscriptionCatch p1 p2 s =
    sendCatch $ (mkAddSourceIdentifierToSubscription p1 p2) &~ s

-- $AddTagsToResource
-- Adds metadata tags to an Amazon RDS resource. These tags can also be used
-- with cost allocation reporting to track cost associated with Amazon RDS
-- resources, or used in Condition statement in IAM policy for Amazon RDS. For
-- an overview on tagging Amazon RDS resources, see Tagging Amazon RDS
-- Resources.
--
-- See: 'Network.AWS.RDS.V2013_09_09.AddTagsToResource'

addTagsToResource :: ( MonadCatch m
                     , MonadResource m
                     , MonadError AWS.Error m
                     , MonadReader Env m
                     )
    => Text -- ^ 'attrResourceName'
    -> [Tag] -- ^ 'attrTags'
    -> State AddTagsToResource a
    -> m AddTagsToResourceResponse
addTagsToResource p1 p2 s =
    send $ (mkAddTagsToResource p1 p2) &~ s

addTagsToResourceCatch :: ( MonadCatch m
                          , MonadResource m
                          , MonadReader Env m
                          )
    => Text -- ^ 'attrResourceName'
    -> [Tag] -- ^ 'attrTags'
    -> State AddTagsToResource a
    -> m (Either ServiceErr AddTagsToResourceResponse)
addTagsToResourceCatch p1 p2 s =
    sendCatch $ (mkAddTagsToResource p1 p2) &~ s

-- $AuthorizeDBSecurityGroupIngress
-- Enables ingress to a DBSecurityGroup using one of two forms of
-- authorization. First, EC2 or VPC security groups can be added to the
-- DBSecurityGroup if the application using the database is running on EC2 or
-- VPC instances. Second, IP ranges are available if the application accessing
-- your database is running on the Internet. Required parameters for this API
-- are one of CIDR range, EC2SecurityGroupId for VPC, or
-- (EC2SecurityGroupOwnerId and either EC2SecurityGroupName or
-- EC2SecurityGroupId for non-VPC). You cannot authorize ingress from an EC2
-- security group in one Region to an Amazon RDS DB instance in another. You
-- cannot authorize ingress from a VPC security group in one VPC to an Amazon
-- RDS DB instance in another. For an overview of CIDR ranges, go to the
-- Wikipedia Tutorial. https://rds.amazonaws.com/ ?CIDRIP=192.168.1.1%2F24
-- &DBSecurityGroupName=mydbsecuritygroup &Version=2013-05-15
-- &Action=AuthorizeDBSecurityGroupIngress &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2011-02-15T17%3A10%3A50.274Z
-- &AWSAccessKeyId= &Signature= My new DBSecurityGroup 192.168.1.1/24
-- authorizing 621567473609 mydbsecuritygroup vpc-1ab2c3d4
-- d9799197-bf2d-11de-b88d-993294bf1c81.
--
-- See: 'Network.AWS.RDS.V2013_09_09.AuthorizeDBSecurityGroupIngress'

authorizeDBSecurityGroupIngress :: ( MonadCatch m
                                   , MonadResource m
                                   , MonadError AWS.Error m
                                   , MonadReader Env m
                                   )
    => Text -- ^ 'adbsgiDBSecurityGroupName'
    -> State AuthorizeDBSecurityGroupIngress a
    -> m AuthorizeDBSecurityGroupIngressResponse
authorizeDBSecurityGroupIngress p1 s =
    send $ (mkAuthorizeDBSecurityGroupIngress p1) &~ s

authorizeDBSecurityGroupIngressCatch :: ( MonadCatch m
                                        , MonadResource m
                                        , MonadReader Env m
                                        )
    => Text -- ^ 'adbsgiDBSecurityGroupName'
    -> State AuthorizeDBSecurityGroupIngress a
    -> m (Either ServiceErr AuthorizeDBSecurityGroupIngressResponse)
authorizeDBSecurityGroupIngressCatch p1 s =
    sendCatch $ (mkAuthorizeDBSecurityGroupIngress p1) &~ s

-- $CopyDBSnapshot
-- Copies the specified DBSnapshot. The source DBSnapshot must be in the
-- "available" state. https://rds.amazonaws.com/ ?Action=CopyDBSnapshot
-- &SourceDBSnapshotIdentifier=rds:simcoprod01-2012-04-02-00-01
-- &TargetDBSnapshotIdentifier=mydbsnapshot &Version=2013-05-15
-- &SignatureVersion=2&SignatureMethod=HmacSHA256
-- &Timestamp=2011-12-12T06%3A27%3A42.551Z &AWSAccessKeyId= &Signature= 3306
-- mysql available us-east-1a general-public-license 2011-05-23T06:06:43.110Z
-- 10 simcoprod01 5.1.50 mydbsnapshot manual master
-- c4181d1d-8505-11e0-90aa-eb648410240d.
--
-- See: 'Network.AWS.RDS.V2013_09_09.CopyDBSnapshot'

copyDBSnapshot :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env m
                  )
    => Text -- ^ 'cdbsSourceDBSnapshotIdentifier'
    -> Text -- ^ 'cdbsTargetDBSnapshotIdentifier'
    -> State CopyDBSnapshot a
    -> m CopyDBSnapshotResponse
copyDBSnapshot p1 p2 s =
    send $ (mkCopyDBSnapshot p1 p2) &~ s

copyDBSnapshotCatch :: ( MonadCatch m
                       , MonadResource m
                       , MonadReader Env m
                       )
    => Text -- ^ 'cdbsSourceDBSnapshotIdentifier'
    -> Text -- ^ 'cdbsTargetDBSnapshotIdentifier'
    -> State CopyDBSnapshot a
    -> m (Either ServiceErr CopyDBSnapshotResponse)
copyDBSnapshotCatch p1 p2 s =
    sendCatch $ (mkCopyDBSnapshot p1 p2) &~ s

-- $CreateDBInstance
-- Creates a new DB instance. https://rds.amazonaws.com/
-- ?Action=CreateDBInstance &DBInstanceIdentifier=SimCoProd01 &Engine=mysql
-- &MasterUserPassword=Password01 &AllocatedStorage=10 &MasterUsername=master
-- &Version=2013-05-15 &DBInstanceClass=db.m1.large
-- &DBSubnetGroupName=dbSubnetgroup01 &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2011-05-23T05%3A54%3A53.578Z
-- &AWSAccessKeyId= &Signature= mysql **** 1 false general-public-license
-- 990524496922 Complete description subnet_grp1 Active subnet-7c5b4115
-- us-east-1c Active subnet-7b5b4112 us-east-1b Active subnet-3ea6bd57
-- us-east-1d creating 5.1.50 simcoprod01 in-sync default.mysql5.1 active
-- default 00:00-00:30 true sat:07:30-sat:08:00 10 db.m1.large master
-- 2e5d4270-8501-11e0-bd9b-a7b1ece36d51.
--
-- See: 'Network.AWS.RDS.V2013_09_09.CreateDBInstance'

createDBInstance :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => Text -- ^ 'cdbiDBInstanceIdentifier'
    -> Integer -- ^ 'cdbiAllocatedStorage'
    -> Text -- ^ 'cdbiDBInstanceClass'
    -> Text -- ^ 'cdbiEngine'
    -> Text -- ^ 'cdbiMasterUsername'
    -> Text -- ^ 'cdbiMasterUserPassword'
    -> State CreateDBInstance a
    -> m CreateDBInstanceResponse
createDBInstance p2 p3 p4 p5 p6 p7 s =
    send $ (mkCreateDBInstance p2 p3 p4 p5 p6 p7) &~ s

createDBInstanceCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => Text -- ^ 'cdbiDBInstanceIdentifier'
    -> Integer -- ^ 'cdbiAllocatedStorage'
    -> Text -- ^ 'cdbiDBInstanceClass'
    -> Text -- ^ 'cdbiEngine'
    -> Text -- ^ 'cdbiMasterUsername'
    -> Text -- ^ 'cdbiMasterUserPassword'
    -> State CreateDBInstance a
    -> m (Either ServiceErr CreateDBInstanceResponse)
createDBInstanceCatch p2 p3 p4 p5 p6 p7 s =
    sendCatch $ (mkCreateDBInstance p2 p3 p4 p5 p6 p7) &~ s

-- $CreateDBInstanceReadReplica
-- Creates a DB instance that acts as a read replica of a source DB instance.
-- All read replica DB instances are created as Single-AZ deployments with
-- backups disabled. All other DB instance attributes (including DB security
-- groups and DB parameter groups) are inherited from the source DB instance,
-- except as specified below. The source DB instance must have backup
-- retention enabled. https://rds.amazonaws.com/
-- ?Action=CreateDBInstanceReadReplica &DBInstanceIdentifier=myreadreplica
-- &SourceDBInstanceIdentifier=mydbinstance &Version=2013-05-15
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-05-15T23%3A35%3A07.325Z &AWSAccessKeyId= &Signature= mysql
-- 0 false general-public-license creating 5.1.50 myreadreplica in-sync
-- default.mysql5.1 mydbinstance active default 23:00-01:00 true
-- sun:05:00-sun:09:00 10 db.m1.small master
-- 3e24c5cd-c6e2-11df-8463-4f0c49644cb7.
--
-- See: 'Network.AWS.RDS.V2013_09_09.CreateDBInstanceReadReplica'

createDBInstanceReadReplica :: ( MonadCatch m
                               , MonadResource m
                               , MonadError AWS.Error m
                               , MonadReader Env m
                               )
    => Text -- ^ 'cdbirrDBInstanceIdentifier'
    -> Text -- ^ 'cdbirrSourceDBInstanceIdentifier'
    -> State CreateDBInstanceReadReplica a
    -> m CreateDBInstanceReadReplicaResponse
createDBInstanceReadReplica p1 p2 s =
    send $ (mkCreateDBInstanceReadReplica p1 p2) &~ s

createDBInstanceReadReplicaCatch :: ( MonadCatch m
                                    , MonadResource m
                                    , MonadReader Env m
                                    )
    => Text -- ^ 'cdbirrDBInstanceIdentifier'
    -> Text -- ^ 'cdbirrSourceDBInstanceIdentifier'
    -> State CreateDBInstanceReadReplica a
    -> m (Either ServiceErr CreateDBInstanceReadReplicaResponse)
createDBInstanceReadReplicaCatch p1 p2 s =
    sendCatch $ (mkCreateDBInstanceReadReplica p1 p2) &~ s

-- $CreateDBParameterGroup
-- Creates a new DB parameter group. A DB parameter group is initially created
-- with the default parameters for the database engine used by the DB
-- instance. To provide custom values for any of the parameters, you must
-- modify the group after creating it using ModifyDBParameterGroup. Once
-- you've created a DB parameter group, you need to associate it with your DB
-- instance using ModifyDBInstance. When you associate a new DB parameter
-- group with a running DB instance, you need to reboot the DB Instance for
-- the new DB parameter group and associated settings to take effect.
-- https://rds.amazonaws.com/ ?Action=CreateDBParameterGroup
-- &DBParameterGroupName=mydbparametergroup3 &DBParameterGroupFamily=MySQL5.1
-- &Version=2013-05-15 &Description=My%20new%20DBParameterGroup
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-05-11T18%3A09%3A29.793Z &AWSAccessKeyId= &Signature=
-- mysql5.1 My new DBParameterGroup mydbparametergroup3
-- 0b447b66-bf36-11de-a88b-7b5b3d23b3a7.
--
-- See: 'Network.AWS.RDS.V2013_09_09.CreateDBParameterGroup'

createDBParameterGroup :: ( MonadCatch m
                          , MonadResource m
                          , MonadError AWS.Error m
                          , MonadReader Env m
                          )
    => Text -- ^ 'cdbpgDBParameterGroupName'
    -> Text -- ^ 'cdbpgDBParameterGroupFamily'
    -> Text -- ^ 'cdbpgDescription'
    -> State CreateDBParameterGroup a
    -> m CreateDBParameterGroupResponse
createDBParameterGroup p1 p2 p3 s =
    send $ (mkCreateDBParameterGroup p1 p2 p3) &~ s

createDBParameterGroupCatch :: ( MonadCatch m
                               , MonadResource m
                               , MonadReader Env m
                               )
    => Text -- ^ 'cdbpgDBParameterGroupName'
    -> Text -- ^ 'cdbpgDBParameterGroupFamily'
    -> Text -- ^ 'cdbpgDescription'
    -> State CreateDBParameterGroup a
    -> m (Either ServiceErr CreateDBParameterGroupResponse)
createDBParameterGroupCatch p1 p2 p3 s =
    sendCatch $ (mkCreateDBParameterGroup p1 p2 p3) &~ s

-- $CreateDBSecurityGroup
-- Creates a new DB security group. DB security groups control access to a DB
-- instance. https://rds.amazonaws.com/ ?Action=CreateDBSecurityGroup
-- &DBSecurityGroupName=mydbsecuritygroup
-- &DBSecurityGroupDescription=My%20new%20DBSecurityGroup
-- &EC2VpcId=vpc-1a2b3c4d &Version=2013-05-15 &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2011-02-15T18%3A14%3A49.482Z
-- &AWSAccessKeyId= &Signature= My new DBSecurityGroup 565419523791
-- mydbsecuritygroup vpc-1a2b3c4d ed662948-a57b-11df-9e38-7ffab86c801f.
--
-- See: 'Network.AWS.RDS.V2013_09_09.CreateDBSecurityGroup'

createDBSecurityGroup :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         )
    => Text -- ^ 'cdbsgDBSecurityGroupName'
    -> Text -- ^ 'cdbsgDBSecurityGroupDescription'
    -> State CreateDBSecurityGroup a
    -> m CreateDBSecurityGroupResponse
createDBSecurityGroup p1 p2 s =
    send $ (mkCreateDBSecurityGroup p1 p2) &~ s

createDBSecurityGroupCatch :: ( MonadCatch m
                              , MonadResource m
                              , MonadReader Env m
                              )
    => Text -- ^ 'cdbsgDBSecurityGroupName'
    -> Text -- ^ 'cdbsgDBSecurityGroupDescription'
    -> State CreateDBSecurityGroup a
    -> m (Either ServiceErr CreateDBSecurityGroupResponse)
createDBSecurityGroupCatch p1 p2 s =
    sendCatch $ (mkCreateDBSecurityGroup p1 p2) &~ s

-- $CreateDBSnapshot
-- Creates a DBSnapshot. The source DBInstance must be in "available" state.
-- https://rds.amazonaws.com/ ?Action=CreateDBSnapshot
-- &DBInstanceIdentifier=simcoprod01 &DBSnapshotIdentifier=mydbsnapshot
-- &Version=2013-05-15 &SignatureVersion=2&SignatureMethod=HmacSHA256
-- &Timestamp=2011-05-23T06%3A27%3A42.551Z &AWSAccessKeyId= &Signature= 3306
-- mysql creating us-east-1a general-public-license 2011-05-23T06:06:43.110Z
-- 10 simcoprod01 5.1.50 mydbsnapshot manual master
-- c4181d1d-8505-11e0-90aa-eb648410240d.
--
-- See: 'Network.AWS.RDS.V2013_09_09.CreateDBSnapshot'

createDBSnapshot :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => Text -- ^ 'cdbs1DBSnapshotIdentifier'
    -> Text -- ^ 'cdbs1DBInstanceIdentifier'
    -> State CreateDBSnapshot a
    -> m CreateDBSnapshotResponse
createDBSnapshot p1 p2 s =
    send $ (mkCreateDBSnapshot p1 p2) &~ s

createDBSnapshotCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => Text -- ^ 'cdbs1DBSnapshotIdentifier'
    -> Text -- ^ 'cdbs1DBInstanceIdentifier'
    -> State CreateDBSnapshot a
    -> m (Either ServiceErr CreateDBSnapshotResponse)
createDBSnapshotCatch p1 p2 s =
    sendCatch $ (mkCreateDBSnapshot p1 p2) &~ s

-- $CreateDBSubnetGroup
-- Creates a new DB subnet group. DB subnet groups must contain at least one
-- subnet in at least two AZs in the region. https://rds.amazonaws.com/
-- ?Action=CreateDBSubnetGroup &DBSubnetGroupName=mydbsubnetgroup
-- &DBSubnetGroupDescription=My%20new%20DBSubnetGroup &Version=2013-05-15
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-02-15T18%3A14%3A49.482Z &AWSAccessKeyId= &Signature=
-- 990524496922 Complete My new DBSubnetGroup mydbsubnetgroup Active
-- subnet-7c5b4115 us-east-1c Active subnet-7b5b4112 us-east-1b Active
-- subnet-3ea6bd57 us-east-1d ed662948-a57b-11df-9e38-7ffab86c801f.
--
-- See: 'Network.AWS.RDS.V2013_09_09.CreateDBSubnetGroup'

createDBSubnetGroup :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
    => Text -- ^ 'cdbsg1DBSubnetGroupName'
    -> Text -- ^ 'cdbsg1DBSubnetGroupDescription'
    -> [Text] -- ^ 'cdbsg1SubnetIds'
    -> State CreateDBSubnetGroup a
    -> m CreateDBSubnetGroupResponse
createDBSubnetGroup p1 p2 p3 s =
    send $ (mkCreateDBSubnetGroup p1 p2 p3) &~ s

createDBSubnetGroupCatch :: ( MonadCatch m
                            , MonadResource m
                            , MonadReader Env m
                            )
    => Text -- ^ 'cdbsg1DBSubnetGroupName'
    -> Text -- ^ 'cdbsg1DBSubnetGroupDescription'
    -> [Text] -- ^ 'cdbsg1SubnetIds'
    -> State CreateDBSubnetGroup a
    -> m (Either ServiceErr CreateDBSubnetGroupResponse)
createDBSubnetGroupCatch p1 p2 p3 s =
    sendCatch $ (mkCreateDBSubnetGroup p1 p2 p3) &~ s

-- $CreateEventSubscription
-- Creates an RDS event notification subscription. This action requires a
-- topic ARN (Amazon Resource Name) created by either the RDS console, the SNS
-- console, or the SNS API. To obtain an ARN with SNS, you must create a topic
-- in Amazon SNS and subscribe to the topic. The ARN is displayed in the SNS
-- console. You can specify the type of source (SourceType) you want to be
-- notified of, provide a list of RDS sources (SourceIds) that triggers the
-- events, and provide a list of event categories (EventCategories) for events
-- you want to be notified of. For example, you can specify SourceType =
-- db-instance, SourceIds = mydbinstance1, mydbinstance2 and EventCategories =
-- Availability, Backup. If you specify both the SourceType and SourceIds,
-- such as SourceType = db-instance and SourceIdentifier = myDBInstance1, you
-- will be notified of all the db-instance events for the specified source. If
-- you specify a SourceType but do not specify a SourceIdentifier, you will
-- receive notice of the events for that source type for all your RDS sources.
-- If you do not specify either the SourceType nor the SourceIdentifier, you
-- will be notified of events generated from all RDS sources belonging to your
-- customer account. https://rds.us-east-1.amazonaws.com/
-- ?Action=CreateEventSubscription &SubscriptionName=EventSubscription02
-- &Enabled=true
-- &SnsTopicArn=arn%3Aaws%3Asns%3Aus-east-1%3A012345678901%3AEventSubscription01
-- &Version=2013-01-10 &SignatureVersion=4 &SignatureMethod=HmacSHA256
-- &Timestamp=20130128T002941Z &AWSAccessKeyId= &Signature= true 012345678901
-- creating Mon Jan 28 00:29:42 UTC 2013 EventSubscription02
-- arn:aws:sns:us-east-1:012345678901:EventSubscription01
-- cf3407aa-68e1-11e2-bd13-a92da73b3119 https://rds.us-east-1.amazonaws.com/
-- ?Action=CreateEventSubscription &SubscriptionName=EventSubscription03
-- &SourceType=db-instance &EventCategories.member.1=creation
-- &EventCategories.member.2=deletion &SourceIds.member.1=dbinstance01
-- &SourceIds.member.2=dbinstance02 &Enabled=true
-- &SnsTopicArn=arn%3Aaws%3Asns%3Aus-east-1%3A012345678901%3AEventSubscription01
-- &Version=2013-01-10 &SignatureVersion=4 &SignatureMethod=HmacSHA256
-- &Timestamp=20130128T014117Z &AWSAccessKeyId= &Signature= true 012345678901
-- db-instance creating dbinstance01 dbinstance02 Mon Jan 28 01:41:19 UTC 2013
-- creation deletion EventSubscription03
-- arn:aws:sns:us-east-1:012345678901:EventSubscription01
-- d064b48c-68eb-11e2-ab10-11125abcb784.
--
-- See: 'Network.AWS.RDS.V2013_09_09.CreateEventSubscription'

createEventSubscription :: ( MonadCatch m
                           , MonadResource m
                           , MonadError AWS.Error m
                           , MonadReader Env m
                           )
    => Text -- ^ 'cesSubscriptionName'
    -> Text -- ^ 'cesSnsTopicArn'
    -> State CreateEventSubscription a
    -> m CreateEventSubscriptionResponse
createEventSubscription p1 p2 s =
    send $ (mkCreateEventSubscription p1 p2) &~ s

createEventSubscriptionCatch :: ( MonadCatch m
                                , MonadResource m
                                , MonadReader Env m
                                )
    => Text -- ^ 'cesSubscriptionName'
    -> Text -- ^ 'cesSnsTopicArn'
    -> State CreateEventSubscription a
    -> m (Either ServiceErr CreateEventSubscriptionResponse)
createEventSubscriptionCatch p1 p2 s =
    sendCatch $ (mkCreateEventSubscription p1 p2) &~ s

-- $CreateOptionGroup
-- Creates a new option group. You can create up to 20 option groups.
-- https://rds.amazonaws.com/ ?Action=CreateOptionGroup
-- &OptionGroupName=myoptiongroup &EngineName=oracle-se1
-- &MajorEngineVersion=11.2 &OptionGroupDescription=Test option group 11.2
-- myoptiongroup oracle-se1 Test option group
-- b2416a8a-84c9-11e1-a264-0b23c28bc344.
--
-- See: 'Network.AWS.RDS.V2013_09_09.CreateOptionGroup'

createOptionGroup :: ( MonadCatch m
                     , MonadResource m
                     , MonadError AWS.Error m
                     , MonadReader Env m
                     )
    => Text -- ^ 'cogOptionGroupName'
    -> Text -- ^ 'cogEngineName'
    -> Text -- ^ 'cogMajorEngineVersion'
    -> Text -- ^ 'cogOptionGroupDescription'
    -> State CreateOptionGroup a
    -> m CreateOptionGroupResponse
createOptionGroup p1 p2 p3 p4 s =
    send $ (mkCreateOptionGroup p1 p2 p3 p4) &~ s

createOptionGroupCatch :: ( MonadCatch m
                          , MonadResource m
                          , MonadReader Env m
                          )
    => Text -- ^ 'cogOptionGroupName'
    -> Text -- ^ 'cogEngineName'
    -> Text -- ^ 'cogMajorEngineVersion'
    -> Text -- ^ 'cogOptionGroupDescription'
    -> State CreateOptionGroup a
    -> m (Either ServiceErr CreateOptionGroupResponse)
createOptionGroupCatch p1 p2 p3 p4 s =
    sendCatch $ (mkCreateOptionGroup p1 p2 p3 p4) &~ s

-- $DeleteDBInstance
-- The DeleteDBInstance action deletes a previously provisioned DB instance. A
-- successful response from the web service indicates the request was received
-- correctly. When you delete a DB instance, all automated backups for that
-- instance are deleted and cannot be recovered. Manual DB snapshots of the DB
-- instance to be deleted are not deleted. If a final DB snapshot is requested
-- the status of the RDS instance will be "deleting" until the DB snapshot is
-- created. The API action DescribeDBInstance is used to monitor the status of
-- this operation. The action cannot be canceled or reverted once submitted.
-- https://rds.amazonaws.com/ ?Action=DeleteDBInstance
-- &DBInstanceIdentifier=myrestoreddbinstance &SkipFinalSnapshot=true
-- &Version=2013-05-15 &Timestamp=2011-05-23T07%3A19%3A35.947Z
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256 &AWSAccessKeyId=
-- &Signature= 2011-05-23T07:15:00Z mysql 1 false general-public-license
-- deleting 5.1.50 3306
-- myrestoreddbinstance.cu7u2t4uz396.us-east.rds.amazonaws.com
-- myrestoreddbinstance in-sync default.mysql5.1 active default 00:00-00:30
-- true sat:07:30-sat:08:00 us-east-1d 2011-05-23T06:52:48.255Z 10 db.m1.large
-- master 03ea4ae8-850d-11e0-90aa-eb648410240d.
--
-- See: 'Network.AWS.RDS.V2013_09_09.DeleteDBInstance'

deleteDBInstance :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => Text -- ^ 'ddbiDBInstanceIdentifier'
    -> State DeleteDBInstance a
    -> m DeleteDBInstanceResponse
deleteDBInstance p1 s =
    send $ (mkDeleteDBInstance p1) &~ s

deleteDBInstanceCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => Text -- ^ 'ddbiDBInstanceIdentifier'
    -> State DeleteDBInstance a
    -> m (Either ServiceErr DeleteDBInstanceResponse)
deleteDBInstanceCatch p1 s =
    sendCatch $ (mkDeleteDBInstance p1) &~ s

-- $DeleteDBParameterGroup
-- Deletes a specified DBParameterGroup. The DBParameterGroup cannot be
-- associated with any RDS instances to be deleted. The specified DB parameter
-- group cannot be associated with any DB instances.
-- https://rds.amazonaws.com/ ?Action=DeleteDBParameterGroup
-- &DBParameterGroupName=mydbparametergroup &Version=2013-05-15
-- &SignatureVersion=2&SignatureMethod=HmacSHA256
-- &Timestamp=2011-05-11T18%3A47%3A08.851Z &AWSAccessKeyId= &Signature=
-- 4dc38be9-bf3b-11de-a88b-7b5b3d23b3a7.
--
-- See: 'Network.AWS.RDS.V2013_09_09.DeleteDBParameterGroup'

deleteDBParameterGroup :: ( MonadCatch m
                          , MonadResource m
                          , MonadError AWS.Error m
                          , MonadReader Env m
                          )
    => Text -- ^ 'ddbpgDBParameterGroupName'
    -> State DeleteDBParameterGroup a
    -> m DeleteDBParameterGroupResponse
deleteDBParameterGroup p1 s =
    send $ (mkDeleteDBParameterGroup p1) &~ s

deleteDBParameterGroupCatch :: ( MonadCatch m
                               , MonadResource m
                               , MonadReader Env m
                               )
    => Text -- ^ 'ddbpgDBParameterGroupName'
    -> State DeleteDBParameterGroup a
    -> m (Either ServiceErr DeleteDBParameterGroupResponse)
deleteDBParameterGroupCatch p1 s =
    sendCatch $ (mkDeleteDBParameterGroup p1) &~ s

-- $DeleteDBSecurityGroup
-- Deletes a DB security group. The specified DB security group must not be
-- associated with any DB instances. https://rds.amazonaws.com/
-- ?Action=DeleteDBSecurityGroup &DBSecurityGroupName=mysecuritygroup
-- &Version=2013-05-15 &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-02-15T17%3A48%3A21.746Z &AWSAccessKeyId= &Signature=
-- 5d013245-4172-11df-8520-e7e1e602a915.
--
-- See: 'Network.AWS.RDS.V2013_09_09.DeleteDBSecurityGroup'

deleteDBSecurityGroup :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         )
    => Text -- ^ 'ddbsgDBSecurityGroupName'
    -> State DeleteDBSecurityGroup a
    -> m DeleteDBSecurityGroupResponse
deleteDBSecurityGroup p1 s =
    send $ (mkDeleteDBSecurityGroup p1) &~ s

deleteDBSecurityGroupCatch :: ( MonadCatch m
                              , MonadResource m
                              , MonadReader Env m
                              )
    => Text -- ^ 'ddbsgDBSecurityGroupName'
    -> State DeleteDBSecurityGroup a
    -> m (Either ServiceErr DeleteDBSecurityGroupResponse)
deleteDBSecurityGroupCatch p1 s =
    sendCatch $ (mkDeleteDBSecurityGroup p1) &~ s

-- $DeleteDBSnapshot
-- Deletes a DBSnapshot. If the snapshot is being copied, the copy operation
-- is terminated. The DBSnapshot must be in the available state to be deleted.
-- https://rds.amazon.com/ &DBSnapshotIdentifier=mydbsnapshot
-- &Version=2013-05-15 &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-05-23T06%3A27%3A42.551Z &AWSAccessKeyId= &Signature= 3306
-- 2011-03-11T07:20:24.082Z mysql deleted us-east-1d general-public-license
-- 2010-07-16T00:06:59.107Z 60 simcoprod01 5.1.47 mysnapshot2 manual master
-- 627a43a1-8507-11e0-bd9b-a7b1ece36d51.
--
-- See: 'Network.AWS.RDS.V2013_09_09.DeleteDBSnapshot'

deleteDBSnapshot :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => Text -- ^ 'ddbsDBSnapshotIdentifier'
    -> State DeleteDBSnapshot a
    -> m DeleteDBSnapshotResponse
deleteDBSnapshot p1 s =
    send $ (mkDeleteDBSnapshot p1) &~ s

deleteDBSnapshotCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => Text -- ^ 'ddbsDBSnapshotIdentifier'
    -> State DeleteDBSnapshot a
    -> m (Either ServiceErr DeleteDBSnapshotResponse)
deleteDBSnapshotCatch p1 s =
    sendCatch $ (mkDeleteDBSnapshot p1) &~ s

-- $DeleteDBSubnetGroup
-- Deletes a DB subnet group. The specified database subnet group must not be
-- associated with any DB instances. https://rds.amazonaws.com/
-- ?Action=DeleteDBSubnetGroup &DBSubnetGroupName=mysubnetgroup
-- &Version=2013-05-15 &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-02-15T17%3A48%3A21.746Z &AWSAccessKeyId= &Signature=
-- 5d013245-4172-11df-8520-e7e1e602a915.
--
-- See: 'Network.AWS.RDS.V2013_09_09.DeleteDBSubnetGroup'

deleteDBSubnetGroup :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
    => Text -- ^ 'ddbsg1DBSubnetGroupName'
    -> State DeleteDBSubnetGroup a
    -> m DeleteDBSubnetGroupResponse
deleteDBSubnetGroup p1 s =
    send $ (mkDeleteDBSubnetGroup p1) &~ s

deleteDBSubnetGroupCatch :: ( MonadCatch m
                            , MonadResource m
                            , MonadReader Env m
                            )
    => Text -- ^ 'ddbsg1DBSubnetGroupName'
    -> State DeleteDBSubnetGroup a
    -> m (Either ServiceErr DeleteDBSubnetGroupResponse)
deleteDBSubnetGroupCatch p1 s =
    sendCatch $ (mkDeleteDBSubnetGroup p1) &~ s

-- $DeleteEventSubscription
-- Deletes an RDS event notification subscription.
-- https://rds.us-east-1.amazonaws.com/ ?Action=DeleteEventSubscription
-- &SubscriptionName=EventSubscription01 &Version=2013-01-10
-- &SignatureVersion=4 &SignatureMethod=HmacSHA256 &Timestamp=20130128T012739Z
-- &AWSAccessKeyId= &Signature= true 012345678901 db-instance deleting
-- 2013-01-28 00:29:23.736 creation deletion EventSubscription01
-- arn:aws:sns:us-east-1:012345678901:EventSubscription01
-- e7cf30ac-68e9-11e2-bd13-a92da73b3119.
--
-- See: 'Network.AWS.RDS.V2013_09_09.DeleteEventSubscription'

deleteEventSubscription :: ( MonadCatch m
                           , MonadResource m
                           , MonadError AWS.Error m
                           , MonadReader Env m
                           )
    => Text -- ^ 'desSubscriptionName'
    -> State DeleteEventSubscription a
    -> m DeleteEventSubscriptionResponse
deleteEventSubscription p1 s =
    send $ (mkDeleteEventSubscription p1) &~ s

deleteEventSubscriptionCatch :: ( MonadCatch m
                                , MonadResource m
                                , MonadReader Env m
                                )
    => Text -- ^ 'desSubscriptionName'
    -> State DeleteEventSubscription a
    -> m (Either ServiceErr DeleteEventSubscriptionResponse)
deleteEventSubscriptionCatch p1 s =
    sendCatch $ (mkDeleteEventSubscription p1) &~ s

-- $DeleteOptionGroup
-- Deletes an existing option group. https://rds.amazonaws.com/
-- ?Action=DeleteOptionGroup &OptionGroupName=myoptiongroup.
--
-- See: 'Network.AWS.RDS.V2013_09_09.DeleteOptionGroup'

deleteOptionGroup :: ( MonadCatch m
                     , MonadResource m
                     , MonadError AWS.Error m
                     , MonadReader Env m
                     )
    => Text -- ^ 'dogOptionGroupName'
    -> State DeleteOptionGroup a
    -> m DeleteOptionGroupResponse
deleteOptionGroup p1 s =
    send $ (mkDeleteOptionGroup p1) &~ s

deleteOptionGroupCatch :: ( MonadCatch m
                          , MonadResource m
                          , MonadReader Env m
                          )
    => Text -- ^ 'dogOptionGroupName'
    -> State DeleteOptionGroup a
    -> m (Either ServiceErr DeleteOptionGroupResponse)
deleteOptionGroupCatch p1 s =
    sendCatch $ (mkDeleteOptionGroup p1) &~ s

-- $DescribeDBEngineVersions
-- Returns a list of the available DB engines. https://rds.amazonaws.com/
-- ?Action=DescribeDBEngineVersions &MaxRecords=100 &Version=2013-05-15
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-05-23T07%3A34%3A17.435Z &AWSAccessKeyId= &Signature=
-- mysql5.1 mysql 5.1.42 mysql5.1 mysql Use instead of mysql5.1 5.1.45 yaSSL
-- Security Fixes mysql5.1 mysql Use instead of mysql5.1 5.1.47 MySQL
-- 5.1.47.R1 with InnoDB Plugin 1.0.8 mysql5.1 mysql Use instead of mysql5.1
-- 5.1.48 MySQL 5.1.47.R1 with InnoDB Plugin 1.0.8 mysql5.1 mysql Use instead
-- of mysql5.1 5.1.49 MySQL 5.1.49-R1 with innodb plugin mysql5.1 mysql Use
-- instead of mysql5.1 5.1.50 MySQL 5.1.50-R3 mysql5.5 mysql Use instead of
-- mysql5.1 5.5.7 MySQL 5.5.7.R1 oracle-ee-11.2 oracle-ee Oracle Database
-- Server EE 11.2.0.2 Oracle EE release AL32UTF8 Unicode 5.0 UTF-8 Universal
-- character set oracle-ee-11.2 oracle-ee Oracle Database Server EE
-- 11.2.0.2.v2 First Oracle Enterprise Edition One - DB Engine Version
-- 11.2.0.2.v2 AL32UTF8 Unicode 5.0 UTF-8 Universal character set
-- oracle-ee-11.2 oracle-ee Oracle Database Server EE 11.2.0.2.v3 Oracle EE
-- release AL32UTF8 Unicode 5.0 UTF-8 Universal character set oracle-se-11.2
-- oracle-se Oracle Database Server SE 11.2.0.2 Oracle SE release AL32UTF8
-- Unicode 5.0 UTF-8 Universal character set oracle-se-11.2 oracle-se Oracle
-- Database Server SE 11.2.0.2.v2 Oracle SE release AL32UTF8 Unicode 5.0 UTF-8
-- Universal character set oracle-se-11.2 oracle-se Oracle Database Server SE
-- 11.2.0.2.v3 Oracle SE release AL32UTF8 Unicode 5.0 UTF-8 Universal
-- character set oracle-se1-11.2 oracle-se1 Oracle Database Server SE1
-- 11.2.0.2 Oracle SE1 release AL32UTF8 Unicode 5.0 UTF-8 Universal character
-- set oracle-se1-11.2 oracle-se1 Oracle Database Server SE1 11.2.0.2.v2
-- Oracle SE1 release AL32UTF8 Unicode 5.0 UTF-8 Universal character set
-- oracle-se1-11.2 oracle-se1 Oracle Database Server SE1 11.2.0.2.v3 Oracle
-- SE1 release AL32UTF8 Unicode 5.0 UTF-8 Universal character set
-- 1162dc55-850f-11e0-90aa-eb648410240d.
--
-- See: 'Network.AWS.RDS.V2013_09_09.DescribeDBEngineVersions'

describeDBEngineVersions :: ( MonadCatch m
                            , MonadResource m
                            , MonadError AWS.Error m
                            , MonadReader Env (ResumableSource m)
                            )
    => State DescribeDBEngineVersions a
    -> ResumableSource m DescribeDBEngineVersionsResponse
describeDBEngineVersions s =
    paginate (mkDescribeDBEngineVersions &~ s)

describeDBEngineVersionsCatch :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadReader Env (ResumableSource m)
                                 )
    => State DescribeDBEngineVersions a
    -> ResumableSource m (Either ServiceErr DescribeDBEngineVersionsResponse)
describeDBEngineVersionsCatch s =
    paginateCatch (mkDescribeDBEngineVersions &~ s)

-- $DescribeDBInstances
-- Returns information about provisioned RDS instances. This API supports
-- pagination. https://rds.amazonaws.com/ ?Action=DescribeDBInstances
-- &Version=2013-05-15 &MaxRecords=100 &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2011-05-23T06%3A54%3A55.116Z
-- &AWSAccessKeyId= &Signature= 2011-05-23T06:50:00Z mysql 1 false
-- general-public-license available 5.1.50 3306
-- simcoprod01.cu7u2t4uz396.us-east-1.rds.amazonaws.com simcoprod01 in-sync
-- default.mysql5.1 active default 00:00-00:30 true sat:07:30-sat:08:00
-- us-east-1a 2011-05-23T06:06:43.110Z 10 default.mysql5.1 in-sync db.m1.large
-- master 9135fff3-8509-11e0-bd9b-a7b1ece36d51.
--
-- See: 'Network.AWS.RDS.V2013_09_09.DescribeDBInstances'

describeDBInstances :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env (ResumableSource m)
                       )
    => State DescribeDBInstances a
    -> ResumableSource m DescribeDBInstancesResponse
describeDBInstances s =
    paginate (mkDescribeDBInstances &~ s)

describeDBInstancesCatch :: ( MonadCatch m
                            , MonadResource m
                            , MonadReader Env (ResumableSource m)
                            )
    => State DescribeDBInstances a
    -> ResumableSource m (Either ServiceErr DescribeDBInstancesResponse)
describeDBInstancesCatch s =
    paginateCatch (mkDescribeDBInstances &~ s)

-- $DescribeDBLogFiles
-- Returns a list of DB log files for the DB instance.
-- https://rds.amazonaws.com/ ?DBInstanceIdentifier=rrak-mysql &MaxRecords=100
-- &Version=2013-02-12 &Action=DescribeDBLogFiles &SignatureVersion=4
-- &SignatureMethod=HmacSHA256 &Timestamp=20130327T173621Z
-- &X-Amz-Algorithm=AWS4-HMAC-SHA256 &X-Amz-Date=20130327T173621Z
-- &X-Amz-SignedHeaders=Host &X-Amz-Expires=20130327T173621Z
-- &X-Amz-Credential= &X-Amz-Signature= 1364403600000
-- error/mysql-error-running.log 0 1364338800000
-- error/mysql-error-running.log.0 0 1364342400000
-- error/mysql-error-running.log.1 0 1364346000000
-- error/mysql-error-running.log.2 0 1364349600000
-- error/mysql-error-running.log.3 0 1364405700000 error/mysql-error.log 0
-- d70fb3b3-9704-11e2-a0db-871552e0ef19.
--
-- See: 'Network.AWS.RDS.V2013_09_09.DescribeDBLogFiles'

describeDBLogFiles :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env (ResumableSource m)
                      )
    => Text -- ^ 'ddblfDBInstanceIdentifier'
    -> State DescribeDBLogFiles a
    -> ResumableSource m DescribeDBLogFilesResponse
describeDBLogFiles p1 s =
    paginate $ (mkDescribeDBLogFiles p1) &~ s

describeDBLogFilesCatch :: ( MonadCatch m
                           , MonadResource m
                           , MonadReader Env (ResumableSource m)
                           )
    => Text -- ^ 'ddblfDBInstanceIdentifier'
    -> State DescribeDBLogFiles a
    -> ResumableSource m (Either ServiceErr DescribeDBLogFilesResponse)
describeDBLogFilesCatch p1 s =
    paginateCatch $ (mkDescribeDBLogFiles p1) &~ s

-- $DescribeDBParameterGroups
-- Returns a list of DBParameterGroup descriptions. If a DBParameterGroupName
-- is specified, the list will contain only the description of the specified
-- DB parameter group. https://rds.amazonaws.com/
-- ?Action=DescribeDBParameterGroups &DBParameterGroupName=myparamsgroup
-- &MaxRecords=100 &Version=2013-05-15 &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2011-02-15T17%3A54%3A32.899Z
-- &AWSAccessKeyId= &Signature= mysql5.1 Default parameter group for mysql5.1
-- default.mysql5.1 mysql5.1 My DB Param Group testdbparamgroup
-- cb8d9bb4-a02a-11df-bd60-c955b7d6e8e0.
--
-- See: 'Network.AWS.RDS.V2013_09_09.DescribeDBParameterGroups'

describeDBParameterGroups :: ( MonadCatch m
                             , MonadResource m
                             , MonadError AWS.Error m
                             , MonadReader Env (ResumableSource m)
                             )
    => State DescribeDBParameterGroups a
    -> ResumableSource m DescribeDBParameterGroupsResponse
describeDBParameterGroups s =
    paginate (mkDescribeDBParameterGroups &~ s)

describeDBParameterGroupsCatch :: ( MonadCatch m
                                  , MonadResource m
                                  , MonadReader Env (ResumableSource m)
                                  )
    => State DescribeDBParameterGroups a
    -> ResumableSource m (Either ServiceErr DescribeDBParameterGroupsResponse)
describeDBParameterGroupsCatch s =
    paginateCatch (mkDescribeDBParameterGroups &~ s)

-- $DescribeDBParameters
-- Returns the detailed parameter list for a particular DB parameter group.
-- https://rds.amazonaws.com/ ?Action=DescribeDBParameters
-- &DBParameterGroupName=mydbparametergroup &Source=system &MaxRecords=100
-- &Version=2013-05-15 &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-05-11T19%3A31%3A42.262Z &AWSAccessKeyId= &Signature=
-- /rdsdbbin/mysql string system false The MySQL installation base directory.
-- static basedir 32768 integer system true The size of the cache to hold the
-- SQL statements for the binary log during a transaction. dynamic
-- 4096-9223372036854775807 binlog_cache_size
-- 8743f2cf-bf41-11de-8c8e-49155882c409.
--
-- See: 'Network.AWS.RDS.V2013_09_09.DescribeDBParameters'

describeDBParameters :: ( MonadCatch m
                        , MonadResource m
                        , MonadError AWS.Error m
                        , MonadReader Env (ResumableSource m)
                        )
    => Text -- ^ 'ddbpDBParameterGroupName'
    -> State DescribeDBParameters a
    -> ResumableSource m DescribeDBParametersResponse
describeDBParameters p1 s =
    paginate $ (mkDescribeDBParameters p1) &~ s

describeDBParametersCatch :: ( MonadCatch m
                             , MonadResource m
                             , MonadReader Env (ResumableSource m)
                             )
    => Text -- ^ 'ddbpDBParameterGroupName'
    -> State DescribeDBParameters a
    -> ResumableSource m (Either ServiceErr DescribeDBParametersResponse)
describeDBParametersCatch p1 s =
    paginateCatch $ (mkDescribeDBParameters p1) &~ s

-- $DescribeDBSecurityGroups
-- Returns a list of DBSecurityGroup descriptions. If a DBSecurityGroupName is
-- specified, the list will contain only the descriptions of the specified DB
-- security group. https://rds.amazonaws.com/ ?Action=DescribeDBSecurityGroups
-- &Version=2013-05-15 &MaxRecords=100 &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2011-02-15T19%3A40%3A19.926Z
-- &AWSAccessKeyId= &Signature= authorized myec2securitygroup 054794666394
-- default 127.0.0.1/30 authorized 621567473609 default vpc-1ab2c3d4 My new
-- DBSecurityGroup 192.168.1.1/24 authorized 621567473609 mydbsecuritygroup
-- vpc-1ab2c3d5 My new DBSecurityGroup 621567473609 mydbsecuritygroup4
-- vpc-1ab2c3d6 bbdad154-bf42-11de-86a4-97241dfaadff.
--
-- See: 'Network.AWS.RDS.V2013_09_09.DescribeDBSecurityGroups'

describeDBSecurityGroups :: ( MonadCatch m
                            , MonadResource m
                            , MonadError AWS.Error m
                            , MonadReader Env (ResumableSource m)
                            )
    => State DescribeDBSecurityGroups a
    -> ResumableSource m DescribeDBSecurityGroupsResponse
describeDBSecurityGroups s =
    paginate (mkDescribeDBSecurityGroups &~ s)

describeDBSecurityGroupsCatch :: ( MonadCatch m
                                 , MonadResource m
                                 , MonadReader Env (ResumableSource m)
                                 )
    => State DescribeDBSecurityGroups a
    -> ResumableSource m (Either ServiceErr DescribeDBSecurityGroupsResponse)
describeDBSecurityGroupsCatch s =
    paginateCatch (mkDescribeDBSecurityGroups &~ s)

-- $DescribeDBSnapshots
-- Returns information about DB snapshots. This API supports pagination.
-- https://rds.amazon.com/ ?Action=DescribeDBSnapshots &MaxRecords=100
-- &Version=2013-05-15 &Timestamp=2011-05-23T06%3A27%3A42.551Z
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256 &AWSAccessKeyId=
-- &Signature= 3306 2011-05-23T06:29:03.483Z mysql available us-east-1a
-- general-public-license 2011-05-23T06:06:43.110Z 10 simcoprod01 5.1.50
-- mydbsnapshot manual master myoptiongroupname 3306 2011-03-11T07:20:24.082Z
-- mysql available us-east-1a general-public-license 2010-08-04T23:27:36.420Z
-- 50 mydbinstance 5.1.49 mysnapshot1 manual sa myoptiongroupname 3306
-- 2012-04-02T00:01:24.082Z mysql available us-east-1d general-public-license
-- 2010-07-16T00:06:59.107Z 60 simcoprod01 5.1.47
-- rds:simcoprod01-2012-04-02-00-01 automated master myoptiongroupname
-- c4191173-8506-11e0-90aa-eb648410240d.
--
-- See: 'Network.AWS.RDS.V2013_09_09.DescribeDBSnapshots'

describeDBSnapshots :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env (ResumableSource m)
                       )
    => State DescribeDBSnapshots a
    -> ResumableSource m DescribeDBSnapshotsResponse
describeDBSnapshots s =
    paginate (mkDescribeDBSnapshots &~ s)

describeDBSnapshotsCatch :: ( MonadCatch m
                            , MonadResource m
                            , MonadReader Env (ResumableSource m)
                            )
    => State DescribeDBSnapshots a
    -> ResumableSource m (Either ServiceErr DescribeDBSnapshotsResponse)
describeDBSnapshotsCatch s =
    paginateCatch (mkDescribeDBSnapshots &~ s)

-- $DescribeDBSubnetGroups
-- Returns a list of DBSubnetGroup descriptions. If a DBSubnetGroupName is
-- specified, the list will contain only the descriptions of the specified
-- DBSubnetGroup. For an overview of CIDR ranges, go to the Wikipedia
-- Tutorial. https://rds.amazonaws.com/ ?Action=DescribeDBSubnetGroups
-- &Version=2013-05-15 &MaxRecords=100 &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2011-02-15T19%3A40%3A19.926Z
-- &AWSAccessKeyId= &Signature= 990524496922 Complete description subnet_grp1
-- Active subnet-7c5b4115 us-east-1c Active subnet-7b5b4112 us-east-1b Active
-- subnet-3ea6bd57 us-east-1d 990524496922 Complete description subnet_grp2
-- Active subnet-7c5b4115 us-east-1c Active subnet-7b5b4112 us-east-1b Active
-- subnet-3ea6bd57 us-east-1d 31d0faee-229b-11e1-81f1-df3a2a803dad.
--
-- See: 'Network.AWS.RDS.V2013_09_09.DescribeDBSubnetGroups'

describeDBSubnetGroups :: ( MonadCatch m
                          , MonadResource m
                          , MonadError AWS.Error m
                          , MonadReader Env (ResumableSource m)
                          )
    => State DescribeDBSubnetGroups a
    -> ResumableSource m DescribeDBSubnetGroupsResponse
describeDBSubnetGroups s =
    paginate (mkDescribeDBSubnetGroups &~ s)

describeDBSubnetGroupsCatch :: ( MonadCatch m
                               , MonadResource m
                               , MonadReader Env (ResumableSource m)
                               )
    => State DescribeDBSubnetGroups a
    -> ResumableSource m (Either ServiceErr DescribeDBSubnetGroupsResponse)
describeDBSubnetGroupsCatch s =
    paginateCatch (mkDescribeDBSubnetGroups &~ s)

-- $DescribeEngineDefaultParameters
-- Returns the default engine and system parameter information for the
-- specified database engine. https://rds.amazonaws.com/
-- ?Action=DescribeEngineDefaultParameters &DBParameterGroupFamily=mysql5.1
-- &Version=2013-05-15 &MaxRecords=100 &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2011-02-15T19%3A10%3A03.510Z
-- &AWSAccessKeyId= &Signature= bG93ZXJfY2FzZV90YWJsZV9uYW1lcw== mysql5.1
-- boolean engine-default false Controls whether user-defined functions that
-- have only an xxx symbol for the main function can be loaded static 0,1
-- allow-suspicious-udfs integer engine-default true Intended for use with
-- master-to-master replication, and can be used to control the operation of
-- AUTO_INCREMENT columns dynamic 1-65535 auto_increment_increment integer
-- engine-default true Determines the starting point for the AUTO_INCREMENT
-- column value dynamic 1-65535 auto_increment_offset
-- 6c1341eb-a124-11df-bf5c-973b09643c5d.
--
-- See: 'Network.AWS.RDS.V2013_09_09.DescribeEngineDefaultParameters'

describeEngineDefaultParameters :: ( MonadCatch m
                                   , MonadResource m
                                   , MonadError AWS.Error m
                                   , MonadReader Env (ResumableSource m)
                                   )
    => Text -- ^ 'dedpDBParameterGroupFamily'
    -> State DescribeEngineDefaultParameters a
    -> ResumableSource m DescribeEngineDefaultParametersResponse
describeEngineDefaultParameters p1 s =
    paginate $ (mkDescribeEngineDefaultParameters p1) &~ s

describeEngineDefaultParametersCatch :: ( MonadCatch m
                                        , MonadResource m
                                        , MonadReader Env (ResumableSource m)
                                        )
    => Text -- ^ 'dedpDBParameterGroupFamily'
    -> State DescribeEngineDefaultParameters a
    -> ResumableSource m (Either ServiceErr DescribeEngineDefaultParametersResponse)
describeEngineDefaultParametersCatch p1 s =
    paginateCatch $ (mkDescribeEngineDefaultParameters p1) &~ s

-- $DescribeEventCategories
-- Displays a list of categories for all event source types, or, if specified,
-- for a specified source type. You can see a list of the event categories and
-- source types in the Events topic in the Amazon RDS User Guide.
-- https://rds.us-east-1.amazonaws.com/ ?Action=DescribeEventCategories
-- &SourceType=db-instance &Version=2013-01-10 &SignatureVersion=4
-- &SignatureMethod=HmacSHA256 &Timestamp=20130128T013452Z &AWSAccessKeyId=
-- &Signature= db-instance failover low storage maintenance recovery
-- restoration deletion configuration change failover availability creation
-- backup notification ea3bf54b-68ea-11e2-bd13-a92da73b3119.
--
-- See: 'Network.AWS.RDS.V2013_09_09.DescribeEventCategories'

describeEventCategories :: ( MonadCatch m
                           , MonadResource m
                           , MonadError AWS.Error m
                           , MonadReader Env m
                           )
    => State DescribeEventCategories a
    -> m DescribeEventCategoriesResponse
describeEventCategories s =
    send (mkDescribeEventCategories &~ s)

describeEventCategoriesCatch :: ( MonadCatch m
                                , MonadResource m
                                , MonadReader Env m
                                )
    => State DescribeEventCategories a
    -> m (Either ServiceErr DescribeEventCategoriesResponse)
describeEventCategoriesCatch s =
    sendCatch (mkDescribeEventCategories &~ s)

-- $DescribeEventSubscriptions
-- Lists all the subscription descriptions for a customer account. The
-- description for a subscription includes SubscriptionName, SNSTopicARN,
-- CustomerID, SourceType, SourceID, CreationTime, and Status. If you specify
-- a SubscriptionName, lists the description for that subscription.
-- https://rds.us-east-1.amazonaws.com/ ?Action=DescribeEventSubscriptions
-- &MaxRecords=100 &Version=2013-01-10 &SignatureVersion=4
-- &SignatureMethod=HmacSHA256 &Timestamp=20130128T004543Z &AWSAccessKeyId=
-- &Signature= true 012345678901 active 2013-01-28 00:29:23.736
-- EventSubscription01 arn:aws:sns:us-east-1:012345678901:EventSubscription01
-- true 012345678901 active 2013-01-28 00:29:42.851 EventSubscription02
-- arn:aws:sns:us-east-1:012345678901:EventSubscription01
-- 0ce48079-68e4-11e2-91fe-5daa8e68c7d4.
--
-- See: 'Network.AWS.RDS.V2013_09_09.DescribeEventSubscriptions'

describeEventSubscriptions :: ( MonadCatch m
                              , MonadResource m
                              , MonadError AWS.Error m
                              , MonadReader Env (ResumableSource m)
                              )
    => State DescribeEventSubscriptions a
    -> ResumableSource m DescribeEventSubscriptionsResponse
describeEventSubscriptions s =
    paginate (mkDescribeEventSubscriptions &~ s)

describeEventSubscriptionsCatch :: ( MonadCatch m
                                   , MonadResource m
                                   , MonadReader Env (ResumableSource m)
                                   )
    => State DescribeEventSubscriptions a
    -> ResumableSource m (Either ServiceErr DescribeEventSubscriptionsResponse)
describeEventSubscriptionsCatch s =
    paginateCatch (mkDescribeEventSubscriptions &~ s)

-- $DescribeEvents
-- Returns events related to DB instances, DB security groups, DB snapshots,
-- and DB parameter groups for the past 14 days. Events specific to a
-- particular DB instance, DB security group, database snapshot, or DB
-- parameter group can be obtained by providing the name as a parameter. By
-- default, the past hour of events are returned. https://rds.amazonaws.com/
-- ?Action=DescribeEvents &Duration=1440 &MaxRecords=100 &Version=2013-05-15
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-02-15T20%3A00%3A44.420Z &AWSAccessKeyId= &Signature=
-- Applied change to security group db-security-group 2010-08-11T17:12:52.860Z
-- mydbsecuritygroup Database instance created db-instance
-- 2010-08-11T18:10:15.269Z mydbinstance3 Backing up database instance
-- db-instance 2010-08-11T18:10:34.690Z mydbinstance3 Backing up DB instance
-- db-instance 2010-08-11T18:25:52.263Z mynewdbinstance Creating user snapshot
-- db-snapshot 2010-08-11T18:25:52.263Z mynewdbsnapshot3
-- 95b948cd-bf45-11de-86a4-97241dfaadff.
--
-- See: 'Network.AWS.RDS.V2013_09_09.DescribeEvents'

describeEvents :: ( MonadCatch m
                  , MonadResource m
                  , MonadError AWS.Error m
                  , MonadReader Env (ResumableSource m)
                  )
    => State DescribeEvents a
    -> ResumableSource m DescribeEventsResponse
describeEvents s =
    paginate (mkDescribeEvents &~ s)

describeEventsCatch :: ( MonadCatch m
                       , MonadResource m
                       , MonadReader Env (ResumableSource m)
                       )
    => State DescribeEvents a
    -> ResumableSource m (Either ServiceErr DescribeEventsResponse)
describeEventsCatch s =
    paginateCatch (mkDescribeEvents &~ s)

-- $DescribeOptionGroupOptions
-- Describes all available options. https://rds.amazonaws.com/
-- ?Action=DescribeOptionGroupOptions &EngineName=oracle-se1
-- &MajorEngineVersion=11.2 11.2 true Oracle Enterprise Manager 1158 OEM
-- oracle-se1 0.2.v3 false false d9c8f6a1-84c7-11e1-a264-0b23c28bc344.
--
-- See: 'Network.AWS.RDS.V2013_09_09.DescribeOptionGroupOptions'

describeOptionGroupOptions :: ( MonadCatch m
                              , MonadResource m
                              , MonadError AWS.Error m
                              , MonadReader Env (ResumableSource m)
                              )
    => Text -- ^ 'dogoEngineName'
    -> State DescribeOptionGroupOptions a
    -> ResumableSource m DescribeOptionGroupOptionsResponse
describeOptionGroupOptions p1 s =
    paginate $ (mkDescribeOptionGroupOptions p1) &~ s

describeOptionGroupOptionsCatch :: ( MonadCatch m
                                   , MonadResource m
                                   , MonadReader Env (ResumableSource m)
                                   )
    => Text -- ^ 'dogoEngineName'
    -> State DescribeOptionGroupOptions a
    -> ResumableSource m (Either ServiceErr DescribeOptionGroupOptionsResponse)
describeOptionGroupOptionsCatch p1 s =
    paginateCatch $ (mkDescribeOptionGroupOptions p1) &~ s

-- $DescribeOptionGroups
-- Describes the available option groups. https://rds.amazonaws.com/
-- ?Action=DescribeOptionGroups &OptionGroupName=myoptiongroup &MaxRecords=100
-- 11.2 myoptiongroup oracle-se1 Test option group
-- 6088823d-84c8-11e1-a264-0b23c28bc344 https://rds.amazonaws.com/
-- ?Action=DescribeOptionGroups &MaxRecords=100 11.2 myoptiongroup oracle-se1
-- Test option group 11.2 default:oracle-se1-11-2 oracle-se1 Default option
-- group. e4b234d9-84d5-11e1-87a6-71059839a52b.
--
-- See: 'Network.AWS.RDS.V2013_09_09.DescribeOptionGroups'

describeOptionGroups :: ( MonadCatch m
                        , MonadResource m
                        , MonadError AWS.Error m
                        , MonadReader Env (ResumableSource m)
                        )
    => State DescribeOptionGroups a
    -> ResumableSource m DescribeOptionGroupsResponse
describeOptionGroups s =
    paginate (mkDescribeOptionGroups &~ s)

describeOptionGroupsCatch :: ( MonadCatch m
                             , MonadResource m
                             , MonadReader Env (ResumableSource m)
                             )
    => State DescribeOptionGroups a
    -> ResumableSource m (Either ServiceErr DescribeOptionGroupsResponse)
describeOptionGroupsCatch s =
    paginateCatch (mkDescribeOptionGroups &~ s)

-- $DescribeOrderableDBInstanceOptions
-- Returns a list of orderable DB instance options for the specified engine.
-- https://rds.amazonaws.com/ ?Action=DescribeOrderableDBInstanceOptions
-- &Engine=mysql &MaxRecords=100 &Version=2013-05-15
-- &Timestamp=2011-05-23T07%3A49%3A17.749Z &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &AWSAccessKeyId= &Signature= true mysql
-- general-public-license true 5.1.45 db.m1.large us-east-1a yes us-east-1b no
-- us-east-1d yes true mysql general-public-license true 5.1.45 db.m1.small
-- us-east-1a yes us-east-1b yes us-east-1d yes true mysql
-- general-public-license true 5.1.45 db.m1.xlarge us-east-1a yes us-east-1b
-- yes us-east-1d yes true mysql general-public-license true 5.1.45
-- db.m2.2xlarge us-east-1a yes us-east-1b yes us-east-1d yes true mysql
-- general-public-license true 5.1.45 db.m2.4xlarge us-east-1a yes us-east-1b
-- no us-east-1d no 2a0406d7-8511-11e0-90aa-eb648410240d.
--
-- See: 'Network.AWS.RDS.V2013_09_09.DescribeOrderableDBInstanceOptions'

describeOrderableDBInstanceOptions :: ( MonadCatch m
                                      , MonadResource m
                                      , MonadError AWS.Error m
                                      , MonadReader Env (ResumableSource m)
                                      )
    => Text -- ^ 'dodbioEngine'
    -> State DescribeOrderableDBInstanceOptions a
    -> ResumableSource m DescribeOrderableDBInstanceOptionsResponse
describeOrderableDBInstanceOptions p1 s =
    paginate $ (mkDescribeOrderableDBInstanceOptions p1) &~ s

describeOrderableDBInstanceOptionsCatch :: ( MonadCatch m
                                           , MonadResource m
                                           , MonadReader Env (ResumableSource m)
                                           )
    => Text -- ^ 'dodbioEngine'
    -> State DescribeOrderableDBInstanceOptions a
    -> ResumableSource m (Either ServiceErr DescribeOrderableDBInstanceOptionsResponse)
describeOrderableDBInstanceOptionsCatch p1 s =
    paginateCatch $ (mkDescribeOrderableDBInstanceOptions p1) &~ s

-- $DescribeReservedDBInstances
-- Returns information about reserved DB instances for this account, or about
-- a specified reserved DB instance. https://rds.amazonaws.com/
-- ?Action=DescribeReservedDBInstances
-- &ReservedDBInstanceId=customerSpecifiedID &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2012-12-18T18%3A31%3A36.118Z
-- &AWSAccessKeyId= &Signature= Medium Utilization USD mysql
-- 649fd0c8-cf6d-47a0-bfa6-060f8e75e95f false active myreservationid 1
-- 2010-12-15T00:25:14.131Z 31536000 227.5 0.046 db.m1.small
-- c695119b-2961-11e1-bd06-6fe008f046c3.
--
-- See: 'Network.AWS.RDS.V2013_09_09.DescribeReservedDBInstances'

describeReservedDBInstances :: ( MonadCatch m
                               , MonadResource m
                               , MonadError AWS.Error m
                               , MonadReader Env (ResumableSource m)
                               )
    => State DescribeReservedDBInstances a
    -> ResumableSource m DescribeReservedDBInstancesResponse
describeReservedDBInstances s =
    paginate (mkDescribeReservedDBInstances &~ s)

describeReservedDBInstancesCatch :: ( MonadCatch m
                                    , MonadResource m
                                    , MonadReader Env (ResumableSource m)
                                    )
    => State DescribeReservedDBInstances a
    -> ResumableSource m (Either ServiceErr DescribeReservedDBInstancesResponse)
describeReservedDBInstancesCatch s =
    paginateCatch (mkDescribeReservedDBInstances &~ s)

-- $DescribeReservedDBInstancesOfferings
-- Lists available reserved DB instance offerings. https://rds.amazonaws.com/
-- ?Action=DescribeReservedDBInstancesOfferings
-- &ReservedDBInstancesOfferingId=438012d3-4052-4cc7-b2e3-8d3372e0e706
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-12-18T18%3A31%3A36.118Z &AWSAccessKeyId= &Signature=
-- c/2012-04-02/"> 31536000 Heavy Utilization USD Hourly 0.123 162.0 mysql 0.0
-- false SampleOfferingId db.m1.small 521b420a-2961-11e1-bd06-6fe008f046c3.
--
-- See: 'Network.AWS.RDS.V2013_09_09.DescribeReservedDBInstancesOfferings'

describeReservedDBInstancesOfferings :: ( MonadCatch m
                                        , MonadResource m
                                        , MonadError AWS.Error m
                                        , MonadReader Env (ResumableSource m)
                                        )
    => State DescribeReservedDBInstancesOfferings a
    -> ResumableSource m DescribeReservedDBInstancesOfferingsResponse
describeReservedDBInstancesOfferings s =
    paginate (mkDescribeReservedDBInstancesOfferings &~ s)

describeReservedDBInstancesOfferingsCatch :: ( MonadCatch m
                                             , MonadResource m
                                             , MonadReader Env (ResumableSource m)
                                             )
    => State DescribeReservedDBInstancesOfferings a
    -> ResumableSource m (Either ServiceErr DescribeReservedDBInstancesOfferingsResponse)
describeReservedDBInstancesOfferingsCatch s =
    paginateCatch (mkDescribeReservedDBInstancesOfferings &~ s)

-- $ListTagsForResource
-- Lists all tags on an Amazon RDS resource. For an overview on tagging an
-- Amazon RDS resource, see Tagging Amazon RDS Resources.
--
-- See: 'Network.AWS.RDS.V2013_09_09.ListTagsForResource'

listTagsForResource :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
    => Text -- ^ 'ltfrResourceName'
    -> State ListTagsForResource a
    -> m ListTagsForResourceResponse
listTagsForResource p1 s =
    send $ (mkListTagsForResource p1) &~ s

listTagsForResourceCatch :: ( MonadCatch m
                            , MonadResource m
                            , MonadReader Env m
                            )
    => Text -- ^ 'ltfrResourceName'
    -> State ListTagsForResource a
    -> m (Either ServiceErr ListTagsForResourceResponse)
listTagsForResourceCatch p1 s =
    sendCatch $ (mkListTagsForResource p1) &~ s

-- $ModifyDBInstance
-- Modify settings for a DB instance. You can change one or more database
-- configuration parameters by specifying these parameters and the new values
-- in the request. https://rds.amazonaws.com/ ?Action=ModifyDBInstance
-- &DBInstanceIdentifier=simcoprod01 &AllocatedStorage=50 &Version=2013-05-15
-- &ApplyImmediately=false &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-05-23T08%3A02%3A09.574Z &AWSAccessKeyId= &Signature=
-- 2011-05-23T08:00:00Z mysql 50 1 false general-public-license available
-- 5.1.50 3306 simcoprod01.cu7u2t4uz396.us-east-1.rds.amazonaws.com
-- simcoprod01 in-sync default.mysql5.1 active default 00:00-00:30 true
-- sat:07:30-sat:08:00 us-east-1a 2011-05-23T06:06:43.110Z 10 db.m1.large
-- master f61a020f-8512-11e0-90aa-eb648410240d.
--
-- See: 'Network.AWS.RDS.V2013_09_09.ModifyDBInstance'

modifyDBInstance :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => Text -- ^ 'mdbiDBInstanceIdentifier'
    -> State ModifyDBInstance a
    -> m ModifyDBInstanceResponse
modifyDBInstance p1 s =
    send $ (mkModifyDBInstance p1) &~ s

modifyDBInstanceCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => Text -- ^ 'mdbiDBInstanceIdentifier'
    -> State ModifyDBInstance a
    -> m (Either ServiceErr ModifyDBInstanceResponse)
modifyDBInstanceCatch p1 s =
    sendCatch $ (mkModifyDBInstance p1) &~ s

-- $ModifyDBParameterGroup
-- Modifies the parameters of a DB parameter group. To modify more than one
-- parameter, submit a list of the following: ParameterName, ParameterValue,
-- and ApplyMethod. A maximum of 20 parameters can be modified in a single
-- request. The apply-immediate method can be used only for dynamic
-- parameters; the pending-reboot method can be used with MySQL and Oracle DB
-- instances for either dynamic or static parameters. For Microsoft SQL Server
-- DB instances, the pending-reboot method can be used only for static
-- parameters. https://rds.amazonaws.com/ ?Action=ModifyDBParameterGroup
-- &DBParameterGroupName=mydbparametergroup
-- &Parameters.member.1.ParameterName=max_user_connections
-- &Parameters.member.1.ParameterValue=24
-- &Parameters.member.1.ApplyMethod=pending-reboot
-- &Parameters.member.2.ParameterName=max_allowed_packet
-- &Parameters.member.2.ParameterValue=1024
-- &Parameters.member.2.ApplyMethod=immediate &Version=2013-05-15
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-05-11T21%3A25%3A00.686Z &AWSAccessKeyId= &Signature=
-- mydbparametergroup 5ba91f97-bf51-11de-bf60-ef2e377db6f3.
--
-- See: 'Network.AWS.RDS.V2013_09_09.ModifyDBParameterGroup'

modifyDBParameterGroup :: ( MonadCatch m
                          , MonadResource m
                          , MonadError AWS.Error m
                          , MonadReader Env m
                          )
    => Text -- ^ 'mdbpgDBParameterGroupName'
    -> [Parameter] -- ^ 'mdbpgParameters'
    -> State ModifyDBParameterGroup a
    -> m ModifyDBParameterGroupResponse
modifyDBParameterGroup p1 p2 s =
    send $ (mkModifyDBParameterGroup p1 p2) &~ s

modifyDBParameterGroupCatch :: ( MonadCatch m
                               , MonadResource m
                               , MonadReader Env m
                               )
    => Text -- ^ 'mdbpgDBParameterGroupName'
    -> [Parameter] -- ^ 'mdbpgParameters'
    -> State ModifyDBParameterGroup a
    -> m (Either ServiceErr ModifyDBParameterGroupResponse)
modifyDBParameterGroupCatch p1 p2 s =
    sendCatch $ (mkModifyDBParameterGroup p1 p2) &~ s

-- $ModifyDBSubnetGroup
-- Modifies an existing DB subnet group. DB subnet groups must contain at
-- least one subnet in at least two AZs in the region.
-- https://rds.amazonaws.com/ ?Action=ModifyDBSubnetGroup
-- &DBSubnetGroupName=mydbsubnetgroup
-- &DBSubnetGroupDescription=My%20modified%20DBSubnetGroup &Version=2013-05-15
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-02-15T18%3A14%3A49.482Z &AWSAccessKeyId= &Signature=
-- 990524496922 Complete My modified DBSubnetGroup mydbsubnetgroup Active
-- subnet-7c5b4115 us-east-1c Active subnet-7b5b4112 us-east-1b Active
-- subnet-3ea6bd57 us-east-1d ed662948-a57b-11df-9e38-7ffab86c801f.
--
-- See: 'Network.AWS.RDS.V2013_09_09.ModifyDBSubnetGroup'

modifyDBSubnetGroup :: ( MonadCatch m
                       , MonadResource m
                       , MonadError AWS.Error m
                       , MonadReader Env m
                       )
    => Text -- ^ 'mdbsgDBSubnetGroupName'
    -> [Text] -- ^ 'mdbsgSubnetIds'
    -> State ModifyDBSubnetGroup a
    -> m ModifyDBSubnetGroupResponse
modifyDBSubnetGroup p1 p3 s =
    send $ (mkModifyDBSubnetGroup p1 p3) &~ s

modifyDBSubnetGroupCatch :: ( MonadCatch m
                            , MonadResource m
                            , MonadReader Env m
                            )
    => Text -- ^ 'mdbsgDBSubnetGroupName'
    -> [Text] -- ^ 'mdbsgSubnetIds'
    -> State ModifyDBSubnetGroup a
    -> m (Either ServiceErr ModifyDBSubnetGroupResponse)
modifyDBSubnetGroupCatch p1 p3 s =
    sendCatch $ (mkModifyDBSubnetGroup p1 p3) &~ s

-- $ModifyEventSubscription
-- Modifies an existing RDS event notification subscription. Note that you
-- cannot modify the source identifiers using this call; to change source
-- identifiers for a subscription, use the AddSourceIdentifierToSubscription
-- and RemoveSourceIdentifierFromSubscription calls. You can see a list of the
-- event categories for a given SourceType in the Events topic in the Amazon
-- RDS User Guide or by using the DescribeEventCategories action.
-- https://rds.us-east-1.amazonaws.com/ ?Action=ModifyEventSubscription
-- &SubscriptionName=EventSubscription01 &EventCategories.member.1=creation
-- &EventCategories.member.2=deletion &SourceType=db-instance &Enabled=true
-- &Version=2013-01-10 &SignatureVersion=4 &SignatureMethod=HmacSHA256
-- &Timestamp=20130128T005359Z &AWSAccessKeyId= &Signature= true 012345678901
-- db-instance modifying 2013-01-28 00:29:23.736 creation deletion
-- EventSubscription01 arn:aws:sns:us-east-1:012345678901:EventSubscription01
-- 34907d48-68e5-11e2-98ef-2b071ac20a57.
--
-- See: 'Network.AWS.RDS.V2013_09_09.ModifyEventSubscription'

modifyEventSubscription :: ( MonadCatch m
                           , MonadResource m
                           , MonadError AWS.Error m
                           , MonadReader Env m
                           )
    => Text -- ^ 'mesSubscriptionName'
    -> State ModifyEventSubscription a
    -> m ModifyEventSubscriptionResponse
modifyEventSubscription p1 s =
    send $ (mkModifyEventSubscription p1) &~ s

modifyEventSubscriptionCatch :: ( MonadCatch m
                                , MonadResource m
                                , MonadReader Env m
                                )
    => Text -- ^ 'mesSubscriptionName'
    -> State ModifyEventSubscription a
    -> m (Either ServiceErr ModifyEventSubscriptionResponse)
modifyEventSubscriptionCatch p1 s =
    sendCatch $ (mkModifyEventSubscription p1) &~ s

-- $ModifyOptionGroup
-- Modifies an existing option group. https://rds.amazonaws.com/
-- ?Action=ModifyOptionGroup &OptionGroupName=myoptiongroup
-- &OptionsToInclude=OEM &DBSecurityGroupMemberships=default
-- &ApplyImmediately=true myoptiongroup Test option group oracle-se1 11.2 OEM
-- Oracle Enterprise Manager 1158 default ACTIVE
-- ed662948-a57b-11df-9e38-7ffab86c801f https://rds.amazonaws.com/
-- ?Action=ModifyOptionGroup &OptionGroupName=myoptiongroup
-- &OptionsToRemove=OEM &ApplyImmediately=true myoptiongroup Test option group
-- oracle-se1 11.2 ed662948-a57b-11df-9e38-7ffab86c801f.
--
-- See: 'Network.AWS.RDS.V2013_09_09.ModifyOptionGroup'

modifyOptionGroup :: ( MonadCatch m
                     , MonadResource m
                     , MonadError AWS.Error m
                     , MonadReader Env m
                     )
    => Text -- ^ 'mogOptionGroupName'
    -> State ModifyOptionGroup a
    -> m ModifyOptionGroupResponse
modifyOptionGroup p1 s =
    send $ (mkModifyOptionGroup p1) &~ s

modifyOptionGroupCatch :: ( MonadCatch m
                          , MonadResource m
                          , MonadReader Env m
                          )
    => Text -- ^ 'mogOptionGroupName'
    -> State ModifyOptionGroup a
    -> m (Either ServiceErr ModifyOptionGroupResponse)
modifyOptionGroupCatch p1 s =
    sendCatch $ (mkModifyOptionGroup p1) &~ s

-- $PromoteReadReplica
-- Promotes a read replica DB instance to a standalone DB instance.
-- https://rds.amazonaws.com/ ?Action=PromoteReadReplica
-- &DBInstanceIdentifier=simcoprod01 &Version=2013-05-15 &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2012-08-23T08%3A02%3A09.574Z
-- &AWSAccessKeyId= &Signature= 2011-05-23T08:00:00Z mysql 50 1 false
-- general-public-license available 5.1.50 3306
-- simcoprod01.cu7u2t4uz396.us-east-1.rds.amazonaws.com simcoprod01 in-sync
-- default.mysql5.1 active default 00:00-00:30 true sat:07:30-sat:08:00
-- us-east-1a 2011-05-23T06:06:43.110Z 10 db.m1.large master
-- f61a020f-8512-11e0-90aa-eb648410240d.
--
-- See: 'Network.AWS.RDS.V2013_09_09.PromoteReadReplica'

promoteReadReplica :: ( MonadCatch m
                      , MonadResource m
                      , MonadError AWS.Error m
                      , MonadReader Env m
                      )
    => Text -- ^ 'prrDBInstanceIdentifier'
    -> State PromoteReadReplica a
    -> m PromoteReadReplicaResponse
promoteReadReplica p1 s =
    send $ (mkPromoteReadReplica p1) &~ s

promoteReadReplicaCatch :: ( MonadCatch m
                           , MonadResource m
                           , MonadReader Env m
                           )
    => Text -- ^ 'prrDBInstanceIdentifier'
    -> State PromoteReadReplica a
    -> m (Either ServiceErr PromoteReadReplicaResponse)
promoteReadReplicaCatch p1 s =
    sendCatch $ (mkPromoteReadReplica p1) &~ s

-- $PurchaseReservedDBInstancesOffering
-- Purchases a reserved DB instance offering. https://rds.amazonaws.com/
-- ?Action=PurchaseReservedDBInstancesOffering
-- &ReservedDBInstanceId=myreservationID
-- &ReservedDBInstancesOfferingId=438012d3-4052-4cc7-b2e3-8d3372e0e706
-- &DBInstanceCount=1 &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-05-10T18%3A31%3A36.118Z &AWSAccessKeyId= &Signature= Medium
-- Utilization USD mysql 438012d3-4052-4cc7-b2e3-8d3372e0e706 true
-- payment-pending myreservationID 10 2011-12-18T23:24:56.577Z 31536000 123.0
-- 0.123 db.m1.small 7f099901-29cf-11e1-bd06-6fe008f046c3.
--
-- See: 'Network.AWS.RDS.V2013_09_09.PurchaseReservedDBInstancesOffering'

purchaseReservedDBInstancesOffering :: ( MonadCatch m
                                       , MonadResource m
                                       , MonadError AWS.Error m
                                       , MonadReader Env m
                                       )
    => Text -- ^ 'prdbioReservedDBInstancesOfferingId'
    -> State PurchaseReservedDBInstancesOffering a
    -> m PurchaseReservedDBInstancesOfferingResponse
purchaseReservedDBInstancesOffering p1 s =
    send $ (mkPurchaseReservedDBInstancesOffering p1) &~ s

purchaseReservedDBInstancesOfferingCatch :: ( MonadCatch m
                                            , MonadResource m
                                            , MonadReader Env m
                                            )
    => Text -- ^ 'prdbioReservedDBInstancesOfferingId'
    -> State PurchaseReservedDBInstancesOffering a
    -> m (Either ServiceErr PurchaseReservedDBInstancesOfferingResponse)
purchaseReservedDBInstancesOfferingCatch p1 s =
    sendCatch $ (mkPurchaseReservedDBInstancesOffering p1) &~ s

-- $RebootDBInstance
-- Rebooting a DB instance restarts the database engine service. A reboot also
-- applies to the DB instance any modifications to the associated DB parameter
-- group that were pending. Rebooting a DB instance results in a momentary
-- outage of the instance, during which the DB instance status is set to
-- rebooting. If the RDS instance is configured for MultiAZ, it is possible
-- that the reboot will be conducted through a failover. An Amazon RDS event
-- is created when the reboot is completed. If your DB instance is deployed in
-- multiple Availability Zones, you can force a failover from one AZ to the
-- other during the reboot. You might force a failover to test the
-- availability of your DB instance deployment or to restore operations to the
-- original AZ after a failover occurs. The time required to reboot is a
-- function of the specific database engine's crash recovery process. To
-- improve the reboot time, we recommend that you reduce database activities
-- as much as possible during the reboot process to reduce rollback activity
-- for in-transit transactions. https://rds.amazonaws.com/
-- ?Action=RebootDBInstance &DBInstanceIdentifier=simcoprod01
-- &Version=2013-05-15 &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-05-23T06%3A10%3A31.216Z &AWSAccessKeyId= &Signature=
-- 2011-05-23T06:07:38.831Z mysql 1 false general-public-license rebooting
-- 5.1.50 3306 simcoprod01.cu7u2t4uz396.us-east-1.rds.amazonaws.com
-- simcoprod01 in-sync default.mysql5.1 active default 00:00-00:30 true
-- sat:07:30-sat:08:00 us-east-1a 2011-05-23T06:06:43.110Z 10 db.m1.large
-- master 5d5df758-8503-11e0-90aa-eb648410240d.
--
-- See: 'Network.AWS.RDS.V2013_09_09.RebootDBInstance'

rebootDBInstance :: ( MonadCatch m
                    , MonadResource m
                    , MonadError AWS.Error m
                    , MonadReader Env m
                    )
    => Text -- ^ 'rdbi1DBInstanceIdentifier'
    -> State RebootDBInstance a
    -> m RebootDBInstanceResponse
rebootDBInstance p1 s =
    send $ (mkRebootDBInstance p1) &~ s

rebootDBInstanceCatch :: ( MonadCatch m
                         , MonadResource m
                         , MonadReader Env m
                         )
    => Text -- ^ 'rdbi1DBInstanceIdentifier'
    -> State RebootDBInstance a
    -> m (Either ServiceErr RebootDBInstanceResponse)
rebootDBInstanceCatch p1 s =
    sendCatch $ (mkRebootDBInstance p1) &~ s

-- $RemoveSourceIdentifierFromSubscription
-- Removes a source identifier from an existing RDS event notification
-- subscription. https://rds.us-east-1.amazonaws.com/
-- ?Action=RemoveSourceIdentifierFromSubscription
-- &SubscriptionName=EventSubscription01 &SourceIdentifier=dbinstance01
-- &Version=2013-01-10 &SignatureVersion=4 &SignatureMethod=HmacSHA256
-- &Timestamp=20130128T012415Z &AWSAccessKeyId= &Signature= true 012345678901
-- db-instance modifying 2013-01-28 00:29:23.736 creation deletion
-- EventSubscription01 arn:aws:sns:us-east-1:012345678901:EventSubscription01
-- 6f0b82bf-68e9-11e2-b97b-43c6362ec60d.
--
-- See: 'Network.AWS.RDS.V2013_09_09.RemoveSourceIdentifierFromSubscription'

removeSourceIdentifierFromSubscription :: ( MonadCatch m
                                          , MonadResource m
                                          , MonadError AWS.Error m
                                          , MonadReader Env m
                                          )
    => Text -- ^ 'rsifsSubscriptionName'
    -> Text -- ^ 'rsifsSourceIdentifier'
    -> State RemoveSourceIdentifierFromSubscription a
    -> m RemoveSourceIdentifierFromSubscriptionResponse
removeSourceIdentifierFromSubscription p1 p2 s =
    send $ (mkRemoveSourceIdentifierFromSubscription p1 p2) &~ s

removeSourceIdentifierFromSubscriptionCatch :: ( MonadCatch m
                                               , MonadResource m
                                               , MonadReader Env m
                                               )
    => Text -- ^ 'rsifsSubscriptionName'
    -> Text -- ^ 'rsifsSourceIdentifier'
    -> State RemoveSourceIdentifierFromSubscription a
    -> m (Either ServiceErr RemoveSourceIdentifierFromSubscriptionResponse)
removeSourceIdentifierFromSubscriptionCatch p1 p2 s =
    sendCatch $ (mkRemoveSourceIdentifierFromSubscription p1 p2) &~ s

-- $RemoveTagsFromResource
-- Removes metadata tags from an Amazon RDS resource. For an overview on
-- tagging an Amazon RDS resource, see Tagging Amazon RDS Resources.
--
-- See: 'Network.AWS.RDS.V2013_09_09.RemoveTagsFromResource'

removeTagsFromResource :: ( MonadCatch m
                          , MonadResource m
                          , MonadError AWS.Error m
                          , MonadReader Env m
                          )
    => Text -- ^ 'rtfrResourceName'
    -> [Text] -- ^ 'rtfrTagKeys'
    -> State RemoveTagsFromResource a
    -> m RemoveTagsFromResourceResponse
removeTagsFromResource p1 p2 s =
    send $ (mkRemoveTagsFromResource p1 p2) &~ s

removeTagsFromResourceCatch :: ( MonadCatch m
                               , MonadResource m
                               , MonadReader Env m
                               )
    => Text -- ^ 'rtfrResourceName'
    -> [Text] -- ^ 'rtfrTagKeys'
    -> State RemoveTagsFromResource a
    -> m (Either ServiceErr RemoveTagsFromResourceResponse)
removeTagsFromResourceCatch p1 p2 s =
    sendCatch $ (mkRemoveTagsFromResource p1 p2) &~ s

-- $ResetDBParameterGroup
-- Modifies the parameters of a DB parameter group to the engine/system
-- default value. To reset specific parameters submit a list of the following:
-- ParameterName and ApplyMethod. To reset the entire DB parameter group,
-- specify the DBParameterGroup name and ResetAllParameters parameters. When
-- resetting the entire group, dynamic parameters are updated immediately and
-- static parameters are set to pending-reboot to take effect on the next DB
-- instance restart or RebootDBInstance request. https://rds.amazonaws.com/
-- ?Action=ResetDBParameterGroup &DBParameterGroupName=mydbparametergroup
-- &Parameters.member.1.ParameterName=max_user_connections
-- &Parameters.member.1.ApplyMethod=pending-reboot
-- &Parameters.member.2.ParameterName=max_allowed_packet
-- &Parameters.member.2.ApplyMethod=immediate &ResetAllParameters=false
-- &Version=2013-05-15 &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &AWSAccessKeyId= &Signature= mydbparametergroup
-- 071e758f-bf57-11de-9f9f-53d6aee22de9.
--
-- See: 'Network.AWS.RDS.V2013_09_09.ResetDBParameterGroup'

resetDBParameterGroup :: ( MonadCatch m
                         , MonadResource m
                         , MonadError AWS.Error m
                         , MonadReader Env m
                         )
    => Text -- ^ 'rdbpgDBParameterGroupName'
    -> State ResetDBParameterGroup a
    -> m ResetDBParameterGroupResponse
resetDBParameterGroup p1 s =
    send $ (mkResetDBParameterGroup p1) &~ s

resetDBParameterGroupCatch :: ( MonadCatch m
                              , MonadResource m
                              , MonadReader Env m
                              )
    => Text -- ^ 'rdbpgDBParameterGroupName'
    -> State ResetDBParameterGroup a
    -> m (Either ServiceErr ResetDBParameterGroupResponse)
resetDBParameterGroupCatch p1 s =
    sendCatch $ (mkResetDBParameterGroup p1) &~ s

-- $RestoreDBInstanceFromDBSnapshot
-- Creates a new DB instance from a DB snapshot. The target database is
-- created from the source database restore point with the same configuration
-- as the original source database, except that the new RDS instance is
-- created with the default security group. https://rds.amazon.com/
-- ?Action=RestoreDBInstanceFromDBSnapshot &DBSnapshotIdentifier=mydbsnapshot
-- &DBInstanceIdentifier=myrestoreddbinstance &Version=2013-05-15
-- &Timestamp=2011-05-23T06%3A47%3A11.071Z &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &AWSAccessKeyId= &Signature= mysql 1 false
-- general-public-license creating 5.1.50 myrestoreddbinstance in-sync
-- default.mysql5.1 active default 00:00-00:30 true sat:07:30-sat:08:00 10
-- db.m1.large master 7ca622e8-8508-11e0-bd9b-a7b1ece36d51.
--
-- See: 'Network.AWS.RDS.V2013_09_09.RestoreDBInstanceFromDBSnapshot'

restoreDBInstanceFromDBSnapshot :: ( MonadCatch m
                                   , MonadResource m
                                   , MonadError AWS.Error m
                                   , MonadReader Env m
                                   )
    => Text -- ^ 'rdbifdbsDBInstanceIdentifier'
    -> Text -- ^ 'rdbifdbsDBSnapshotIdentifier'
    -> State RestoreDBInstanceFromDBSnapshot a
    -> m RestoreDBInstanceFromDBSnapshotResponse
restoreDBInstanceFromDBSnapshot p1 p2 s =
    send $ (mkRestoreDBInstanceFromDBSnapshot p1 p2) &~ s

restoreDBInstanceFromDBSnapshotCatch :: ( MonadCatch m
                                        , MonadResource m
                                        , MonadReader Env m
                                        )
    => Text -- ^ 'rdbifdbsDBInstanceIdentifier'
    -> Text -- ^ 'rdbifdbsDBSnapshotIdentifier'
    -> State RestoreDBInstanceFromDBSnapshot a
    -> m (Either ServiceErr RestoreDBInstanceFromDBSnapshotResponse)
restoreDBInstanceFromDBSnapshotCatch p1 p2 s =
    sendCatch $ (mkRestoreDBInstanceFromDBSnapshot p1 p2) &~ s

-- $RestoreDBInstanceToPointInTime
-- Restores a DB instance to an arbitrary point-in-time. Users can restore to
-- any point in time before the latestRestorableTime for up to
-- backupRetentionPeriod days. The target database is created from the source
-- database with the same configuration as the original database except that
-- the DB instance is created with the default DB security group.
-- https://rds.amazon.com/ ?Action=RestoreDBInstanceToPointInTime
-- &TargetDBInstanceIdentifier=restored-db
-- &SourceDBInstanceIdentifier=simcoprod01 &UseLatestRestorableTime=true
-- &Version=2013-05-15 &Timestamp=2011-05-23T07%3A06%3A02.313Z
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256 &AWSAccessKeyId=
-- &Signature= mysql 1 false general-public-license creating 5.1.50
-- restored-db in-sync default.mysql5.1 active default 00:00-00:30 true
-- sat:07:30-sat:08:00 10 db.m1.large master
-- 1ef546bc-850b-11e0-90aa-eb648410240d.
--
-- See: 'Network.AWS.RDS.V2013_09_09.RestoreDBInstanceToPointInTime'

restoreDBInstanceToPointInTime :: ( MonadCatch m
                                  , MonadResource m
                                  , MonadError AWS.Error m
                                  , MonadReader Env m
                                  )
    => Text -- ^ 'rdbitpitSourceDBInstanceIdentifier'
    -> Text -- ^ 'rdbitpitTargetDBInstanceIdentifier'
    -> State RestoreDBInstanceToPointInTime a
    -> m RestoreDBInstanceToPointInTimeResponse
restoreDBInstanceToPointInTime p1 p2 s =
    send $ (mkRestoreDBInstanceToPointInTime p1 p2) &~ s

restoreDBInstanceToPointInTimeCatch :: ( MonadCatch m
                                       , MonadResource m
                                       , MonadReader Env m
                                       )
    => Text -- ^ 'rdbitpitSourceDBInstanceIdentifier'
    -> Text -- ^ 'rdbitpitTargetDBInstanceIdentifier'
    -> State RestoreDBInstanceToPointInTime a
    -> m (Either ServiceErr RestoreDBInstanceToPointInTimeResponse)
restoreDBInstanceToPointInTimeCatch p1 p2 s =
    sendCatch $ (mkRestoreDBInstanceToPointInTime p1 p2) &~ s

-- $RevokeDBSecurityGroupIngress
-- Revokes ingress from a DBSecurityGroup for previously authorized IP ranges
-- or EC2 or VPC Security Groups. Required parameters for this API are one of
-- CIDRIP, EC2SecurityGroupId for VPC, or (EC2SecurityGroupOwnerId and either
-- EC2SecurityGroupName or EC2SecurityGroupId). https://rds.amazonaws.com/
-- ?Action=RevokeDBSecurityGroupIngress &DBSecurityGroupName=mydbsecuritygroup
-- &CIDRIP=192.168.1.1%2F24 &Version=2013-05-15
-- &SignatureVersion=2&SignatureMethod=HmacSHA256
-- &Timestamp=2011-02-15T22%3A32%3A12.515Z &AWSAccessKeyId= &Signature= My new
-- DBSecurityGroup 192.168.1.1/24 revoking 621567473609 mydbsecuritygroup
-- vpc-1ab2c3d4 beecb8ac-bf5a-11de-9f9f-53d6aee22de9.
--
-- See: 'Network.AWS.RDS.V2013_09_09.RevokeDBSecurityGroupIngress'

revokeDBSecurityGroupIngress :: ( MonadCatch m
                                , MonadResource m
                                , MonadError AWS.Error m
                                , MonadReader Env m
                                )
    => Text -- ^ 'rdbsgiDBSecurityGroupName'
    -> State RevokeDBSecurityGroupIngress a
    -> m RevokeDBSecurityGroupIngressResponse
revokeDBSecurityGroupIngress p1 s =
    send $ (mkRevokeDBSecurityGroupIngress p1) &~ s

revokeDBSecurityGroupIngressCatch :: ( MonadCatch m
                                     , MonadResource m
                                     , MonadReader Env m
                                     )
    => Text -- ^ 'rdbsgiDBSecurityGroupName'
    -> State RevokeDBSecurityGroupIngress a
    -> m (Either ServiceErr RevokeDBSecurityGroupIngressResponse)
revokeDBSecurityGroupIngressCatch p1 s =
    sendCatch $ (mkRevokeDBSecurityGroupIngress p1) &~ s
