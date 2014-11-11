{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.ElastiCache.CreateReplicationGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The CreateReplicationGroup operation creates a replication group. A
-- replication group is a collection of cache clusters, where one of the cache
-- clusters is a read/write primary and the others are read-only replicas.
-- Writes to the primary are automatically propagated to the replicas. When
-- you create a replication group, you must specify an existing cache cluster
-- that is in the primary role. When the replication group has been
-- successfully created, you can add one or more read replica replicas to it,
-- up to a total of five read replicas. Note: This action is valid only for
-- Redis.
module Network.AWS.ElastiCache.CreateReplicationGroup
    (
    -- * Request
      CreateReplicationGroupMessage
    -- ** Request constructor
    , createReplicationGroupMessage
    -- ** Request lenses
    , crgmAutoMinorVersionUpgrade
    , crgmAutomaticFailoverEnabled
    , crgmCacheNodeType
    , crgmCacheParameterGroupName
    , crgmCacheSecurityGroupNames
    , crgmCacheSubnetGroupName
    , crgmEngine
    , crgmEngineVersion
    , crgmNotificationTopicArn
    , crgmNumCacheClusters
    , crgmPort
    , crgmPreferredCacheClusterAZs
    , crgmPreferredMaintenanceWindow
    , crgmPrimaryClusterId
    , crgmReplicationGroupDescription
    , crgmReplicationGroupId
    , crgmSecurityGroupIds
    , crgmSnapshotArns
    , crgmSnapshotName
    , crgmSnapshotRetentionLimit
    , crgmSnapshotWindow

    -- * Response
    , CreateReplicationGroupResult
    -- ** Response constructor
    , createReplicationGroupResult
    -- ** Response lenses
    , crgrReplicationGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types

data CreateReplicationGroupMessage = CreateReplicationGroupMessage
    { _crgmAutoMinorVersionUpgrade     :: Maybe Bool
    , _crgmAutomaticFailoverEnabled    :: Maybe Bool
    , _crgmCacheNodeType               :: Maybe Text
    , _crgmCacheParameterGroupName     :: Maybe Text
    , _crgmCacheSecurityGroupNames     :: [Text]
    , _crgmCacheSubnetGroupName        :: Maybe Text
    , _crgmEngine                      :: Maybe Text
    , _crgmEngineVersion               :: Maybe Text
    , _crgmNotificationTopicArn        :: Maybe Text
    , _crgmNumCacheClusters            :: Maybe Int
    , _crgmPort                        :: Maybe Int
    , _crgmPreferredCacheClusterAZs    :: [Text]
    , _crgmPreferredMaintenanceWindow  :: Maybe Text
    , _crgmPrimaryClusterId            :: Maybe Text
    , _crgmReplicationGroupDescription :: Text
    , _crgmReplicationGroupId          :: Text
    , _crgmSecurityGroupIds            :: [Text]
    , _crgmSnapshotArns                :: [Text]
    , _crgmSnapshotName                :: Maybe Text
    , _crgmSnapshotRetentionLimit      :: Maybe Int
    , _crgmSnapshotWindow              :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateReplicationGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crgmAutoMinorVersionUpgrade' @::@ 'Maybe' 'Bool'
--
-- * 'crgmAutomaticFailoverEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'crgmCacheNodeType' @::@ 'Maybe' 'Text'
--
-- * 'crgmCacheParameterGroupName' @::@ 'Maybe' 'Text'
--
-- * 'crgmCacheSecurityGroupNames' @::@ ['Text']
--
-- * 'crgmCacheSubnetGroupName' @::@ 'Maybe' 'Text'
--
-- * 'crgmEngine' @::@ 'Maybe' 'Text'
--
-- * 'crgmEngineVersion' @::@ 'Maybe' 'Text'
--
-- * 'crgmNotificationTopicArn' @::@ 'Maybe' 'Text'
--
-- * 'crgmNumCacheClusters' @::@ 'Maybe' 'Int'
--
-- * 'crgmPort' @::@ 'Maybe' 'Int'
--
-- * 'crgmPreferredCacheClusterAZs' @::@ ['Text']
--
-- * 'crgmPreferredMaintenanceWindow' @::@ 'Maybe' 'Text'
--
-- * 'crgmPrimaryClusterId' @::@ 'Maybe' 'Text'
--
-- * 'crgmReplicationGroupDescription' @::@ 'Text'
--
-- * 'crgmReplicationGroupId' @::@ 'Text'
--
-- * 'crgmSecurityGroupIds' @::@ ['Text']
--
-- * 'crgmSnapshotArns' @::@ ['Text']
--
-- * 'crgmSnapshotName' @::@ 'Maybe' 'Text'
--
-- * 'crgmSnapshotRetentionLimit' @::@ 'Maybe' 'Int'
--
-- * 'crgmSnapshotWindow' @::@ 'Maybe' 'Text'
--
createReplicationGroupMessage :: Text -- ^ 'crgmReplicationGroupId'
                              -> Text -- ^ 'crgmReplicationGroupDescription'
                              -> CreateReplicationGroupMessage
createReplicationGroupMessage p1 p2 = CreateReplicationGroupMessage
    { _crgmReplicationGroupId          = p1
    , _crgmReplicationGroupDescription = p2
    , _crgmPrimaryClusterId            = Nothing
    , _crgmAutomaticFailoverEnabled    = Nothing
    , _crgmNumCacheClusters            = Nothing
    , _crgmPreferredCacheClusterAZs    = mempty
    , _crgmCacheNodeType               = Nothing
    , _crgmEngine                      = Nothing
    , _crgmEngineVersion               = Nothing
    , _crgmCacheParameterGroupName     = Nothing
    , _crgmCacheSubnetGroupName        = Nothing
    , _crgmCacheSecurityGroupNames     = mempty
    , _crgmSecurityGroupIds            = mempty
    , _crgmSnapshotArns                = mempty
    , _crgmSnapshotName                = Nothing
    , _crgmPreferredMaintenanceWindow  = Nothing
    , _crgmPort                        = Nothing
    , _crgmNotificationTopicArn        = Nothing
    , _crgmAutoMinorVersionUpgrade     = Nothing
    , _crgmSnapshotRetentionLimit      = Nothing
    , _crgmSnapshotWindow              = Nothing
    }

-- | Determines whether minor engine upgrades will be applied automatically to
-- the node group during the maintenance window. A value of true allows
-- these upgrades to occur; false disables automatic upgrades. Default:
-- true.
crgmAutoMinorVersionUpgrade :: Lens' CreateReplicationGroupMessage (Maybe Bool)
crgmAutoMinorVersionUpgrade =
    lens _crgmAutoMinorVersionUpgrade
        (\s a -> s { _crgmAutoMinorVersionUpgrade = a })

-- | Specifies whether a read-only replica will be automatically promoted to
-- read/write primary if the existing primary fails. If true, automatic
-- failover is enabled for this replication group. If false, automatic
-- failover is disabled for this replication group. Default: false.
crgmAutomaticFailoverEnabled :: Lens' CreateReplicationGroupMessage (Maybe Bool)
crgmAutomaticFailoverEnabled =
    lens _crgmAutomaticFailoverEnabled
        (\s a -> s { _crgmAutomaticFailoverEnabled = a })

-- | The compute and memory capacity of the nodes in the node group. Valid
-- node types are as follows: General purpose: Current generation:
-- cache.t2.micro, cache.t2.small, cache.t2.medium, cache.m3.medium,
-- cache.m3.large, cache.m3.xlarge, cache.m3.2xlarge Previous generation:
-- cache.t1.micro, cache.m1.small, cache.m1.medium, cache.m1.large,
-- cache.m1.xlarge Compute optimized: cache.c1.xlarge Memory optimized
-- Current generation: cache.r3.large, cache.r3.xlarge, cache.r3.2xlarge,
-- cache.r3.4xlarge, cache.r3.8xlarge Previous generation: cache.m2.xlarge,
-- cache.m2.2xlarge, cache.m2.4xlarge Notes: All t2 instances are created in
-- an Amazon Virtual Private Cloud (VPC). Redis backup/restore is not
-- supported for t2 instances. Redis Append-only files (AOF) functionality
-- is not supported for t1 or t2 instances. For a complete listing of cache
-- node types and specifications, see Amazon ElastiCache Product Features
-- and Details and Cache Node Type-Specific Parameters for Memcached or
-- Cache Node Type-Specific Parameters for Redis.
crgmCacheNodeType :: Lens' CreateReplicationGroupMessage (Maybe Text)
crgmCacheNodeType =
    lens _crgmCacheNodeType (\s a -> s { _crgmCacheNodeType = a })

-- | The name of the parameter group to associate with this replication group.
-- If this argument is omitted, the default cache parameter group for the
-- specified engine is used.
crgmCacheParameterGroupName :: Lens' CreateReplicationGroupMessage (Maybe Text)
crgmCacheParameterGroupName =
    lens _crgmCacheParameterGroupName
        (\s a -> s { _crgmCacheParameterGroupName = a })

-- | A list of cache security group names to associate with this replication
-- group.
crgmCacheSecurityGroupNames :: Lens' CreateReplicationGroupMessage [Text]
crgmCacheSecurityGroupNames =
    lens _crgmCacheSecurityGroupNames
        (\s a -> s { _crgmCacheSecurityGroupNames = a })

-- | The name of the cache subnet group to be used for the replication group.
crgmCacheSubnetGroupName :: Lens' CreateReplicationGroupMessage (Maybe Text)
crgmCacheSubnetGroupName =
    lens _crgmCacheSubnetGroupName
        (\s a -> s { _crgmCacheSubnetGroupName = a })

-- | The name of the cache engine to be used for the cache clusters in this
-- replication group. Default: redis.
crgmEngine :: Lens' CreateReplicationGroupMessage (Maybe Text)
crgmEngine = lens _crgmEngine (\s a -> s { _crgmEngine = a })

-- | The version number of the cach engine to be used for the cache clusters
-- in this replication group. To view the supported cache engine versions,
-- use the DescribeCacheEngineVersions operation.
crgmEngineVersion :: Lens' CreateReplicationGroupMessage (Maybe Text)
crgmEngineVersion =
    lens _crgmEngineVersion (\s a -> s { _crgmEngineVersion = a })

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- (SNS) topic to which notifications will be sent.
crgmNotificationTopicArn :: Lens' CreateReplicationGroupMessage (Maybe Text)
crgmNotificationTopicArn =
    lens _crgmNotificationTopicArn
        (\s a -> s { _crgmNotificationTopicArn = a })

-- | The number of cache clusters this replication group will initially have.
-- If AutomaticFailover is enabled, the value of this parameter must be at
-- least 2. The maximum permitted value for NumCacheClusters is 6 (primary
-- plus 5 replicas). If you need to exceed this limit, please fill out the
-- ElastiCache Limit Increase Request forrm at
-- http://aws.amazon.com/contact-us/elasticache-node-limit-request.
crgmNumCacheClusters :: Lens' CreateReplicationGroupMessage (Maybe Int)
crgmNumCacheClusters =
    lens _crgmNumCacheClusters (\s a -> s { _crgmNumCacheClusters = a })

-- | The port number on which each member of the replication group will accept
-- connections.
crgmPort :: Lens' CreateReplicationGroupMessage (Maybe Int)
crgmPort = lens _crgmPort (\s a -> s { _crgmPort = a })

-- | A list of EC2 availability zones in which the replication group's cache
-- clusters will be created. The order of the availability zones in the list
-- is not important. Default: system chosen availability zones. Example: One
-- Redis cache cluster in each of three availability zones.
-- PreferredAvailabilityZones.member.1=us-east-1a
-- PreferredAvailabilityZones.member.2=us-east-1c
-- PreferredAvailabilityZones.member.3=us-east-1d.
crgmPreferredCacheClusterAZs :: Lens' CreateReplicationGroupMessage [Text]
crgmPreferredCacheClusterAZs =
    lens _crgmPreferredCacheClusterAZs
        (\s a -> s { _crgmPreferredCacheClusterAZs = a })

-- | The weekly time range (in UTC) during which system maintenance can occur.
-- Example: sun:05:00-sun:09:00.
crgmPreferredMaintenanceWindow :: Lens' CreateReplicationGroupMessage (Maybe Text)
crgmPreferredMaintenanceWindow =
    lens _crgmPreferredMaintenanceWindow
        (\s a -> s { _crgmPreferredMaintenanceWindow = a })

-- | The identifier of the cache cluster that will serve as the primary for
-- this replication group. This cache cluster must already exist and have a
-- status of available. This parameter is not required if NumCacheClusters
-- is specified.
crgmPrimaryClusterId :: Lens' CreateReplicationGroupMessage (Maybe Text)
crgmPrimaryClusterId =
    lens _crgmPrimaryClusterId (\s a -> s { _crgmPrimaryClusterId = a })

-- | A user-created description for the replication group.
crgmReplicationGroupDescription :: Lens' CreateReplicationGroupMessage Text
crgmReplicationGroupDescription =
    lens _crgmReplicationGroupDescription
        (\s a -> s { _crgmReplicationGroupDescription = a })

-- | The replication group identifier. This parameter is stored as a lowercase
-- string. Constraints: A name must contain from 1 to 20 alphanumeric
-- characters or hyphens. The first character must be a letter. A name
-- cannot end with a hyphen or contain two consecutive hyphens.
crgmReplicationGroupId :: Lens' CreateReplicationGroupMessage Text
crgmReplicationGroupId =
    lens _crgmReplicationGroupId (\s a -> s { _crgmReplicationGroupId = a })

-- | One or more Amazon VPC security groups associated with this replication
-- group. Use this parameter only when you are creating a replication group
-- in an Amazon Virtual Private Cloud (VPC).
crgmSecurityGroupIds :: Lens' CreateReplicationGroupMessage [Text]
crgmSecurityGroupIds =
    lens _crgmSecurityGroupIds (\s a -> s { _crgmSecurityGroupIds = a })

-- | A single-element string list containing an Amazon Resource Name (ARN)
-- that uniquely identifies a Redis RDB snapshot file stored in Amazon S3.
-- The snapshot file will be used to populate the node group. The Amazon S3
-- object name in the ARN cannot contain any commas. Note: This parameter is
-- only valid if the Engine parameter is redis. Example of an Amazon S3 ARN:
-- arn:aws:s3:::my_bucket/snapshot1.rdb.
crgmSnapshotArns :: Lens' CreateReplicationGroupMessage [Text]
crgmSnapshotArns = lens _crgmSnapshotArns (\s a -> s { _crgmSnapshotArns = a })

-- | The name of a snapshot from which to restore data into the new node
-- group. The snapshot status changes to restoring while the new node group
-- is being created. Note: This parameter is only valid if the Engine
-- parameter is redis.
crgmSnapshotName :: Lens' CreateReplicationGroupMessage (Maybe Text)
crgmSnapshotName = lens _crgmSnapshotName (\s a -> s { _crgmSnapshotName = a })

-- | The number of days for which ElastiCache will retain automatic snapshots
-- before deleting them. For example, if you set SnapshotRetentionLimit to
-- 5, then a snapshot that was taken today will be retained for 5 days
-- before being deleted. Note: This parameter is only valid if the Engine
-- parameter is redis. Default: 0 (i.e., automatic backups are disabled for
-- this cache cluster).
crgmSnapshotRetentionLimit :: Lens' CreateReplicationGroupMessage (Maybe Int)
crgmSnapshotRetentionLimit =
    lens _crgmSnapshotRetentionLimit
        (\s a -> s { _crgmSnapshotRetentionLimit = a })

-- | The daily time range (in UTC) during which ElastiCache will begin taking
-- a daily snapshot of your node group. Example: 05:00-09:00 If you do not
-- specify this parameter, then ElastiCache will automatically choose an
-- appropriate time range. Note: This parameter is only valid if the Engine
-- parameter is redis.
crgmSnapshotWindow :: Lens' CreateReplicationGroupMessage (Maybe Text)
crgmSnapshotWindow =
    lens _crgmSnapshotWindow (\s a -> s { _crgmSnapshotWindow = a })
instance ToQuery CreateReplicationGroupMessage

instance ToPath CreateReplicationGroupMessage where
    toPath = const "/"

newtype CreateReplicationGroupResult = CreateReplicationGroupResult
    { _crgrReplicationGroup :: Maybe ReplicationGroup
    } deriving (Eq, Show, Generic)

-- | 'CreateReplicationGroupResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crgrReplicationGroup' @::@ 'Maybe' 'ReplicationGroup'
--
createReplicationGroupResult :: CreateReplicationGroupResult
createReplicationGroupResult = CreateReplicationGroupResult
    { _crgrReplicationGroup = Nothing
    }

crgrReplicationGroup :: Lens' CreateReplicationGroupResult (Maybe ReplicationGroup)
crgrReplicationGroup =
    lens _crgrReplicationGroup (\s a -> s { _crgrReplicationGroup = a })
instance FromXML CreateReplicationGroupResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateReplicationGroupResult"

instance AWSRequest CreateReplicationGroupMessage where
    type Sv CreateReplicationGroupMessage = ElastiCache
    type Rs CreateReplicationGroupMessage = CreateReplicationGroupResult

    request  = post "CreateReplicationGroup"
    response = xmlResponse $ \h x -> CreateReplicationGroupResult
        <$> x %| "ReplicationGroup"
