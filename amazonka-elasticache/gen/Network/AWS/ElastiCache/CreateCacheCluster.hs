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

-- Module      : Network.AWS.ElastiCache.CreateCacheCluster
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The CreateCacheCluster operation creates a cache cluster. All nodes in the
-- cache cluster run the same protocol-compliant cache engine software, either
-- Memcached or Redis.
module Network.AWS.ElastiCache.CreateCacheCluster
    (
    -- * Request
      CreateCacheClusterMessage
    -- ** Request constructor
    , createCacheClusterMessage
    -- ** Request lenses
    , cccmAZMode
    , cccmAutoMinorVersionUpgrade
    , cccmCacheClusterId
    , cccmCacheNodeType
    , cccmCacheParameterGroupName
    , cccmCacheSecurityGroupNames
    , cccmCacheSubnetGroupName
    , cccmEngine
    , cccmEngineVersion
    , cccmNotificationTopicArn
    , cccmNumCacheNodes
    , cccmPort
    , cccmPreferredAvailabilityZone
    , cccmPreferredAvailabilityZones
    , cccmPreferredMaintenanceWindow
    , cccmReplicationGroupId
    , cccmSecurityGroupIds
    , cccmSnapshotArns
    , cccmSnapshotName
    , cccmSnapshotRetentionLimit
    , cccmSnapshotWindow

    -- * Response
    , CreateCacheClusterResult
    -- ** Response constructor
    , createCacheClusterResult
    -- ** Response lenses
    , cccrCacheCluster
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types

data CreateCacheClusterMessage = CreateCacheClusterMessage
    { _cccmAZMode                     :: Maybe Text
    , _cccmAutoMinorVersionUpgrade    :: Maybe Bool
    , _cccmCacheClusterId             :: Text
    , _cccmCacheNodeType              :: Maybe Text
    , _cccmCacheParameterGroupName    :: Maybe Text
    , _cccmCacheSecurityGroupNames    :: [Text]
    , _cccmCacheSubnetGroupName       :: Maybe Text
    , _cccmEngine                     :: Maybe Text
    , _cccmEngineVersion              :: Maybe Text
    , _cccmNotificationTopicArn       :: Maybe Text
    , _cccmNumCacheNodes              :: Maybe Int
    , _cccmPort                       :: Maybe Int
    , _cccmPreferredAvailabilityZone  :: Maybe Text
    , _cccmPreferredAvailabilityZones :: [Text]
    , _cccmPreferredMaintenanceWindow :: Maybe Text
    , _cccmReplicationGroupId         :: Maybe Text
    , _cccmSecurityGroupIds           :: [Text]
    , _cccmSnapshotArns               :: [Text]
    , _cccmSnapshotName               :: Maybe Text
    , _cccmSnapshotRetentionLimit     :: Maybe Int
    , _cccmSnapshotWindow             :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateCacheClusterMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cccmAZMode' @::@ 'Maybe' 'Text'
--
-- * 'cccmAutoMinorVersionUpgrade' @::@ 'Maybe' 'Bool'
--
-- * 'cccmCacheClusterId' @::@ 'Text'
--
-- * 'cccmCacheNodeType' @::@ 'Maybe' 'Text'
--
-- * 'cccmCacheParameterGroupName' @::@ 'Maybe' 'Text'
--
-- * 'cccmCacheSecurityGroupNames' @::@ ['Text']
--
-- * 'cccmCacheSubnetGroupName' @::@ 'Maybe' 'Text'
--
-- * 'cccmEngine' @::@ 'Maybe' 'Text'
--
-- * 'cccmEngineVersion' @::@ 'Maybe' 'Text'
--
-- * 'cccmNotificationTopicArn' @::@ 'Maybe' 'Text'
--
-- * 'cccmNumCacheNodes' @::@ 'Maybe' 'Int'
--
-- * 'cccmPort' @::@ 'Maybe' 'Int'
--
-- * 'cccmPreferredAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'cccmPreferredAvailabilityZones' @::@ ['Text']
--
-- * 'cccmPreferredMaintenanceWindow' @::@ 'Maybe' 'Text'
--
-- * 'cccmReplicationGroupId' @::@ 'Maybe' 'Text'
--
-- * 'cccmSecurityGroupIds' @::@ ['Text']
--
-- * 'cccmSnapshotArns' @::@ ['Text']
--
-- * 'cccmSnapshotName' @::@ 'Maybe' 'Text'
--
-- * 'cccmSnapshotRetentionLimit' @::@ 'Maybe' 'Int'
--
-- * 'cccmSnapshotWindow' @::@ 'Maybe' 'Text'
--
createCacheClusterMessage :: Text -- ^ 'cccmCacheClusterId'
                          -> CreateCacheClusterMessage
createCacheClusterMessage p1 = CreateCacheClusterMessage
    { _cccmCacheClusterId             = p1
    , _cccmReplicationGroupId         = Nothing
    , _cccmAZMode                     = Nothing
    , _cccmPreferredAvailabilityZone  = Nothing
    , _cccmPreferredAvailabilityZones = mempty
    , _cccmNumCacheNodes              = Nothing
    , _cccmCacheNodeType              = Nothing
    , _cccmEngine                     = Nothing
    , _cccmEngineVersion              = Nothing
    , _cccmCacheParameterGroupName    = Nothing
    , _cccmCacheSubnetGroupName       = Nothing
    , _cccmCacheSecurityGroupNames    = mempty
    , _cccmSecurityGroupIds           = mempty
    , _cccmSnapshotArns               = mempty
    , _cccmSnapshotName               = Nothing
    , _cccmPreferredMaintenanceWindow = Nothing
    , _cccmPort                       = Nothing
    , _cccmNotificationTopicArn       = Nothing
    , _cccmAutoMinorVersionUpgrade    = Nothing
    , _cccmSnapshotRetentionLimit     = Nothing
    , _cccmSnapshotWindow             = Nothing
    }

-- | Specifies whether the nodes in this Memcached node group are created in a
-- single Availability Zone or created across multiple Availability Zones in
-- the cluster's region. This parameter is only supported for Memcached
-- cache clusters. If the AZMode and PreferredAvailabilityZones are not
-- specified, ElastiCache assumes single-az mode.
cccmAZMode :: Lens' CreateCacheClusterMessage (Maybe Text)
cccmAZMode = lens _cccmAZMode (\s a -> s { _cccmAZMode = a })

-- | Determines whether minor engine upgrades will be applied automatically to
-- the node group during the maintenance window. A value of true allows
-- these upgrades to occur; false disables automatic upgrades. Default:
-- true.
cccmAutoMinorVersionUpgrade :: Lens' CreateCacheClusterMessage (Maybe Bool)
cccmAutoMinorVersionUpgrade =
    lens _cccmAutoMinorVersionUpgrade
        (\s a -> s { _cccmAutoMinorVersionUpgrade = a })

-- | The node group identifier. This parameter is stored as a lowercase
-- string. Constraints: A name must contain from 1 to 20 alphanumeric
-- characters or hyphens. The first character must be a letter. A name
-- cannot end with a hyphen or contain two consecutive hyphens.
cccmCacheClusterId :: Lens' CreateCacheClusterMessage Text
cccmCacheClusterId =
    lens _cccmCacheClusterId (\s a -> s { _cccmCacheClusterId = a })

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
cccmCacheNodeType :: Lens' CreateCacheClusterMessage (Maybe Text)
cccmCacheNodeType =
    lens _cccmCacheNodeType (\s a -> s { _cccmCacheNodeType = a })

-- | The name of the parameter group to associate with this cache cluster. If
-- this argument is omitted, the default parameter group for the specified
-- engine is used.
cccmCacheParameterGroupName :: Lens' CreateCacheClusterMessage (Maybe Text)
cccmCacheParameterGroupName =
    lens _cccmCacheParameterGroupName
        (\s a -> s { _cccmCacheParameterGroupName = a })

-- | A list of security group names to associate with this cache cluster. Use
-- this parameter only when you are creating a cache cluster outside of an
-- Amazon Virtual Private Cloud (VPC).
cccmCacheSecurityGroupNames :: Lens' CreateCacheClusterMessage [Text]
cccmCacheSecurityGroupNames =
    lens _cccmCacheSecurityGroupNames
        (\s a -> s { _cccmCacheSecurityGroupNames = a })

-- | The name of the subnet group to be used for the cache cluster. Use this
-- parameter only when you are creating a cache cluster in an Amazon Virtual
-- Private Cloud (VPC).
cccmCacheSubnetGroupName :: Lens' CreateCacheClusterMessage (Maybe Text)
cccmCacheSubnetGroupName =
    lens _cccmCacheSubnetGroupName
        (\s a -> s { _cccmCacheSubnetGroupName = a })

-- | The name of the cache engine to be used for this cache cluster. Valid
-- values for this parameter are: memcached | redis.
cccmEngine :: Lens' CreateCacheClusterMessage (Maybe Text)
cccmEngine = lens _cccmEngine (\s a -> s { _cccmEngine = a })

-- | The version number of the cache engine to be used for this cache cluster.
-- To view the supported cache engine versions, use the
-- DescribeCacheEngineVersions operation.
cccmEngineVersion :: Lens' CreateCacheClusterMessage (Maybe Text)
cccmEngineVersion =
    lens _cccmEngineVersion (\s a -> s { _cccmEngineVersion = a })

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- (SNS) topic to which notifications will be sent.
cccmNotificationTopicArn :: Lens' CreateCacheClusterMessage (Maybe Text)
cccmNotificationTopicArn =
    lens _cccmNotificationTopicArn
        (\s a -> s { _cccmNotificationTopicArn = a })

-- | The initial number of cache nodes that the cache cluster will have. For
-- Memcached, valid values are between 1 and 20. If you need to exceed this
-- limit, please fill out the ElastiCache Limit Increase Request form at
-- http://aws.amazon.com/contact-us/elasticache-node-limit-request/. For
-- Redis, only single-node cache cluster are supported at this time, so the
-- value for this parameter must be 1.
cccmNumCacheNodes :: Lens' CreateCacheClusterMessage (Maybe Int)
cccmNumCacheNodes =
    lens _cccmNumCacheNodes (\s a -> s { _cccmNumCacheNodes = a })

-- | The port number on which each of the cache nodes will accept connections.
cccmPort :: Lens' CreateCacheClusterMessage (Maybe Int)
cccmPort = lens _cccmPort (\s a -> s { _cccmPort = a })

-- | The EC2 Availability Zone in which the cache cluster will be created. All
-- nodes belonging to this Memcached cache cluster are placed in the
-- preferred Availability Zone. If you want to create your nodes across
-- multiple Availability Zones, use PreferredAvailabilityZones. Default:
-- System chosen Availability Zone.
cccmPreferredAvailabilityZone :: Lens' CreateCacheClusterMessage (Maybe Text)
cccmPreferredAvailabilityZone =
    lens _cccmPreferredAvailabilityZone
        (\s a -> s { _cccmPreferredAvailabilityZone = a })

-- | A list of the Availability Zones in which cache nodes will be created.
-- The order of the zones in the list is not important. This option is only
-- supported on Memcached. If you want all the nodes in the same
-- Availability Zone, use PreferredAvailabilityZone instead, or repeat the
-- Availability Zone multiple times in the list. Default: System chosen
-- Availability Zones. Example: One Memcached node in each of three
-- different Availability Zones:
-- PreferredAvailabilityZones.member.1=us-east-1a&amp;PreferredAvailabilityZones.member.2=us-east-1b&amp;PreferredAvailabilityZones.member.3=us-east-1d
-- Example: All three Memcached nodes in one Availability Zone:
-- PreferredAvailabilityZones.member.1=us-east-1a&amp;PreferredAvailabilityZones.member.2=us-east-1a&amp;PreferredAvailabilityZones.member.3=us-east-1a.
-- 
cccmPreferredAvailabilityZones :: Lens' CreateCacheClusterMessage [Text]
cccmPreferredAvailabilityZones =
    lens _cccmPreferredAvailabilityZones
        (\s a -> s { _cccmPreferredAvailabilityZones = a })

-- | The weekly time range (in UTC) during which system maintenance can occur.
-- Example: sun:05:00-sun:09:00.
cccmPreferredMaintenanceWindow :: Lens' CreateCacheClusterMessage (Maybe Text)
cccmPreferredMaintenanceWindow =
    lens _cccmPreferredMaintenanceWindow
        (\s a -> s { _cccmPreferredMaintenanceWindow = a })

-- | The ID of the replication group to which this cache cluster should
-- belong. If this parameter is specified, the cache cluster will be added
-- to the specified replication group as a read replica; otherwise, the
-- cache cluster will be a standalone primary that is not part of any
-- replication group. If the specified replication group is Automatic
-- Failover enabled and the availability zone is not specified, the cache
-- cluster will be created in availability zones that provide the best
-- spread of read replicas across availability zones. Note: This parameter
-- is only valid if the Engine parameter is redis.
cccmReplicationGroupId :: Lens' CreateCacheClusterMessage (Maybe Text)
cccmReplicationGroupId =
    lens _cccmReplicationGroupId (\s a -> s { _cccmReplicationGroupId = a })

-- | One or more VPC security groups associated with the cache cluster. Use
-- this parameter only when you are creating a cache cluster in an Amazon
-- Virtual Private Cloud (VPC).
cccmSecurityGroupIds :: Lens' CreateCacheClusterMessage [Text]
cccmSecurityGroupIds =
    lens _cccmSecurityGroupIds (\s a -> s { _cccmSecurityGroupIds = a })

-- | A single-element string list containing an Amazon Resource Name (ARN)
-- that uniquely identifies a Redis RDB snapshot file stored in Amazon S3.
-- The snapshot file will be used to populate the node group. The Amazon S3
-- object name in the ARN cannot contain any commas. Note: This parameter is
-- only valid if the Engine parameter is redis. Example of an Amazon S3 ARN:
-- arn:aws:s3:::my_bucket/snapshot1.rdb.
cccmSnapshotArns :: Lens' CreateCacheClusterMessage [Text]
cccmSnapshotArns = lens _cccmSnapshotArns (\s a -> s { _cccmSnapshotArns = a })

-- | The name of a snapshot from which to restore data into the new node
-- group. The snapshot status changes to restoring while the new node group
-- is being created. Note: This parameter is only valid if the Engine
-- parameter is redis.
cccmSnapshotName :: Lens' CreateCacheClusterMessage (Maybe Text)
cccmSnapshotName = lens _cccmSnapshotName (\s a -> s { _cccmSnapshotName = a })

-- | The number of days for which ElastiCache will retain automatic snapshots
-- before deleting them. For example, if you set SnapshotRetentionLimit to
-- 5, then a snapshot that was taken today will be retained for 5 days
-- before being deleted. Note: This parameter is only valid if the Engine
-- parameter is redis. Default: 0 (i.e., automatic backups are disabled for
-- this cache cluster).
cccmSnapshotRetentionLimit :: Lens' CreateCacheClusterMessage (Maybe Int)
cccmSnapshotRetentionLimit =
    lens _cccmSnapshotRetentionLimit
        (\s a -> s { _cccmSnapshotRetentionLimit = a })

-- | The daily time range (in UTC) during which ElastiCache will begin taking
-- a daily snapshot of your node group. Example: 05:00-09:00 If you do not
-- specify this parameter, then ElastiCache will automatically choose an
-- appropriate time range. Note: This parameter is only valid if the Engine
-- parameter is redis.
cccmSnapshotWindow :: Lens' CreateCacheClusterMessage (Maybe Text)
cccmSnapshotWindow =
    lens _cccmSnapshotWindow (\s a -> s { _cccmSnapshotWindow = a })
instance ToQuery CreateCacheClusterMessage

instance ToPath CreateCacheClusterMessage where
    toPath = const "/"

newtype CreateCacheClusterResult = CreateCacheClusterResult
    { _cccrCacheCluster :: Maybe CacheCluster
    } deriving (Eq, Show, Generic)

-- | 'CreateCacheClusterResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cccrCacheCluster' @::@ 'Maybe' 'CacheCluster'
--
createCacheClusterResult :: CreateCacheClusterResult
createCacheClusterResult = CreateCacheClusterResult
    { _cccrCacheCluster = Nothing
    }

cccrCacheCluster :: Lens' CreateCacheClusterResult (Maybe CacheCluster)
cccrCacheCluster = lens _cccrCacheCluster (\s a -> s { _cccrCacheCluster = a })
instance FromXML CreateCacheClusterResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateCacheClusterResult"

instance AWSRequest CreateCacheClusterMessage where
    type Sv CreateCacheClusterMessage = ElastiCache
    type Rs CreateCacheClusterMessage = CreateCacheClusterResult

    request  = post "CreateCacheCluster"
    response = xmlResponse $ \h x -> CreateCacheClusterResult
        <$> x %| "CacheCluster"
