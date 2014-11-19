{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_CreateCacheCluster.html>
module Network.AWS.ElastiCache.CreateCacheCluster
    (
    -- * Request
      CreateCacheCluster
    -- ** Request constructor
    , createCacheCluster
    -- ** Request lenses
    , cccAZMode
    , cccAutoMinorVersionUpgrade
    , cccCacheClusterId
    , cccCacheNodeType
    , cccCacheParameterGroupName
    , cccCacheSecurityGroupNames
    , cccCacheSubnetGroupName
    , cccEngine
    , cccEngineVersion
    , cccNotificationTopicArn
    , cccNumCacheNodes
    , cccPort
    , cccPreferredAvailabilityZone
    , cccPreferredAvailabilityZones
    , cccPreferredMaintenanceWindow
    , cccReplicationGroupId
    , cccSecurityGroupIds
    , cccSnapshotArns
    , cccSnapshotName
    , cccSnapshotRetentionLimit
    , cccSnapshotWindow

    -- * Response
    , CreateCacheClusterResponse
    -- ** Response constructor
    , createCacheClusterResponse
    -- ** Response lenses
    , cccrCacheCluster
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import qualified GHC.Exts

data CreateCacheCluster = CreateCacheCluster
    { _cccAZMode                     :: Maybe Text
    , _cccAutoMinorVersionUpgrade    :: Maybe Bool
    , _cccCacheClusterId             :: Text
    , _cccCacheNodeType              :: Maybe Text
    , _cccCacheParameterGroupName    :: Maybe Text
    , _cccCacheSecurityGroupNames    :: [Text]
    , _cccCacheSubnetGroupName       :: Maybe Text
    , _cccEngine                     :: Maybe Text
    , _cccEngineVersion              :: Maybe Text
    , _cccNotificationTopicArn       :: Maybe Text
    , _cccNumCacheNodes              :: Maybe Int
    , _cccPort                       :: Maybe Int
    , _cccPreferredAvailabilityZone  :: Maybe Text
    , _cccPreferredAvailabilityZones :: [Text]
    , _cccPreferredMaintenanceWindow :: Maybe Text
    , _cccReplicationGroupId         :: Maybe Text
    , _cccSecurityGroupIds           :: [Text]
    , _cccSnapshotArns               :: [Text]
    , _cccSnapshotName               :: Maybe Text
    , _cccSnapshotRetentionLimit     :: Maybe Int
    , _cccSnapshotWindow             :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateCacheCluster' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cccAZMode' @::@ 'Maybe' 'Text'
--
-- * 'cccAutoMinorVersionUpgrade' @::@ 'Maybe' 'Bool'
--
-- * 'cccCacheClusterId' @::@ 'Text'
--
-- * 'cccCacheNodeType' @::@ 'Maybe' 'Text'
--
-- * 'cccCacheParameterGroupName' @::@ 'Maybe' 'Text'
--
-- * 'cccCacheSecurityGroupNames' @::@ ['Text']
--
-- * 'cccCacheSubnetGroupName' @::@ 'Maybe' 'Text'
--
-- * 'cccEngine' @::@ 'Maybe' 'Text'
--
-- * 'cccEngineVersion' @::@ 'Maybe' 'Text'
--
-- * 'cccNotificationTopicArn' @::@ 'Maybe' 'Text'
--
-- * 'cccNumCacheNodes' @::@ 'Maybe' 'Int'
--
-- * 'cccPort' @::@ 'Maybe' 'Int'
--
-- * 'cccPreferredAvailabilityZone' @::@ 'Maybe' 'Text'
--
-- * 'cccPreferredAvailabilityZones' @::@ ['Text']
--
-- * 'cccPreferredMaintenanceWindow' @::@ 'Maybe' 'Text'
--
-- * 'cccReplicationGroupId' @::@ 'Maybe' 'Text'
--
-- * 'cccSecurityGroupIds' @::@ ['Text']
--
-- * 'cccSnapshotArns' @::@ ['Text']
--
-- * 'cccSnapshotName' @::@ 'Maybe' 'Text'
--
-- * 'cccSnapshotRetentionLimit' @::@ 'Maybe' 'Int'
--
-- * 'cccSnapshotWindow' @::@ 'Maybe' 'Text'
--
createCacheCluster :: Text -- ^ 'cccCacheClusterId'
                   -> CreateCacheCluster
createCacheCluster p1 = CreateCacheCluster
    { _cccCacheClusterId             = p1
    , _cccReplicationGroupId         = Nothing
    , _cccAZMode                     = Nothing
    , _cccPreferredAvailabilityZone  = Nothing
    , _cccPreferredAvailabilityZones = mempty
    , _cccNumCacheNodes              = Nothing
    , _cccCacheNodeType              = Nothing
    , _cccEngine                     = Nothing
    , _cccEngineVersion              = Nothing
    , _cccCacheParameterGroupName    = Nothing
    , _cccCacheSubnetGroupName       = Nothing
    , _cccCacheSecurityGroupNames    = mempty
    , _cccSecurityGroupIds           = mempty
    , _cccSnapshotArns               = mempty
    , _cccSnapshotName               = Nothing
    , _cccPreferredMaintenanceWindow = Nothing
    , _cccPort                       = Nothing
    , _cccNotificationTopicArn       = Nothing
    , _cccAutoMinorVersionUpgrade    = Nothing
    , _cccSnapshotRetentionLimit     = Nothing
    , _cccSnapshotWindow             = Nothing
    }

-- | Specifies whether the nodes in this Memcached node group are created in a
-- single Availability Zone or created across multiple Availability Zones in
-- the cluster's region. This parameter is only supported for Memcached
-- cache clusters. If the AZMode and PreferredAvailabilityZones are not
-- specified, ElastiCache assumes single-az mode.
cccAZMode :: Lens' CreateCacheCluster (Maybe Text)
cccAZMode = lens _cccAZMode (\s a -> s { _cccAZMode = a })

-- | Determines whether minor engine upgrades will be applied automatically to
-- the node group during the maintenance window. A value of true allows
-- these upgrades to occur; false disables automatic upgrades. Default:
-- true.
cccAutoMinorVersionUpgrade :: Lens' CreateCacheCluster (Maybe Bool)
cccAutoMinorVersionUpgrade =
    lens _cccAutoMinorVersionUpgrade
        (\s a -> s { _cccAutoMinorVersionUpgrade = a })

-- | The node group identifier. This parameter is stored as a lowercase
-- string. Constraints: A name must contain from 1 to 20 alphanumeric
-- characters or hyphens. The first character must be a letter. A name
-- cannot end with a hyphen or contain two consecutive hyphens.
cccCacheClusterId :: Lens' CreateCacheCluster Text
cccCacheClusterId =
    lens _cccCacheClusterId (\s a -> s { _cccCacheClusterId = a })

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
cccCacheNodeType :: Lens' CreateCacheCluster (Maybe Text)
cccCacheNodeType = lens _cccCacheNodeType (\s a -> s { _cccCacheNodeType = a })

-- | The name of the parameter group to associate with this cache cluster. If
-- this argument is omitted, the default parameter group for the specified
-- engine is used.
cccCacheParameterGroupName :: Lens' CreateCacheCluster (Maybe Text)
cccCacheParameterGroupName =
    lens _cccCacheParameterGroupName
        (\s a -> s { _cccCacheParameterGroupName = a })

-- | A list of security group names to associate with this cache cluster. Use
-- this parameter only when you are creating a cache cluster outside of an
-- Amazon Virtual Private Cloud (VPC).
cccCacheSecurityGroupNames :: Lens' CreateCacheCluster [Text]
cccCacheSecurityGroupNames =
    lens _cccCacheSecurityGroupNames
        (\s a -> s { _cccCacheSecurityGroupNames = a })

-- | The name of the subnet group to be used for the cache cluster. Use this
-- parameter only when you are creating a cache cluster in an Amazon Virtual
-- Private Cloud (VPC).
cccCacheSubnetGroupName :: Lens' CreateCacheCluster (Maybe Text)
cccCacheSubnetGroupName =
    lens _cccCacheSubnetGroupName (\s a -> s { _cccCacheSubnetGroupName = a })

-- | The name of the cache engine to be used for this cache cluster. Valid
-- values for this parameter are: memcached | redis.
cccEngine :: Lens' CreateCacheCluster (Maybe Text)
cccEngine = lens _cccEngine (\s a -> s { _cccEngine = a })

-- | The version number of the cache engine to be used for this cache cluster.
-- To view the supported cache engine versions, use the
-- DescribeCacheEngineVersions operation.
cccEngineVersion :: Lens' CreateCacheCluster (Maybe Text)
cccEngineVersion = lens _cccEngineVersion (\s a -> s { _cccEngineVersion = a })

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- (SNS) topic to which notifications will be sent.
cccNotificationTopicArn :: Lens' CreateCacheCluster (Maybe Text)
cccNotificationTopicArn =
    lens _cccNotificationTopicArn (\s a -> s { _cccNotificationTopicArn = a })

-- | The initial number of cache nodes that the cache cluster will have. For
-- Memcached, valid values are between 1 and 20. If you need to exceed this
-- limit, please fill out the ElastiCache Limit Increase Request form at
-- http://aws.amazon.com/contact-us/elasticache-node-limit-request/. For
-- Redis, only single-node cache cluster are supported at this time, so the
-- value for this parameter must be 1.
cccNumCacheNodes :: Lens' CreateCacheCluster (Maybe Int)
cccNumCacheNodes = lens _cccNumCacheNodes (\s a -> s { _cccNumCacheNodes = a })

-- | The port number on which each of the cache nodes will accept connections.
cccPort :: Lens' CreateCacheCluster (Maybe Int)
cccPort = lens _cccPort (\s a -> s { _cccPort = a })

-- | The EC2 Availability Zone in which the cache cluster will be created. All
-- nodes belonging to this Memcached cache cluster are placed in the
-- preferred Availability Zone. If you want to create your nodes across
-- multiple Availability Zones, use PreferredAvailabilityZones. Default:
-- System chosen Availability Zone.
cccPreferredAvailabilityZone :: Lens' CreateCacheCluster (Maybe Text)
cccPreferredAvailabilityZone =
    lens _cccPreferredAvailabilityZone
        (\s a -> s { _cccPreferredAvailabilityZone = a })

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
cccPreferredAvailabilityZones :: Lens' CreateCacheCluster [Text]
cccPreferredAvailabilityZones =
    lens _cccPreferredAvailabilityZones
        (\s a -> s { _cccPreferredAvailabilityZones = a })

-- | The weekly time range (in UTC) during which system maintenance can occur.
-- Example: sun:05:00-sun:09:00.
cccPreferredMaintenanceWindow :: Lens' CreateCacheCluster (Maybe Text)
cccPreferredMaintenanceWindow =
    lens _cccPreferredMaintenanceWindow
        (\s a -> s { _cccPreferredMaintenanceWindow = a })

-- | The ID of the replication group to which this cache cluster should
-- belong. If this parameter is specified, the cache cluster will be added
-- to the specified replication group as a read replica; otherwise, the
-- cache cluster will be a standalone primary that is not part of any
-- replication group. If the specified replication group is Automatic
-- Failover enabled and the availability zone is not specified, the cache
-- cluster will be created in availability zones that provide the best
-- spread of read replicas across availability zones. Note: This parameter
-- is only valid if the Engine parameter is redis.
cccReplicationGroupId :: Lens' CreateCacheCluster (Maybe Text)
cccReplicationGroupId =
    lens _cccReplicationGroupId (\s a -> s { _cccReplicationGroupId = a })

-- | One or more VPC security groups associated with the cache cluster. Use
-- this parameter only when you are creating a cache cluster in an Amazon
-- Virtual Private Cloud (VPC).
cccSecurityGroupIds :: Lens' CreateCacheCluster [Text]
cccSecurityGroupIds =
    lens _cccSecurityGroupIds (\s a -> s { _cccSecurityGroupIds = a })

-- | A single-element string list containing an Amazon Resource Name (ARN)
-- that uniquely identifies a Redis RDB snapshot file stored in Amazon S3.
-- The snapshot file will be used to populate the node group. The Amazon S3
-- object name in the ARN cannot contain any commas. Note: This parameter is
-- only valid if the Engine parameter is redis. Example of an Amazon S3 ARN:
-- arn:aws:s3:::my_bucket/snapshot1.rdb.
cccSnapshotArns :: Lens' CreateCacheCluster [Text]
cccSnapshotArns = lens _cccSnapshotArns (\s a -> s { _cccSnapshotArns = a })

-- | The name of a snapshot from which to restore data into the new node
-- group. The snapshot status changes to restoring while the new node group
-- is being created. Note: This parameter is only valid if the Engine
-- parameter is redis.
cccSnapshotName :: Lens' CreateCacheCluster (Maybe Text)
cccSnapshotName = lens _cccSnapshotName (\s a -> s { _cccSnapshotName = a })

-- | The number of days for which ElastiCache will retain automatic snapshots
-- before deleting them. For example, if you set SnapshotRetentionLimit to
-- 5, then a snapshot that was taken today will be retained for 5 days
-- before being deleted. Note: This parameter is only valid if the Engine
-- parameter is redis. Default: 0 (i.e., automatic backups are disabled for
-- this cache cluster).
cccSnapshotRetentionLimit :: Lens' CreateCacheCluster (Maybe Int)
cccSnapshotRetentionLimit =
    lens _cccSnapshotRetentionLimit
        (\s a -> s { _cccSnapshotRetentionLimit = a })

-- | The daily time range (in UTC) during which ElastiCache will begin taking
-- a daily snapshot of your node group. Example: 05:00-09:00 If you do not
-- specify this parameter, then ElastiCache will automatically choose an
-- appropriate time range. Note: This parameter is only valid if the Engine
-- parameter is redis.
cccSnapshotWindow :: Lens' CreateCacheCluster (Maybe Text)
cccSnapshotWindow =
    lens _cccSnapshotWindow (\s a -> s { _cccSnapshotWindow = a })

newtype CreateCacheClusterResponse = CreateCacheClusterResponse
    { _cccrCacheCluster :: Maybe CacheCluster
    } deriving (Eq, Show, Generic)

-- | 'CreateCacheClusterResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cccrCacheCluster' @::@ 'Maybe' 'CacheCluster'
--
createCacheClusterResponse :: CreateCacheClusterResponse
createCacheClusterResponse = CreateCacheClusterResponse
    { _cccrCacheCluster = Nothing
    }

cccrCacheCluster :: Lens' CreateCacheClusterResponse (Maybe CacheCluster)
cccrCacheCluster = lens _cccrCacheCluster (\s a -> s { _cccrCacheCluster = a })

instance ToPath CreateCacheCluster where
    toPath = const "/"

instance ToQuery CreateCacheCluster

instance ToHeaders CreateCacheCluster

instance AWSRequest CreateCacheCluster where
    type Sv CreateCacheCluster = ElastiCache
    type Rs CreateCacheCluster = CreateCacheClusterResponse

    request  = post "CreateCacheCluster"
    response = xmlResponse

instance FromXML CreateCacheClusterResponse where
    parseXML = withElement "CreateCacheClusterResult" $ \x ->
            <$> x .@? "CacheCluster"
