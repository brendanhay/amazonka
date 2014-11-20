{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_CreateReplicationGroup.html>
module Network.AWS.ElastiCache.CreateReplicationGroup
    (
    -- * Request
      CreateReplicationGroup
    -- ** Request constructor
    , createReplicationGroup
    -- ** Request lenses
    , crgAutoMinorVersionUpgrade
    , crgAutomaticFailoverEnabled
    , crgCacheNodeType
    , crgCacheParameterGroupName
    , crgCacheSecurityGroupNames
    , crgCacheSubnetGroupName
    , crgEngine
    , crgEngineVersion
    , crgNotificationTopicArn
    , crgNumCacheClusters
    , crgPort
    , crgPreferredCacheClusterAZs
    , crgPreferredMaintenanceWindow
    , crgPrimaryClusterId
    , crgReplicationGroupDescription
    , crgReplicationGroupId
    , crgSecurityGroupIds
    , crgSnapshotArns
    , crgSnapshotName
    , crgSnapshotRetentionLimit
    , crgSnapshotWindow

    -- * Response
    , CreateReplicationGroupResponse
    -- ** Response constructor
    , createReplicationGroupResponse
    -- ** Response lenses
    , crgrReplicationGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import qualified GHC.Exts

data CreateReplicationGroup = CreateReplicationGroup
    { _crgAutoMinorVersionUpgrade     :: Maybe Bool
    , _crgAutomaticFailoverEnabled    :: Maybe Bool
    , _crgCacheNodeType               :: Maybe Text
    , _crgCacheParameterGroupName     :: Maybe Text
    , _crgCacheSecurityGroupNames     :: List "CacheSecurityGroupName" Text
    , _crgCacheSubnetGroupName        :: Maybe Text
    , _crgEngine                      :: Maybe Text
    , _crgEngineVersion               :: Maybe Text
    , _crgNotificationTopicArn        :: Maybe Text
    , _crgNumCacheClusters            :: Maybe Int
    , _crgPort                        :: Maybe Int
    , _crgPreferredCacheClusterAZs    :: List "AvailabilityZone" Text
    , _crgPreferredMaintenanceWindow  :: Maybe Text
    , _crgPrimaryClusterId            :: Maybe Text
    , _crgReplicationGroupDescription :: Text
    , _crgReplicationGroupId          :: Text
    , _crgSecurityGroupIds            :: List "SecurityGroupId" Text
    , _crgSnapshotArns                :: List "SnapshotArn" Text
    , _crgSnapshotName                :: Maybe Text
    , _crgSnapshotRetentionLimit      :: Maybe Int
    , _crgSnapshotWindow              :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'CreateReplicationGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crgAutoMinorVersionUpgrade' @::@ 'Maybe' 'Bool'
--
-- * 'crgAutomaticFailoverEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'crgCacheNodeType' @::@ 'Maybe' 'Text'
--
-- * 'crgCacheParameterGroupName' @::@ 'Maybe' 'Text'
--
-- * 'crgCacheSecurityGroupNames' @::@ ['Text']
--
-- * 'crgCacheSubnetGroupName' @::@ 'Maybe' 'Text'
--
-- * 'crgEngine' @::@ 'Maybe' 'Text'
--
-- * 'crgEngineVersion' @::@ 'Maybe' 'Text'
--
-- * 'crgNotificationTopicArn' @::@ 'Maybe' 'Text'
--
-- * 'crgNumCacheClusters' @::@ 'Maybe' 'Int'
--
-- * 'crgPort' @::@ 'Maybe' 'Int'
--
-- * 'crgPreferredCacheClusterAZs' @::@ ['Text']
--
-- * 'crgPreferredMaintenanceWindow' @::@ 'Maybe' 'Text'
--
-- * 'crgPrimaryClusterId' @::@ 'Maybe' 'Text'
--
-- * 'crgReplicationGroupDescription' @::@ 'Text'
--
-- * 'crgReplicationGroupId' @::@ 'Text'
--
-- * 'crgSecurityGroupIds' @::@ ['Text']
--
-- * 'crgSnapshotArns' @::@ ['Text']
--
-- * 'crgSnapshotName' @::@ 'Maybe' 'Text'
--
-- * 'crgSnapshotRetentionLimit' @::@ 'Maybe' 'Int'
--
-- * 'crgSnapshotWindow' @::@ 'Maybe' 'Text'
--
createReplicationGroup :: Text -- ^ 'crgReplicationGroupId'
                       -> Text -- ^ 'crgReplicationGroupDescription'
                       -> CreateReplicationGroup
createReplicationGroup p1 p2 = CreateReplicationGroup
    { _crgReplicationGroupId          = p1
    , _crgReplicationGroupDescription = p2
    , _crgPrimaryClusterId            = Nothing
    , _crgAutomaticFailoverEnabled    = Nothing
    , _crgNumCacheClusters            = Nothing
    , _crgPreferredCacheClusterAZs    = mempty
    , _crgCacheNodeType               = Nothing
    , _crgEngine                      = Nothing
    , _crgEngineVersion               = Nothing
    , _crgCacheParameterGroupName     = Nothing
    , _crgCacheSubnetGroupName        = Nothing
    , _crgCacheSecurityGroupNames     = mempty
    , _crgSecurityGroupIds            = mempty
    , _crgSnapshotArns                = mempty
    , _crgSnapshotName                = Nothing
    , _crgPreferredMaintenanceWindow  = Nothing
    , _crgPort                        = Nothing
    , _crgNotificationTopicArn        = Nothing
    , _crgAutoMinorVersionUpgrade     = Nothing
    , _crgSnapshotRetentionLimit      = Nothing
    , _crgSnapshotWindow              = Nothing
    }

-- | Determines whether minor engine upgrades will be applied automatically to
-- the node group during the maintenance window. A value of true allows
-- these upgrades to occur; false disables automatic upgrades. Default:
-- true.
crgAutoMinorVersionUpgrade :: Lens' CreateReplicationGroup (Maybe Bool)
crgAutoMinorVersionUpgrade =
    lens _crgAutoMinorVersionUpgrade
        (\s a -> s { _crgAutoMinorVersionUpgrade = a })

-- | Specifies whether a read-only replica will be automatically promoted to
-- read/write primary if the existing primary fails. If true, automatic
-- failover is enabled for this replication group. If false, automatic
-- failover is disabled for this replication group. Default: false.
crgAutomaticFailoverEnabled :: Lens' CreateReplicationGroup (Maybe Bool)
crgAutomaticFailoverEnabled =
    lens _crgAutomaticFailoverEnabled
        (\s a -> s { _crgAutomaticFailoverEnabled = a })

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
crgCacheNodeType :: Lens' CreateReplicationGroup (Maybe Text)
crgCacheNodeType = lens _crgCacheNodeType (\s a -> s { _crgCacheNodeType = a })

-- | The name of the parameter group to associate with this replication group.
-- If this argument is omitted, the default cache parameter group for the
-- specified engine is used.
crgCacheParameterGroupName :: Lens' CreateReplicationGroup (Maybe Text)
crgCacheParameterGroupName =
    lens _crgCacheParameterGroupName
        (\s a -> s { _crgCacheParameterGroupName = a })

-- | A list of cache security group names to associate with this replication
-- group.
crgCacheSecurityGroupNames :: Lens' CreateReplicationGroup [Text]
crgCacheSecurityGroupNames =
    lens _crgCacheSecurityGroupNames
        (\s a -> s { _crgCacheSecurityGroupNames = a })
            . _List

-- | The name of the cache subnet group to be used for the replication group.
crgCacheSubnetGroupName :: Lens' CreateReplicationGroup (Maybe Text)
crgCacheSubnetGroupName =
    lens _crgCacheSubnetGroupName (\s a -> s { _crgCacheSubnetGroupName = a })

-- | The name of the cache engine to be used for the cache clusters in this
-- replication group. Default: redis.
crgEngine :: Lens' CreateReplicationGroup (Maybe Text)
crgEngine = lens _crgEngine (\s a -> s { _crgEngine = a })

-- | The version number of the cach engine to be used for the cache clusters
-- in this replication group. To view the supported cache engine versions,
-- use the DescribeCacheEngineVersions operation.
crgEngineVersion :: Lens' CreateReplicationGroup (Maybe Text)
crgEngineVersion = lens _crgEngineVersion (\s a -> s { _crgEngineVersion = a })

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- (SNS) topic to which notifications will be sent.
crgNotificationTopicArn :: Lens' CreateReplicationGroup (Maybe Text)
crgNotificationTopicArn =
    lens _crgNotificationTopicArn (\s a -> s { _crgNotificationTopicArn = a })

-- | The number of cache clusters this replication group will initially have.
-- If AutomaticFailover is enabled, the value of this parameter must be at
-- least 2. The maximum permitted value for NumCacheClusters is 6 (primary
-- plus 5 replicas). If you need to exceed this limit, please fill out the
-- ElastiCache Limit Increase Request forrm at
-- http://aws.amazon.com/contact-us/elasticache-node-limit-request.
crgNumCacheClusters :: Lens' CreateReplicationGroup (Maybe Int)
crgNumCacheClusters =
    lens _crgNumCacheClusters (\s a -> s { _crgNumCacheClusters = a })

-- | The port number on which each member of the replication group will accept
-- connections.
crgPort :: Lens' CreateReplicationGroup (Maybe Int)
crgPort = lens _crgPort (\s a -> s { _crgPort = a })

-- | A list of EC2 availability zones in which the replication group's cache
-- clusters will be created. The order of the availability zones in the list
-- is not important. Default: system chosen availability zones. Example: One
-- Redis cache cluster in each of three availability zones.
-- PreferredAvailabilityZones.member.1=us-east-1a
-- PreferredAvailabilityZones.member.2=us-east-1c
-- PreferredAvailabilityZones.member.3=us-east-1d.
crgPreferredCacheClusterAZs :: Lens' CreateReplicationGroup [Text]
crgPreferredCacheClusterAZs =
    lens _crgPreferredCacheClusterAZs
        (\s a -> s { _crgPreferredCacheClusterAZs = a })
            . _List

-- | The weekly time range (in UTC) during which system maintenance can occur.
-- Example: sun:05:00-sun:09:00.
crgPreferredMaintenanceWindow :: Lens' CreateReplicationGroup (Maybe Text)
crgPreferredMaintenanceWindow =
    lens _crgPreferredMaintenanceWindow
        (\s a -> s { _crgPreferredMaintenanceWindow = a })

-- | The identifier of the cache cluster that will serve as the primary for
-- this replication group. This cache cluster must already exist and have a
-- status of available. This parameter is not required if NumCacheClusters
-- is specified.
crgPrimaryClusterId :: Lens' CreateReplicationGroup (Maybe Text)
crgPrimaryClusterId =
    lens _crgPrimaryClusterId (\s a -> s { _crgPrimaryClusterId = a })

-- | A user-created description for the replication group.
crgReplicationGroupDescription :: Lens' CreateReplicationGroup Text
crgReplicationGroupDescription =
    lens _crgReplicationGroupDescription
        (\s a -> s { _crgReplicationGroupDescription = a })

-- | The replication group identifier. This parameter is stored as a lowercase
-- string. Constraints: A name must contain from 1 to 20 alphanumeric
-- characters or hyphens. The first character must be a letter. A name
-- cannot end with a hyphen or contain two consecutive hyphens.
crgReplicationGroupId :: Lens' CreateReplicationGroup Text
crgReplicationGroupId =
    lens _crgReplicationGroupId (\s a -> s { _crgReplicationGroupId = a })

-- | One or more Amazon VPC security groups associated with this replication
-- group. Use this parameter only when you are creating a replication group
-- in an Amazon Virtual Private Cloud (VPC).
crgSecurityGroupIds :: Lens' CreateReplicationGroup [Text]
crgSecurityGroupIds =
    lens _crgSecurityGroupIds (\s a -> s { _crgSecurityGroupIds = a })
        . _List

-- | A single-element string list containing an Amazon Resource Name (ARN)
-- that uniquely identifies a Redis RDB snapshot file stored in Amazon S3.
-- The snapshot file will be used to populate the node group. The Amazon S3
-- object name in the ARN cannot contain any commas. Note: This parameter is
-- only valid if the Engine parameter is redis. Example of an Amazon S3 ARN:
-- arn:aws:s3:::my_bucket/snapshot1.rdb.
crgSnapshotArns :: Lens' CreateReplicationGroup [Text]
crgSnapshotArns = lens _crgSnapshotArns (\s a -> s { _crgSnapshotArns = a }) . _List

-- | The name of a snapshot from which to restore data into the new node
-- group. The snapshot status changes to restoring while the new node group
-- is being created. Note: This parameter is only valid if the Engine
-- parameter is redis.
crgSnapshotName :: Lens' CreateReplicationGroup (Maybe Text)
crgSnapshotName = lens _crgSnapshotName (\s a -> s { _crgSnapshotName = a })

-- | The number of days for which ElastiCache will retain automatic snapshots
-- before deleting them. For example, if you set SnapshotRetentionLimit to
-- 5, then a snapshot that was taken today will be retained for 5 days
-- before being deleted. Note: This parameter is only valid if the Engine
-- parameter is redis. Default: 0 (i.e., automatic backups are disabled for
-- this cache cluster).
crgSnapshotRetentionLimit :: Lens' CreateReplicationGroup (Maybe Int)
crgSnapshotRetentionLimit =
    lens _crgSnapshotRetentionLimit
        (\s a -> s { _crgSnapshotRetentionLimit = a })

-- | The daily time range (in UTC) during which ElastiCache will begin taking
-- a daily snapshot of your node group. Example: 05:00-09:00 If you do not
-- specify this parameter, then ElastiCache will automatically choose an
-- appropriate time range. Note: This parameter is only valid if the Engine
-- parameter is redis.
crgSnapshotWindow :: Lens' CreateReplicationGroup (Maybe Text)
crgSnapshotWindow =
    lens _crgSnapshotWindow (\s a -> s { _crgSnapshotWindow = a })

newtype CreateReplicationGroupResponse = CreateReplicationGroupResponse
    { _crgrReplicationGroup :: Maybe ReplicationGroup
    } deriving (Eq, Show)

-- | 'CreateReplicationGroupResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crgrReplicationGroup' @::@ 'Maybe' 'ReplicationGroup'
--
createReplicationGroupResponse :: CreateReplicationGroupResponse
createReplicationGroupResponse = CreateReplicationGroupResponse
    { _crgrReplicationGroup = Nothing
    }

crgrReplicationGroup :: Lens' CreateReplicationGroupResponse (Maybe ReplicationGroup)
crgrReplicationGroup =
    lens _crgrReplicationGroup (\s a -> s { _crgrReplicationGroup = a })

instance ToPath CreateReplicationGroup where
    toPath = const "/"

instance ToQuery CreateReplicationGroup where
    toQuery CreateReplicationGroup{..} = mconcat
        [ "AutoMinorVersionUpgrade"     =? _crgAutoMinorVersionUpgrade
        , "AutomaticFailoverEnabled"    =? _crgAutomaticFailoverEnabled
        , "CacheNodeType"               =? _crgCacheNodeType
        , "CacheParameterGroupName"     =? _crgCacheParameterGroupName
        , "CacheSecurityGroupNames"     =? _crgCacheSecurityGroupNames
        , "CacheSubnetGroupName"        =? _crgCacheSubnetGroupName
        , "Engine"                      =? _crgEngine
        , "EngineVersion"               =? _crgEngineVersion
        , "NotificationTopicArn"        =? _crgNotificationTopicArn
        , "NumCacheClusters"            =? _crgNumCacheClusters
        , "Port"                        =? _crgPort
        , "PreferredCacheClusterAZs"    =? _crgPreferredCacheClusterAZs
        , "PreferredMaintenanceWindow"  =? _crgPreferredMaintenanceWindow
        , "PrimaryClusterId"            =? _crgPrimaryClusterId
        , "ReplicationGroupDescription" =? _crgReplicationGroupDescription
        , "ReplicationGroupId"          =? _crgReplicationGroupId
        , "SecurityGroupIds"            =? _crgSecurityGroupIds
        , "SnapshotArns"                =? _crgSnapshotArns
        , "SnapshotName"                =? _crgSnapshotName
        , "SnapshotRetentionLimit"      =? _crgSnapshotRetentionLimit
        , "SnapshotWindow"              =? _crgSnapshotWindow
        ]

instance ToHeaders CreateReplicationGroup

query

instance AWSRequest CreateReplicationGroup where
    type Sv CreateReplicationGroup = ElastiCache
    type Rs CreateReplicationGroup = CreateReplicationGroupResponse

    request  = post "CreateReplicationGroup"
    response = xmlResponse

instance FromXML CreateReplicationGroupResponse where
    parseXML = withElement "CreateReplicationGroupResult" $ \x -> CreateReplicationGroupResponse
        <$> x .@? "ReplicationGroup"
