{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ElastiCache.V2014_07_15.CreateCacheCluster
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The CreateCacheCluster operation creates a new cache cluster. All nodes in
-- the cache cluster run the same protocol-compliant cache engine software -
-- either Memcached or Redis. https://elasticache.us-east-1.amazonaws.com/
-- ?Action=CreateCacheCluster &CacheClusterId=myMemcachedCluster
-- &CacheNodeType=cache.m1.small &CacheSecurityGroupNames.member.1=default
-- &Engine=memcached &NumCacheNodes=3
-- &PreferredAvailabilityZones.member.1=us-east-1a
-- &PreferredAvailabilityZones.member.2=us-east-1b
-- &PreferredAvailabilityZones.member.3=us-east-1e
-- &SignatureMethod=HmacSHA256&SignatureVersion=4 &Version=2014-03-24
-- &X-Amz-Algorithm=AWS4-HMAC-SHA256
-- &X-Amz-Credential=[your-access-key-id]/20140721/us-east-1/elasticache/aws4_request
-- &X-Amz-Date=20140724T170651Z
-- &X-Amz-SignedHeaders=content-type;host;user-agent;x-amz-content-sha256;x-amz-date
-- &X-Amz-Signature=[signature-value] creating 3 memcached 1.4.5 true
-- sun:08:00-sun:09:00 cache.m1.large default active mycache
-- aaf2e796-363f-11e0-a564-8f11342c56b0 ]]> -->.
module Network.AWS.ElastiCache.V2014_07_15.CreateCacheCluster
    (
    -- * Request
      CreateCacheCluster
    -- ** Request constructor
    , mkCreateCacheClusterMessage
    -- ** Request lenses
    , cccmCacheClusterId
    , cccmReplicationGroupId
    , cccmNumCacheNodes
    , cccmCacheNodeType
    , cccmEngine
    , cccmEngineVersion
    , cccmCacheParameterGroupName
    , cccmCacheSubnetGroupName
    , cccmCacheSecurityGroupNames
    , cccmSecurityGroupIds
    , cccmSnapshotArns
    , cccmSnapshotName
    , cccmAZMode
    , cccmPreferredAvailabilityZone
    , cccmPreferredAvailabilityZones
    , cccmPreferredMaintenanceWindow
    , cccmPort
    , cccmNotificationTopicArn
    , cccmAutoMinorVersionUpgrade
    , cccmSnapshotRetentionLimit
    , cccmSnapshotWindow

    -- * Response
    , CreateCacheClusterResponse
    -- ** Response lenses
    , ccwCacheCluster
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.V2014_07_15.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateCacheCluster' request.
mkCreateCacheClusterMessage :: Text -- ^ 'cccmCacheClusterId'
                            -> CreateCacheCluster
mkCreateCacheClusterMessage p1 = CreateCacheCluster
    { _cccmCacheClusterId = p1
    , _cccmReplicationGroupId = Nothing
    , _cccmNumCacheNodes = Nothing
    , _cccmCacheNodeType = Nothing
    , _cccmEngine = Nothing
    , _cccmEngineVersion = Nothing
    , _cccmCacheParameterGroupName = Nothing
    , _cccmCacheSubnetGroupName = Nothing
    , _cccmCacheSecurityGroupNames = mempty
    , _cccmSecurityGroupIds = mempty
    , _cccmSnapshotArns = mempty
    , _cccmSnapshotName = Nothing
    , _cccmAZMode = Nothing
    , _cccmPreferredAvailabilityZone = Nothing
    , _cccmPreferredAvailabilityZones = mempty
    , _cccmPreferredMaintenanceWindow = Nothing
    , _cccmPort = Nothing
    , _cccmNotificationTopicArn = Nothing
    , _cccmAutoMinorVersionUpgrade = Nothing
    , _cccmSnapshotRetentionLimit = Nothing
    , _cccmSnapshotWindow = Nothing
    }
{-# INLINE mkCreateCacheClusterMessage #-}

data CreateCacheCluster = CreateCacheCluster
    { _cccmCacheClusterId :: Text
      -- ^ The cache cluster identifier. This parameter is stored as a
      -- lowercase string. Constraints: Must contain from 1 to 20
      -- alphanumeric characters or hyphens. First character must be a
      -- letter. Cannot end with a hyphen or contain two consecutive
      -- hyphens.
    , _cccmReplicationGroupId :: Maybe Text
      -- ^ The replication group to which this cache cluster should belong.
      -- If this parameter is specified, the cache cluster will be added
      -- to the specified replication group as a read replica; otherwise,
      -- the cache cluster will be a standalone primary that is not part
      -- of any replication group.
    , _cccmNumCacheNodes :: Maybe Integer
      -- ^ The initial number of cache nodes that the cache cluster will
      -- have. For a Memcached cluster, valid values are between 1 and 20.
      -- If you need to exceed this limit, please fill out the ElastiCache
      -- Limit Increase Request form at . For Redis, only single-node
      -- cache clusters are supported at this time, so the value for this
      -- parameter must be 1.
    , _cccmCacheNodeType :: Maybe Text
      -- ^ The compute and memory capacity of the nodes in the cache
      -- cluster. Valid cache types Microcache.t1.micro | cache.m1.small
      -- General Purpose Current Generationcache.m3.medium |
      -- cache.m3.large | cache.m3.xlarge | cache.m3.2xlarge Previous
      -- Generationcache.m1.medium | cache.m1.large | cache.m1.xlarge
      -- Compute Optimizedcache.c1.xlarge Memory Optimized Current
      -- Generationcache.r3.large | cache.r3.xlarge | cache.r3.2xlarge |
      -- cache.r3.4xlarge | cache.r3.8xlarge Previous
      -- Generationcache.m2.xlarge | cache.m2.2xlarge | cache.m2.4xlarge
      -- For a complete listing of cache node types and specifications,
      -- see Cache Node Type-Specific Parameters for Memcached or Cache
      -- Node Type-Specific Parameters for Redis and Amazon ElastiCache
      -- Product Features and Details.
    , _cccmEngine :: Maybe Text
      -- ^ The name of the cache engine to be used for this cache cluster.
      -- Valid values for this parameter are: memcached | redis.
    , _cccmEngineVersion :: Maybe Text
      -- ^ The version number of the cache engine to be used for this
      -- cluster. To view the supported cache engine versions, use the
      -- DescribeCacheEngineVersions operation.
    , _cccmCacheParameterGroupName :: Maybe Text
      -- ^ The name of the cache parameter group to associate with this
      -- cache cluster. If this argument is omitted, the default cache
      -- parameter group for the specified engine will be used.
    , _cccmCacheSubnetGroupName :: Maybe Text
      -- ^ The name of the cache subnet group to be used for the cache
      -- cluster. Use this parameter only when you are creating a cluster
      -- in an Amazon Virtual Private Cloud (VPC).
    , _cccmCacheSecurityGroupNames :: [Text]
      -- ^ A list of cache security group names to associate with this cache
      -- cluster. Use this parameter only when you are creating a cluster
      -- outside of an Amazon Virtual Private Cloud (VPC).
    , _cccmSecurityGroupIds :: [Text]
      -- ^ One or more VPC security groups associated with the cache
      -- cluster. Use this parameter only when you are creating a cluster
      -- in an Amazon Virtual Private Cloud (VPC).
    , _cccmSnapshotArns :: [Text]
      -- ^ A single-element string list containing an Amazon Resource Name
      -- (ARN) that uniquely identifies a Redis RDB snapshot file stored
      -- in Amazon S3. The snapshot file will be used to populate the
      -- Redis cache in the new cache cluster. The Amazon S3 object name
      -- in the ARN cannot contain any commas. Here is an example of an
      -- Amazon S3 ARN: arn:aws:s3:::my_bucket/snapshot1.rdb Note: This
      -- parameter is only valid if the Engine parameter is redis.
    , _cccmSnapshotName :: Maybe Text
      -- ^ The name of a snapshot from which to restore data into the new
      -- cache cluster. The snapshot's status changes to restoring while
      -- the new cache cluster is being created.
    , _cccmAZMode :: Maybe Text
      -- ^ Specifies whether the nodes in this Memcached cache cluster are
      -- created in a single Availability Zone or created across multiple
      -- Availability Zones. This option is only supported for Memcached
      -- cache clusters. If the AZMode and PreferredAvailabilityZones are
      -- not specified, ElastiCache assumes single-az mode. Valid values:
      -- single-az | cross-az.
    , _cccmPreferredAvailabilityZone :: Maybe Text
      -- ^ The EC2 Availability Zone in which the cache cluster will be
      -- created. All cache nodes belonging to this Memcached cache
      -- cluster are placed in the preferred Availability Zone. If you
      -- want to create your cache nodes across multiple Availability
      -- Zones, use PreferredAvailabilityZones. Default: System chosen
      -- Availability Zone.
    , _cccmPreferredAvailabilityZones :: [Text]
      -- ^ A list of the Availability Zones in which nodes will be created.
      -- The order of the zones in the list is not important. This option
      -- is only supported on Memcached clusters. If you are creating your
      -- cache cluster in an Amazon VPC (recommended) you can only locate
      -- nodes in Availability Zones that are associated with the subnets
      -- in the selected subnet group. The number of Availability Zones
      -- listed must equal the value of NumCacheNodes. If you want all
      -- your cache nodes in the same Availability Zone, use
      -- PreferredAvailabilityZone instead or repeat the Availability Zone
      -- multiple times in the list. Default: System chosen Availability
      -- Zones. Example: One Memcached node in each of three Availability
      -- Zones:
      -- PreferredAvailabilityZones.member.1=us-east-1a&amp;PreferredAvailabilityZones.member.2=us-east-1b&amp;PreferredAvailabilityZones.member.3=us-east-1d
      -- Example: All three Memcached nodes in one Availability Zone:
      -- PreferredAvailabilityZones.member.1=us-east-1a&amp;PreferredAvailabilityZones.member.2=us-east-1a&amp;PreferredAvailabilityZones.member.3=us-east-1a.
      -- 
    , _cccmPreferredMaintenanceWindow :: Maybe Text
      -- ^ The weekly time range (in UTC) during which system maintenance
      -- can occur. Example: sun:05:00-sun:09:00.
    , _cccmPort :: Maybe Integer
      -- ^ The port number on which each of the cache nodes will accept
      -- connections.
    , _cccmNotificationTopicArn :: Maybe Text
      -- ^ The Amazon Resource Name (ARN) of the Amazon Simple Notification
      -- Service (SNS) topic to which notifications will be sent. The
      -- Amazon SNS topic owner must be the same as the cache cluster
      -- owner.
    , _cccmAutoMinorVersionUpgrade :: Maybe Bool
      -- ^ Determines whether minor engine upgrades will be applied
      -- automatically to the cache cluster during the maintenance window.
      -- A value of true allows these upgrades to occur; false disables
      -- automatic upgrades. Default: true.
    , _cccmSnapshotRetentionLimit :: Maybe Integer
      -- ^ The number of days for which ElastiCache will retain automatic
      -- cache cluster snapshots before deleting them. For example, if you
      -- set SnapshotRetentionLimit to 5, then a snapshot that was taken
      -- today will be retained for 5 days before being deleted. If you do
      -- not specify this parameter, then SnapshotRetentionLimit will be
      -- set to 0 (i.e., automatic backups will be disabled for this cache
      -- cluster).
    , _cccmSnapshotWindow :: Maybe Text
      -- ^ The daily time range (in UTC) during which ElastiCache will begin
      -- taking a daily snapshot of your cache cluster. Example:
      -- 05:00-09:00 If you do not specify this parameter, then
      -- ElastiCache will automatically choose an appropriate time range.
    } deriving (Show, Generic)

-- | The cache cluster identifier. This parameter is stored as a lowercase
-- string. Constraints: Must contain from 1 to 20 alphanumeric characters or
-- hyphens. First character must be a letter. Cannot end with a hyphen or
-- contain two consecutive hyphens.
cccmCacheClusterId :: Lens' CreateCacheCluster (Text)
cccmCacheClusterId = lens _cccmCacheClusterId (\s a -> s { _cccmCacheClusterId = a })
{-# INLINE cccmCacheClusterId #-}

-- | The replication group to which this cache cluster should belong. If this
-- parameter is specified, the cache cluster will be added to the specified
-- replication group as a read replica; otherwise, the cache cluster will be a
-- standalone primary that is not part of any replication group.
cccmReplicationGroupId :: Lens' CreateCacheCluster (Maybe Text)
cccmReplicationGroupId = lens _cccmReplicationGroupId (\s a -> s { _cccmReplicationGroupId = a })
{-# INLINE cccmReplicationGroupId #-}

-- | The initial number of cache nodes that the cache cluster will have. For a
-- Memcached cluster, valid values are between 1 and 20. If you need to exceed
-- this limit, please fill out the ElastiCache Limit Increase Request form at
-- . For Redis, only single-node cache clusters are supported at this time, so
-- the value for this parameter must be 1.
cccmNumCacheNodes :: Lens' CreateCacheCluster (Maybe Integer)
cccmNumCacheNodes = lens _cccmNumCacheNodes (\s a -> s { _cccmNumCacheNodes = a })
{-# INLINE cccmNumCacheNodes #-}

-- | The compute and memory capacity of the nodes in the cache cluster. Valid
-- cache types Microcache.t1.micro | cache.m1.small General Purpose Current
-- Generationcache.m3.medium | cache.m3.large | cache.m3.xlarge |
-- cache.m3.2xlarge Previous Generationcache.m1.medium | cache.m1.large |
-- cache.m1.xlarge Compute Optimizedcache.c1.xlarge Memory Optimized Current
-- Generationcache.r3.large | cache.r3.xlarge | cache.r3.2xlarge |
-- cache.r3.4xlarge | cache.r3.8xlarge Previous Generationcache.m2.xlarge |
-- cache.m2.2xlarge | cache.m2.4xlarge For a complete listing of cache node
-- types and specifications, see Cache Node Type-Specific Parameters for
-- Memcached or Cache Node Type-Specific Parameters for Redis and Amazon
-- ElastiCache Product Features and Details.
cccmCacheNodeType :: Lens' CreateCacheCluster (Maybe Text)
cccmCacheNodeType = lens _cccmCacheNodeType (\s a -> s { _cccmCacheNodeType = a })
{-# INLINE cccmCacheNodeType #-}

-- | The name of the cache engine to be used for this cache cluster. Valid
-- values for this parameter are: memcached | redis.
cccmEngine :: Lens' CreateCacheCluster (Maybe Text)
cccmEngine = lens _cccmEngine (\s a -> s { _cccmEngine = a })
{-# INLINE cccmEngine #-}

-- | The version number of the cache engine to be used for this cluster. To view
-- the supported cache engine versions, use the DescribeCacheEngineVersions
-- operation.
cccmEngineVersion :: Lens' CreateCacheCluster (Maybe Text)
cccmEngineVersion = lens _cccmEngineVersion (\s a -> s { _cccmEngineVersion = a })
{-# INLINE cccmEngineVersion #-}

-- | The name of the cache parameter group to associate with this cache cluster.
-- If this argument is omitted, the default cache parameter group for the
-- specified engine will be used.
cccmCacheParameterGroupName :: Lens' CreateCacheCluster (Maybe Text)
cccmCacheParameterGroupName = lens _cccmCacheParameterGroupName (\s a -> s { _cccmCacheParameterGroupName = a })
{-# INLINE cccmCacheParameterGroupName #-}

-- | The name of the cache subnet group to be used for the cache cluster. Use
-- this parameter only when you are creating a cluster in an Amazon Virtual
-- Private Cloud (VPC).
cccmCacheSubnetGroupName :: Lens' CreateCacheCluster (Maybe Text)
cccmCacheSubnetGroupName = lens _cccmCacheSubnetGroupName (\s a -> s { _cccmCacheSubnetGroupName = a })
{-# INLINE cccmCacheSubnetGroupName #-}

-- | A list of cache security group names to associate with this cache cluster.
-- Use this parameter only when you are creating a cluster outside of an
-- Amazon Virtual Private Cloud (VPC).
cccmCacheSecurityGroupNames :: Lens' CreateCacheCluster ([Text])
cccmCacheSecurityGroupNames = lens _cccmCacheSecurityGroupNames (\s a -> s { _cccmCacheSecurityGroupNames = a })
{-# INLINE cccmCacheSecurityGroupNames #-}

-- | One or more VPC security groups associated with the cache cluster. Use this
-- parameter only when you are creating a cluster in an Amazon Virtual Private
-- Cloud (VPC).
cccmSecurityGroupIds :: Lens' CreateCacheCluster ([Text])
cccmSecurityGroupIds = lens _cccmSecurityGroupIds (\s a -> s { _cccmSecurityGroupIds = a })
{-# INLINE cccmSecurityGroupIds #-}

-- | A single-element string list containing an Amazon Resource Name (ARN) that
-- uniquely identifies a Redis RDB snapshot file stored in Amazon S3. The
-- snapshot file will be used to populate the Redis cache in the new cache
-- cluster. The Amazon S3 object name in the ARN cannot contain any commas.
-- Here is an example of an Amazon S3 ARN:
-- arn:aws:s3:::my_bucket/snapshot1.rdb Note: This parameter is only valid if
-- the Engine parameter is redis.
cccmSnapshotArns :: Lens' CreateCacheCluster ([Text])
cccmSnapshotArns = lens _cccmSnapshotArns (\s a -> s { _cccmSnapshotArns = a })
{-# INLINE cccmSnapshotArns #-}

-- | The name of a snapshot from which to restore data into the new cache
-- cluster. The snapshot's status changes to restoring while the new cache
-- cluster is being created.
cccmSnapshotName :: Lens' CreateCacheCluster (Maybe Text)
cccmSnapshotName = lens _cccmSnapshotName (\s a -> s { _cccmSnapshotName = a })
{-# INLINE cccmSnapshotName #-}

-- | Specifies whether the nodes in this Memcached cache cluster are created in
-- a single Availability Zone or created across multiple Availability Zones.
-- This option is only supported for Memcached cache clusters. If the AZMode
-- and PreferredAvailabilityZones are not specified, ElastiCache assumes
-- single-az mode. Valid values: single-az | cross-az.
cccmAZMode :: Lens' CreateCacheCluster (Maybe Text)
cccmAZMode = lens _cccmAZMode (\s a -> s { _cccmAZMode = a })
{-# INLINE cccmAZMode #-}

-- | The EC2 Availability Zone in which the cache cluster will be created. All
-- cache nodes belonging to this Memcached cache cluster are placed in the
-- preferred Availability Zone. If you want to create your cache nodes across
-- multiple Availability Zones, use PreferredAvailabilityZones. Default:
-- System chosen Availability Zone.
cccmPreferredAvailabilityZone :: Lens' CreateCacheCluster (Maybe Text)
cccmPreferredAvailabilityZone = lens _cccmPreferredAvailabilityZone (\s a -> s { _cccmPreferredAvailabilityZone = a })
{-# INLINE cccmPreferredAvailabilityZone #-}

-- | A list of the Availability Zones in which nodes will be created. The order
-- of the zones in the list is not important. This option is only supported on
-- Memcached clusters. If you are creating your cache cluster in an Amazon VPC
-- (recommended) you can only locate nodes in Availability Zones that are
-- associated with the subnets in the selected subnet group. The number of
-- Availability Zones listed must equal the value of NumCacheNodes. If you
-- want all your cache nodes in the same Availability Zone, use
-- PreferredAvailabilityZone instead or repeat the Availability Zone multiple
-- times in the list. Default: System chosen Availability Zones. Example: One
-- Memcached node in each of three Availability Zones:
-- PreferredAvailabilityZones.member.1=us-east-1a&amp;PreferredAvailabilityZones.member.2=us-east-1b&amp;PreferredAvailabilityZones.member.3=us-east-1d
-- Example: All three Memcached nodes in one Availability Zone:
-- PreferredAvailabilityZones.member.1=us-east-1a&amp;PreferredAvailabilityZones.member.2=us-east-1a&amp;PreferredAvailabilityZones.member.3=us-east-1a.
-- 
cccmPreferredAvailabilityZones :: Lens' CreateCacheCluster ([Text])
cccmPreferredAvailabilityZones = lens _cccmPreferredAvailabilityZones (\s a -> s { _cccmPreferredAvailabilityZones = a })
{-# INLINE cccmPreferredAvailabilityZones #-}

-- | The weekly time range (in UTC) during which system maintenance can occur.
-- Example: sun:05:00-sun:09:00.
cccmPreferredMaintenanceWindow :: Lens' CreateCacheCluster (Maybe Text)
cccmPreferredMaintenanceWindow = lens _cccmPreferredMaintenanceWindow (\s a -> s { _cccmPreferredMaintenanceWindow = a })
{-# INLINE cccmPreferredMaintenanceWindow #-}

-- | The port number on which each of the cache nodes will accept connections.
cccmPort :: Lens' CreateCacheCluster (Maybe Integer)
cccmPort = lens _cccmPort (\s a -> s { _cccmPort = a })
{-# INLINE cccmPort #-}

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- (SNS) topic to which notifications will be sent. The Amazon SNS topic owner
-- must be the same as the cache cluster owner.
cccmNotificationTopicArn :: Lens' CreateCacheCluster (Maybe Text)
cccmNotificationTopicArn = lens _cccmNotificationTopicArn (\s a -> s { _cccmNotificationTopicArn = a })
{-# INLINE cccmNotificationTopicArn #-}

-- | Determines whether minor engine upgrades will be applied automatically to
-- the cache cluster during the maintenance window. A value of true allows
-- these upgrades to occur; false disables automatic upgrades. Default: true.
cccmAutoMinorVersionUpgrade :: Lens' CreateCacheCluster (Maybe Bool)
cccmAutoMinorVersionUpgrade = lens _cccmAutoMinorVersionUpgrade (\s a -> s { _cccmAutoMinorVersionUpgrade = a })
{-# INLINE cccmAutoMinorVersionUpgrade #-}

-- | The number of days for which ElastiCache will retain automatic cache
-- cluster snapshots before deleting them. For example, if you set
-- SnapshotRetentionLimit to 5, then a snapshot that was taken today will be
-- retained for 5 days before being deleted. If you do not specify this
-- parameter, then SnapshotRetentionLimit will be set to 0 (i.e., automatic
-- backups will be disabled for this cache cluster).
cccmSnapshotRetentionLimit :: Lens' CreateCacheCluster (Maybe Integer)
cccmSnapshotRetentionLimit = lens _cccmSnapshotRetentionLimit (\s a -> s { _cccmSnapshotRetentionLimit = a })
{-# INLINE cccmSnapshotRetentionLimit #-}

-- | The daily time range (in UTC) during which ElastiCache will begin taking a
-- daily snapshot of your cache cluster. Example: 05:00-09:00 If you do not
-- specify this parameter, then ElastiCache will automatically choose an
-- appropriate time range.
cccmSnapshotWindow :: Lens' CreateCacheCluster (Maybe Text)
cccmSnapshotWindow = lens _cccmSnapshotWindow (\s a -> s { _cccmSnapshotWindow = a })
{-# INLINE cccmSnapshotWindow #-}

instance ToQuery CreateCacheCluster where
    toQuery = genericQuery def

newtype CreateCacheClusterResponse = CreateCacheClusterResponse
    { _ccwCacheCluster :: Maybe CacheCluster
      -- ^ Contains all of the attributes of a specific cache cluster.
    } deriving (Show, Generic)

-- | Contains all of the attributes of a specific cache cluster.
ccwCacheCluster :: Lens' CreateCacheClusterResponse (Maybe CacheCluster)
ccwCacheCluster = lens _ccwCacheCluster (\s a -> s { _ccwCacheCluster = a })
{-# INLINE ccwCacheCluster #-}

instance FromXML CreateCacheClusterResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateCacheCluster where
    type Sv CreateCacheCluster = ElastiCache
    type Rs CreateCacheCluster = CreateCacheClusterResponse

    request = post "CreateCacheCluster"
    response _ = xmlResponse
