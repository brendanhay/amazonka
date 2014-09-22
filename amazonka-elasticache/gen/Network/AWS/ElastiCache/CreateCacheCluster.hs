{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
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
module Network.AWS.ElastiCache.CreateCacheCluster
    (
    -- * Request
      CreateCacheCluster
    -- ** Request constructor
    , createCacheCluster
    -- ** Request lenses
    , cccCacheClusterId
    , cccReplicationGroupId
    , cccNumCacheNodes
    , cccCacheNodeType
    , cccEngine
    , cccEngineVersion
    , cccCacheParameterGroupName
    , cccCacheSubnetGroupName
    , cccCacheSecurityGroupName
    , cccSecurityGroupId
    , cccSnapshotArn
    , cccSnapshotName
    , cccAZMode
    , cccPreferredAvailabilityZone
    , cccPreferredAvailabilityZone
    , cccPreferredMaintenanceWindow
    , cccPort
    , cccNotificationTopicArn
    , cccAutoMinorVersionUpgrade
    , cccSnapshotRetentionLimit
    , cccSnapshotWindow

    -- * Response
    , CreateCacheClusterResponse
    -- ** Response constructor
    , createCacheClusterResponse
    -- ** Response lenses
    , cccrCacheCluster
    ) where

import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import Network.AWS.Prelude

-- | Represents the input of a CreateCacheCluster operation.
data CreateCacheCluster = CreateCacheCluster
    { _cccCacheClusterId :: Text
    , _cccReplicationGroupId :: Maybe Text
    , _cccNumCacheNodes :: Maybe Integer
    , _cccCacheNodeType :: Maybe Text
    , _cccEngine :: Maybe Text
    , _cccEngineVersion :: Maybe Text
    , _cccCacheParameterGroupName :: Maybe Text
    , _cccCacheSubnetGroupName :: Maybe Text
    , _cccCacheSecurityGroupName :: [Text]
    , _cccSecurityGroupId :: [Text]
    , _cccSnapshotArn :: [Text]
    , _cccSnapshotName :: Maybe Text
    , _cccAZMode :: Maybe Text
    , _cccPreferredAvailabilityZone :: Maybe Text
    , _cccPreferredAvailabilityZone :: [Text]
    , _cccPreferredMaintenanceWindow :: Maybe Text
    , _cccPort :: Maybe Integer
    , _cccNotificationTopicArn :: Maybe Text
    , _cccAutoMinorVersionUpgrade :: Maybe Bool
    , _cccSnapshotRetentionLimit :: Maybe Integer
    , _cccSnapshotWindow :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateCacheCluster' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CacheClusterId ::@ @Text@
--
-- * @ReplicationGroupId ::@ @Maybe Text@
--
-- * @NumCacheNodes ::@ @Maybe Integer@
--
-- * @CacheNodeType ::@ @Maybe Text@
--
-- * @Engine ::@ @Maybe Text@
--
-- * @EngineVersion ::@ @Maybe Text@
--
-- * @CacheParameterGroupName ::@ @Maybe Text@
--
-- * @CacheSubnetGroupName ::@ @Maybe Text@
--
-- * @CacheSecurityGroupName ::@ @[Text]@
--
-- * @SecurityGroupId ::@ @[Text]@
--
-- * @SnapshotArn ::@ @[Text]@
--
-- * @SnapshotName ::@ @Maybe Text@
--
-- * @AZMode ::@ @Maybe Text@
--
-- * @PreferredAvailabilityZone ::@ @Maybe Text@
--
-- * @PreferredAvailabilityZone ::@ @[Text]@
--
-- * @PreferredMaintenanceWindow ::@ @Maybe Text@
--
-- * @Port ::@ @Maybe Integer@
--
-- * @NotificationTopicArn ::@ @Maybe Text@
--
-- * @AutoMinorVersionUpgrade ::@ @Maybe Bool@
--
-- * @SnapshotRetentionLimit ::@ @Maybe Integer@
--
-- * @SnapshotWindow ::@ @Maybe Text@
--
createCacheCluster :: Text -- ^ 'cccCacheClusterId'
                   -> CreateCacheCluster
createCacheCluster p1 = CreateCacheCluster
    { _cccCacheClusterId = p1
    , _cccReplicationGroupId = Nothing
    , _cccNumCacheNodes = Nothing
    , _cccCacheNodeType = Nothing
    , _cccEngine = Nothing
    , _cccEngineVersion = Nothing
    , _cccCacheParameterGroupName = Nothing
    , _cccCacheSubnetGroupName = Nothing
    , _cccCacheSecurityGroupName = mempty
    , _cccSecurityGroupId = mempty
    , _cccSnapshotArn = mempty
    , _cccSnapshotName = Nothing
    , _cccAZMode = Nothing
    , _cccPreferredAvailabilityZone = Nothing
    , _cccPreferredAvailabilityZone = mempty
    , _cccPreferredMaintenanceWindow = Nothing
    , _cccPort = Nothing
    , _cccNotificationTopicArn = Nothing
    , _cccAutoMinorVersionUpgrade = Nothing
    , _cccSnapshotRetentionLimit = Nothing
    , _cccSnapshotWindow = Nothing
    }

-- | The cache cluster identifier. This parameter is stored as a lowercase
-- string. Constraints: Must contain from 1 to 20 alphanumeric characters or
-- hyphens. First character must be a letter. Cannot end with a hyphen or
-- contain two consecutive hyphens.
cccCacheClusterId :: Lens' CreateCacheCluster Text
cccCacheClusterId =
    lens _cccCacheClusterId (\s a -> s { _cccCacheClusterId = a })

-- | The replication group to which this cache cluster should belong. If this
-- parameter is specified, the cache cluster will be added to the specified
-- replication group as a read replica; otherwise, the cache cluster will be a
-- standalone primary that is not part of any replication group.
cccReplicationGroupId :: Lens' CreateCacheCluster (Maybe Text)
cccReplicationGroupId =
    lens _cccReplicationGroupId (\s a -> s { _cccReplicationGroupId = a })

-- | The initial number of cache nodes that the cache cluster will have. For a
-- Memcached cluster, valid values are between 1 and 20. If you need to exceed
-- this limit, please fill out the ElastiCache Limit Increase Request form at
-- . For Redis, only single-node cache clusters are supported at this time, so
-- the value for this parameter must be 1.
cccNumCacheNodes :: Lens' CreateCacheCluster (Maybe Integer)
cccNumCacheNodes =
    lens _cccNumCacheNodes (\s a -> s { _cccNumCacheNodes = a })

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
cccCacheNodeType :: Lens' CreateCacheCluster (Maybe Text)
cccCacheNodeType =
    lens _cccCacheNodeType (\s a -> s { _cccCacheNodeType = a })

-- | The name of the cache engine to be used for this cache cluster. Valid
-- values for this parameter are: memcached | redis.
cccEngine :: Lens' CreateCacheCluster (Maybe Text)
cccEngine = lens _cccEngine (\s a -> s { _cccEngine = a })

-- | The version number of the cache engine to be used for this cluster. To view
-- the supported cache engine versions, use the DescribeCacheEngineVersions
-- operation.
cccEngineVersion :: Lens' CreateCacheCluster (Maybe Text)
cccEngineVersion =
    lens _cccEngineVersion (\s a -> s { _cccEngineVersion = a })

-- | The name of the cache parameter group to associate with this cache cluster.
-- If this argument is omitted, the default cache parameter group for the
-- specified engine will be used.
cccCacheParameterGroupName :: Lens' CreateCacheCluster (Maybe Text)
cccCacheParameterGroupName =
    lens _cccCacheParameterGroupName
         (\s a -> s { _cccCacheParameterGroupName = a })

-- | The name of the cache subnet group to be used for the cache cluster. Use
-- this parameter only when you are creating a cluster in an Amazon Virtual
-- Private Cloud (VPC).
cccCacheSubnetGroupName :: Lens' CreateCacheCluster (Maybe Text)
cccCacheSubnetGroupName =
    lens _cccCacheSubnetGroupName
         (\s a -> s { _cccCacheSubnetGroupName = a })

-- | A list of cache security group names to associate with this cache cluster.
-- Use this parameter only when you are creating a cluster outside of an
-- Amazon Virtual Private Cloud (VPC).
cccCacheSecurityGroupName :: Lens' CreateCacheCluster [Text]
cccCacheSecurityGroupName =
    lens _cccCacheSecurityGroupName
         (\s a -> s { _cccCacheSecurityGroupName = a })

-- | One or more VPC security groups associated with the cache cluster. Use this
-- parameter only when you are creating a cluster in an Amazon Virtual Private
-- Cloud (VPC).
cccSecurityGroupId :: Lens' CreateCacheCluster [Text]
cccSecurityGroupId =
    lens _cccSecurityGroupId (\s a -> s { _cccSecurityGroupId = a })

-- | A single-element string list containing an Amazon Resource Name (ARN) that
-- uniquely identifies a Redis RDB snapshot file stored in Amazon S3. The
-- snapshot file will be used to populate the Redis cache in the new cache
-- cluster. The Amazon S3 object name in the ARN cannot contain any commas.
-- Here is an example of an Amazon S3 ARN:
-- arn:aws:s3:::my_bucket/snapshot1.rdb Note: This parameter is only valid if
-- the Engine parameter is redis.
cccSnapshotArn :: Lens' CreateCacheCluster [Text]
cccSnapshotArn = lens _cccSnapshotArn (\s a -> s { _cccSnapshotArn = a })

-- | The name of a snapshot from which to restore data into the new cache
-- cluster. The snapshot's status changes to restoring while the new cache
-- cluster is being created.
cccSnapshotName :: Lens' CreateCacheCluster (Maybe Text)
cccSnapshotName = lens _cccSnapshotName (\s a -> s { _cccSnapshotName = a })

-- | Specifies whether the nodes in this Memcached cache cluster are created in
-- a single Availability Zone or created across multiple Availability Zones.
-- This option is only supported for Memcached cache clusters. If the AZMode
-- and PreferredAvailabilityZones are not specified, ElastiCache assumes
-- single-az mode. Valid values: single-az | cross-az.
cccAZMode :: Lens' CreateCacheCluster (Maybe Text)
cccAZMode = lens _cccAZMode (\s a -> s { _cccAZMode = a })

-- | The EC2 Availability Zone in which the cache cluster will be created. All
-- cache nodes belonging to this Memcached cache cluster are placed in the
-- preferred Availability Zone. If you want to create your cache nodes across
-- multiple Availability Zones, use PreferredAvailabilityZones. Default:
-- System chosen Availability Zone.
cccPreferredAvailabilityZone :: Lens' CreateCacheCluster (Maybe Text)
cccPreferredAvailabilityZone =
    lens _cccPreferredAvailabilityZone
         (\s a -> s { _cccPreferredAvailabilityZone = a })

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
cccPreferredAvailabilityZone :: Lens' CreateCacheCluster [Text]
cccPreferredAvailabilityZone =
    lens _cccPreferredAvailabilityZone
         (\s a -> s { _cccPreferredAvailabilityZone = a })

-- | The weekly time range (in UTC) during which system maintenance can occur.
-- Example: sun:05:00-sun:09:00.
cccPreferredMaintenanceWindow :: Lens' CreateCacheCluster (Maybe Text)
cccPreferredMaintenanceWindow =
    lens _cccPreferredMaintenanceWindow
         (\s a -> s { _cccPreferredMaintenanceWindow = a })

-- | The port number on which each of the cache nodes will accept connections.
cccPort :: Lens' CreateCacheCluster (Maybe Integer)
cccPort = lens _cccPort (\s a -> s { _cccPort = a })

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- (SNS) topic to which notifications will be sent. The Amazon SNS topic owner
-- must be the same as the cache cluster owner.
cccNotificationTopicArn :: Lens' CreateCacheCluster (Maybe Text)
cccNotificationTopicArn =
    lens _cccNotificationTopicArn
         (\s a -> s { _cccNotificationTopicArn = a })

-- | Determines whether minor engine upgrades will be applied automatically to
-- the cache cluster during the maintenance window. A value of true allows
-- these upgrades to occur; false disables automatic upgrades. Default: true.
cccAutoMinorVersionUpgrade :: Lens' CreateCacheCluster (Maybe Bool)
cccAutoMinorVersionUpgrade =
    lens _cccAutoMinorVersionUpgrade
         (\s a -> s { _cccAutoMinorVersionUpgrade = a })

-- | The number of days for which ElastiCache will retain automatic cache
-- cluster snapshots before deleting them. For example, if you set
-- SnapshotRetentionLimit to 5, then a snapshot that was taken today will be
-- retained for 5 days before being deleted. If you do not specify this
-- parameter, then SnapshotRetentionLimit will be set to 0 (i.e., automatic
-- backups will be disabled for this cache cluster).
cccSnapshotRetentionLimit :: Lens' CreateCacheCluster (Maybe Integer)
cccSnapshotRetentionLimit =
    lens _cccSnapshotRetentionLimit
         (\s a -> s { _cccSnapshotRetentionLimit = a })

-- | The daily time range (in UTC) during which ElastiCache will begin taking a
-- daily snapshot of your cache cluster. Example: 05:00-09:00 If you do not
-- specify this parameter, then ElastiCache will automatically choose an
-- appropriate time range.
cccSnapshotWindow :: Lens' CreateCacheCluster (Maybe Text)
cccSnapshotWindow =
    lens _cccSnapshotWindow (\s a -> s { _cccSnapshotWindow = a })

instance ToQuery CreateCacheCluster where
    toQuery = genericQuery def

newtype CreateCacheClusterResponse = CreateCacheClusterResponse
    { _cccrCacheCluster :: Maybe CacheCluster
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateCacheClusterResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CacheCluster ::@ @Maybe CacheCluster@
--
createCacheClusterResponse :: CreateCacheClusterResponse
createCacheClusterResponse = CreateCacheClusterResponse
    { _cccrCacheCluster = Nothing
    }

-- | Contains all of the attributes of a specific cache cluster.
cccrCacheCluster :: Lens' CreateCacheClusterResponse (Maybe CacheCluster)
cccrCacheCluster =
    lens _cccrCacheCluster (\s a -> s { _cccrCacheCluster = a })

instance FromXML CreateCacheClusterResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateCacheCluster where
    type Sv CreateCacheCluster = ElastiCache
    type Rs CreateCacheCluster = CreateCacheClusterResponse

    request = post "CreateCacheCluster"
    response _ = xmlResponse
