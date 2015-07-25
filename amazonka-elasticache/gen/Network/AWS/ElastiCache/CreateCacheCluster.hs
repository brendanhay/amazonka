{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.CreateCacheCluster
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The /CreateCacheCluster/ action creates a cache cluster. All nodes in
-- the cache cluster run the same protocol-compliant cache engine software,
-- either Memcached or Redis.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_CreateCacheCluster.html>
module Network.AWS.ElastiCache.CreateCacheCluster
    (
    -- * Request
      CreateCacheCluster
    -- ** Request constructor
    , createCacheCluster
    -- ** Request lenses
    , cccCacheNodeType
    , cccEngineVersion
    , cccSecurityGroupIds
    , cccAutoMinorVersionUpgrade
    , cccSnapshotARNs
    , cccCacheParameterGroupName
    , cccSnapshotWindow
    , cccEngine
    , cccPreferredAvailabilityZones
    , cccPreferredMaintenanceWindow
    , cccCacheSubnetGroupName
    , cccPreferredAvailabilityZone
    , cccSnapshotRetentionLimit
    , cccAZMode
    , cccSnapshotName
    , cccReplicationGroupId
    , cccNotificationTopicARN
    , cccTags
    , cccNumCacheNodes
    , cccCacheSecurityGroupNames
    , cccPort
    , cccCacheClusterId

    -- * Response
    , CreateCacheClusterResponse
    -- ** Response constructor
    , createCacheClusterResponse
    -- ** Response lenses
    , cccrsCacheCluster
    , cccrsStatus
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /CreateCacheCluster/ action.
--
-- /See:/ 'createCacheCluster' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cccCacheNodeType'
--
-- * 'cccEngineVersion'
--
-- * 'cccSecurityGroupIds'
--
-- * 'cccAutoMinorVersionUpgrade'
--
-- * 'cccSnapshotARNs'
--
-- * 'cccCacheParameterGroupName'
--
-- * 'cccSnapshotWindow'
--
-- * 'cccEngine'
--
-- * 'cccPreferredAvailabilityZones'
--
-- * 'cccPreferredMaintenanceWindow'
--
-- * 'cccCacheSubnetGroupName'
--
-- * 'cccPreferredAvailabilityZone'
--
-- * 'cccSnapshotRetentionLimit'
--
-- * 'cccAZMode'
--
-- * 'cccSnapshotName'
--
-- * 'cccReplicationGroupId'
--
-- * 'cccNotificationTopicARN'
--
-- * 'cccTags'
--
-- * 'cccNumCacheNodes'
--
-- * 'cccCacheSecurityGroupNames'
--
-- * 'cccPort'
--
-- * 'cccCacheClusterId'
data CreateCacheCluster = CreateCacheCluster'
    { _cccCacheNodeType              :: !(Maybe Text)
    , _cccEngineVersion              :: !(Maybe Text)
    , _cccSecurityGroupIds           :: !(Maybe [Text])
    , _cccAutoMinorVersionUpgrade    :: !(Maybe Bool)
    , _cccSnapshotARNs               :: !(Maybe [Text])
    , _cccCacheParameterGroupName    :: !(Maybe Text)
    , _cccSnapshotWindow             :: !(Maybe Text)
    , _cccEngine                     :: !(Maybe Text)
    , _cccPreferredAvailabilityZones :: !(Maybe [Text])
    , _cccPreferredMaintenanceWindow :: !(Maybe Text)
    , _cccCacheSubnetGroupName       :: !(Maybe Text)
    , _cccPreferredAvailabilityZone  :: !(Maybe Text)
    , _cccSnapshotRetentionLimit     :: !(Maybe Int)
    , _cccAZMode                     :: !(Maybe AZMode)
    , _cccSnapshotName               :: !(Maybe Text)
    , _cccReplicationGroupId         :: !(Maybe Text)
    , _cccNotificationTopicARN       :: !(Maybe Text)
    , _cccTags                       :: !(Maybe [Tag])
    , _cccNumCacheNodes              :: !(Maybe Int)
    , _cccCacheSecurityGroupNames    :: !(Maybe [Text])
    , _cccPort                       :: !(Maybe Int)
    , _cccCacheClusterId             :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateCacheCluster' smart constructor.
createCacheCluster :: Text -> CreateCacheCluster
createCacheCluster pCacheClusterId_ =
    CreateCacheCluster'
    { _cccCacheNodeType = Nothing
    , _cccEngineVersion = Nothing
    , _cccSecurityGroupIds = Nothing
    , _cccAutoMinorVersionUpgrade = Nothing
    , _cccSnapshotARNs = Nothing
    , _cccCacheParameterGroupName = Nothing
    , _cccSnapshotWindow = Nothing
    , _cccEngine = Nothing
    , _cccPreferredAvailabilityZones = Nothing
    , _cccPreferredMaintenanceWindow = Nothing
    , _cccCacheSubnetGroupName = Nothing
    , _cccPreferredAvailabilityZone = Nothing
    , _cccSnapshotRetentionLimit = Nothing
    , _cccAZMode = Nothing
    , _cccSnapshotName = Nothing
    , _cccReplicationGroupId = Nothing
    , _cccNotificationTopicARN = Nothing
    , _cccTags = Nothing
    , _cccNumCacheNodes = Nothing
    , _cccCacheSecurityGroupNames = Nothing
    , _cccPort = Nothing
    , _cccCacheClusterId = pCacheClusterId_
    }

-- | The compute and memory capacity of the nodes in the node group.
--
-- Valid node types are as follows:
--
-- -   General purpose:
--     -   Current generation: @cache.t2.micro@, @cache.t2.small@,
--         @cache.t2.medium@, @cache.m3.medium@, @cache.m3.large@,
--         @cache.m3.xlarge@, @cache.m3.2xlarge@
--     -   Previous generation: @cache.t1.micro@, @cache.m1.small@,
--         @cache.m1.medium@, @cache.m1.large@, @cache.m1.xlarge@
-- -   Compute optimized: @cache.c1.xlarge@
-- -   Memory optimized
--     -   Current generation: @cache.r3.large@, @cache.r3.xlarge@,
--         @cache.r3.2xlarge@, @cache.r3.4xlarge@, @cache.r3.8xlarge@
--     -   Previous generation: @cache.m2.xlarge@, @cache.m2.2xlarge@,
--         @cache.m2.4xlarge@
--
-- __Notes:__
--
-- -   All t2 instances are created in an Amazon Virtual Private Cloud
--     (VPC).
-- -   Redis backup\/restore is not supported for t2 instances.
-- -   Redis Append-only files (AOF) functionality is not supported for t1
--     or t2 instances.
--
-- For a complete listing of cache node types and specifications, see
-- <http://aws.amazon.com/elasticache/details Amazon ElastiCache Product Features and Details>
-- and
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Memcached.html#CacheParameterGroups.Memcached.NodeSpecific Cache Node Type-Specific Parameters for Memcached>
-- or
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/CacheParameterGroups.Redis.html#CacheParameterGroups.Redis.NodeSpecific Cache Node Type-Specific Parameters for Redis>.
cccCacheNodeType :: Lens' CreateCacheCluster (Maybe Text)
cccCacheNodeType = lens _cccCacheNodeType (\ s a -> s{_cccCacheNodeType = a});

-- | The version number of the cache engine to be used for this cache
-- cluster. To view the supported cache engine versions, use the
-- /DescribeCacheEngineVersions/ action.
cccEngineVersion :: Lens' CreateCacheCluster (Maybe Text)
cccEngineVersion = lens _cccEngineVersion (\ s a -> s{_cccEngineVersion = a});

-- | One or more VPC security groups associated with the cache cluster.
--
-- Use this parameter only when you are creating a cache cluster in an
-- Amazon Virtual Private Cloud (VPC).
cccSecurityGroupIds :: Lens' CreateCacheCluster [Text]
cccSecurityGroupIds = lens _cccSecurityGroupIds (\ s a -> s{_cccSecurityGroupIds = a}) . _Default . _Coerce;

-- | This parameter is currently disabled.
cccAutoMinorVersionUpgrade :: Lens' CreateCacheCluster (Maybe Bool)
cccAutoMinorVersionUpgrade = lens _cccAutoMinorVersionUpgrade (\ s a -> s{_cccAutoMinorVersionUpgrade = a});

-- | A single-element string list containing an Amazon Resource Name (ARN)
-- that uniquely identifies a Redis RDB snapshot file stored in Amazon S3.
-- The snapshot file will be used to populate the node group. The Amazon S3
-- object name in the ARN cannot contain any commas.
--
-- __Note:__ This parameter is only valid if the @Engine@ parameter is
-- @redis@.
--
-- Example of an Amazon S3 ARN: @arn:aws:s3:::my_bucket\/snapshot1.rdb@
cccSnapshotARNs :: Lens' CreateCacheCluster [Text]
cccSnapshotARNs = lens _cccSnapshotARNs (\ s a -> s{_cccSnapshotARNs = a}) . _Default . _Coerce;

-- | The name of the parameter group to associate with this cache cluster. If
-- this argument is omitted, the default parameter group for the specified
-- engine is used.
cccCacheParameterGroupName :: Lens' CreateCacheCluster (Maybe Text)
cccCacheParameterGroupName = lens _cccCacheParameterGroupName (\ s a -> s{_cccCacheParameterGroupName = a});

-- | The daily time range (in UTC) during which ElastiCache will begin taking
-- a daily snapshot of your node group.
--
-- Example: @05:00-09:00@
--
-- If you do not specify this parameter, then ElastiCache will
-- automatically choose an appropriate time range.
--
-- __Note:__ This parameter is only valid if the @Engine@ parameter is
-- @redis@.
cccSnapshotWindow :: Lens' CreateCacheCluster (Maybe Text)
cccSnapshotWindow = lens _cccSnapshotWindow (\ s a -> s{_cccSnapshotWindow = a});

-- | The name of the cache engine to be used for this cache cluster.
--
-- Valid values for this parameter are:
--
-- @memcached@ | @redis@
cccEngine :: Lens' CreateCacheCluster (Maybe Text)
cccEngine = lens _cccEngine (\ s a -> s{_cccEngine = a});

-- | A list of the Availability Zones in which cache nodes will be created.
-- The order of the zones in the list is not important.
--
-- This option is only supported on Memcached.
--
-- If you are creating your cache cluster in an Amazon VPC (recommended)
-- you can only locate nodes in Availability Zones that are associated with
-- the subnets in the selected subnet group.
--
-- The number of Availability Zones listed must equal the value of
-- @NumCacheNodes@.
--
-- If you want all the nodes in the same Availability Zone, use
-- @PreferredAvailabilityZone@ instead, or repeat the Availability Zone
-- multiple times in the list.
--
-- Default: System chosen Availability Zones.
--
-- Example: One Memcached node in each of three different Availability
-- Zones:
-- @PreferredAvailabilityZones.member.1=us-west-2a&PreferredAvailabilityZones.member.2=us-west-2b&PreferredAvailabilityZones.member.3=us-west-2c@
--
-- Example: All three Memcached nodes in one Availability Zone:
-- @PreferredAvailabilityZones.member.1=us-west-2a&PreferredAvailabilityZones.member.2=us-west-2a&PreferredAvailabilityZones.member.3=us-west-2a@
cccPreferredAvailabilityZones :: Lens' CreateCacheCluster [Text]
cccPreferredAvailabilityZones = lens _cccPreferredAvailabilityZones (\ s a -> s{_cccPreferredAvailabilityZones = a}) . _Default . _Coerce;

-- | Specifies the weekly time range during which maintenance on the cache
-- cluster is performed. It is specified as a range in the format
-- ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window
-- is a 60 minute period. Valid values for @ddd@ are:
--
-- -   @sun@
-- -   @mon@
-- -   @tue@
-- -   @wed@
-- -   @thu@
-- -   @fri@
-- -   @sat@
--
-- Example: @sun:05:00-sun:09:00@
cccPreferredMaintenanceWindow :: Lens' CreateCacheCluster (Maybe Text)
cccPreferredMaintenanceWindow = lens _cccPreferredMaintenanceWindow (\ s a -> s{_cccPreferredMaintenanceWindow = a});

-- | The name of the subnet group to be used for the cache cluster.
--
-- Use this parameter only when you are creating a cache cluster in an
-- Amazon Virtual Private Cloud (VPC).
cccCacheSubnetGroupName :: Lens' CreateCacheCluster (Maybe Text)
cccCacheSubnetGroupName = lens _cccCacheSubnetGroupName (\ s a -> s{_cccCacheSubnetGroupName = a});

-- | The EC2 Availability Zone in which the cache cluster will be created.
--
-- All nodes belonging to this Memcached cache cluster are placed in the
-- preferred Availability Zone. If you want to create your nodes across
-- multiple Availability Zones, use @PreferredAvailabilityZones@.
--
-- Default: System chosen Availability Zone.
cccPreferredAvailabilityZone :: Lens' CreateCacheCluster (Maybe Text)
cccPreferredAvailabilityZone = lens _cccPreferredAvailabilityZone (\ s a -> s{_cccPreferredAvailabilityZone = a});

-- | The number of days for which ElastiCache will retain automatic snapshots
-- before deleting them. For example, if you set @SnapshotRetentionLimit@
-- to 5, then a snapshot that was taken today will be retained for 5 days
-- before being deleted.
--
-- __Note:__ This parameter is only valid if the @Engine@ parameter is
-- @redis@.
--
-- Default: 0 (i.e., automatic backups are disabled for this cache
-- cluster).
cccSnapshotRetentionLimit :: Lens' CreateCacheCluster (Maybe Int)
cccSnapshotRetentionLimit = lens _cccSnapshotRetentionLimit (\ s a -> s{_cccSnapshotRetentionLimit = a});

-- | Specifies whether the nodes in this Memcached node group are created in
-- a single Availability Zone or created across multiple Availability Zones
-- in the cluster\'s region.
--
-- This parameter is only supported for Memcached cache clusters.
--
-- If the @AZMode@ and @PreferredAvailabilityZones@ are not specified,
-- ElastiCache assumes @single-az@ mode.
cccAZMode :: Lens' CreateCacheCluster (Maybe AZMode)
cccAZMode = lens _cccAZMode (\ s a -> s{_cccAZMode = a});

-- | The name of a snapshot from which to restore data into the new node
-- group. The snapshot status changes to @restoring@ while the new node
-- group is being created.
--
-- __Note:__ This parameter is only valid if the @Engine@ parameter is
-- @redis@.
cccSnapshotName :: Lens' CreateCacheCluster (Maybe Text)
cccSnapshotName = lens _cccSnapshotName (\ s a -> s{_cccSnapshotName = a});

-- | The ID of the replication group to which this cache cluster should
-- belong. If this parameter is specified, the cache cluster will be added
-- to the specified replication group as a read replica; otherwise, the
-- cache cluster will be a standalone primary that is not part of any
-- replication group.
--
-- If the specified replication group is Multi-AZ enabled and the
-- availability zone is not specified, the cache cluster will be created in
-- availability zones that provide the best spread of read replicas across
-- availability zones.
--
-- __Note:__ This parameter is only valid if the @Engine@ parameter is
-- @redis@.
cccReplicationGroupId :: Lens' CreateCacheCluster (Maybe Text)
cccReplicationGroupId = lens _cccReplicationGroupId (\ s a -> s{_cccReplicationGroupId = a});

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- (SNS) topic to which notifications will be sent.
--
-- The Amazon SNS topic owner must be the same as the cache cluster owner.
cccNotificationTopicARN :: Lens' CreateCacheCluster (Maybe Text)
cccNotificationTopicARN = lens _cccNotificationTopicARN (\ s a -> s{_cccNotificationTopicARN = a});

-- | A list of cost allocation tags to be added to this resource. A tag is a
-- key-value pair. A tag key must be accompanied by a tag value.
cccTags :: Lens' CreateCacheCluster [Tag]
cccTags = lens _cccTags (\ s a -> s{_cccTags = a}) . _Default . _Coerce;

-- | The initial number of cache nodes that the cache cluster will have.
--
-- For clusters running Redis, this value must be 1. For clusters running
-- Memcached, this value must be between 1 and 20.
--
-- If you need more than 20 nodes for your Memcached cluster, please fill
-- out the ElastiCache Limit Increase Request form at
-- <http://aws.amazon.com/contact-us/elasticache-node-limit-request/>.
cccNumCacheNodes :: Lens' CreateCacheCluster (Maybe Int)
cccNumCacheNodes = lens _cccNumCacheNodes (\ s a -> s{_cccNumCacheNodes = a});

-- | A list of security group names to associate with this cache cluster.
--
-- Use this parameter only when you are creating a cache cluster outside of
-- an Amazon Virtual Private Cloud (VPC).
cccCacheSecurityGroupNames :: Lens' CreateCacheCluster [Text]
cccCacheSecurityGroupNames = lens _cccCacheSecurityGroupNames (\ s a -> s{_cccCacheSecurityGroupNames = a}) . _Default . _Coerce;

-- | The port number on which each of the cache nodes will accept
-- connections.
cccPort :: Lens' CreateCacheCluster (Maybe Int)
cccPort = lens _cccPort (\ s a -> s{_cccPort = a});

-- | The node group identifier. This parameter is stored as a lowercase
-- string.
--
-- Constraints:
--
-- -   A name must contain from 1 to 20 alphanumeric characters or hyphens.
-- -   The first character must be a letter.
-- -   A name cannot end with a hyphen or contain two consecutive hyphens.
cccCacheClusterId :: Lens' CreateCacheCluster Text
cccCacheClusterId = lens _cccCacheClusterId (\ s a -> s{_cccCacheClusterId = a});

instance AWSRequest CreateCacheCluster where
        type Sv CreateCacheCluster = ElastiCache
        type Rs CreateCacheCluster =
             CreateCacheClusterResponse
        request = post
        response
          = receiveXMLWrapper "CreateCacheClusterResult"
              (\ s h x ->
                 CreateCacheClusterResponse' <$>
                   (x .@? "CacheCluster") <*> (pure (fromEnum s)))

instance ToHeaders CreateCacheCluster where
        toHeaders = const mempty

instance ToPath CreateCacheCluster where
        toPath = const "/"

instance ToQuery CreateCacheCluster where
        toQuery CreateCacheCluster'{..}
          = mconcat
              ["Action" =: ("CreateCacheCluster" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "CacheNodeType" =: _cccCacheNodeType,
               "EngineVersion" =: _cccEngineVersion,
               "SecurityGroupIds" =:
                 toQuery
                   (toQueryList "SecurityGroupId" <$>
                      _cccSecurityGroupIds),
               "AutoMinorVersionUpgrade" =:
                 _cccAutoMinorVersionUpgrade,
               "SnapshotArns" =:
                 toQuery
                   (toQueryList "SnapshotArn" <$> _cccSnapshotARNs),
               "CacheParameterGroupName" =:
                 _cccCacheParameterGroupName,
               "SnapshotWindow" =: _cccSnapshotWindow,
               "Engine" =: _cccEngine,
               "PreferredAvailabilityZones" =:
                 toQuery
                   (toQueryList "PreferredAvailabilityZone" <$>
                      _cccPreferredAvailabilityZones),
               "PreferredMaintenanceWindow" =:
                 _cccPreferredMaintenanceWindow,
               "CacheSubnetGroupName" =: _cccCacheSubnetGroupName,
               "PreferredAvailabilityZone" =:
                 _cccPreferredAvailabilityZone,
               "SnapshotRetentionLimit" =:
                 _cccSnapshotRetentionLimit,
               "AZMode" =: _cccAZMode,
               "SnapshotName" =: _cccSnapshotName,
               "ReplicationGroupId" =: _cccReplicationGroupId,
               "NotificationTopicArn" =: _cccNotificationTopicARN,
               "Tags" =: toQuery (toQueryList "Tag" <$> _cccTags),
               "NumCacheNodes" =: _cccNumCacheNodes,
               "CacheSecurityGroupNames" =:
                 toQuery
                   (toQueryList "CacheSecurityGroupName" <$>
                      _cccCacheSecurityGroupNames),
               "Port" =: _cccPort,
               "CacheClusterId" =: _cccCacheClusterId]

-- | /See:/ 'createCacheClusterResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cccrsCacheCluster'
--
-- * 'cccrsStatus'
data CreateCacheClusterResponse = CreateCacheClusterResponse'
    { _cccrsCacheCluster :: !(Maybe CacheCluster)
    , _cccrsStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateCacheClusterResponse' smart constructor.
createCacheClusterResponse :: Int -> CreateCacheClusterResponse
createCacheClusterResponse pStatus_ =
    CreateCacheClusterResponse'
    { _cccrsCacheCluster = Nothing
    , _cccrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
cccrsCacheCluster :: Lens' CreateCacheClusterResponse (Maybe CacheCluster)
cccrsCacheCluster = lens _cccrsCacheCluster (\ s a -> s{_cccrsCacheCluster = a});

-- | FIXME: Undocumented member.
cccrsStatus :: Lens' CreateCacheClusterResponse Int
cccrsStatus = lens _cccrsStatus (\ s a -> s{_cccrsStatus = a});
