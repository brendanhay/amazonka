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
    , cccrqCacheNodeType
    , cccrqEngineVersion
    , cccrqSecurityGroupIds
    , cccrqAutoMinorVersionUpgrade
    , cccrqSnapshotARNs
    , cccrqCacheParameterGroupName
    , cccrqSnapshotWindow
    , cccrqEngine
    , cccrqPreferredAvailabilityZones
    , cccrqPreferredMaintenanceWindow
    , cccrqCacheSubnetGroupName
    , cccrqPreferredAvailabilityZone
    , cccrqSnapshotRetentionLimit
    , cccrqAZMode
    , cccrqSnapshotName
    , cccrqReplicationGroupId
    , cccrqNotificationTopicARN
    , cccrqTags
    , cccrqNumCacheNodes
    , cccrqCacheSecurityGroupNames
    , cccrqPort
    , cccrqCacheClusterId

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
-- * 'cccrqCacheNodeType'
--
-- * 'cccrqEngineVersion'
--
-- * 'cccrqSecurityGroupIds'
--
-- * 'cccrqAutoMinorVersionUpgrade'
--
-- * 'cccrqSnapshotARNs'
--
-- * 'cccrqCacheParameterGroupName'
--
-- * 'cccrqSnapshotWindow'
--
-- * 'cccrqEngine'
--
-- * 'cccrqPreferredAvailabilityZones'
--
-- * 'cccrqPreferredMaintenanceWindow'
--
-- * 'cccrqCacheSubnetGroupName'
--
-- * 'cccrqPreferredAvailabilityZone'
--
-- * 'cccrqSnapshotRetentionLimit'
--
-- * 'cccrqAZMode'
--
-- * 'cccrqSnapshotName'
--
-- * 'cccrqReplicationGroupId'
--
-- * 'cccrqNotificationTopicARN'
--
-- * 'cccrqTags'
--
-- * 'cccrqNumCacheNodes'
--
-- * 'cccrqCacheSecurityGroupNames'
--
-- * 'cccrqPort'
--
-- * 'cccrqCacheClusterId'
data CreateCacheCluster = CreateCacheCluster'
    { _cccrqCacheNodeType              :: !(Maybe Text)
    , _cccrqEngineVersion              :: !(Maybe Text)
    , _cccrqSecurityGroupIds           :: !(Maybe [Text])
    , _cccrqAutoMinorVersionUpgrade    :: !(Maybe Bool)
    , _cccrqSnapshotARNs               :: !(Maybe [Text])
    , _cccrqCacheParameterGroupName    :: !(Maybe Text)
    , _cccrqSnapshotWindow             :: !(Maybe Text)
    , _cccrqEngine                     :: !(Maybe Text)
    , _cccrqPreferredAvailabilityZones :: !(Maybe [Text])
    , _cccrqPreferredMaintenanceWindow :: !(Maybe Text)
    , _cccrqCacheSubnetGroupName       :: !(Maybe Text)
    , _cccrqPreferredAvailabilityZone  :: !(Maybe Text)
    , _cccrqSnapshotRetentionLimit     :: !(Maybe Int)
    , _cccrqAZMode                     :: !(Maybe AZMode)
    , _cccrqSnapshotName               :: !(Maybe Text)
    , _cccrqReplicationGroupId         :: !(Maybe Text)
    , _cccrqNotificationTopicARN       :: !(Maybe Text)
    , _cccrqTags                       :: !(Maybe [Tag])
    , _cccrqNumCacheNodes              :: !(Maybe Int)
    , _cccrqCacheSecurityGroupNames    :: !(Maybe [Text])
    , _cccrqPort                       :: !(Maybe Int)
    , _cccrqCacheClusterId             :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateCacheCluster' smart constructor.
createCacheCluster :: Text -> CreateCacheCluster
createCacheCluster pCacheClusterId =
    CreateCacheCluster'
    { _cccrqCacheNodeType = Nothing
    , _cccrqEngineVersion = Nothing
    , _cccrqSecurityGroupIds = Nothing
    , _cccrqAutoMinorVersionUpgrade = Nothing
    , _cccrqSnapshotARNs = Nothing
    , _cccrqCacheParameterGroupName = Nothing
    , _cccrqSnapshotWindow = Nothing
    , _cccrqEngine = Nothing
    , _cccrqPreferredAvailabilityZones = Nothing
    , _cccrqPreferredMaintenanceWindow = Nothing
    , _cccrqCacheSubnetGroupName = Nothing
    , _cccrqPreferredAvailabilityZone = Nothing
    , _cccrqSnapshotRetentionLimit = Nothing
    , _cccrqAZMode = Nothing
    , _cccrqSnapshotName = Nothing
    , _cccrqReplicationGroupId = Nothing
    , _cccrqNotificationTopicARN = Nothing
    , _cccrqTags = Nothing
    , _cccrqNumCacheNodes = Nothing
    , _cccrqCacheSecurityGroupNames = Nothing
    , _cccrqPort = Nothing
    , _cccrqCacheClusterId = pCacheClusterId
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
cccrqCacheNodeType :: Lens' CreateCacheCluster (Maybe Text)
cccrqCacheNodeType = lens _cccrqCacheNodeType (\ s a -> s{_cccrqCacheNodeType = a});

-- | The version number of the cache engine to be used for this cache
-- cluster. To view the supported cache engine versions, use the
-- /DescribeCacheEngineVersions/ action.
cccrqEngineVersion :: Lens' CreateCacheCluster (Maybe Text)
cccrqEngineVersion = lens _cccrqEngineVersion (\ s a -> s{_cccrqEngineVersion = a});

-- | One or more VPC security groups associated with the cache cluster.
--
-- Use this parameter only when you are creating a cache cluster in an
-- Amazon Virtual Private Cloud (VPC).
cccrqSecurityGroupIds :: Lens' CreateCacheCluster [Text]
cccrqSecurityGroupIds = lens _cccrqSecurityGroupIds (\ s a -> s{_cccrqSecurityGroupIds = a}) . _Default;

-- | This parameter is currently disabled.
cccrqAutoMinorVersionUpgrade :: Lens' CreateCacheCluster (Maybe Bool)
cccrqAutoMinorVersionUpgrade = lens _cccrqAutoMinorVersionUpgrade (\ s a -> s{_cccrqAutoMinorVersionUpgrade = a});

-- | A single-element string list containing an Amazon Resource Name (ARN)
-- that uniquely identifies a Redis RDB snapshot file stored in Amazon S3.
-- The snapshot file will be used to populate the node group. The Amazon S3
-- object name in the ARN cannot contain any commas.
--
-- __Note:__ This parameter is only valid if the @Engine@ parameter is
-- @redis@.
--
-- Example of an Amazon S3 ARN: @arn:aws:s3:::my_bucket\/snapshot1.rdb@
cccrqSnapshotARNs :: Lens' CreateCacheCluster [Text]
cccrqSnapshotARNs = lens _cccrqSnapshotARNs (\ s a -> s{_cccrqSnapshotARNs = a}) . _Default;

-- | The name of the parameter group to associate with this cache cluster. If
-- this argument is omitted, the default parameter group for the specified
-- engine is used.
cccrqCacheParameterGroupName :: Lens' CreateCacheCluster (Maybe Text)
cccrqCacheParameterGroupName = lens _cccrqCacheParameterGroupName (\ s a -> s{_cccrqCacheParameterGroupName = a});

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
cccrqSnapshotWindow :: Lens' CreateCacheCluster (Maybe Text)
cccrqSnapshotWindow = lens _cccrqSnapshotWindow (\ s a -> s{_cccrqSnapshotWindow = a});

-- | The name of the cache engine to be used for this cache cluster.
--
-- Valid values for this parameter are:
--
-- @memcached@ | @redis@
cccrqEngine :: Lens' CreateCacheCluster (Maybe Text)
cccrqEngine = lens _cccrqEngine (\ s a -> s{_cccrqEngine = a});

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
cccrqPreferredAvailabilityZones :: Lens' CreateCacheCluster [Text]
cccrqPreferredAvailabilityZones = lens _cccrqPreferredAvailabilityZones (\ s a -> s{_cccrqPreferredAvailabilityZones = a}) . _Default;

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
cccrqPreferredMaintenanceWindow :: Lens' CreateCacheCluster (Maybe Text)
cccrqPreferredMaintenanceWindow = lens _cccrqPreferredMaintenanceWindow (\ s a -> s{_cccrqPreferredMaintenanceWindow = a});

-- | The name of the subnet group to be used for the cache cluster.
--
-- Use this parameter only when you are creating a cache cluster in an
-- Amazon Virtual Private Cloud (VPC).
cccrqCacheSubnetGroupName :: Lens' CreateCacheCluster (Maybe Text)
cccrqCacheSubnetGroupName = lens _cccrqCacheSubnetGroupName (\ s a -> s{_cccrqCacheSubnetGroupName = a});

-- | The EC2 Availability Zone in which the cache cluster will be created.
--
-- All nodes belonging to this Memcached cache cluster are placed in the
-- preferred Availability Zone. If you want to create your nodes across
-- multiple Availability Zones, use @PreferredAvailabilityZones@.
--
-- Default: System chosen Availability Zone.
cccrqPreferredAvailabilityZone :: Lens' CreateCacheCluster (Maybe Text)
cccrqPreferredAvailabilityZone = lens _cccrqPreferredAvailabilityZone (\ s a -> s{_cccrqPreferredAvailabilityZone = a});

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
cccrqSnapshotRetentionLimit :: Lens' CreateCacheCluster (Maybe Int)
cccrqSnapshotRetentionLimit = lens _cccrqSnapshotRetentionLimit (\ s a -> s{_cccrqSnapshotRetentionLimit = a});

-- | Specifies whether the nodes in this Memcached node group are created in
-- a single Availability Zone or created across multiple Availability Zones
-- in the cluster\'s region.
--
-- This parameter is only supported for Memcached cache clusters.
--
-- If the @AZMode@ and @PreferredAvailabilityZones@ are not specified,
-- ElastiCache assumes @single-az@ mode.
cccrqAZMode :: Lens' CreateCacheCluster (Maybe AZMode)
cccrqAZMode = lens _cccrqAZMode (\ s a -> s{_cccrqAZMode = a});

-- | The name of a snapshot from which to restore data into the new node
-- group. The snapshot status changes to @restoring@ while the new node
-- group is being created.
--
-- __Note:__ This parameter is only valid if the @Engine@ parameter is
-- @redis@.
cccrqSnapshotName :: Lens' CreateCacheCluster (Maybe Text)
cccrqSnapshotName = lens _cccrqSnapshotName (\ s a -> s{_cccrqSnapshotName = a});

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
cccrqReplicationGroupId :: Lens' CreateCacheCluster (Maybe Text)
cccrqReplicationGroupId = lens _cccrqReplicationGroupId (\ s a -> s{_cccrqReplicationGroupId = a});

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- (SNS) topic to which notifications will be sent.
--
-- The Amazon SNS topic owner must be the same as the cache cluster owner.
cccrqNotificationTopicARN :: Lens' CreateCacheCluster (Maybe Text)
cccrqNotificationTopicARN = lens _cccrqNotificationTopicARN (\ s a -> s{_cccrqNotificationTopicARN = a});

-- | A list of cost allocation tags to be added to this resource. A tag is a
-- key-value pair. A tag key must be accompanied by a tag value.
cccrqTags :: Lens' CreateCacheCluster [Tag]
cccrqTags = lens _cccrqTags (\ s a -> s{_cccrqTags = a}) . _Default;

-- | The initial number of cache nodes that the cache cluster will have.
--
-- For clusters running Redis, this value must be 1. For clusters running
-- Memcached, this value must be between 1 and 20.
--
-- If you need more than 20 nodes for your Memcached cluster, please fill
-- out the ElastiCache Limit Increase Request form at
-- <http://aws.amazon.com/contact-us/elasticache-node-limit-request/>.
cccrqNumCacheNodes :: Lens' CreateCacheCluster (Maybe Int)
cccrqNumCacheNodes = lens _cccrqNumCacheNodes (\ s a -> s{_cccrqNumCacheNodes = a});

-- | A list of security group names to associate with this cache cluster.
--
-- Use this parameter only when you are creating a cache cluster outside of
-- an Amazon Virtual Private Cloud (VPC).
cccrqCacheSecurityGroupNames :: Lens' CreateCacheCluster [Text]
cccrqCacheSecurityGroupNames = lens _cccrqCacheSecurityGroupNames (\ s a -> s{_cccrqCacheSecurityGroupNames = a}) . _Default;

-- | The port number on which each of the cache nodes will accept
-- connections.
cccrqPort :: Lens' CreateCacheCluster (Maybe Int)
cccrqPort = lens _cccrqPort (\ s a -> s{_cccrqPort = a});

-- | The node group identifier. This parameter is stored as a lowercase
-- string.
--
-- Constraints:
--
-- -   A name must contain from 1 to 20 alphanumeric characters or hyphens.
-- -   The first character must be a letter.
-- -   A name cannot end with a hyphen or contain two consecutive hyphens.
cccrqCacheClusterId :: Lens' CreateCacheCluster Text
cccrqCacheClusterId = lens _cccrqCacheClusterId (\ s a -> s{_cccrqCacheClusterId = a});

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
               "CacheNodeType" =: _cccrqCacheNodeType,
               "EngineVersion" =: _cccrqEngineVersion,
               "SecurityGroupIds" =:
                 toQuery
                   (toQueryList "SecurityGroupId" <$>
                      _cccrqSecurityGroupIds),
               "AutoMinorVersionUpgrade" =:
                 _cccrqAutoMinorVersionUpgrade,
               "SnapshotArns" =:
                 toQuery
                   (toQueryList "SnapshotArn" <$> _cccrqSnapshotARNs),
               "CacheParameterGroupName" =:
                 _cccrqCacheParameterGroupName,
               "SnapshotWindow" =: _cccrqSnapshotWindow,
               "Engine" =: _cccrqEngine,
               "PreferredAvailabilityZones" =:
                 toQuery
                   (toQueryList "PreferredAvailabilityZone" <$>
                      _cccrqPreferredAvailabilityZones),
               "PreferredMaintenanceWindow" =:
                 _cccrqPreferredMaintenanceWindow,
               "CacheSubnetGroupName" =: _cccrqCacheSubnetGroupName,
               "PreferredAvailabilityZone" =:
                 _cccrqPreferredAvailabilityZone,
               "SnapshotRetentionLimit" =:
                 _cccrqSnapshotRetentionLimit,
               "AZMode" =: _cccrqAZMode,
               "SnapshotName" =: _cccrqSnapshotName,
               "ReplicationGroupId" =: _cccrqReplicationGroupId,
               "NotificationTopicArn" =: _cccrqNotificationTopicARN,
               "Tags" =: toQuery (toQueryList "Tag" <$> _cccrqTags),
               "NumCacheNodes" =: _cccrqNumCacheNodes,
               "CacheSecurityGroupNames" =:
                 toQuery
                   (toQueryList "CacheSecurityGroupName" <$>
                      _cccrqCacheSecurityGroupNames),
               "Port" =: _cccrqPort,
               "CacheClusterId" =: _cccrqCacheClusterId]

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
createCacheClusterResponse pStatus =
    CreateCacheClusterResponse'
    { _cccrsCacheCluster = Nothing
    , _cccrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
cccrsCacheCluster :: Lens' CreateCacheClusterResponse (Maybe CacheCluster)
cccrsCacheCluster = lens _cccrsCacheCluster (\ s a -> s{_cccrsCacheCluster = a});

-- | FIXME: Undocumented member.
cccrsStatus :: Lens' CreateCacheClusterResponse Int
cccrsStatus = lens _cccrsStatus (\ s a -> s{_cccrsStatus = a});
